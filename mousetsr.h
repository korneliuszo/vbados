/*
 * VBMouse - DOS mouse driver resident part
 * Copyright (C) 2022 Javier S. Pedro
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

#ifndef MOUSETSR_H
#define MOUSETSR_H

#include <stdbool.h>
#include <stdint.h>

#include "int2fwin.h"
#include "int10vga.h"

// User customizable defines

/** Enable VirtualBox integration. */
#define USE_VIRTUALBOX 1
/** Enable VMware integration. */
#define USE_VMWARE 1
/** Enable Windows 386/protected mode integration .*/
#define USE_WIN386 1
/** Enable the wheel. */
#define USE_WHEEL 1
/** Trace mouse events verbosily. */
#define TRACE_EVENTS 0
/** Trace (noisy) API calls. */
#define TRACE_CALLS 1

/** The reported MS MOUSE compatible version to programs who ask for it. */
#define REPORTED_VERSION_MAJOR 6
#define REPORTED_VERSION_MINOR 0x30

// End of user customizable defines

#define USE_INTEGRATION (USE_VIRTUALBOX || USE_VMWARE)

/** Max size of PS/2 packet that we support. */
#define MAX_PS2_PACKET_SIZE 4

/** Maximum number of 55ms ticks that may pass between two bytes of the same PS/2 packet */
#define MAX_PS2_PACKET_DELAY 2

/** Number of buttons reported back to user programs. */
#define NUM_BUTTONS 3

/** Size of int33 graphic cursor shape definitions. */
#define GRAPHIC_CURSOR_WIDTH 16
#define GRAPHIC_CURSOR_HEIGHT 16
#define GRAPHIC_CURSOR_SCANLINE_LEN 2
#define GRAPHIC_CURSOR_MASK_LEN (GRAPHIC_CURSOR_HEIGHT * GRAPHIC_CURSOR_SCANLINE_LEN)
#define GRAPHIC_CURSOR_DATA_LEN (2 * GRAPHIC_CURSOR_MASK_LEN)

#if USE_VIRTUALBOX
#include "vbox.h"

/** Size of the VBox buffer. The maximum message length that may be sent.
 *  Enough to fit a set_pointer_shape message with a 16x16 cursor.  */
#define VBOX_BUFFER_SIZE (1024 + 32 + 24 + 20)
#endif

struct point {
	int16_t x, y;
};

typedef struct tsrdata {
	// TSR installation data
	/** Previous int33 ISR, storing it for uninstall. */
	void (__interrupt __far *prev_int33_handler)();
#if USE_WIN386
	void (__interrupt __far *prev_int2f_handler)();
#endif
	// Settings configured via the command line
#if USE_WHEEL
	/** Whether to enable & use wheel mouse. */
	bool usewheel;
	/** Key (scancode) to generate on wheel scroll up/down, or 0 for none. */
	uint16_t wheel_up_key, wheel_down_key;
#endif

	// Video settings
	/** Information of the current video mode. */
	struct modeinfo video_mode;
	/** Max (virtual) coordinates of full screen in the current mode.
	 *  Used for rendering graphic cursor, mapping absolute coordinates,
	 *  and initializing the default min/max window. */
	struct point screen_max;
	/** In some modes, the virtual coordinates are larger than the
	 *  physical screen coordinates.
	 *  real coordinates = virtual coordinates * screen_scale. */
	struct point screen_scale;
	/** In text modes, we want to snap the cursor position to the cell grid.
	 *  This stores the desired grid granularity. */
	struct point screen_granularity;

	// Detected mouse hardware & status
#if USE_WHEEL
	/** Whether the current mouse has a wheel (and support is enabled). */
	bool haswheel;
#endif
	/** Packet size that the BIOS is currently using. Either 1 (streaming) or 3 (plain). */
	uint8_t bios_packet_size;
	/** Packet size that we are currently expecting internally. Usually 3 (plain) or 4 (with wheel). */
	uint8_t packet_size;
	/** For streaming mode: number of bytes received so far (< packet_size). */
	uint8_t cur_packet_bytes;
	/** Stores the bytes received so far (cur_bytes). */
	uint8_t ps2_packet[MAX_PS2_PACKET_SIZE];
	/** Number of ticks at the point when we started to receive this packet. */
	uint16_t cur_packet_ticks;

	// Current mouse settings
	/** Mouse sensitivity/speed. */
	struct point mickeysPerLine; // mickeys per 8 pixels
	/** Mouse acceleration "double-speed threshold". */
	uint16_t doubleSpeedThreshold; // mickeys
	/** Current window min coordinates. */
	struct point min;
	/** Current window max coordinates. */
	struct point max;
	/** If N > 0, cursor has been hidden N times, and will require N show calls to unhide. */
	uint8_t hidden_count;
	/** For text cursor, whether this is a software or hardware cursor. */
	uint8_t cursor_text_type;
	/** Masks for the text cursor. */
	uint16_t cursor_text_and_mask, cursor_text_xor_mask;
	/** Hotspot for the graphic cursor. */
	struct point cursor_hotspot;
	/** Masks for the graphic cursor. */
	uint16_t cursor_graphic[GRAPHIC_CURSOR_DATA_LEN/sizeof(uint16_t)];
#if USE_WHEEL
	/** Whether someone asked for the int33 wheel API, in which case we
	 *  should send them wheel movement rather than fake keypresses. */
	bool usewheelapi;
#endif

	// Current mouse status
	/** Current cursor position (in pixels). */
	struct point pos;
	/** Current remainder of movement that does not yet translate to an entire pixel
	 *  (8ths of pixel). */
	struct point pos_frac;
	/** Current delta movement (in mickeys) since the last report. */
	struct point delta;
	/** Current remainder of delta movement that does not yet translate to an entire mickey
	 *  Usually only when mickeysPerLine is not a multiple of 8. */
	struct point delta_frac;
	/** Last absolute position (to compute a delta for relative motion emulation). Using -1 for "none". */
	struct point abs_pos;
	/** Total mickeys moved in the last second. */
	uint16_t total_motion;
	/** Ticks when the above value was last reset. */
	uint16_t last_motion_ticks;
	/** Current status of buttons (as bitfield). */
	uint16_t buttons;
	struct {
		struct buttoncounter {
			struct point last;
			uint16_t count;
		} pressed, released;
	} button[NUM_BUTTONS];
	/** Total delta movement of the wheel since the last wheel report. */
	int16_t wheel_delta;
	/** Last position where the wheel was moved. */
	struct point wheel_last;

	// Cursor information
	/** Whether the cursor is currently displayed or not. */
	bool cursor_visible;
	/** The current position at which the cursor is displayed. */
	struct point cursor_pos;
	/** For text mode cursor, the character data that was displayed below the cursor. */
	uint16_t cursor_prev_char;
	/** For graphical mode cursor, contents of the screen that were displayed below
	 *  the cursor before the cursor was drawn. */
	uint8_t cursor_prev_graphic[GRAPHIC_CURSOR_WIDTH * GRAPHIC_CURSOR_HEIGHT];

	// Current handlers
	/** Address of the event handler. */
	void (__far *event_handler)();
	/** Events for which we should call the event handler. */
	uint16_t event_mask;

#if USE_WIN386
	/** Information that we pass to Windows 386 on startup. */
	win386_startup_info w386_startup;
	win386_instance_item w386_instance[2];
	/** Whether Windows 386 is running. */
	bool haswin386 : 1;
	/** Whether Windows 386 is rendering the cursor for us,
	 *  and therefore we should hide our own. */
	bool w386cursor : 1;
#endif

#if USE_VMWARE
	/** VMware is available. */
	bool vmwavail : 1;
#endif

#if USE_VIRTUALBOX
	/** VirtualBox is available. */
	bool vbavail : 1;
	/** Want to use the VirtualBox "host" cursor. */
	bool vbwantcursor : 1;
	/** Have VirtualBox absolute coordinates. */
	bool vbhaveabs : 1;
	struct vboxcomm vb;
	char vbbuf[VBOX_BUFFER_SIZE];
#endif
} TSRDATA;

typedef TSRDATA * PTSRDATA;
typedef TSRDATA __far * LPTSRDATA;

extern void __declspec(naked) __far int33_isr(void);

extern void __declspec(naked) __far int2f_isr(void);

extern LPTSRDATA __far get_tsr_data(bool installed);

/** This symbol is always at the end of the TSR segment */
extern int resident_end;

/** This is not just data, but the entire segment. */
static inline unsigned get_resident_size(void)
{
	return FP_OFF(&resident_end);
}

#endif /* MOUSETSR_H */
