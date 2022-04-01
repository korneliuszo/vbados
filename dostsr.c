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

#include <i86.h>

#include "dlog.h"
#include "ps2.h"
#include "int10vga.h"
#include "int33.h"
#include "dostsr.h"

TSRDATA data;

static const uint16_t default_cursor_graphic[] = {
    0x3FFF, 0x1FFF, 0x0FFF, 0x07FF,
    0x03FF, 0x01FF, 0x00FF, 0x007F,
    0x003F, 0x001F, 0x01FF, 0x00FF,
    0x30FF, 0xF87F, 0xF87F, 0xFCFF,
    0x0000, 0x4000, 0x6000, 0x7000,
    0x7800, 0x7C00, 0x7E00, 0x7F00,
    0x7F80, 0x7C00, 0x6C00, 0x4600,
    0x0600, 0x0300, 0x0300, 0x0000
};

static void bound_position_to_window(void)
{
	if (data.pos.x < data.min.x) data.pos.x = data.min.x;
	if (data.pos.x > data.max.x) data.pos.x = data.max.x;
	if (data.pos.y < data.min.y) data.pos.y = data.min.y;
	if (data.pos.y > data.max.y) data.pos.y = data.max.y;
}

static inline bool is_text_mode(uint8_t mode)
{
	switch (mode) {
	case 0:
	case 1:
	case 2:
	case 3: // CGA text modes with 25 rows and variable columns
	case 7: // MDA Mono text mode
		return true;
	default:
		return false;
	}
}

static void hide_text_cursor(void)
{
	// Restore the character under the old position of the cursor
	uint16_t __far *ch = get_video_char(data.screen_page,
	                                    data.cursor_pos.x / 8, data.cursor_pos.y / 8);
	*ch = data.cursor_prev_char;
	data.cursor_visible = false;
}

static void show_text_cursor(void)
{
	uint16_t __far *ch = get_video_char(data.screen_page,
	                                    data.pos.x / 8, data.pos.y / 8);
	data.cursor_prev_char = *ch;
	data.cursor_pos = data.pos;
	*ch = (*ch & data.cursor_text_and_mask) ^ data.cursor_text_xor_mask;
	data.cursor_visible = true;
}

static bool get_graphic_cursor_area(struct point __far *cursor_pos,
                                    struct point __far *start,
                                    struct point __far *size,
                                    struct point __far *offset)
{
	struct point screen_size;
	screen_size.x = ((data.screen_max.x + 1) / data.screen_scale.x);
	screen_size.y = ((data.screen_max.y + 1) / data.screen_scale.y);

	start->x = (cursor_pos->x / data.screen_scale.x) - data.cursor_hotspot.x;
	start->y = (cursor_pos->y / data.screen_scale.y) - data.cursor_hotspot.y;
	size->x = GRAPHIC_CURSOR_WIDTH;
	size->y = GRAPHIC_CURSOR_HEIGHT;
	offset->x = 0;
	offset->y = 0;

	// Start clipping around
	if (start->x < 0) {
		offset->x += -start->x;
		start->x = 0;
	}
	if (start->y < 0) {
		offset->y += -start->y;
		start->y = 0;
	}
	if (start->x > screen_size.x) {
		return false; // Don't render cursor
	} else if (start->x + size->x > screen_size.x) {
		size->x -= (start->x + size->x) - screen_size.x;
	}
	if (start->y > screen_size.y) {
		return false;
	} else if (start->y + size->y > screen_size.y) {
		size->y -= (start->y + size->y) - screen_size.y;
	}

	return true;
}

static inline uint8_t * get_prev_graphic_cursor_scanline(unsigned y)
{
	return &data.cursor_prev_graphic[y * GRAPHIC_CURSOR_WIDTH];
}

static inline uint16_t get_graphic_cursor_and_mask(unsigned y)
{
	return data.cursor_graphic[y];
}

static inline uint16_t get_graphic_cursor_xor_mask(unsigned y)
{
	return data.cursor_graphic[GRAPHIC_CURSOR_HEIGHT + y];
}

static void hide_graphic_cursor(void)
{
	uint8_t __far *pixel;
	uint8_t *cursor_prev;
	struct point start, size, offset;
	unsigned pixels_per_byte, y, x;

	// Compute the area where the cursor is currently positioned
	if (!get_graphic_cursor_area(&data.cursor_pos, &start, &size, &offset)) {
		return;
	}

	switch (data.screen_mode) {
	case 4:
	case 5:
	case 6: // CGA modes
		pixels_per_byte = data.screen_mode == 0x6 ? 8 : 4;

		for (y = 0; y < size.y; y++) {
			cursor_prev = get_prev_graphic_cursor_scanline(offset.y + y);
			pixel = get_video_scanline(data.screen_mode, data.screen_page, start.y + y)
			        + start.x / pixels_per_byte;

			// Restore this scaline from cursor_prev
			nfmemcpy(pixel, cursor_prev,
			         ((start.x % pixels_per_byte) + size.x + (pixels_per_byte - 1)) / pixels_per_byte);
		}

		break;
	}

	data.cursor_visible = false;
}

static void show_graphic_cursor(void)
{
	static const uint16_t msb_mask = 0x8000;
	uint16_t cursor_and_mask, cursor_xor_mask;
	uint8_t __far *pixel, pixel_mask;
	uint8_t *cursor_prev;
	struct point start, size, offset;
	unsigned pixels_per_byte, x, y;

	// Compute the area where the cursor is supposed to be drawn
	if (!get_graphic_cursor_area(&data.pos, &start, &size, &offset)) {
		return;
	}

	switch (data.screen_mode) {
	case 4:
	case 5:
	case 6: // CGA modes
		pixels_per_byte = data.screen_mode == 0x6 ? 8 : 4;

		for (y = 0; y < size.y; y++) {
			cursor_and_mask = get_graphic_cursor_and_mask(offset.y + y) << offset.x;
			cursor_xor_mask = get_graphic_cursor_xor_mask(offset.y + y) << offset.y;
			cursor_prev = get_prev_graphic_cursor_scanline(offset.y + y);
			pixel = get_video_scanline(data.screen_mode, data.screen_page, start.y + y)
			        + start.x / pixels_per_byte;

			// First copy this scanline to cursor_prev before any changes
			fnmemcpy(cursor_prev, pixel,
			         ((start.x % pixels_per_byte) + size.x + (pixels_per_byte - 1)) / pixels_per_byte);

			// pixel points the previous multiple of pixels_per_byte;
			// now advance to the start of cursor while updating the pixel_mask
			pixel_mask = pixels_per_byte == 8 ? 0x80 : 0xC0;
			for (x = 0; x < (start.x % pixels_per_byte); x++) {
				pixel_mask >>= 8 / pixels_per_byte;
			}

			// Now pixel points to the start of cursor
			for (; x < (start.x % pixels_per_byte) + size.x; x++) {
				uint8_t rest = *pixel & ~pixel_mask;

				if (!(cursor_and_mask & msb_mask)) {
					*pixel = rest;
				}
				if (cursor_xor_mask & msb_mask) {
					*pixel = rest | (*pixel ^ 0xFF) & pixel_mask;
				}

				// Advance to the next pixel
				if (x % pixels_per_byte == pixels_per_byte - 1) {
					// Next iteration starts new byte, reload pixel_mask
					pixel++;
					pixel_mask = pixels_per_byte == 8 ? 0x80 : 0xC0;
				} else {
					pixel_mask >>= 8 / pixels_per_byte;
				}

				// Advance to the next bit in the cursor mask
				cursor_and_mask <<= 1;
				cursor_xor_mask <<= 1;
			}
		}
		break;
	default:
		dlog_print("Graphic mode 0x");
		dlog_printx(data.screen_mode);
		dlog_puts(" not supported");
		return;
	}

	data.cursor_pos = data.pos;
	data.cursor_visible = true;
}

/** Refreshes cursor position and visibility. */
static void refresh_cursor(void)
{
	bool should_show = data.visible_count >= 0;

#if USE_VIRTUALBOX
	if (data.vbwantcursor) {
		int err = 0;
		if (should_show != data.cursor_visible) {
			int err = vbox_set_pointer_visible(&data.vb, should_show);
			if (err == 0 && data.vbhaveabs) {
				data.cursor_visible = should_show;
			}
		}
		if (err == 0 & data.vbhaveabs) {
			// No need to show the cursor; VirtualBox is already showing it for us.
			return;
		}
	}
#endif

	if (is_text_mode(data.screen_mode)) {
		if (data.cursor_visible) {
			// Hide the cursor at the old position if any
			hide_text_cursor();
		}
		if (should_show) {
			// Show the cursor at the new position
			show_text_cursor();
		}
	} else {
		if (data.cursor_visible) {
			hide_graphic_cursor();
		}
		if (should_show) {
			show_graphic_cursor();
		}
	}
}

/** Forcefully hides the mouse cursor if shown. */
static void hide_cursor(void)
{
#if USE_VIRTUALBOX
	if (data.vbwantcursor) {
		vbox_set_pointer_visible(&data.vb, false);
		if (data.vbhaveabs) {
			data.cursor_visible = false;
		}
	}
#endif

	if (is_text_mode(data.screen_mode)) {
		if (data.cursor_visible) {
			hide_text_cursor();
		}
	} else {
		if (data.cursor_visible) {
			hide_graphic_cursor();
		}
	}
}

static void load_cursor(void)
{
#if USE_VIRTUALBOX
	if (data.vbwantcursor) {
		VMMDevReqMousePointer *req = (VMMDevReqMousePointer *) data.vb.buf;
		const unsigned width = GRAPHIC_CURSOR_WIDTH, height = GRAPHIC_CURSOR_HEIGHT;
		const unsigned and_mask_size = (width + 7) / 8 * height;
		const unsigned xor_mask_size = width * height * 4;
		const unsigned data_size = and_mask_size + xor_mask_size;
		const unsigned full_size = MAX(sizeof(VMMDevReqMousePointer), 24 + 20 + data_size);
		unsigned int offset = 0, y, x;

		bzero(req, full_size);

		req->header.size = full_size;
		req->header.version = VMMDEV_REQUEST_HEADER_VERSION;
		req->header.requestType = VMMDevReq_SetPointerShape;
		req->header.rc = -1;

		req->fFlags = VBOX_MOUSE_POINTER_SHAPE;
		req->xHot = BOUND(data.cursor_hotspot.x, 0, width);
		req->yHot = BOUND(data.cursor_hotspot.y, 0, height);
		req->width = width;
		req->height = height;

		// Just byteswap the AND mask
		for (y = 0; y < height; ++y) {
			uint16_t line = get_graphic_cursor_and_mask(y);
			req->pointerData[(y*2)]   = (line >> 8) & 0xFF;
			req->pointerData[(y*2)+1] = line & 0xFF;
		}
		offset += and_mask_size;

		// But the XOR mask needs to be converted to huge 4-byte "RGBA" format.
		for (y = 0; y < height; ++y) {
			uint16_t line = get_graphic_cursor_xor_mask(y);

			for (x = 0; x < width; ++x) {
				unsigned int pos = offset + (y * width * 4) + (x*4) + 0;
				uint8_t val = (line & 0x8000) ? 0xFF : 0;

				req->pointerData[pos + 0] = val;
				req->pointerData[pos + 1] = val;
				req->pointerData[pos + 2] = val;
				req->pointerData[pos + 3] = 0;

				line <<= 1;
			}
		}

		dlog_puts("Loading cursor to VBox");

		vbox_send_request(data.vb.iobase, data.vb.buf_physaddr);

		if (req->header.rc != 0) {
			dlog_puts("Could not send cursor to VirtualBox");
		}

		// After we send this message, it looks like VirtualBox shows the cursor
		// even if we didn't actually want it to be visible at this point.
		// Mark it as visible so that refresh_cursor() will rehide it if necessary.
		data.cursor_visible = true;
		refresh_cursor();
	}
#endif
}

static void refresh_video_info(void)
{
	uint8_t screen_columns = bda_get_num_columns();
	uint8_t mode = bda_get_video_mode() & ~0x80;
	bool mode_change = mode != data.screen_mode;

	if (mode_change && data.cursor_visible) {
		// Assume cursor is lost
		data.cursor_visible = false;
	}

	dlog_print("Current video mode=");
	dlog_printx(mode);
	dlog_print(" with cols=");
	dlog_printd(screen_columns);
	dlog_endline();

	data.screen_mode = mode;
	data.screen_page = bda_get_cur_video_page();
	data.screen_scale.x = 1;
	data.screen_scale.y = 1;

	// Compute screen coordinates which are used to events from
	// absolute mouse coordinates.
	switch (mode) {
	case 0:
	case 1:
	case 2:
	case 3: // CGA text modes with 25 rows and variable columns
	case 7: // MDA Mono text mode
		data.screen_max.x = (screen_columns * 8) - 1;
		data.screen_max.y = (25 * 8) - 1;
		break;

	case 4:
	case 5: // CGA low-res modes
	case 0xd: // EGA low-res mode
		data.screen_max.x = 640 - 1;
		data.screen_max.y = 200 - 1;
		data.screen_scale.x = 2; // Really 320x200
		break;

	case 6: // CGA hi-res mode
	case 0xe: // EGA modes
	case 0x13: // VGA 256color mode
		data.screen_max.x = 640 - 1;
		data.screen_max.y = 200 - 1;
		break;

	case 0xf:
	case 0x10: // EGA 640x350 modes
		data.screen_max.x = 640 - 1;
		data.screen_max.y = 350 - 1;
		break;

	case 0x11:
	case 0x12: // VGA 640x480 modes
		data.screen_max.x = 640 - 1;
		data.screen_max.y = 480 - 1;
		break;

	default:
		// Unknown mode; assume default coordinates
		// Note that if program sets up larger window coordinates, we'll use that instead.
		data.screen_max.x = 640 - 1;
		data.screen_max.y = 200 - 1;
		break;
	}
}

static void call_event_handler(void (__far *handler)(), uint16_t events,
                               uint16_t buttons, int16_t x, int16_t y,
                               int16_t delta_x, int16_t delta_y)
{
#if TRACE_EVENT
	dlog_print("calling event handler events=");
	dlog_printx(events);
	dlog_print(" buttons=");
	dlog_printx(buttons);
	dlog_print(" x=");
	dlog_printd(x);
	dlog_print(" y=");
	dlog_printd(y);
	dlog_print(" dx=");
	dlog_printd(delta_x);
	dlog_print(" dy=");
	dlog_printd(delta_y);
	dlog_endline();
#endif

	__asm {
		mov ax, [events]
		mov bx, [buttons]
		mov cx, [x]
		mov dx, [y]
		mov si, [delta_x]
		mov di, [delta_y]

		call dword ptr [handler]
	}
}

static void handle_mouse_event(uint16_t buttons, bool absolute, int x, int y, int z)
{
	uint16_t events = 0;
	int i;

#if TRACE_EVENT
	dlog_print("handle mouse event");
	if (absolute) dlog_print(" absolute");
	dlog_print(" buttons=");
	dlog_printx(buttons);
	dlog_print(" x=");
	dlog_printd(x);
	dlog_print(" y=");
	dlog_printd(y);
	dlog_print(" z=");
	dlog_printd(z);
	dlog_endline();
#endif

	if (absolute) {
		// Absolute movement: x,y are in screen pixels units
		events |= INT33_EVENT_MASK_ABSOLUTE;

		if (x != data.pos.x || y != data.pos.y) {
			events |= INT33_EVENT_MASK_MOVEMENT;

			// Simulate a fake relative movement in mickeys
			// This is almost certainly broken.
			// Programs that expect relative movement data
			// will almost never set a mickeyPerPixel value.
			// So all we can do is guess.
			data.delta.x += (x - data.pos.x) * 8;
			data.delta.y += (y - data.pos.y) * 8;

			// Store the new absolute position
			data.pos.x = x;
			data.pos.y = y;
			data.pos_frac.x = 0;
			data.pos_frac.y = 0;
		}
	} else if (x || y) {
		// Relative movement: x,y are in mickeys
		uint16_t ticks = bda_get_tick_count_lo();
		unsigned ax = ABS(x), ay = ABS(y);

		events |= INT33_EVENT_MASK_MOVEMENT;

		// Check if around one second has passed
		if ((ticks - data.last_ticks) >= 18) {
			data.total_motion = 0;
			data.last_ticks = ticks;
		}

		// If more than the double speed threshold has been moved in the last second,
		// double the speed
		data.total_motion += ax * ax + ay * ay;
		if (data.total_motion > data.doubleSpeedThreshold * data.doubleSpeedThreshold) {
			x *= 2;
			y *= 2;
		}

		data.delta.x += x;
		data.delta.y += y;
		data.delta_frac.x = 0;
		data.delta_frac.y = 0;

		// Convert mickeys into pixels
		data.pos.x += scalei_rem(x, data.mickeysPerLine.x, 8, &data.pos_frac.x);
		data.pos.y += scalei_rem(y, data.mickeysPerLine.y, 8, &data.pos_frac.y);
	}

	bound_position_to_window();

	if (data.haswheel && z) {
		events |= INT33_EVENT_MASK_WHEEL_MOVEMENT;
		// Higher byte of buttons contains wheel movement
		buttons |= (z & 0xFF) << 8;
	}

	// Update button status
	for (i = 0; i < NUM_BUTTONS; ++i) {
		uint8_t btn = 1 << i;
		uint8_t evt = 0;
		if ((buttons & btn) && !(data.buttons & btn)) {
			// Button pressed
			evt = 1 << (1 + (i * 2)); // Press event mask
			data.button[i].pressed.count++;
			data.button[i].pressed.last.x = data.pos.x;
			data.button[i].pressed.last.y = data.pos.y;
		} else if (!(buttons & btn) && (data.buttons & btn)) {
			// Button released
			evt = 1 << (2 + (i * 2)); // Release event mask
			data.button[i].released.count++;
			data.button[i].released.last.x = data.pos.x;
			data.button[i].released.last.y = data.pos.y;
		}
		events |= evt;
	}
	data.buttons = buttons;

	refresh_cursor();

	events &= data.event_mask;
	if (data.event_handler && events) {
		call_event_handler(data.event_handler, events,
		                   buttons, data.pos.x, data.pos.y, data.delta.x, data.delta.y);
	}
}

static void __far ps2_mouse_callback(uint8_t status, uint8_t x, uint8_t y, uint8_t z)
{
#pragma aux (PS2_CB) ps2_mouse_callback

	int sx =   status & PS2M_STATUS_X_NEG ? 0xFF00 | x : x;
	int sy = -(status & PS2M_STATUS_Y_NEG ? 0xFF00 | y : y);
	int sz = z;
	bool abs = false;

#if TRACE_EVENT
	dlog_print("ps2 callback status=");
	dlog_printx(status);
	dlog_print(" sx=");
	dlog_printd(sx);
	dlog_print(" sy=");
	dlog_printd(sy);
	dlog_print(" sz=");
	dlog_printd(z);
	dlog_endline();
#endif

#if USE_VIRTUALBOX
	if (data.vbavail) {
		uint16_t vbx, vby;
		if ((vbox_get_mouse(&data.vb, &abs, &vbx, &vby) == 0) && abs) {
			sx = scaleu(vbx, 0xFFFFU, MAX(data.max.x, data.screen_max.x));
			sy = scaleu(vby, 0xFFFFU, MAX(data.max.y, data.screen_max.y));
			data.vbhaveabs = true;
		} else {
			// VirtualBox does not support absolute coordinates,
			// or user has disabled them.
			data.vbhaveabs = false;
		}
	}
#endif

	// VirtualBox/Bochs BIOS does not pass wheel data to the callback,
	// so we will fetch it directly from the BIOS data segment.
	if (data.haswheel && !sz) {
		int8_t __far * mouse_packet = MK_FP(bda_get_ebda_segment(), 0x28);
		sz = mouse_packet[3];
	}

	handle_mouse_event(status & (PS2M_STATUS_BUTTON_1 | PS2M_STATUS_BUTTON_2 | PS2M_STATUS_BUTTON_3),
	                   abs, sx, sy, sz);
}

#if USE_VIRTUALBOX
static void enable_vbox_absolute(bool enable)
{
	data.vbhaveabs = false;

	if (data.vbavail) {
		int err = vbox_set_mouse(&data.vb, enable, false);
		if (enable && !err) {
			dlog_puts("VBox absolute mouse enabled");
			data.vbhaveabs = true;
		} else if (!enable) {
			dlog_puts("VBox absolute mouse disabled");
		}
	}
}
#endif

static void reset_mouse_hardware()
{
	ps2m_enable(false);

#if USE_VIRTUALBOX
	// By default, enable the integration
	enable_vbox_absolute(true);
	load_cursor();
#endif

	if (data.usewheel && ps2m_detect_wheel()) {
		// Detect wheel also reinitializes the mouse to the proper packet size
		data.haswheel = true;
	} else {
		// Otherwise do an extra reset to return back to initial state, just in case
		data.haswheel = false;
		ps2m_init(PS2M_PACKET_SIZE_PLAIN);
	}

	ps2m_set_resolution(3);     // 3 = 200 dpi, 8 counts per millimeter
	ps2m_set_sample_rate(4);    // 4 = 80 reports per second
	ps2m_set_scaling_factor(1); // 1 = 1:1 scaling

	ps2m_set_callback(ps2_mouse_callback);

	ps2m_enable(true);
}

static void reset_mouse_settings()
{
	data.event_mask = 0;
	data.event_handler = 0;

	data.mickeysPerLine.x = 8;
	data.mickeysPerLine.y = 16;
	data.doubleSpeedThreshold = 64;
	data.min.x = 0;
	data.max.x = data.screen_max.x;
	data.min.y = 0;
	data.max.y = data.screen_max.y;
	data.visible_count = -1;
	data.cursor_text_type = 0;
	data.cursor_text_and_mask = 0xFFFFU;
	data.cursor_text_xor_mask = 0x7700U;
	data.cursor_hotspot.x = 0;
	data.cursor_hotspot.y = 0;
	nnmemcpy(data.cursor_graphic, default_cursor_graphic, sizeof(data.cursor_graphic));

	refresh_cursor(); // This will hide the cursor and update data.cursor_visible
}

static void reset_mouse_state()
{
	int i;
	data.pos.x = data.min.x;
	data.pos.y = data.min.y;
	data.pos_frac.x = 0;
	data.pos_frac.y = 0;
	data.delta.x = 0;
	data.delta.y = 0;
	data.delta_frac.x = 0;
	data.delta_frac.y = 0;
	data.buttons = 0;
	for (i = 0; i < NUM_BUTTONS; i++) {
		data.button[i].pressed.count = 0;
		data.button[i].pressed.last.x = 0;
		data.button[i].pressed.last.y = 0;
		data.button[i].released.count = 0;
		data.button[i].released.last.x = 0;
		data.button[i].released.last.y = 0;
	}
	data.cursor_visible = false;
	data.cursor_pos.x = 0;
	data.cursor_pos.y = 0;
	data.cursor_prev_char = 0;
	bzero(data.cursor_prev_graphic, sizeof(data.cursor_prev_graphic));
}

static void return_clear_button_counter(union INTPACK __far *r, struct buttoncounter *c)
{
	r->x.cx = c->last.x;
	r->x.dx = c->last.y;
	r->x.bx = c->count;
	c->last.x = 0;
	c->last.y = 0;
	c->count = 0;
}

static void int33_handler(union INTPACK r)
#pragma aux int33_handler "*" parm caller [] modify [ax bx cx dx es]
{
	switch (r.x.ax) {
	case INT33_RESET_MOUSE:
		dlog_puts("Mouse reset");
		refresh_video_info();
		reset_mouse_settings();
		reset_mouse_hardware();
		reset_mouse_state();
		r.x.ax = INT33_MOUSE_FOUND;
		r.x.bx = NUM_BUTTONS;
		break;
	case INT33_SHOW_CURSOR:
		data.visible_count++;
		refresh_cursor();
		break;
	case INT33_HIDE_CURSOR:
		data.visible_count--;
		refresh_cursor();
		break;
	case INT33_GET_MOUSE_POSITION:
		r.x.cx = data.pos.x;
		r.x.dx = data.pos.y;
		r.x.bx = data.buttons;
		break;
	case INT33_SET_MOUSE_POSITION:
		data.pos.x = r.x.cx;
		data.pos.y = r.x.dx;
		data.pos_frac.x = 0;
		data.pos_frac.y = 0;
		data.delta.x = 0;
		data.delta.y = 0;
		data.delta_frac.x = 0;
		data.delta_frac.y = 0;
		bound_position_to_window();
		break;
	case INT33_GET_BUTTON_PRESSED_COUNTER:
		r.x.ax = data.buttons;
		return_clear_button_counter(&r, &data.button[MIN(r.x.bx, NUM_BUTTONS - 1)].pressed);
		break;
	case INT33_GET_BUTTON_RELEASED_COUNTER:
		r.x.ax = data.buttons;
		return_clear_button_counter(&r, &data.button[MIN(r.x.bx, NUM_BUTTONS - 1)].released);
		break;
	case INT33_SET_HORIZONTAL_WINDOW:
		dlog_print("Mouse set horizontal window [");
		dlog_printd(r.x.cx);
		dlog_putc(',');
		dlog_printd(r.x.dx);
		dlog_puts("]");
		// Recheck in case someone changed the video mode
		refresh_video_info();
		data.min.x = r.x.cx;
		data.max.x = r.x.dx;
		bound_position_to_window();
		break;
	case INT33_SET_VERTICAL_WINDOW:
		dlog_print("Mouse set vertical window [");
		dlog_printd(r.x.cx);
		dlog_putc(',');
		dlog_printd(r.x.dx);
		dlog_puts("]");
		refresh_video_info();
		data.min.y = r.x.cx;
		data.max.y = r.x.dx;
		bound_position_to_window();
		break;
	case INT33_SET_GRAPHICS_CURSOR:
		dlog_puts("Mouse set graphics cursor");
		hide_cursor();
		data.cursor_hotspot.x = r.x.bx;
		data.cursor_hotspot.y = r.x.cx;
		fnmemcpy(data.cursor_graphic, MK_FP(r.x.es, r.x.dx), 64);
		load_cursor();
		refresh_cursor();
		break;
	case INT33_SET_TEXT_CURSOR:
		dlog_print("Mouse set text cursor ");
		dlog_printd(r.x.bx);
		dlog_endline();
		hide_cursor();
		data.cursor_text_type = r.x.bx;
		data.cursor_text_and_mask = r.x.cx;
		data.cursor_text_xor_mask = r.x.dx;
		refresh_cursor();
		break;
	case INT33_GET_MOUSE_MOTION:
		r.x.cx = data.delta.x;
		r.x.dx = data.delta.y;
		data.delta.x = 0;
		data.delta.y = 0;
#if USE_VIRTUALBOX
		// Likely this means we need a relative mouse, or we will get out of sync
		//if (data.vbabs) enable_vbox_absolute(false);
#endif
		break;
	case INT33_SET_EVENT_HANDLER:
		dlog_puts("Mouse set event handler");
		data.event_mask = r.x.cx;
		data.event_handler = MK_FP(r.x.es, r.x.dx);
		break;
	case INT33_SET_MOUSE_SPEED:
		dlog_print("Mouse set speed x=");
		dlog_printd(r.x.cx);
		dlog_print(" y=");
		dlog_printd(r.x.dx);
		dlog_endline();
		data.mickeysPerLine.x = r.x.cx;
		data.mickeysPerLine.y = r.x.dx;
		break;
	case INT33_SET_SPEED_DOUBLE_THRESHOLD:
		dlog_print("Mouse set speed double threshold=");
		dlog_printd(r.x.dx);
		dlog_endline();
		data.doubleSpeedThreshold = r.x.dx;
		break;
	case INT33_EXCHANGE_EVENT_HANDLER:
		dlog_puts("Mouse exchange event handler");
		data.event_mask = r.x.cx;
	    {
		    void (__far *prev_event_handler)() = data.event_handler;
			data.event_handler = MK_FP(r.x.es, r.x.dx);
			r.x.es = FP_SEG(prev_event_handler);
			r.x.dx = FP_OFF(prev_event_handler);
	    }
		break;
	case INT33_GET_MOUSE_STATUS_SIZE:
		dlog_puts("Mouse get status size");
		r.x.bx = sizeof(TSRDATA);
		break;
	case INT33_SAVE_MOUSE_STATUS:
		dlog_puts("Mouse save status");
		nfmemcpy(MK_FP(r.x.es, r.x.dx), &data, sizeof(TSRDATA));
		break;
	case INT33_LOAD_MOUSE_STATUS:
		dlog_puts("Mouse load status");
		fnmemcpy(&data, MK_FP(r.x.es, r.x.dx), sizeof(TSRDATA));
		break;
	case INT33_SET_MOUSE_SENSITIVITY:
		dlog_print("Mouse set speed x=");
		dlog_printd(r.x.bx);
		dlog_print(" y=");
		dlog_printd(r.x.cx);
		dlog_print(" threshold=");
		dlog_printd(r.x.dx);
		dlog_endline();
		data.mickeysPerLine.x = r.x.bx;
		data.mickeysPerLine.y = r.x.cx;
		data.doubleSpeedThreshold = r.x.dx;
		break;
	case INT33_GET_MOUSE_SENSITIVITY:
		r.x.bx = data.mickeysPerLine.x;
		r.x.cx = data.mickeysPerLine.y;
		r.x.dx = data.doubleSpeedThreshold;
		break;
	case INT33_RESET_SETTINGS:
		dlog_puts("Mouse reset settings");
		refresh_video_info();
		reset_mouse_settings();
		reset_mouse_state();
		r.x.ax = INT33_MOUSE_FOUND;
		r.x.bx = NUM_BUTTONS;
		break;
	case INT33_GET_LANGUAGE:
		r.x.bx = 0;
		break;
	case INT33_GET_DRIVER_INFO:
		dlog_puts("Mouse get driver info");
		r.h.bh = REPORTED_VERSION_MAJOR;
		r.h.bl = REPORTED_VERSION_MINOR;
		r.h.ch = INT33_MOUSE_TYPE_PS2;
		r.h.cl = 0;
		break;
	case INT33_GET_MAX_COORDINATES:
		r.x.cx = data.screen_max.x;
		r.x.dx = data.screen_max.y;
		break;
	case INT33_GET_WINDOW:
		r.x.ax = data.min.x;
		r.x.bx = data.min.y;
		r.x.cx = data.max.x;
		r.x.dx = data.max.y;
		break;
	case INT33_GET_TSR_DATA:
		dlog_puts("Get TSR data");
		r.x.es = FP_SEG(&data);
		r.x.di = FP_OFF(&data);
		break;
	default:
		dlog_print("Unknown mouse function ax=");
		dlog_printx(r.x.ax);
		dlog_endline();
		break;
	}
}

// Can't use __interrupt, it makes a call to GETDS on the runtime
void __declspec(naked) __far int33_isr(void)
{
	__asm {
		pusha
		push ds
		push es
		push fs
		push gs

		mov bp, sp
		push cs
		pop ds

		call int33_handler

		pop gs
		pop fs
		pop es
		pop ds
		popa
		iret
	}
}

#if USE_INT2F
static void int2f_handler(union INTPACK r)
#pragma aux int2f_handler "*" parm caller [] modify [ax bx cx dx es]
{
}

void __declspec(naked) __far int2f_isr(void)
{
	__asm {
		; Space for the pointer to the next ISR in the chain
		push dword ptr 0

		; Save all registers (also acts as the INTPACK paramenter later)
		pusha
		push ds
		push es
		push fs
		push gs

		mov bp, sp
		push cs
		pop ds

		; Load the address of the next ISR in the chain
		; Stack looks like sp+0:  gs, fs, es, ds (4*2bytes)
		;                  sp+8:  pusha (8 * 2bytes)
		;				   sp+24: dword ptr (the space we reserved to store the next ISR)
		mov ax, word ptr [data + 4] ; i.e. data.prev_int2f_handler -- Watcom doesn't support structs
		mov [bp + 24], ax
		mov ax, word ptr [data + 6] ; i.e. data.prev_int2f_handler[2]
		mov [bp + 26], ax

		; Now call our handler
		call int2f_handler

		pop gs
		pop fs
		pop es
		pop ds
		popa

		; This will jump to the address of the next ISR we loaded before
		retf
	}
}
#endif

static LPTSRDATA int33_get_tsr_data(void);
#pragma aux int33_get_tsr_data = \
	"xor ax, ax" \
	"mov es, ax" \
	"mov di, ax" \
	"mov ax, 0x7f" \
	"int 0x33"   \
	__value [es di] \
	__modify [ax]

static LPTSRDATA local_get_tsr_data(void);
#pragma aux local_get_tsr_data = \
	"mov ax, cs" \
	"mov es, ax" \
	"mov di, offset data" \
	__value [es di] \
	__modify [ax]

LPTSRDATA __far get_tsr_data(bool installed)
{
	if (installed) {
		return int33_get_tsr_data();
	} else {
		return local_get_tsr_data();
	}
}

int resident_end;
