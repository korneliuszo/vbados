/*
 * VBMouse - win16 mouse driver entry points
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

#include <string.h>
#include <limits.h>
#include <windows.h>

#include "utils.h"
#include "int33.h"
#include "int21dos.h"
#include "int2fwin.h"
#include "mousew16.h"

/** Whether to enable wheel mouse handling. */
#define USE_WHEEL 1
/** If win386 is available, hook int2f to detect DOS/Windows screen switches
 *  and reconfigure the int33 driver. */
#define USE_WIN386 1
/** Verbosely log events as they happen. */
#define TRACE_EVENTS 0
/** Verbosely trace scroll wheel code. */
#define TRACE_WHEEL 0
/** Number of lines to scroll per wheel event. */
#define WHEEL_SCROLL_LINES 2

/** Windows 3.x only supports 1-2 mouse buttons anyway. */
#define MOUSE_NUM_BUTTONS 2

/** Prefix for debug messages. */
#define DPREFIX "mousew16: "

/** The routine Windows gave us which we should use to report events. */
static LPFN_MOUSEEVENT eventproc;
/** Current status of the driver. */
static struct {
	/** Whether the driver is currently enabled. */
	bool enabled : 1;
#if USE_WHEEL
	/** Whether a mouse wheel was detected. */
	bool wheel : 1;
#endif
#if USE_WIN386
	/** Whether we are running under windows 386. */
	bool has_win386 : 1;
#endif
} flags;
/** Previous deltaX, deltaY from the int33 mouse callback (for relative motion) */
static short prev_delta_x, prev_delta_y;
/** Maximum X and Y coordinates expected from the int33 driver. */
static unsigned short max_x, max_y;
#if USE_WHEEL
/** Some delay-loaded functions from USER.EXE to provide wheel mouse events. */
static struct {
	HINSTANCE hUser;
	void WINAPI (*GetCursorPos)( POINT FAR * );
	HWND WINAPI (*WindowFromPoint)( POINT );
	HWND WINAPI (*GetParent)( HWND );
	int WINAPI  (*GetClassName)( HWND, LPSTR, int );
	int WINAPI  (*GetWindowText)( HWND, LPSTR, int );
	LONG WINAPI (*GetWindowLong)( HWND, int );
	BOOL WINAPI (*IsWindowEnabled)( HWND );
	BOOL WINAPI (*EnumChildWindows)( HWND, WNDENUMPROC, LPARAM );
	BOOL WINAPI (*PostMessage)( HWND, UINT, WPARAM, LPARAM );
} userapi;
#endif
#if USE_WIN386
/** Previous int2f handler. */
static LPFN prev_int2f_handler;
#endif

/* This is how events are delivered to Windows */

static void send_event(unsigned short Status, short deltaX, short deltaY, short ButtonCount, short extra1, short extra2);
#pragma aux (MOUSEEVENTPROC) send_event = \
	"call dword ptr [eventproc]"

/* Our "CALLBACKS" segment which is fixed and non-relocatable. */

#pragma code_seg ( "CALLBACKS" )

#include "dlog.h"

#if USE_WHEEL
typedef struct {
	/** Input: whether to find vertical scrollbars. */
	BOOL vertical;
	/** Output: found scrollbar handle, or 0. */
	HWND scrollbar;
} FINDSCROLLBARDATA, FAR * LPFINDSCROLLBARDATA;

#define ENUM_CHILD_WINDOW_CONTINUE TRUE
#define ENUM_CHILD_WINDOW_STOP     FALSE

#if TRACE_WHEEL
static void print_window_name(HWND hWnd)
{
	char buffer[32];
	if (userapi.GetWindowText(hWnd, buffer, sizeof(buffer)) > 0) {
		dprintf(DPREFIX "hWnd=0x%x name=%s\n", hWnd, buffer);
	}
}
#endif

/** Helper function to traverse a window hierarchy and find a candidate scrollbar. */
static BOOL CALLBACK __loadds find_scrollbar(HWND hWnd, LPARAM lParam)
{
	LPFINDSCROLLBARDATA data = (LPFINDSCROLLBARDATA) lParam;
	char buffer[16];

#if TRACE_WHEEL
	print_window_name(hWnd);
#endif

	if (!userapi.IsWindowEnabled(hWnd)) {
		return ENUM_CHILD_WINDOW_CONTINUE;
	}

	if (userapi.GetClassName(hWnd, buffer, sizeof(buffer)) == 0) {
		return ENUM_CHILD_WINDOW_CONTINUE;
	}

	if (_fstrcmp(buffer, "ScrollBar") == 0) {
		LONG style = userapi.GetWindowLong(hWnd, GWL_STYLE);

		if (data->vertical && (style & SBS_VERT)) {
			data->scrollbar = hWnd;
			return ENUM_CHILD_WINDOW_STOP;
		} else if (!data->vertical && !(style & SBS_VERT)) {
			data->scrollbar = hWnd;
			return ENUM_CHILD_WINDOW_STOP;
		}
	}

	return ENUM_CHILD_WINDOW_CONTINUE;
}

/** Send scrolling messages to given window.
 *  @param hWnd window to scroll.
 *  @param vertical true if vertical, false if horizontal.
 *  @param z number of lines to scroll.
 *  @param hScrollBar corresponding scrollbar handle.
 */
static void post_scroll_msg(HWND hWnd, BOOL vertical, int z, HWND hScrollBar)
{
	UINT msg = vertical ? WM_VSCROLL : WM_HSCROLL;
	WPARAM wParam = z < 0 ? SB_LINEUP : SB_LINEDOWN;
	LPARAM lParam = MAKELPARAM(0, hScrollBar);
	UINT i, lines = (z < 0 ? -z : z) * WHEEL_SCROLL_LINES;

#if TRACE_WHEEL
	dprintf("w16mouse: sending scroll msg to hWnd=0x%x from=0x%x vert=%d lines=%u\n", hWnd, hScrollBar, vertical, lines);
#endif

	for (i = 0; i < lines; i++) {
		userapi.PostMessage(hWnd, msg, wParam, lParam);
	}
	userapi.PostMessage(hWnd, msg, SB_ENDSCROLL, lParam);
}

/** Send wheel scrolling events to the most likely candidate window. */
static void send_wheel_movement(int8_t z)
{
	POINT point;
	HWND hWnd;

#if TRACE_WHEEL
	dprintf("w16mouse: wheel=%d\n", z);
#endif

	// TODO It's highly unlikely that we can call this many functions from
	// an interrupt handler without causing a re-entrancy issue somewhere.
	// Likely it would be better to just move all of this into a hook .DLL

	// Find current window below the mosue cursor
	userapi.GetCursorPos(&point);
	hWnd = userapi.WindowFromPoint(point);

#if TRACE_WHEEL
	dprintf("w16mouse: initial hWnd=0x%x\n", hWnd);
#endif

	while (hWnd) {
		LONG style = userapi.GetWindowLong(hWnd, GWL_STYLE);

#if TRACE_WHEEL
		print_window_name(hWnd);
		dprintf(DPREFIX "hWnd=0x%x style=0x%lx\n", hWnd, style);
#endif

		if (style & WS_VSCROLL) {
#if TRACE_WHEEL
			dprintf(DPREFIX "found WS_VSCROLL\n");
#endif
			post_scroll_msg(hWnd, TRUE, z, 0);
			break;
		} else if (style & WS_HSCROLL) {
#if TRACE_WHEEL
			dprintf(DPREFIX "found WS_HSCROLL\n");
#endif
			post_scroll_msg(hWnd, FALSE, z, 0);
			break;
		} else {
			FINDSCROLLBARDATA data;

			// Let's check if we can find a vertical scroll bar in this window..
#if TRACE_WHEEL
			dprintf(DPREFIX "find vertical scrollbar...\n");
#endif
			data.vertical = TRUE;
			data.scrollbar = 0;
			userapi.EnumChildWindows(hWnd, find_scrollbar, (LONG) (LPVOID) &data);
			if (data.scrollbar) {
				post_scroll_msg(hWnd, TRUE, z, data.scrollbar);
				break;
			}

			// Try a horizontal scrollbar now
#if TRACE_WHEEL
			dprintf(DPREFIX "find horizontal scrollbar...\n");
#endif
			data.vertical = FALSE;
			data.scrollbar = 0;
			userapi.EnumChildWindows(hWnd, find_scrollbar, (LONG) (LPVOID) &data);
			if (data.scrollbar) {
				post_scroll_msg(hWnd, FALSE, z, data.scrollbar);
				break;
			}

			// Otherwise, try again on the parent window
			if (style & WS_CHILD) {
#if TRACE_WHEEL
				dprintf(DPREFIX "go into parent...\n");
#endif
				hWnd = userapi.GetParent(hWnd);
			} else {
				// This was already a topmost window
				break;
			}
		}
	}

#if TRACE_WHEEL
	dprintf(DPREFIX "wheel end\n");
#endif
}
#endif /* USE_WHEEL */

/** Called by the int33 mouse driver. */
static void FAR int33_mouse_callback(uint16_t events, uint16_t buttons, int16_t x, int16_t y, int16_t delta_x, int16_t delta_y)
#pragma aux (INT33_CB) int33_mouse_callback
{
	int status = 0;

#if TRACE_EVENTS
	dprintf(DPREFIX "events=0x%x buttons=0x%x x=%d y=%d dx=%d dy=%d\n",
	        events, buttons, x, y, delta_x, delta_y);
#endif

	if (events & INT33_EVENT_MASK_LEFT_BUTTON_PRESSED)   status |= SF_B1_DOWN;
	if (events & INT33_EVENT_MASK_LEFT_BUTTON_RELEASED)  status |= SF_B1_UP;
	if (events & INT33_EVENT_MASK_RIGHT_BUTTON_PRESSED)  status |= SF_B2_DOWN;
	if (events & INT33_EVENT_MASK_RIGHT_BUTTON_RELEASED) status |= SF_B2_UP;

	if (events & INT33_EVENT_MASK_MOVEMENT) {
		status |= SF_MOVEMENT;
	}

#if USE_WHEEL
	if (flags.wheel && (events & INT33_EVENT_MASK_WHEEL_MOVEMENT)) {
		// If wheel API is enabled, higher byte of buttons contains wheel movement
		int8_t z = (buttons & 0xFF00) >> 8;
		if (z) {
			send_wheel_movement(z);
		}
	}
#endif

	if (events & INT33_EVENT_MASK_ABSOLUTE) {
		status |= SF_ABSOLUTE;

		// Clip the coordinates, we don't want any overflow error below...
		if (x < 0) x = 0;
		if (y < 0) y = 0;
		if (x > max_x) x = max_x;
		if (y > max_y) y = max_y;

		x = scaleu(x, max_x, 0xFFFF);
		y = scaleu(y, max_y, 0xFFFF);
	} else {
		// Prefer to use mickeys for relative motion if we don't have absolute data
		x = delta_x - prev_delta_x;
		y = delta_y - prev_delta_y;

		prev_delta_x = delta_x;
		prev_delta_y = delta_y;
	}

	// Unused
	(void) buttons;

	if ((status & ~SF_ABSOLUTE) == 0) {
		return; // Nothing to post
	}

#if TRACE_EVENTS
	dprintf(DPREFIX "post status=0x%x ", status);
	if (status & SF_ABSOLUTE) {
		dprintf("x=%u y=%u\n", x, y);
	} else {
		dprintf("x=%d y=%d\n", x, y);
	}
#endif

	send_event(status, x, y, MOUSE_NUM_BUTTONS, 0, 0);
}

#if USE_WIN386
static void display_switch_handler(int function)
#pragma aux display_switch_handler parm caller [ax] modify [ax bx cx dx si di]
{
	if (!flags.enabled) {
		return;
	}

	switch (function) {
	case INT2F_NOTIFY_BACKGROUND_SWITCH:
		dputs(DPREFIX "windows going background\n");

		break;
	case INT2F_NOTIFY_FOREGROUND_SWITCH:
		dputs(DPREFIX "windows going foreground, reconfiguring int33 driver\n");

		// Normally, we wouldn't need to do anything on a foreground switch
		// since Windows (VMD) would transparently switch to the int33 driver instance
		// corresponding to the Windows VM.
		// i.e. the state of the int33 driver should be transparently restored for us.
		// However, with some display drivers, the BIOS screen mode may not be completely
		// restored when Windows goes fullscreen, and this may confuse the int33 driver
		// into forgetting the window size, even if the int33 driver state was restored.
		// To avoid this, let's re-set the window size each time Windows goes fullscreen.

		// Additionally, the builtin int33 driver of some emulators cannot be instanced
		// by VMD (because these drivers effectively exist outside the emulated machine),
		// in which case the int33 driver is going to be "shared" between DOS and Windows,
		// resulting in loss of mouse function in Windows whenever a DOS VM is focused.
		// For these cases, let's just steal control from the DOS program when coming
		// back into Windows (i.e. let's set the event handler to our own).
		// The DOS program will likely lose mouse functionality at this point, but
		// at least it's better than Windows losing control of the mouse permanently.

		// Reset the window size...
		int33_set_mouse_speed(1, 1);
		int33_set_horizontal_window(0, max_x);
		int33_set_vertical_window(0, max_y);
		// and the event handler, but nothing else.
		int33_set_event_handler(INT33_EVENT_MASK_ALL, int33_mouse_callback);

		break;
	}
}

/** Interrupt 2F handler, which will be called on Windows 386 mode events. */
static void __declspec(naked) __far int2f_handler(void)
{
	_asm {
		; Preserve data segment
		push ds

		; Load our data segment
		push SEG prev_int2f_handler ; Let's hope that Windows relocates segments with interrupts disabled
		pop ds

		; Check functions we are interested in hooking
		cmp ax, 0x4001  ; Notify Background Switch
		je handle_it
		cmp ax, 0x4002  ; Notify Foreground Switch
		je handle_it

		; Otherwise directly jump to next handler
		jmp next_handler

	handle_it:
		pushad ; Save and restore 32-bit registers, we may clobber them from C
		call display_switch_handler
		popad

	next_handler:
		; Store the address of the previous handler
		push dword ptr [prev_int2f_handler]

		; Restore original data segment without touching the stack,
		; since we want to keep the prev handler address at the top
		push bp
		mov bp, sp
		mov ds, [bp + 6]  ; Stack looks like 0: bp, 2: prev_int2f_handler, 6: ds
		pop bp

		retf 2
	}
}

#endif /* USE_WIN386 */

#pragma code_seg ()

/* Driver exported functions. */

/** DLL entry point (or driver initialization routine).
 * The initialization routine should check whether a mouse exists.
 * @return nonzero value indicates a mouse exists.
 */
#pragma off (unreferenced)
BOOL FAR PASCAL LibMain(HINSTANCE hInstance, WORD wDataSegment,
                        WORD wHeapSize, LPSTR lpszCmdLine)
#pragma pop (unreferenced)
{
	uint16_t version = int33_get_driver_version();

	// For now we just check for the presence of any int33 driver version
	if (version == 0) {
		// No one responded to our request, we can assume no driver
		dos_print_string("To use VBMOUSE.DRV you have to run/install VBMOUSE.EXE first\r\n(or at least any other DOS mouse driver).\r\n\n$");
		dos_print_string("Press any key to terminate.$");
		dos_read_character();
		// This will cause a "can't load .drv" message from Windows
		// TODO: This aborts Windows startup, maybe we should still go on
		return 0;
	}

#if USE_WIN386
	if (is_windows_386_enhanced_mode()) {
		flags.has_win386 = true;
	}
#endif

	return 1;
}

/** Called by Windows to retrieve information about the mouse hardware. */
WORD FAR PASCAL Inquire(LPMOUSEINFO lpMouseInfo)
{
	lpMouseInfo->msExist = 1;
	lpMouseInfo->msRelative = 0;
	lpMouseInfo->msNumButtons = MOUSE_NUM_BUTTONS;
	lpMouseInfo->msRate = 80;
	return sizeof(MOUSEINFO);
}

/** Called by Windows to enable the mouse driver.
  * @param lpEventProc Callback function to call when a mouse event happens. */
VOID FAR PASCAL Enable(LPFN_MOUSEEVENT lpEventProc)
{
	// Store the windows-given callback
	_disable(); // Write to far pointer may not be atomic, and we could be interrupted mid-write
	eventproc = lpEventProc;
	_enable();

	if (!flags.enabled) {
		int33_reset();

#if USE_WIN386
		// If we are running under win386, hook int2f now.
		if (flags.has_win386) {
			_disable();
			prev_int2f_handler = _dos_getvect(0x2F);
			_dos_setvect(0x2F, int2f_handler);
			_enable();
		}
#endif

#if USE_WHEEL
		// Detect whether the int33 driver has wheel support
		flags.wheel = int33_get_capabilities() & 1;
		if (flags.wheel) {
			userapi.hUser = (HINSTANCE) GetModuleHandle("USER");
			userapi.GetCursorPos = (LPVOID) GetProcAddress(userapi.hUser, "GetCursorPos");
			userapi.WindowFromPoint = (LPVOID) GetProcAddress(userapi.hUser, "WindowFromPoint");
			userapi.GetParent = (LPVOID) GetProcAddress(userapi.hUser, "GetParent");
			userapi.GetClassName = (LPVOID) GetProcAddress(userapi.hUser, "GetClassName");
			userapi.GetWindowText = (LPVOID) GetProcAddress(userapi.hUser, "GetWindowText");
			userapi.GetWindowLong = (LPVOID) GetProcAddress(userapi.hUser, "GetWindowLong");
			userapi.IsWindowEnabled = (LPVOID) GetProcAddress(userapi.hUser, "IsWindowEnabled");
			userapi.EnumChildWindows = (LPVOID) GetProcAddress(userapi.hUser, "EnumChildWindows");
			userapi.PostMessage = (LPVOID) GetProcAddress(userapi.hUser, "PostMessage");
		}
#endif

		// Set the speed to 1,1 to enlarge dosemu coordinate range by 8x16 times.
		// In other absolute drivers, this doesn't change coordinates nor actual speed (which is inherited from host)
		// In normal relative drivers, we'll use the raw mickeys anyways, so speed should also have no effect.
		int33_set_mouse_speed(1, 1);

		// Get what the int33 driver thinks the current coordinate range is
		max_x = 0; max_y = 0;
		int33_get_max_coordinates(&max_x, &max_y);
		if (!max_x || !max_y) { max_x = 640; max_y = 200; } // For really bad drivers

		// Multiply that by 8x16 to match dosemu coordinate range
		// (and also in case the driver-provided coordinate range is too small)
		max_x *= 8; max_y *= 16;

		// And use that to define the window size which is what most
		// absolute drivers will use to define the coordinate range
		int33_set_horizontal_window(0, max_x);
		int33_set_vertical_window(0, max_y);

		int33_set_event_handler(INT33_EVENT_MASK_ALL, int33_mouse_callback);

		flags.enabled = true;
	}
}

/** Called by Windows to disable the mouse driver. */
VOID FAR PASCAL Disable(VOID)
{
	if (flags.enabled) {
#if USE_WIN386
		if (flags.has_win386) {
			// When Windows is shutting down, it's not that important to correctly
			// preserve the interrupt chain since it will end up being restored.
			_dos_setvect(0x2F, prev_int2f_handler);
		}
#endif

		int33_reset(); // This removes our handler and all other settings

		flags.enabled = false;
	}
}

/** Called by Window to retrieve the interrupt vector number used by this driver, or -1. */
int FAR PASCAL MouseGetIntVect(VOID)
{
	return 0x33;
}
