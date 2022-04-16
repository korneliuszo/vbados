#ifndef UNIXTIME_H
#define UNIXTIME_H

#include <stdbool.h>
#include <stdint.h>

#include "dlog.h"

#define UNIX_EPOCH_YEAR 1970
#define DOS_EPOCH_YEAR  1980

static bool is_leap_year(unsigned year)
{
	return ((year % 4) == 0 && (year % 100) != 0 || (year % 400) == 0);
}

static int days_per_year(unsigned year)
{
	return is_leap_year(year) ? 366 : 365;
}

static int days_per_month(unsigned month, bool leapyear)
{
	switch (month) {
	case 1:
	case 3:
	case 5:
	case 7:
	case 8:
	case 10:
	case 12:
		return 31;
	case 4:
	case 6:
	case 9:
	case 11:
		return 30;
	case 2:
		return leapyear ? 29 : 28;
	}

	return 0;
}

static void timestampns_to_dos_time(uint16_t __far *dos_time, uint16_t __far *dos_date, int64_t timestampns, int32_t tzoffset)
{
	// To maximize range of 32-bit ints, we'll use DOS's "2 seconds" as a unit instead of 1 second
	long days_since_epoch = 0;
	unsigned hours = 0, minutes = 0, seconds2 = 0;
	unsigned year, month, day;
	int per_year, per_month;
	bool is_leap;

	// Since we can only run on >= 386 anyway, let's do the initial
	// 64-bit division and the rest of 32-bit divisions/modulos
	// in asm using 386 32-bit instructions.

	__asm {
		push eax                     /* Preserve 32-bit regs */
		push ecx
		push edx
		mov eax, dword ptr [timestampns]
		mov edx, dword ptr [timestampns + 4]
		mov ecx, 2 * 1000000000      /* nanoseconds in 2 seconds, should still fit in a dword */

		idiv ecx                     /*  64-bit signed divison edx:eax / ecx, returns quotient in eax, remainder in edx */
		                             /* TODO: Handle overflow, which will trigger an interrupt */

		/* eax now contains seconds_since_epoch / 2 */
		xor edx, edx                 /* Discard the remainder (less than 2 seconds) */

		sub eax, [tzoffset]          /* Subtract tzoffset now (which is in seconds / 2 units) */

		mov ecx, (24 * 60 * 60) / 2  /* seconds in one day / 2 */

		idiv ecx
		/* eax now contains days_since_epoch */
		/* edx contains seconds (since start of day) /2 , this now fits in a 16-bit word */

		mov [days_since_epoch], eax

		mov eax, edx                /* eax = seconds / 2 */
		xor edx, edx
		mov ecx, 60 / 2             /* seconds in a minute / 2; also clears upper part of ecx, we'll start using 16-bit registers from now on */

		div cx
		/* ax contains minutes, dx contains remainder seconds/2 -- should be < 30 */
		mov [seconds2], dx

		xor dx, dx
		mov cx, 60                  /* minutes per hour */
		div cx
		/* ax contains hours (< 24), dx contains remainder minutes (< 60) */
		mov [minutes], dx
		mov [hours], ax

		pop edx
		pop ecx
		pop eax
	}

	year = UNIX_EPOCH_YEAR;
	if (days_since_epoch > 0) {
		while (days_since_epoch >= (per_year = days_per_year(year))) {
			days_since_epoch -= per_year;
			year++;
		}
	} else {
		while (days_since_epoch < 0) {
			days_since_epoch += days_per_year(year);
			year--;
		}
	}

	month = 1;
	is_leap = is_leap_year(year);
	while (days_since_epoch >= (per_month = days_per_month(month, is_leap))) {
		days_since_epoch -= per_month;
		month++;
	}
	day = 1 + days_since_epoch; // (day is 1-based)

	if (year < DOS_EPOCH_YEAR) {
		dlog_puts("Year is too old, will show as 0");
		year = 0;
	} else {
		year -= DOS_EPOCH_YEAR;
	}

	*dos_time = ((hours << 11) & 0xF800) | ((minutes << 5) & 0x7E0) | (seconds2 & 0x1F);
	*dos_date = ((year << 9) & 0xFE00)   | ((month << 5) & 0x1E0)   | (day & 0x1F);
}

#endif // UNIXTIME_H
