/*
 * VBSF - Shared folders entry point
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <dos.h>
#include <ctype.h>

#include "kitten.h"
#include "version.h"
#include "dlog.h"
#include "vboxshfl.h"
#include "dostsr.h"
#include "sftsr.h"
#include "unicode.h"

static nl_catd cat;

static char get_drive_letter(const char *path) {
	if (!path || path[0] == '\0') return '\0';
	if (path[1] == '\0' || (path[1] == ':' && path[2] == '\0')) {
		return path[0];
	} else {
		return '\0';
	}
}

static char find_free_drive_letter() {
	DOSLOL __far *lol = dos_get_list_of_lists();
	DOSCDS __far *cds = lol->cds;
	char letter;

	for (letter = 'A'; letter <= 'Z'; letter++) {
		int drive = drive_letter_to_index(letter);

		if (cds[drive].flags == 0) {
			// Looks free to me...
			return letter;
		}
	}

	return 0;
}

static void list_mountopts(MOUNTOPTS __far *opts, char *s)
{
	s[0] = '\0';

	if (opts->generate_sfn) {
		if (opts->hash_chars != DEF_HASH_CHARS) {
			s += sprintf(s, "/hash %u ", opts->hash_chars);
		}
	} else {
		s += sprintf(s, "/nohash ");
	}
	if (opts->use_host_sfn) {
		s += sprintf(s, "/host ");
	}
	if (opts->require_uppercase) {
		s += sprintf(s, "/upper ");
	}
}

static int list_folders(LPTSRDATA data)
{
	int32_t err;
	SHFLMAPPING maps[SHFL_MAX_MAPPINGS];
	SHFLSTRING_WITH_BUF(str, SHFL_MAX_LEN);
	char optstr[32];
	unsigned num_maps = sizeof(maps) / sizeof(SHFLMAPPING);
	unsigned i;

	printf(_(1, 0, "Mounted drives:\n"));

	for (i = 0; i < NUM_DRIVES; i++) {
		if (data->drives[i].root == SHFL_ROOT_NIL) {
			// Not mounted
			continue;
		}

		err = vbox_shfl_query_map_name(&data->vb, data->hgcm_client_id, data->drives[i].root, &str.shflstr);
		if (err) {
			printf(_(3, 0, "Error on Query Map Name, err=%ld\n"), err);
			continue;
		}

		(void)utf8_to_local(data, str.buf, str.buf, NULL);

		list_mountopts(&data->drives[i].opt, optstr);


		printf(_(1, 1, " %s on %c:  %s\n"), str.buf, drive_index_to_letter(i), optstr);
	}

	err = vbox_shfl_query_mappings(&data->vb, data->hgcm_client_id, 0, &num_maps, maps);
	if (err) {
		printf(_(3, 1, "Error on Query Mappings, err=%ld\n"), err);
		return err;
	}

	printf(_(1, 2, "Available shared folders:\n"));

	for (i = 0 ; i < num_maps; i++) {
		err = vbox_shfl_query_map_name(&data->vb, data->hgcm_client_id, maps[i].root, &str.shflstr);
		if (err) {
			printf(_(3, 0, "Error on Query Map Name, err=%ld\n"), err);
			continue;
		}

		(void)utf8_to_local(data, str.buf, str.buf, NULL);
		printf(" %s\n", str.buf);
	}

	return 0;
}

/** Closes all currently open files.
 *  @param drive drive number, or -1 to close files from all drives. */
static void close_openfiles(LPTSRDATA data, int drive)
{
	SHFLROOT search_root = SHFL_ROOT_NIL;
	int32_t err;
	unsigned i;

	if (drive >= 0) {
		search_root = data->drives[drive].root;
	}

	for (i = 0; i < NUM_FILES; i++) {
		if (data->files[i].root == SHFL_ROOT_NIL) {
			// Already closed
			continue;
		}
		if (search_root != SHFL_ROOT_NIL &&
		        data->files[i].root != search_root) {
			// File from a different shared folder
			continue;
		}

		err = vbox_shfl_close(&data->vb, data->hgcm_client_id, data->files[i].root, data->files[i].handle);
		if (err) {
			printf(_(3, 2, "Error on Close File, err=%ld\n"), err);
			// Ignore it
		}

		data->files[i].root = SHFL_ROOT_NIL;
		data->files[i].handle = SHFL_HANDLE_NIL;
	}
}

static int mount_shfl(LPTSRDATA data, int drive, const char *folder)
{
	int32_t err;
	SHFLSTRING_WITH_BUF(str, SHFL_MAX_LEN);
	SHFLROOT root = SHFL_ROOT_NIL;
	unsigned flags = SHFL_MIQF_DRIVE_LETTER, version = 0;

	shflstring_strcpy(&str.shflstr, folder);

	err = vbox_shfl_map_folder(&data->vb, data->hgcm_client_id, &str.shflstr, &root);
	if (err) {
		fprintf(stderr, _(3, 3, "Cannot mount shared folder '%s', err=%d\n"), folder, err);
		return -1;
	}

	data->drives[drive].root = root;

	return 0;
}

static int unmount_shfl(LPTSRDATA data, int drive)
{
	int32_t err;

	close_openfiles(data, drive);

	err = vbox_shfl_unmap_folder(&data->vb, data->hgcm_client_id,
	                             data->drives[drive].root);
	if (err) {
		fprintf(stderr, _(3, 5, "Cannot unmount shared folder, err=%d\n"), err);
		return -1;
	}

	data->drives[drive].root = SHFL_ROOT_NIL;

	return 0;
}

static void set_mountopts_defaults(MOUNTOPTS *opts)
{
	opts->generate_sfn = true;
	opts->require_uppercase = false;
	opts->use_host_sfn = false;
	opts->hash_chars = DEF_HASH_CHARS;
}

static bool is_valid_drive(DOSLOL __far *lol, char drive_letter, int drive)
{
	if (drive < 0) {
		fprintf(stderr, _(3, 6, "Invalid drive %c:\n"), drive_letter);
		return false;
	}

	if (drive >= lol->last_drive || drive >= NUM_DRIVES) {
		fprintf(stderr, _(3, 7, "Drive %c: is after LASTDRIVE\n"), drive_letter);
		return false;
	}

	return true;
}

static bool is_mounted_drive(LPTSRDATA data, char drive_letter, int drive)
{
	if (data->drives[drive].root == SHFL_ROOT_NIL) {
		fprintf(stderr, _(3, 11, "Drive %c not mounted\n"), drive_letter);
		return false;
	}

	return true;
}

static int mount(LPTSRDATA data, char *folder, char drive_letter, MOUNTOPTS *opts)
{
	int drive = drive_letter_to_index(drive_letter);
	DOSLOL __far *lol = dos_get_list_of_lists();
	DOSCDS __far *cds;

	if (!is_valid_drive(lol, drive_letter, drive)) {
		return EXIT_FAILURE;
	}

	if (data->drives[drive].root != SHFL_ROOT_NIL) {
		fprintf(stderr, _(3, 8, "Drive %c already mounted\n"), drive_letter);
		return EXIT_FAILURE;
	}

	cds = &lol->cds[drive];

	if (cds->flags) {
		fprintf(stderr, _(3, 9, "Drive %c: already exists\n"), drive_letter);
		return EXIT_FAILURE;
	}

	if (mount_shfl(data, drive, folder) != 0) {
		fprintf(stderr, _(3, 10, "Cannot mount drive %c:\n"), drive_letter);
		return EXIT_FAILURE;
	}

	data->drives[drive].opt = *opts;

	// Ok, set the network flag.
	// By setting the physical flag, we also let DOS know the drive is present
	cds->flags = DOS_CDS_FLAG_NETWORK | DOS_CDS_FLAG_PHYSICAL;

	(void)utf8_to_local(data, folder, folder, NULL);
	printf(_(1, 3, "Shared folder '%s' mounted as drive %c:\n"), folder, drive_letter);

	return EXIT_SUCCESS;
}

static int unmount(LPTSRDATA data, char drive_letter)
{
	int drive = drive_letter_to_index(drive_letter);
	DOSLOL __far *lol = dos_get_list_of_lists();
	DOSCDS __far *cds;

	if (!is_valid_drive(lol, drive_letter, drive)
	        || !is_mounted_drive(data, drive_letter, drive)) {
		return EXIT_FAILURE;
	}

	if (unmount_shfl(data, drive) != 0) {
		return EXIT_FAILURE;
	}

	// Hide the drive from DOS
	cds = &lol->cds[drive];
	cds->flags = 0;

	// TODO Clear current directory ?

	printf(_(1, 4, "Drive %c: unmounted\n"), drive_letter);

	return EXIT_SUCCESS;
}

static int get_mountopts(LPTSRDATA data, char drive_letter, MOUNTOPTS *opts)
{
	int drive = drive_letter_to_index(drive_letter);
	DOSLOL __far *lol = dos_get_list_of_lists();

	if (!is_valid_drive(lol, drive_letter, drive)
	        || !is_mounted_drive(data, drive_letter, drive)) {
		return EXIT_FAILURE;
	}

	*opts = data->drives[drive].opt;

	return EXIT_SUCCESS;
}

static int set_mountopts(LPTSRDATA data, char drive_letter, MOUNTOPTS *opts)
{
	int drive = drive_letter_to_index(drive_letter);
	DOSLOL __far *lol = dos_get_list_of_lists();

	if (!is_valid_drive(lol, drive_letter, drive)
	        || !is_mounted_drive(data, drive_letter, drive)) {
		return EXIT_FAILURE;
	}

	data->drives[drive].opt = *opts;

	return EXIT_SUCCESS;
}

static int automount(LPTSRDATA data)
{
	int32_t err;
	SHFLMAPPING maps[SHFL_MAX_MAPPINGS];
	SHFLSTRING_WITH_BUF(name, SHFL_MAX_LEN);
	SHFLSTRING_WITH_BUF(mountPoint, SHFL_MAX_LEN);
	MOUNTOPTS opts;
	unsigned num_maps = sizeof(maps) / sizeof(SHFLMAPPING);
	unsigned flags, version;
	unsigned i;

	set_mountopts_defaults(&opts);

	err = vbox_shfl_query_mappings(&data->vb, data->hgcm_client_id,
	                               SHFL_MF_AUTOMOUNT, &num_maps, maps);
	if (err) {
		printf(_(3, 1, "Error on Query Mappings, err=%ld\n"), err);
		return err;
	}

	for (i = 0 ; i < num_maps; i++) {
		unsigned flags = SHFL_MIQF_DRIVE_LETTER, version = 0;
		char drive_letter = '\0';

		err = vbox_shfl_query_map_info(&data->vb, data->hgcm_client_id, maps[i].root,
		                               &name.shflstr, &mountPoint.shflstr, &flags, &version);
		if (err) {
			printf(_(3, 12, "Error on Query Map Info, err=%ld\n"), err);
			continue;
		}

		if (!(flags & SHFL_MIF_AUTO_MOUNT)) {
			// No automount...
			continue;
		}

		// Can we try to use the mountpoint as a drive letter?
		if (mountPoint.shflstr.u16Length >= 1) {
			drive_letter = get_drive_letter(mountPoint.shflstr.ach);
		}

		// Otherwise try to find a free one...
		if (!drive_letter) {
			drive_letter = find_free_drive_letter();
		}

		mount(data, name.buf, drive_letter, &opts);
	}

	return 0;
}

static int unmount_all(LPTSRDATA data)
{
	int err = 0;
	unsigned i;

	// Close all files
	close_openfiles(data, -1);

	// Unmount all drives
	for (i = 0 ; i < NUM_DRIVES; i++) {
		if (data->drives[i].root != SHFL_ROOT_NIL) {
			if (unmount(data, drive_index_to_letter(i)) != 0) {
				err = -1;
			}
		}
	}

	return err;
}

static int rescan(LPTSRDATA data)
{
	int err;

	if ((err = unmount_all(data))) {
		return err;
	}

	if ((err = automount(data))) {
		return err;
	}

	return 0;
}

static int get_nls(uint8_t __far * __far *file_upper_case, FCHAR __far * __far *file_char)
{
	union REGS r;
	struct SREGS s;
	static NLSTABLE nls_table;

	segread(&s);

	// Get FUCASE (File Uppercase Table)
	//
	r.w.ax = 0x6504;
	r.x.bx  = r.x.dx = 0xffff;
	r.x.cx  = 5;

	s.es    = s.ds;
	r.x.di  = (uint16_t) &nls_table;

	intdosx(&r, &r, &s);

	if (r.x.cx != 5)
		return 1;

	*file_upper_case = (uint8_t __far *)nls_table.table_data + 2;   // Skip size word

	// Get FCHAR (File Terminator Table)
	//
	r.w.ax  = 0x6505;

	intdosx(&r, &r, &s);

	if ( r.x.cx != 5 )
		return 1;

	*file_char = (FCHAR __far *)nls_table.table_data;

	return 0;
}

/** Tries to find a file with `name` in the same path as the current exec.
 *  @param buf MAX_PATH buffer to use
 *  @returns true if file exists
 */
static bool search_cmd_path(const char *name, char *buf)
{
	char cmd_drive[_MAX_DRIVE];
	char cmd_dir[_MAX_DIR];
	unsigned int attr;

	_splitpath(__argv[0], cmd_drive, cmd_dir, NULL, NULL);
	_makepath(buf, cmd_drive, cmd_dir, name, NULL);

	// Check if file exists
	return (_dos_getfileattr(buf, &attr) == 0);
}

static void load_unicode_table(uint16_t far *unicode_table)
{
	union REGS r;
	char filename[13];
	char fullpath[_MAX_PATH];
	char buffer[256];
	FILE *f;
	int i, ret;

	// get current Code Page
	//
	//      AX = 6601h
	//        Return: CF set on error
	//          AX = error code (see #01680 at AH=59h/BX=0000h)
	//        CF clear if successful
	//      BX = active code page (see #01757) <---
	//      DX = system code page (see #01757)
	//
	r.w.ax = 0x6601;

	intdos(&r, &r);

	if (r.x.cflag) {
		// Can't get codepage. Use ASCII only
		fputs(_(2, 0, "Warning: Active code page not found"), stderr);
		goto error;
	}

	sprintf(filename, r.x.bx > 999 ? "c%duni.tbl" : "cp%duni.tbl", r.x.bx);

	// Search in the same directory as the executable first
	if (!search_cmd_path(filename, fullpath)) {
		// Search in path if not found
		_searchenv(filename, "PATH", fullpath);
		if ('\0' == fullpath[0]) {
			fprintf(stderr, _(2, 1, "Warning: Can't find Unicode table: %s"), filename);
			goto error;
		}
	}

	f = fopen(fullpath, "rb");
	if (!f) {
		fprintf(stderr, _(2, 2, "Warning: Can't load Unicode table: %s"), filename);
		goto error;
	}

	// Read "Description of table" (terminated by \r\n)
	if (!fgets(buffer, sizeof(buffer), f)) {
		fprintf(stderr, _(2, 3, "Warning: Invalid file format: %s"), filename);
		goto close;
	}

	// Read "Table format designator" (currently only format 1 is supported)
	ret = fread(buffer, 1, 1, f);
	if (ret != 1 || buffer[0] != 1) {
		fprintf(stderr, _(2, 3, "Warning: Invalid file format: %s"), filename);
		goto close;
	}

	// Now read the actual table (256 bytes in format 1)
	if ((ret = fread(buffer, 1, 256, f)) != 256) {
		fprintf(stderr, _(2, 2, "Warning: Can't load Unicode table: %s"), filename);
		goto close;
	}

	fclose(f);

	_fmemcpy(unicode_table, buffer, 256);

	return;

close:
	fclose(f);
error:
	fputs(_(2, 4, ". Defaulting to cp437\n"), stderr);
}

static int configure_driver(LPTSRDATA data)
{
	unsigned i;
	int32_t err;

	// Clear up data structures
	for (i = 0; i < NUM_DRIVES; ++i) {
		data->drives[i].root = SHFL_ROOT_NIL;
	}
	for (i = 0; i < NUM_FILES; ++i) {
		data->files[i].root = SHFL_ROOT_NIL;
		data->files[i].handle = SHFL_HANDLE_NIL;
	}

	// Configure the debug logging port
	dlog_init();

	// Initialize TSR data
	data->dossda = dos_get_swappable_dos_area();

	// Get the current timezone offset
	if (getenv("TZ")) {
		tzset();
		printf(_(1, 5, "Using timezone from TZ variable (%s)\n"), tzname[0]);
		data->tz_offset = timezone / 2;
	} else {
		data->tz_offset = 0;
	}

	// Get uppercase and valid file char tables
	err = get_nls(&data->file_upper_case, &data->file_char);
	if (err) {
		puts(_(3, 13, "Cannot get the NLS tables"));
		return -1;
	}

	// Now try to initialize VirtualBox communication
	err = vbox_init_device(&data->vb);
	if (err) {
		fprintf(stderr, _(3, 14, "Cannot find VirtualBox PCI device, err=%ld\n"), err);
		return -1;
	}

	err = vbox_init_buffer(&data->vb, VBOX_BUFFER_SIZE);
	if (err) {
		fprintf(stderr, _(3, 15, "Cannot lock buffer used for VirtualBox communication, err=%ld\n"), err);
		return -1;
	}

	err = vbox_report_guest_info(&data->vb, VBOXOSTYPE_DOS);
	if (err) {
		fprintf(stderr, _(3, 16, "VirtualBox communication is not working, err=%ld\n"), err);
		return -1;
	}

	err = vbox_hgcm_connect_existing(&data->vb, "VBoxSharedFolders", &data->hgcm_client_id);
	if (err) {
		printf(_(3, 17, "Cannot connect to shared folder service, err=%ld\n"), err);
		return -1;
	}

	err = vbox_shfl_set_utf8(&data->vb, data->hgcm_client_id);
	if (err) {
		printf(_(3, 18, "Cannot configure UTF-8 on shared folder service, err=%ld\n"), err);
		return -1;
	}

	load_unicode_table( &data->unicode_table);
	
	printf(_(1, 6, "Connected to VirtualBox shared folder service\n"));

	return 0;
}

static int move_driver_to_umb(LPTSRDATA __far * data)
{
	segment_t cur_seg = FP_SEG(*data);
	segment_t umb_seg = reallocate_to_umb(cur_seg,  get_resident_size() + DOS_PSP_SIZE);

	if (umb_seg) {
		// Update the data pointer with the new segment
		*data = MK_FP(umb_seg, FP_OFF(*data));
		return 0;
	} else {
		return -1;
	}
}

static __declspec(aborts) int install_driver(LPTSRDATA data, bool high)
{
	const unsigned int resident_size = DOS_PSP_SIZE + get_resident_size();

	// No more interruptions from now on and until we TSR.
	// Inserting ourselves in the interrupt chain should be atomic.
	_disable();

	data->prev_int2f_handler = _dos_getvect(0x2f);
	_dos_setvect(0x2f, data:>int2f_isr);

	printf(_(1, 7, "Driver installed\n"));

	// If we reallocated ourselves to UMB,
	// it's time to free our initial conventional memory allocation
	if (high) {
		finish_reallocation(_psp, FP_SEG(data));
	}

	kittenclose();
	_dos_keep(EXIT_SUCCESS, get_paragraphs(resident_size));

	// Shouldn't reach this part
	return EXIT_FAILURE;
}

static bool check_if_driver_uninstallable(LPTSRDATA data)
{
	void (__interrupt __far *cur_int2f_handler)() = _dos_getvect(0x2f);

	// Compare the segment of the installed handler to see if its ours
	// or someone else's
	if (FP_SEG(cur_int2f_handler) != FP_SEG(data)) {
		fprintf(stderr, _(3, 19, "INT2F has been hooked by someone else, cannot safely remove\n"));
		return false;
	}

	return true;
}

static int unconfigure_driver(LPTSRDATA data)
{
	unmount_all(data);

	vbox_hgcm_disconnect(&data->vb, data->hgcm_client_id);
	data->hgcm_client_id = 0;

	vbox_release_buffer(&data->vb);

	return 0;
}

static int uninstall_driver(LPTSRDATA data)
{
	_dos_setvect(0x2f, data->prev_int2f_handler);

	// Find and deallocate the PSP (including the entire program),
	// it is always 256 bytes (16 paragraphs) before the TSR segment
	dos_free(FP_SEG(data) - (DOS_PSP_SIZE/16));

	printf(_(1, 8, "Driver uninstalled\n"));

	return 0;
}

static int driver_not_found(void)
{
	fprintf(stderr, _(3, 20, "Driver data not found (driver not installed?)\n"));
	return EXIT_FAILURE;
}

static void print_help(void)
{
	putchar('\n');
	puts(  _(0, 0,   "Usage: "));
	puts(  _(0, 1,   "  VBSF <ACTION> <ARGS..> [<OPTIONS..>]"));
	putchar('\n');
	puts(  _(0, 2,   "Supported actions and options:"));
	puts(  _(0, 3,   "  install                 Install the driver (default)."));
	puts(  _(0, 4,   "    low                     Install in conventional memory (otherwise UMB)."));
	puts(  _(0, 5,   "  uninstall               Uninstall the driver from memory."));
	puts(  _(0, 6,   "  list                    List available shared folders."));
	puts(  _(0, 7,   "  mount <FOLD> <X:> ...   Mount a shared folder into drive X:."));
	puts(  _(0, 8,   "    /hash <n>               Number of hash chars following the '~'"));
	puts(  _(0, 9,   "                              for generated DOS short filenames."));
	printf(_(0, 10,  "                              (between %d and %d; default %d; 0 to disable)\n"),
	                                               MIN_HASH_CHARS, MAX_HASH_CHARS, DEF_HASH_CHARS);
	puts(  _(0, 11,  "    /host                   Use short file names from Windows hosts."));
	puts(  _(0, 12,  "    /upper                  Require uppercase host filenames."));
	puts(  _(0, 13,  "  remount <X:> ...        Change mount options for mounted drive X:."));
	puts(  _(0, 14,  "  umount <X:>             Unmount shared folder from drive X:."));
	puts(  _(0, 15,  "  rescan                  Unmount everything and recreate automounts."));
}

static int invalid_arg(const char *s)
{
	fprintf(stderr, _(3, 21, "Invalid argument '%s'\n"), s);
	print_help();
	return EXIT_FAILURE;
}

static int arg_required(const char *s)
{
	fprintf(stderr, _(3, 22, "Argument required for '%s'\n"), s);
	print_help();
	return EXIT_FAILURE;
}

static bool is_true(const char *s)
{
	return stricmp(s, "yes") == 0
	       || stricmp(s, "y") == 0
	       || stricmp(s, "on") == 0
	       || stricmp(s, "true") == 0
	       || stricmp(s, "enabled") == 0
	       || stricmp(s, "enable") == 0
	       || stricmp(s, "1") == 0;
}

static bool is_false(const char *s)
{
	return stricmp(s, "no") == 0
	       || stricmp(s, "n") == 0
	       || stricmp(s, "off") == 0
	       || stricmp(s, "false") == 0
	       || stricmp(s, "disabled") == 0
	       || stricmp(s, "disable") == 0
	       || stricmp(s, "0") == 0;
}

static int parse_mountopts(MOUNTOPTS *opts, int *argi, int argc, const char *argv[])
{
	int i = *argi;
	while (argv[i][0] == '/' && i < argc) {
		if (stricmp(argv[i], "/hash") == 0) {
			i++;
			if (i >= argc) return arg_required("/hash");

			if (is_false(argv[i])) {
				opts->generate_sfn = false;
			} else if (strlen(argv[i]) == 1 && isdigit(argv[i][0])) {
				int hash_chars = argv[i][0] - '0';
				if (hash_chars < MIN_HASH_CHARS || hash_chars > MAX_HASH_CHARS) {
					return invalid_arg(argv[i]);
				}
				opts->generate_sfn = true;
				opts->hash_chars = hash_chars;
			} else {
				return invalid_arg(argv[i]);
			}
		} else if (stricmp(argv[i], "/nohash") == 0) {
			opts->generate_sfn = false;
		} else if (stricmp(argv[i], "/host") == 0) {
			opts->use_host_sfn = true;
		} else if (stricmp(argv[i], "/nohost") == 0) {
			opts->use_host_sfn = false;
		} else if (stricmp(argv[i], "/upper") == 0) {
			opts->require_uppercase = true;
		} else if (stricmp(argv[i], "/noupper") == 0) {
			opts->require_uppercase = false;
		} else {
			return invalid_arg(argv[i]);
		}
		i++;
	}
	*argi = i;
	return EXIT_SUCCESS;
}

int main(int argc, const char *argv[])
{
	LPTSRDATA data = get_tsr_data(true);
	int err, argi = 1;
	SHFLSTRING_WITH_BUF(utf8name, SHFL_MAX_LEN);

	cat = kittenopen("vbsf");

	if (argi >= argc || stricmp(argv[argi], "install") == 0) {
		uint8_t hash_chars = DEF_HASH_CHARS;
		bool high = true;

		argi++;
		for (; argi < argc; argi++) {
			if (stricmp(argv[argi], "low") == 0) {
				high = false;
			} else if (stricmp(argv[argi], "high") == 0) {
				high = true;
			} else {
				return invalid_arg(argv[argi]);
			}
		}

		printf(_(1, 9, "\nVBSharedFolders %x.%x\n"), VERSION_MAJOR, VERSION_MINOR);

		if (data) {
			printf(_(1, 10, "VBSF already installed\n"));
			print_help();
			return EXIT_SUCCESS;
		}

		data = get_tsr_data(false);
		if (high) {
			err = move_driver_to_umb(&data);
			if (err) high = false; // Not fatal
		} else {
			deallocate_environment(_psp);
		}
		err = configure_driver(data);
		if (err) {
			if (high) cancel_reallocation(FP_SEG(data));
			return EXIT_FAILURE;
		}
		err = automount(data);
		if (err) {
			// Automount errors are not fatal
		}
		return install_driver(data, high);
	} else if (stricmp(argv[argi], "uninstall") == 0) {
		if (!data) return driver_not_found();
		if (!check_if_driver_uninstallable(data)) {
			return EXIT_FAILURE;
		}
		err = unconfigure_driver(data);
		if (err) {
			return EXIT_FAILURE;
		}
		return uninstall_driver(data);
	} else if (stricmp(argv[argi], "list") == 0) {
		if (!data) return driver_not_found();
		return list_folders(data);
	} else if (stricmp(argv[argi], "mount") == 0) {
		MOUNTOPTS opts;
		char *folder;
		char drive;
		if (!data) return driver_not_found();

		argi++;
		if (argi >= argc) return arg_required("mount");

		folder = (char *) argv[argi];
		argi++;
		if (argi >= argc) return arg_required("mount");
		drive = get_drive_letter(argv[argi]);
		if (!drive) return invalid_arg(argv[argi]);

		set_mountopts_defaults(&opts);
		err = parse_mountopts(&opts, &argi, argc, argv);
		if (err) return err;

		local_to_utf8(data, utf8name.buf, folder, utf8name.shflstr.u16Size);
		return mount(data, utf8name.buf, drive, &opts);
	} else if (stricmp(argv[argi], "umount") == 0 || stricmp(argv[argi], "unmount") == 0) {
		char drive;
		if (!data) return driver_not_found();

		argi++;
		if (argi >= argc) return arg_required("umount");
		drive = get_drive_letter(argv[argi]);
		if (!drive) return invalid_arg(argv[argi]);

		return unmount(data, drive);
	} else if (stricmp(argv[argi], "remount") == 0) {
		MOUNTOPTS opts;
		char drive;
		if (!data) return driver_not_found();

		argi++;
		if (argi >= argc) return arg_required("remount");
		drive = get_drive_letter(argv[argi]);
		if (!drive) return invalid_arg(argv[argi]);

		err = get_mountopts(data, drive, &opts);
		if (err) return err;

		argi++;
		err = parse_mountopts(&opts, &argi, argc, argv);
		if (err) return err;

		return set_mountopts(data, drive, &opts);
	} else if (stricmp(argv[argi], "rescan") == 0) {
		if (!data) return driver_not_found();
		return rescan(data);
	} else {
		return invalid_arg(argv[argi]);
	}
}
