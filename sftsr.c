/*
 * VBSF - VirtualBox shared folders for DOS, resident part
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

#include <stdlib.h>
#include <string.h>
#include <i86.h>

#include "int21dos.h"
#include "unixtime.h"
#include "vboxshfl.h"
#include "sftsr.h"

TSRDATA data;

/** Private buffer for VirtualBox filenames. */
static SHFLSTRING_WITH_BUF(shflstr, SHFL_MAX_LEN);

static SHFLDIRINFO_WITH_BUF(shfldirinfo, SHFL_MAX_LEN);

static SHFLCREATEPARMS createparms;

static uint8_t map_shfl_attr_to_dosattr(const SHFLFSOBJATTR *a)
{
	// DOS attributes are in the higher word of fMode (see RTFS_DOS_*)
	uint8_t attr = (a->fMode >> 16) & 0x3F;
	if (a->fMode & 0x4000U) attr |= _A_SUBDIR;
	return attr;
}

static void map_shfl_info_to_dossft(DOSSFT __far *sft, SHFLFSOBJINFO *i)
{
	sft->attr = map_shfl_attr_to_dosattr(&i->Attr);
	sft->f_size = i->cbObject;
	timestampns_to_dos_time(&sft->f_time, &sft->f_date, i->ModificationTime, data.tz_offset);
}

static void map_shfl_info_to_dosdir(DOSDIR __far *dir, SHFLFSOBJINFO *i)
{
	dir->attr = map_shfl_attr_to_dosattr(&i->Attr);
	dir->f_size = i->cbObject;
	timestampns_to_dos_time(&dir->f_time, &dir->f_date, i->ModificationTime, data.tz_offset);
	dir->start_cluster = 0;
}

static void map_shfl_info_to_getattr(union INTPACK __far *r, SHFLFSOBJINFO *i)
{
	r->x.ax = map_shfl_attr_to_dosattr(&i->Attr);
	r->x.bx = ((uint32_t)i->cbObject) >> 16;
	r->x.di = ((uint16_t)i->cbObject);
	timestampns_to_dos_time(&r->x.cx, &r->x.dx, i->ModificationTime, data.tz_offset);
}

static int get_op_drive_num(union INTPACK __far *r)
{
	DOSSFT __far *sft;

	switch (r->h.al) {
	case DOS_FN_CLOSE:
	case DOS_FN_COMMIT:
	case DOS_FN_READ:
	case DOS_FN_WRITE:
	case DOS_FN_LOCK:
	case DOS_FN_UNLOCK:
	case DOS_FN_SEEK_END:
		// Some operations use an SFT and we directly get the drive from it
		sft = MK_FP(r->x.es, r->x.di);
		return sft->dev_info & 0x1F;

	case DOS_FN_RMDIR:
	case DOS_FN_MKDIR:
	case DOS_FN_CHDIR:
	case DOS_FN_SET_FILE_ATTR:
	case DOS_FN_GET_FILE_ATTR:
	case DOS_FN_RENAME:
	case DOS_FN_DELETE:
	case DOS_FN_OPEN:
	case DOS_FN_CREATE:
	case DOS_FN_FIND_FIRST:
	case DOS_FN_OPEN_EX:
		// We can use the drive of the first filename argument for these operations
		return drive_letter_to_index(data.dossda->fn1[0]);

	case DOS_FN_GET_DISK_FREE:
		// Otherwise just get the current drive from dos
		return drive_letter_to_index(data.dossda->drive_cds->curr_path[0]);

	case DOS_FN_FIND_NEXT:
		return data.dossda->sdb.drive_letter & 0x1F;

	default:
		// We don't support this operation
		return -1;
	}
}

static bool is_call_for_mounted_drive(union INTPACK __far *r)
{
	int drive = get_op_drive_num(r);

	if (drive < 0 || drive >= MAX_NUM_DRIVE) {
		return false;
	}

	return data.drives[drive].root != SHFL_ROOT_NIL;
}

static void clear_dos_err(union INTPACK __far *r)
{
	r->x.flags &= ~INTR_CF;
}

static void set_dos_err(union INTPACK __far *r, int err)
{
	r->x.flags |= INTR_CF;
	r->x.ax = err;
}

static int vbox_err_to_dos(int32_t err)
{
	switch (err) {
	case VINF_SUCCESS:
		return DOS_ERROR_SUCCESS;
	case VERR_INVALID_PARAMETER:
		return DOS_ERROR_INVALID_FUNCTION;
	case VERR_INVALID_HANDLE:
		return DOS_ERROR_INVALID_HANDLE;
	case VERR_FILE_NOT_FOUND:
		return DOS_ERROR_FILE_NOT_FOUND;
	case VERR_PATH_NOT_FOUND:
		return DOS_ERROR_PATH_NOT_FOUND;
	case VERR_NO_MORE_FILES:
		return DOS_ERROR_NO_MORE_FILES;
	case VERR_ALREADY_EXISTS:
		return DOS_ERROR_FILE_EXISTS;
	default:
		return DOS_ERROR_GEN_FAILURE;
	}
}

static void set_vbox_err(union INTPACK __far *r, int32_t err)
{
	dlog_print("vbox error ");
	dlog_printx(err >> 16);
	dlog_print(":");
	dlog_printx(err & 0xFFFF);
	dlog_endline();
	r->x.flags |= INTR_CF;
	r->x.ax = vbox_err_to_dos(err);
}

static int my_strrchr(const char __far *str, char c)
{
	int last = -1;
	const char __far *s = str;
	while (*s) {
		if (*s == c) last = s - str;
		s++;
	}
	return last;
}

static const char * get_basename(const char *path)
{
	int last_sep = my_strrchr(path, '\\');
	if (last_sep >= 0) {
		return &path[last_sep+1];
	} else {
		return path;
	}
}

static void translate_filename_to_host(SHFLSTRING *str)
{
	// TODO This should map UTF-8 to local CP :(
	(void) str;
}

static void translate_filename_from_host(SHFLSTRING *str)
{
	// TODO This should map UTF-8 to local CP :(
	// At least do a poor man's uppercase...
	unsigned i;
	for (i = 0; i < str->u16Length; i++) {
		if (str->ach[i] >= 'a' && str->ach[i] <= 'z') {
			str->ach[i] = 'A' + (str->ach[i] - 'a');
		}
	}
}

static void fix_wildcards(SHFLSTRING *str)
{
	unsigned i;

	// If this is the standard ????????.??? patterns, replace with *
	if (str->u16Length >= 8+1+3) {
		i = str->u16Length - (8+1+3);
		if (memcmp(&str->ach[i], "????????.???", (8+1+3)) == 0) {
			strcpy(&str->ach[i], "*");
			str->u16Length = i + 1;
		}
	}

	for (i = 1; i < str->u16Length; i++) {
		// VirtualBox will only match N consecutive ?s to filenames longer than N
		// Replace consecutive ???? with ?***
		if ( str->ach[i] == '?' &&
		     (str->ach[i-1] == '*' || str->ach[i-1] == '?') ) {
			str->ach[i] = '*';
		}
	}
}


static void copy_drive_relative_filename(SHFLSTRING *str, const char __far *path)
{
	// Assume X:.... path for now, i.e. drive_relative path starts at char 2
	shflstring_strcpy(str, path + 2);
}

static void copy_drive_relative_dirname(SHFLSTRING *str, const char __far *path)
{
	int last_sep = my_strrchr(path + 2, '\\');
	if (last_sep >= 0) {
		shflstring_strncpy(str, path + 2, last_sep == 0 ? 1 : last_sep);
	} else {
		shflstring_strcpy(str, path + 2);
	}
}

static bool copy_to_8_3_filename(char __far *dst, const SHFLSTRING *str)
{
	int last_dot = my_strrchr(str->ach, '.');
	unsigned namelen, extlen;
	bool valid_8_3 = true;

	namelen = last_dot >= 0 ? last_dot : str->u16Length;
	extlen = last_dot >= 0 ? str->u16Length - (last_dot + 1) : 0;

	if (extlen == 0 && (namelen <= 1 && str->ach[0] == '.')) {
		// . , .. files
		namelen = str->u16Length;
		extlen = 0;
	}

	if (namelen > 8) {
		namelen = 8;
		valid_8_3 = false;
	}

	if (extlen > 3) {
		extlen = 3;
		valid_8_3 = false;
	}

	_fmemcpy(&dst[0], str->ach, namelen);
	_fmemset(&dst[namelen], ' ', 8 - namelen);
	_fmemcpy(&dst[8], str->ach + last_dot + 1, extlen);
	_fmemset(&dst[8+extlen], ' ', 3 - extlen);

	return valid_8_3;
}

static uint16_t find_free_openfile()
{
	unsigned i;
	for (i = 1; i < NUM_FILES; i++) {
		if (data.files[i].root == SHFL_ROOT_NIL) {
			return i;
		}
	}
	return 0;
}

static bool is_valid_openfile_index(unsigned index)
{
	if (index > NUM_FILES) return false;
	if (index < 1) return false; // User programs cannot use index 0
	if (data.files[index].root == SHFL_ROOT_NIL) return 0;
	return true;
}

static inline void set_sft_openfile_index(DOSSFT __far *sft, unsigned index)
{
	sft->start_cluster = index;
}

static inline unsigned get_sft_openfile_index(DOSSFT __far *sft)
{
	return sft->start_cluster;
}

static void handle_create_open_ex(union INTPACK __far *r)
{
	const char __far *path = data.dossda->fn1;
	int drive = drive_letter_to_index(path[0]);
	SHFLROOT root = data.drives[drive].root;
	DOSSFT __far *sft = MK_FP(r->x.es, r->x.di);
	unsigned int action, mode;
	unsigned openfile;
	bool save_result;
	int32_t err;

	switch (r->h.al) {
	case DOS_FN_CREATE:
		action = OPENEX_CREATE_IF_NEW | OPENEX_REPLACE_IF_EXISTS;
		mode = OPENEX_MODE_WRITE;
		save_result = false;
		break;
	case DOS_FN_OPEN:
		action = OPENEX_FAIL_IF_NEW | OPENEX_OPEN_IF_EXISTS;
		mode = data.dossda->open_mode & 0x7F;
		save_result = false;
		break;
	case DOS_FN_OPEN_EX:
	default:
		action = data.dossda->openex_act;
		mode = data.dossda->openex_mode & 0x7F;
		save_result = true;
		break;
	}

	dlog_print("handle_open for ");
	dlog_fprint(path);
	dlog_print(" act=");
	dlog_printx(action);
	dlog_print(" mode=");
	dlog_printx(mode);
	dlog_endline();

	openfile = find_free_openfile();
	if (!openfile) {
		set_dos_err(r, DOS_ERROR_ERROR_TOO_MANY_OPEN_FILES);
		return;
	}

	copy_drive_relative_filename(&shflstr.shflstr, path);
	translate_filename_to_host(&shflstr.shflstr);

	memset(&createparms, 0, sizeof(SHFLCREATEPARMS));
	if (action & OPENEX_REPLACE_IF_EXISTS) {
		createparms.CreateFlags |= SHFL_CF_ACT_REPLACE_IF_EXISTS;
	} else if (action & OPENEX_OPEN_IF_EXISTS) {
		createparms.CreateFlags |= SHFL_CF_ACT_OPEN_IF_EXISTS;
	} else {
		createparms.CreateFlags |= SHFL_CF_ACT_FAIL_IF_EXISTS;
	}
	if (action & OPENEX_CREATE_IF_NEW) {
		createparms.CreateFlags |= SHFL_CF_ACT_CREATE_IF_NEW;
	} else {
		createparms.CreateFlags |= SHFL_CF_ACT_FAIL_IF_NEW;
	}

	if ((mode & OPENEX_MODE_RDWR) == OPENEX_MODE_RDWR) {
		createparms.CreateFlags |= SHFL_CF_ACCESS_READWRITE;
	} else if (mode & OPENEX_MODE_WRITE) {
		createparms.CreateFlags |= SHFL_CF_ACCESS_WRITE;
	} else {
		createparms.CreateFlags |= SHFL_CF_ACCESS_READ;
	}

	dlog_print("vbox createparms flags=");
	dlog_printx(createparms.CreateFlags);
	dlog_endline();

	err = vbox_shfl_open(&data.vb, data.hgcm_client_id, root, &shflstr.shflstr, &createparms);
	if (err) {
		set_vbox_err(r, err);
		return;
	}

	dlog_print("vbox success result=");
	dlog_printd(createparms.Result);
	dlog_print(" openfile=");
	dlog_printu(openfile);
	dlog_endline();

	switch (createparms.Result) {
	case SHFL_PATH_NOT_FOUND:
		set_dos_err(r, DOS_ERROR_PATH_NOT_FOUND);
		return;
	case SHFL_FILE_NOT_FOUND:
		set_dos_err(r, DOS_ERROR_FILE_NOT_FOUND);
		return;
	case SHFL_FILE_EXISTS:
		if (save_result) r->x.cx = OPENEX_FILE_OPENED;
		break;
	case SHFL_FILE_CREATED:
		if (save_result) r->x.cx = OPENEX_FILE_CREATED;
		break;
	case SHFL_FILE_REPLACED:
		if (save_result) r->x.cx = OPENEX_FILE_REPLACED;
		break;
	}

	if (createparms.Handle == SHFL_HANDLE_NIL) {
		set_dos_err(r, DOS_ERROR_GEN_FAILURE);
		return;
	}

	data.files[openfile].root = root;
	data.files[openfile].handle = createparms.Handle;

	// Fill in the SFT
	map_shfl_info_to_dossft(sft, &createparms.Info);
	sft->open_mode = mode;
	sft->dev_info = 0x8040 | drive; // "Network drive, unwritten to"
	sft->f_pos = 0;
	set_sft_openfile_index(sft, openfile);

	clear_dos_err(r);
}

static void handle_close(union INTPACK __far *r)
{
	DOSSFT __far *sft = MK_FP(r->x.es, r->x.di);
	unsigned openfile = get_sft_openfile_index(sft);
	int32_t err;

	dlog_print("handle_close openfile=");
	dlog_printu(openfile);
	dlog_endline();

	if (!is_valid_openfile_index(openfile)) {
		set_dos_err(r, DOS_ERROR_INVALID_HANDLE);
		return;
	}

	err = vbox_shfl_close(&data.vb, data.hgcm_client_id,
	                      data.files[openfile].root, data.files[openfile].handle);
	if (err) {
		set_vbox_err(r, err);
		return;
	}

	data.files[openfile].root = SHFL_ROOT_NIL;
	data.files[openfile].handle = SHFL_HANDLE_NIL;

	clear_dos_err(r);
}

static void handle_read(union INTPACK __far *r)
{
	DOSSFT __far *sft = MK_FP(r->x.es, r->x.di);
	unsigned openfile = get_sft_openfile_index(sft);
	uint8_t __far *buffer = data.dossda->cur_dta;
	unsigned long offset = sft->f_pos;
	unsigned bytes = r->x.cx;
	int32_t err;

	dlog_print("handle_read openfile=");
	dlog_printu(openfile);
	dlog_print(" bytes=");
	dlog_printu(bytes);
	dlog_endline();

	if (!is_valid_openfile_index(openfile)) {
		set_dos_err(r, DOS_ERROR_INVALID_HANDLE);
		return;
	}

	err = vbox_shfl_read(&data.vb, data.hgcm_client_id,
	                     data.files[openfile].root, data.files[openfile].handle,
	                     offset, &bytes, buffer);
	if (err) {
		set_vbox_err(r, err);
		return;
	}

	dlog_print("handle_read bytes_read=");
	dlog_printu(bytes);
	dlog_endline();

	// Advance the file position
	sft->f_pos += bytes;

	r->x.cx = bytes;
	clear_dos_err(r);
}

static void handle_write(union INTPACK __far *r)
{
	DOSSFT __far *sft = MK_FP(r->x.es, r->x.di);
	unsigned openfile = get_sft_openfile_index(sft);
	uint8_t __far *buffer = data.dossda->cur_dta;
	unsigned long offset = sft->f_pos;
	unsigned bytes = r->x.cx;
	int32_t err;

	dlog_print("handle_write openfile=");
	dlog_printu(openfile);
	dlog_print(" bytes=");
	dlog_printu(bytes);
	dlog_endline();

	if (!is_valid_openfile_index(openfile)) {
		set_dos_err(r, DOS_ERROR_INVALID_HANDLE);
		return;
	}

	err = vbox_shfl_write(&data.vb, data.hgcm_client_id,
	                      data.files[openfile].root, data.files[openfile].handle,
	                      offset, &bytes, buffer);
	if (err) {
		set_vbox_err(r, err);
		return;
	}

	dlog_print("handle_write bytes_written=");
	dlog_printu(bytes);
	dlog_endline();

	// Advance the file position
	sft->f_pos += bytes;

	// Assume the file has grown if we've written past the end
	if (sft->f_pos > sft->f_size) sft->f_size = sft->f_pos;

	r->x.cx = bytes;
	clear_dos_err(r);
}

static void handle_lock(union INTPACK __far *r)
{
	DOSSFT __far *sft = MK_FP(r->x.es, r->x.di);
	unsigned openfile = get_sft_openfile_index(sft);
	bool unlock = r->h.bl ? true : false;
	unsigned numops = r->x.cx, i;
	unsigned flags = (unlock ? SHFL_LOCK_CANCEL : SHFL_LOCK_EXCLUSIVE)
	        | SHFL_LOCK_WAIT | SHFL_LOCK_PARTIAL;
	DOSLOCK __far *ops = MK_FP(r->x.ds, r->x.dx);
	int32_t err;

	dlog_print("handle_lock ");
	if (unlock) dlog_print("unlock");
	dlog_print(" numops=");
	dlog_printu(numops);
	dlog_endline();

	for (i = 0; i < numops; i++) {
		err = vbox_shfl_lock(&data.vb, data.hgcm_client_id,
		                     data.files[openfile].root, data.files[openfile].handle,
		                     ops[i].start_offset, ops[i].region_size, flags);
		if (err) {
			set_vbox_err(r, err);
			return;
		}
	}

	clear_dos_err(r);
}

static void handle_close_all(union INTPACK __far *r)
{
	int32_t err;
	unsigned i;

	dlog_puts("handle_close_all");

	for (i = 0; i < NUM_FILES; ++i) {
		if (data.files[i].root != SHFL_ROOT_NIL) {
			err = vbox_shfl_close(&data.vb, data.hgcm_client_id,
			                      data.files[i].root, data.files[i].handle);
			if (err) {
				dlog_puts("vbox error on close all...");
				// We'll leak this handle...
			}

			data.files[i].root = SHFL_ROOT_NIL;
			data.files[i].handle = SHFL_HANDLE_NIL;
		}
	}

	clear_dos_err(r);
}

static void handle_delete(union INTPACK __far *r)
{
	const char __far *path = data.dossda->fn1;
	int drive = drive_letter_to_index(path[0]);
	SHFLROOT root = data.drives[drive].root;
	int32_t err;

	dlog_print("handle_delete ");
	dlog_fprint(path);
	dlog_endline();

	copy_drive_relative_filename(&shflstr.shflstr, path);
	translate_filename_to_host(&shflstr.shflstr);

	err = vbox_shfl_remove(&data.vb, data.hgcm_client_id, root,
	                       &shflstr.shflstr, SHFL_REMOVE_FILE);
	if (err) {
		set_vbox_err(r, err);
		return;
	}

	clear_dos_err(r);
}

static void handle_getattr(union INTPACK __far *r)
{
	const char __far *path = data.dossda->fn1;
	int drive = drive_letter_to_index(path[0]);
	SHFLROOT root = data.drives[drive].root;
	int32_t err;

	dlog_print("handle_getattr ");
	dlog_fprint(path);
	dlog_endline();

	copy_drive_relative_dirname(&shflstr.shflstr, path);
	translate_filename_to_host(&shflstr.shflstr);

	memset(&createparms, 0, sizeof(SHFLCREATEPARMS));
	createparms.CreateFlags = SHFL_CF_LOOKUP;

	err = vbox_shfl_open(&data.vb, data.hgcm_client_id, root,
	                     &shflstr.shflstr, &createparms);
	if (err) {
		set_vbox_err(r, err);
		return;
	}
	switch (createparms.Result) {
	case SHFL_PATH_NOT_FOUND:
		set_dos_err(r, DOS_ERROR_PATH_NOT_FOUND);
		return;
	case SHFL_FILE_NOT_FOUND:
		set_dos_err(r, DOS_ERROR_PATH_NOT_FOUND);
		return;
	default:
		break;
	}

	map_shfl_info_to_getattr(r, &createparms.Info);
	clear_dos_err(r);
}


static int32_t open_search_dir(SHFLROOT root, const char __far *path)
{
	int32_t err;

	dlog_puts("open_search_dir");

	copy_drive_relative_dirname(&shflstr.shflstr, path);
	translate_filename_to_host(&shflstr.shflstr);

	memset(&createparms, 0, sizeof(SHFLCREATEPARMS));
	createparms.CreateFlags = SHFL_CF_DIRECTORY
	        | SHFL_CF_ACT_OPEN_IF_EXISTS | SHFL_CF_ACT_FAIL_IF_NEW
	        | SHFL_CF_ACCESS_READ;

	err = vbox_shfl_open(&data.vb, data.hgcm_client_id, root,
	                     &shflstr.shflstr, &createparms);
	if (err) {
		dlog_puts("open search dir failed");
		return err;
	}

	switch (createparms.Result) {
	case SHFL_PATH_NOT_FOUND:
		return VERR_PATH_NOT_FOUND;
	case SHFL_FILE_NOT_FOUND:
		return VERR_FILE_NOT_FOUND;
	default:
		break;
	}

	if (createparms.Handle == SHFL_HANDLE_NIL) {
		dlog_puts("open search dir returned no handle...");
		return VERR_INVALID_HANDLE;
	}

	data.files[SEARCH_DIR_FILE].root = root;
	data.files[SEARCH_DIR_FILE].handle = createparms.Handle;

	return 0;
}

static void close_search_dir()
{
	int32_t err;

	if (data.files[SEARCH_DIR_FILE].root == SHFL_ROOT_NIL) {
		// Already closed
		return;
	}

	err = vbox_shfl_close(&data.vb, data.hgcm_client_id,
	                      data.files[SEARCH_DIR_FILE].root,
	                      data.files[SEARCH_DIR_FILE].handle);
	if (err) {
		dlog_puts("vbox error on close_search_dir, ignoring");
	}

	data.files[SEARCH_DIR_FILE].root = SHFL_ROOT_NIL;
	data.files[SEARCH_DIR_FILE].handle = SHFL_HANDLE_NIL;
}

static inline bool is_search_dir_open()
{
	return data.files[SEARCH_DIR_FILE].root != SHFL_ROOT_NIL;
}

static int32_t find_volume_label(SHFLROOT root)
{
	DOSDIR __far *found_file = &data.dossda->found_file;
	int32_t err;

	shflstring_clear(&shflstr.shflstr);

	err = vbox_shfl_query_map_name(&data.vb, data.hgcm_client_id, root, &shflstr.shflstr);
	if (err) return err;

	translate_filename_from_host(&shflstr.shflstr);

	dlog_print("label : ");
	dlog_fprint(shflstr.buf);
	dlog_endline();

	copy_to_8_3_filename(found_file->filename, &shflstr.shflstr);
	found_file->attr = _A_VOLID | _A_HIDDEN;

	return 0;
}

static int32_t find_next_from_vbox(uint8_t search_attr)
{
	DOSDIR __far *found_file = &data.dossda->found_file;
	int32_t err;

	if (!is_search_dir_open()) {
		dlog_puts("find_next called, but no opendir handle");
		return VERR_INVALID_HANDLE;
	}

	shfldirinfo.dirinfo.name.u16Size = sizeof(shfldirinfo.buf);

	while (1) { // Loop until we have a valid file (or an error)
		unsigned size = sizeof(shfldirinfo), resume = 0, count = 0;
		dlog_puts("calling vbox list");

		err = vbox_shfl_list(&data.vb, data.hgcm_client_id,
		                     data.files[SEARCH_DIR_FILE].root, data.files[SEARCH_DIR_FILE].handle,
		                     SHFL_LIST_RETURN_ONE, &size, &shflstr.shflstr, &shfldirinfo.dirinfo,
		                     &resume, &count);
		if (err) {
			return err;
		}

		if (count != 1) {
			return VERR_IO_BAD_LENGTH;
		}

		dlog_print("got diritem name=");
		dlog_fprint(shfldirinfo.dirinfo.name.ach);
		dlog_print(" sfnLen=");
		dlog_printu(shfldirinfo.dirinfo.cucShortName);
		dlog_endline();

		// TODO Use the short filename if available from a windows host

		translate_filename_from_host(&shfldirinfo.dirinfo.name);

		if (!copy_to_8_3_filename(found_file->filename, &shfldirinfo.dirinfo.name)) {
			dlog_puts("hiding file with long filename");
			continue;
		}
		map_shfl_info_to_dosdir(found_file, &shfldirinfo.dirinfo.Info);

		if (found_file->attr & ~search_attr) {
			continue; // Skip this one
		}

		break;
	};

	return 0;
}

static void handle_find(union INTPACK __far *r)
{
	const char __far *path = data.dossda->fn1;
	int drive = drive_letter_to_index(path[0]);
	SHFLROOT root = data.drives[drive].root;
	DOSDIR __far *found_file = &data.dossda->found_file;
	uint8_t search_attr;
	int32_t err;

	if (r->h.al == DOS_FN_FIND_FIRST) {
		search_attr = data.dossda->search_attr;

		dlog_print("find_first path=");
		dlog_fprint(path);
		dlog_print(" attr=");
		dlog_printx(search_attr);
		dlog_endline();

		close_search_dir();
		err = open_search_dir(root, path);
		if (err) {
			set_vbox_err(r, err);
			return;
		}

		copy_drive_relative_filename(&shflstr.shflstr, path);
		translate_filename_to_host(&shflstr.shflstr);
		fix_wildcards(&shflstr.shflstr);

		data.dossda->sdb.drive_letter = 0x80 | drive;
		data.dossda->sdb.search_attr = search_attr;
		_fmemset(data.dossda->sdb.search_templ, ' ', 8+3);
		data.dossda->sdb.dir_entry = 0;
		data.dossda->sdb.par_clstr = 0;

		if (search_attr & _A_VOLID) {
			// Simulate an initial entry: volume label
			dlog_puts("search volid");
			// DOS wants volume label; optimize that
			err = find_volume_label(root);
			if (err) {
				dlog_puts("search volid err");
				set_vbox_err(r, err);
				return;
			}
			dlog_puts("search volid OK");
			clear_dos_err(r);
			return;
		}
	} else {
		search_attr = data.dossda->sdb.search_attr;

		dlog_print("find_next");
		dlog_print(" attr=");
		dlog_printx(search_attr);
		dlog_endline();
	}

	found_file->attr = 0;

	err = find_next_from_vbox(search_attr);
	if (err) {
		if (err == VERR_NO_MORE_FILES) {
			dlog_puts("no more files");
		}
		close_search_dir();
		set_vbox_err(r, err);
		return;
	}

	dlog_print("accepted file name='");
	dlog_fnprint(&found_file->filename[0], 8);
	dlog_putc(' ');
	dlog_fnprint(&found_file->filename[8], 3);
	dlog_print("' attr=");
	dlog_printx(found_file->attr);
	dlog_endline();

	clear_dos_err(r);
}

static void handle_chdir(union INTPACK __far *r)
{
	const char __far *path = data.dossda->fn1;
	int drive = drive_letter_to_index(path[0]);
	SHFLROOT root = data.drives[drive].root;
	int32_t err;

	dlog_print("handle_chdir to ");
	dlog_fprint(path);
	dlog_endline();

	// Just have to check if the directory exists
	copy_drive_relative_filename(&shflstr.shflstr, path);
	translate_filename_to_host(&shflstr.shflstr);

	memset(&createparms, 0, sizeof(SHFLCREATEPARMS));
	createparms.CreateFlags = SHFL_CF_LOOKUP;

	err = vbox_shfl_open(&data.vb, data.hgcm_client_id, root,
	                     &shflstr.shflstr, &createparms);
	if (err) {
		set_vbox_err(r, err);
		return;
	}

	switch (createparms.Result) {
	case SHFL_PATH_NOT_FOUND:
	case SHFL_FILE_NOT_FOUND:
		set_dos_err(r, DOS_ERROR_PATH_NOT_FOUND);
		return;
	default:
		break;
	}

	// Also check whether it is really a directory
	if (!(map_shfl_attr_to_dosattr(&createparms.Info.Attr) & _A_SUBDIR)) {
		set_dos_err(r, DOS_ERROR_PATH_NOT_FOUND);
		return;
	}

	clear_dos_err(r);
}

static void handle_mkdir(union INTPACK __far *r)
{
	const char __far *path = data.dossda->fn1;
	int drive = drive_letter_to_index(path[0]);
	SHFLROOT root = data.drives[drive].root;
	int32_t err;

	dlog_print("handle_mkdir ");
	dlog_fprint(path);
	dlog_endline();

	copy_drive_relative_filename(&shflstr.shflstr, path);
	translate_filename_to_host(&shflstr.shflstr);

	memset(&createparms, 0, sizeof(SHFLCREATEPARMS));
	createparms.CreateFlags = SHFL_CF_DIRECTORY
	        | SHFL_CF_ACT_FAIL_IF_EXISTS | SHFL_CF_ACT_CREATE_IF_NEW;

	err = vbox_shfl_open(&data.vb, data.hgcm_client_id, root,
	                     &shflstr.shflstr, &createparms);
	if (err) {
		set_vbox_err(r, err);
		return;
	}

	switch (createparms.Result) {
	case SHFL_PATH_NOT_FOUND:
	case SHFL_FILE_NOT_FOUND:
		set_dos_err(r, DOS_ERROR_PATH_NOT_FOUND);
		return;
	case SHFL_FILE_EXISTS:
		set_dos_err(r, DOS_ERROR_FILE_EXISTS);
		return;
	default:
		break;
	}

	clear_dos_err(r);
}

static void handle_rmdir(union INTPACK __far *r)
{
	const char __far *path = data.dossda->fn1;
	int drive = drive_letter_to_index(path[0]);
	SHFLROOT root = data.drives[drive].root;
	int32_t err;

	dlog_print("handle_rmdir ");
	dlog_fprint(path);
	dlog_endline();

	copy_drive_relative_filename(&shflstr.shflstr, path);
	translate_filename_to_host(&shflstr.shflstr);

	err = vbox_shfl_remove(&data.vb, data.hgcm_client_id, root,
	                       &shflstr.shflstr, SHFL_REMOVE_DIR);
	if (err) {
		set_vbox_err(r, err);
		return;
	}

	clear_dos_err(r);
}

static bool int2f_11_handler(union INTPACK r)
#pragma aux int2f_11_handler "*" parm caller [] value [al] modify [ax bx cx dx si di es gs fs]
{
	if (r.h.ah != 0x11) return false; // Only interested in network redirector functions
	if (r.h.al == 0xff && r.x.bx == 0x5742 && r.x.cx == 0x5346) {
		// These are the magic numbers to our private "Get TSR data" function
		dlog_puts("Get TSR data");
		r.x.es = get_ds();
		r.x.di = FP_OFF(&data);
		r.x.bx = 0x5444;
		r.x.cx = 1;
		return true;
	}

	dlog_print("2f al=");
	dlog_printx(r.h.al);
	dlog_endline();

	// Handle special functions that target all redirectors first
	switch (r.h.al) {
	case DOS_FN_CLOSE_ALL:
		handle_close_all(&r);
		return false; // Let others do the same
	}

	// Now handle normal functions if they refer to our mounted drives
	if (!is_call_for_mounted_drive(&r)) {
		return false;
	}

	switch (r.h.al) {
	case DOS_FN_CLOSE:
		handle_close(&r);
		return true;
	case DOS_FN_CREATE:
	case DOS_FN_OPEN:
	case DOS_FN_OPEN_EX:
		handle_create_open_ex(&r);
		return true;
	case DOS_FN_READ:
		handle_read(&r);
		return true;
	case DOS_FN_WRITE:
		handle_write(&r);
		return true;
	case DOS_FN_LOCK:
		handle_lock(&r);
		return true;
	case DOS_FN_DELETE:
		handle_delete(&r);
		return true;
	case DOS_FN_GET_FILE_ATTR:
		handle_getattr(&r);
		return true;
	case DOS_FN_FIND_FIRST:
	case DOS_FN_FIND_NEXT:
		handle_find(&r);
		return true;
	case DOS_FN_CHDIR:
		handle_chdir(&r);
		return true;
	case DOS_FN_MKDIR:
		handle_mkdir(&r);
		return true;
	case DOS_FN_RMDIR:
		handle_rmdir(&r);
		return true;
	case DOS_FN_GET_DISK_FREE:
		// We don't support this
		set_dos_err(&r, DOS_ERROR_INVALID_FUNCTION);
		return true;
	}

	return false;
}

void __declspec(naked) __far int2f_isr(void)
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

		call int2f_11_handler
		test al, al
		jnz handled

		pop gs
		pop fs
		pop es
		pop ds
		popa

		; Jump to the next handler in the chain
		jmp dword ptr cs:[data + 0] ; wasm doesn't support structs, this is data.prev_int2f_handler

	handled:
		pop gs
		pop fs
		pop es
		pop ds
		popa
		iret
	}
}

static LPTSRDATA int2f_get_tsr_data(void);
#pragma aux int2f_get_tsr_data = \
	"mov ax, 0x11ff" \
	"mov bx, 0x5742" /* Add magic numbers */ \
	"mov cx, 0x5346" \
	"int 0x2f"   \
	"cmp bx, 0x5444" /* Test output magic number */ \
	"jne fail" \
	"cmp cx, 1" \
	"jne fail" \
	"jmp end" \
	"fail:" \
	"xor ax, ax" \
	"mov es, ax" \
	"mov di, ax" \
	"end:" \
	__value [es di] \
	__modify [ax bx cx]

LPTSRDATA __far get_tsr_data(bool installed)
{
	if (installed) {
		return int2f_get_tsr_data();
	} else {
		return MK_FP(get_cs(), FP_OFF(&data));
	}
}

int resident_end;