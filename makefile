# This is an Open Watcom wmake makefile, not GNU make.
# Assuming you have sourced `owsetenv` beforehand.

# All binaries to build
bins = vbmouse.exe vbsf.exe vbmouse.drv

# Inf files
infs = oemsetup.inf

# Object files for vbmouse
mousedos_objs = mousetsr.obj mousmain.obj kitten.obj vbox.obj
mousew16_objs = mousew16.obj

# Object files for vbsf
sfdos_objs = sftsr.obj sfmain.obj kitten.obj vbox.obj

# Compiler arguments for DOS
dos_cflags = -bt=dos -ms -6 -osi -w3 -wcd=202
# -ms to use small memory model (though sometimes ss != ds...)
# -osi to optimize for size, put intrinsics inline (to avoid runtime calls)
# -w3 enables warnings
# -wcd=202 disables the unreferenced function warning (e.g., for inline functions in headers)

# Compiler arguments for DOS TSR files
dostsr_cflags = $(dos_cflags) -DIN_TSR -zu -s -g=RES_GROUP -nd=RES -nt=RES_TEXT -nc=RES_CODE
# -s to disable stack checks, since it inserts calls to the runtime from the TSR part
# -zu since ss != ds on the TSR

# Compiler arguments for W16 .DLL/.DRV files
w16dll_cflags = -bt=windows -bd -mc -zu -s -6 -osi -w3 -wcd=202
# -bd to build DLL
# -mc to use compact memory model (far data pointers, ss != ds in a DLL)
# -zu for DLL calling convention (ss != ds)
# -s to disable stack checks, since the runtime uses MessageBox() to abort (which we can't call from mouse.drv)

# Full compiler command lines
compile_dos = *wcc -fo=$^@ $(dos_cflags) $[@
compile_dostsr = *wcc -fo=$^@ $(dostsr_cflags) $[@
compile_w16dll = *wcc -fo=$^@ $(w16dll_cflags) $[@

.BEFORE:
	# We need DOS and Windows headers, not host platform's
	set include=$(%watcom)/h/win;$(%watcom)/h

all: $(bins) .SYMBOLIC

# DOS mouse driver
vbmouse.exe: vbmouse.lnk $(mousedos_objs)
	*wlink @$[@ name $@ file { $(mousedos_objs) }

mousetsr.obj: mousetsr.c .AUTODEPEND
	$(compile_dostsr)

mousmain.obj: mousmain.c .AUTODEPEND
	$(compile_dos)

# Windows 3.x mouse driver
vbmouse.drv: mousew16.lnk $(mousew16_objs)
	*wlink @$[@ name $@ file { $(mousew16_objs) }

mousew16.obj: mousew16.c .AUTODEPEND
	$(compile_w16dll)

# DOS shared folders
vbsf.exe: vbsf.lnk $(sfdos_objs)
	*wlink @$[@ name $@ file { $(sfdos_objs) }

sftsr.obj: sftsr.c .AUTODEPEND
	$(compile_dostsr)

sfmain.obj: sfmain.c .AUTODEPEND
	$(compile_dos)

# Auxiliary object files
vbox.obj: vbox.c .AUTODEPEND
	$(compile_dos)

kitten.obj: kitten.c .AUTODEPEND
	$(compile_dos)

# Other targets
clean: .SYMBOLIC
	rm -f vbmouse.exe vbmouse.drv vbsf.exe vbados.flp *.obj *.map

vbados.flp:
	mformat -C -f 1440 -v VBADOS -i $^@ ::
	mcopy -i $^@ nls/*.tbl ::
	mcopy -i $^@ nls/vbsf.* nls/vbmouse.* ::

# Build a floppy image containing the driver
flp: vbados.flp $(bins) $(infs) .SYMBOLIC
	mcopy -i vbados.flp -o $(bins) $(infs) ::

# Build a zip with the driver binaries
zip: vbmouse.exe vbmouse.drv oemsetup.inf vbsf.exe .SYMBOLIC
	zip --DOS-names -fz- -j vbados.zip nls/*.tbl nls/vbsf.* nls/vbmouse.*
	zip --DOS-names -fz- vbados.zip $(bins) $(infs)
