ifeq ($(CLUSTER), x64_cygwin)
    BUILDFLAGS     += -D__NON_STD_TCL_INSTALL
    TCL_BASE       = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/tcltk-8.6.4
    TK_BASE        = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/tcltk-8.6.4
    TCLTK_INCDIR   = -I$(TCL_BASE)/include -I$(TK_BASE)/include
    TCLTK_LIBDIR   = $(LIBPATH_COMPILER_FLAG)$(TCL_BASE)/lib $(LIBPATH_COMPILER_FLAG)$(TK_BASE)/lib
    TCLTK_DLLS     = $(TCL_BASE)/bin/tcl86.$(SOEXT) $(TCL_BASE)/bin/tk86.$(SOEXT)
    TCLTK_LIBS     = $(TCLTK_LIBDIR) \
                     $(LIBFLAG)tcl86$(LIBLINKEXT) $(LIBFLAG)tk86$(LIBLINKEXT) \
                     $(LIBFLAG)user32$(LIBLINKEXT) $(LIBFLAG)advapi32$(LIBLINKEXT) \
                     $(LIBFLAG)gdi32$(LIBLINKEXT)  $(LIBFLAG)comdlg32$(LIBLINKEXT) \
                     $(LIBFLAG)imm32$(LIBLINKEXT)  $(LIBFLAG)comctl32$(LIBLINKEXT) \
                     $(LIBFLAG)shell32$(LIBLINKEXT) $(LIBFLAG)Shlwapi$(LIBLINKEXT)
    # Shlwapi was added to make mingw32 compile happy
    TCLTK_LIBS     +=$(LIBFLAG)Shlwapi$(LIBLINKEXT)
    TKCXIMAGE_BASE = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/tkcximage-0.98.9/tcltk-8.6.4
    TKCXIMAGE_DLL  = $(TKCXIMAGE_BASE)/bin/Tkcximage.$(SOEXT)
    TCLTK_SO_PATH  = $(TCL_BASE)/bin
    TCL_LIBRARY    = $(TCL_BASE)/lib/tcl8.6
    TK_LIBRARY     = $(TCL_BASE)/lib/tk8.6
    TCLSH          = $(TCL_BASE)/bin/tclsh86.exe
endif
