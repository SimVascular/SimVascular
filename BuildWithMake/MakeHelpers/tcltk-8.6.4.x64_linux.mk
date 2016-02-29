ifeq ($(CLUSTER), x64_linux)
    BUILDFLAGS     += -D__NON_STD_TCL_INSTALL
    TCL_BASE       = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/tcltk-8.6.4
    TK_BASE        = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/tcltk-8.6.4
    TCLTK_INCDIR   = -I$(TCL_BASE)/include -I$(TK_BASE)/include
    TCLTK_LIBDIR   = -L$(TCL_BASE)/lib -L$(TK_BASE)/lib
    TCLTK_DLLS     = $(TCL_BASE)/bin/tcl8.6.$(SOEXT) $(TCL_BASE)/bin/tk8.6.$(SOEXT)
    TCLTK_LIBS     = $(TCLTK_LIBDIR) -ltcl8.6 -ltk8.6
    TKCXIMAGE_BASE = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/tkcximage-0.98.9/tcltk-8.6.4
    TKCXIMAGE_DLL  = $(TKCXIMAGE_BASE)/bin/Tkcximage.$(SOEXT)
    TCLTK_SO_PATH  = $(TCL_BASE)/lib
    TCL_LIBRARY    = $(TCL_BASE)/lib/tcl8.6
    TK_LIBRARY     = $(TCL_BASE)/lib/tk8.6
    TCLSH          = $(TCL_BASE)/bin/tclsh8.6
endif
