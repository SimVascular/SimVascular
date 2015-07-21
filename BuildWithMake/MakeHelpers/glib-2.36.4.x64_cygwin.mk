ifeq ($(CLUSTER), x64_cygwin)
    GLIB_TOPLEVEL = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/glib-2.36.4
    GLIB_INCDIR   = -I$(GLIB_TOPLEVEL)/include/glib-2.0 -I$(GLIB_TOPLEVEL)/lib/glib-2.0/include
    GLIB_LIBDIR   = $(GLIB_TOPLEVEL)/lib
    GLIB_SO_PATH  = $(GLIB_TOPLEVEL)/bin
    GLIB_LIBS     = $(LIBPATH_COMPILER_FLAG)$(GLIB_LIBDIR) $(LIBFLAG)glib-2.0.lib
    GLIB_DLLS     = $(GLIB_TOPLEVEL)/bin/glib*.$(SOEXT)
endif
