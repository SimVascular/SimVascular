ifeq ($(CLUSTER), x64_cygwin)
    TETGEN_TOP        = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/tetgen-1.5.0
    TETGEN_INCDIR   = -I$(TETGEN_TOP)
    TETGEN_LIBS     = /LIBPATH:$(TETGEN_TOP) libtet.lib
    TETGEN150       = 1
endif
