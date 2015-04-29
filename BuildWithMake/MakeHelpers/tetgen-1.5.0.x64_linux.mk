ifeq ($(CLUSTER), x64_linux)
    TETGEN_TOP        = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/tetgen-1.5.0
    TETGEN_INCDIR   = -I$(TETGEN_TOP)
    TETGEN_LIBS     = -L$(TETGEN_TOP) -ltet
    TETGEN150       = 1
endif
