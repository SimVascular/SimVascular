ifeq ($(CLUSTER), x64_linux)
    SPARSE_TOP     = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/sparse-1.4
    SPARSE_INCDIR  = -I $(SPARSE_TOP)
    SPARSE_LIBS    = -L $(SPARSE_TOP) -lsparse
endif
