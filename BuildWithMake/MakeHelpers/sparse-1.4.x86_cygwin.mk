ifeq ($(CLUSTER), x86_cygwin)
    SPARSE_TOP     = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/sparse-1.4
    SPARSE_INCDIR  = -I $(SPARSE_TOP)
    SPARSE_LIBS    = -LIBPATH:$(SPARSE_TOP) libsparse.lib
endif
