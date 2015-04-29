ifeq ($(CLUSTER), x86_cygwin)
    NSPCG_TOP     = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/nspcg
    NSPCG_INCDIR  = -I $(NSPCG_TOP)
    NSPCG_LIBS    = -LIBPATH:$(NSPCG_TOP) libnspcg.lib
endif
