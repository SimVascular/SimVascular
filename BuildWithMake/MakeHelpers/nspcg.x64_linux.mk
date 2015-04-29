ifeq ($(CLUSTER), x64_linux)
    NSPCG_TOP     = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/nspcg
    NSPCG_INCDIR  = -I $(NSPCG_TOP)
    NSPCG_LIBS    = -L$(NSPCG_TOP) -lnspcg
endif
