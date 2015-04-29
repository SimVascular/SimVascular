ifeq ($(CLUSTER), x64_linux)
    METIS_TOP     = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/metis-4.0.1
    METIS_INCDIR  = -I $(METIS_TOP)
    METIS_LIBS    = -L$(METIS_TOP) -lmetis
endif
