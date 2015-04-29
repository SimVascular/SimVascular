ifeq ($(CLUSTER), x64_linux)
    PARASOLID_TOP     = $(LICENSED_SOFTWARE_TOPLEVEL)/parasolid-26.1/intel_linux/base
    PARASOLID_INCDIR  = -I$(PARASOLID_TOP)
    PARASOLID_LIBDIR  = -L$(PARASOLID_TOP)
    PARASOLID_SO_PATH = $(PARASOLID_TOP)/shared_object
    PARASOLID_DLLS    = $(PARASOLID_SO_PATH)/libpskernel.$(SOEXT)
    P_SCHEMA          = $(PARASOLID_TOP)/schema
    PARASOLID_LIBS    = -L$(PARASOLID_SO_PATH) -lpskernel
endif
