ifeq ($(CLUSTER), x64_linux)
    MESHSIM_TOP      = $(LICENSED_SOFTWARE_TOPLEVEL)/meshsim-9.0-151017
    MESHSIM_INCDIR   = -I$(MESHSIM_TOP)/include
    MESHSIM_LIBDIR   = $(MESHSIM_TOP)/lib/x64_rhel6_gcc44
    MESHSIM_LIBS     = -L$(MESHSIM_LIBDIR) \
                       -lSimAdvMeshing -lSimMeshing -lSimMeshTools
    MESHSIM_SO_PATH  = $(MESHSIM_LIBDIR)
    ifeq ($(MESHSIM_MODELER),parasolid)
      MESHSIM_LIBS   := $(MESHSIM_LIBS) -lSimParasolid260
    endif 
    ifeq ($(MAKE_WITH_MESHSIM_DISCRETE_MODEL),1)
      MESHSIM_LIBS   := $(MESHSIM_LIBS) -lSimDiscrete
    endif 
    MESHSIM_LIBS     += -lSimMeshTools -lSimModel
endif
