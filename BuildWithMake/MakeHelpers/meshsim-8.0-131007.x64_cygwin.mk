ifeq ($(CLUSTER), x64_cygwin)
    MESHSIM_TOP      = $(LICENSED_SOFTWARE_TOPLEVEL)/meshsim-8.0-131007
    MESHSIM_INCDIR   = -I$(MESHSIM_TOP)/include
    MESHSIM_LIBDIR   = $(MESHSIM_TOP)/lib/x64_win_vc10
    MESHSIM_DEFS     = -DSimDeprecated70
    MESHSIM_LIBS     = /LIBPATH:$(MESHSIM_LIBDIR) \
                       SimAdvMeshing.lib SimMeshTools.lib \
                       SimMeshing.lib SimModel.lib
    ifeq ($(MESHSIM_MODELER),parasolid)
      MESHSIM_LIBS   := $(MESHSIM_LIBS) SimParasolid240.lib
    endif 
    ifeq ($(MAKE_WITH_MESHSIM_DISCRETE_MODEL),1)
      MESHSIM_LIBS   := $(MESHSIM_LIBS) SimDiscrete.lib
    endif 
    MESHSIM_LIBS     := $(MESHSIM_LIBS) Ws2_32.lib rpcrt4.lib ole32.lib
endif
