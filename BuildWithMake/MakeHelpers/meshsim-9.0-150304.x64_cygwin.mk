ifeq ($(CLUSTER), x64_cygwin)
    MESHSIM_TOP      = $(LICENSED_SOFTWARE_TOPLEVEL)/meshsim-9.0-150304
    MESHSIM_INCDIR   = -I$(MESHSIM_TOP)/include
    MESHSIM_LIBDIR   = $(MESHSIM_TOP)/lib/x64_win_vc10
    MESHSIM_DEFS     = -DSimDeprecated70
    MESHSIM_LIBS     = $(LIBPATH_COMPILER_FLAG)$(MESHSIM_LIBDIR) \
                       $(LIBFLAG)SimAdvMeshing.lib $(LIBFLAG)SimMeshTools.lib \
                       $(LIBFLAG)SimMeshing.lib $(LIBFLAG)SimModel.lib
    ifeq ($(MESHSIM_MODELER),parasolid)
      MESHSIM_LIBS   := $(MESHSIM_LIBS) $(LIBFLAG)SimParasolid260.lib
    endif 
    ifeq ($(MAKE_WITH_MESHSIM_DISCRETE_MODEL),1)
      MESHSIM_LIBS   := $(MESHSIM_LIBS) $(LIBFLAG)SimDiscrete.lib
    endif 
    MESHSIM_LIBS     := $(MESHSIM_LIBS) $(LIBFLAG)Ws2_32.lib $(LIBFLAG)rpcrt4.lib $(LIBFLAG)ole32.lib $(LIBFLAG)psapi.lib
endif
