ifeq ($(CLUSTER), x64_cygwin)
    MESHSIM_TOP      = $(LICENSED_SOFTWARE_TOPLEVEL)/meshsim-9.0-150704
    MESHSIM_INCDIR   = -I$(MESHSIM_TOP)/include
    MESHSIM_LIBDIR   = $(MESHSIM_TOP)/lib/x64_win_vc12
    MESHSIM_DEFS     = -DSimDeprecated70
    MESHSIM_LIBS     = $(LIBPATH_COMPILER_FLAG)$(MESHSIM_LIBDIR) \
                       $(LIBFLAG)SimAdvMeshing$(LIBLINKEXT) \
                       $(LIBFLAG)SimMeshing$(LIBLINKEXT) \
                       $(LIBFLAG)SimMeshTools$(LIBLINKEXT) \
                       $(LIBFLAG)SimModel$(LIBLINKEXT)
    ifeq ($(MESHSIM_MODELER),parasolid)
      MESHSIM_LIBS   := $(MESHSIM_LIBS) $(LIBFLAG)SimParasolid270$(LIBLINKEXT)
    endif 
    ifeq ($(MAKE_WITH_MESHSIM_DISCRETE_MODEL),1)
      MESHSIM_LIBS   := $(MESHSIM_LIBS) $(LIBFLAG)SimDiscrete$(LIBLINKEXT)
    endif 
    MESHSIM_LIBS     := $(MESHSIM_LIBS) $(LIBFLAG)Ws2_32$(LIBLINKEXT) \
                        $(LIBFLAG)rpcrt4$(LIBLINKEXT) \
                        $(LIBFLAG)ole32$(LIBLINKEXT) $(LIBFLAG)psapi$(LIBLINKEXT)
    MESHSIM_LIBS     += $(LIBFLAG)SimMeshTools$(LIBLINKEXT) \
                        $(LIBFLAG)SimMeshing$(LIBLINKEXT) \
                        $(LIBFLAG)SimModel$(LIBLINKEXT)
endif
