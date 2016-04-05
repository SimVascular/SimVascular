ifeq ($(CLUSTER), x64_cygwin)
    MESHSIM_TOP      = $(LICENSED_SOFTWARE_TOPLEVEL)/meshsim-9.0-150704
    MESHSIM_INCDIR   = -I$(MESHSIM_TOP)/include
    ifeq ($(SV_USE_MESHSIM_SHARED),1)
      MESHSIM_LIBDIR   = $(MESHSIM_TOP)/lib/x64_win_vc12_shared
    else
      MESHSIM_LIBDIR   = $(MESHSIM_TOP)/lib/x64_win_vc12
    endif
    MESHSIM_SO_PATH  = $(MESHSIM_LIBDIR)
    MESHSIM_LIBS     = $(LIBPATH_COMPILER_FLAG)$(MESHSIM_LIBDIR) \
                       $(LIBFLAG)SimAdvMeshing$(LIBLINKEXT) \
                       $(LIBFLAG)SimMeshing$(LIBLINKEXT) \
                       $(LIBFLAG)SimMeshTools$(LIBLINKEXT) \
                       $(LIBFLAG)SimModel$(LIBLINKEXT)
    ifeq ($(MESHSIM_MODELER),parasolid)
      MESHSIM_LIBS   := $(MESHSIM_LIBS) $(LIBFLAG)SimParasolid270$(LIBLINKEXT)
    endif 
    ifeq ($(SV_USE_MESHSIM_DISCRETE_MODEL),1)
      MESHSIM_LIBS   := $(MESHSIM_LIBS) $(LIBFLAG)SimDiscrete$(LIBLINKEXT)
    endif 
    MESHSIM_LIBS     := $(MESHSIM_LIBS) $(LIBFLAG)Ws2_32$(LIBLINKEXT) \
                        $(LIBFLAG)rpcrt4$(LIBLINKEXT) \
                        $(LIBFLAG)ole32$(LIBLINKEXT) $(LIBFLAG)psapi$(LIBLINKEXT)
    MESHSIM_LIBS     += $(LIBFLAG)SimMeshTools$(LIBLINKEXT) \
                        $(LIBFLAG)SimMeshing$(LIBLINKEXT) \
                        $(LIBFLAG)SimModel$(LIBLINKEXT)
endif
