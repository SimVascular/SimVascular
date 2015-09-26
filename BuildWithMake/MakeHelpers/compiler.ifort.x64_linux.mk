ifeq ($(CLUSTER), x64_linux)
    F90             = ifort -threads -fpp
ifeq ($(MAKE_FULLY_OPTIMIZED),1)
    OPT_FFLAGS      = -O3 -fPIC
    DEBUG_FFLAGS    =
else
  ifeq ($(MAKE_OPTIMIZED),1)
    OPT_FFLAGS      = -O2 -fPIC
    DEBUG_FFLAGS    =
  else
    OPT_FFLAGS      =
    DEBUG_FFLAGS    = -O0 -fPIC -debug -g -check -traceback 
  endif
endif
    GLOBAL_FFLAGS   = $(BUILDFLAGS) $(DEBUG_FFLAGS) $(OPT_FFLAGS) \
                      -W0 -132 -heap-arrays 256 
ifeq ($(LINK_WITH_DEBUG),1)
    GLOBAL_LFLAGS   += -g
endif  
#    F90_LIBS        = -lifcoremt -lifport -limf -lintlc -ldl
    F90_LIBS        = -lifcoremt -lifport -limf
endif
