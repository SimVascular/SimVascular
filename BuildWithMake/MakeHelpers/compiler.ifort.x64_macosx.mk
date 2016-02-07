ifeq ($(CLUSTER), x64_macosx)
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
#    F90_LIBS        = -lifcoremt -lifport -limf
    F90_LIBS        = /opt/intel/compilers_and_libraries_2016/mac/lib/libifcoremt.a \
                      /opt/intel/compilers_and_libraries_2016/mac/lib/libifport.a \
                      /opt/intel/compilers_and_libraries_2016/mac/lib/libimf.a \
                      /opt/intel/compilers_and_libraries_2016/mac/lib/libintlc.dylib \
                      /opt/intel/compilers_and_libraries_2016/mac/lib/libsvml.a 
endif
