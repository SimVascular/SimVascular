# ----------------------------------
# Platform-specific compiler options
# ----------------------------------

ifeq ($(CLUSTER), x64_cygwin)
    F90             = x86_64-w64-mingw32-gfortran.exe -cpp
ifeq ($(MAKE_FULLY_OPTIMIZED),1)
    DEBUG_FFLAGS    = 
    OPT_FFLAGS      = -O2
else
  ifeq ($(MAKE_OPTIMIZED),1)
    DEBUG_FFLAGS    =  
    OPT_FFLAGS      = -g -O2
  else
    DEBUG_FFLAGS    = -g -fcheck=all
  endif
endif
    GLOBAL_FFLAGS   = $(BUILDFLAGS) $(DEBUG_FFLAGS) $(OPT_FFLAGS) -ffixed-line-length-132 -fmax-stack-var-size=256
ifeq ($(LINK_WITH_DEBUG),1)
    GLOBAL_LFLAGS   += -g
endif
endif

     # required by msmpi headers
     GLOBAL_FFLAGS   += -fno-range-check

     F90_LIBS        = -lgfortran
