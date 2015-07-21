# ----------------------------------
# Platform-specific compiler options
# ----------------------------------

ifeq ($(CLUSTER), x64_cygwin)
    F90             = ifort -fpp
ifeq ($(MAKE_FULLY_OPTIMIZED),1)
    DEBUG_FFLAGS    = 
    OPT_FFLAGS      = /MD /Ox
else
  ifeq ($(MAKE_OPTIMIZED),1)
    DEBUG_FFLAGS    =  
    OPT_FFLAGS      = /MD /Zi /O2
  else
    DEBUG_FFLAGS    = /MD /Od /debug:all /debug:all /check:all -traceback
  endif
endif
    GLOBAL_FFLAGS   = $(BUILDFLAGS) $(DEBUG_FFLAGS) $(OPT_FFLAGS) /W0 /4L132
ifeq ($(LINK_WITH_DEBUG),1)
    GLOBAL_LFLAGS   += /DEBUG
endif
endif
