ifeq ($(CLUSTER), x64_macosx)
    F90             = gfortran -pthread -cpp
ifeq ($(MAKE_FULLY_OPTIMIZED),1)
    OPT_FFLAGS      = -O3 -fPIC
    DEBUG_FFLAGS    =
else
  ifeq ($(MAKE_OPTIMIZED),1)
    OPT_FFLAGS      = -O2 -fPIC
    DEBUG_FFLAGS    =
  else
    OPT_FFLAGS      =
    DEBUG_FFLAGS    = -O0 -g
  endif
endif
    GLOBAL_FFLAGS   = $(BUILDFLAGS) $(DEBUG_FFLAGS) $(OPT_FFLAGS) -ffixed-line-length-132 -fmax-stack-var-size=256
    F90_LIBS        = $(wordlist 2,99,$(shell mpif90 -link_info)) -lm
ifeq ($(LINK_WITH_DEBUG),1)
    GLOBAL_LFLAGS   += -g
endif
endif
