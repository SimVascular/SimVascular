ifeq ($(COMPILER_VERSION), gnu)
    SHELL           =/bin/sh
    CXX             = g++ -pthread -w
    CC              = gcc -pthread -w
    F90             = gfortran -cpp
    CXXDEP          = $(CXX) -MM
    CCDEP           = $(CC) -MM
    GLOBAL_CXXFLAGS += -fpermissive
    GLOBAL_FFLAGS   = $(BUILDFLAGS) $(DEBUG_FFLAGS) $(OPT_FFLAGS) -ffixed-line-length-132
    CC_LIBS         = -L$(INTEL_COMPILER_SO_PATH) -lirc -limf -lsvml
    CXX_LIBS        = -L$(INTEL_COMPILER_SO_PATH) -lirc -limf -lsvml
    F90_LIBS        = -L$(INTEL_COMPILER_SO_PATH) -lirc -limf -lsvml -lifcore -lifport -lgfortran -lm
endif
