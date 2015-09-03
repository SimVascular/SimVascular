# already set all flags from intel.x64_linux.mk before coming here
# the flags below are used to replace the previous flags.
    SHELL           =/bin/sh
    CXX             = g++ -pthread -w
    CC              = gcc -pthread -w
    F90             = gfortran -cpp
    CXXDEP          = $(CXX) -MM
    CCDEP           = $(CC) -MM
    GLOBAL_CXXFLAGS += -fpermissive
    GLOBAL_FFLAGS   = $(BUILDFLAGS) $(DEBUG_FFLAGS) $(OPT_FFLAGS) -ffixed-line-length-132
#    CC_LIBS         = -L$(INTEL_COMPILER_SO_PATH) -lirc -limf -lsvml
#    CXX_LIBS        = -L$(INTEL_COMPILER_SO_PATH) -lirc -limf -lsvml
#    F90_LIBS        = -L$(INTEL_COMPILER_SO_PATH) -lirc -limf -lsvml -lifcore -lifport -lgfortran -lm
    CC_LIBS         = 
    CXX_LIBS        = 
    F90_LIBS        = -lgfortran -lm
    LINK_EXE        = $(CXX)  -L$(TOP)/Lib -o 
    LIBPATH_COMPILER_FLAG = -L
    LIBFLAG         = -l
    LIBLINKEXT      =

