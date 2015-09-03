SHELL           =/bin/sh
CXX             = g++ -pthread -w
CC              = gcc -pthread -w
F90             = gfortran -cpp
CXXDEP          = g++ -MM
CCDEP           = gcc -MM
AR              = ar -cru  
ifeq ($(MAKE_FULLY_OPTIMIZED),1)
    OPT_FLAGS       = -O2 -fPIC
    DEBUG_FLAGS     =
    OPT_FFLAGS      = -O2 -fPIC
    DEBUG_FFLAGS    =
    LINK_EXE        = $(CXX) -o
else
  ifeq ($(MAKE_OPTIMIZED),1)
    DEBUG_FLAGS     =
    OPT_FLAGS       = -O2 -fPIC
    OPT_FFLAGS      = -O2 -fPIC
    DEBUG_FFLAGS    =
    LINK_EXE        = $(CXX) -o
  else
    DEBUG_FLAGS     = -O0 -debug -g
    OPT_FLAGS       =
    OPT_FFLAGS      =
    DEBUG_FFLAGS    = -O0 -debug -g -zero
    LINK_EXE        = $(CXX) -debug -g -o
  endif
endif
SHAR            = $(CXX) -shared -o
SHARED          = 0
SOEXT           = so
STATICEXT       = a
OBJECTEXT       = o
EXEEXT          = 
BUILDFLAGS      = $(GLOBAL_DEFINES)
GLOBAL_CXXFLAGS = $(BUILDFLAGS) $(DEBUG_FLAGS) $(OPT_FLAGS) -fpermissive
GLOBAL_CCFLAGS  = $(BUILDFLAGS) $(DEBUG_FLAGS) $(OPT_FLAGS)
GLOBAL_FFLAGS   = $(BUILDFLAGS) $(DEBUG_FFLAGS) $(OPT_FFLAGS) -ffixed-line-length-132
GLOBAL_LFLAGS   = -lm
SHARED_LFLAGS   =
STATIC_FLAG     =
DYNAMIC_FLAG    =
TEMPLATE_AR     = $(AR)
CC_LIBS         = 
CXX_LIBS        = 
F90_LIBS        = -lgfortran -lm
#CC_LIBS         = -L$(INTEL_COMPILER_SO_PATH) -lirc -limf -lsvml
#CXX_LIBS        = -L$(INTEL_COMPILER_SO_PATH) -lirc -limf -lsvml
#F90_LIBS        = -L$(INTEL_COMPILER_SO_PATH) -lirc -limf -lsvml -lifcore -lifport -lgfortran -lm
#LINK_EXE        = $(CXX)  -L$(TOP)/Lib -o 
LIBPATH_COMPILER_FLAG = -L
LIBFLAG         = -l
LIBLINKEXT      =

