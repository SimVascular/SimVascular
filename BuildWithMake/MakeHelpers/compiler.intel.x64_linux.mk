SHELL           =/bin/sh
CXX             = icpc -pthread 
CC              = icc -pthread 
F90             = ifort -threads -fpp
CXXDEP          = icpc -MM
CCDEP           = icc -MM
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
GLOBAL_CXXFLAGS = $(BUILDFLAGS) $(DEBUG_FLAGS) $(OPT_FLAGS)
GLOBAL_CCFLAGS  = $(BUILDFLAGS) $(DEBUG_FLAGS) $(OPT_FLAGS)
GLOBAL_FFLAGS   = $(BUILDFLAGS) $(DEBUG_FFLAGS) $(OPT_FFLAGS) \
                  -W0 -132
#GLOBAL_LFLAGS   =
#ifneq ($(MAKE_FULLY_OPTIMIZED),1)
#    GLOBAL_LFLAGS   +=
#endif
GLOBAL_LFLAGS   = -lm
SHARED_LFLAGS   =
STATIC_FLAG     =
DYNAMIC_FLAG    =
TEMPLATE_AR     = $(AR)
CC_LIBS         =
CXX_LIBS        =   
F90_LIBS        = -lifcore -lifport -lm 

