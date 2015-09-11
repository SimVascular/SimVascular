ifeq ($(CLUSTER), x64_linux)
    SHELL           =/bin/sh
    CXX             = icpc -pthread 
    CC              = icc -pthread 
    CXXDEP          = icpc -MM
    CCDEP           = icc -MM
    AR              = ar -cru  
ifeq ($(MAKE_FULLY_OPTIMIZED),1)
    OPT_FLAGS       = -O3 -fPIC
    DEBUG_FLAGS     =
    LINK_EXE        = $(CXX) -o
else
  ifeq ($(MAKE_OPTIMIZED),1)
    DEBUG_FLAGS     =
    OPT_FLAGS       = -O2 -fPIC
    LINK_EXE        = $(CXX) -o
  else
    DEBUG_FLAGS     = -O0 -debug -g -fp-stack-check -fstack-protector-all 
    OPT_FLAGS       =
    LINK_EXE        = $(CXX) -debug -g -fp-stack-check -fstack-protector-all -traceback -o  
  endif
endif
    SHAR            = $(CXX) -shared -o
    SOEXT           = so
    STATICEXT       = a
    OBJECTEXT       = o
    EXEEXT          = 
    BUILDFLAGS      = $(GLOBAL_DEFINES)
    GLOBAL_CXXFLAGS = $(BUILDFLAGS) $(DEBUG_FLAGS) $(OPT_FLAGS)
    GLOBAL_CCFLAGS  = $(BUILDFLAGS) $(DEBUG_FLAGS) $(OPT_FLAGS)
    GLOBAL_LFLAGS   =
    GLOBAL_LFLAGS   += -lm
    SHARED_LFLAGS   =
    STATIC_FLAG     =
    DYNAMIC_FLAG    =
    TEMPLATE_AR     = $(AR)
    CC_LIBS         =
    CXX_LIBS        =
ifeq ($(LINK_WITH_DEBUG),1)
    GLOBAL_LFLAGS   += -debug -g
endif
#    LINK_EXE        = $(CXX)  -L$(TOP)/Lib -o 
    LIBPATH_COMPILER_FLAG = -L
    LIBFLAG         = -l
    SVLIBFLAG       = -l
endif
