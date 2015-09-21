# ----------------------------------
# Platform-specific compiler options
# ----------------------------------

ifeq ($(CLUSTER), x64_cygwin)
    SHELL           =/bin/sh
    CXX             = x86_64-w64-mingw32-g++ -w -fpermissive
    CC              = x86_64-w64-mingw32-gcc -w
    CXXDEP          = x86_64-w64-mingw32-g++ -MM
    CCDEP           =  x86_64-w64-mingw32-gcc -MM
    AR              = x86_64-w64-mingw32-ar -cru  
ifeq ($(MAKE_FULLY_OPTIMIZED),1)
    DEBUG_FLAGS     = 
    OPT_FLAGS       = -O3
else
  ifeq ($(MAKE_OPTIMIZED),1)
    DEBUG_FLAGS     =
    OPT_FLAGS       = -O2 -g
  else
    OPT_FLAGS       = 
    DEBUG_FLAGS     = -g
  endif
endif
    SHAR            = 
    SOEXT           = dll
    STATICEXT       = lib
    OBJECTEXT       = obj
    BUILDFLAGS      = $(GLOBAL_DEFINES)
    GLOBAL_CXXFLAGS = $(BUILDFLAGS) $(DEBUG_FLAGS) $(OPT_FLAGS)
    GLOBAL_CCFLAGS  = $(BUILDFLAGS) $(DEBUG_FLAGS) $(OPT_FLAGS)
#    GLOBAL_LFLAGS   = /STACK:64000000,64000000
#    SOLVER_STCKSZ   = /STACK:512000000,512000000
ifeq ($(LINK_WITH_DEBUG),1)
    GLOBAL_LFLAGS   += -g
endif


    GLOBAL_LFLAGS   = 
    SHARED_LFLAGS   = /DLL $(GLOBAL_LFLAGS) $(CXX_LIBS)
    STATIC_FLAG     = 
    DYNAMIC_FLAG    = 
    TEMPLATE_AR     = $(AR)
    CC_LIBS         =
    LINK_EXE        = $(CXX)  -L$(TOP)/Lib -o 
    LIBPATH_COMPILER_FLAG = -L
    LIBFLAG         = -l
    LIBCMD          = lib 
    SVLIBFLAG       = -llib
endif
