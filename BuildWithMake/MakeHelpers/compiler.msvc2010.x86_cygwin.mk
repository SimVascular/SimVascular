# ----------------------------------
# Platform-specific compiler options
# ----------------------------------

ifeq ($(CLUSTER), x86_cygwin)
    SHELL           =/bin/sh
    CXX             = CL
    CC              = CL
    F90             = ifort
    CXXDEP          = g++ -MM
    CCDEP           = gcc -MM
    AR              = lib -out:
ifeq ($(MAKE_FULLY_OPTIMIZED),1)
    DEBUG_FLAGS     =
    DEBUG_FFLAGS    = 
    OPT_FLAGS       = /MD /Ox /EHsc
    OPT_FFLAGS      = /MD /Ox
else
  ifeq ($(MAKE_OPTIMIZED),1)
    DEBUG_FLAGS     =
    DEBUG_FFLAGS    = 
    OPT_FLAGS       = /MD /Zi /O2 /EHsc /GS
    OPT_FFLAGS      = /MD /Zi /O2 
  else
    OPT_FLAGS       =
    DEBUG_FLAGS     = /MD /Zi /Od /EHsc -D_CRT_SECURE_NO_DEPRECATE /GS /GR
    DEBUG_FFLAGS    = /MD /Od /debug:all /check:all -traceback
  endif
endif
    SHAR            = link
    SHARED          = 0
    SOEXT           = dll
    STATICEXT       = lib
    OBJECTEXT       = obj
    BUILDFLAGS      = $(GLOBAL_DEFINES)
    GLOBAL_CXXFLAGS = $(BUILDFLAGS) $(DEBUG_FLAGS) $(OPT_FLAGS) /MD /EHsc /GS -D_CRT_SECURE_NO_DEPRECATE
    GLOBAL_CCFLAGS  = $(BUILDFLAGS) $(DEBUG_FLAGS) $(OPT_FLAGS) /MD /EHsc /GS -D_CRT_SECURE_NO_DEPRECATE
    GLOBAL_FFLAGS   = $(BUILDFLAGS) $(DEBUG_FFLAGS) $(OPT_FFLAGS) /MD /W0 /4L132
    GLOBAL_LFLAGS   = /STACK:64000000,64000000
    SOLVER_STCKSZ   = /STACK:512000000,512000000
ifeq ($(MAKE_OPTIMIZED_WITH_DEBUG),1)
    GLOBAL_LFLAGS   += /DEBUG
endif
    GLOBAL_LFLAGS   += /LARGEADDRESSAWARE /INCREMENTAL:NO /FIXED:NO /RELEASE /NOLOGO \
                       /VERBOSE:LIB \
                       /NODEFAULTLIB:libc.lib /NODEFAULTLIB:libcd.lib \
                       /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:libcpmt.lib \
                       /NODEFAULTLIB:libcmtd.lib /NODEFAULTLIB:libcpmtd.lib \
                       /NODEFAULTLIB:msvcrt.lib /NODEFAULTLIB:msvcrtd.lib \
                       /MACHINE:IX86 -subsystem:console
    SHARED_LFLAGS   = /DLL $(GLOBAL_LFLAGS) $(CXX_LIBS)
    STATIC_FLAG     = 
    DYNAMIC_FLAG    = 
    TEMPLATE_AR     = $(AR)
    CC_LIBS         =
    CXX_LIBS        = msvcrt.lib Advapi32.lib Ws2_32.lib
    F90_LIBS        = /NODEFAULTLIB:libifcoremt.lib /NODEFAULTLIB:libguide40.lib libifcoremd.lib libifportmd.lib libmmd.lib
    LINK_EXE        = link /out:
endif

