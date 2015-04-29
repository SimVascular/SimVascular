# ----------------------------------
# Platform-specific compiler options
# ----------------------------------

ifeq ($(CLUSTER), x64_cygwin)
    SHELL           =/bin/sh
    CXX             = CL
    CC              = CL
    F90             = ifort -fpp
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
    DEBUG_FFLAGS    = /MD /Od /debug:all /debug:all /check:all -traceback
  endif
endif
    SHAR            = link
    SHARED          = 0
    SOEXT           = dll
    STATICEXT       = lib
    OBJECTEXT       = obj
    BUILDFLAGS      = $(GLOBAL_DEFINES)
    GLOBAL_CXXFLAGS = $(BUILDFLAGS) $(DEBUG_FLAGS) $(OPT_FLAGS)
    GLOBAL_CCFLAGS  = $(BUILDFLAGS) $(DEBUG_FLAGS) $(OPT_FLAGS)
    GLOBAL_FFLAGS   = $(BUILDFLAGS) $(DEBUG_FFLAGS) $(OPT_FFLAGS) /W0 /4L132
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
                       /NODEFAULTLIB:msvcrtd.lib \
                      /MACHINE:X64 -subsystem:console

    SHARED_LFLAGS   = /DLL $(GLOBAL_LFLAGS) $(CXX_LIBS)
    STATIC_FLAG     = 
    DYNAMIC_FLAG    = 
    TEMPLATE_AR     = $(AR)
    CC_LIBS         =
#    CXX_LIBS        = msvcprt.lib
     CXX_LIBS       = Advapi32.lib Ws2_32.lib Shlwapi.lib
#    F90_LIBS        = /NODEFAULTLIB:libifcoremt.lib libifcoremd.lib \
                      /NODEFAULTLIB:libmmt.lib libmmd.lib
    LINK_EXE        = link /out:
endif

