# ----------------------------------
# Platform-specific compiler options
# ----------------------------------

ifeq ($(CLUSTER), x64_cygwin)
    SHELL           =/bin/sh
    CXX             = CL
    CC              = CL
    CXXDEP          = g++ -MM
    CCDEP           = gcc -MM
    AR              = lib -out:
ifeq ($(MAKE_FULLY_OPTIMIZED),1)
    DEBUG_FLAGS     = 
    OPT_FLAGS       = /MD /Ox /EHsc /MP /FS
else
  ifeq ($(MAKE_OPTIMIZED),1)
    DEBUG_FLAGS     =
    OPT_FLAGS       = /MD /Zi /O2 /EHsc /GS /MP /FS
  else
    OPT_FLAGS       = 
    DEBUG_FLAGS     = /MD /Zi /Od /EHsc -D_CRT_SECURE_NO_DEPRECATE /GS /GR /MP /FS
  endif
endif
    SHAR            = "/cygdrive/c/Program Files (x86)/Microsoft Visual Studio 12.0/VC/BIN/amd64/link.exe"
    SOEXT           = dll
    STATICEXT       = lib
    OBJECTEXT       = obj
    BUILDFLAGS      = $(GLOBAL_DEFINES)
    GLOBAL_CXXFLAGS = $(BUILDFLAGS) $(DEBUG_FLAGS) $(OPT_FLAGS)
    GLOBAL_CCFLAGS  = $(BUILDFLAGS) $(DEBUG_FLAGS) $(OPT_FLAGS)
    GLOBAL_LFLAGS   = /STACK:64000000,64000000
    SOLVER_STCKSZ   = /STACK:512000000,512000000
ifeq ($(LINK_WITH_DEBUG),1)
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
    CXX_LIBS        = Advapi32.lib Ws2_32.lib Shlwapi.lib
    LINK_EXE        = "/cygdrive/c/Program Files (x86)/Microsoft Visual Studio 12.0/VC/BIN/amd64/link.exe" /out:
    LIBPATH_COMPILER_FLAG = /LIBPATH:
    LIBFLAG         =
    LIBCMD          = lib 
    SVLIBFLAG       =lib
    LIBLINKEXT      =.lib
endif
