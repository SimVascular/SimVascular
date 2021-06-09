# Copyright (c) Stanford University, The Regents of the University of
#               California, and others.
#
# All Rights Reserved.
#
# See Copyright-SimVascular.txt for additional details.
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
    OPT_FLAGS       = /nologo /MD /Ox /EHsc /MP /FS
else
  ifeq ($(MAKE_OPTIMIZED),1)
    DEBUG_FLAGS     =
    OPT_FLAGS       = /nologo /MD /Zi /O2 /EHsc /GS /MP /FS
  else
    OPT_FLAGS       =
    DEBUG_FLAGS     = /nologo /MD /Zi /Od /EHsc -D_CRT_SECURE_NO_DEPRECATE /GS /GR /MP /FS
  endif
endif
    SHAR            = "/cygdrive/c/Program Files (x86)/Microsoft Visual Studio/2017/Community/VC/Tools/MSVC/14.16.27023/bin/HostX64/x64/link.exe"
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
                       /NODEFAULTLIB:libc.lib /NODEFAULTLIB:libcd.lib \
                       /NODEFAULTLIB:libcmt.lib /NODEFAULTLIB:libcpmt.lib \
                       /NODEFAULTLIB:libcmtd.lib /NODEFAULTLIB:libcpmtd.lib \
                       /NODEFAULTLIB:msvcrtd.lib \
                      /MACHINE:X64 -subsystem:console
    # add verbose flag to debug linking issues (lists all search libraries)
    #GLOBAL_LFLAGS   += /VERBOSE:LIB
    SHARED_LFLAGS   = /DLL $(GLOBAL_LFLAGS) $(CXX_LIBS)
    STATIC_FLAG     =
    DYNAMIC_FLAG    =
    TEMPLATE_AR     = $(AR)
    CC_LIBS         =
    CXX_LIBS        = Advapi32.lib Ws2_32.lib Shlwapi.lib
    LINK_EXE        = $(SHAR) /out:
    LIBPATH_COMPILER_FLAG = /LIBPATH:
    LIBFLAG         =
    LIBCMD          = lib
    SVLIBFLAG       =lib
    LIBLINKEXT      =.lib
    # comment out the SV_QUIET_FLAG to get more debug info at compiler time
    SV_QUIET_FLAG   = @
endif
