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
    SV_QUIET_FLAG   = @
endif
