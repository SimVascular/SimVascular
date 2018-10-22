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

ifeq ($(CLUSTER), x64_macosx)
    SHELL           =/bin/sh
    CXX             = clang++ -std=c++11 -pthread -w
    CC              = clang -pthread -w
    CXXDEP          = $(CXX) -MM
    CCDEP           = $(CC) -MM
    AR              = ar -cru 
ifeq ($(MAKE_FULLY_OPTIMIZED),1)
    OPT_FLAGS       = -O3 -fPIC
    DEBUG_FLAGS     =
    LINK_EXE        = $(CXX) -headerpad_max_install_names -o
else
  ifeq ($(MAKE_OPTIMIZED),1)
    DEBUG_FLAGS     =
    OPT_FLAGS       = -O2 -fPIC
    LINK_EXE        = $(CXX) -headerpad_max_install_names -o
  else
    DEBUG_FLAGS     = -O0 -g -fstack-protector-all
    OPT_FLAGS       =
    LINK_EXE        = $(CXX) -headerpad_max_install_names -g -fstack-protector-all -o
  endif
endif
#    SHAR            = $(CXX) -dynamiclib -current_version 1.0 -compatibility_version 1.0 -fvisibility=hidden -o
    SHAR            = $(CXX) -dynamiclib -current_version 1.0 -compatibility_version 1.0 -o
    SOEXT           = dylib
    STATICEXT       = a
    OBJECTEXT       = o
    EXEEXT          =
    BUILDFLAGS      = $(GLOBAL_DEFINES)
    GLOBAL_CXXFLAGS = $(BUILDFLAGS) $(DEBUG_FLAGS) $(OPT_FLAGS)
    GLOBAL_CXXFLAGS += -fpermissive
  ifeq ($(SV_USE_CXX11),1)
      GLOBAL_CXXFLAGS += -std=c++11
  endif
    GLOBAL_CCFLAGS  = $(BUILDFLAGS) $(DEBUG_FLAGS) $(OPT_FLAGS)
    GLOBAL_LFLAGS   = -lz -framework GLUT -framework OpenGL -framework Cocoa
ifeq ($(USE_SYSTEM_TCLTK),1)
	GLOBAL_LFLAGS += -framework Tcl -framework Tk
endif
ifeq ($(LINK_WITH_DEBUG),1)
    GLOBAL_LFLAGS   += -g
endif
    GLOBAL_LFLAGS   += -lm
    SHARED_LFLAGS   =
    STATIC_FLAG     =
    DYNAMIC_FLAG    =
    TEMPLATE_AR     = $(AR)
    CC_LIBS         =
    CXX_LIBS        =
#    LINK_EXE        = $(CXX)  -L$(TOP)/Lib -o
    LIBPATH_COMPILER_FLAG = -L
    LIBFLAG         = -l
    SVLIBFLAG       = -l
    SV_QUIET_FLAG   = @
endif
