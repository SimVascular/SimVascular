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

ifeq ($(CLUSTER), x64_linux)
    MESHSIM_TOP      = $(LICENSED_SOFTWARE_TOPLEVEL)/meshsim-9.0-150704
    MESHSIM_INCDIR   = -I$(MESHSIM_TOP)/include
    MESHSIM_LIBDIR   = $(MESHSIM_TOP)/lib/x64_rhel6_gcc44
    MESHSIM_LIBS     = -L$(MESHSIM_LIBDIR) \
                       -lSimAdvMeshing -lSimMeshing -lSimMeshTools
    MESHSIM_SO_PATH  = $(MESHSIM_LIBDIR)
    ifeq ($(MESHSIM_MODELER),parasolid)
      MESHSIM_LIBS   := $(MESHSIM_LIBS) -lSimParasolid260
    endif
    ifeq ($(SV_USE_MESHSIM_DISCRETE_MODEL),1)
      MESHSIM_LIBS   := $(MESHSIM_LIBS) -lSimDiscrete
    endif
    MESHSIM_LIBS     += -lSimMeshTools -lSimModel
endif
