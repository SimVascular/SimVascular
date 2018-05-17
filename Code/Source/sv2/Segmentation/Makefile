# Hey emacs, this is a -*- makefile -*-

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

TOP=../../../../BuildWithMake

include $(TOP)/include.mk

# --------------
# Compiler flags
# --------------

CXXFLAGS = $(GLOBAL_CXXFLAGS) $(LOCAL_INCDIR) $(TCLTK_INCDIR) \
	   $(VTK_INCDIRS)

CREATE_EXPORTS_CV_ALL_CAPS=SV_LSET
CREATE_EXPORTS_CV_EXPORT_NAME=SV_EXPORT_LSET
CREATE_EXPORTS_CV_FILE_H=svLSetExports.h

CXXFLAGS += -DSV_EXPORT_LSET_COMPILE

HDRS	= sv2_LevelSet.h sv2_LevelSetNode.h \
	  sv2_LevelSetStructuredGrid.h sv2_LevelSetDenseGrid.h \
          sv2_LevelSetSparseGrid.h

HDRS   += sv2_LevelSetVelocity.h sv2_LevelSetVelocityKGI.h \
          sv2_LevelSetVelocityConstant.h \
	  sv2_LevelSetVelocityThreshold.h sv2_LevelSetVelocityPotential.h \
          sv2_LevelSetVelocityExponentialDecay.h sv2_LevelSetVelocitySmooth.h \
          sv2_LevelSetVelocityImage.h \
          sv2_IntArrayList.h sv2_StateArray.h sv2_Timer.h

CXXSRCS	= sv2_LevelSet.cxx sv2_LevelSetNode.cxx \
	  sv2_LevelSetStructuredGrid.cxx sv2_LevelSetDenseGrid.cxx \
          sv2_LevelSetSparseGrid.cxx

CXXSRCS += sv2_LevelSetVelocity.cxx sv2_LevelSetVelocityKGI.cxx \
          sv2_LevelSetVelocityConstant.cxx \
	  sv2_LevelSetVelocityThreshold.cxx sv2_LevelSetVelocityPotential.cxx \
          sv2_LevelSetVelocityExponentialDecay.cxx \
          sv2_LevelSetVelocitySmooth.cxx sv2_LevelSetVelocityImage.cxx \
          sv2_IntArrayList.cxx sv2_StateArray.cxx sv2_Timer.cxx

DLLHDRS = sv2_LsetCore_init.h sv2_LsetV_init.h sv2_Lset_init.h
DLLSRCS = sv2_LsetCore_init.cxx sv2_LsetV_init.cxx sv2_Lset_init.cxx

# -------
# Targets
# -------

TARGET_LIB_NAME = $(SV_LIB_LSET_NAME)

DLLLIBS = $(SVLIBFLAG)$(SV_LIB_IMAGE_NAME)$(LIBLINKEXT) \
	  $(SVLIBFLAG)$(SV_LIB_SOLID_NAME)$(LIBLINKEXT) \
	  $(SVLIBFLAG)$(SV_LIB_GEOM_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_REPOSITORY_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_UTILS_NAME)$(LIBLINKEXT) \
	  $(VMTK_LIBS)

include $(TOP)/targetlib.mk

