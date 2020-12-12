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

CXXFLAGS = $(GLOBAL_CXXFLAGS) $(LOCAL_INCDIR) $(TCLTK_INCDIR) $(PYTHON_INCDIR) $(VTK_INCDIRS) \
           $(SV_TINYXML_INCDIR) $(SV_TINYXML2_INCDIR)

# hack until we don't use tinyxml from mitk
ifeq ($(SV_USE_MITK),0)
  MITK_MAJOR_VERSION=2018
  MITK_MINOR_VERSION=04
  MITK_PATCH_VERSION=0
  MITK_VERSION=$(MITK_MAJOR_VERSION).$(MITK_MINOR_VERSION).$(MITK_PATCH_VERSION)
  MITK_BINDIR = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/mitk-$(MITK_VERSION)
endif
CXXFLAGS += -I$(MITK_BINDIR)/include/tinyxml

CREATE_EXPORTS_CV_ALL_CAPS=SV_PATH
CREATE_EXPORTS_CV_EXPORT_NAME=SV_EXPORT_PATH
CREATE_EXPORTS_CV_FILE_H=sv3PathExports.h

CXXFLAGS += -DSV_EXPORT_PATH_COMPILE

HDRS	= sv3_PathUtils.h sv3_PathElement.h sv3_PathGroup.h sv3_VtkParametricSpline.h sv3_Spline.h sv3_PathIO.h
CXXSRCS	= sv3_PathUtils.cxx sv3_PathElement.cxx sv3_PathGroup.cxx sv3_VtkParametricSpline.cxx sv3_Spline.cxx sv3_PathIO.cxx


SVPYHDRS = sv3_PathElement_init_py.h sv3_PathGroup_init_py.h sv3_PathIO_init_py.h
SVPYSRCS = sv3_PathElement_init_py.cxx sv3_PathGroup_init_py.cxx sv3_PathIO_init_py.cxx

DLLLIBS = $(SVLIBFLAG)$(SV_LIB_REPOSITORY_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_COMMON_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_SOLID_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_UTILS_NAME)$(LIBLINKEXT)

ifeq ($(SV_USE_TINYXML),1)
  DLLLIBS += $(SV_TINYXML_LIBS)
endif

ifeq ($(SV_USE_TINYXML2),1)
  DLLLIBS += $(SV_TINYXML2_LIBS)
endif

TARGET_LIB_NAME = $(SV_LIB_PATH_NAME)

SV_PYTHON_MODULE_NAME = pyPath

include $(TOP)/targetlib.mk
