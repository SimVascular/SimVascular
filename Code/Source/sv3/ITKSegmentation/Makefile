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

CXXFLAGS = $(GLOBAL_CXXFLAGS) $(ITK_DEFS) $(LOCAL_INCDIR) $(TCLTK_INCDIR) $(PYTHON_INCDIR) $(VTK_INCDIRS) \
           $(ITK_INCDIRS)  $(SV_GDCM_INCDIR)

CREATE_EXPORTS_CV_ALL_CAPS=SV_SEGITK
CREATE_EXPORTS_CV_EXPORT_NAME=SV_EXPORT_SEGITK
CREATE_EXPORTS_CV_FILE_H=svSegITKExports.h

CXXFLAGS += -DSV_EXPORT_SEGITK_COMPILE

HDRS	=  sv3_ITKLset_ITKUtils.h sv3_ITKLevelSet.h \
           sv3_ITKLset_ConnectVTKITK.h sv3_ITKLset_ITK_Macros.h \
           sv3_ITKLset_Macros.h sv3_ITKLset_TCL_Macros.h \
           sv3_ITKLset_VTK_Macros.h sv3_ITKLset_ImgInfo.h

CXXSRCS	=  sv3_ITKUtils.cxx sv3_ITKLevelSet.cxx

DLLHDRS = sv3_ITKUtils_init.h sv3_ITKLset2d_init.h sv3_ITKLset3d_init.h sv3_ITKLset_init.h
DLLSRCS = sv3_ITKUtils_init.cxx sv3_ITKLset2d_init.cxx  sv3_ITKLset3d_init.cxx sv3_ITKLset_init.cxx

SVPYHDRS = sv3_ITKLset_init_py.h  sv3_ITKUtils_init_py.h sv3_ITKLset2d_init_py.h sv3_ITKLset3d_init_py.h
SVPYSRCS = sv3_ITKLset_init_py.cxx  sv3_ITKUtils_init_py.cxx sv3_ITKLset2d_init_py.cxx  sv3_ITKLset3d_init_py.cxx

TARGET_LIB_NAME = $(SV_LIB_ITK_LSET_NAME)

SV_PYTHON_MODULE_NAME = pyItkls

DLLLIBS =  $(SVLIBFLAG)$(SV_LIB_REPOSITORY_NAME)$(LIBLINKEXT) \
           $(SVLIBFLAG)$(SV_LIB_UTILS_NAME)$(LIBLINKEXT) \
	   $(ITK_LIBS) $(SV_GDCM_LIBS)

include $(TOP)/targetlib.mk
