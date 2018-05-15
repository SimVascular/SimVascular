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

CXXFLAGS = $(GLOBAL_CXXFLAGS) $(LOCAL_INCDIR) \
	   $(TCLTK_INCDIR) $(PYTHON_INCDIR) \
	   $(VTK_INCDIRS)

CREATE_EXPORTS_CV_ALL_CAPS=SV_REPOSITORY
CREATE_EXPORTS_CV_EXPORT_NAME=SV_EXPORT_REPOSITORY
CREATE_EXPORTS_CV_FILE_H=svRepositoryExports.h

CXXFLAGS += -DSV_EXPORT_REPOSITORY_COMPILE

HDRS	= sv_Repository.h sv_RepositoryData.h sv_DataObject.h \
	  sv_PolyData.h sv_StrPts.h sv_UnstructuredGrid.h \
          sv_DataSet.h

CXXSRCS	= sv_Repository.cxx sv_RepositoryData.cxx sv_DataObject.cxx \
	  sv_PolyData.cxx sv_StrPts.cxx sv_UnstructuredGrid.cxx \
	  sv_DataSet.cxx

DLLHDRS = sv_repos_init.h
DLLSRCS = sv_repos_init.cxx

SVPYHDRS = sv_repos_init_py.h
SVPYSRCS = sv_repos_init_py.cxx

DLLLIBS = $(SVLIBFLAG)$(SV_LIB_UTILS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GLOBALS_NAME)$(LIBLINKEXT)

TARGET_LIB_NAME = $(SV_LIB_REPOSITORY_NAME)

SV_PYTHON_MODULE_NAME = pyRepository

include $(TOP)/targetlib.mk
