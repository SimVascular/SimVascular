# Hey emacs, this is a -*- makefile -*-

# Copyright (c) 2009-2011 Open Source Medical Software Corporation,
#                         University of California, San Diego.
# 
# All rights reserved. 
#
# Portions copyright (c) 1999-2007 Stanford University,
# Nathan Wilson, Ken Wang, Charles Taylor.
# 
# See SimVascular Acknowledgements file for additional
# contributors to the source code. 
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
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
# OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
# IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
# CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
# TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
# SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

TOP=../../../../BuildWithMake

include $(TOP)/include.mk

CXXFLAGS = $(GLOBAL_CXXFLAGS) $(ITK_DEFS) $(LOCAL_INCDIR) $(TCLTK_INCDIR) $(VTK_INCDIRS) \
           $(ITK_INCDIRS)  $(SV_GDCM_INCDIR) -I./Include -I./ITKCode

CXXFLAGS += -DSV_EXPORT_SEGITK_COMPILE

HDRS	=  cvITKUtils.h cvITKLevelSet.h
CXXSRCS	=  cvITKUtils.cxx cvITKLevelSet.cxx

DLLHDRS = cv_ITKUtils_init.h cv_ITKLset2d_init.h cv_ITKLset3d_init.h cv_ITKLset_init.h
DLLSRCS = cv_ITKUtils_init.cxx cv_ITKLset2d_init.cxx  cv_ITKLset3d_init.cxx cv_ITKLset_init.cxx

TARGET_LIB_NAME = simvascular_itk_lset

DLLLIBS =  $(SVLIBFLAG)_simvascular_repository$(LIBLINKEXT) \
           $(SVLIBFLAG)_simvascular_utils$(LIBLINKEXT) \
	   $(ITK_LIBS) $(SV_GDCM_LIBS)

include $(TOP)/targetlib.mk
