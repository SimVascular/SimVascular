# Hey emacs, this is a -*- makefile -*-

# Copyright (c) 2009-2011, Open Source Medical Software Corporation,
#              University of California, San Diego.
#
# All rights reserved.
#
# Portions copyright (c) 1999-2007 Stanford University,
# Nathan Wilson, Ken Wang, Charles Taylor.
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

CXXFLAGS = $(GLOBAL_CXXFLAGS) \
	   $(LOCAL_INCDIR) \
	   $(VTK_INCDIRS) \
	   $(TCLTK_INCDIR) \
           $(PYTHON_INCDIR) \
           $(ZLIB_INCDIR) \
           $(TETGEN_INCDIR) \
           $(QT_INCDIRS) $(QT_DEFS) \
	   $(ITK_INCDIRS) \
           $(MITK_INCDIRS) $(MITK_DEFS)

MODULE_NAME = svMesh
MODULE_NAME_ALL_CAPS = SVMESH
CXXFLAGS += -D$(MODULE_NAME)_EXPORTS

CXXFLAGS += -DUS_MODULE_NAME="$(MODULE_NAME)"

HDRS	= \
    svMesh.h \
    svMeshTetGen.h \
    svMitkMesh.h \
    svMitkMeshOperation.h \
    svMitkMeshMapper3D.h \
    svMitkMeshMapper2D.h \
    svMitkMeshIO.h \
    svMeshLegacyIO.h \
    svMitkMeshObjectFactory.h

CXXSRCS	= \
    svMesh.cxx \
    svMeshTetGen.cxx \
    svMitkMesh.cxx \
    svMitkMeshOperation.cxx \
    svMitkMeshMapper3D.cxx \
    svMitkMeshMapper2D.cxx \
    svMitkMeshIO.cxx \
    svMeshLegacyIO.cxx \
    svMitkMeshObjectFactory.cxx

CXXSRCS += us_init.cxx

UIFILES = 

# I don't know how to handle just xml files yet...
RCFILES =

QRCFILES = 

CXXSRCS += $(addprefix moc_,$(HDRS:.h=.cxx))

CXXSRCS += $(addprefix rcc_,$(notdir $(QRCFILES:.qrc=.cxx)))

DLLHDRS =

DLLSRCS =

DLLLIBS += $(PYTHON_LIB) $(VTK_LIBS) $(ITK_LIBS) $(QT_LIBS) $(MITK_LIBS)

DLLLIBS += \
          $(SVLIBFLAG)_simvascular_qtgui_module_common$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_qtgui_module_model$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_itk_lset$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_repository$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_geom$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_tetgen_mesh$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_mesh$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_mmg_mesh$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_polydata_solid$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_solid$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_thirdparty_tetgen$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_thirdparty_zlib$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_utils$(LIBLINKEXT) \
          $(SVLIBFLAG)_simvascular_globals$(LIBLINKEXT)

TARGET_LIB_NAME = simvascular_qtgui_module_mesh

include $(TOP)/targetlib.mk
