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

TOP=../../../../../../BuildWithMake

include $(TOP)/include.mk

CXXFLAGS = $(GLOBAL_CXXFLAGS) \
	   $(LOCAL_INCDIR) \
	   $(VTK_INCDIRS) \
	   $(TCLTK_INCDIR) \
           $(PYTHON_INCDIR) \
           $(QT_INCDIRS) $(QT_DEFS) \
	   $(ITK_INCDIRS) \
           $(MITK_INCDIRS) $(MITK_DEFS) \
           $(VMTK_INCDIR) \
	   $(SV_MMG_INCDIR)

ifeq ($(SV_USE_OPENCASCADE),1)
    CXXFLAGS += $(OPENCASCADE_INCDIR)
endif

MODULE_NAME = sv4guiModuleModel
MODULE_NAME_ALL_CAPS = SV4GUIMODULEMODEL
CXXFLAGS += -D$(MODULE_NAME)_EXPORTS
CXXFLAGS += -DUS_MODULE_NAME="$(MODULE_NAME)"

HDRS	= \
    sv4gui_ModelUtils.h \
    sv4gui_ModelElement.h \
    sv4gui_ModelElementPolyData.h \
    sv4gui_ModelElementAnalytic.h \
    sv4gui_ModelElementFactory.h \
    sv4gui_ModelOperation.h \
    sv4gui_Model.h \
    sv4gui_ModelVtkMapper2D.h \
    sv4gui_ModelVtkMapper3D.h \
    sv4gui_ModelDataInteractor.h \
    sv4gui_ModelIO.h \
    sv4gui_ModelLegacyIO.h \
    sv4gui_ModelObjectFactory.h \
    sv4gui_RegisterPolyDataFunction.h

CXXSRCS	= \
    sv4gui_ModelUtils.cxx \
    sv4gui_ModelElement.cxx \
    sv4gui_ModelElementPolyData.cxx \
    sv4gui_ModelElementAnalytic.cxx \
    sv4gui_ModelElementFactory.cxx \
    sv4gui_ModelOperation.cxx \
    sv4gui_Model.cxx \
    sv4gui_ModelVtkMapper2D.cxx \
    sv4gui_ModelVtkMapper3D.cxx \
    sv4gui_ModelDataInteractor.cxx \
    sv4gui_ModelIO.cxx \
    sv4gui_ModelLegacyIO.cxx \
    sv4gui_ModelObjectFactory.cxx \
    sv4gui_RegisterPolyDataFunction.cxx

CXXSRCS += us_init.cxx

UIFILES =

SV_APPEND_CPPMICROSERVICES_TO_DLL = 1
RCFILES = resource/Interactions/sv4gui_ModelInteraction.xml \
          resource/Interactions/sv4gui_ModelConfig.xml

QRCFILES =

CXXSRCS += $(addprefix moc_,$(HDRS:.h=.cxx))

CXXSRCS += $(addprefix ui_,$(UIFILES:.ui=.cxx))

CXXSRCS += $(addprefix rcc_,$(notdir $(QRCFILES:.qrc=.cxx)))

DLLHDRS =

DLLSRCS =

# these DLLLIBS came from Segmentation and can probably be cleaned up
# significantly!

DLLLIBS += $(PYTHON_LIB) $(VTK_LIBS) $(ITK_LIBS) $(QT_LIBS) $(MITK_LIBS)

DLLLIBS += \
          $(SVLIBFLAG)$(SV_LIB_MODULE_COMMON_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_PATH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_MODULE_SEGMENTATION_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_VTKSVCOMMON_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_VTKSVMISC_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_VTKSVNURBS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_POLYDATA_SOLID_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_SOLID_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_ITK_LSET_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_REPOSITORY_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_GEOM_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_PATH_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_UTILS_NAME)$(LIBLINKEXT) \
          $(SVLIBFLAG)$(SV_LIB_SEGMENTATION_NAME)$(LIBLINKEXT)

ifeq ($(SV_USE_OPENCASCADE),1)
    DLLLIBS += $(SVLIBFLAG)$(SV_LIB_OpenCASCADE_SOLID_NAME)$(LIBLINKEXT)
endif

ifeq ($(SV_USE_OPENCASCADE),1)
    DLLLIBS += $(OPENCASCADE_LIBS)
endif

ifeq ($(SV_USE_VMTK),1)
	DLLLIBS += $(SVLIBFLAG)$(SV_LIB_VMTK_UTILS_NAME)$(LIBLINKEXT)
	DLLLIBS += $(VMTK_LIBS)
	DLLLIBS += $(SV_MMG_LIBS)
endif

ifeq ($(SV_USE_MMG),1)
	DLLLIBS += $(SVLIBFLAG)$(SV_LIB_MMG_MESH_NAME)$(LIBLINKEXT)
endif

TARGET_LIB_NAME = $(SV_LIB_MODULE_MODEL_NAME)

include $(TOP)/targetlib.mk
