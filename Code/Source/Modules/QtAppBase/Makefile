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
           $(QT_INCDIRS) $(QT_DEFS) \
	   $(ITK_INCDIRS) \
           $(MITK_INCDIRS) $(MITK_DEFS) \
           -DSV_NO_HARDCODED_PYTHON_PATHS

MODULE_NAME = svQtAppBase
MODULE_NAME_ALL_CAPS = SVQTAPPBASE
CXXFLAGS += -D$(MODULE_NAME)_EXPORTS
CXXFLAGS += -DUS_MODULE_NAME="$(MODULE_NAME)"

HDRS	= QmitkDataManagerItemDelegate.h \
          QmitkDataNodeItemModel.h \
          QmitkInfoDialog.h \
          QmitkStatusBar.h \
          svAbstractExtension.h \
          svAbstractFunctionality.h \
          svAbstractView.h \
          svApplication.h \
          svBasicFunctionality.h \
          svCollapsiblePane.h \
          svDisplayEditor.h \
          svDnDFrameWidget.h \
          svExtensionManager.h \
          svExtensionRegistry.h \
          svMacros.h \
          svPluginActivator.h \
          svPythonManager.h \
          svQmitkDataManager.h \
          svQmitkImageNavigator.h \
          svQmitkNodeTableViewKeyFilter.h \
          svRenderWindowPart.h \
          svScriptUtil.h

HDRS	+= svMainWindow.h qttclnotifier.h

CXXSRCS	= QmitkDataManagerItemDelegate.cxx \
          QmitkDataNodeItemModel.cxx \
          QmitkInfoDialog.cxx \
          QmitkStatusBar.cxx \
          svAbstractFunctionality.cxx \
          svAbstractView.cxx \
          svApplication.cxx \
          svBasicFunctionality.cxx \
          svCollapsiblePane.cxx \
          svDisplayEditor.cxx \
          svDnDFrameWidget.cxx \
          svExtensionManager.cxx \
          svExtensionRegistry.cxx \
          svPythonManager.cxx \
          svQmitkDataManager.cxx \
          svQmitkImageNavigator.cxx \
          svQmitkNodeTableViewKeyFilter.cxx \
          svRenderWindowPart.cxx \
          svScriptUtil.cxx

CXXSRCS	+= svMainWindow.cxx qttclnotifier.cxx

CXXSRCS += us_init.cxx

UIFILES = svQmitkImageNavigator.ui

RCFILES = resources/qtappbase-sv.qrc

CXXSRCS += $(addprefix moc_,$(HDRS:.h=.cxx))

#CXXSRCS += $(addprefix ui_,$(UIFILES:.ui=.cxx))

CXXSRCS += $(addprefix rcc_,$(notdir $(RCFILES:.qrc=.cxx)))

DLLHDRS =

DLLSRCS =

DLLLIBS += $(PYTHON_LIB) $(VTK_LIBS) $(ITK_LIBS) $(QT_LIBS) $(MITK_LIBS)

TARGET_LIB_NAME = simvascular_qtgui_module_appbase

include $(TOP)/targetlib.mk
