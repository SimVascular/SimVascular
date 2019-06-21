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

Qt5_MAJOR_VERSION=5
Qt5_MINOR_VERSION=11
Qt5_PATCH_VERSION=3
Qt5_VERSION=$(Qt5_MAJOR_VERSION).$(Qt5_MINOR_VERSION).$(Qt5_PATCH_VERSION)

ifeq ($(SV_EXTERNALS_PREBUILT_QT_SYSTEM_INSTALL),1)
  QT_TOP_DIR        = C:/OpenSource/Qt/Qt$(Qt5_VERSION)/$(Qt5_VERSION)/msvc2017_64
  QT_WEBENGINE_PROC = QtWebEngineProcess.exe
  QT_WEBENGINE_PATH = $(QT_TOP_DIR)/bin/$(QT_WEBENGINE_PROC)
  QT_QPA_FONTDIR    = $(QT_TOP_DIR)/resources
else
  QT_TOP_DIR        = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/qt-$(Qt5_VERSION)/$(Qt5_VERSION)/msvc2017_64
  QT_WEBENGINE_PROC = QtWebEngineProcess.exe
  QT_WEBENGINE_PATH = $(QT_TOP_DIR)/bin/$(QT_WEBENGINE_PROC)
  QT_QPA_FONTDIR    = $(QT_TOP_DIR)/resources
endif

#
# CTK required Qt libs
#
# Concurrent Core Gui Network OpenGL Sql
# Test UiTools WebEngineWidgets
# WebKitWidgets Widgets Xml XmlPatterns
#

#
# MITK required Qt libs
#
# DBus Gui Help Network OpenGL
# PrintSupport UiTools WebEngineWidgets
# Widgets Xml XmlPatterns
#

#
# Combined requirements for Qt
#
# Concurrent
# Core
# DBus
# Gui
# Help
# Network
# OpenGL
# PrintSupport
# Sql
# Test
# UiTools
# WebEngineWidgets
# WebKitWidgets
# Widgets
# Xml
# XmlPatterns
#

# don't know where flags are for DBus and UiTools

QT_DEPLOY_FLAGS = --release --no-compiler-runtime --angle

QT_DEPLOY_FLAGS += \
 -concurrent -core -gui -qthelp \
 -network -opengl -positioning -printsupport \
 -sql -svg -test -widgets -winextras -xml -xmlpatterns \
 -webenginecore -webengine -webenginewidgets

QT_DEFS    = -DNDEBUG -DWIN32 -DWIN64 -DQT_NO_DEBUG

ifeq ($(SV_USE_SHARED),0)
  QT_DEFS += -DQT_STATICPLUGIN
endif

QT_DEFS += -DQT_CLUCENE_LIB
QT_DEFS += -DQT_CONCURRENT_LIB
QT_DEFS += -DQT_CORE_LIB
QT_DEFS += -DQT_DBUS_LIB
QT_DEFS += -DQT_GUI_LIB
QT_DEFS += -DQT_HELP_LIB
QT_DEFS += -DQT_NETWORK_LIB
QT_DEFS += -DQT_OPENGL_LIB
QT_DEFS += -DQT_OPENGLEXTENSIONS_LIB
QT_DEFS += -DQT_PLATFORMSUPPORT_LIB
QT_DEFS += -DQT_POSITIONING_LIB
QT_DEFS += -DQT_PRINTSUPPORT_LIB
QT_DEFS += -DQT_QML_LIB
QT_DEFS += -DQT_QMLTEST_LIB
QT_DEFS += -DQT_SQL_LIB
QT_DEFS += -DQT_SVG_LIB
QT_DEFS += -DQT_TESTLIB_LIB
QT_DEFS += -DQT_UIPLUGIN_LIB
QT_DEFS += -DQT_UITOOLS_LIB
QT_DEFS += -DQT_WEBCHANNEL_LIB
QT_DEFS += -DQT_WEBENGINE_LIB
QT_DEFS += -DQT_WEBENGINECORE_LIB
QT_DEFS += -DQT_WEBENGINECOREHEADERS_LIB
QT_DEFS += -DQT_WEBENGINEWIDGETS_LIB
QT_DEFS += -DQT_WEBSOCKETS_LIB
QT_DEFS += -DQT_WIDGETS_LIB
QT_DEFS += -DQT_WINEXTRAS_LIB
QT_DEFS += -DQT_XML_LIB
QT_DEFS += -DQT_XMLPATTERNS_LIB

QT_LIBDIRS = $(QT_TOP_DIR)/lib
QT_BIN_LIBS = $(QT_TOP_DIR)/bin

QT_MOC_PARSER=$(QT_TOP_DIR)/bin/moc.exe
QT_UIC_CMD=$(QT_TOP_DIR)/bin/uic.exe
QT_RCC_CMD=$(QT_TOP_DIR)/bin/rcc.exe
QMAKE=$(QT_TOP_DIR)/bin/qmake.exe

QT_INCDIRS = \
      -I$(QT_TOP_DIR)/include \
      -I$(QT_TOP_DIR)/include/QtWidgets \
      -I$(QT_TOP_DIR)/include/QtGui \
      -I$(QT_TOP_DIR)/include/QtCore \
      -I$(QT_TOP_DIR)/include/QtXml \
      -I$(QT_TOP_DIR)/include/QtConcurrent \
      -I$(QT_TOP_DIR)/include/QtNetwork \
      -I$(QT_TOP_DIR)/include/QtWebEngine \
      -I$(QT_TOP_DIR)/include/QtWebView \
      -I$(QT_TOP_DIR)/include/QtWebEngineWidgets \
      -I$(QT_TOP_DIR)/include/mkspecs/win32-msvc2015
QT_MOC_INCDIRS = \
      -I $(QT_TOP_DIR)/include \
      -I $(QT_TOP_DIR)/include/QtWidgets \
      -I $(QT_TOP_DIR)/include/QtGui \
      -I $(QT_TOP_DIR)/include/QtCore \
      -I $(QT_TOP_DIR)/include/QtXml \
      -I $(QT_TOP_DIR)/include/QtConcurrent \
      -I $(QT_TOP_DIR)/include/QtNetwork \
      -I $(QT_TOP_DIR)/include/QtWebEngine \
      -I $(QT_TOP_DIR)/include/QtWebView \
      -I $(QT_TOP_DIR)/include/QtWebEngineWidgets \
      -I $(QT_TOP_DIR)/include/mkspecs/win32-msvc2015
QT_LIBS =    $(LIBPATH_COMPILER_FLAG)$(QT_LIBDIRS) \
      $(LIBFLAG)Qt5WebEngine$(LIBLINKEXT) \
      $(LIBFLAG)Qt5WebEngineWidgets$(LIBLINKEXT) \
      $(LIBFLAG)Qt5WebView$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Network$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Xml$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Widgets$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Gui$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Concurrent$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Core$(LIBLINKEXT)
QT_SO_PATH=$(QT_TOP_DIR)/bin
QT_PLUGIN_PATH=$(QT_TOP_DIR)/plugins

