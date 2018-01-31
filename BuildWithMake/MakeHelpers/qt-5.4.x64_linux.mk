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
#    QT_TOP_DIR	= /opt/Qt5.4.2/5.4/gcc_64
    QT_TOP_DIR  = $(OPEN_SOFTWARE_BINARIES_TOPLEVEL)/qt-5.4.2
    QT_DEFS = -DUNIX -D_REENTRANT -DNDEBUG
    QT_DEFS += -DQT_NO_DEBUG -DQT_WIDGETS_LIB -DQT_GUI_LIB -DQT_CORE_LIB -DQT_XML_LIB -DQT_CONCURRENT_LIB -DQT_NETWORK_LIB -DQT_PLUGIN -DQT_WEBVIEW -DQT_WEBKITWIDGETS -DQT_SQL_LIB

ifeq ($(SV_USE_SHARED),0)
  QT_DEFS += -DQT_STATICPLUGIN
endif

    QT_LIBDIRS = $(QT_TOP_DIR)/lib
    QT_BIN_LIBS = $(QT_TOP_DIR)/bin
    QT_MOC_PARSER=$(QT_TOP_DIR)/bin/moc
    QT_UIC_CMD=$(QT_TOP_DIR)/bin/uic
    QT_RCC_CMD=$(QT_TOP_DIR)/bin/rcc
    QMAKE=$(QT_TOP_DIR)/bin/qmake
    QT_INCDIRS = \
      -I$(QT_TOP_DIR)/include \
      -I$(QT_TOP_DIR)/include/QtWidgets \
      -I$(QT_TOP_DIR)/include/QtGui \
      -I$(QT_TOP_DIR)/include/QtCore \
      -I$(QT_TOP_DIR)/include/QtXml \
      -I$(QT_TOP_DIR)/include/QtConcurrent \
      -I$(QT_TOP_DIR)/include/QtNetwork \
      -I$(QT_TOP_DIR)/include/QtWebView \
      -I$(QT_TOP_DIR)/include/QtWebKitWidgets \
      -I$(QT_TOP_DIR)/include/mkspecs/linux-g++
    QT_MOC_INCDIRS = \
      -I $(QT_TOP_DIR)/include \
      -I $(QT_TOP_DIR)/include/QtWidgets \
      -I $(QT_TOP_DIR)/include/QtGui \
      -I $(QT_TOP_DIR)/include/QtCore \
      -I $(QT_TOP_DIR)/include/QtXml \
      -I $(QT_TOP_DIR)/include/QtConcurrent \
      -I $(QT_TOP_DIR)/include/QtNetwork \
      -I $(QT_TOP_DIR)/include/QtWebKitWidgets \
      -I $(QT_TOP_DIR)/include/QtWebView \
      -I $(QT_TOP_DIR)/include/mkspecs/linux-g++
    QT_LIBS =    $(LIBPATH_COMPILER_FLAG)$(QT_LIBDIRS) \
      $(LIBFLAG)Qt5Sql$(LIBLINKEXT) \
      $(LIBFLAG)Qt5WebKitWidgets$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Network$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Gui$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Qml$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Xml$(LIBLINKEXT) \
      $(LIBFLAG)Qt5PrintSupport$(LIBLINKEXT) \
      $(LIBFLAG)Qt5WebKit$(LIBLINKEXT) \
      $(LIBFLAG)Qt5WebChannel$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Network$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Widgets$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Gui$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Concurrent$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Core$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Quick$(LIBLINKEXT)
    QT_SO_PATH=$(QT_TOP_DIR)/lib
    QT_PLUGIN_PATH=$(QT_TOP_DIR)/plugins
endif

# in Qt version of compile but not ours...
#      $(QT_LIBDIRS)/libicuuc.so.53 \
#      $(QT_LIBDIRS)/libicui18n.so.53 \
#      $(QT_LIBDIRS)/libicudata.so.53

#     $(LIBFLAG)Qt5WebView$(LIBLINKEXT) \

