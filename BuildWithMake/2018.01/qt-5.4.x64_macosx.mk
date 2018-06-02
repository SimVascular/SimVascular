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

ifeq ($(CLUSTER), x64_macosx)

    QT_TOP_DIR	= /usr/local/package/Qt5.4.2/5.4/clang_64
    QT_DEFS = -D__APPLE__ -D__GNUC__=4
    QT_DEFS += -DQT_NO_DEBUG -DQT_WIDGETS_LIB -DQT_GUI_LIB -DQT_CORE_LIB -DQT_XML_LIB -DQT_CONCURRENT_LIB -DQT_NETWORK_LIB -DQT_PLUGIN -DQT_WEBVIEW -DQT_WEBKITWIDGETS

ifeq ($(SV_USE_SHARED),0)
  QT_DEFS += -DQT_STATICPLUGIN
endif

    QT_LIBDIRS = $(QT_TOP_DIR)/lib
    QT_BIN_LIBS = $(QT_TOP_DIR)/bin

    QT_MOC_PARSER=$(QT_TOP_DIR)/bin/moc
    QT_UIC_CMD=$(QT_TOP_DIR)/bin/uic
    QT_RCC_CMD=$(QT_TOP_DIR)/bin/rcc
    QMAKE=$(QT_TOP_DIR)/bin/qmake

    QT_INCDIRS = -I$(QT_TOP_DIR)/lib/QtWidgets.framework/Versions/5/Headers \
                 -I$(QT_TOP_DIR)/lib/QtGui.framework/Versions/5/Headers \
                 -I$(QT_TOP_DIR)/lib/QtCore.framework/Versions/5/Headers \
                 -I$(QT_TOP_DIR)/lib/QtXml.framework/Versions/5/Headers \
                 -I$(QT_TOP_DIR)/lib/QtConcurrent.framework/Versions/5/Headers \
                 -I$(QT_TOP_DIR)/lib/QtNetwork.framework/Versions/5/Headers \
                 -I$(QT_TOP_DIR)/lib/QtWebView.framework/Versions/5/Headers \
                 -I$(QT_TOP_DIR)/lib/QtWebKitWidgets.framework/Versions/5/Headers \
                 -I$(QT_TOP_DIR)/mkspecs/macx-clang \
                 -I/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk/System/Library/Frameworks/OpenGL.framework/Versions/A/Headers \
                 -I/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk/System/Library/Frameworks/AGL.framework/Headers \
                 -F$(QT_TOP_DIR)/lib

    QT_MOC_INCDIRS = -I$(QT_TOP_DIR)/lib/QtWidgets.framework/Versions/5/Headers \
                 -I$(QT_TOP_DIR)/lib/QtGui.framework/Versions/5/Headers \
                 -I$(QT_TOP_DIR)/lib/QtCore.framework/Versions/5/Headers \
                 -I$(QT_TOP_DIR)/lib/QtXml.framework/Versions/5/Headers \
                 -I$(QT_TOP_DIR)/lib/QtConcurrent.framework/Versions/5/Headers \
                 -I$(QT_TOP_DIR)/lib/QtNetwork.framework/Versions/5/Headers \
                 -I$(QT_TOP_DIR)/lib/QtWebView.framework/Versions/5/Headers \
                 -I$(QT_TOP_DIR)/lib/QtWebKitWidgets.framework/Versions/5/Headers \
                 -I$(QT_TOP_DIR)/mkspecs/macx-clang \
                 -I/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk/System/Library/Frameworks/OpenGL.framework/Versions/A/Headers \
                 -I/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk/System/Library/Frameworks/AGL.framework/Headers \
                 -F$(QT_TOP_DIR)/lib

    QT_SO_PATH=$(QT_TOP_DIR)/lib
    QT_PLUGIN_PATH=$(QT_TOP_DIR)/plugins

    QT_LIBS= -F$(QT_TOP_DIR)/lib \
             -framework QtWebKitWidgets \
             -framework QtWebView \
             -framework QtWidgets -framework QtGui -framework QtCore \
             -framework QtXml -framework QtQml -framework QtConcurrent -framework QtNetwork \
             -framework DiskArbitration -framework IOKit -framework OpenGL -framework AGL
endif
