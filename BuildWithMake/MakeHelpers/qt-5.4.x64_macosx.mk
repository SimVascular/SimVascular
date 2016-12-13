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
