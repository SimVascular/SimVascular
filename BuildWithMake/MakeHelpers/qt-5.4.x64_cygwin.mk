ifeq ($(CLUSTER), x64_cygwin)
    QT_TOP_DIR	= C:/OpenSource/Qt/Qt5.4.2/5.4/msvc2013_64_opengl

    QT_DEFS = -DNDEBUG -DWIN32 -DWIN64
    QT_DEFS += -DQT_NO_DEBUG -DQT_WIDGETS_LIB -DQT_GUI_LIB -DQT_CORE_LIB -DQT_XML_LIB -DQT_CONCURRENT_LIB -DQT_NETWORK_LIB -DQT_PLUGIN -DQT_WEBVIEW -DQT_WEBKITWIDGETS

ifeq ($(SV_USE_SHARED),0)
  QT_DEFS += -DQT_STATICPLUGIN
endif

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
      -I$(QT_TOP_DIR)/include/QtWebView \
      -I$(QT_TOP_DIR)/include/QtWebKitWidgets \
      -I$(QT_TOP_DIR)/include/mkspecs/win32-msvc2013
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
      -I $(QT_TOP_DIR)/include/mkspecs/win32-msvc2013
    QT_LIBS =    $(LIBPATH_COMPILER_FLAG)$(QT_LIBDIRS) \
      $(LIBFLAG)Qt5WebKitWidgets$(LIBLINKEXT) \
      $(LIBFLAG)Qt5WebView$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Network$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Xml$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Widgets$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Gui$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Concurrent$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Core$(LIBLINKEXT)
    QT_SO_PATH=$(QT_TOP_DIR)/bin
    QT_PLUGIN_PATH=$(QT_TOP_DIR)/plugins
endif
