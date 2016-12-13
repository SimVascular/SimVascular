ifeq ($(CLUSTER), x64_linux)
    QT_TOP_DIR	= /opt/Qt5.4.2/5.4/gcc_64
    QT_DEFS = -DUNIX -D_REENTRANT -DNDEBUG
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
      $(LIBFLAG)Qt5WebKitWidgets$(LIBLINKEXT) \
      $(LIBFLAG)Qt5WebView$(LIBLINKEXT) \
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
      $(LIBFLAG)Qt5Quick$(LIBLINKEXT) \
      $(QT_LIBDIRS)/libicuuc.so.53 \
      $(QT_LIBDIRS)/libicui18n.so.53 \
      $(QT_LIBDIRS)/libicudata.so.53
    QT_SO_PATH=$(QT_TOP_DIR)/lib
    QT_PLUGIN_PATH=$(QT_TOP_DIR)/plugins
endif
