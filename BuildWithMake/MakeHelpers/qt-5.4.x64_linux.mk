ifeq ($(CLUSTER), x64_linux)
    QT_TOP_DIR	= /opt/Qt5.4.2/5.4/gcc_64
    QT_DEFS = -D_REENTRANT -DQT_NO_DEBUG -DQT_WIDGETS_LIB -DQT_GUI_LIB -DQT_CORE_LIB -DNDEBUG -DQT_NO_THREAD
    QT_LIBDIRS = $(QT_TOP_DIR)/lib
    QT_BIN_LIBS = $(QT_TOP_DIR)/bin
    QT_MOC_PARSER=$(QT_TOP_DIR)/bin/moc
    QMAKE=$(QT_TOP_DIR)/bin/qmake
    QT_INCDIRS = \
      -I$(QT_TOP_DIR)/include \
      -I$(QT_TOP_DIR)/include/QtWidgets \
      -I$(QT_TOP_DIR)/include/QtGui \
      -I$(QT_TOP_DIR)/include/QtCore \
      -I$(QT_TOP_DIR)/include/mkspecs/linux-g++
    QT_LIBS =    $(LIBPATH_COMPILER_FLAG)$(QT_LIBDIRS) \
      $(LIBFLAG)Qt5Widgets$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Gui$(LIBLINKEXT) \
      $(LIBFLAG)Qt5Core$(LIBLINKEXT) \
      $(QT_LIBDIRS)/libicuuc.so.53 \
      $(QT_LIBDIRS)/libicui18n.so.53 \
      $(QT_LIBDIRS)/libicudata.so.53
    QT_SO_PATH=$(QT_TOP_DIR)/bin
endif
