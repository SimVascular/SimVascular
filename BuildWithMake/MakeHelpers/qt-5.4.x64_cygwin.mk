ifeq ($(CLUSTER), x64_cygwin)
    QT_TOP_DIR	= C:/OpenSource/Qt/Qt5.4.2/5.4/msvc2013_64_opengl
    QT_DEFS = -DWIN32 -DWIN64 -DQT_NO_DEBUG -DQT_WIDGETS_LIB -DQT_GUI_LIB -DQT_CORE_LIB -DNDEBUG -DQT_NO_THREAD
    QT_LIBDIRS = $(QT_TOP_DIR)/lib
    QT_BIN_LIBS = $(QT_TOP_DIR)/bin
    QT_MOC_PARSER=$(QT_TOP_DIR)/bin/moc.exe
    QMAKE=$(QT_TOP_DIR)/bin/qmake.exe
    QT_INCDIRS = \
      -I$(QT_TOP_DIR)/include \
      -I$(QT_TOP_DIR)/include/QtWidgets \
      -I$(QT_TOP_DIR)/include/QtGui \
      -I$(QT_TOP_DIR)/include/QtCore \
      -I$(QT_TOP_DIR)/include/mkspecs/win32-msvc2013
    QT_LIBS =    $(LIBPATH_COMPILER_FLAG)$(QT_LIBDIRS) \
      $(LIBFLAG)Qt5Widgets.lib \
      $(LIBFLAG)Qt5Gui.lib \
      $(LIBFLAG)Qt5Core.lib
    QT_SO_PATH=$(QT_TOP_DIR)/bin
endif
