ifeq ($(CLUSTER), x64_macosx)
    QT_TOP_DIR	= /usr/local/package/Qt5.4.2/5.4/clang_64
    QT_DEFS = -DQT_NO_DEBUG -DQT_WIDGETS_LIB -DQT_GUI_LIB -DQT_CORE_LIB -D__APPLE__ -D__GNUC__=4
    QT_LIBDIRS = $(QT_TOP_DIR)/lib
    QT_BIN_LIBS = $(QT_TOP_DIR)/bin
    QT_MOC_PARSER=$(QT_TOP_DIR)/bin/moc
    QMAKE=$(QT_TOP_DIR)/bin/qmake
    QT_INCDIRS = -I/usr/local/package/Qt5.4.2/5.4/clang_64/lib/QtWidgets.framework/Versions/5/Headers \
                 -I/usr/local/package/Qt5.4.2/5.4/clang_64/lib/QtGui.framework/Versions/5/Headers \
                 -I/usr/local/package/Qt5.4.2/5.4/clang_64/lib/QtCore.framework/Versions/5/Headers \
                 -I/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk/System/Library/Frameworks/OpenGL.framework/Versions/A/Headers \
                 -I/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX10.11.sdk/System/Library/Frameworks/AGL.framework/Headers \
                 -I/usr/local/package/Qt5.4.2/5.4/clang_64/mkspecs/macx-clang -F/usr/local/package/Qt5.4.2/5.4/clang_64/lib
    QT_SO_PATH=$(QT_TOP_DIR)/bin
    QT_LIBS= -F/usr/local/package/Qt5.4.2/5.4/clang_64/lib -framework QtWidgets -framework QtGui -framework QtCore \
             -framework DiskArbitration -framework IOKit -framework OpenGL -framework AGL 
endif
