
call compile.qt.msvc.env.bat

configure -opensource -nomake examples -nomake tests -confirm-license -skip qtwebkit -skip qtwebengine -opengl desktop -mp -release -prefix C:/cygwin64/usr/local/sv/ext/bin/msvc/18.0/x64/relwithdebinfo/qt-5.4.2
