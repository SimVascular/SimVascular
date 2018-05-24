REM @echo off

REM need to clean the garbage out of the path
SET PATH=%SystemRoot%\system32;%SystemRoot%;%SystemRoot%\System32\Wbem

REM Microsoft Visual Studio
REM touch .gitignore in qtbase
REM configure -opensource -nomake examples -nomake tests -confirm-license -skip qtwebkit -skip qtwebengine -opengl desktop -mp -release

call "C:/Program Files (x86)/Microsoft Visual Studio 12.0/VC/vcvarsall.bat" x64

REM Strawberry Perl
SET PATH=C:/OpenSource/Strawberry/perl/c/bin;C:/OpenSource/Strawberry/perl/bin;C:/OpenSource/Strawberry/perl/site/bin;%PATH%

REM Ruby
SET PATH=C:/OpenSource/Ruby24-x64/bin;%PATH%

REM Edit this location to point to the source code of Qt
SET _ROOT=C:/cygwin64/usr/local/sv/ext/src/qt-5.4.2

SET PATH=%_ROOT%/qtbase/bin;%_ROOT%/gnuwin32/bin;%PATH%

REM Uncomment the below line when building with OpenSSL enabled. If so, make sure the directory points
REM to the correct location (binaries for OpenSSL).
SET PATH=C:/OpenSource/OpenSSL-Win64/bin;%PATH%

REM When compiling with ICU, uncomment the lines below and change <icupath> appropriately:
SET INCLUDE=C:/cygwin64/usr/local/sv/ext/bin/msvc/18.0/x64/relwithdebinfo/icu-5.3.1/include;%INCLUDE%
SET LIB=C:/cygwin64/usr/local/sv/ext/bin/msvc/18.0/x64/relwithdebinfo/icu-5.3.1/lib;%LIB%
SET PATH=C:/cygwin64/usr/local/sv/ext/bin/msvc/18.0/x64/relwithdebinfo/icu-5.3.1/lib;%PATH%

REM python

SET PYTHONHOME=C:/cygwin64/usr/local/sv/ext/bin/msvc/18.0/x64/relwithdebinfo/python-2.7.13
SET PYTHONPATH=C:/cygwin64/usr/local/sv/ext/bin/msvc/18.0/x64/relwithdebinfo/python-2.7.13/lib/python2.7/lib-dynload;C:/cygwin64/usr/local/sv/ext/bin/msvc/18.0/x64/relwithdebinfo/python-2.7.13/lib;C:/cygwin64/usr/local/sv/ext/bin/msvc/18.0/x64/relwithdebinfo/python-2.7.13/lib/python2.7;C:/cygwin64/usr/local/sv/ext/bin/msvc/18.0/x64/relwithdebinfo/python-2.7.13/lib/python2.7/site-packages
SET PATH=C:/cygwin64/usr/local/sv/ext/bin/msvc/18.0/x64/relwithdebinfo/python-2.7.13/bin;%PATH%
SET INCLUDE=C:/cygwin64/usr/local/sv/ext/bin/msvc/18.0/x64/relwithdebinfo/python-2.7.13/include;%INCLUDE%

REM Contrary to earlier recommendations, do NOT set QMAKESPEC.

SET _ROOT=

REM Keeps the command line open when this script is run.
REM cmd /k

REM configure -opensource -nomake examples -nomake tests -confirm-license -skip qtwebkit -opengl desktop -mp -release -prefix C:/cygwin64/usr/local/sv/ext/bin/msvc/18.0/x64/relwithdebinfo/qt-5.4.2

REM nmake
