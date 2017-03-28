------------------------------------------------------------------------
            Compiling Instructions for SimVascular on Windows
                       Revised 2017-03-28
------------------------------------------------------------------------

--------
Overview
--------

By default, SimVascular is configured to build on Windows using
makefiles.

Our base test configuration for linux is:

Windows 10 64-bit desktop (w/ patches)
Intel 7 processor

-----------
Major Steps
-----------

1. Windows prerequisities
-------------------------
The following packages are required to build simvascular

A.  Qt 5.4.2  -> installed in C:/OpenSource/Qt/Qt5.4.2

You can download from Qt's website or use the installer found on simvascular.stanford.edu:

http://simvascular.stanford.edu/downloads/public/open_source/windows/qt/5.4/qt-opensource-windows-x86-msvc2013_64_opengl-5.4.2.exe

B. MSVC 2013 (12.5) win64 compiler  -> default install directory (e.g. "Program Files") is okay

We use the community edition of the MSVC 2013 compiler (Windows 64-bit only)

2.  Building SimVascular
------------------------

% cd BuildWithMake
% source quick-build-windows.sh
