------------------------------------------------------------------------
      Compiling Instructions for itk/vtk on Windows for mingw64
                       Revised 2015-08-12
------------------------------------------------------------------------

--------
Overview
--------

Building itk/vtk with the mingw64 compilers is possible on Windows
using CMake.  It is unknown if you can build them using the
Cygwin version of cmake without explicitly linking against
the cygwin dll, so there were built here using the Windows
version of the mingw64 compilers.

---------------
Software Needed
---------------

CMake 3.3.0  (Windows Version)
mingw64 compilers (Windows Version for itk/vtk)
mingw64 compilers (Cygwin version to build tcl/tk from scripts)

TO DO: clean up from notes below!

Set default compilers:

C:       C:/Program Files/mingw-w64/x86_64-4.9.2-win32-seh-rt_v4-rev3/mingw64/bin/gcc.exe
C++:     C:/Program Files/mingw-w64/x86_64-4.9.2-win32-seh-rt_v4-rev3/mingw64/bin/g++.exe
Fortran: C:/Program Files/mingw-w64/x86_64-4.9.2-win32-seh-rt_v4-rev3/mingw64/bin/gfortran.exe

CMAKE_BUILD_TYPE RelWithDebInfo
need to add -Wa,-mbig-obj to CMAKE_CXX_FLAGS_RELWITHDEBINFO


CMAKE_MAKE_PROGRAM:

C:/Program Files/mingw-w64/x86_64-4.9.2-win32-seh-rt_v4-rev3/mingw64/bin/mingw32-make.exe

ignore popup errors of missing .dll files

C:/cygwin64/SV15/bin/mingw64-gcc4.9/x64/tcltk-8.5.18/include

Run a MINGW32 Windows terminal

cd C:/cygwin64/...

mingw32-make
