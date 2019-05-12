------------------------------------------------------------------------
            Compiling Instructions for SimVascular on Windows
                       Revised 2019-03-11
------------------------------------------------------------------------

--------
Overview
--------

By default, SimVascular is configured to build on Windows using
makefiles.

You can build a developer version using CMake, but you cannot
currently build the windows installer (i.e. .msi) using cmake.
You must use the make system to build windows installers.

Building the "external" open source packages is very complicated
and requires additional packages not discussed below. Only
advanced users should attempt to build the externals open source
packages.

Our base test configuration for Windows is:

Windows 10 64-bit desktop (w/ patches)
Intel 7/9 processor

These instructions are valid for the 2018.05 version of the
externals only.  See below for older versions.

You CANNOT mix and match compiler versions, you must use
the required version of MSVC depending on externals version.

-------------------------------
Major Steps (2018.05 externals)
-------------------------------

1. Windows prerequisities
-------------------------
The following packages are required to build simvascular

A. Recent Cygwin 64 bit (www.cygwin.com) with at least:

   make, tclsh, zip, gzip, tar, date, patch

   installed in the default C:/cygwin64

We use:

   CYGWIN_NT-10.0 xxxxxxx 2.10.0(0.325/5/3) 2018-02-02 15:16 x86_64 Cygwin

B. MSVC 2015 (vs 14.x) win64 compiler  -> default
   install directory (e.g. "Program Files") okay

We use the community edition of the MSVC 2015 compiler
(Windows 64-bit only) but commercial versions should also work.

2.  Building SimVascular
------------------------

% cd BuildWithMake
% source quick-build-windows.sh

3. Launching SimVascular
------------------------

% cd BuildWithMake
% sv       (default Qt gui)
% sv -tk   (old tcl/tk gui)

4. Building an Installer (optional)
-----------------------------------

% cd Release
% make
