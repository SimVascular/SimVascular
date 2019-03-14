------------------------------------------------------------------------
            Compiling Instructions for SimVascular on Linux
                       Revised 2019-03-11
------------------------------------------------------------------------

--------
Overview
--------

By default, SimVascular is configured to build on Windows using
makefiles.  You must override the deafult options to build on linux.

Our base test configuration for linux is:

Ubuntu 16.04 64-bit desktop (w/ patches)
Intel 7/9 processor
gcc/g++ version 5.4

-------------------------------
Major Steps (2019.02 externals)
-------------------------------

1. Ubuntu prerequisities
------------------------

You must install certain system libraries before you compile:

% cd Externals/Prep/2019.02
% ./ubuntu-16-prep.sh

2.  Building SimVascular
------------------------

% cd BuildWithMake
% source quick-build-linux.sh

3. Launching SimVascular
------------------------

% cd BuildWithMake
% ./sv       (default Qt gui)
% ./sv -tk   (old tcl/tk gui)

4. Building an Installer (optional)
-----------------------------------

% cd BuildWithMake/Release
% make

5. Override options (optional)
------------------------------

Override defaults with:

  * cluster_overrides.mk
  * global_overrides.mk
  * site_overrides.mk
  * pk_overrides.mk

See include.mk for all options.

6.  To build external open source packages (very optional)
----------------------------------------------------------

% cd Externals/Make/2019.02
% source build-sv-exeternals-linux.sh

## 7. Installing a distribution (out of date)
## ----------------------------
## sudo tar --directory /usr/local -xvzpsf simvascular-linux-x64.*.tar.gz
## sudo /usr/local/package/simvascular/xxxxxxxx/post-install.sh
##
## % sudo apt-get install gcc-multilib
## % sudo apt-get install ia32-libs
