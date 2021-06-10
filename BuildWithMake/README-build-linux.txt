------------------------------------------------------------------------
            Compiling Instructions for SimVascular on Linux
                       Revised 2021-06-10
------------------------------------------------------------------------

--------
Overview
--------

By default, SimVascular is configured to build on Windows using
makefiles.  You must override the deafult options to build on linux.

Our base test configuration for linux is:

Recommended:

Ubuntu 18.04 64-bit desktop (w/ patches)
Intel 7/9 processor
gcc/g++ version 7.5

Partially support platforms:

Ubuntu 20.04 64-bit desktop (w/ patches)
Intel 7/9 processor
gcc/g++ version 7.5 NOTE: gcc 9 is not supported!!

NOTE: On Ubuntu 20, you need to switch to the gcc 7 compilers to compile SV.  For help, see for example:

  * https://linuxconfig.org/how-to-switch-between-multiple-gcc-and-g-compiler-versions-on-ubuntu-20-04-lts-focal-fossa

Experimental platforms:

CentOS 8.1 64-bit (w/ patches)
Intel 7/9 processor
gcc/g++ version 8.3

Note: SimVascular builds and launches on CentOS but no testing has
been done.

-----------------------------------------------
Major Steps (2019.06 externals) - for Ubuntu 18
-----------------------------------------------

1. Ubuntu prerequisities
------------------------

You must install certain system libraries before you compile:

% cd Externals/Prep/2019.06
% ./ubuntu-18-prep.sh

2.  Building SimVascular
------------------------

% cd BuildWithMake
% source quick-build-linux.sh

3. Launching SimVascular
------------------------

% cd BuildWithMake
% ./sv       (default Qt gui)
% ./sv -tk   (old tcl/tk gui)

4. Override options (optional)
------------------------------

Override defaults with:

  * cluster_overrides.mk
  * global_overrides.mk
  * site_overrides.mk
  * pk_overrides.mk

See include.mk for all options.

5. Building an Installer (optional)
-----------------------------------

% cd BuildWithMake/Release
% make

6. Installing a distribution (optional)
---------------------------------------
% sudo mkdir -p /usr/local/package/simvascular
% sudo chmod a+rwx /usr/local/package/simvascular
% sudo tar --directory /usr/local -xvzpsf tar_files/simvascular-linux-x64.*.tar.gz

To install "simvascular" launch script in /usr/local/bin:

% sudo /usr/local/package/simvascular/yyyy-mm-dd/post-install.sh

where "yyyy-mm-dd" is the date of the release.  You can then launch simvascular
from the command line anywhere:

% simvascular

7.  To build external open source packages (very optional)
----------------------------------------------------------

% cd Externals/Prep/2019.06
% source ./ubuntu-18-prep.sh
% cd Externals/Make/2019.06
% source ./build-sv-exeternals-linux.sh
