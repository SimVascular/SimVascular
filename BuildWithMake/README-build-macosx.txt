------------------------------------------------------------------------
            Compiling Instructions for SimVascular on Linux
                       Revised 2014-08-14
------------------------------------------------------------------------

sudo port install py27-scipy
sudo port install py27-numpy

check out /opt/local/bin/gfortran-mp-4.9

--------
Overview
--------

By default, SimVascular is configured to build on macosx using
makefiles.  Our base test configuration for macosx is:

Apple OSX 10.10.5 64-bit
Intel 7 processor

clang/clang++ version 7.0
macports mpich/gfortran5

-----------
Major Steps
-----------

1. MacOSX prerequisities
------------------------
XCode command line tools are required
% xcode-select --install

MacPorts is required which can be downloaded at https://www.macports.org
The following packages are required to build simvascular

### compilers
% sudo port install gcc5
% sudo port install gfortran5
% sudo port install mpich-gcc5

2.  /sv_extern
--------------
Download necessary external packages from simtk.org:

% svn co https://simtk.org/svn/sv_private/trunk sv_private_trunk
% svn co https://simtk.org/svn/sv_thirdparty/trunk/macos sv_thirdparty_macosx
% svn co https://simtk.org/svn/sv_thirdparty/trunk/src sv_thirdparty_src

untar all files into "/sv_extern", e.g. using bash:

% cd sv_thirdparty_macosx
% for f in *.tar.gz; do tar -xf $f -C /;done

or using tclsh:

% cd sv_thirdparty_macosx
% tclsh
tclsh% foreach fn [glob *.tar.gz] {
tclsh%   tar --directory / -xvzf $fn
tclsh% }
tclsh% exit

repeat for "sv_thirdparty_src" and "sv_private_trunk".

4. Override options
-------------------
Override defaults with:

  * cluster_overrides.mk
  * global_overrides.mk
  * site_overrides.mk
  * pk_overrides.mk

Building with clang and gfortran compilers and normal /sv_extern should
build "out of the box" without required overrides.

See include.mk for all options.  The most common:

COMPILER_VERSION = clang_70
EXCLUDE_ALL_BUT_THREEDSOLVER = 1 (build flow solver only)

5. Build
--------
% cd simvascular/Code
% make

6. Running developer version
----------------------------
% ./sv

7. Build release
-----------------
% cd Release
% make
