------------------------------------------------------------------------
            Compiling Instructions for SimVascular on Linux
                       Revised 2014-08-14
------------------------------------------------------------------------

--------
Overview
--------

By default, SimVascular is configured to build on linux using
makefiles.  Our base test configuration for linux is:

Ubuntu 12.04 64-bit desktop (w/ patches)
Intel 7 processor

gcc/g++/gfortran version 4.6.3

and/or

ifort/icpc/icc intel compilers verison 13.0 (2013.1.117)

-----------
Major Steps
-----------

1. Ubuntu prerequisities
------------------------
The following packages are required to build simvascular

### compilers
% sudo apt-get install g++
% sudo apt-get install gfortran
### for flowsolver
% sudo apt-get install libmpich2-dev
### for vtk
% sudo apt-get install libglu1-mesa-dev
% sudo apt-get install libxt-dev
### used by SV make scripts
% sudo apt-get install tcl8.5
### used by SV to read DICOM headers
% sudo apt-get install dcmtk
% sudo apt-get install libgdcm-tools

% sudo apt-get install gcc-multilib
% sudo apt-get install ia32-libs

#
#  choose your version of subversion (1.6, 1.7, 1.8)
#

#
# default subversion for precise (ubuntu 12.04) is 1.6
#

% sudo apt-get install subversion

#
#use PPA to get svn 1.7 on precise (ubuntu 12.04)
#
#https://launchpad.net/~svn/+archive/ppa
#
# add the following two lines to /etc/apt/sources.list
#
# deb http://ppa.launchpad.net/svn/ppa/ubuntu precise main
# deb-src http://ppa.launchpad.net/svn/ppa/ubuntu precise main
#
% sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-key A2F4C039
% sudo apt-get install subversion

#
#use third party to get svn 1.8 on precise (ubuntu 12.04)
#
% sudo sh -c 'echo "deb http://opensource.wandisco.com/ubuntu precise svn18" >> /etc/apt/sources.list.d/subversion18.list'
% sudo wget -q http://opensource.wandisco.com/wandisco-debian.gpg -O- | sudo apt-key add -
% sudo apt-get update
% sudo apt-get install subversion

#
# for cmake binary from www.cmake.org
#  NOTE: vtk requires at least cmake 2.8.12.1
#
%
%
#
# some optional helpers
#
% sudo apt-get install dos2unix
% sudo apt-get install emacs

2.  /sv_extern
--------------
Download necessary external packages from simtk.org:

% svn co https://simtk.org/svn/sv_private/trunk sv_private_trunk
% svn co https://simtk.org/svn/sv_thirdparty/trunk/linux sv_thirdparty_linux
% svn co https://simtk.org/svn/sv_thirdparty/trunk/src sv_thirdparty_src

untar all files into "/sv_extern", e.g. using bash:

% cd sv_thirdparty_linux
% for f in *.tar.gz; do tar -xf $f -C /;done

or using tclsh:

% cd sv_thirdparty_linux
% tclsh
tclsh% foreach fn [glob *.tar.gz] {
tclsh%   tar --directory / -xvzf $fn
tclsh% }
tclsh% exit

repeat for "sv_thirdparty_src" and "sv_private_trunk".

3. Checkout SV source code
--------------------------
% svn co https://simtk.org/svn/sv_reorg/trunk/ReleaseCandidate1 simvascular

4. Override options
-------------------
Override defaults with:

  * cluster_overrides.mk
  * global_overrides.mk
  * site_overrides.mk
  * pk_overrides.mk

Building with gnu compilers and normal /sv_extern should
build "out of the box" without required overrides.

See include.mk for all options.  The most common:

COMPILER_VERSION = intel_13.0  (build with intel instead of gcc)
EXCLUDE_ALL_BUT_THREEDSOLVER = 1 (build flow solver only)

# build with binary distribution of svls
MAKE_WITH_BINARY_SVLS = 1
MAKE_WITH_DUMMY_SVLS = 0

5. Copy meshsim license file
----------------------------
% cp license.dat simvascular/Licenses/MeshSim/license.dat

6. Build
--------
% cd simvascular/Code
% make

7. Running developer version
----------------------------
% cd simvascular/trunk/Code
% ./mysim

8. Build release
-----------------
% cd simvascular/trunk/Release
% make

9. Installing a distribution
----------------------------
sudo tar --directory /usr/local -xvzpsf simvascular-linux-x64.*.tar.gz
sudo /usr/local/package/simvascular/xxxxxxxx/post-install.sh
sudo /usr/local/package/simvascular/xxxxxxxx/post-solver-install.sh
