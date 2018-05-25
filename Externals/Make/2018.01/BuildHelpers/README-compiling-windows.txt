#-----------------------------------------------------
#      Compiling Instructions for SV16 on Windows
#                       Revised 2016-09-01
#-----------------------------------------------------

#--------
#Overview
#--------

#Building SimVascular Externals

#---------------
#Software Needed
#---------------

# CMake 3.5.1
# CL compiler (Visual Studio 2013)
# swig from http://www.swig.org/download.html
# Qt 5.4

#
#  Build on windows
#
#  The follow commands were used to create the SV16 distribution
#  for windows.  You can source this file.
#
#  NOTE: make sure that you have write permissions
#        to C:/cygwin64/usr/local/sv.  You may need to change
#        permissions usings Windows before starting
#        to compile.
#
#  NOTE: the installers for tcllib and tklib are interactive.
#        simply click "install" button when the windows pop up.


mkdir -p /usr/local/sv/ext/src
cd /usr/local/sv/ext/src
svn co https://www.simtk.org/svn/sv_thirdparty/trunk/SV16/src/BuildHelpers BuildHelpers

cd /usr/local/sv/ext/src/BuildHelpers

source CygwinHelpers/msvc_2013_x64
source Scripts/superbuild-windows-cl.sh

