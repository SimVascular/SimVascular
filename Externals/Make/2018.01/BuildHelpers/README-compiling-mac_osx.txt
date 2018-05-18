#-----------------------------------------------------
#      Compiling Instructions for SV16 on Mac OSX
#                       Revised 2016-09-01
#-----------------------------------------------------

#--------
#Overview
#--------

#Building SimVascular Externals

#---------------
#Software Needed
#---------------

# CMake 3.4.0  (Mac OSX Version)
# tar
# zip
# MacPorts
#
#  Build on macosx
#
#  NOTE: the installers for tcllib and tklib are interactive.
#        simply click "install" button when the windows pop up.

cd /usr/local/sv/ext/src/BuildHelpers/
Scripts/superbuild-mac_osx-clang.sh
