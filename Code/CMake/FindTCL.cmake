# Copyright (c) Stanford University, The Regents of the University of
#               California, and others.
#
# All Rights Reserved.
#
# See Copyright-SimVascular.txt for additional details.
#
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject
# to the following conditions:
#
# The above copyright notice and this permission notice shall be included
# in all copies or substantial portions of the Software.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
# IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
# TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
# PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
# OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
# EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
# PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
# PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
# LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# - Find Tcl includes and libraries.
# This module finds if Tcl is installed and determines where the
# include files and libraries are. It also determines what the name of
# the library is. This code sets the following variables:
#  TCL_FOUND              = Tcl was found
#  TK_FOUND               = Tk was found
#  TCLTK_FOUND            = Tcl and Tk were found
#  TCL_LIBRARY            = path to Tcl library (tcl tcl80)
#  TCL_INCLUDE_PATH       = path to where tcl.h can be found
#  TCL_TCLSH              = path to tclsh binary (tcl tcl80)
#  TK_LIBRARY             = path to Tk library (tk tk80 etc)
#  TK_INCLUDE_PATH        = path to where tk.h can be found
#  TK_WISH                = full path to the wish executable
#
# In an effort to remove some clutter and clear up some issues for people
# who are not necessarily Tcl/Tk gurus/developpers, some variables were
# moved or removed. Changes compared to CMake 2.4 are:
# - The stub libraries are now found in FindTclStub.cmake
#   => they were only useful for people writing Tcl/Tk extensions.
# - TCL_LIBRARY_DEBUG and TK_LIBRARY_DEBUG were removed.
#   => these libs are not packaged by default with Tcl/Tk distributions.
#      Even when Tcl/Tk is built from source, several flavors of debug libs
#      are created and there is no real reason to pick a single one
#      specifically (say, amongst tcl84g, tcl84gs, or tcl84sgx).
#      Let's leave that choice to the user by allowing him to assign
#      TCL_LIBRARY to any Tcl library, debug or not.
# - TK_INTERNAL_PATH was removed.
#   => this ended up being only a Win32 variable, and there is a lot of
#      confusion regarding the location of this file in an installed Tcl/Tk
#      tree anyway (see 8.5 for example). If you need the internal path at
#      this point it is safer you ask directly where the *source* tree is
#      and dig from there.

set(proj TCL)

include(FindPackageHandleStandardArgs)
include(CMakeFindFrameworks)

if(NOT ${proj}_DIR)
  set(${proj}_DIR "${proj}_DIR-NOTFOUND" CACHE PATH "Path of toplevel ${proj} dir. Specify this if ${proj} cannot be found.")
  message(FATAL_ERROR "${proj}_DIR was not specified. Set ${proj}_DIR to the toplevel ${proj} dir that contains bin, lib, include")
endif()

find_package(TclSh REQUIRED)
find_package(Wish REQUIRED)

set(TCLTK_POSSIBLE_LIB_PATHS
  "${${proj}_DIR}/*"
  "${${proj}_DIR}/lib/*"
  )

unset(TCL_LIBRARY CACHE)
find_library(TCL_LIBRARY
  NAMES
  tcl${TCL_MAJOR_VERSION}.${TCL_MINOR_VERSION}
  PATHS
  ${TCLTK_POSSIBLE_LIB_PATHS}
  NO_DEFAULT_PATH
  )

unset(TK_LIBRARY CACHE)
find_library(TK_LIBRARY
  NAMES
  tk${TCL_MAJOR_VERSION}.${TCL_MINOR_VERSION}
  PATHS
  ${TCLTK_POSSIBLE_LIB_PATHS}
  NO_DEFAULT_PATH
  )

set(TCLTK_POSSIBLE_INCLUDE_PATHS
  "${${proj}_DIR}/*"
  "${${proj}_DIR}/include/*"
  )

unset(TCL_INCLUDE_PATH CACHE)
find_path(TCL_INCLUDE_PATH
  NAMES
  tcl.h
  PATHS
  ${TCLTK_POSSIBLE_INCLUDE_PATHS}
  NO_DEFAULT_PATH
  )

unset(TK_INCLUDE_PATH CACHE)
find_path(TK_INCLUDE_PATH
  NAMES
  tk.h
  PATHS
  ${TCLTK_POSSIBLE_INCLUDE_PATHS}
  NO_DEFAULT_PATH
  )

# handle the QUIETLY and REQUIRED arguments and set TCL_FOUND to TRUE if
# all listed variables are TRUE

find_package_handle_standard_args(TCL
  FOUND_VAR TCL_FOUND
  REQUIRED_VARS TCL_LIBRARY TCL_INCLUDE_PATH
  FAIL_MESSAGE "Could NOT find Tcl")

find_package_handle_standard_args(TCLTK
  FOUND_VAR TCLTK_FOUND
  REQUIRED_VARS TCL_LIBRARY TCL_INCLUDE_PATH TK_LIBRARY TK_INCLUDE_PATH
  FAIL_MESSAGE "Could NOT find TclTk")

find_package_handle_standard_args(TK
  FOUND_VAR TK_FOUND
  REQUIRED_VARS TK_LIBRARY TK_INCLUDE_PATH
  FAIL_MESSAGE "Could NOT find Tk")

mark_as_advanced(
  TCL_INCLUDE_PATH
  TK_INCLUDE_PATH
  TCL_LIBRARY
  TK_LIBRARY
  )
