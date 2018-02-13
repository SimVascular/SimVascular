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

#.rst:
# FindPythonLibs
# --------------
#
# Find python libraries
#
# This module finds if Python is installed and determines where the
# include files and libraries are.  It also determines what the name of
# the library is.  This code sets the following variables:
#
# ::
#
#   PYTHON_FOUND           - have the Python libs been found
#   PYTHON_LIBRARIES           - path to the python library
#   PYTHON_INCLUDE_PATH        - path to where Python.h is found (deprecated)
#   PYTHON_INCLUDE_DIRS        - path to where Python.h is found
#   PYTHON_DEBUG_LIBRARIES     - path to the debug library (deprecated)
#   PYTHON_VERSION_STRING  - version of the Python libs found (since CMake 2.8.8)
#
#
#
# The Python_ADDITIONAL_VERSIONS variable can be used to specify a list
# of version numbers that should be taken into account when searching
# for Python.  You need to set this variable before calling
# find_package(PythonLibs).
#
# If you'd like to specify the installation of Python to use, you should
# modify the following cache variables:
#
# ::
#
#   PYTHON_LIBRARY             - path to the python library
#   PYTHON_INCLUDE_DIR         - path to where Python.h is found
#
# If calling both ``find_package(PythonInterp)`` and
# ``find_package(PythonLibs)``, call ``find_package(PythonInterp)`` first to
# get the currently active Python version by default with a consistent version
# of PYTHON_LIBRARIES.

#=============================================================================
# Copyright 2001-2009 Kitware, Inc.
#
# Distributed under the OSI-approved BSD License (the "License");
# see accompanying file Copyright.txt for details.
#
# This software is distributed WITHOUT ANY WARRANTY; without even the
# implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
# See the License for more information.
#=============================================================================
# (To distribute this file outside of CMake, substitute the full
#  License text for the above reference.)

set(proj PYTHON)

include(FindPackageHandleStandardArgs)
include(CMakeFindFrameworks)
# Search for the python framework on Apple.

if(NOT ${proj}_DIR)
  set(${proj}_DIR "${proj}_DIR-NOTFOUND" CACHE PATH "Path of toplevel ${proj} dir. Specify this if ${proj} cannot be found.")
  message(FATAL_ERROR "${proj}_DIR was not specified. Set ${proj}_DIR to the toplevel ${proj} dir that contains bin, lib, include")
endif()

set(${proj}_POSSIBLE_LIB_PATHS
  "${${proj}_DIR}/*"
  "${${proj}_DIR}/lib/*"
  )

unset(${proj}_LIBRARY CACHE)
find_library(${proj}_LIBRARY
  NAMES
  python${${proj}_MAJOR_VERSION}.${${proj}_MINOR_VERSION}
  python${${proj}_MAJOR_VERSION}.${${proj}_MINOR_VERSION}mu
  python${${proj}_MAJOR_VERSION}.${${proj}_MINOR_VERSION}m
  python${${proj}_MAJOR_VERSION}.${${proj}_MINOR_VERSION}u
  PATHS
  ${${proj}_POSSIBLE_LIB_PATHS}
  NO_DEFAULT_PATH
  )

set(${proj}_POSSIBLE_INCLUDE_PATHS
  "${${proj}_DIR}/*"
  "${${proj}_DIR}/include/*"
  )

unset(${proj}_INCLUDE_DIR CACHE)
find_path(${proj}_INCLUDE_DIR
  NAMES
  Python.h
  PATHS
  ${${proj}_POSSIBLE_INCLUDE_PATHS}
  NO_DEFAULT_PATH
  )

set(${proj}_POSSIBLE_EXECUTABLE_PATHS
  "${${proj}_DIR}/*"
  "${${proj}_DIR}/bin/*"
  )

unset(${proj}_EXECUTABLE CACHE)
find_program(${proj}_EXECUTABLE
  NAMES
  python
  python${${proj}_MAJOR_VERSION}.${${proj}_MINOR_VERSION}
  PATHS
  ${${proj}_POSSIBLE_EXECUTABLE_PATHS}
  NO_DEFAULT_PATH
  )

set(${proj}_INCLUDE_PATH "${${proj}_INCLUDE_DIR}")
set(${proj}_INCLUDE_DIRS "${${proj}_INCLUDE_DIR}")

find_package_handle_standard_args(${proj}
  FOUND_VAR ${proj}_FOUND
  REQUIRED_VARS ${proj}_LIBRARY ${proj}_INCLUDE_DIR ${proj}_EXECUTABLE
  VERSION_VAR ${proj}_VERSION_STRING
  FAIL_MESSAGE "Could NOT find ${proj}")

mark_as_advanced(
  ${proj}_INCLUDE_DIR
  ${proj}_INCLUDE_DIRS
  ${proj}_INCLUDE_PATH
  ${proj}_LIBRARY
  ${proj}_EXECUTABLE
  )
