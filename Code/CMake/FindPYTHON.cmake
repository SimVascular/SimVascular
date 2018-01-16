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

if(NOT "${PYTHON_DIR}" STREQUAL "")
  set(PYTHON_POSSIBLE_LIB_PATHS ${PYTHON_DIR}/lib ${PYTHON_POSSIBLE_LIB_PATHS})
  set(PYTHON_POSSIBLE_EXECUTABLE_PATHS ${PYTHON_DIR}/bin ${PYTHON_POSSIBLE_EXECUTABLE_PATHS})
  set(PYTHON_POSSIBLE_INCLUDE_PATHS ${PYTHON_DIR}/include ${PYTHON_POSSIBLE_INCLUDE_PATHS})
endif()

if(NOT "${PYTHON_LIBRARIES}" STREQUAL "")
  get_filename_component(PYTHON_POSSIBLE_LIB_PATHS ${PYTHON_LIBRARIES} PATH)
endif()
if(NOT "${PYTHON_EXECUTABLE}" STREQUAL "")
  get_filename_component(PYTHON_POSSIBLE_EXECUTABLE_PATHS ${PYTHON_EXECUTABLE} PATH)
endif()

if(NOT "${PYTHON_INCLUDE_PATH}" STREQUAL "")
    set(PYTHON_POSSIBLE_INCLUDE_PATHS
    "${PYTHON_INCLUDE_PATH}")
  set(_Python_LIBRARY_PATH_HINT
    "${PYTHON_INCLUDE_PATH}")
endif()

include(CMakeFindFrameworks)
# Search for the python framework on Apple.
cmake_find_frameworks(Python)

# Save CMAKE_FIND_FRAMEWORK
if(DEFINED CMAKE_FIND_FRAMEWORK)
  set(_PythonLibs_CMAKE_FIND_FRAMEWORK ${CMAKE_FIND_FRAMEWORK})
else()
  unset(_PythonLibs_CMAKE_FIND_FRAMEWORK)
endif()
# To avoid picking up the system Python.h pre-maturely.
set(CMAKE_FIND_FRAMEWORK LAST)

set(_PYTHON1_VERSIONS 1.6 1.5)
set(_PYTHON2_VERSIONS 2.7 2.6 2.5 2.4 2.3 2.2 2.1 2.0)
set(_PYTHON3_VERSIONS 3.6 3.5 3.4 3.3 3.2 3.1 3.0)

if(PythonLibs_FIND_VERSION)
  if(PythonLibs_FIND_VERSION_COUNT GREATER 1)
    set(_PYTHON_FIND_MAJ_MIN "${PythonLibs_FIND_VERSION_MAJOR}.${PythonLibs_FIND_VERSION_MINOR}")
    unset(_PYTHON_FIND_OTHER_VERSIONS)
    if(PythonLibs_FIND_VERSION_EXACT)
      if(_PYTHON_FIND_MAJ_MIN STREQUAL PythonLibs_FIND_VERSION)
        set(_PYTHON_FIND_OTHER_VERSIONS "${PythonLibs_FIND_VERSION}")
      else()
        set(_PYTHON_FIND_OTHER_VERSIONS "${PythonLibs_FIND_VERSION}" "${_PYTHON_FIND_MAJ_MIN}")
      endif()
    else()
      foreach(_PYTHON_V ${_PYTHON${PythonLibs_FIND_VERSION_MAJOR}_VERSIONS})
        if(NOT _PYTHON_V VERSION_LESS _PYTHON_FIND_MAJ_MIN)
          list(APPEND _PYTHON_FIND_OTHER_VERSIONS ${_PYTHON_V})
        endif()
       endforeach()
    endif()
    unset(_PYTHON_FIND_MAJ_MIN)
  else()
    set(_PYTHON_FIND_OTHER_VERSIONS ${_PYTHON${PythonLibs_FIND_VERSION_MAJOR}_VERSIONS})
  endif()
else()
	#set(_PYTHON_FIND_OTHER_VERSIONS ${_PYTHON3_VERSIONS} ${_PYTHON2_VERSIONS} ${_PYTHON1_VERSIONS})
  set(_PYTHON_FIND_OTHER_VERSIONS ${_PYTHON2_VERSIONS})
endif()

# Set up the versions we know about, in the order we will search. Always add
# the user supplied additional versions to the front.
# If FindPythonInterp has already found the major and minor version,
# insert that version between the user supplied versions and the stock
# version list.
set(_Python_VERSIONS ${Python_ADDITIONAL_VERSIONS})
if(DEFINED PYTHON_VERSION_MAJOR AND DEFINED PYTHON_VERSION_MINOR)
  list(APPEND _Python_VERSIONS ${PYTHON_VERSION_MAJOR}.${PYTHON_VERSION_MINOR})
endif()
list(APPEND _Python_VERSIONS ${_PYTHON_FIND_OTHER_VERSIONS})

unset(_PYTHON_FIND_OTHER_VERSIONS)
unset(_PYTHON1_VERSIONS)
unset(_PYTHON2_VERSIONS)
unset(_PYTHON3_VERSIONS)

foreach(_CURRENT_VERSION ${_Python_VERSIONS})
  string(REPLACE "." "" _CURRENT_VERSION_NO_DOTS ${_CURRENT_VERSION})
  if(WIN32)
    find_library(PYTHON_DEBUG_LIBRARY
      NAMES python${_CURRENT_VERSION_NO_DOTS}_d python
      HINTS ${_Python_LIBRARY_PATH_HINT}
      PATHS
      ${PYTHON_POSSIBLE_LIB_PATHS}
      [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\${_CURRENT_VERSION}\\InstallPath]/libs/Debug
      [HKEY_CURRENT_USER\\SOFTWARE\\Python\\PythonCore\\${_CURRENT_VERSION}\\InstallPath]/libs/Debug
      [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\${_CURRENT_VERSION}\\InstallPath]/libs
      [HKEY_CURRENT_USER\\SOFTWARE\\Python\\PythonCore\\${_CURRENT_VERSION}\\InstallPath]/libs
      NO_DEFAULT_PATH
      )
  endif()

  set(PYTHON_FRAMEWORK_LIBRARIES)
  if(Python_FRAMEWORKS AND NOT PYTHON_LIBRARY)
    foreach(dir ${Python_FRAMEWORKS})
      list(APPEND PYTHON_FRAMEWORK_LIBRARIES
           ${dir}/Versions/${_CURRENT_VERSION}/lib)
    endforeach()
  endif()
  find_library(PYTHON_LIBRARY
    NAMES
      python${_CURRENT_VERSION_NO_DOTS}
      python${_CURRENT_VERSION}mu
      python${_CURRENT_VERSION}m
      python${_CURRENT_VERSION}u
      python${_CURRENT_VERSION}
    HINTS
      ${_Python_LIBRARY_PATH_HINT}
    PATHS
      ${PYTHON_POSSIBLE_LIB_PATHS}
      ${PYTHON_FRAMEWORK_LIBRARIES}
      [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\${_CURRENT_VERSION}\\InstallPath]/libs
      [HKEY_CURRENT_USER\\SOFTWARE\\Python\\PythonCore\\${_CURRENT_VERSION}\\InstallPath]/libs
    # Avoid finding the .dll in the PATH.  We want the .lib.
    NO_DEFAULT_PATH
  )
  # Look for the static library in the Python config directory
  find_library(PYTHON_LIBRARY
    NAMES python${_CURRENT_VERSION_NO_DOTS} python${_CURRENT_VERSION}
    # Avoid finding the .dll in the PATH.  We want the .lib.
    NO_SYSTEM_ENVIRONMENT_PATH
    # This is where the static library is usually located
    PATH_SUFFIXES python${_CURRENT_VERSION}/config
  )

  # Don't search for include dir until library location is known
  if(PYTHON_LIBRARY)

    # Use the library's install prefix as a hint
    set(_Python_INCLUDE_PATH_HINT)
    get_filename_component(_Python_PREFIX ${PYTHON_LIBRARY} PATH)
    get_filename_component(_Python_PREFIX ${_Python_PREFIX} PATH)
    if(_Python_PREFIX)
      set(_Python_INCLUDE_PATH_HINT ${_Python_PREFIX}/include)
    endif()
    unset(_Python_PREFIX)

    # Add framework directories to the search paths
    set(PYTHON_FRAMEWORK_INCLUDES)
    if(Python_FRAMEWORKS AND NOT PYTHON_INCLUDE_DIR)
      foreach(dir ${Python_FRAMEWORKS})
        list(APPEND PYTHON_FRAMEWORK_INCLUDES
          ${dir}/Versions/${_CURRENT_VERSION}/include)
      endforeach()
    endif()

    find_path(PYTHON_INCLUDE_DIR
      NAMES Python.h
      HINTS
        ${_Python_INCLUDE_PATH_HINT}
      PATHS
        ${PYTHON_POSSIBLE_INCLUDE_PATHS}
        ${PYTHON_FRAMEWORK_INCLUDES}
        [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\${_CURRENT_VERSION}\\InstallPath]/include
        [HKEY_CURRENT_USER\\SOFTWARE\\Python\\PythonCore\\${_CURRENT_VERSION}\\InstallPath]/include
      PATH_SUFFIXES
        python${_CURRENT_VERSION}mu
        python${_CURRENT_VERSION}m
        python${_CURRENT_VERSION}u
        python${_CURRENT_VERSION}
	#NO_DEFAULT_PATH
    )
  endif()

  # For backward compatibility, set PYTHON_INCLUDE_PATH.
  set(PYTHON_INCLUDE_PATH "${PYTHON_INCLUDE_DIR}")

  if(PYTHON_INCLUDE_DIR AND EXISTS "${PYTHON_INCLUDE_DIR}/patchlevel.h")
    file(STRINGS "${PYTHON_INCLUDE_DIR}/patchlevel.h" python_version_str
         REGEX "^#define[ \t]+PY_VERSION[ \t]+\"[^\"]+\"")
    string(REGEX REPLACE "^#define[ \t]+PY_VERSION[ \t]+\"([^\"]+)\".*" "\\1"
                         PYTHON_VERSION_STRING "${python_version_str}")
    unset(python_version_str)
  endif()

  if(PYTHON_LIBRARY AND PYTHON_INCLUDE_DIR)
    break()
  endif()
endforeach()

foreach(_CURRENT_VERSION ${_Python_VERSIONS})
  set(_Python_NAMES python${_CURRENT_VERSION})
   if(WIN32)
     list(APPEND _Python_NAMES python)
   endif()
     find_program(PYTHON_EXECUTABLE
                  NAMES ${_Python_NAMES}
		  HINTS
		  ${PYTHON_POSSIBLE_EXECUTABLE_PATHS}
		  PATHS
		  ${PYTHON_POSSIBLE_EXECUTABLE_PATHS}
		  [HKEY_LOCAL_MACHINE\\SOFTWARE\\Python\\PythonCore\\${_CURRENT_VERSION}\\InstallPath]
		  /usr/bin
		  /usr/local/bin
		  NO_DEFAULT_PATH)
endforeach()

unset(_Python_INCLUDE_PATH_HINT)
unset(_Python_LIBRARY_PATH_HINT)

# We use PYTHON_INCLUDE_DIR, PYTHON_LIBRARY and PYTHON_DEBUG_LIBRARY for the
# cache entries because they are meant to specify the location of a single
# library. We now set the variables listed by the documentation for this
# module.
set(PYTHON_INCLUDE_DIRS "${PYTHON_INCLUDE_DIR}")
set(PYTHON_DEBUG_LIBRARIES "${PYTHON_DEBUG_LIBRARY}")

# These variables have been historically named in this module different from
# what SELECT_LIBRARY_CONFIGURATIONS() expects.
set(PYTHON_LIBRARY_DEBUG "${PYTHON_DEBUG_LIBRARY}")
set(PYTHON_LIBRARY_RELEASE "${PYTHON_LIBRARY}")
include(SelectLibraryConfigurations)
SELECT_LIBRARY_CONFIGURATIONS(PYTHON)

# Restore CMAKE_FIND_FRAMEWORK
if(DEFINED _PythonLibs_CMAKE_FIND_FRAMEWORK)
  set(CMAKE_FIND_FRAMEWORK ${_PythonLibs_CMAKE_FIND_FRAMEWORK})
  unset(_PythonLibs_CMAKE_FIND_FRAMEWORK)
else()
  unset(CMAKE_FIND_FRAMEWORK)
endif()

include(FindPackageHandleStandardArgs)
find_package_handle_standard_args(PythonLibs
  FOUND_VAR PYTHONLIBS_FOUND
  REQUIRED_VARS PYTHON_LIBRARY PYTHON_INCLUDE_DIR PYTHON_EXECUTABLE
  VERSION_VAR PYTHON_VERSION_STRING
  FAIL_MESSAGE "Could not find PYTHON")
if(PYTHONLIBS_FOUND)
  set(PYTHON_FOUND TRUE)
else()
  set(PYTHON_FOUND FALSE)
endif()
