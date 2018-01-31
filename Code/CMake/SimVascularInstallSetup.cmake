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

#-----------------------------------------------------------------------------
# Setup SimVascular Install options and directories
#-----------------------------------------------------------------------------
  #  "Enabling this option will install automatically SimVascular in Superbuild Mode" OFF)
option(SV_ENABLE_DISTRIBUTION
  "Enable Distribution Targets (CPack)" OFF)
mark_as_advanced(SV_ENABLE_DISTRIBUTION)

if(NOT SV_INSTALL_HOME_DIR)
  set(SV_INSTALL_HOME_DIR ".")
endif()

if(NOT SV_INSTALL_TIMESTAMP_DIR)
  set(SV_INSTALL_TIMESTAMP_DIR timestamp)
endif()

if(NOT SV_INSTALL_SCRIPT_DIR)
  set(SV_INSTALL_SCRIPT_DIR ".")
endif()

if(NOT SV_INSTALL_RUNTIME_DIR)
  set(SV_INSTALL_RUNTIME_DIR bin)
endif()

if(NOT SV_INSTALL_LIBRARY_DIR)
  set(SV_INSTALL_LIBRARY_DIR lib)
endif()

if(NOT SV_INSTALL_CMAKE_DIR)
  set(SV_INSTALL_CMAKE_DIR lib/cmake/simvascular-${SV_MAJOR_VERSION}.${SV_MINOR_VERSION})
endif()

if(NOT SV_INSTALL_ARCHIVE_DIR)
  set(SV_INSTALL_ARCHIVE_DIR lib)
endif()

if(NOT SV_INSTALL_INCLUDE_DIR)
  set(SV_INSTALL_INCLUDE_DIR include)
endif()

if(NOT SV_INSTALL_DATA_DIR)
  set(SV_INSTALL_DATA_DIR data)
endif()

if(NOT SV_INSTALL_DOC_DIR)
 set(SV_INSTALL_DOC_DIR doc)
endif()

if(NOT SV_INSTALL_DOXYGEN_DIR)
  set(SV_INSTALL_DOXYGEN_DIR ${SV_INSTALL_DOC_DIR}/doxygen)
endif()

if(NOT SV_INSTALL_TCL_CODE_DIR)
  #this refers to simvascular TCL code, not tcltk itself
  set(SV_INSTALL_TCL_CODE_DIR Tcl)
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Third Party install locations
if(NOT SV_INSTALL_EXTERNALS_RUNTIME_DIR)
  set(SV_INSTALL_EXTERNALS_RUNTIME_DIR lib)
endif()

if(NOT SV_INSTALL_MPI_RUNTIME_DIR)
  if(WIN32)
    set(SV_INSTALL_MPI_RUNTIME_DIR lib)
  else()
    set(SV_INSTALL_MPI_RUNTIME_DIR lib)
  endif()
endif()

if(NOT SV_INSTALL_MPI_LIBRARY_DIR)
  set(SV_INSTALL_MPI_LIBRARY_DIR lib)
endif()

if(NOT SV_INSTALL_MPI_EXE_DIR)
  set(SV_INSTALL_MPI_EXE_DIR ".")
endif()

if(NOT SV_INSTALL_EXTERNAL_EXE_DIR)
  set(SV_INSTALL_EXTERNAL_EXE_DIR ".")
endif()

#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Parasolid install locations
if(NOT SV_INSTALL_PARASOLID_RUNTIME_DIR)
  if(WIN32)
    set(SV_INSTALL_PARASOLID_RUNTIME_DIR ".")
  else()
    set(SV_INSTALL_PARASOLID_RUNTIME_DIR ".")
  endif()
endif()

if(NOT SV_INSTALL_PARASOLID_SCHEMA_DIR)
  set(SV_INSTALL_PARASOLID_SCHEMA_DIR schema)
endif()

if(NOT SV_INSTALL_RUNTIME_DIR)
 if(WIN32)
  set(SV_INSTALL_RUNTIME_DIR ".")
else()
  set(SV_INSTALL_RUNTIME_DIR ".")
endif()
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Get list of vars starting with SV_INSTALL and make native install var
getListOfVarsPrefix("SV_INSTALL" _VARLIST)
foreach(_var ${_VARLIST})
  string(REPLACE "SV_INSTALL" "SV_INSTALL_NATIVE" _var_native ${_var})
  if(${_var})
    file(TO_NATIVE_PATH ${${_var}} ${_var_native})
    dev_message("${_var_native} ${${_var_native}}")
  else()
    dev_message("NO ${_var}")
  endif()
endforeach()

if(SV_DEVELOPER_OUTPUT)
  getListOfVarsPrefix("SV_INSTALL" _VARLIST)
  list(INSERT _VARLIST 0 CMAKE_INSTALL_PREFIX)
  print_vars(_VARLIST)
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Setup Output directories for compiling
if(NOT DEFINED OUTBIN_DIR OR NOT DEFINED OUTLIB_DIR)
  set(OUTBIN_DIR "${SV_BINARY_DIR}/bin")
  set(OUTLIB_DIR ${SV_BINARY_DIR}/lib)
endif()

if(NOT DEFINED SV_DEVELOPER_SCRIPT_DIR)
  set(SV_DEVELOPER_SCRIPT_DIR "${SV_BINARY_DIR}")
endif()

if(NOT CMAKE_RUNTIME_OUTPUT_DIRECTORY)
  set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${OUTBIN_DIR}")
endif()

if(NOT CMAKE_LIBRARY_OUTPUT_DIRECTORY)
  set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${OUTLIB_DIR}")
endif()

if(NOT CMAKE_ARCHIVE_OUTPUT_DIRECTORY)
  set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${OUTLIB_DIR}")
endif()

mark_as_advanced(CMAKE_RUNTIME_OUTPUT_DIRECTORY
  CMAKE_LIBRARY_OUTPUT_DIRECTORY
  CMAKE_ARCHIVE_OUTPUT_DIRECTORY)

if(SV_DEVELOPER_OUTPUT)
  set(_VARLIST OUTBIN_DIR
    OUTLIB_DIR
    SCRIPT_DIR)
  print_vars(_VARLIST)
endif()
#-----------------------------------------------------------------------------
