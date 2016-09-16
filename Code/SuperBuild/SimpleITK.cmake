# Copyright (c) 2014-2015 The Regents of the University of California.
# All Rights Reserved.
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
#
set(proj SimpleITK)

set(${proj}_DEPENDENCIES GDCM ITK)

ExternalProject_Include_Dependencies(${proj}
  PROJECT_VAR proj
  DEPENDS_VAR ${proj}_DEPENDENCIES
  EP_ARGS_VAR ${proj}_EXTERNAL_PROJECT_ARGS
  USE_SYSTEM_VAR ${CMAKE_PROJECT_NAME}_USE_SYSTEM_${proj}
  )

if(NOT ${CMAKE_PROJECT_NAME}_USE_SYSTEM_${proj})

  if(WIN32)
    set(${proj}_PFX_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_PFX_DIR} 
      CACHE PATH "On windows, there is a bug with SimpleITK source code directory path length, you can change this path to avoid it")
    set(${proj}_SRC_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_SRC_DIR} 
      CACHE PATH "On windows, there is a bug with SimpleITK source code directory path length, you can change this path to avoid it")
    set(${proj}_BLD_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_BLD_DIR}  
      CACHE PATH "On windows, there is a bug with SimpleITK source code directory path length, you can change this path to avoid it")
    set(${proj}_BIN_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_BIN_DIR}  
      CACHE PATH "On windows, there is a bug with SimpleITK source code directory path length, you can change this path to avoid it")
  else()
    set(${proj}_PFX_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_PFX_DIR})
    set(${proj}_SRC_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_SRC_DIR})
    set(${proj}_BLD_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_BLD_DIR})
    set(${proj}_BIN_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_BIN_DIR})
  endif()

  set(revision_tag "simvascular-patch-0.8.1")

  #ExternalProject_add(${proj}-download
  #  ${location_args}
  #  PREFIX ${${proj}_SRC_DIR}-download-prefix
  #  SOURCE_DIR ${${proj}_SRC_DIR}
  #  CONFIGURE_COMMAND ""
  #  INSTALL_COMMAND ""
  #  BUILD_COMMAND ""
  # )

  ExternalProject_Add(${proj}
   GIT_REPOSITORY "https://github.com/SimVascular/SimpleITK.git"
   GIT_TAG ${revision_tag}
   PREFIX ${${proj}_PFX_DIR}
   SOURCE_DIR ${${proj}_SRC_DIR}
   BINARY_DIR ${${proj}_BLD_DIR}
   UPDATE_COMMAND ""
   CMAKE_CACHE_ARGS
   -DCMAKE_CXX_COMPILER:STRING=${CMAKE_CXX_COMPILER}
   -DCMAKE_C_COMPILER:STRING=${CMAKE_C_COMPILER}
   -DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS}
   -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
   -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
   -DCMAKE_THREAD_LIBS:STRING=-lpthread
   -DBUILD_EXAMPLES:BOOL=OFF
   -DBUILD_SHARED_LIBS:BOOL=${SV_USE_${proj}_SHARED}
   -DBUILD_TESTING:BOOL=OFF
   -DWRAP_CSHARP:BOOL=OFF
   -DWRAP_TCL:BOOL=OFF
   -DWRAP_LUA:BOOL=OFF
   -DWRAP_JAVA:BOOL=OFF
   -DWRAP_RUBY:BOOL=OFF
   -DWRAP_R:BOOL=OFF
   -DWRAP_PYTHON:BOOL=${SV_USE_PYTHON}
   -DSimpleITK_PYTHON_THREADS:BOOL=ON
   -DSimpleITK_BUILD_DISTRIBUTE:BOOL=ON
   -DPYTHON_EXECUTABLE:FILEPATH=${PYTHON_EXECUTABLE}
   -DPYTHON_INCLUDE_DIRS:PATH=${PYTHON_INCLUDE_PATH}
   -DPYTHON_LIBRARY:FILEPATH=${PYTHON_LIBRARY}
   -DUSE_SYSTEM_ITK:BOOL=ON
   -DITK_DIR:PATH=${ITK_DIR}
   -DGDCM_DIR:PATH=${GDCM_DIR}
   -DCMAKE_INSTALL_PREFIX:STRING=${${proj}_BIN_DIR}
   DEPENDS ${${proj}_DEPENDENCIES}
   #DEPENDS ${proj}-download ${${proj}_DEPENDENCIES}
   )
set(${proj}_SOURCE_DIR ${${proj}_SRC_DIR})
set(SV_${proj}_DIR ${${proj}_BIN_DIR})
set(${proj}_DIR ${${proj}_BLD_DIR})

mark_as_superbuild(${proj}_DIR})

else()
  # Sanity checks
  if((DEFINED SV_SimpleITK_DIR AND NOT EXISTS ${SV_SimpleITK_DIR})
     AND (DEFINED SimpleITK_DIR AND NOT EXISTS ${SimpleITK_DIR}))
    message(FATAL_ERROR "SV_SimpleITK_DIR variable is defined but corresponds to non-existing directory")
  endif()
  ExternalProject_Add_Empty(${proj} DEPENDS ${${proj}_DEPENDENCIES})
endif()
if(SV_INSTALL_EXTERNALS)
  ExternalProject_Install_CMake(${proj})
endif()
mark_as_superbuild(${proj}_SOURCE_DIR:PATH)

mark_as_superbuild(
  VARS SV_${proj}_DIR:PATH
  LABELS "FIND_PACKAGE"
  )
