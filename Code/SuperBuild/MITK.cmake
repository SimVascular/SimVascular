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
set(proj MITK)

set(${proj}_DEPENDENCIES VTK)
if(SV_USE_GDCM)
  set(${proj}_DEPENDENCIES ${${proj}_DEPENDENCIES} GDCM)
endif()
if(SV_USE_ITK)
  set(${proj}_DEPENDENCIES ${${proj}_DEPENDENCIES} ITK)
endif()
if(SV_USE_CUSTOM_CTK)
  set (${proj}_DEPENDENCIES ${${proj}_DEPENDENCIES} CTK)
endif()
if(SV_USE_CUSTOM_SimpleITK)
  set (${proj}_DEPENDENCIES ${${proj}_DEPENDENCIES} SimpleITK)
endif()
if(SV_USE_PYTHON)
  set(${proj}_DEPENDENCIES ${${proj}_DEPENDENCIES} PYTHON)
endif()

ExternalProject_Include_Dependencies(${proj}
  PROJECT_VAR proj
  DEPENDS_VAR ${proj}_DEPENDENCIES
  EP_ARGS_VAR ${proj}_EXTERNAL_PROJECT_ARGS
  USE_SYSTEM_VAR ${CMAKE_PROJECT_NAME}_USE_SYSTEM_${proj}
  )

if(NOT ${CMAKE_PROJECT_NAME}_USE_SYSTEM_${proj})

  if(WIN32)
    set(${proj}_PFX_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_PFX_DIR} 
      CACHE PATH "On windows, there is a bug with MITK source code directory path length, you can change this path to avoid it")
    set(${proj}_SRC_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_SRC_DIR} 
      CACHE PATH "On windows, there is a bug with MITK source code directory path length, you can change this path to avoid it")
    set(${proj}_BLD_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_BLD_DIR}  
      CACHE PATH "On windows, there is a bug with MITK source code directory path length, you can change this path to avoid it")
    set(${proj}_BIN_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_BIN_DIR}  
      CACHE PATH "On windows, there is a bug with MITK source code directory path length, you can change this path to avoid it")
  else()
    set(${proj}_PFX_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_PFX_DIR})
    set(${proj}_SRC_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_SRC_DIR})
    set(${proj}_BLD_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_BLD_DIR})
    set(${proj}_BIN_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_BIN_DIR})
  endif()

  set(revision_tag "simvascular-patch-2016.03.0")

  set(additional_cmake_args )
  if(SV_USE_CUSTOM_CTK)
    list(APPEND additional_cmake_args
         -DEXTERNAL_CTK_DIR_SimpleITK_DIR:PATH=${CTK_DIR})
  endif()
  if(SV_USE_CUSTOM_SimpleITK)
    list(APPEND additional_cmake_args
         -DEXTERNAL_SimpleITK_DIR_SimpleITK_DIR:PATH=${SimpleITK_DIR})
  endif()

  ExternalProject_Add(${proj}
   GIT_REPOSITORY "https://github.com/SimVascular/MITK.git"
   GIT_TAG ${revision_tag}
   PREFIX ${${proj}_PFX_DIR}
   SOURCE_DIR ${${proj}_SRC_DIR}
   BINARY_DIR ${${proj}_BLD_DIR}
   UPDATE_COMMAND ""
   INSTALL_COMMAND ""
   CMAKE_CACHE_ARGS
   -DCMAKE_CXX_COMPILER:STRING=${CMAKE_CXX_COMPILER}
   -DCMAKE_C_COMPILER:STRING=${CMAKE_C_COMPILER}
   -DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS}
   -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
   -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
   -DCMAKE_THREAD_LIBS:STRING=-lpthread
   -DMITK_BUILD_EXAMPLES:BOOL=OFF
   -DBUILD_SHARED_LIBS:BOOL=${SV_USE_${proj}_SHARED}
   -DBUILD_TESTING:BOOL=OFF
   -DDESIRED_QT_VERSION:STRING=5
   -DCMAKE_PREFIX_PATH:PATH=${CMAKE_PREFIX_PATH}
   -DMITK_USE_SUPERBUILD:BOOL=ON
   -DMITK_USE_GDCM:BOOL=ON
   -DMITK_USE_SWIG:BOOL=ON
   -DMITK_USE_Python:BOOL=${SV_USE_PYTHON}
   -DMITK_USE_Numpy:BOOL=${SV_USE_PYTHON}
   -DMITK_USE_VMTK:BOOL=OFF
   -DMITK_USE_SYSTEM_PYTHON:BOOL=${SV_USE_PYTHON}
   -DMITK_PYTHON_SITE_DIR:PATH=${PYTHON_LIBRARY_PATH}/python2.7/site-packages
   -DEXTERNAL_GDCM_DIR:PATH=${GDCM_DIR}
   -DEXTERNAL_VTK_DIR:PATH=${VTK_DIR}
   -DEXTERNAL_Numpy_DIR:PATH=${PYTHON_LIBRARY_PATH}/python2.7/site-packages/numpy
   -DEXTERNAL_ITK_DIR:PATH=${ITK_DIR}
   -DPYTHON_EXECUTABLE:FILEPATH=${PYTHON_EXECUTABLE}
   -DPYTHON_INCLUDE_DIRS:PATH=${PYTHON_INCLUDE_PATH}
   -DPYTHON_LIBRARIES:FILEPATH=${PYTHON_LIBRARIES}
   -DMITK_INITIAL_CACHE_FILE:FILEPATH=${CMAKE_SOURCE_DIR}/SuperBuild/MITK_Init.txt
   -DCMAKE_INSTALL_PREFIX:STRING=${${proj}_BIN_DIR}
   ${additional_cmake_args}
   DEPENDS ${${proj}_DEPENDENCIES}
   )
set(${proj}_SOURCE_DIR ${${proj}_SRC_DIR})
set(SV_${proj}_DIR ${${proj}_BIN_DIR})
set(${proj}_DIR ${${proj}_BLD_DIR}/${proj}-build)

mark_as_superbuild(${proj}_DIR})

else()
  # Sanity checks
  if((DEFINED SV_MITK_DIR AND NOT EXISTS ${SV_MITK_DIR})
     AND (DEFINED MITK_DIR AND NOT EXISTS ${MITK_DIR}))
    message(FATAL_ERROR "SV_MITK_DIR variable is defined but corresponds to non-existing directory")
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
