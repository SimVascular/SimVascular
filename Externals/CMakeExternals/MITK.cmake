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
#-----------------------------------------------------------------------------
# MITK
set(proj MITK)

# Dependencies
set(${proj}_DEPENDENCIES "VTK" "Qt")
#set(${proj}_DEPENDENCIES "VTK")

if(SV_EXTERNALS_ENABLE_PYTHON)
  set(${proj}_DEPENDENCIES
    ${${proj}_DEPENDENCIES} "PYTHON")
endif()
if(SV_EXTERNALS_ENABLE_NUMPY)
  set(${proj}_DEPENDENCIES
    ${${proj}_DEPENDENCIES} "NUMPY")
endif()
if(SV_EXTERNALS_ENABLE_PIP)
  set(${proj}_DEPENDENCIES
    ${${proj}_DEPENDENCIES} "PIP")
endif()
if(SV_EXTERNALS_ENABLE_GDCM)
  set(${proj}_DEPENDENCIES
    ${${proj}_DEPENDENCIES} "GDCM")
endif()
if(SV_EXTERNALS_ENABLE_ITK)
  set(${proj}_DEPENDENCIES
    ${${proj}_DEPENDENCIES} "ITK")
endif()
if(SV_EXTERNALS_ENABLE_SWIG)
  set(${proj}_DEPENDENCIES
    ${${proj}_DEPENDENCIES} "SWIG")
endif()

# Git info
#set(SV_EXTERNALS_${proj}_GIT_URL "${SV_EXTERNALS_GIT_URL}/MITK.git" CACHE STRING "Location of ${proj}, can be web address or local path")
#mark_as_advanced(SV_EXTERNALS_${proj}_GIT_URL)
#set(SV_EXTERNALS_${proj}_GIT_TAG "simvascular-patch-2016.03.0" CACHE STRING "Tag for ${proj}")
#mark_as_advanced(SV_EXTERNALS_${proj}_GIT_TAG)
set(SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL "" CACHE STRING "Manual specification of ${proj}, can be web address or local path to tar file")
mark_as_advanced(SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL)
if(NOT SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL)
  set(SV_EXTERNALS_${proj}_SOURCE_URL "${SV_EXTERNALS_ORIGINALS_URL}/mitk/mitk-v${SV_EXTERNALS_${proj}_VERSION}.0.tar.gz")
else()
  set(SV_EXTERNALS_${proj}_SOURCE_URL "${SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL}")
endif()

set(SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS )
#Special for Qt, make sure that MITK uses the same libs we are!
foreach(comp ${SV_EXTERNALS_Qt5_COMPONENTS})
  #if(Qt5${comp}_LIBRARIES)
    list(APPEND SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS
      -DQt5${comp}_DIR:PATH=${SV_EXTERNALS_Qt_TOPLEVEL_CMAKE_DIR}/Qt5${comp}
      )
      #-DQt5${comp}_DIR:PATH=${Qt5${comp}_DIR}
  #endif()
endforeach()

#If using PYTHON
if(SV_EXTERNALS_ENABLE_PYTHON)
  #Need to make directory for site-packages if we are using our own python
  #otherwise simpleitk install crashes
  file(MAKE_DIRECTORY "${SV_EXTERNALS_${proj}_BLD_DIR}/ep/lib/python${SV_EXTERNALS_PYTHON_MAJOR_VERSION}.${SV_EXTERNALS_PYTHON_MINOR_VERSION}/site-packages")
  list(APPEND SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS
   -DPYTHON_EXECUTABLE:FILEPATH=${SV_EXTERNALS_PYTHON_EXECUTABLE}
   -DPYTHON_INCLUDE_DIRS:PATH=${SV_EXTERNALS_PYTHON_INCLUDE_DIR}
   -DPYTHON_LIBRARIES:FILEPATH=${SV_EXTERNALS_PYTHON_LIBRARY}
   -DMITK_PYTHON_SITE_DIR:PATH=${SV_EXTERNALS_PYTHON_SITE_DIR}
    )
endif()

#If using NUMPY
if(SV_EXTERNALS_ENABLE_NUMPY)
  list(APPEND SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS
    -DEXTERNAL_Numpy_DIR:PATH=${SV_EXTERNALS_NUMPY_DIR}
    )
endif()

#If using GDCM
if(SV_EXTERNALS_ENABLE_GDCM)
  list(APPEND SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS
    -DEXTERNAL_GDCM_DIR:PATH=${SV_EXTERNALS_GDCM_CMAKE_DIR}
    )
endif()

#If using ITK
if(SV_EXTERNALS_ENABLE_ITK)
  list(APPEND SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS
    -DEXTERNAL_ITK_DIR:PATH=${SV_EXTERNALS_ITK_CMAKE_DIR}
    )
endif()

#If using SWIG
if(SV_EXTERNALS_ENABLE_SWIG)
  list(APPEND SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS
    -DSWIG_EXECUTABLE:FILEPATH=${SV_EXTERNALS_SWIG_EXECUTABLE}
    -DSWIG_DIR:PATH=${SV_EXTERNALS_SWIG_BIN_DIR}
    -DSWIG_VERSION:STRING=${SV_EXTERNALS_SWIG_VERSION}
    )
endif()

# Configure file for custom install!!!
if(APPLE)
  set(SV_EXTERNALS_${proj}_INSTALL_SCRIPT install-mitk-mac_osx.sh)
  set(SV_${proj}_CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
elseif(LINUX)
  set(SV_EXTERNALS_${proj}_INSTALL_SCRIPT install-mitk-linux.sh)
  set(SV_${proj}_CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fpermissive -Wno-deprecated-declarations")
else()
  set(SV_EXTERNALS_${proj}_INSTALL_SCRIPT install-mitk-windows.sh)
  set(SV_${proj}_CMAKE_CXX_FLAGS ${CMAKE_CXX_FLAGS})
endif()
configure_file(${SV_EXTERNALS_CMAKE_DIR}/Install/${SV_EXTERNALS_${proj}_INSTALL_SCRIPT}.in "${SV_EXTERNALS_${proj}_BIN_DIR}/${SV_EXTERNALS_${proj}_INSTALL_SCRIPT}" @ONLY)

#Patch for cppmicroservices
set(SV_EXTERNALS_${proj}_CUSTOM_PATCH patch -N -p1 -i ${SV_EXTERNALS_CMAKE_DIR}/PATCH/patch-mitk-2016.03.patch)
if("${COMPILER_VERSION}" STREQUAL "Clang" AND
  NOT ("${CMAKE_CXX_COMPILER_VERSION}" LESS "9.0"))
  set(SV_EXTERNALS_${proj}_CUSTOM_PATCH ${SV_EXTERNALS_${proj}_CUSTOM_PATCH}
    COMMAND patch -N -p1 -i ${SV_EXTERNALS_CMAKE_DIR}/Patch/patch-mitk-2016.03-clang-9.0.patch)
endif()

# Add external project
if(SV_EXTERNALS_DOWNLOAD_${proj})
  ExternalProject_Add(${proj}
    URL ${SV_EXTERNALS_${proj}_BINARIES_URL}
    PREFIX ${SV_EXTERNALS_${proj}_PFX_DIR}
    SOURCE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}
    BINARY_DIR ${SV_EXTERNALS_${proj}_BLD_DIR}
    DEPENDS ${${proj}_DEPENDENCIES}
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND ""
    UPDATE_COMMAND ""
    )
else()
  ExternalProject_Add(${proj}
    URL ${SV_EXTERNALS_${proj}_SOURCE_URL}
    PREFIX ${SV_EXTERNALS_${proj}_PFX_DIR}
    SOURCE_DIR ${SV_EXTERNALS_${proj}_SRC_DIR}
    BINARY_DIR ${SV_EXTERNALS_${proj}_BLD_DIR}
    DEPENDS ${${proj}_DEPENDENCIES}
    PATCH_COMMAND ${SV_EXTERNALS_${proj}_CUSTOM_PATCH}
    UPDATE_COMMAND ""
    INSTALL_COMMAND ${SV_EXTERNALS_${proj}_BIN_DIR}/${SV_EXTERNALS_${proj}_INSTALL_SCRIPT}
     CMAKE_CACHE_ARGS
     -DCMAKE_CXX_COMPILER:STRING=${CMAKE_CXX_COMPILER}
      -DCMAKE_C_COMPILER:STRING=${CMAKE_C_COMPILER}
      -DCMAKE_CXX_FLAGS:STRING=${SV_${proj}_CMAKE_CXX_FLAGS}
      -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
      -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
      -DCMAKE_MACOSX_RPATH:BOOL=ON
      -DMITK_BUILD_EXAMPLES:BOOL=OFF
      -DBUILD_SHARED_LIBS:BOOL=${SV_EXTERNALS_ENABLE_${proj}_SHARED}
      -DBUILD_TESTING:BOOL=OFF
      -DDESIRED_QT_VERSION:STRING=5
      -DCMAKE_PREFIX_PATH:PATH=${CMAKE_PREFIX_PATH}
      -DMITK_BUILD_ALL_PLUGINS:BOOL=ON
      -DMITK_USE_SUPERBUILD:BOOL=ON
      -DMITK_USE_GDCM:BOOL=${SV_EXTERNALS_ENABLE_GDCM}
      -DMITK_USE_SWIG:BOOL=${SV_EXTERNALS_ENABLE_SWIG}
      -DMITK_USE_Python:BOOL=${SV_EXTERNALS_ENABLE_PYTHON}
      -DMITK_USE_SYSTEM_PYTHON:BOOL=${SV_EXTERNALS_ENABLE_PYTHON}
      -DMITK_USE_Numpy:BOOL=${SV_EXTERNALS_ENABLE_NUMPY}
      -DMITK_USE_VMTK:BOOL=OFF
      -DEXTERNAL_VTK_DIR:PATH=${SV_EXTERNALS_VTK_CMAKE_DIR}
      -DQt5_DIR:PATH:STRING=${SV_EXTERNALS_Qt_CMAKE_DIR}
      -DQT_QMAKE_EXECUTABLE:FILEPATH=${SV_EXTERNALS_Qt_QMAKE_EXECUTABLE}
      -DCMAKE_INSTALL_PREFIX:STRING=${SV_EXTERNALS_${proj}_BIN_DIR}
      ${SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS}
      )
endif()
