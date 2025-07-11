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
# GDCM
set(proj GDCM)

message(STATUS "[GDCM.cmake] ")
message(STATUS "[GDCM.cmake] -------------------------------------------------------------------------------------")
message(STATUS "[GDCM.cmake] +++++                            GDCM.cmake                                          ")
message(STATUS "[GDCM.cmake] -------------------------------------------------------------------------------------")
message(STATUS "[GDCM.cmake] proj: ${proj}")
message(STATUS "[GDCM.cmake] SV_GDCM_DIR: ${SV_GDCM_DIR}")

#if(NOT SV_EXTERNALS_DOWNLOAD_${proj})
#  # Find SWIG!
#  find_package(SWIG REQUIRED)
#endif()

# Dependencies
set(${proj}_DEPENDENCIES "")
if(${SV_EXTERNALS_ENABLE_PYTHON})
  set(${proj}_DEPENDENCIES
    ${${proj}_DEPENDENCIES} "PYTHON")
endif()
if(${SV_EXTERNALS_ENABLE_SWIG})
  set(${proj}_DEPENDENCIES
    ${${proj}_DEPENDENCIES} "SWIG")
endif()

# Git info
set(SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL "" CACHE STRING "Manual specification of ${proj}, can be web address or local path to tar file")
mark_as_advanced(SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL)
if(NOT SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL)
  set(SV_EXTERNALS_${proj}_SOURCE_URL "${SV_EXTERNALS_ORIGINALS_URL}/gdcm/gdcm-${SV_EXTERNALS_${proj}_VERSION}.tar.gz")
else()
  set(SV_EXTERNALS_${proj}_SOURCE_URL "${SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL}")
endif()

#If using PYTHON
if(SV_EXTERNALS_ENABLE_PYTHON)
  list(APPEND SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS
    -DPYTHON_DIR:PATH=${SV_EXTERNALS_PYTHON_DIR}
    -DPYTHON_DEBUG_LIBRARY:FILEPATH=""
    -DPYTHON_EXECUTABLE:FILEPATH=${SV_EXTERNALS_PYTHON_EXECUTABLE}
    -DPYTHON_INCLUDE:PATH=${SV_EXTERNALS_PYTHON_INCLUDE_DIR}
    -DPYTHON_INCLUDE_DIR:PATH=${SV_EXTERNALS_PYTHON_INCLUDE_DIR}
    -DPYTHON_INCLUDE_DIR2:PATH=${SV_EXTERNALS_PYTHON_INCLUDE_DIR}
    -DPYTHON_LIBRARY:FILEPATH=${SV_EXTERNALS_PYTHON_LIBRARY}
    -DPYTHON_LIBRARIES:FILEPATH=${SV_EXTERNALS_PYTHON_LIBRARY}
    -DPYTHON_LIBRARY_DEBUG:FILEPATH=""
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

#If certain mac os
if(APPLE AND SV_EXTERNALS_${proj}_VERSION VERSION_EQUAL "2.6.3")
  set(SV_EXTERNALS_${proj}_CUSTOM_PATCH ${SV_EXTERNALS_${proj}_CUSTOM_PATCH}
    COMMAND patch -N -p1 -i ${SV_EXTERNALS_SOURCE_DIR}/Patches/2018.03/patch-gdcm-2.6.3-macos.patch)
endif()

# Add external project

if (SV_GDCM_DIR STREQUAL "system")
#if(SV_EXTERNALS_DOWNLOAD_${proj})

  message(STATUS "[GDCM.cmake] Use system GDCM") 

  find_package(GDCM REQUIRED PATHS ${SV_GDCM_DIR} NO_DEFAULT_PATH)

  if(GDCM_FOUND)
    message(STATUS "[GDCM.cmake] GDCM found") 
  else()
    message(FATAL_ERROR "[GDCM.cmake] GDCM not found")
  endif()

  message(STATUS "[GDCM.cmake] GDCM_DIR: ${GDCM_DIR}") 
  message(STATUS "[GDCM.cmake] GDCM_INCLUDE_DIRS: ${GDCM_INCLUDE_DIRS}") 
  message(STATUS "[GDCM.cmake] GDCM_LIBRARY_DIRS: ${GDCM_LIBRARY_DIRS}") 

  #ExternalProject_Add(${proj}
    #URL ${SV_EXTERNALS_${proj}_BINARIES_URL}
    #PREFIX ${SV_EXTERNALS_${proj}_PFX_DIR}
    #SOURCE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}
    #BINARY_DIR ${SV_EXTERNALS_${proj}_BLD_DIR}
    #DEPENDS ${${proj}_DEPENDENCIES}
    #CONFIGURE_COMMAND ""
    #BUILD_COMMAND ""
    #INSTALL_COMMAND ""
    #UPDATE_COMMAND ""
    #)

else()

  #ExternalProject_Add(${proj}
    #URL ${SV_EXTERNALS_${proj}_SOURCE_URL}
    #PREFIX ${SV_EXTERNALS_${proj}_PFX_DIR}
    #SOURCE_DIR ${SV_EXTERNALS_${proj}_SRC_DIR}
    #BINARY_DIR ${SV_EXTERNALS_${proj}_BLD_DIR}
    #DEPENDS ${${proj}_DEPENDENCIES}
    #PATCH_COMMAND ${SV_EXTERNALS_${proj}_CUSTOM_PATCH}
    #UPDATE_COMMAND ""
    #CMAKE_CACHE_ARGS
      #-DCMAKE_CXX_COMPILER:STRING=${CMAKE_CXX_COMPILER}
      #-DCMAKE_C_COMPILER:STRING=${CMAKE_C_COMPILER}
      #-DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS}
      #-DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
      #-DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
      #-DCMAKE_MACOSX_RPATH:BOOL=ON
      #-DGDCM_BUILD_SHARED_LIBS:BOOL=${SV_EXTERNALS_ENABLE_${proj}_SHARED}
      #-DGDCM_USE_VTK:BOOL=OFF
      #-DGDCM_WRAP_PYTHON:BOOL=${SV_EXTERNALS_ENABLE_PYTHON}
      #-DGDCM_BUILD_APPLICATIONS:BOOL=ON
      #-DCMAKE_INSTALL_PREFIX:STRING=${SV_EXTERNALS_${proj}_BIN_DIR}
      #${SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS}
    #)

endif()
#TODO Add install rpath

# GDCM variables needed later on
set(SV_EXTERNALS_${proj}_CMAKE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}/lib/gdcm-${SV_EXTERNALS_${proj}_MAJOR_VERSION}.${SV_EXTERNALS_${proj}_MINOR_VERSION})
set(SV_EXTERNALS_${proj}_INCLUDE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}/include)
