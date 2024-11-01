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
# ITK
set(proj ITK)

message(STATUS "[ITK.cmake] ")
message(STATUS "[ITK.cmake] -------------------------------------------------------------------------------------")
message(STATUS "[ITK.cmake] +++++                             ITK.cmake                                          ")
message(STATUS "[ITK.cmake] -------------------------------------------------------------------------------------")
message(STATUS "[ITK.cmake] proj: ${proj}")
message(STATUS "[ITK.cmake] SV_ITK_DIR: ${SV_ITK_DIR}")

# Dependencies
#
if(${SV_EXTERNALS_ENABLE_VTK})
  set(${proj}_DEPENDENCIES
    ${${proj}_DEPENDENCIES} "VTK")
endif()

if(${SV_EXTERNALS_ENABLE_GDCM})
  set(${proj}_DEPENDENCIES
    ${${proj}_DEPENDENCIES} "GDCM")
endif()

if(${SV_EXTERNALS_ENABLE_HDF5})
  set(${proj}_DEPENDENCIES
    ${${proj}_DEPENDENCIES} "HDF5")
endif()

# Git info
set(SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL "" CACHE STRING "Manual specification of ${proj}, can be web address or local path to tar file")
mark_as_advanced(SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL)
if(NOT SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL)
  set(SV_EXTERNALS_${proj}_SOURCE_URL "${SV_EXTERNALS_ORIGINALS_URL}/itk/InsightToolkit-${SV_EXTERNALS_${proj}_VERSION}.tar.gz")
else()
  set(SV_EXTERNALS_${proj}_SOURCE_URL "${SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL}")
endif()

# If using QT
#
if(SV_EXTERNALS_ENABLE_QT)
  #MINGW specific flags
  if(MINGW)
    list(APPEND SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS
      -DCMAKE_USE_WIN32_THREADS:BOOL=ON
      -DCMAKE_USE_PTHREADS:BOOL=OFF
      )
  endif()
  list(APPEND SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS
    -DUSE_WRAP_ITK:BOOL=OFF
    -DModule_ITKOpenJPEG:BOOL=ON
    )
endif()

# if using VTK
#
if(SV_EXTERNALS_ENABLE_VTK)
  list(APPEND SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS
    -DModule_ITKVtkGlue:BOOL=ON
    -DVTK_DIR:PATH=${SV_EXTERNALS_VTK_CMAKE_DIR}
    )
endif()

# If using GDCM
if(SV_EXTERNALS_ENABLE_GDCM)
  list(APPEND SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS
    -DITK_USE_SYTEM_GDCM:BOOL=ON
    -DGDCM_DIR:PATH=${SV_EXTERNALS_GDCM_CMAKE_DIR}
    )
endif()

# if using HDF5
if(SV_EXTERNALS_ENABLE_HDF5)
  list(APPEND SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS
    -DITK_USE_SYSTEM_HDF5:BOOL=ON
    -DHDF5_DIR:PATH=${SV_EXTERNALS_HDF5_CMAKE_DIR}
    )
endif()

# Patch for vclcompiler if ITK less than 4.7.2 and gcc version > 5
#
if("${SV_EXTERNALS_${proj}_VERSION}" VERSION_LESS "4.7.2" AND
    "${COMPILER_VERSION}" STREQUAL "GNU" AND
    "${CMAKE_CXX_COMPILER_VERSION}" VERSION_GREATER "5.0")
  set(SV_EXTERNALS_${proj}_CUSTOM_PATCH patch -N -p1 -i ${SV_EXTERNALS_SOURCE_DIR}/Patches/2018.01/patch-itk-4.7.1-gnu.patch)
elseif("${SV_EXTERNALS_${proj}_VERSION}" VERSION_LESS "4.7.2" AND
    "${COMPILER_VERSION}" STREQUAL "Clang" AND
    NOT ("${CMAKE_CXX_COMPILER_VERSION}" LESS "9.0"))
    set(SV_EXTERNALS_${proj}_CUSTOM_PATCH patch -N -p1 -i ${SV_EXTERNALS_SOURCE_DIR}/Patches/2018.01/patch-itk-4.7.1-clang-9.0.patch)
else()
  set(SV_EXTERNALS_${proj}_CUSTOM_PATCH "")
endif()

# Add external project

# Add ITK as an external project
#
if (SV_ITK_DIR STREQUAL "system")
#if(SV_EXTERNALS_DOWNLOAD_${proj})

  message(STATUS "[ITK.cmake] +++++ Use system ITK ")

  if(SV_ITK_DIR STREQUAL "system")
    message(STATUS "[ITK.cmake] Use system ITK")
    find_package(ITK REQUIRED)
  else()
    message(STATUS "[ITK.cmake] Use ITK from custom build ${SV_ITK_DIR}")
    find_package(ITK REQUIRED PATHS ${SV_ITK_DIR} NO_DEFAULT_PATH)
  endif()

  message(STATUS "[ITK.cmake] ITK_DIR: ${ITK_DIR}")
  message(STATUS "[ITK.cmake] ITK_USE_FILE: ${ITK_USE_FILE}")
  message(STATUS "[ITK.cmake] ITK_LIBRARIES: ${ITK_LIBRARIES}")

  find_package(EIGEN3 REQUIRED)

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
      #-DBUILD_SHARED_LIBS:BOOL=${SV_EXTERNALS_ENABLE_${proj}_SHARED}
      #-DBUILD_EXAMPLES:BOOL=OFF
      #-DBUILD_TESTING:BOOL=OFF
      #-DITK_USE_SYSTEM_GDCM:BOOL=${SV_EXTERNALS_ENABLE_GDCM}
      #-DITK_WRAP_PYTHON:BOOL=OFF
      #-DITK_LEGACY_SILENT:BOOL=OFF
      #-DModule_ITKReview:BOOL=ON
      #-DCMAKE_INSTALL_PREFIX:STRING=${SV_EXTERNALS_${proj}_BIN_DIR}
      #${SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS}
    #)

endif()

# ITK variables needed later on
set(SV_EXTERNALS_${proj}_CMAKE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}/lib/cmake/ITK-${SV_EXTERNALS_${proj}_MAJOR_VERSION}.${SV_EXTERNALS_${proj}_MINOR_VERSION})
set(SV_EXTERNALS_${proj}_INCLUDE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}/include)
