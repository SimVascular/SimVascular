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
# PYTHON
set(proj HDF5)

message(STATUS "[HDF5.cmake] ")
message(STATUS "[HDF5.cmake] -------------------------------------------------------------------------------------")
message(STATUS "[HDF5.cmake] +++++ HDF5.cmake +++++")
message(STATUS "[HDF5.cmake] -------------------------------------------------------------------------------------")
message(STATUS "[HDF5.cmake] proj: ${proj}")
message(STATUS "[HDF5.cmake] SV_HDF5_DIR: ${SV_HDF5_DIR}")

# Dependencies
set(${proj}_DEPENDENCIES "")

# Source URL
set(SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL "" CACHE STRING "Manual specification of ${proj}, can be web address or local path to tar file")
mark_as_advanced(SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL)
if(NOT SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL)
  set(SV_EXTERNALS_${proj}_SOURCE_URL "${SV_EXTERNALS_ORIGINALS_URL}/hdf5/hdf5-${SV_EXTERNALS_${proj}_VERSION}.tar.gz")
else()
  set(SV_EXTERNALS_${proj}_SOURCE_URL "${SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL}")
endif()

set(SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS )

# Add external project

if (SV_HDF5_DIR STREQUAL "system")
#if(SV_EXTERNALS_DOWNLOAD_${proj})

  message(STATUS "[HDF5.cmake] +++++ Use system HDF5")

  find_package(HDF5 REQUIRED)

  message(STATUS "[HDF5.cmake] HDF5_DIR: ${HDF5_DIR}")
  message(STATUS "[HDF5.cmake] HDF5_INCLUDE_DIRS: ${HDF5_INCLUDE_DIRS}")

  ExternalProject_Add(${proj}
    PREFIX ${SV_EXTERNALS_${proj}_PFX_DIR}-empty
    SOURCE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}-empty
    BINARY_DIR ${SV_EXTERNALS_${proj}_BLD_DIR}-empty
    DEPENDS ${${proj}_DEPENDENCIES}
    DOWNLOAD_COMMAND ""
    CONFIGURE_COMMAND ""
    BUILD_COMMAND ""
    INSTALL_COMMAND ""
    )

  #ExternalProject_Add(${proj}
  #  URL ${SV_EXTERNALS_${proj}_BINARIES_URL}
  #  PREFIX ${SV_EXTERNALS_${proj}_PFX_DIR}
  #  SOURCE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}
  #  BINARY_DIR ${SV_EXTERNALS_${proj}_BLD_DIR}
  #  DEPENDS ${${proj}_DEPENDENCIES}
  #  CONFIGURE_COMMAND ""
  #  BUILD_COMMAND ""
  #  INSTALL_COMMAND ""
  #  UPDATE_COMMAND ""
  #  )

else()

  message(STATUS "[HDF5.cmake] +++++ Use HDF5 from ${SV_HDF5_DIR}")

  find_package(HDF5 REQUIRED PATHS ${SV_HDF5_DIR} NO_DEFAULT_PATH)

  message(STATUS "[HDF5.cmake] HDF5_DIR: ${HDF5_DIR}")
  message(STATUS "[HDF5.cmake] HDF5_INCLUDE_DIRS: ${HDF5_INCLUDE_DIRS}")

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
      #-DBUILD_TESTING:BOOL=OFF
      #-DHDF5_BUILD_SHARED_LIBS:BOOL=${SV_EXTERNALS_ENABLE_${proj}_SHARED}
      #-DHDF5_BUILD_HI_LIB:BOOL=ON
      #-DHDF5_BUILD_CPP_LIB:BOOL=ON
      #-DCMAKE_INSTALL_PREFIX:STRING=${SV_EXTERNALS_${proj}_BIN_DIR}
      #${SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS}
    #)

endif()

# HDF5 variables used later on
if(WIN32)
  set(SV_EXTERNALS_${proj}_CMAKE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}/cmake)
else()
  set(SV_EXTERNALS_${proj}_CMAKE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}/share/cmake)
endif()

message(STATUS "[HDF5.cmake] ")
message(STATUS "[HDF5.cmake] ----- Done HDF5.cmake -----")
message(STATUS "[HDF5.cmake] ")
