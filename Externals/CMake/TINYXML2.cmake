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
set(proj TINYXML2)

message(STATUS "[TINYXML2.cmake] ")
message(STATUS "[TINYXML2.cmake] +++++ TINYXML2.cmake +++++")
message(STATUS "[TINYXML2.cmake] proj: ${proj}")
message(STATUS "[TINYXML2.cmake] SV_TINYXML2_DIR: ${SV_TINYXML2_DIR}")

# Dependencies
set(${proj}_DEPENDENCIES "")

# Source URL
#
set(SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL "" CACHE STRING "Manual specification of ${proj}, can be web address or local path to tar file")
mark_as_advanced(SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL)

if(NOT SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL)
  set(SV_EXTERNALS_${proj}_SOURCE_URL "${SV_EXTERNALS_ORIGINALS_URL}/tinyxml2/tinyxml2-${SV_EXTERNALS_${proj}_VERSION}.tar.gz")
else()
  set(SV_EXTERNALS_${proj}_SOURCE_URL "${SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL}")
endif()

set(SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS )

# Make custom install to install relative and move to correct location
set(SV_EXTERNALS_${proj}_CUSTOM_INSTALL make install
  COMMAND ${CMAKE_COMMAND} -E copy_directory share ${SV_EXTERNALS_${proj}_BIN_DIR}
  )

# Add external project.

if (SV_TINYXML2_DIR STREQUAL "system")
  message(STATUS "[TINYXML2.cmake] +++++ Use system TINYXML2")

  find_package(TINYXML2 REQUIRED)
  message(STATUS "[TINYXML2.cmake] TINYXML2_INCLUDE_DIRS: ${TINYXML2_INCLUDE_DIRS}")
  message(STATUS "[TINYXML2.cmake] TINYXML2_DIR: ${TINYXML2_DIR}")

  #ExternalProject_Add(${proj}
     #LIST_SEPARATOR ${sep}
     #GIT_REPOSITORY https://github.com/leethomason/tinyxml2.git
     #GIT_TAG 8.0.0
     #CMAKE_GENERATOR ${gen}
     #CMAKE_GENERATOR_PLATFORM ${gen_platform}
     #CMAKE_ARGS
       #${ep_common_args}
       #${additional_cmake_args}
     #CMAKE_CACHE_ARGS
       #${ep_common_cache_args}
       #-DBUILD_TESTING:BOOL=OFF
       #-DBUILD_TESTS:BOOL=OFF
     #CMAKE_CACHE_DEFAULT_ARGS
       #${ep_common_cache_default_args}
     #DEPENDS ${proj_DEPENDENCIES}
    #)

endif()

#if(SV_EXTERNALS_DOWNLOAD_${proj})
##  ExternalProject_Add(${proj}
#    URL ${SV_EXTERNALS_${proj}_BINARIES_URL}
#    PREFIX ${SV_EXTERNALS_${proj}_PFX_DIR}
#    SOURCE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}
#    BINARY_DIR ${SV_EXTERNALS_${proj}_BLD_DIR}
#    DEPENDS ${${proj}_DEPENDENCIES}
#    CONFIGURE_COMMAND ""
#    BUILD_COMMAND ""
#    INSTALL_COMMAND ""
#    UPDATE_COMMAND ""
#    )
#else()
  #ExternalProject_Add(${proj}
    #URL ${SV_EXTERNALS_${proj}_SOURCE_URL}
    #PREFIX ${SV_EXTERNALS_${proj}_PFX_DIR}
    #SOURCE_DIR ${SV_EXTERNALS_${proj}_SRC_DIR}
    #BINARY_DIR ${SV_EXTERNALS_${proj}_BLD_DIR}
    #DEPENDS ${${proj}_DEPENDENCIES}
    #PATCH_COMMAND ${SV_EXTERNALS_${proj}_CUSTOM_PATCH}
    #INSTALL_COMMAND ${SV_EXTERNALS_${proj}_CUSTOM_INSTALL}
    #UPDATE_COMMAND ""
    #CMAKE_CACHE_ARGS
      #-DCMAKE_CXX_COMPILER:STRING=${CMAKE_CXX_COMPILER}
      #-DCMAKE_C_COMPILER:STRING=${CMAKE_C_COMPILER}
      #-DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS}
      #-DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
      #-DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
      #-DCMAKE_MACOSX_RPATH:BOOL=ON
      #-DBUILD_TESTING:BOOL=OFF
      #-DCMAKE_INSTALL_PREFIX:STRING=share
      #${SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS}
    #)
#endif()
