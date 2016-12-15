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
set(proj PYTHON)

# Dependencies
set(${proj}_DEPENDENCIES "")

# Source URL
set(SV_EXTERNALS_${proj}_SOURCE_URL "${SV_EXTERNALS_STANFORD_URL}/python/python-2.7.11-cmakebuild.tar.gz" CACHE STRING "Location of ${proj}, can be web address or local path")
mark_as_advanced(SV_EXTERNALS_${proj}_SOURCE_URL)

set(SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS )
if(WIN32)
  #list(APPEND SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS
  #  -DCMAKE_ASM_COMPILER:FILEPATH="C:/Program Files (x86)/Microsoft Visual Studio 12.0/VC/bin/amd64/cl.exe"
  #  -DCMAKE_ASM_MASM_COMPILER:FILEPATH="C:/Program Files (x86)/Microsoft Visual Studio 12.0/VC/bin/amd64/ml64.exe"
  #  -DCMAKE_ASM_MASM_COMPILE_OBJECT="<CMAKE_ASM_MASM_COMPILER> <FLAGS> /c /Fo <OBJECT> <SOURCE>"
  #  )
endif()

# PYTHON variables needed later on
if(SV_EXTERNALS_BUILD_${proj}_SHARED)
  set(${proj}_LIBRARY_NAME libpython${SV_EXTERNALS_${proj}_MAJOR_VERSION}.${SV_EXTERNALS_${proj}_MINOR_VERSION}${CMAKE_SHARED_LIBRARY_SUFFIX})
else()
  set(${proj}_LIBRARY_NAME libpython${SV_EXTERNALS_${proj}_MAJOR_VERSION}.${SV_EXTERNALS_${proj}_MINOR_VERSION}${CMAKE_STATIC_LIBRARY_SUFFIX})
endif()

set(SV_EXTERNALS_${proj}_EXECUTABLE ${SV_EXTERNALS_${proj}_BIN_DIR}/bin/python${SV_EXTERNALS_${proj}_MAJOR_VERSION}.${SV_EXTERNALS_${proj}_MINOR_VERSION})
set(SV_EXTERNALS_${proj}_INCLUDE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}/include/python${SV_EXTERNALS_${proj}_MAJOR_VERSION}.${SV_EXTERNALS_${proj}_MINOR_VERSION})
set(SV_EXTERNALS_${proj}_LIBRARY ${SV_EXTERNALS_${proj}_BIN_DIR}/lib/${${proj}_LIBRARY_NAME})
get_filename_component(SV_EXTERNALS_${proj}_LIBRARY_DIR ${SV_EXTERNALS_${proj}_LIBRARY} DIRECTORY)
set(SV_EXTERNALS_${proj}_SITE_DIR ${SV_EXTERNALS_${proj}_LIBRARY_DIR}/python${SV_EXTERNALS_${proj}_MAJOR_VERSION}.${SV_EXTERNALS_${proj}_MINOR_VERSION}/site-packages)

if(APPLE)
  list(APPEND SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS
    -DCMAKE_PREFIX_PATH:PATH=/opt/local
    )
  set(SV_EXTERNALS_${proj}_CUSTOM_INSTALL make install
    COMMAND install_name_tool -id @rpath/${${proj}_LIBRARY_NAME} ${SV_EXTERNALS_${proj}_LIBRARY}
    COMMAND cp /opt/local/lib/libcrypto.dylib ${SV_EXTERNALS_${proj}_LIBRARY_DIR}
    COMMAND cp /opt/local/lib/libssl.dylib ${SV_EXTERNALS_${proj}_LIBRARY_DIR}
    COMMAND cp /opt/local/lib/libz.dylib ${SV_EXTERNALS_${proj}_LIBRARY_DIR})
else()
  set(SV_EXTERNALS_${proj}_CUSTOM_INSTALL make install)
endif()

# Add external project
ExternalProject_Add(${proj}
  URL ${SV_EXTERNALS_${proj}_SOURCE_URL}
  PREFIX ${SV_EXTERNALS_${proj}_PFX_DIR}
  SOURCE_DIR ${SV_EXTERNALS_${proj}_SRC_DIR}
  BINARY_DIR ${SV_EXTERNALS_${proj}_BLD_DIR}
  DEPENDS ${${proj}_DEPENDENCIES}
  INSTALL_COMMAND ${SV_EXTERNALS_${proj}_CUSTOM_INSTALL}
  UPDATE_COMMAND ""
  CMAKE_CACHE_ARGS
    -DCMAKE_CXX_COMPILER:STRING=${CMAKE_CXX_COMPILER}
    -DCMAKE_C_COMPILER:STRING=${CMAKE_C_COMPILER}
    -DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS}
    -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
    -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
    -DCMAKE_MACOSX_RPATH:BOOL=ON
    -DBUILD_TESTING:BOOL=OFF
    -DBUILD_LIBPYTHON_SHARED:BOOL=${SV_EXTERNALS_BUILD_${proj}_SHARED}
    -DENABLE_SSL:BOOL=ON
    -DBUILTIN_SSL:BOOL=ON
    -DBUILTIN_HASHLIB:BOOL=ON
    -DENABLE_CTYPES:BOOL=ON
    -DBUILTIN_CTYPES:BOOL=ON
    -DCMAKE_INSTALL_PREFIX:STRING=${SV_EXTERNALS_${proj}_BIN_DIR}
    ${SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS}
  )
#TODO Add install rpath

