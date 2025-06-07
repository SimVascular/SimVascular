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

set(SV_PYTHON_DIR /Users/parkerda/software/ktbolt/svExternals/install/python)
    
set(msg "[Externals/CMake/PYTHON.cmake] ")
message(STATUS "${msg} ")
message(STATUS "${msg} -------------------------------------------------------------------------------------")
message(STATUS "${msg} +++++                               PYTHON.cmake                                      ")
message(STATUS "${msg} -------------------------------------------------------------------------------------")
message(STATUS "${msg} proj: ${proj}")
message(STATUS "${msg} SV_PYTHON_DIR: ${SV_PYTHON_DIR}")

# Dependencies
set(${proj}_DEPENDENCIES "")

# Source URL
#
set(SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL "" CACHE STRING "Manual specification of ${proj}, can be web address or local path to tar file")
mark_as_advanced(SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL)

if(NOT SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL)
  set(SV_EXTERNALS_${proj}_SOURCE_URL "${SV_EXTERNALS_ORIGINALS_URL}/python/python-${SV_EXTERNALS_${proj}_VERSION}-cmakebuild.tar.gz")
else()
  set(SV_EXTERNALS_${proj}_SOURCE_URL "${SV_EXTERNALS_${proj}_MANUAL_SOURCE_URL}")
endif()

set(SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS )

if(WIN32)
  #list(APPEND SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS
  #  -DCMAKE_ASM_COMPILER:FILEPATH="C:/Program Files (x86)/Microsoft Visual Studio 12.0/VC/bin/amd64/cl.exe"
  #  -DCMAKE_ASM_MASM_COMPILER:FILEPATH="C:/Program Files (x86)/Microsoft Visual Studio 12.0/VC/bin/amd64/ml64.exe"
  #  -DCMAKE_ASM_MASM_COMPILE_OBJECT="<CMAKE_ASM_MASM_COMPILER> <FLAGS> /c /Fo <OBJECT> <SOURCE>"
  #  )
endif()

# PYTHON variables needed later on
#
if(SV_EXTERNALS_ENABLE_${proj}_SHARED)
  if(SV_EXTERNALS_${proj}_MAJOR_VERSION VERSION_EQUAL "3" AND SV_EXTERNALS_${proj}_MINOR_VERSION VERSION_EQUAL "5")
    set(${proj}_LIBRARY_NAME libpython${SV_EXTERNALS_${proj}_MAJOR_VERSION}.${SV_EXTERNALS_${proj}_MINOR_VERSION}m${CMAKE_SHARED_LIBRARY_SUFFIX})
  else()
    set(${proj}_LIBRARY_NAME libpython${SV_EXTERNALS_${proj}_MAJOR_VERSION}.${SV_EXTERNALS_${proj}_MINOR_VERSION}${CMAKE_SHARED_LIBRARY_SUFFIX})
  endif()

else()
  if(SV_EXTERNALS_${proj}_MAJOR_VERSION VERSION_EQUAL "3" AND SV_EXTERNALS_${proj}_MINOR_VERSION VERSION_EQUAL "5")
    set(${proj}_LIBRARY_NAME libpython${SV_EXTERNALS_${proj}_MAJOR_VERSION}.${SV_EXTERNALS_${proj}_MINOR_VERSION}m${CMAKE_STATIC_LIBRARY_SUFFIX})
  else()
    set(${proj}_LIBRARY_NAME libpython${SV_EXTERNALS_${proj}_MAJOR_VERSION}.${SV_EXTERNALS_${proj}_MINOR_VERSION}${CMAKE_STATIC_LIBRARY_SUFFIX})
  endif()

endif()

set(SV_EXTERNALS_${proj}_EXECUTABLE ${SV_EXTERNALS_${proj}_BIN_DIR}/bin/python${SV_EXTERNALS_${proj}_MAJOR_VERSION}.${SV_EXTERNALS_${proj}_MINOR_VERSION})

if(SV_EXTERNALS_${proj}_MAJOR_VERSION VERSION_EQUAL "3" AND SV_EXTERNALS_${proj}_MINOR_VERSION VERSION_EQUAL "5")
  set(SV_EXTERNALS_${proj}_INCLUDE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}/include/python${SV_EXTERNALS_${proj}_MAJOR_VERSION}.${SV_EXTERNALS_${proj}_MINOR_VERSION}m)

else()
  set(SV_EXTERNALS_${proj}_INCLUDE_DIR ${SV_EXTERNALS_${proj}_BIN_DIR}/include/python${SV_EXTERNALS_${proj}_MAJOR_VERSION}.${SV_EXTERNALS_${proj}_MINOR_VERSION})
endif()

set(SV_EXTERNALS_${proj}_LIBRARY ${SV_EXTERNALS_${proj}_BIN_DIR}/lib/${${proj}_LIBRARY_NAME})
get_filename_component(SV_EXTERNALS_${proj}_LIBRARY_DIR ${SV_EXTERNALS_${proj}_LIBRARY} DIRECTORY)
set(SV_EXTERNALS_${proj}_SITE_DIR ${SV_EXTERNALS_${proj}_LIBRARY_DIR}/python${SV_EXTERNALS_${proj}_MAJOR_VERSION}.${SV_EXTERNALS_${proj}_MINOR_VERSION}/site-packages)

# turn off patching if 3.5
if(SV_EXTERNALS_${proj}_VERSION VERSION_EQUAL "3.5.2")
  list(APPEND SV_EXTERNALS_${proj}_CUSTOM_PATCH ${SV_EXTERNALS_${proj}_CUSTOM_PATCH}
    COMMAND patch -N -p1 -i ${SV_EXTERNALS_SOURCE_DIR}/Patches/2018.03/patch-python-3.5.2.patch)
endif()

if(LINUX)
endif()

if(APPLE)

  if (SV_EXTERNALS_MAC_PACKAGE_MANAGER STREQUAL HOMEBREW)

    list(APPEND SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS
      -DCMAKE_PREFIX_PATH:PATH=/usr/local/opt
      -DOPENSSL_CRYPTO_LIBRARY:FILEPATH=/usr/local/opt/openssl/lib/libcrypto.dylib
      -DOPENSSL_INCLUDE_DIR:PATH=/usr/local/opt/openssl/include
      -DOPENSSL_SSL_LIBRARY:FILEPATH=/usr/local/opt/openssl/lib/libssl.dylib
      )

    set(OPENSSL_ROOT "/usr/local/opt/openssl")

  elseif(SV_EXTERNALS_MAC_PACKAGE_MANAGER STREQUAL MACPORTS)

    list(APPEND SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS
      -DCMAKE_PREFIX_PATH:PATH=/opt/local
      )

    set(OPENSSL_ROOT "/opt/local")

  endif()
  set(SV_EXTERNALS_${proj}_INSTALL_SCRIPT install-python-mac_osx.sh)
  set(SV_EXTERNALS_${proj}_CONFIG_SCRIPT ${SV_EXTERNALS_${proj}_BIN_DIR}/bin/python${SV_EXTERNALS_${proj}_MAJOR_VERSION}.${SV_EXTERNALS_${proj}_MINOR_VERSION}-config)
  set(EXENAME @EXENAME@)
  configure_file(${SV_EXTERNALS_CMAKE_DIR}/Install/${SV_EXTERNALS_${proj}_INSTALL_SCRIPT}.in "${SV_EXTERNALS_${proj}_BIN_DIR}/${SV_EXTERNALS_${proj}_INSTALL_SCRIPT}" @ONLY)
  set(SV_EXTERNALS_${proj}_CUSTOM_INSTALL make install
    COMMAND ${SV_EXTERNALS_${proj}_BIN_DIR}/${SV_EXTERNALS_${proj}_INSTALL_SCRIPT})
elseif(LINUX)
  list(APPEND SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS
    -DCMAKE_INSTALL_RPATH:PATH=${SV_EXTERNALS_${proj}_LIBRARY_DIR}
    )
  set(SV_EXTERNALS_${proj}_CUSTOM_INSTALL make install)
else()
  set(SV_EXTERNALS_${proj}_CUSTOM_INSTALL make install)
endif()

# Add external project

if(SV_PYTHON_DIR)
  message(STATUS "${msg} +++++ Use installed PYTHON") 

  #find_package(Python COMPONENTS Interpreter Development REQUIRED)

  find_package(Python3 COMPONENTS Interpreter Development REQUIRED)

  #find_package(Python REQUIRED PATHS ${SV_PYTHON_DIR} NO_DEFAULT_PATH)
  #find_package(Python REQUIRED)

  message(STATUS "${msg} PYTHON_EXECUTABLE: ${PYTHON_EXECUTABLE}")
  message(STATUS "${msg} PYTHON_INCLUDE_DIR: ${PYTHON_INCLUDE_DIR}")
  message(STATUS "${msg} PYTHON_LIBRARY: ${PYTHON_LIBRARY}")

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

  find_package(Python REQUIRED PATHS ${SV_PYTHON_DIR} NO_DEFAULT_PATH)

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
      #-DBUILD_LIBPYTHON_SHARED:BOOL=${SV_EXTERNALS_ENABLE_${proj}_SHARED}
      #-DENABLE_SSL:BOOL=ON
      #-DBUILTIN_SSL:BOOL=ON
      #-DBUILTIN_HASHLIB:BOOL=ON
      #-DENABLE_CTYPES:BOOL=ON
      #-DBUILTIN_CTYPES:BOOL=ON
      #-DPYTHON_VERSION:STRING=${SV_EXTERNALS_${proj}_VERSION}
      #-DCMAKE_INSTALL_PREFIX:STRING=${SV_EXTERNALS_${proj}_BIN_DIR}
      #${SV_EXTERNALS_${proj}_ADDITIONAL_CMAKE_ARGS}
    #)

endif()
