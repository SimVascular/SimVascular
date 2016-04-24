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
set(proj GDCM)

set(${proj}_DEPENDENCIES VTK)

ExternalProject_Include_Dependencies(${proj}
  PROJECT_VAR proj
  DEPENDS_VAR ${proj}_DEPENDENCIES
  EP_ARGS_VAR ${proj}_EXTERNAL_PROJECT_ARGS
  USE_SYSTEM_VAR ${CMAKE_PROJECT_NAME}_USE_SYSTEM_${proj}
  )

# Sanity checks
if(DEFINED GDCM_DIR AND NOT EXISTS ${GDCM_DIR})
  message(FATAL_ERROR "GDCM_DIR variable is defined but corresponds to non-existing directory")
endif()

set(GDCM_BUILD_LIBRARY_TYPE "Static")
if(${GDCM_SHARED_LIBRARIES})
  set(GDCM_BUILD_LIBRARY_TYPE "Shared")
endif()

if(NOT ${CMAKE_PROJECT_NAME}_USE_SYSTEM_${proj})

	#set(revision_tag "6.9")
  set(location_args GIT_REPOSITORY "https://github.com/SimVascular/OpenCASCADE-7.0.git")
	  #GIT_TAG ${revision_tag})
  if(WIN32)
    set(${proj}_OUTPUT_DIR ${CMAKE_BINARY_DIR}/ThirdParty/${proj} 
      CACHE PATH "On windows, there is a bug with GDCM source code directory path length, you can change this path to avoid it")
    set(${proj}_OUTPUT_BIN_DIR ${CMAKE_BINARY_DIR}/ThirdParty/${proj}-build  
      CACHE PATH "On windows, there is a bug with GDCM source code directory path length, you can change this path to avoid it")
  else()
    set(${proj}_OUTPUT_DIR ${CMAKE_BINARY_DIR}/ThirdParty/${proj})
    set(${proj}_OUTPUT_BIN_DIR ${CMAKE_BINARY_DIR}/ThirdParty/${proj}-build)
  endif()
  if(WIN32 AND NOT TK_INTERNAL_PATH)
    set(TK_INTERNAL_PATH ${${proj}_OUTPUT_DIR}/ThirdParty/TclTk/internals/tk8.5)
    set(VTK_TK_INTENAL_PATH_DEFINE  "-DTK_INTERNAL_PATH:PATH=${TK_INTERNAL_PATH}")
  endif()
  if(WIN32 AND NOT TK_XLIB_PATH)
    set(TK_XLIB_PATH ${TCL_INCLUDE_PATH})
    set(VTK_TK_XLIB_PATH_DEFINE  "-DTK_XLIB_PATH:PATH=${TK_XLIB_PATH}")
  endif()

  set(${proj}_INSTALL_DIR "opencascade")

  #if(APPLE)
  #  set(${proj}_BUILD_TYPE "Debug")
  #else()
    set(${proj}_BUILD_TYPE ${CMAKE_BUILD_TYPE})
  #endif()

  ExternalProject_Add(${proj}
   ${location_args}
   PREFIX ${${proj}_OUTPUT_DIR}-prefix
   SOURCE_DIR ${${proj}_OUTPUT_DIR}
   BINARY_DIR ${${proj}_OUTPUT_BIN_DIR}
   UPDATE_COMMAND ""
   CMAKE_CACHE_ARGS
   -DCMAKE_CXX_COMPILER:STRING=${CMAKE_CXX_COMPILER}
   -DCMAKE_C_COMPILER:STRING=${CMAKE_C_COMPILER}
   -DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS}
   -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
   -DCMAKE_THREAD_LIBS:STRING=-lpthread
   -DCMAKE_MACOSX_RPATH:INTERNAL=1
   -DBUILD_EXAMPLES:BOOL=OFF
   -DBUILD_SHARED_LIBS:BOOL=${GDCM_BUILD_SHARED_LIBRARIES}
   -DBUILD_LIBRARY_TYPE:STRING=${GDCM_BUILD_LIBRARY_TYPE}
   -DBUILD_TESTING:BOOL=OFF
   -DBUILD_MODULE_Draw:BOOL=OFF
   -DCMAKE_BUILD_TYPE:STRING=${${proj}_BUILD_TYPE}
   -D3RDPARTY_TCL_INCLUDE_DIR:PATH=${TCL_INCLUDE_PATH}
   -D3RDPARTY_TCL_LIBRARY_DIR:PATH=${TCL_LIBRARY_DIR}
   -D3RDPARTY_TK_INCLUDE_DIR:PATH=${TK_INCLUDE_PATH}
   -D3RDPARTY_TK_LIBRARY_DIR:PATH=${TK_LIBRARY_DIR}
   -DUSE_VTK:BOOL=ON
   -DVTK_VERSION:STRING=${VTK_MAJOR_VERSION}.${VTK_MINOR_VERSION}
   -DVTK_DIR:PATH=${VTK_DIR}
   -D3RDPARTY_VTK_DIR:PATH=${VTK_DIR}
   -D3RDPARTY_VTK_INCLUDE_DIR:PATH=${3RDPARTY_VTK_INCLUDE_DIR}
   -D3RDPARTY_VTK_LIBRARY_DIR:PATH=${3RDPARTY_VTK_LIBRARY_DIR}
   -DINSTALL_DIR:PATH=${${proj}_INSTALL_DIR}
   -DCMAKE_INSTALL_PREFIX:STRING=${SV_INSTALL_ROOT_DIR}
   INSTALL_COMMAND ""
   DEPENDS
   ${${proj}_DEPENDENCIES}
   )
set(${proj}_SOURCE_DIR ${${proj}_OUTPUT_DIR})
set(${proj}_DIR ${${proj}_OUTPUT_BIN_DIR})

else()
  ExternalProject_Add_Empty(${proj} DEPENDS ${${proj}_DEPENDENCIES})
  #file(COPY ${${proj}_DIR}/cmake_install.cmake
  #     DESTINATION ${CMAKE_BINARY_DIR}/empty/${proj}-build/)
endif()
if(SV_INSTALL_EXTERNALS)
  ExternalProject_Install_CMake(${proj})
endif()
mark_as_superbuild(${proj}_SOURCE_DIR:PATH)

mark_as_superbuild(
  VARS ${proj}_DIR:PATH
  LABELS "FIND_PACKAGE"
  )
