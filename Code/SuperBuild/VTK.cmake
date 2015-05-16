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
set(proj VTK)
if(${CMAKE_PROJECT_NAME}_USE_SYSTEM_${proj})
  set(${proj}_DEPENDENCIES "")
  if(NOT ${CMAKE_PROJECT_NAME}_USE_SYSTEM_TCL)
    message(WARNING  "You have specified ${CMAKE_PROJECT_NAME}_USE_SYSTEM_${proj} but not ${CMAKE_PROJECT_NAME}_USE_SYSTEM_TCL. This is not reccomended, as the libraries may not match, so it may lead to namespace pollution.")
  endif()
else()
  set(${proj}_DEPENDENCIES "TCL")
endif()

ExternalProject_Include_Dependencies(${proj} 
  PROJECT_VAR proj
  EP_ARGS_VAR ${proj}_EP_ARGS 
  DEPENDS_VAR ${proj}_DEPENDENCIES)

# Sanity checks
if(DEFINED VTK_DIR AND NOT EXISTS ${VTK_DIR})
  message(FATAL_ERROR "VTK_DIR variable is defined but corresponds to non-existing directory")
endif()

if(APPLE)
  if(NOT DEFINED TK_INTERNAL_PATH)
    STRING(REGEX REPLACE "/Headers" "/PrivateHeaders" _TK_INTERNAL_PATH ${TK_INCLUDE_PATH})
    set(TK_INTERNAL_PATH "${_TK_INTERNAL_PATH}" CACHE STRING "The path to the Tk internal headers (tkInt.h)")
    set(VTK_TK_INTENAL_PATH_DEFINE  "-DTK_INTERNAL_PATH:PATH=${TK_INTERNAL_PATH}")
  endif()
  list(APPEND EXTERNAL_PROJECT_OPTIONAL_ARGS
    -DTK_INTERNAL_PATH:STRING=${TK_INTERNAL_PATH}
    -DVTK_USE_CARBON:BOOL=OFF
    -DVTK_USE_COCOA:BOOL=ON # Default to Cocoa, VTK/CMakeLists.txt will enable Carbon and disable cocoa if needed
    -DVTK_USE_X:BOOL=OFF
    )
endif()


if(NOT ${CMAKE_PROJECT_NAME}_USE_SYSTEM_${proj})

  set(${proj}_OUTPUT_DIR ${CMAKE_BINARY_DIR}/kw/${proj})
  set(${proj}_OUTPUT_BIN_DIR ${CMAKE_BINARY_DIR}/kw/${proj}-build)
  if(WIN32 AND NOT TK_INTERNAL_PATH)
    set(TK_INTERNAL_PATH ${${proj}_OUTPUT_DIR}/ThirdParty/TclTk/internals/tk8.5)
    set(VTK_TK_INTENAL_PATH_DEFINE  "-DTK_INTERNAL_PATH:PATH=${TK_INTERNAL_PATH}")
  endif()
  if(WIN32 AND NOT TK_XLIB_PATH)
    set(TK_XLIB_PATH ${TCL_INCLUDE_PATH})
    set(VTK_TK_XLIB_PATH_DEFINE  "-DTK_XLIB_PATH:PATH=${TK_XLIB_PATH}")
  endif()

  ExternalProject_Add(${proj}
   ${${proj}_EP_ARGS}
   GIT_REPOSITORY "${git_protocol}://github.com/Kitware/VTK.git"
   PREFIX ${${proj}_OUTPUT_DIR}-PREFIX
   SOURCE_DIR ${${proj}_OUTPUT_DIR}
   BINARY_DIR ${${proj}_OUTPUT_BIN_DIR}
   GIT_TAG "v6.0.0"
   UPDATE_COMMAND ""
   CMAKE_CACHE_ARGS
   -DCMAKE_CXX_COMPILER:STRING=${CMAKE_CXX_COMPILER}
   -DCMAKE_C_COMPILER:STRING=${CMAKE_C_COMPILER}
   -DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS}
   -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
   -DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
   -DCMAKE_THREAD_LIBS:STRING=-lpthread
   -DBUILD_SHARED_LIBS:BOOL=OFF
   -DBUILD_TESTING:BOOL=OFF
   -DVTK_WRAP_TCL:BOOL=ON
   -DVTK_Group_Tk:BOOL=ON
   -DTCL_INCLUDE_PATH:PATH=${TCL_INCLUDE_PATH}
   -DTCL_LIBRARY:FILEPATH=${TCL_LIBRARY}
   -DTCL_TCLSH:FILEPATH=${TCL_TCLSH}
   -DTK_INCLUDE_PATH:PATH=${TK_INCLUDE_PATH}
   ${VTK_TK_XLIB_PATH_DEFINE}
   ${VTK_TK_INTENAL_PATH_DEFINE}
   -DTK_LIBRARY:FILEPATH=${TK_LIBRARY}
   -DBUILD_EXAMPLES:BOOL=OFF
   -DCMAKE_INSTALL_PREFIX:STRING=${SIMVASCULAR_INSTALL_ROOT_DIR}
   -DVTK_INSTALL_RUNTIME_DIR:PATH=${SIMVASCULAR_INSTALL_VTK_RUNTIME_DIR}
   -DVTK_INSTALL_LIBRARY_DIR:PATH=${SIMVASCULAR_INSTALL_VTK_LIBRARY_DIR}
   -DVTK_INSTALL_ARCHIVE_DIR:PATH=${SIMVASCULAR_INSTALL_VTK_ARCHIVE_DIR}
   -DVTK_INSTALL_INCLUDE_DIR:PATH=${SIMVASCULAR_INSTALL_VTK_INCLUDE_DIR}
   INSTALL_COMMAND ""
   )

set(${proj}_SOURCE_DIR ${${proj}_OUTPUT_DIR})
mark_as_superbuild(${proj}_SOURCE_DIR:PATH)

set(${proj}_DIR ${${proj}_OUTPUT_BIN_DIR})

if(SIMVASCULAR_INSTALL_EXTERNALS)
  ExternalProject_Install_CMake(${proj})
endif()

else()
  ExternalProject_Add_Empty(${proj} DEPENDS ${${proj}_DEPENDENCIES})
endif()




mark_as_superbuild(
  VARS ${proj}_DIR:PATH
  LABELS "FIND_PACKAGE"
  )
