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

set(proj GTS)

set(${proj}_DEPENDENCIES "GLIB")

ExternalProject_Include_Dependencies(${proj}
  PROJECT_VAR proj
  DEPENDS_VAR ${proj}_DEPENDENCIES
  EP_ARGS_VAR ${proj}_EP_ARGS
  USE_SYSTEM_VAR ${CMAKE_PROJECT_NAME}_USE_SYSTEM_${proj}
  )

if(${PROJECT_NAME}_USE_SYSTEM_${proj})
  string(TOLOWER ${proj} _lower)
  set(${proj}_LIB_DIR "" CACHE PATH "A directory where ${_lower} may be, use this if its not in the path")
endif()

set(${proj}_OUTPUT_DIR ${CMAKE_BINARY_DIR}/externals/${proj})
set(${proj}_OUTPUT_BIN_DIR ${CMAKE_BINARY_DIR}/externals/${proj}-build)


if(UNIX)
  set(SuperBuild_${proj}_URL "${SuperBuild_Libraries_Dir}/gts-snapshot-121130.tar" CACHE
    STRING "Location of gts, can be web address or local path")
  set(${proj}_INSTALL_DIR ${CMAKE_BINARY_DIR}/externals/${proj}-install)
  set(configure_cmd CONFIGURE_COMMAND CC="${CMAKE_C_COMPILER}" && CPP="${CMAKE_CXX_COMPILER}" && ${${proj}_OUTPUT_DIR}/configure --prefix=${${proj}_INSTALL_DIR})
  set(build_cmd ${MAKE})
  set(INSTALL_ARGS)
endif()

if(MSVC)
	dev_message("WINDOWS")
	set(SuperBuild_${proj}_URL "${SuperBuild_Libraries_Dir}/gts-2010.03.21-bin-win-x64.tar.gz" CACHE
		STRING "Location of gts, can be web address or local path")
	unset(configure_cmd)
	unset(build_cmd)
	set(INSTALL_ARGS INSTALL_COMMAND "echo")
	set(${proj}_INSTALL_DIR ${${proj}_OUTPUT_DIR})
endif()

mark_as_superbuild(SuperBuild_${proj}_URL:STRING)
mark_as_advanced(SuperBuild_${proj}_URL)

if(NOT ${CMAKE_PROJECT_NAME}_USE_SYSTEM_${proj})
  ExternalProject_Add(${proj}
   ${${proj}_EP_ARGS}
   URL ${SuperBuild_${proj}_URL}
   PREFIX ${${proj}_OUTPUT_DIR}-prefix
   SOURCE_DIR ${${proj}_OUTPUT_DIR}
   CONFIGURE_COMMAND "${configure_cmd}"
   BUILD_COMMAND "${build_cmd}"
   UPDATE_COMMAND ""
   ${INSTALL_ARGS}
   BUILD_IN_SOURCE 1
   )

  set(${proj}_SOURCE_DIR ${${proj}_OUTPUT_DIR})
  set(${proj}_INCLUDE_DIR ${${proj}_INSTALL_DIR}/include)
  set(${proj}_LIB_DIR ${${proj}_INSTALL_DIR}/lib)
  if(UNIX)
    set(GTS_LIBRARIES ${${proj}_LIB_DIR}/libgts.a)
    set(GTS_gts_LIBRARY ${${proj}_LIB_DIR}/libgts.a)
    mark_as_superbuild(GTS_gts_LIBRARY:FILEPATH)
    mark_as_superbuild(GTS_LIBRARIES:FILEPATH)
  endif()

  
  #message("${proj}_VERSION ${${proj}_VERSION}") 
else()
  ExternalProject_Add_Empty(${proj} DEPENDS ${${proj}_DEPENDENCIES})
endif()

mark_as_superbuild(${proj}_SOURCE_DIR:PATH)
mark_as_superbuild(${proj}_LIB_DIR:PATH)
mark_as_superbuild(${proj}_INCLUDE_DIR:PATH)
mark_as_superbuild(${proj}_PATH:PATH)

mark_as_superbuild(
  VARS ${proj}_DIR:PATH
  LABELS "FIND_PACKAGE"
  )
