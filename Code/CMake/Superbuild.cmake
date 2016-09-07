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

#message(STATUS "Depends var: ${${PROJECT_NAME}_DEPENDS}")
#------------------------------------------------------------------------------
# Configure and build SimVascular
#------------------------------------------------------------------------------
set(proj ${PROJECT_NAME})

ExternalProject_Add(${proj}
  ${${proj}_EP_ARGS}
  PREFIX ${${PROJECT_NAME}_BINARY_DIR}/cache
  SOURCE_DIR ${CMAKE_CURRENT_SOURCE_DIR}
  BINARY_DIR ${${PROJECT_NAME}_BINARY_DIR}/build
  DOWNLOAD_COMMAND ""
  UPDATE_COMMAND ""
  INSTALL_COMMAND ""
  CMAKE_ARGS
  CMAKE_CACHE_ARGS
  -DSV_SUPERBUILD:BOOL=OFF
  -DCMAKE_INSTALL_PREFIX:PATH=${CMAKE_INSTALL_PREFIX}
  -DCMAKE_CXX_COMPILER:FILEPATH=${CMAKE_CXX_COMPILER}
  -DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS}
  -DCMAKE_EXE_LINKER_FLAGS:STRING=${CMAKE_EXE_LINKER_FLAGS}
  -DCMAKE_C_COMPILER:FILEPATH=${CMAKE_C_COMPILER}
  -DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
  -DADDITIONAL_C_FLAGS:STRING=${ADDITIONAL_C_FLAGS}
  -DADDITIONAL_CXX_FLAGS:STRING=${ADDITIONAL_CXX_FLAGS}
  -DSV_RELEASE_TIMESTAMP:STRING=${SV_RELEASE_TIMESTAMP}
  -DCMAKE_PREFIX_PATH:PATH=${CMAKE_PREFIX_PATH}
  DEPENDS 
  ${${PROJECT_NAME}_DEPENDS}
  )

ExternalProject_Install_CMake(${proj})

# This custom external project step forces the build and later
# steps to run whenever a top level build is done...
ExternalProject_Add_Step(${proj} forcebuild
  COMMAND ${CMAKE_COMMAND} -E remove
  ${${PROJECT_NAME}_BINARY_DIR}/cache/src/${PROJECT_NAME}-stamp/${PROJECT_NAME}-build
   COMMAND ${CMAKE_COMMAND} -E remove
   ${${PROJECT_NAME}_BINARY_DIR}/cache/src/${PROJECT_NAME}-stamp/RelWithDebInfo/${PROJECT_NAME}-build
  COMMENT "Forcing build step for '${proj}'"
  DEPENDEES build
  ALWAYS 1
  )

file(REMOVE ${${PROJECT_NAME}_BINARY_DIR}/cache/src/${PROJECT_NAME}-stamp/${PROJECT_NAME}-configure)
file(REMOVE ${${PROJECT_NAME}_BINARY_DIR}/cache/src/${PROJECT_NAME}-stamp/RelWithDebInfo/${PROJECT_NAME}-configure)

add_custom_target(forcebuild)
add_custom_command(TARGET forcebuild
  ${CMAKE_COMMAND} -E remove
  ${${PROJECT_NAME}_BINARY_DIR}/cache/src/${PROJECT_NAME}-stamp/${PROJECT_NAME}-build
  COMMENT "Forcing build step for '${proj}'")

# This will extend this target to the top level build
add_custom_target(copy-tcl)
add_custom_command(TARGET copy-tcl POST_BUILD
COMMAND ${CMAKE_COMMAND} --build ${${PROJECT_NAME}_BINARY_DIR}/build --target copy-tcl)
