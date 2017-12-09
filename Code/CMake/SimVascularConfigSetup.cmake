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

#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Get used lib names
set(SV_CONF_USED_LIBS )
set(SV_CONF_USED_PLUGINS )
set(SV_TARGETS )
foreach(lib ${SV_LIB_NAMES})
  if(TARGET ${lib})
    get_target_property(${lib}_TYPE ${lib} "TYPE")
    if("${${lib}_TYPE}" STREQUAL "STATIC_LIBRARY")
      list(APPEND SV_TARGETS "${lib}")
      list(APPEND SV_CONF_USED_LIBS "${CMAKE_STATIC_LIBRARY_PREFIX}${lib}${CMAKE_STATIC_LIBRARY_SUFFIX}")
    elseif("${${lib}_TYPE}" STREQUAL "SHARED_LIBRARY")
      list(APPEND SV_TARGETS "${lib}")
      list(APPEND SV_CONF_USED_LIBS "${CMAKE_SHARED_LIBRARY_PREFIX}${lib}${CMAKE_SHARED_LIBRARY_SUFFIX}")
    endif()
  endif()
endforeach()
foreach(lib ${SV_PLUGIN_NAMES})
  if(TARGET ${lib})
    get_target_property(${lib}_TYPE ${lib} "TYPE")
    if("${${lib}_TYPE}" STREQUAL "STATIC_LIBRARY")
      list(APPEND SV_TARGETS "${lib}")
      list(APPEND SV_CONF_USED_PLUGINS "${CMAKE_STATIC_LIBRARY_PREFIX}${lib}${CMAKE_STATIC_LIBRARY_SUFFIX}")
    elseif("${${lib}_TYPE}" STREQUAL "SHARED_LIBRARY")
      list(APPEND SV_TARGETS "${lib}")
      list(APPEND SV_CONF_USED_PLUGINS "${CMAKE_SHARED_LIBRARY_PREFIX}${lib}${CMAKE_SHARED_LIBRARY_SUFFIX}")
    endif()
  endif()
endforeach()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Export all targets
export(TARGETS ${SV_TARGETS} FILE "${CMAKE_BINARY_DIR}/SimVascularTargets.cmake")
export(PACKAGE SimVascular)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# BUILD TREE
# SimVascular Configure file
set(SV_CONF_INCLUDE_DIRS "${SV_INCLUDE_DIRS}")
foreach(dir ${SV_CORE_LIBRARY_DIRS})
  list(APPEND SV_CONF_INCLUDE_DIRS ${dir})
endforeach()
foreach(dir ${SV_MODULE_DIRS})
  list(APPEND SV_CONF_INCLUDE_DIRS ${dir})
endforeach()
foreach(dir ${SV_PLUGIN_DIRS})
  list(APPEND SV_CONF_INCLUDE_DIRS ${dir})
  list(APPEND SV_CONF_INCLUDE_DIRS ${dir}/src)
  list(APPEND SV_CONF_INCLUDE_DIRS ${dir}/src/internal)
endforeach()
set(SV_CONF_SOURCE_DIR "${SV_SOURCE_DIR}")
set(SV_CONF_BINARY_DIR "${SV_BINARY_DIR}")
configure_file("${SV_SOURCE_DIR}/CMake/SimVascularConfig.cmake.in" "${SV_BINARY_DIR}/SimVascularConfig.cmake" @ONLY)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# INSTALL TREE
# SimVascular Configure file
set(SV_CONF_INCLUDE_DIRS "")
set(SV_CONF_SOURCE_DIR "")
set(SV_CONF_BINARY_DIR "")
configure_file("${SV_SOURCE_DIR}/CMake/SimVascularConfig.cmake.in" "${SV_BINARY_DIR}/tmp/SimVascularConfig.cmake" @ONLY)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# VERSION
configure_file("${SV_SOURCE_DIR}/CMake/SimVascularConfigVersion.cmake.in"
  "${SV_BINARY_DIR}/SimVascularConfigVersion.cmake" @ONLY)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# INSTALL
# SimVascular Version Configure file
# Install the FooBarConfig.cmake and FooBarConfigVersion.cmake
install(FILES
  "${SV_BINARY_DIR}/tmp/SimVascularConfig.cmake"
  "${SV_BINARY_DIR}/SimVascularConfigVersion.cmake"
  DESTINATION "${SV_INSTALL_CMAKE_DIR}" COMPONENT CMake)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# INSTALL CMake stuff
install(DIRECTORY "${SV_SOURCE_DIR}/CMake"
  DESTINATION "${SV_INSTALL_CMAKE_DIR}" COMPONENT CMake)

install(DIRECTORY "${SV_SOURCE_DIR}/../Externals/CMakeExternals"
  DESTINATION "${SV_INSTALL_CMAKE_DIR}/Externals" COMPONENT CMake)
install(FILES "${SV_SOURCE_DIR}/../Externals/CMakeLists.txt"
  DESTINATION "${SV_INSTALL_CMAKE_DIR}/Externals" COMPONENT CMake)
#-----------------------------------------------------------------------------
