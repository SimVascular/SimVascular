# Copyright (c) Stanford University, The Regents of the University of
#               California, and others.
#
# All Rights Reserved.
#
# See Copyright-SimVascular.txt for additional details.
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

# A package config for CppMicroServices.
# This file loads component specific configuration files and
# sets the following variables which can be used in other
# CMake projects to build and link against CppMicroServices:
#
#   US_INCLUDE_DIRS
#   US_LIBRARIES
#   US_RUNTIME_LIBRARY_DIRS
#
# The following variables are aliases for the ones above:
#
#   CppMicroServices_INCLUDE_DIRS
#   CppMicroServices_LIBRARIES
#   CppMicroServices_RUNTIME_LIBRARY_DIRS
#
# To specify a compatible version for a specific component,
# set the following variable before calling find_package:
#
#   US_<component>_FIND_VERSION
#
# After find_package returns successfully, the following additional
# variables will be set:
#
#   US_FOUND
#   CPPMICROSERVICES_FOUND
#
#   US_RCC_EXECUTABLE
#
#   US_CXX_FLAGS
#   US_CXX_FLAGS_RELEASE
#   US_CXX_FLAGS_DEBUG
#   US_C_FLAGS
#   US_C_FLAGS_RELEASE
#   US_C_FLAGS_DEBUG
#   US_LINK_FLAGS
#   US_LINK_FLAGS_RELEASE
#   US_LINK_FLAGS_DEBUG
#   US_COMPILE_DEFINITIONS
#   US_COMPILE_DEFINITIONS_RELEASE
#   US_COMPILE_DEFINITIONS_DEBUG
#
#   US_VERSION
#   US_VERSION_MAJOR
#   US_VERSION_MINOR
#   US_VERSION_PATCH
#   US_VERSION_TWEAK
#   US_VERSION_COUNT
#
# Additional component specific variables:
#
#   US_<component>_FOUND
#
#   US_<component>_VERSION
#   US_<component>_VERSION_MAJOR
#   US_<component>_VERSION_MINOR
#   US_<component>_VERSION_PATCH
#   US_<component>_VERSION_TWEAK
#   US_<component>_VERSION_COUNT
#
#   US_<component>_CXX_FLAGS
#   US_<component>_CXX_FLAGS_RELEASE
#   US_<component>_CXX_FLAGS_DEBUG
#   US_<component>_C_FLAGS
#   US_<component>_C_FLAGS_RELEASE
#   US_<component>_C_FLAGS_DEBUG
#   US_<component>_LINK_FLAGS
#   US_<component>_LINK_FLAGS_RELEASE
#   US_<component>_LINK_FLAGS_DEBUG
#   US_<component>_COMPILE_DEFINITIONS
#   US_<component>_COMPILE_DEFINITIONS_RELEASE
#   US_<component>_COMPILE_DEFINITIONS_DEBUG
#
include(FindPackageHandleStandardArgs)
include(CMakeParseArguments)

set(CppMicroServices_POSSIBLE_US_FILES_PATHS
  ${SV_SOURCE_DIR}/CMake/CppMicroServices
  ${SimVascular_CMAKE_DIR}/CppMicroServices
  )

find_path(US_CMAKE_FILES_DIR
  NAMES
  usModuleInit.cpp
  PATHS
  ${CppMicroServices_POSSIBLE_US_FILES_PATHS}
  NO_DEFAULT_PATH
  )

set(US_RCC_EXECUTABLE_NAME usResourceCompiler)
set(US_MODULE_INIT_TEMPLATE "${US_CMAKE_FILES_DIR}/usModuleInit.cpp")
set(US_RESOURCE_RC_TEMPLATE "${US_CMAKE_FILES_DIR}/us_resources.rc.in")
set(US_CMAKE_RESOURCE_DEPENDENCIES_CPP "${US_CMAKE_FILES_DIR}/usCMakeResourceDependencies.cpp")

# The CppMicroServices resource compiler
set(CppMicroServices_POSSIBLE_US_EXECUTABLE_PATHS
  "${MITK_DIR}/bin"
  "${MITK_DIR}/bin/Release"
  "${MITK_DIR}/bin/Debug"
  "${MITK_DIR}/bin/RelWithDebInfo"
  "${MITK_DIR}/bin/MinSizeRel"
  )

find_program(US_RCC_EXECUTABLE
  NAMES
  ${US_RCC_EXECUTABLE_NAME}
  PATHS
  ${CppMicroServices_POSSIBLE_US_EXECUTABLE_PATHS}
  NO_DEFAULT_PATH
  )

mark_as_advanced(US_RCC_EXECUTABLE)

include("${US_CMAKE_FILES_DIR}/usFunctionGenerateModuleInit.cmake")
include("${US_CMAKE_FILES_DIR}/usFunctionAddResources.cmake")
include("${US_CMAKE_FILES_DIR}/usFunctionCheckCompilerFlags.cmake")
include("${US_CMAKE_FILES_DIR}/usFunctionEmbedResources.cmake")
include("${US_CMAKE_FILES_DIR}/usFunctionCheckResourceLinking.cmake")
include("${US_CMAKE_FILES_DIR}/usFunctionGetResourceSource.cmake")

usFunctionCheckResourceLinking()

# Clear variables
set(US_LIBRARIES )
set(US_RUNTIME_LIBRARY_DIRS )
set(US_CXX_FLAGS )
set(US_CXX_FLAGS_RELEASE )
set(US_CXX_FLAGS_DEBUG )
set(US_C_FLAGS )
set(US_C_FLAGS_RELEASE )
set(US_C_FLAGS_DEBUG )
set(US_LINK_FLAGS )
set(US_LINK_FLAGS_RELEASE )
set(US_LINK_FLAGS_DEBUG )
set(US_COMPILE_DEFINITIONS )
set(US_COMPILE_DEFINITIONS_RELEASE )
set(US_COMPILE_DEFINITIONS_DEBUG )

# Components support
set(US_MODULES Core) # ConfigAdmin EventAdmin ...

set(US_Core_MODULE_DEPS )

if(NOT CppMicroServices_FIND_COMPONENTS)
  set(CppMicroServices_FIND_COMPONENTS Core)
endif()

set(US_MODULES_NEEDED )
foreach(component ${CppMicroServices_FIND_COMPONENTS})
  list(APPEND US_MODULES_NEEDED ${US_${component}_MODULE_DEPS} ${component})
endforeach()
list(REMOVE_DUPLICATES US_MODULES_NEEDED)

set(CppMicroServices_FOUND TRUE)
foreach(component ${US_MODULES_NEEDED})
  if(NOT EXISTS "${CMAKE_CURRENT_LIST_DIR}/us${component}Config.cmake")
    set(US_${component}_FOUND 0)
    set(CppMicroServices_${component}_FOUND 0)
  else()
    find_package(us${component} ${US_${component}_FIND_VERSION} QUIET REQUIRED
                 HINTS ${CMAKE_CURRENT_LIST_DIR}
                 NO_DEFAULT_PATH
                )
    mark_as_advanced(us${component}_DIR)
    set(US_${component}_FOUND ${us${component}_FOUND})
    set(CppMicroServices_${component}_FOUND ${US_${component}_FOUND})
  endif()

  if(US_${component}_FOUND)
    list(APPEND US_INCLUDE_DIRS ${US_${component}_INCLUDE_DIRS})
    list(APPEND US_LIBRARIES ${US_${component}_LIBRARIES})
    list(APPEND US_RUNTIME_LIBRARY_DIRS ${US_${component}_RUNTIME_LIBRARY_DIRS})
    set(US_CXX_FLAGS "${US_CXX_FLAGS} ${US_${component}_CXX_FLAGS}")
    set(US_CXX_FLAGS_RELEASE "${US_CXX_FLAGS_RELEASE} ${US_${component}_CXX_FLAGS_RELEASE}")
    set(US_CXX_FLAGS_DEBUG "${US_CXX_FLAGS_DEBUG} ${US_${component}_CXX_FLAGS_DEBUG}")
    set(US_C_FLAGS "${US_C_FLAGS} ${US_${component}_C_FLAGS}")
    set(US_C_FLAGS_RELEASE "${US_C_FLAGS_RELEASE} ${US_${component}_C_FLAGS_RELEASE}")
    set(US_C_FLAGS_DEBUG "${US_C_FLAGS_DEBUG} ${US_${component}_C_FLAGS_DEBUG}")
    set(US_LINK_FLAGS "${US_LINK_FLAGS} ${US_${component}_LINK_FLAGS}")
    set(US_LINK_FLAGS_RELEASE "${US_LINK_FLAGS_RELEASE} ${US_${component}_LINK_FLAGS_RELEASE}")
    set(US_LINK_FLAGS_DEBUG "${US_LINK_FLAGS_DEBUG} ${US_${component}_LINK_FLAGS_DEBUG}")
    set(US_COMPILE_DEFINITIONS "${US_COMPILE_DEFINITIONS} ${US_${component}_COMPILE_DEFINITIONS}")
    set(US_COMPILE_DEFINITIONS_RELEASE "${US_COMPILE_DEFINITIONS_RELEASE} ${US_${component}_COMPILE_DEFINITIONS_RELEASE}")
    set(US_COMPILE_DEFINITIONS_DEBUG "${US_COMPILE_DEFINITIONS_DEBUG} ${US_${component}_COMPILE_DEFINITIONS_DEBUG}")

    set(US_${component}_VERSION ${${component}_VERSION})
    set(US_${component}_VERSION_MAJOR ${${component}_VERSION_MAJOR})
    set(US_${component}_VERSION_MINOR ${${component}_VERSION_MINOR})
    set(US_${component}_VERSION_PATCH ${${component}_VERSION_PATCH})
    set(US_${component}_VERSION_TWEAK ${${component}_VERSION_TWEAK})
    set(US_${component}_VERSION_COUNT ${${component}_VERSION_COUNT})
  else()
    if(CppMicroServices_FIND_REQUIRED_${component})
      set(CppMicroServices_FOUND FALSE)
    endif()
  endif()
endforeach()

if(US_INCLUDE_DIRS)
  list(REMOVE_DUPLICATES US_INCLUDE_DIRS)
endif()
if(US_LIBRARIES)
  list(REMOVE_DUPLICATES US_LIBRARIES)
endif()
if(US_RUNTIME_LIBRARY_DIRS)
  list(REMOVE_DUPLICATES US_RUNTIME_LIBRARY_DIRS)
endif()

set(CppMicroServices_INCLUDE_DIRS ${US_INCLUDE_DIRS})
set(CppMicroServices_LIBRARIES ${US_LIBRARIES})
set(CppMicroServices_RUNTIME_LIBRARY_DIRS ${US_RUNTIME_LIBRARY_DIRS})

set(CppMicroServices_CONFIG ${CMAKE_CURRENT_LIST_FILE})
find_package_handle_standard_args(CppMicroServices
  HANDLE_COMPONENTS
  CONFIG_MODE
)

string(TOUPPER "CppMicroServices" CppMicroServices_UPPER)
set(US_FOUND ${${CppMicroServices_UPPER}_FOUND})
