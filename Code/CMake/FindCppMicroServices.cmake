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

set(US_RCC_EXECUTABLE_NAME usResourceCompiler)
set(US_MODULE_INIT_TEMPLATE "${SV_SOURCE_DIR}/CMake/CppMicroServices/usModuleInit.cpp")
set(US_RESOURCE_RC_TEMPLATE "${SV_SOURCE_DIR}/CMake/CppMicroServices/us_resources.rc.in")
set(US_CMAKE_RESOURCE_DEPENDENCIES_CPP "${SV_SOURCE_DIR}/CMake/CppMicroServices/usCMakeResourceDependencies.cpp")

# The CppMicroServices resource compiler
find_program(US_RCC_EXECUTABLE ${US_RCC_EXECUTABLE_NAME}
  PATHS "${MITK_DIR}/bin"
  PATH_SUFFIXES Release Debug RelWithDebInfo MinSizeRel)
mark_as_advanced(US_RCC_EXECUTABLE)

# Include helper macros
include(CMakeParseArguments)
if(CMAKE_VERSION VERSION_LESS "2.8.8")
  # We need the HANDLE_COMPONENTS argument
  include("${SV_SOURCE_DIR}/CMake/CppMicroServices/FindPackageHandleStandardArgs.cmake")
elseif(NOT COMMAND find_package_handle_standard_args)
  include(FindPackageHandleStandardArgs)
endif()
include("${SV_SOURCE_DIR}/CMake/CppMicroServices/usFunctionGenerateModuleInit.cmake")
include("${SV_SOURCE_DIR}/CMake/CppMicroServices/usFunctionAddResources.cmake")
include("${SV_SOURCE_DIR}/CMake/CppMicroServices/usFunctionCheckCompilerFlags.cmake")
include("${SV_SOURCE_DIR}/CMake/CppMicroServices/usFunctionEmbedResources.cmake")
include("${SV_SOURCE_DIR}/CMake/CppMicroServices/usFunctionCheckResourceLinking.cmake")
include("${SV_SOURCE_DIR}/CMake/CppMicroServices/usFunctionGetResourceSource.cmake")

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
