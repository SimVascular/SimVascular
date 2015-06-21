cmake_minimum_required(VERSION 2.8.8)
project(METIS)
if (NOT METIS_LIBRARY_NAME)
    set(METIS_LIBRARY_NAME metis)
endif()

#set default build type to relwithdebinfo, also sets flags O2 on linux 
if(NOT CMAKE_BUILD_TYPE AND NOT CMAKE_CONFIGURATION_TYPES)
    message(STATUS "Setting build type to 'RelWithDebInfo' as none was specified.")
    set(CMAKE_BUILD_TYPE RelWithDebInfo CACHE STRING "Choose the type of build." FORCE)
    mark_as_advanced(CMAKE_BUILD_TYPE)
    # Set the possible values of build type for cmake-gui
    set_property(CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release"
        "MinSizeRel" "RelWithDebInfo")
endif()

# set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${METIS_BINARY_DIR}")
# set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${METIS_BINARY_DIR}")
# set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${METIS_BINARY_DIR}")

if(WIN32)
    add_definitions("/Zi")
    add_definitions("-DWINDOWS")
endif()

add_subdirectory(Lib)
#message(STATUS "METIS_INSTALL_DIR: ${METIS_INSTALL_DIR}")

#add_subdirectory(Programs)
#add_subdirectory(Test)
