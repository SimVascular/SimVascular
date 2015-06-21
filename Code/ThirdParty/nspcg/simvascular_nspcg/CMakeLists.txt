cmake_minimum_required(VERSION 2.8.8)

Project(NSPCG Fortran)
if (NOT NSPCG_LIBRARY_NAME)
    set(NSPCG_LIBRARY_NAME nspcg)
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

if(WIN32)
    add_definitions("/Zi /Ox /MD")
endif()

add_definitions("-w")

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(CMAKE_Fortran_FLAGS "-ffixed-line-length-132")
else()
    set(CMAKE_Fortran_FLAGS "-extend_source 132")
endif()


set(FSRCS nspcg1.f nspcg2.f nspcg3.f nspcg4.f nspcg5.f)
add_library(${NSPCG_LIBRARY_NAME} STATIC ${FSRCS})
