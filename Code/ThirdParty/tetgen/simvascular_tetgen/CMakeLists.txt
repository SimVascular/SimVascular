# THIS FILE WAS EDITED BY Jameson Merkow (jmerkow@eng.ucsd.edu) to include install support  - March 25 2011

# Set  the minimum  required version  of cmake  for a  project.
cmake_minimum_required(VERSION 2.6)
project(TETGEN)
# message(STATUS "TETGEN_INSTALL_DIR: ${TETGEN_INSTALL_DIR}")
# Add an executable to the project using the specified source files.
###SIMVASCULAR_CHANGE
# add_executable(tetgen tetgen.cxx predicates.cxx)

#Add a library to the project using the specified source files.
# In Linux/Unix, it will creates the libtet.a
###SIMVASCULAR_CHANGE
set(lib ${SV_LIB_THIRDPARTY_TETGEN_NAME})
if(SV_USE_THIRDPARTY_SHARED_LIBRARIES)
  add_library(${lib} SHARED tetgen.cxx predicates.cxx)
else()
  add_library(${lib} STATIC tetgen.cxx predicates.cxx)
endif()

#Set properties on a target.
#We use this here to set -DTETLIBRARY for when compiling the
#library
###SIMVASCULAR_CHANGE
set_target_properties(${lib} PROPERTIES "COMPILE_DEFINITIONS" TETLIBRARY)

if(SV_INSTALL_LIBS)
	install(TARGETS ${lib}
		RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR} COMPONENT ThirdPartyExecutables
		LIBRARY DESTINATION ${SV_INSTALL_LIBRARY_DIR} COMPONENT ThirdPartyLibraries
		ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR} COMPONENT ThirdPartyLibraries
		)
endif()
if(SV_INSTALL_HEADERS)
        install(FILES tetgen.h
		DESTINATION ${SV_INSTALL_INCLUDE_DIR}/thirdparty/tetgen COMPONENT ThirdPartyHeaders
		)
endif()
