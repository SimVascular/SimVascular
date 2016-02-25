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
set(lib lib_simvascular_tet)
add_library(${lib} ${SV_LIBRARY_TYPE} tetgen.cxx predicates.cxx)

#Set properties on a target.
#We use this here to set -DTETLIBRARY for when compiling the
#library
###SIMVASCULAR_CHANGE
set_target_properties(${lib} PROPERTIES "COMPILE_DEFINITIONS" TETLIBRARY)

if(SV_INSTALL_LIBS)
	install(TARGETS ${lib}
		RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR} COMPONENT PluginExecutables
		LIBRARY DESTINATION ${SV_INSTALL_LIBRARY_DIR} COMPONENT PluginLibraries
		ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR} COMPONENT PluginLibraries
		)
endif()
if(SV_INSTALL_HEADERS)
        install(FILES tetgen.h
		DESTINATION ${SV_INSTALL_INCLUDE_DIR}/thirdparty/tetgen COMPONENT PluginHeaders
		)
endif()
