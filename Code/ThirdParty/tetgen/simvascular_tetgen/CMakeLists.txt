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
add_library(lib_simvascular_tet STATIC tetgen.cxx predicates.cxx)

#Set properties on a target. 
#We use this here to set -DTETLIBRARY for when compiling the
#library
###SIMVASCULAR_CHANGE
set_target_properties(lib_simvascular_tet PROPERTIES "COMPILE_DEFINITIONS" TETLIBRARY)

# ###SIMVASCULAR_CHANGE
# install(TARGETS tet 
# 	RUNTIME DESTINATION ${TETGEN_INSTALL_DIR}
# 	LIBRARY DESTINATION ${TETGEN_INSTALL_DIR}
# 	ARCHIVE DESTINATION ${TETGEN_INSTALL_DIR}
# 	)

# install(FILES tetgen.h DESTINATION ${TETGEN_INSTALL_DIR})
