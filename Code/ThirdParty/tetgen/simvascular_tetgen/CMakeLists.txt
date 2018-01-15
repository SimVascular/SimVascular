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
