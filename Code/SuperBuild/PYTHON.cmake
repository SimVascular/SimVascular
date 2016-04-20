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
#
set(proj PYTHON)
set(${proj}_DEPENDENCIES "")

ExternalProject_Include_Dependencies(${proj}
	PROJECT_VAR proj
	DEPENDS_VAR ${proj}_DEPENDENCIES
	EP_ARGS_VAR ${proj}_EP_ARGS
	USE_SYSTEM_VAR ${CMAKE_PROJECT_NAME}_USE_SYSTEM_${proj}
	)

if(${PROJECT_NAME}_USE_SYSTEM_${proj})
	unset(${proj}_LIBRARIES CACHE)
endif()

if(NOT ${CMAKE_PROJECT_NAME}_USE_SYSTEM_${proj})

	unset(PYTHONLIBS_FOUND CACHE)
	unset(PYTHON_INCLUDE_PATH CACHE)
	unset(PYTHON_LIBRARIES CACHE)
	unset(PYTHON_LIBRARY_PATH CACHE)
	unset(PYTHON_EXECUTABLE CACHE)
	unset(PYTHON_VERSION_STRING CACHE)

	set(${proj}_OUTPUT_DIR ${CMAKE_BINARY_DIR}/externals/PYTHON)
	set(${proj}_OUTPUT_BIN_DIR ${CMAKE_BINARY_DIR}/externals/PYTHON)

	if(LINUX)
		set(SuperBuild_${proj}_URL "${SV_SUPERBUILD_LIBS_DIR}/python-2.7-linux-x64-gnu.tar.gz" CACHE
			STRING "Location of ${proj}, can be web address or local path")
	elseif(APPLE)
		set(SuperBuild_${proj}_URL "/Users/adamupdegrove/Desktop/SV16/bin/osx/clang_70/x64/python-2.7-osx-x64.tar.gz" CACHE
			STRING "Location of ${proj}, can be web address or local path")
	elseif(WIN32)
		set(SuperBuild_${proj}_URL "${SV_SUPERBUILD_LIBS_DIR}/python-2.7-win-x64.tar.gz" CACHE
			STRING "Location of ${proj}, can be web address or local path")
	endif()

	mark_as_superbuild(SuperBuild_${proj}_URL:STRING)
	mark_as_advanced(SuperBuild_${proj}_URL)

	set(SV_${proj}_DIR ${${proj}_SOURCE_DIR})

	ExternalProject_Add(${proj}
		URL ${SuperBuild_${proj}_URL}
		PREFIX ${${proj}_OUTPUT_DIR}-prefix
		SOURCE_DIR ${${proj}_OUTPUT_DIR}
		BINARY_DIR ${${proj}_OUTPUT_BIN_DIR}
		CONFIGURE_COMMAND ""
		BUILD_COMMAND ""
		UPDATE_COMMAND ""
		INSTALL_COMMAND ""
		CMAKE_CACHE_ARGS
		-DCMAKE_CXX_COMPILER:STRING=${CMAKE_CXX_COMPILER}
		-DCMAKE_C_COMPILER:STRING=${CMAKE_C_COMPILER}
		-DCMAKE_CXX_FLAGS:STRING=${CMAKE_CXX_FLAGS}
		-DCMAKE_C_FLAGS:STRING=${CMAKE_C_FLAGS}
		-DCMAKE_BUILD_TYPE:STRING=${CMAKE_BUILD_TYPE}
		)

	if(WIN32)
	endif()
	if(LINUX)
	endif()
	if(APPLE)
		set(${proj}_SOURCE_DIR ${${proj}_OUTPUT_DIR})
		set(SV_${proj}_DIR ${${proj}_SOURCE_DIR})

		set(${proj}_EXECUTABLE ${${proj}_OUTPUT_BIN_DIR}/Python.framework/Versions/2.7/python2.7)
		set(${proj}_INCLUDE_PATH ${${proj}_OUTPUT_BIN_DIR}/Python.framework/Headers) 
		set(${proj}_LIBRARIES ${${proj}_OUTPUT_BIN_DIR}/libpython2.7.dylib)
	endif()
	get_filename_component(${proj}_LIBRARY_PATH ${${proj}_LIBRARIES} PATH)

else()
	ExternalProject_Add_Empty(${proj} DEPENDS ${${proj}_DEPENDENCIES})
endif()

mark_as_superbuild(${proj}_SOURCE_DIR:PATH)
mark_as_superbuild(SV_${proj}_DIR:PATH)
mark_as_superbuild(${proj}_INCLUDE_PATH:PATH)
mark_as_superbuild(${proj}_LIBRARIES:PATH)
mark_as_superbuild(${proj}_LIBRARY_PATH:PATH)
mark_as_superbuild(${proj}_EXECUTABLE:PATH)
