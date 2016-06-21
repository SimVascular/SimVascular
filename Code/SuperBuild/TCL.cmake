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
set(proj TCL)
set(${proj}_DEPENDENCIES "")
if(LINUX)

endif()

ExternalProject_Include_Dependencies(${proj}
	PROJECT_VAR proj
	DEPENDS_VAR ${proj}_DEPENDENCIES
	EP_ARGS_VAR ${proj}_EP_ARGS
	USE_SYSTEM_VAR ${CMAKE_PROJECT_NAME}_USE_SYSTEM_${proj}
	)

if(${PROJECT_NAME}_USE_SYSTEM_${proj})
	unset(${proj}_LIB_DIR CACHE)
endif()

if(NOT ${CMAKE_PROJECT_NAME}_USE_SYSTEM_${proj})

	# Because we are downloading prebuilt libraries and binaries for tcl,
	# we set the source dir and bin dir to both be TCL_EXT_BIN_DIR
	if(WIN32)
	  set(${proj}_PFX_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_PFX_DIR} 
	    CACHE PATH "On windows, there is a bug with GDCM source code directory path length, you can change this path to avoid it")
	  set(${proj}_SRC_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_BIN_DIR} 
	    CACHE PATH "On windows, there is a bug with GDCM source code directory path length, you can change this path to avoid it")
	  set(${proj}_BLD_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_BLD_DIR}  
	    CACHE PATH "On windows, there is a bug with GDCM source code directory path length, you can change this path to avoid it")
	  set(${proj}_BIN_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_BIN_DIR}  
	    CACHE PATH "On windows, there is a bug with GDCM source code directory path length, you can change this path to avoid it")
	else()
	  set(${proj}_PFX_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_PFX_DIR})
	  set(${proj}_SRC_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_BIN_DIR})
	  set(${proj}_BLD_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_BLD_DIR})
	  set(${proj}_BIN_DIR ${SV_EXTERNALS_TOPLEVEL_DIR}/${SV_EXT_${proj}_BIN_DIR})
	endif()

	if(LINUX)
		set(SuperBuild_${proj}_URL "${SV_SUPERBUILD_LIBS_DIR}/linux/ubuntu/14.04/latest/linux.gcc-4.8.x64.tcltk-8.6.4.tar.gz" CACHE
			STRING "Location of ${proj}, can be web address or local path")
	elseif(APPLE)
		set(SuperBuild_${proj}_URL "${SV_SUPERBUILD_LIBS_DIR}/mac_osx/10.10/latest/mac_osx.clang-7.0.x64.tcltk-8.6.4.tar.gz" CACHE
			STRING "Location of ${proj}, can be web address or local path")
	elseif(WIN32)
		set(SuperBuild_${proj}_URL "${SV_SUPERBUILD_LIBS_DIR}/windows/msvc_2013/latest/msvc_2013.x64.tcltk-8.6.4.zip" CACHE
			STRING "Location of ${proj}, can be web address or local path")
	endif()
	mark_as_superbuild(SuperBuild_${proj}_URL:STRING)
	mark_as_advanced(SuperBuild_${proj}_URL)
	
	ExternalProject_Add(${proj}
		URL ${SuperBuild_${proj}_URL}
		PREFIX ${${proj}_PFX_DIR}
		SOURCE_DIR ${${proj}_SRC_DIR}
		BINARY_DIR ${${proj}_BIN_DIR}
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
		set(TCL_DLL_PATH ${${proj}_BIN_DIR}/bin)
		set(TCL_INCLUDE_PATH ${${proj}_BIN_DIR}/include)
		set(TCL_LIBRARY ${${proj}_BIN_DIR}/lib/tcl86t.lib)
		set(TCL_TCLSH ${${proj}_BIN_DIR}/bin/tclsh86t.exe)

		set(TK_INCLUDE_PATH ${${proj}_BIN_DIR}/include)
		set(TK_LIBRARY ${${proj}_BIN_DIR}/lib/tk86t.lib)
		set(TK_WISH ${${proj}_BIN_DIR}/bin/wish86t.exe)
	endif()
	if(LINUX)
		set(TCL_DLL_PATH ${${proj}_BIN_DIR}/bin)
		set(TCL_INCLUDE_PATH ${${proj}_BIN_DIR}/include)
		set(TCL_LIBRARY ${${proj}_BIN_DIR}/lib/libtcl8.6.so)
		set(TCL_TCLSH ${${proj}_BIN_DIR}/bin/tclsh8.6)

		set(TK_INCLUDE_PATH ${${proj}_BIN_DIR}/include)
		set(TK_LIBRARY ${${proj}_BIN_DIR}/lib/libtk8.6.so)
		set(TK_WISH ${${proj}_BIN_DIR}/bin/wish8.6)
		mark_as_superbuild(TCL_INIT_PATH:PATH)
	endif()
	if(APPLE)
		#set(Tcl_Framework_Dir ${${proj}_BIN_DIR}/Library/Frameworks/Tcl.framework)
		#set(Tk_Framework_Dir ${${proj}_BIN_DIR}/Library/Frameworks/Tk.framework)

		set(TCL_INCLUDE_PATH ${${proj}_BIN_DIR}/include)
		set(TCL_LIBRARY ${${proj}_BIN_DIR}/lib/libtcl8.6.dylib)
		set(TCL_TCLSH ${${proj}_BIN_DIR}/bin/tclsh8.6)

		set(TK_INCLUDE_PATH ${${proj}_BIN_DIR}/include)
		set(TK_LIBRARY ${${proj}_BIN_DIR}/lib/libtk8.6.dylib)
		set(TK_WISH ${${proj}_BIN_DIR}/bin/wish8.6)
		mark_as_superbuild(TCL_INIT_PATH:PATH)
	endif()
	set(${proj}_SOURCE_DIR ${${proj}_SRC_DIR})
        set(SV_${proj}_DIR ${${proj}_BIN_DIR})

else()
  # Sanity checks
  if(DEFINED SV_TCL_DIR AND NOT EXISTS ${SV_TCL_DIR})
    message(FATAL_ERROR "SV_TCL_DIR variable is defined but corresponds to non-existing directory")
  endif()

  ExternalProject_Add_Empty(${proj} DEPENDS ${${proj}_DEPENDENCIES})
endif()
if(SV_INSTALL_EXTERNALS)
  ExternalProject_Install_CMake(${proj})
endif()

mark_as_superbuild(${proj}_SOURCE_DIR:PATH)
mark_as_superbuild(SV_${proj}_DIR:PATH)
mark_as_superbuild(TCL_DLL_PATH:PATH)
mark_as_superbuild(TCL_INCLUDE_PATH:PATH)
mark_as_superbuild(TCL_LIBRARY:PATH)
mark_as_superbuild(TCL_TCLSH:PATH)
mark_as_superbuild(TK_INCLUDE_PATH:PATH)
mark_as_superbuild(TK_LIBRARY:PATH)
mark_as_superbuild(TK_WISH:PATH)

mark_as_superbuild(
          VARS SV_${proj}_DIR:PATH
	  LABELS "FIND_PACKAGE"
	  )

