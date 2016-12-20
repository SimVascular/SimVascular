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

#-----------------------------------------------------------------------------
# Initial cpack setup
if(WIN32)
	find_path(INTEL_REDIST_DIR names fmpich2.dll PATHS $ENV{PATH})
	file(GLOB INTEL_REDIST_DLLS "${INTEL_REDIST_DIR}/*.dll")
	set(CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS} ${INTEL_REDIST_DLLS})
	set(RM_DLL "fmpich2.dll;fmpich2g.dll;mpich2mpi.dll")
	set(RM_DLL_FULL)
	foreach(dll ${RM_DLL})
		sv_list_match(tmp ${dll} ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS})
		list(APPEND RM_DLL_FULL ${tmp})
	endforeach()
	list(REMOVE_ITEM CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS ${RM_DLL_FULL})
endif()

install(PROGRAMS ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS}
	DESTINATION ${SV_INSTALL_RUNTIME_DIR}
	COMPONENT SystemRuntimes)

include(InstallRequiredSystemLibraries)
include(CPackComponent)

set(CPACK_PACKAGE_NAME "SimVascular")
set(CPACK_PACKAGE_VENDOR "SimVascular.org")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "SimVascular - Cardiovascular Modeling and Simulation Application")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/license.txt")
set(CPACK_PACKAGE_VERSION_MAJOR ${SV_MAJOR_VERSION})
set(CPACK_PACKAGE_VERSION_MINOR ${SV_MINOR_VERSION})
set(CPACK_PACKAGE_VERSION_PATCH ${SV_PATCH_VERSION})
set(INSTALL_TYPE_STR)

if(Licensed_PARASOLID)
	set(INSTALL_TYPE_STR "${INSTALL_TYPE_STR}-parasolid")
endif()

set(CPACK_INSTALL_CMAKE_PROJECTS "${CPACK_INSTALL_CMAKE_PROJECTS};${${PROJECT_NAME}_BINARY_DIR};SimVascular;ALL;/")
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# LINUX setup
if(LINUX)
	SET(CPACK_GENERATOR "TGZ")
	set(CPACK_INCLUDE_TOPLEVEL_DIRECTORY ON)
	SET(CPACK_PACKAGE_FILE_NAME "simvascular-${SV_FULL_VERSION}-Linux-x64${INSTALL_TYPE_STR}")
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# APPLE setup
if(APPLE)
	# Use the bundle-generator on osx ...
	set(CPACK_GENERATOR "Bundle")
	set(CPACK_BUNDLE_ICON "${CMAKE_CURRENT_SOURCE_DIR}/OSX/SimVascular.icns")
	if(SV_RELEASE_TYPE MATCHES "^Release$")
		set(CPACK_BUNDLE_NAME "SimVascular")
	elseif(SV_RELEASE_TYPE MATCHES "^Beta$")
		set(CPACK_BUNDLE_NAME "SimVascular-Beta")
	endif()

	configure_file(${CMAKE_CURRENT_SOURCE_DIR}/OSX/Info.plist.in
		${CMAKE_CURRENT_BINARY_DIR}/OSX/Info.plist @ONLY)

	set(CPACK_BUNDLE_PLIST "${CMAKE_CURRENT_BINARY_DIR}/OSX/Info.plist")
	set(CPACK_BUNDLE_STARTUP_COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/OSX/simvascular.sh")
	set(CPACK_PACKAGE_FILE_NAME "simvascular-${SV_FULL_VERSION}-darwin-x86_64${INSTALL_TYPE_STR}")

	if(SV_DEVELOPER_OUTPUT)
		message(STATUS "CPACK_PACKAGE_FILE_NAME: ${CPACK_PACKAGE_FILE_NAME}.dmg")
	endif()
endif(APPLE)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# WINDOWS
if(WIN32)
	set(CPACK_MONOLITHIC_INSTALL 1)
	set(CPACK_COMPONENTS_IGNORE_GROUPS 1)
	find_path(INTEL_REDIST_DIR names fmpich2.dll PATHS $ENV{PATH})
	file(GLOB INTEL_REDIST_DLLS "${INTEL_REDIST_DIR}/*.dll")
	set(CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS ${CMAKE_INSTALL_SYSTEM_RUNTIME_LIBS} ${INTEL_REDIST_DLLS})
	if(MSVC)
		set(CPACK_GENERATOR "WIX")
		set(CPACK_WIX_EXTENSIONS WixUtilExtension)
		set(CPACK_WIX_UI_BANNER ${CMAKE_CURRENT_SOURCE_DIR}/windows/msi-banner.jpg)
		set(CPACK_WIX_UI_DIALOG ${CMAKE_CURRENT_SOURCE_DIR}/windows/msi-dialog.jpg)
		if(SV_RELEASE_TYPE MATCHES "Release")
			set(CPACK_PACKAGE_INSTALL_DIRECTORY "SimVascular/Release")
			set(CPACK_WIX_PROGRAM_MENU_FOLDER "SimVascular")
			set(CPACK_WIX_PRODUCT_GUID "39068F8A-D4E9-405F-B22B-9B417849AEE6")
			set(CPACK_WIX_UPGRADE_GUID "2359EF69-C03F-401C-9BC3-C19B10297E3B")
			set(PACKAGE_NAME "simvascular")
		elseif(SV_RELEASE_TYPE MATCHES "Beta")
			set(CPACK_WIX_PROGRAM_MENU_FOLDER "SimVascular-Beta")
			set(PACKAGE_NAME "simvascular-beta")
			set(CPACK_PACKAGE_INSTALL_DIRECTORY "SimVascular/${TIMESTAMP}")
			set(CPACK_WIX_PRODUCT_GUID "9453BB7E-CD7E-46ED-AA9E-5514B56FC901")
			set(CPACK_WIX_UPGRADE_GUID "39C845FF-68E1-4AA7-B329-3407B47B3361")
		endif()

		set(CPACK_PACKAGE_FILE_NAME "${PACKAGE_NAME}-${SV_FULL_VERSION}-Win-x64${INSTALL_TYPE_STR}")
		set(CPACK_WIX_PATCH_FILE "${TEMP_DIR}/patch.xml")
		set(CPACK_WIX_PRODUCT_ICON "${CMAKE_CURRENT_SOURCE_DIR}/windows/SimVascular.ico")
		configure_file("${CMAKE_CURRENT_SOURCE_DIR}/windows/mywix.template-notargets.in" "${CMAKE_CURRENT_BINARY_DIR}/mywix.template")
		set(CPACK_WIX_TEMPLATE "${CMAKE_CURRENT_BINARY_DIR}/mywix.template")
	endif()
endif()
#-----------------------------------------------------------------------------

if(CPACK_PACKAGE_FILENAME_TAG)
	SET(CPACK_PACKAGE_FILE_NAME "${CPACK_PACKAGE_FILE_NAME}-${CPACK_PACKAGE_FILENAME_TAG}")
endif()
message(STATUS "Packname name: ${CPACK_PACKAGE_FILE_NAME}")
include(CPack)
