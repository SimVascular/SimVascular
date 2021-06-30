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

option(SV_OPEN_TERMINAL "Open terminal when launching SimVascular" ON)
option(SV_CREATE_DESKTOP_LAUNCHER "Create desktop launcher" OFF)

if(LINUX)
   IF("${SV_PLATFORM_DIR}" MATCHES "centos")
     set(_sv_installer_type_default TGZ)
   else()
     set(_sv_installer_type_default DEB)
   endif()
   set(SV_INSTALLER_TYPE ${_sv_installer_type_default} CACHE STRING "When distributing, specify the installer package type.")
   set_property(CACHE SV_INSTALLER_TYPE PROPERTY STRINGS DEB TGZ)
endif()
option(SV_USE_MSMPI "Use MSMPI" OFF)

set(opensource_components CoreExecutables CoreLibraries CoreHeaders CoreScripts CoreFiles
  ThirdPartyExecutables ThirdPartyLibraries ThirdPartyHeaders ExternalExecutables ExternalLibraries
  ExternalHeaders CMake
	Unspecified
    )

set(licensed_components LicensedExecutables LicensedLibraries
	LicensedData LicenseFiles)

set(CPACK_PACKAGE_NAME "SimVascular")
set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "SimVascular - Cardiovascular Modeling and Simulation Application")
set(CPACK_PACKAGE_VENDOR "SimVascular.org")
set(CPACK_PACKAGE_CONTACT "SimVascular<sv@simvascular.org>")
set(CPACK_RESOURCE_FILE_LICENSE "${CMAKE_CURRENT_SOURCE_DIR}/license.txt")
set(CPACK_PACKAGE_VERSION_MAJOR ${SV_MAJOR_VERSION})
set(CPACK_PACKAGE_VERSION_MINOR ${SV_MINOR_VERSION})
set(CPACK_PACKAGE_VERSION_PATCH ${SV_PATCH_VERSION})
set(INSTALL_TYPE_STR)
string(REPLACE "." "-" SV_INSTALL_VERSION_DIR ${SV_FULL_VERSION})

if(SV_DISTRIBUTE_COMPONENTS STREQUAL OpenSource)
	set(CPACK_COMPONENTS_ALL ${opensource_components})
elseif(SV_DISTRIBUTE_COMPONENTS STREQUAL Licensed)
    set(CPACK_COMPONENTS_ALL ${licensed_components})
    set(CPACK_PACKAGE_NAME "svLicensedModules")
    set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "SimVascular Licensed Modules")
endif()

if(Licensed_PARASOLID)
	set(INSTALL_TYPE_STR "${INSTALL_TYPE_STR}-parasolid")
endif()

set(CPACK_INSTALL_CMAKE_PROJECTS "${CPACK_INSTALL_CMAKE_PROJECTS};${${PROJECT_NAME}_BINARY_DIR};SimVascular;ALL;/")
#-----------------------------------------------------------------------------
message(STATUS "CPACK_COMPONENTS_ALL: ${CPACK_COMPONENTS_ALL}")
#-----------------------------------------------------------------------------
# LINUX setup
if(LINUX)
	if(SV_DISTRIBUTE_COMPONENTS STREQUAL Licensed)
		if(SV_INSTALLER_TYPE STREQUAL DEB)
			set(CPACK_GENERATOR "DEB")
			set(CPACK_PACKAGING_INSTALL_PREFIX "/usr/local/sv/licensed/${SV_INSTALL_VERSION_DIR}")
			set(CPACK_DEB_COMPONENT_INSTALL ON)
			set(CPACK_COMPONENTS_GROUPING "ALL_COMPONENTS_IN_ONE")
			set(SV_CREATE_DESKTOP_LAUNCHER OFF)
		elseif(SV_INSTALLER_TYPE STREQUAL TGZ)
			set(CPACK_GENERATOR "TGZ")
			set(CPACK_INCLUDE_TOPLEVEL_DIRECTORY ON)
			set(CPACK_ARCHIVE_COMPONENT_INSTALL ON)
			set(CPACK_COMPONENTS_GROUPING "ALL_COMPONENTS_IN_ONE")
		endif()
	elseif(SV_DISTRIBUTE_COMPONENTS STREQUAL OpenSource)
		if(SV_INSTALLER_TYPE STREQUAL DEB)
			set(CPACK_GENERATOR "DEB")
			set(CPACK_PACKAGING_INSTALL_PREFIX "/usr/local/sv/simvascular/${SV_INSTALL_VERSION_DIR}")
			set(CPACK_DEB_COMPONENT_INSTALL ON)
			set(CPACK_COMPONENTS_GROUPING "ALL_COMPONENTS_IN_ONE")
			set(SV_CREATE_DESKTOP_LAUNCHER ON)
		elseif(SV_INSTALLER_TYPE STREQUAL TGZ)
			set(CPACK_GENERATOR "TGZ")
			set(CPACK_INCLUDE_TOPLEVEL_DIRECTORY ON)
			set(CPACK_ARCHIVE_COMPONENT_INSTALL ON)
			set(CPACK_COMPONENTS_GROUPING "ALL_COMPONENTS_IN_ONE")
		endif()
	else()
		if(SV_INSTALLER_TYPE STREQUAL DEB)
			set(CPACK_GENERATOR "DEB")
			set(CPACK_PACKAGING_INSTALL_PREFIX "/usr/local/sv/simvascular/${SV_INSTALL_VERSION_DIR}")
		elseif(SV_INSTALLER_TYPE STREQUAL TGZ)
			set(CPACK_GENERATOR "TGZ")
			set(CPACK_INCLUDE_TOPLEVEL_DIRECTORY ON)
		endif()
	endif()

	if( (SV_INSTALLER_TYPE STREQUAL DEB) AND SV_CREATE_DESKTOP_LAUNCHER AND (SV_DISTRIBUTE_COMPONENTS STREQUAL OpenSource OR SV_DISTRIBUTE_COMPONENTS STREQUAL All) )
		INSTALL (FILES ${CMAKE_CURRENT_SOURCE_DIR}/icons/svlogo.png DESTINATION "/usr/local/sv/simvascular/${SV_INSTALL_VERSION_DIR}")

		set(SV_FULL_LAUNCH_FILE "/usr/local/sv/simvascular/${SV_INSTALL_VERSION_DIR}/simvascular")
		set(SV_FULL_ICON_FILE "/usr/local/sv/simvascular/${SV_INSTALL_VERSION_DIR}/svlogo.png")
		set(TERMINAL_FLAG "false")
		if(SV_OPEN_TERMINAL)
			set(TERMINAL_FLAG "true")
		endif()

		configure_file(${CMAKE_CURRENT_SOURCE_DIR}/linux/simvascular.desktop.in
						${CMAKE_CURRENT_BINARY_DIR}/linux/simvascular.desktop @ONLY)

		SET(SV_DESKTOP_DIR "/usr/share/applications/")

		INSTALL (FILES ${CMAKE_CURRENT_BINARY_DIR}/linux/simvascular.desktop DESTINATION ${SV_DESKTOP_DIR})

	endif()

	SET(CPACK_PACKAGE_FILE_NAME "${CPACK_PACKAGE_NAME}-linux-x64-${SV_FULL_VERSION}${INSTALL_TYPE_STR}")

        # we always use DEB on Ubuntu, but CentOS we need to use TGZ
	if(SV_INSTALLER_TYPE STREQUAL DEB)
	  #code needed to put qt.conf files in install directory/install package
	  INSTALL(FILES ${CMAKE_CURRENT_SOURCE_DIR}/qt.conf DESTINATION "/usr/local/sv/simvascular/${SV_INSTALL_VERSION_DIR}/bin")
	  INSTALL(FILES ${CMAKE_CURRENT_SOURCE_DIR}/qt_ext.conf DESTINATION "/usr/local/sv/simvascular/${SV_INSTALL_VERSION_DIR}/svExternals/bin" RENAME "qt.conf")
	  INSTALL(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../Python/site-packages" DESTINATION "/usr/local/sv/simvascular/${SV_INSTALL_VERSION_DIR}/Python${PYTHON_MAJOR_VERSION}.${PYTHON_MINOR_VERSION}")
	elseif(SV_INSTALLER_TYPE STREQUAL TGZ)
	  #code needed to put qt.conf files in install directory/install package
	  INSTALL(FILES ${CMAKE_CURRENT_SOURCE_DIR}/qt.conf DESTINATION "bin")
	  INSTALL(FILES ${CMAKE_CURRENT_SOURCE_DIR}/qt_ext.conf DESTINATION "svExternals/bin" RENAME "qt.conf")
	  INSTALL(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../Python/site-packages" DESTINATION "Python${PYTHON_MAJOR_VERSION}.${PYTHON_MINOR_VERSION}")
	endif()

endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# APPLE setup
if(APPLE)
	set(CPACK_PACKAGE_FILE_NAME "${CPACK_PACKAGE_NAME}-darwin-x64-${SV_FULL_VERSION}${INSTALL_TYPE_STR}")

	if(SV_DISTRIBUTE_COMPONENTS STREQUAL Licensed)
		set(CPACK_GENERATOR "productbuild")
		set(CPACK_PACKAGING_INSTALL_PREFIX "/usr/local/sv/licensed/${SV_INSTALL_VERSION_DIR}")
	else()
		# Use the bundle-generator on osx ...
		set(CPACK_GENERATOR "Bundle")
		set(CPACK_BUNDLE_ICON "${CMAKE_CURRENT_SOURCE_DIR}/OSX/SimVascular.icns")
		if(SV_RELEASE_TYPE MATCHES "^Release$")
	    	set(CPACK_BUNDLE_NAME "SimVascular")
		elseif(SV_RELEASE_TYPE MATCHES "^Beta$")
			set(CPACK_BUNDLE_NAME "SimVascular-Beta")
		endif()

		# component-based distribution doesn't work for bundle; instead, control which to install before cpack

		configure_file(${CMAKE_CURRENT_SOURCE_DIR}/OSX/Info.plist.in
			${CMAKE_CURRENT_BINARY_DIR}/OSX/Info.plist @ONLY)

		set(CPACK_BUNDLE_PLIST "${CMAKE_CURRENT_BINARY_DIR}/OSX/Info.plist")
		set(CPACK_BUNDLE_STARTUP_COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/OSX/simvascular.sh")

		if(SV_DEVELOPER_OUTPUT)
			message(STATUS "CPACK_PACKAGE_FILE_NAME: ${CPACK_PACKAGE_FILE_NAME}.dmg")
		endif()

                INSTALL(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/../Python/site-packages" 
                        DESTINATION "Python${PYTHON_MAJOR_VERSION}.${PYTHON_MINOR_VERSION}")
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

		set(CPACK_PACKAGE_FILE_NAME "sv-windows-x64-${SV_FULL_VERSION}${INSTALL_TYPE_STR}")
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
