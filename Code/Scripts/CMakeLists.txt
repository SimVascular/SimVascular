include(${CMAKE_CURRENT_SOURCE_DIR}/SimVascularScriptMacros.cmake)
file(MAKE_DIRECTORY ${TEMP_DIR}/developer ${TEMP_DIR}/install)

set(core_str)
if(UNIX)
	file(READ ${SV_SOURCE_DIR}/Scripts/home-locate-linux.sh locate_script)
	set(core_str "${locate_script}\n")
	set_env_string_concat(SV_HOME "$DIR" core_str)
endif()
if(WIN32 AND NOT UNIX)
	set(core_str "@echo off\r\n")
	set_env_string_concat(SV_HOME "%~dp0" core_str)
endif()

set(core_vars SV_OS SOFTWARE_PLATFORM COMPILER_VERSION
	SV_VERSION OPEN_SOFTWARE_BINARIES_TOPLEVEL
	LICENSED_SOFTWARE_TOPLEVEL SV_STATIC_BUILD)

foreach(var ${core_vars})
	set_env_string_concat(${var} "\@${var}\@" core_str)
	string(CONFIGURE "${core_str}" core_str @ONLY)
endforeach()

set(SCRIPT_FILES ${HOME_FILE_STRING})
if(APPLE)
	set(WIN_PATH )
	set(WIN_EXE )
	set(WIN_BAT )
	set(OSTYPE macos)
endif()

if(UNIX AND NOT APPLE)
	set(WIN_PATH )
	set(WIN_EXE )
	set(WIN_BAT )
	set(OSTYPE linux)
endif()

#Generate output scripts
if(WIN32 AND NOT CYGWIN)
	set(WIN_EXE ".exe")
	set(WIN_BAT ".bat")
	set(OSTYPE windows)
endif()

set(developer_script_string)
set(install_script_string)
set(FRAGMENT_INJECTION_STRING)
#---
# Write extra FILES
env_variable_to_value_variable(ENV_SV_HOME SV_HOME)

#----------------------------------------------------------------------------
# Find external licensed module dir
#----------------------------------------------------------------------------
if(UNIX AND SV_ENABLE_DISTRIBUTION AND SV_DISTRIBUTE_COMPONENTS STREQUAL OpenSource)
	if(SV_USE_PARASOLID OR SV_USE_MESHSIM)
		file(READ ${SV_SOURCE_DIR}/Scripts/licensed-home-locate-unix.sh licensed_locate_script)
		set(install_script_string "${install_script_string}${licensed_locate_script}\n")

		set(install_script_string "${install_script_string}# Set env variables for licensed modules\n")
		set(install_script_string "${install_script_string}if ! [ -z \"$licdir\" ]; then\n")
		append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "$licdir/lib" install_script_string)
		if(SV_USE_PARASOLID)
			set_env_string_concat(P_SCHEMA "$licdir/schema" install_script_string)
		endif()
		if(SV_USE_MESHSIM)
			set_env_string_concat(SIM_LICENSE_FILE "$licdir/meshsim-license.dat" install_script_string)
		endif()
		set(install_script_string "${install_script_string}fi\n\n")
	endif()
endif()

#----------------------------------------------------------------------------
# PARASOLID Install Rules
#----------------------------------------------------------------------------
if(SV_USE_PARASOLID)
	#Parasolid as a few special directories to add
	set_env_string_concat(P_SCHEMA "${PARASOLID_SCHEMA_DIR}" developer_script_string)
	append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${PARASOLID_DIR}/shared_object" developer_script_string)

	if(NOT SV_ENABLE_DISTRIBUTION OR SV_DISTRIBUTE_COMPONENTS STREQUAL All)
		set_env_string_concat(P_SCHEMA "${ENV_SV_HOME}/${SV_INSTALL_NATIVE_PARASOLID_SCHEMA_DIR}" install_script_string)
	endif()
        set(FRAGMENT_INJECTION_STRING "${FRAGMENT_INJECTION_STRING}
		<Registry Id='regid.parasolid' Root='HKLM'
		Key='Software\\SimVascular\\@SV_VERSION@ @SV_MAJOR_VERSION@'
		Name='PSchemaDir' Action='write' Type='string'
		Value='[INSTALL_ROOT]@SV_INSTALL_NATIVE_PARASOLID_SCHEMA_DIR@' />")
endif()

#----------------------------------------------------------------------------
# MESHSIM Install Rules
#----------------------------------------------------------------------------
if(SV_USE_MESHSIM)
	if(NOT MESHSIM_LICENSE_IN_WIN32_REGISTRY)
		if(MESHSIM_LICENSE_FILE)
			set_env_string_concat(SIM_LICENSE_FILE "${MESHSIM_LICENSE_FILE}" developer_script_string)
		else()
			set_env_string_concat(SIM_LICENSE_FILE "${ENV_SV_HOME}/meshsim-license.dat" developer_script_string)
		endif()
		if(NOT SV_ENABLE_DISTRIBUTION OR SV_DISTRIBUTE_COMPONENTS STREQUAL All)
			set_env_string_concat(SIM_LICENSE_FILE "${ENV_SV_HOME}/meshsim-license.dat" install_script_string)
		endif()
	endif()
endif()

#----------------------------------------------------------------------------
# Opensource Externals Install Rules
if(WIN32)
  set(developer_script_string "${developer_script_string}REM Externals link directories\n")
  set(install_script_string "${install_script_string}REM Externals link directories\n")
else()
  set(developer_script_string "${developer_script_string}# Externals link directories\n")
  set(install_script_string "${install_script_string}# Externals link directories\n")
endif()

# Add Qt if it was used
if(SV_USE_QT OR SV_USE_QT_GUI)
  if(QT_DLL_PATH)
    append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} ${QT_DLL_PATH} developer_script_string)
    set_env_string_concat(QT_PLUGIN_PATH ${QT_PLUGIN_PATH} developer_script_string)
    if(WIN32)
      set(developer_script_string "${developer_script_string}REM Plugins Path for Qt GUI\n")
      set(install_script_string "${install_script_string}REM Plugins Path for Qt GUI\n")
    else()
      set(developer_script_string "${developer_script_string}# Plugins Path for Qt GUI\n")
      set(install_script_string "${install_script_string}# Plugins Path for Qt GUI\n")
    endif()
    if(WIN32)
      set_env_string_concat(SV_PLUGIN_PATH "${MITK_DIR}/bin/RelWithDebInfo" developer_script_string)
      append_env_string_concat(SV_PLUGIN_PATH "${MITK_DIR}/bin/plugins/RelWithDebInfo" developer_script_string)
      append_env_string_concat(SV_PLUGIN_PATH "%SV_HOME%/bin/plugins" developer_script_string)
    endif()
  endif()
endif()

# Add Tcl path for win32
if(WIN32)
  append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${TCL_DLL_PATH}" developer_script_string)
endif()

foreach(proj ${SV_EXTERNAL_SHARED_LIBS})
  dev_message("Install ${proj}")
  dev_message("SV_${lib}_DIR: ${SV_${proj}_DIR}")
	# For Each library that we have linked against
	if(${proj}_DLL_PATH)
	  dev_message("${proj}_DLL_PATH: ${${proj}_DLL_PATH}")
		# If there is a DLL path associated with it, go ahead and add it to the scripts
		append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${${proj}_DLL_PATH}" developer_script_string)
	endif()

	# on unix systems we only need this path if its installed (moved)
	# if this library was not specified USE_SYSTEM, it will need to be installed, and the path updated.
	if(SV_INSTALL_${proj}_LIBRARY_DIR)
  #if there is an install library directory, add it to the path for installation, it will be installed there.
    if(SV_${proj}_DIR)
      append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${SV_${proj}_DIR}/lib" developer_script_string)
    endif()
		if(NOT SV_ENABLE_DISTRIBUTION)
      append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SV_HOME}/${SV_INSTALL_${proj}_LIBRARY_DIR}" install_script_string)
		endif()
	endif()
	if(SV_INSTALL_${proj}_RUNTIME_DIR)
    # if there is an install runtime directory, add it to the path for installation
    if(SV_${proj}_DIR)
      append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${SV_${proj}_DIR}/bin" developer_script_string)
    endif()
		if(NOT SV_ENABLE_DISTRIBUTION)
		  append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SV_HOME}/${SV_INSTALL_${proj}_RUNTIME_DIR}" install_script_string)
		endif()
	endif()
endforeach()

# Add MITK if it was used (not sure why this isn't being added already)
if (SV_USE_MITK)
  if (MITK_DIR)
    #append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${MITK_DIR}/bin" developer_script_string)
    append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${MITK_DIR}/lib/plugins" developer_script_string)
  endif()
endif()

if(SV_ENABLE_DISTRIBUTION)
  append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SV_HOME}/${SV_EXTERNALS_INSTALL_PREFIX}/lib" install_script_string)
  append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SV_HOME}/${SV_EXTERNALS_INSTALL_PREFIX}/lib/plugins" install_script_string)
endif()
#----------------------------------------------------------------------------

#----------------------------------------------------------------------------
# TCL Install Rules
if(WIN32)
  set(developer_script_string "${developer_script_string}\nREM Tcl environment variables\n")
  set(install_script_string "${install_script_string}\nREM Tcl environment variables\n")
else()
  set(developer_script_string "${developer_script_string}\n# Tcl environment variables\n")
  set(install_script_string "${install_script_string}\n# Tcl environment variables\n")
endif()

if(SV_USE_TKCXIMAGE)
	if(TKCXIMAGE_DLL)
		set_env_string_concat(TKCXIMAGE_DLL "${TKCXIMAGE_DLL}" developer_script_string)
	endif()
endif()

set_env_string_concat(TCL_LIBRARY "${TCL_LIBRARY_PATH}/tcl8.6" developer_script_string)
set_env_string_concat(TK_LIBRARY "${TCL_LIBRARY_PATH}/tk8.6" developer_script_string)
set_env_string_concat(TCL_LIBRARY "${ENV_SV_HOME}/${SV_INSTALL_TCL_LIBRARY_DIR}/tcl8.6" install_script_string)
set_env_string_concat(TK_LIBRARY "${ENV_SV_HOME}/${SV_INSTALL_TCL_LIBRARY_DIR}/tk8.6" install_script_string)

#----------------------------------------------------------------------------
# VTK TCL Install Rules
if(DEFINED ENV{TCLLIBPATH})
  set_env_string_concat(TCLLIBPATH "$ENV{TCLLIBPATH}" developer_script_string)
else()
  if(NOT TCLLIBPATH)
    set_env_string_concat(TCLLIBPATH "${SV_VTK_DIR}/lib/tcltk/vtk-${VTK_MAJOR_VERSION}.${VTK_MINOR_VERSION}" developer_script_string)
  endif()
endif()
set_env_string_concat(TCLLIBPATH "${ENV_SV_HOME}/${SV_INSTALL_VTK_LIBRARY_DIR}/tcltk/vtk-${VTK_MAJOR_VERSION}.${VTK_MINOR_VERSION}" install_script_string)
#----------------------------------------------------------------------------

#----------------------------------------------------------------------------
# PYTHON Install Rules
if(SV_USE_PYTHON)
  if(WIN32)
    set(developer_script_string "${developer_script_string}\nREM Python environment variables\n")
    set(install_script_string "${install_script_string}\nREM Python environment variables\n")
  else()
    set(developer_script_string "${developer_script_string}\n# Python environment variables\n")
    set(install_script_string "${install_script_string}\n# Python environment variables\n")
  endif()
  # PYTHONHOME
  if(SV_PYTHON_DIR)
    set_env_string_concat(PYTHONHOME "${SV_PYTHON_DIR}" developer_script_string)
  endif()
  set_env_string_concat(PYTHONHOME "${ENV_SV_HOME}/${SV_INSTALL_PYTHON_RUNTIME_DIR}/.." install_script_string)

  # PYTHONPATH
  if(SV_PYTHON_DIR)
    set_env_string_concat(PYTHONPATH "${SV_PYTHON_DIR}/lib/python${PYTHON_MAJOR_VERSION}.${PYTHON_MINOR_VERSION}/site-packages" developer_script_string)
  endif()
  append_env_string_concat(PYTHONPATH "${SV_VTK_DIR}/lib/python${PYTHON_MAJOR_VERSION}.${PYTHON_MINOR_VERSION}/site-packages" developer_script_string)
  set_env_string_concat(PYTHONPATH "${ENV_SV_HOME}/${SV_INSTALL_PYTHON_LIBRARY_DIR}/python${PYTHON_MAJOR_VERSION}.${PYTHON_MINOR_VERSION}/site-packages" install_script_string)
  if(NOT SV_ENABLE_DISTRIBUTION)
    append_env_string_concat(PYTHONPATH "${ENV_SV_HOME}/${SV_INSTALL_VTK_LIBRARY_DIR}/python${PYTHON_MAJOR_VERSION}.${PYTHON_MINOR_VERSION}/site-packages" install_script_string)
  endif()

  # Add to PYTHONHOME or PYTHONPATH if SITE_PACAKGES information is given
	if(VTK_PYTHON_SITE_PACKAGES)
	  append_env_string_concat(PYTHONPATH "${VTK_PYTHON_SITE_PACKAGES}" developer_script_string)
  endif()
  if(PYTHON_SITE_PACKAGES)
    append_env_string_concat(PYTHONPATH "${PYTHON_SITE_PACKAGES}" developer_script_string)
    append_env_string_concat(PYTHONPATH "${PYTHON_CORE_PACKAGES}" developer_script_string)
    append_env_string_concat(PATH "${PYTHON_DLL_PATH}" developer_script_string)
  endif()

endif()
#----------------------------------------------------------------------------

#----------------------------------------------------------------------------
# This is a catch all if no DLL_PATH was specified but we will need the so for installed libraries.
if(WIN32)
  set(developer_script_string "${developer_script_string}\nREM SimVascular runtime locations\n")
  set(install_script_string "${install_script_string}\nREM SimVascular runtime locations\n")
else()
  set(developer_script_string "${developer_script_string}\n# SimVascular runtime locations\n")
  set(install_script_string "${install_script_string}\n# SimVascular runtime locations\n")
endif()

append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SV_HOME}/${SV_INSTALL_LIBRARY_DIR}" install_script_string)
append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SV_HOME}/${SV_INSTALL_HOME_DIR}" install_script_string)
append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SV_HOME}/${SV_INSTALL_RUNTIME_DIR}" install_script_string)
append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SV_HOME}/${SV_INSTALL_LIBRARY_DIR}/plugins" install_script_string)
set_env_string_concat(DYLD_FALLBACK_FRAMEWORK_PATH "${ENV_SV_HOME}/${SV_EXTERNALS_INSTALL_PREFIX}/lib" install_script_string)

append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${CMAKE_BINARY_DIR}/bin" developer_script_string)
append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${CMAKE_BINARY_DIR}/lib/plugins" developer_script_string)
append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${CMAKE_BINARY_DIR}/lib" developer_script_string)
append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${CMAKE_BINARY_DIR}/bin/RelWithDebInfo" developer_script_string)
#----------------------------------------------------------------------------

#----------------------------------------------------------------------------
# These are the locations of the simvascular plugins
if(WIN32)
  set(developer_script_string "${developer_script_string}\nREM SimVascular plugin locations\n")
  set(install_script_string "${install_script_string}\nREM SimVascular plugin locations\n")
else()
  set(developer_script_string "${developer_script_string}\n# SimVascular plugin locations\n")
  set(install_script_string "${install_script_string}\n# SimVascular plugin locations\n")
endif()

append_env_string_concat(SV_PLUGIN_PATH "${ENV_SV_HOME}/${SV_INSTALL_LIBRARY_DIR}/plugins" install_script_string)
if(SV_ENABLE_DISTRIBUTION)
  append_env_string_concat(SV_PLUGIN_PATH "${ENV_SV_HOME}/${SV_EXTERNALS_INSTALL_PREFIX}/lib/plugins" install_script_string)
  else()
    append_env_string_concat(SV_PLUGIN_PATH "${ENV_SV_HOME}/${SV_INSTALL_MITK_LIBRARY_DIR}/plugins" install_script_string)
endif()

append_env_string_concat(SV_PLUGIN_PATH "${CMAKE_BINARY_DIR}/lib/plugins" developer_script_string)
if (SV_USE_MITK)
  if (MITK_DIR)
    append_env_string_concat(SV_PLUGIN_PATH "${MITK_DIR}/lib/plugins" developer_script_string)
  endif()
endif()
#----------------------------------------------------------------------------

#----------------------------------------------------------------------------
configure_file(${SV_SOURCE_DIR}/../Distribution/windows/registry-patch-notargets.xml.in "${TEMP_DIR}/patch.xml" @ONLY)
set(PREAMBLE_DEVELOPER_FILE "${TEMP_DIR}/developer-preample")
set(PREAMBLE_INSTALL_FILE "${TEMP_DIR}/install-preample")
set(EXE_FILE_STRING "")

file(WRITE ${PREAMBLE_DEVELOPER_FILE} "${core_str}\n")
file(APPEND ${PREAMBLE_DEVELOPER_FILE} "${developer_script_string}\n")

file(WRITE ${PREAMBLE_INSTALL_FILE} "${core_str}\n")
file(APPEND ${PREAMBLE_INSTALL_FILE} "${install_script_string}\n")

set(SCRIPT_FILES "${TEMP_DIR}/\@mode\@-preample")

# CHANGE FOR EXECUTABLE RENAME REMOVE
set(SCRIPT_FILES ${SCRIPT_FILES} "${SV_SOURCE_DIR}/Scripts/executable/\@mode\@/\@tar\@-\@OSTYPE\@.in")

unset(GDB)
unset(GDB_FLAGS)
set(INSTALL_SYMLINK_FILE "${TEMP_DIR}/setup-symlinks.sh")
file(WRITE ${INSTALL_SYMLINK_FILE} "${locate_script}\n\necho $DIR\n\n")
dev_message("SV_SCRIPT_TARGETS: ${SV_SCRIPT_TARGETS}")
foreach(tar ${SV_SCRIPT_TARGETS})
	simvascular_configure_script(
		TARGET "${tar}"
		FILES ${SCRIPT_FILES}
		INSTALL
		)
	if(WIN32 AND NOT CYGWIN)
		message("${tar}")
		ADD_CUSTOM_TARGET(${tar}-copy ALL
			DEPENDS ${tar}
			COMMAND ${CMAKE_COMMAND} -E remove ${OUTBIN_DIR}/$<TARGET_FILE_NAME:${tar}>
			COMMAND ${CMAKE_COMMAND} -E copy $<TARGET_FILE:${tar}> ${OUTBIN_DIR}
			COMMENT "Copying ${tar} to ${OUTBIN_DIR}"
			)
	endif()
	if(UNIX)
		file(APPEND ${INSTALL_SYMLINK_FILE} "rm -f /usr/local/bin/${${tar}_INSTALL_SCRIPT_NAME}${WIN_BAT}\n")
		file(APPEND ${INSTALL_SYMLINK_FILE} "ln -s $DIR/${${tar}_INSTALL_SCRIPT_NAME}${WIN_BAT} /usr/local/bin/${${tar}_INSTALL_SCRIPT_NAME}${WIN_BAT}\n")
	endif()
endforeach()
if(UNIX)
	install(PROGRAMS ${INSTALL_SYMLINK_FILE} DESTINATION ${SV_INSTALL_SCRIPT_DIR} COMPONENT CoreScripts)
	install(FILES "README-install" DESTINATION ${SV_INSTALL_SCRIPT_DIR} COMPONENT CoreFiles)
endif()
if(BUILD_TESTING)
	configure_file(executable_names.tcl.in ${SV_TEST_DIR}/common/executable_names.tcl)
endif()
#----------------------------------------------------------------------------
