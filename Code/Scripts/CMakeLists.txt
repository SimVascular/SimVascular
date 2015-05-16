include(${CMAKE_CURRENT_SOURCE_DIR}/SimVascularScriptMacros.cmake)
file(MAKE_DIRECTORY ${TEMP_DIR}/developer ${TEMP_DIR}/install)

set(core_str)
if(UNIX)
	file(READ ${SimVascular_SOURCE_DIR}/Scripts/home-locate-linux.sh locate_script)
	set(core_str "${locate_script}\n")
	set_env_string_concat(SIMVASCULAR_HOME "$DIR" core_str)
endif()
if(WIN32 AND NOT UNIX)
	set(core_str "@echo off\r\n")
	set_env_string_concat(SIMVASCULAR_HOME "%~dp0" core_str)
endif()

set(core_vars 	SIMVASCULAR_OS SOFTWARE_PLATFORM COMPILER_VERSION 
	SIMVASCULAR_VERSION OPEN_SOFTWARE_BINARIES_TOPLEVEL 
	LICENSED_SOFTWARE_TOPLEVEL SIMVASCULAR_STATIC_BUILD)


foreach(var ${core_vars})
	set_env_string_concat(${var} "\@${var}\@" core_str)
	string(CONFIGURE "${core_str}" core_str @ONLY)
endforeach()


set(SCRIPT_FILES ${HOME_FILE_STRING})
if(UNIX)
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
env_variable_to_value_variable(ENV_SIMVASCULAR_HOME SIMVASCULAR_HOME)

if(Licensed_PARASOLID)
	#Parasolid as a few special directories to add
	set_env_string_concat(P_SCHEMA "${PARASOLID_SCHEMA_DIR}" developer_script_string)
	set_env_string(P_SCHEMA "${ENV_SIMVASCULAR_HOME}/${SIMVASCULAR_INSTALL_NATIVE_PARASOLID_SCHEMA_DIR}" install_script_string)
	set(FRAGMENT_INJECTION_STRING "${FRAGMENT_INJECTION_STRING}
		<Registry Id='regid.parasolid' Root='HKLM' 
		Key='Software\\SimVascular\\@SIMVASCULAR_VERSION@ @SIMVASCULAR_MAJOR_VERSION@' 
		Name='PSchemaDir' Action='write' Type='string' 
		Value='[INSTALL_ROOT]@SIMVASCULAR_INSTALL_NATIVE_PARASOLID_SCHEMA_DIR@' />")
endif()

if(Licensed_MESHSIM)
	if(NOT MESHSIM_LICENSE_IN_WIN32_REGISTRY)
		if(MESHSIM_LICENSE_FILE)
			set_env_string_concat(SIM_LICENSE_FILE "${MESHSIM_LICENSE_FILE}" developer_script_string)
		else()
			set_env_string_concat(SIM_LICENSE_FILE "meshsim-license.dat" developer_script_string)
		endif()
		set_env_string_concat(SIM_LICENSE_FILE "meshsim-license.dat" install_script_string)
	endif()
endif()

foreach(lib ${SIMVASCULAR_EXTERNAL_SHARED_LIBS})
	# For Each library that we have linked against
	if(${lib}_DLL_PATH)
		# If there is a DLL path associated with it, go ahead and add it to the scripts
		append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${${lib}_DLL_PATH}" developer_script_string)
	endif()
	dev_message("SimVascular_USE_SYSTEM_${lib} ${SimVascular_USE_SYSTEM_${lib}}")
	# on unix systems we only need this path if its installed (moved)
	if(SimVascular_ENABLE_DISTRIBUTION OR NOT SimVascular_USE_SYSTEM_${lib})
		# if this library was not specified USE_SYSTEM, it will need to be installed, and the path updated. 
		if(SIMVASCULAR_INSTALL_${lib}_LIBRARY_DIR)
			#if there is an install library directory, add it to the path for installation, it will be installed there.
			append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SIMVASCULAR_HOME}/${SIMVASCULAR_INSTALL_${lib}_LIBRARY_DIR}" install_script_string)
		endif()
		if(SIMVASCULAR_INSTALL_${lib}_RUNTIME_DIR)
			# if there is an install runtime directory, add it to the path for installation
			# set(FRAGMENT_INJECTION_STRING "${FRAGMENT_INJECTION_STRING}
				# 	<Environment Id='UpdatePath.runtime.${lib}' Name='PATH'
				# 		Value='[INSTALL_ROOT]\\${SIMVASCULAR_INSTALL_${lib}_RUNTIME_DIR}' 
				# 		Permanent='yes' Part='last' Action='set' System='yes' />")
append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SIMVASCULAR_HOME}/${SIMVASCULAR_INSTALL_${lib}_RUNTIME_DIR}" install_script_string)
endif()
else()
	if(${lib}_DLL_PATH)
		append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${${lib}_DLL_PATH}" install_script_string)
	endif()
endif()
endforeach()
# This is a catch all if no DLL_PATH was specified but we will need the so for installed libraries.
append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SIMVASCULAR_HOME}/${SIMVASCULAR_INSTALL_EXTERNALS_RUNTIME_DIR}" install_script_string)
append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SIMVASCULAR_HOME}/${SIMVASCULAR_INSTALL_HOME_DIR}" install_script_string)
append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SIMVASCULAR_HOME}/${SIMVASCULAR_INSTALL_RUNTIME_DIR}" install_script_string)
# set(FRAGMENT_INJECTION_STRING "${FRAGMENT_INJECTION_STRING}
	# 				<Environment Id='UpdatePath.runtime.${lib}' Name='PATH' 
	# 					Value='[INSTALL_ROOT]\\@SIMVASCULAR_INSTALL_EXTERNALS_RUNTIME_DIR@' 
	# 					Permanent='yes' Part='last' Action='set' System='yes' />")

if(SimVascular_USE_TKCXIMAGE)
	if(TKCXIMAGE_DLL)
		set_env_string_concat(TKCXIMAGE_DLL "${TKCXIMAGE_DLL}" developer_script_string)
	endif()
endif()

if(NOT SimVascular_USE_SYSTEM_TCL OR SIMVASCULAR_INSTALL_SYSTEM_LIBS)
	#Lib additions
	set_env_string_concat(TCL_LIBRARY "${TCL_LIBRARY_PATH}/tcl8.5" developer_script_string)
	set_env_string_concat(TK_LIBRARY "${TK_LIBRARY_PATH}/tk8.5" developer_script_string)
	set_env_string_concat(TCL_LIBRARY "${ENV_SIMVASCULAR_HOME}/${SIMVASCULAR_INSTALL_TCL_LIBRARY_DIR}/tcl8.5" install_script_string)
	set_env_string_concat(TK_LIBRARY "${ENV_SIMVASCULAR_HOME}/${SIMVASCULAR_INSTALL_TCL_LIBRARY_DIR}/tk8.5" install_script_string)
endif()

configure_file(${SimVascular_SOURCE_DIR}/../Distribution/windows/registry-patch-notargets.xml.in "${TEMP_DIR}/patch.xml" @ONLY)
set(PREAMBLE_DEVELOPER_FILE "${TEMP_DIR}/developer-preample")
set(PREAMBLE_INSTALL_FILE "${TEMP_DIR}/install-preample")
set(EXE_FILE_STRING "")

file(WRITE ${PREAMBLE_DEVELOPER_FILE} "${core_str}\n")
file(APPEND ${PREAMBLE_DEVELOPER_FILE} "${developer_script_string}\n")

file(WRITE ${PREAMBLE_INSTALL_FILE} "${core_str}\n")
file(APPEND ${PREAMBLE_INSTALL_FILE} "${install_script_string}\n")

set(SCRIPT_FILES "${TEMP_DIR}/\@mode\@-preample")

# CHANGE FOR EXECUTABLE RENAME REMOVE 
set(SCRIPT_FILES ${SCRIPT_FILES} "${SimVascular_SOURCE_DIR}/Scripts/executable/\@mode\@/\@tar\@-\@OSTYPE\@.in")

unset(GDB)
unset(GDB_FLAGS)
set(INSTALL_SYMLINK_FILE "${TEMP_DIR}/setup-symlinks.sh")
file(WRITE ${INSTALL_SYMLINK_FILE} "${locate_script}\n\necho $DIR\n\n")
dev_message("SIMVASCULAR_SCRIPT_TARGETS: ${SIMVASCULAR_SCRIPT_TARGETS}")
foreach(tar ${SIMVASCULAR_SCRIPT_TARGETS})
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
		#file(APPEND ${INSTALL_SYMLINK_FILE} "chmod a+x /usr/local/bin/${${tar}_INSTALL_SCRIPT_NAME}${WIN_BAT}\n")
	endif()
endforeach()
if(UNIX)
	install(PROGRAMS ${INSTALL_SYMLINK_FILE} DESTINATION ${SIMVASCULAR_INSTALL_SCRIPT_DIR})
	install(FILES "README-install" DESTINATION ${SIMVASCULAR_INSTALL_SCRIPT_DIR})
	#install(CODE "execute_process(COMMAND \"${INSTALL_SYMLINK_FILE}\"")")
endif()
if(BUILD_TESTING)
	configure_file(executable_names.tcl.in ${SimVascular_TEST_DIR}/common/executable_names.tcl)
endif()