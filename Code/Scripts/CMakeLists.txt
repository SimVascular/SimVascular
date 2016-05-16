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

set(core_vars 	SV_OS SOFTWARE_PLATFORM COMPILER_VERSION
	SV_VERSION OPEN_SOFTWARE_BINARIES_TOPLEVEL
	LICENSED_SOFTWARE_TOPLEVEL SV_STATIC_BUILD)


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
env_variable_to_value_variable(ENV_SV_HOME SV_HOME)

if(SV_USE_PARASOLID)
	#Parasolid as a few special directories to add
	set_env_string_concat(P_SCHEMA "${PARASOLID_SCHEMA_DIR}" developer_script_string)
	set_env_string(P_SCHEMA "${ENV_SV_HOME}/${SV_INSTALL_NATIVE_PARASOLID_SCHEMA_DIR}" install_script_string)
	set(FRAGMENT_INJECTION_STRING "${FRAGMENT_INJECTION_STRING}
		<Registry Id='regid.parasolid' Root='HKLM'
		Key='Software\\SimVascular\\@SV_VERSION@ @SV_MAJOR_VERSION@'
		Name='PSchemaDir' Action='write' Type='string'
		Value='[INSTALL_ROOT]@SV_INSTALL_NATIVE_PARASOLID_SCHEMA_DIR@' />")
endif()

if(SV_USE_MESHSIM)
	if(NOT MESHSIM_LICENSE_IN_WIN32_REGISTRY)
		if(MESHSIM_LICENSE_FILE)
			set_env_string_concat(SIM_LICENSE_FILE "${MESHSIM_LICENSE_FILE}" developer_script_string)
		else()
			set_env_string_concat(SIM_LICENSE_FILE "meshsim-license.dat" developer_script_string)
		endif()
		set_env_string_concat(SIM_LICENSE_FILE "meshsim-license.dat" install_script_string)
	endif()
endif()

foreach(lib ${SV_EXTERNAL_SHARED_LIBS})
        dev_message("SV_USE_SYSTEM_${lib} ${SV_USE_SYSTEM_${lib}}")
	# For Each library that we have linked against
	if(${lib}_DLL_PATH)
	        dev_message("${lib}_DLL_PATH: ${${lib}_DLL_PATH}")
		# If there is a DLL path associated with it, go ahead and add it to the scripts
		append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${${lib}_DLL_PATH}" developer_script_string)
	endif()

	# on unix systems we only need this path if its installed (moved)
	# if this library was not specified USE_SYSTEM, it will need to be installed, and the path updated.
	if(SV_INSTALL_${lib}_LIBRARY_DIR)
		#if there is an install library directory, add it to the path for installation, it will be installed there.
		append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${${lib}_DIR}/lib" developer_script_string)
		if(NOT SV_ENABLE_DISTRIBUTION)
		  append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SV_HOME}/Externals/${SV_INSTALL_${lib}_LIBRARY_DIR}" install_script_string)
		endif()
	endif()
	if(SV_INSTALL_${lib}_RUNTIME_DIR)
		# if there is an install runtime directory, add it to the path for installation
		# set(FRAGMENT_INJECTION_STRING "${FRAGMENT_INJECTION_STRING}
			# 	<Environment Id='UpdatePath.runtime.${lib}' Name='PATH'
			# 		Value='[INSTALL_ROOT]\\${SV_INSTALL_${lib}_RUNTIME_DIR}'
			# 		Permanent='yes' Part='last' Action='set' System='yes' />")
		append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${${lib}_DIR}/bin" developer_script_string)
		if(NOT SV_ENABLE_DISTRIBUTION)
		  append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SV_HOME}/Externals/${SV_INSTALL_${lib}_RUNTIME_DIR}" install_script_string)
		endif()
	endif()
	if(NOT SV_USE_SYSTEM_${lib})
        else()
	        if(${lib}_DLL_PATH)
		        append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${${lib}_DLL_PATH}" install_script_string)
	        endif()
        endif()
endforeach()
if(SV_ENABLE_DISTRIBUTION)
	append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SV_HOME}/Externals/lib" install_script_string)
endif()
# This is a catch all if no DLL_PATH was specified but we will need the so for installed libraries.
append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SV_HOME}/${SV_INSTALL_EXTERNALS_RUNTIME_DIR}" install_script_string)
append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SV_HOME}/${SV_INSTALL_HOME_DIR}" install_script_string)
append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SV_HOME}/${SV_INSTALL_RUNTIME_DIR}" install_script_string)
# set(FRAGMENT_INJECTION_STRING "${FRAGMENT_INJECTION_STRING}
	# 				<Environment Id='UpdatePath.runtime.${lib}' Name='PATH'
	# 					Value='[INSTALL_ROOT]\\@SV_INSTALL_EXTERNALS_RUNTIME_DIR@'
	# 					Permanent='yes' Part='last' Action='set' System='yes' />")

if(SV_USE_TKCXIMAGE)
	if(TKCXIMAGE_DLL)
		set_env_string_concat(TKCXIMAGE_DLL "${TKCXIMAGE_DLL}" developer_script_string)
	endif()
endif()

if (SV_USE_SYSTEM_TCL)
	set_env_string_concat(TCL_LIBRARY "${TCL_LIBRARY_PATH}/tcl8.6" developer_script_string)
	set_env_string_concat(TK_LIBRARY "${TK_LIBRARY_PATH}/tk8.6" developer_script_string)
endif()

if(NOT SV_USE_SYSTEM_TCL OR SV_INSTALL_SYSTEM_LIBS)
	#Lib additions
	set_env_string_concat(TCL_LIBRARY "${TCL_LIBRARY_PATH}/tcl8.6" developer_script_string)
	set_env_string_concat(TK_LIBRARY "${TK_LIBRARY_PATH}/tk8.6" developer_script_string)
	append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${TCL_LIBRARY_PATH}" developer_script_string)
	set_env_string_concat(TCL_LIBRARY "${ENV_SV_HOME}/${SV_INSTALL_TCL_LIBRARY_DIR}/tcl8.6" install_script_string)
	set_env_string_concat(TK_LIBRARY "${ENV_SV_HOME}/${SV_INSTALL_TCL_LIBRARY_DIR}/tk8.6" install_script_string)
	append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SV_HOME}/${SV_INSTALL_TCL_LIBRARY_DIR}" install_script_string)
endif()

#--------
# Python
if(SV_USE_PYTHON)
  if (SV_USE_SYSTEM_PYTHON)
	   append_env_string_concat(PYTHONPATH "${PYTHON_SITE_PACKAGES}" developer_script_string)
	if (SV_USE_SYSTEM_VTK)
	   append_env_string_concat(PYTHONPATH "${VTK_DLL_PATH}" developer_script_string)
	   append_env_string_concat(PYTHONPATH "${VTK_PYTHON_SITE_PACKAGES}" developer_script_string)
	endif()
  else()
	append_env_string_concat(PYTHONPATH "${VTK_DIR}/Wrapping/Python" developer_script_string)
	append_env_string_concat(PYTHONPATH "${VTK_DIR}/Wrapping/Python/vtk" developer_script_string)
	append_env_string_concat(PYTHONPATH "${VTK_DIR}/bin" developer_script_string)
	append_env_string_concat(PYTHONPATH "${VTK_DIR}/lib" developer_script_string)
	append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${VTK_DIR}/bin" developer_script_string)
	append_env_string_concat(PYTHONPATH "${ENV_SV_HOME}/${SV_INSTALL_VTK_LIBRARY_DIR}/python${PYTHONLIBS_MAJOR_VERSION}.${PYTHONLIBS_MINOR_VERSION}/site-packages" install_script_string)
	append_env_string_concat(PYTHONPATH "${ENV_SV_HOME}/${SV_INSTALL_VTK_LIBRARY_DIR}/python${PYTHONLIBS_MAJOR_VERSION}.${PYTHONLIBS_MINOR_VERSION}/site-packages/vtk" install_script_string)
	append_env_string_concat(PYTHONPATH "${ENV_SV_HOME}/${SV_INSTALL_VTK_RUNTIME_DIR}" install_script_string)
	append_env_string_concat(PYTHONPATH "${ENV_SV_HOME}/${SV_INSTALL_VTK_LIBRARY_DIR}" install_script_string)
	append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SV_HOME}/${SV_INSTALL_VTK_RUNTIME_DIR}" install_script_string)
  endif()
endif()

#--------
# Tcl is VTK built as shared
if (NOT SV_USE_SYSTEM_VTK)
  if(SV_USE_VTK_SHARED)
	append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${VTK_DIR}/lib" developer_script_string)
	append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SV_HOME}/${SV_INSTALL_VTK_LIBRARY_DIR}" install_script_string)
  endif()
endif()
set_env_string_concat(TCLLIBPATH "${VTK_DIR}/lib/tcltk/modules" developer_script_string)
if(SV_ENABLE_DISTRIBUTION)
  set_env_string_concat(TCLLIBPATH "${ENV_SV_HOME}/Externals/lib/tcltk/modules" install_script_string)
else()
  set_env_string_concat(TCLLIBPATH "${ENV_SV_HOME}/Externals/${SV_EXT_VTK_BIN_DIR}/lib/tcltk/modules" install_script_string)
endif()

# ITK built as shared for install
if (NOT SV_USE_SYSTEM_ITK)
  if(SV_USE_ITK AND SV_USE_ITK_SHARED)
	append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ITK_DIR}/lib" developer_script_string)
	append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SV_HOME}/${SV_INSTALL_ITK_LIBRARY_DIR}" install_script_string)
  endif()
endif()

# If using opencascade for install
if (NOT SV_USE_SYSTEM_OpenCASCADE)
  if(OpenCASCADE_DIR)
	  append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${OpenCASCADE_DIR}/lib" developer_script_string)
        append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${ENV_SV_HOME}/${SV_INSTALL_OpenCASCADE_LIBRARY_DIR}" install_script_string)
  endif()
 endif()

# If using system installs, add paths to shared libs if needed
if(SV_USE_SYSTEM_GDCM AND GDCM_DLL_PATH)
	append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${GDCM_DLL_PATH}" developer_script_string)
endif()
if(SV_USE_SYSTEM_OpenCASCADE AND OpenCASCADE_DLL_PATH)
	append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${OpenCASCADE_DLL_PATH}" developer_script_string)
endif()
if(SV_USE_SYSTEM_QT AND QT_DLL_PATH)
	append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${QT_DLL_PATH}" developer_script_string)
endif()
if(SV_USE_SYSTEM_TCL AND TCL_DLL_PATH)
	append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${TCL_DLL_PATH}" developer_script_string)
endif()

# need the path to the dll's
append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${CMAKE_BINARY_DIR}/Bin" developer_script_string)
append_env_string_concat(${ENV_LIBRARY_PATH_VARIABLE} "${CMAKE_BINARY_DIR}/Bin/RelWithDebInfo" developer_script_string)

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
		#file(APPEND ${INSTALL_SYMLINK_FILE} "chmod a+x /usr/local/bin/${${tar}_INSTALL_SCRIPT_NAME}${WIN_BAT}\n")
	endif()
endforeach()
if(UNIX)
	install(PROGRAMS ${INSTALL_SYMLINK_FILE} DESTINATION ${SV_INSTALL_SCRIPT_DIR})
	install(FILES "README-install" DESTINATION ${SV_INSTALL_SCRIPT_DIR})
	#install(CODE "execute_process(COMMAND \"${INSTALL_SYMLINK_FILE}\"")")
endif()
if(SV_BUILD_TESTING)
	configure_file(executable_names.tcl.in ${SV_TEST_DIR}/common/executable_names.tcl)
endif()
