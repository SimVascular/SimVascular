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

macro(simvascular_install_prereqs tar location)
    GET_PREREQUISITES(${tar} DEPENDENCIES 1 1 "" "")
    FOREACH(DEPENDENCY ${DEPENDENCIES})
        GET_FILENAME_COMPONENT(DEPENDENCY_NAME "${DEPENDENCY}" NAME)
        GET_FILENAME_COMPONENT(DEPENDENCY_NAME_WE "${DEPENDENCY}" NAME_WE)
        GET_FILENAME_COMPONENT(DEPENDENCY_ACTUAL "${DEPENDENCY}" REALPATH)
        dev_message("[${tar} Dep] ${DEPENDENCY_ACTUAL}")
        install(FILES "${DEPENDENCY_ACTUAL}"
         DESTINATION "${location}"
         RENAME "${DEPENDENCY_NAME}"
         )
    ENDFOREACH()
endmacro()

foreach(var ${SIMVASCULAR_EXTERNAL_SHARED_LIBS})
    if(NOT ${var}_DLL_LIBRARIES)
        set(${var}_DLL_LIBRARIES ${${var}_LIBRARY} ${${var}_LIBRARIES} ${${var}_DLL_LIBRARY})
    endif()
    if(${var}_DLL_LIBRARIES)
        list(REMOVE_DUPLICATES ${var}_DLL_LIBRARIES)
    endif()
    if(SimVascular_ENABLE_DISTRIBUTION OR NOT SimVascular_USE_SYSTEM_${var} )
        # We only want to install libraries they are not system libraries.
        # we want to install them regardles if this is a distribution.
        dev_message("[${var}]  This is either not a system library, or this is a distribution install.")
        if(NOT SIMVASCULAR_INSTALL_${var}_RUNTIME_DIR)
            dev_message("[${var}]  SIMVASCULAR_INSTALL_${var}_RUNTIME_DIR does not exist, ${lib_name} installing into externals")
            # Variables with a install directory are handled differently
            # if the directory doesn't exist, install in in the catch all
            foreach(lib ${${var}_DLL_LIBRARIES})
                dev_message("[${var}]  Do we install ${lib}")
                get_filename_component(lib_name ${lib} NAME)
                get_filename_component(_EXT ${lib_name} EXT)
                get_filename_component(lib_name_we ${lib} NAME_WE)
                get_filename_component(lib_path ${lib} PATH)
                if(_EXT MATCHES "${CMAKE_SHARED_LIBRARY_SUFFIX}$")
                    dev_message("[${var}] ${lib_name} marked for install into ${SIMVASCULAR_INSTALL_EXTERNALS_RUNTIME_DIR}")
                    if(IS_SYMLINK ${lib})
                        dev_message("${lib_name} ${lib_name_we} is a symbolic link\n${lib_path}/${lib_name_we}*")
                        #get_filename_component(lib ${})
                        FILE(GLOB lib "${lib_path}/${lib_name_we}*")
                    endif()
                    dev_message("Installing ${lib}")
                    install(FILES ${lib} DESTINATION ${SIMVASCULAR_INSTALL_RUNTIME_DIR} COMPONENT "ExteralRuntimes")
                    simvascular_install_prereqs(${lib} ${SIMVASCULAR_INSTALL_EXTERNALS_RUNTIME_DIR})
                else()
                    dev_message("[${var}]  ${lib_name} is not a shared lib, removing ${lib_name}, EXT ${_EXT}, do not install")
                    list(REMOVE_ITEM ${var}_DLL_LIBRARIES ${lib})
                endif()
            endforeach()
        else()
            #debug only
            dev_message("SIMVASCULAR_INSTALL_${var}_RUNTIME_DIR exists, ${var} will be handled elsewhere")
        endif()
    else()
        dev_message("[${var}] This is system library, and distribution is not enabled.")
    endif()
    dev_message("")
endforeach()


#-----------------------------------------------------------------------------
# Copy External Libs
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# TCL

if(NOT SimVascular_USE_SYSTEM_TCL OR SIMVASCULAR_INSTALL_SYSTEM_LIBS)
    if(NOT APPLE)
        message("${TCL_DLL_PATH}")
        set(extra_exclude_pattern)
        if(UNIX)
          list(APPEND extra_exclude_pattern
            REGEX "/bin" EXCLUDE
            )
      endif()
      install(DIRECTORY 
        ${TCL_SOURCE_DIR}/
        DESTINATION ${SIMVASCULAR_INSTALL_TCL_TOP_DIR}
        COMPONENT TclTkLibraries
        PATTERN "tcllib_doc" EXCLUDE
        PATTERN "tklib_doc" EXCLUDE
        PATTERN "*.a" EXCLUDE
        PATTERN "include" EXCLUDE
        PATTERN "*.lib" EXCLUDE
        PATTERN "*.sh" EXCLUDE
        )
      # Tcl should be installed with linux, and windows copies, but not apple.
      file(GLOB TCL_DLLS "${TCL_DLL_PATH}/*${CMAKE_SHARED_LIBRARY_SUFFIX}")
      # CHANGE FOR EXECUTABLE RENAME REMOVE
      # if(NOT WIN32)
      #     install(FILES ${TCL_DLLS} DESTINATION ${SIMVASCULAR_INSTALL_TCL_RUNTIME_DIR})
      # else()
      #   install(FILES ${TCL_DLLS}  DESTINATION ${SIMVASCULAR_INSTALL_RUNTIME_DIR})
      #endif()
      install(FILES ${TCL_DLLS} DESTINATION ${SIMVASCULAR_INSTALL_RUNTIME_DIR} COMPONENT TclTkRuntimes)
  endif()
endif()

#-----------------------------------------------------------------------------
# MPI
if(USE_MPI AND NOT SimVascular_USE_DUMMY_MPICH2)
    if(SimVascular_ENABLE_DISTRIBUTION OR NOT SimVascular_USE_SYSTEM_MPI)
        if(NOT WIN32)
            # MPI needs to be insalled, if its not a system LIBRARY.
            # or this is a distribution, unless its WIN32

            # Find all the libraries and install them
            getListOfVars("MPI" "LIBRARIES" _VARLIST)
            getListOfVars_concat("MPI" "LIBRARY" _VARLIST)
            listvars2vals(_VARLIST MPI_LIBS)
            set(MPI_INSTALL_LIBS)
            foreach(lib ${MPI_LIBS})
                get_filename_component(lib_name ${lib} NAME)
                get_filename_component(_EXT ${lib_name} EXT)
                get_filename_component(lib_name_we ${lib} NAME_WE)
                get_filename_component(lib_path ${lib} PATH)
                get_filename_component(real_lib ${lib} REALPATH)
                file(GLOB libs "${lib_path}/${lib_name_we}*")
                foreach(lib2 ${libs})
                    if(NOT ${lib2} MATCHES "${CMAKE_STATIC_LIBRARY_SUFFIX}$")
                        set(MPI_INSTALL_LIBS ${MPI_INSTALL_LIBS} ${lib2})
                    endif()
                endforeach()
            endforeach()
            if(MPI_INSTALL_LIBS)
                list(REMOVE_DUPLICATES MPI_INSTALL_LIBS)
            endif()
            install(FILES ${MPI_INSTALL_LIBS} 
                DESTINATION ${SIMVASCULAR_INSTALL_MPI_LIBRARY_DIR} COMPONENT MPIRuntimes)
            foreach(lib ${MPI_INSTALL_LIBS})
            simvascular_install_prereqs(${lib} ${SIMVASCULAR_INSTALL_MPI_RUNTIME_DIR})
        endforeach()
        endif()
        #find MPIEXEC's path, and install it and everything with MPI or hydra in the name
        get_filename_component(MPIEXEC_PATH ${MPIEXEC} PATH)
        file(GLOB MPIEXEC_INSTALL_PROGS "${MPIEXEC_PATH}/mpiexec*")
        file(GLOB HYDRA_INSTALL_PROGS "${MPIEXEC_PATH}/*hydra*")
        set(MPIEXEC_INSTALL_PROGS ${MPIEXEC_INSTALL_PROGS} ${HYDRA_INSTALL_PROGS} ${MPIEXEC})
        if(MPIEXEC_INSTALL_PROGS)
            list(REMOVE_DUPLICATES MPIEXEC_INSTALL_PROGS)
            # CHANGE NO TARGETS
            # install(PROGRAMS ${MPIEXEC_INSTALL_PROGS} DESTINATION ${SIMVASCULAR_INSTALL_MPI_EXE_DIR})
            install(PROGRAMS ${MPIEXEC_INSTALL_PROGS} 
                DESTINATION ${SIMVASCULAR_INSTALL_HOME_DIR} COMPONENT MPIExecutables)
        endif()
    endif()
endif()


if(USE_INTEL)
    #Something for intel
endif()

if(USE_MESHSIM)

endif()

if(SimVascular_ENABLE_DISTRIBUTION OR NOT SimVascular_USE_SYSTEM_PARSOLID)
    if(NOT SimVascular_EXCLUDE_SOLID_MODEL)
        if(Licensed_PARASOLID)
            file(GLOB PARASOLID_DLLS "${PARASOLID_DLL_PATH}/*${CMAKE_SHARED_LIBRARY_SUFFIX}")
            install(FILES ${PARASOLID_DLLS} DESTINATION ${SIMVASCULAR_INSTALL_RUNTIME_DIR})
            file(GLOB PARASOLID_INSTALL_SCHEMAS "${PARASOLID_SCHEMA_DIR}/*.*")
            install(FILES ${PARASOLID_INSTALL_SCHEMAS}
                DESTINATION ${SIMVASCULAR_INSTALL_PARASOLID_SCHEMA_DIR})
        endif()
    endif()
endif()

#-----------------------------------------------------------------------------
# Copy External Executables
#-----------------------------------------------------------------------------

set(EXTERNAL_EXES dicom2 dcmdump dcmodify)

if(WIN32)
    set(_DICOM2_NAMES "dicom2-win32.exe")
    set(_DCMDUMP_NAMES "dcmdump-win32.exe")
    set(_DCMODIFY_NAMES "dcmdump-win32.exe")
endif()

if(APPLE)
    # THESE NEED TO BE CHANGED
    set(_DICOM2_NAMES "dicom2-macos.exe")
    set(_DCMDUMP_NAMES "dcmdump-macos.exe")
    set(_DCMODIFY_NAMES "dcmdump-macos.exe")
endif()

if(LINUX)
    set(_DICOM2_NAMES "dicom2-linux.exe")
    set(_DCMDUMP_NAMES "dcmdump-linux.exe")
    set(_DCMODIFY_NAMES "dcmdump-linux.exe")
endif()

dev_message("Will install external executables")

foreach(exe ${EXTERNAL_EXES})
    string(TOUPPER "${exe}" _EXE)
    string(TOLOWER "${exe}" _exe)

    find_program(EXTERNALEXE_${_EXE}
        NAMES ${_${_EXE}_NAMES}
        PATHS ${SimVascular_DISTRIBUTION_DIR}/dicom2 ${SimVascular_DISTRIBUTION_DIR}/dcmtk
        )
    
    if(NOT EXTERNALEXE_${_EXE})
      message(STATUS "${exe} was not found. Skipping install")
      add_custom_target(${exe}-copy ALL
        COMMENT "${exe} was not found, functionality may be impaired"
        )
  else()
    dev_message("    ${EXTERNALEXE_${_EXE}} as ${_exe}${${CMAKE_EXECUTABLE_SUFFIX}}.")
    add_custom_target(${exe}-copy ALL
        COMMAND ${CMAKE_COMMAND} -E remove ${SimVascular_HOME}/${exe}${CMAKE_EXECUTABLE_SUFFIX}
        COMMAND ${CMAKE_COMMAND} -E copy ${EXTERNALEXE_${_EXE}} ${SimVascular_HOME}/${exe}${CMAKE_EXECUTABLE_SUFFIX}
        COMMENT "Copying ${exe} to ${OUTBIN_DIR}/${exe}"
        )

endif()
install(PROGRAMS ${EXTERNALEXE_${_EXE}} 
    DESTINATION ${SIMVASCULAR_INSTALL_EXTERNAL_EXE_DIR}
    RENAME "${_exe}${CMAKE_EXECUTABLE_SUFFIX}" OPTIONAL)
#unset(EXTERNALEXE_${_EXE} CACHE)
endforeach()




