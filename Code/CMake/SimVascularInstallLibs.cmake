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

macro(simvascular_install_prereqs tar location)
  get_prerequisites(${tar} DEPENDENCIES 1 1 "" "")
  foreach(DEPENDENCY ${DEPENDENCIES})
    get_filename_component(DEPENDENCY_NAME "${DEPENDENCY}" NAME)
    get_filename_component(DEPENDENCY_NAME_WE "${DEPENDENCY}" NAME_WE)
    get_filename_component(DEPENDENCY_ACTUAL "${DEPENDENCY}" REALPATH)
    dev_message("[${tar} Dep] ${DEPENDENCY_ACTUAL}")
    install(FILES "${DEPENDENCY_ACTUAL}"
      DESTINATION "${location}"
      RENAME "${DEPENDENCY_NAME}"
      COMPONENT ExternalLibraries
      )
  endforeach()
endmacro()

  set(msg "[SimVascularInstallLibs] ")
  
  message(STATUS "${msg} ")
  message(STATUS "${msg} ========== SimVascularInstallLibs.cmake ==========")
  message(STATUS "${msg} SV_ENABLE_DISTRIBUTION: ${SV_ENABLE_DISTRIBUTION} ")
  message(STATUS "${msg} SV_EXTERNAL_SHARED_LIBS: ${SV_EXTERNAL_SHARED_LIBS} ")

message(STATUS "${msg} ------ for SV_EXTERNAL_SHARED_LIBS ...") 

foreach(var ${SV_EXTERNAL_SHARED_LIBS})
  message(STATUS "${msg} ----- var: ${var} ----- ")

  if(NOT ${var}_DLL_LIBRARIES)
      set(${var}_DLL_LIBRARIES ${${var}_LIBRARY} ${${var}_LIBRARIES} ${${var}_DLL_LIBRARY})
  endif()

  if(${var}_DLL_LIBRARIES)
    list(REMOVE_DUPLICATES ${var}_DLL_LIBRARIES)
  endif()

  if(SV_ENABLE_DISTRIBUTION OR NOT SV_USE_SYSTEM_${var} )

    # We only want to install libraries they are not system libraries.
    # we want to install them regardles if this is a distribution.

    dev_message("[${var}]  This is either not a system library, or this is a distribution install.")

    if(NOT SV_INSTALL_${var}_RUNTIME_DIR)
      message("${msg} [${var}]  SV_INSTALL_${var}_RUNTIME_DIR does not exist, ${lib_name} installing into externals")

      # Variables with a install directory are handled differently
      # if the directory doesn't exist, install in in the catch all

      foreach(lib ${${var}_DLL_LIBRARIES})
        message(STATUS "${msg} lib: ${lib} ")
        dev_message("[${var}]  Do we install ${lib}")
        get_filename_component(lib_name ${lib} NAME)
        get_filename_component(_EXT ${lib_name} EXT)
        get_filename_component(lib_name_we ${lib} NAME_WE)
        get_filename_component(lib_path ${lib} PATH)

        if(_EXT MATCHES "${CMAKE_SHARED_LIBRARY_SUFFIX}$")
          dev_message("[${var}] ${lib_name} marked for install into ${SV_INSTALL_EXTERNALS_RUNTIME_DIR}")
          if(IS_SYMLINK ${lib})
            dev_message("${lib_name} ${lib_name_we} is a symbolic link\n${lib_path}/${lib_name_we}*")
            #get_filename_component(lib ${})
            file(GLOB lib "${lib_path}/${lib_name_we}*")
          endif()
          dev_message("Installing ${lib}")
          install(FILES ${lib} DESTINATION ${SV_INSTALL_RUNTIME_DIR} COMPONENT ExternalLibraries)
          simvascular_install_prereqs(${lib} ${SV_INSTALL_EXTERNALS_RUNTIME_DIR})
        else()
          dev_message("[${var}]  ${lib_name} is not a shared lib, removing ${lib_name}, EXT ${_EXT}, do not install")
          list(REMOVE_ITEM ${var}_DLL_LIBRARIES ${lib})
        endif()
      endforeach()
    else()
      #debug only
      dev_message("SV_INSTALL_${var}_RUNTIME_DIR exists, ${var} will be handled elsewhere")
    endif()
  else()
    dev_message("[${var}] This is system library, and distribution is not enabled.")
  endif()
  dev_message("")
endforeach()



#-----------------------------------------------------------------------------
# Copy External Libs
#-----------------------------------------------------------------------------

#message("${msg} ") 
#message("${msg} ----- Copy External Libs -----") 

#-----------------------------------------------------------------------------
# MPI
if(SV_USE_MPI AND NOT SV_USE_DUMMY_MPICH2)
  if(SV_ENABLE_DISTRIBUTION OR NOT SV_USE_SYSTEM_MPI)
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
        DESTINATION ${SV_INSTALL_MPI_LIBRARY_DIR} COMPONENT MPIRuntimes)
      foreach(lib ${MPI_INSTALL_LIBS})
      simvascular_install_prereqs(${lib} ${SV_INSTALL_MPI_RUNTIME_DIR})
    endforeach()
    endif()
    #find MPIEXEC's path, and install it and everything with MPI or hydra in the name
    get_filename_component(MPIEXEC_PATH ${MPIEXEC} PATH)
    file(GLOB MPIEXEC_INSTALL_PROGS "${MPIEXEC_PATH}/mpiexec-*")
    file(GLOB HYDRA_INSTALL_PROGS "${MPIEXEC_PATH}/*hydra*")
    set(MPIEXEC_INSTALL_PROGS ${MPIEXEC_INSTALL_PROGS} ${HYDRA_INSTALL_PROGS} ${MPIEXEC})
    if(MPIEXEC_INSTALL_PROGS)
      list(REMOVE_DUPLICATES MPIEXEC_INSTALL_PROGS)
      # CHANGE NO TARGETS
      # install(PROGRAMS ${MPIEXEC_INSTALL_PROGS} DESTINATION ${SV_INSTALL_MPI_EXE_DIR})
      install(PROGRAMS ${MPIEXEC_INSTALL_PROGS}
        DESTINATION ${SV_INSTALL_HOME_DIR} COMPONENT MPIExecutables)
    endif()
    if(APPLE)
      install(CODE "
      file(GLOB INSTALLED_MPIEXECS \"\${CMAKE_INSTALL_PREFIX}/mpiexec-*\")
      list(GET INSTALLED_MPIEXECS 0 WORK_MPIEXEC)
      get_filename_component(ONE_MPIEXEC \"\${WORK_MPIEXEC}\" NAME)
      if (EXISTS \"\${CMAKE_INSTALL_PREFIX}/mpiexec\")
        file(REMOVE \"\${CMAKE_INSTALL_PREFIX}/mpiexec\")
      endif()
      execute_process(COMMAND \${CMAKE_COMMAND} -E create_symlink \"\${ONE_MPIEXEC}\" \"mpiexec\" WORKING_DIRECTORY \${CMAKE_INSTALL_PREFIX})
      ")
    endif()
  endif()
endif()

if(SV_ENABLE_DISTRIBUTION OR NOT SV_USE_SYSTEM_PARSOLID)
  if(SV_USE_PARASOLID)
    file(GLOB PARASOLID_DLLS "${PARASOLID_DLL_PATH}/*${CMAKE_SHARED_LIBRARY_SUFFIX}")

    set(PARASOLID_INSTALL_LIB_DIR ${SV_INSTALL_LIBRARY_DIR})
    if(WIN32)
      set(PARASOLID_INSTALL_LIB_DIR ${SV_INSTALL_RUNTIME_DIR})
    endif()

    file(GLOB PARASOLID_INSTALL_SCHEMAS "${PARASOLID_SCHEMA_DIR}/*.*")

    if(NOT SV_DISTRIBUTE_COMPONENTS STREQUAL OpenSource)
      install(FILES ${PARASOLID_DLLS}
        DESTINATION ${PARASOLID_INSTALL_LIB_DIR}
        COMPONENT LicensedLibraries)

      install(FILES ${PARASOLID_INSTALL_SCHEMAS}
      DESTINATION ${SV_INSTALL_PARASOLID_SCHEMA_DIR}
      COMPONENT LicensedData)
    endif()

  endif()
endif()

if(SV_ENABLE_DISTRIBUTION OR NOT SV_USE_SYSTEM_MESHSIM)
  if(SV_USE_MESHSIM AND MESHSIM_USE_LICENSE_FILE)
    if(NOT SV_DISTRIBUTE_COMPONENTS STREQUAL OpenSource)
      if(EXISTS ${MESHSIM_LICENSE_FILE})
        install(FILES ${MESHSIM_LICENSE_FILE}
          DESTINATION ${SV_INSTALL_HOME_DIR}
          RENAME "meshsim-license.dat"
          COMPONENT LicenseFiles)
      endif()
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
    PATHS ${SV_DISTRIBUTION_DIR}/dicom2 ${SV_DISTRIBUTION_DIR}/dcmtk
    )

  if(NOT EXTERNALEXE_${_EXE})
    dev_message(STATUS "${exe} was not found. Skipping install")
    add_custom_target(${exe}-copy ALL
      COMMENT "${exe} was not found, functionality may be impaired"
      )
  else()
    dev_message("    ${EXTERNALEXE_${_EXE}} as ${_exe}${${CMAKE_EXECUTABLE_SUFFIX}}.")
    add_custom_target(${exe}-copy ALL
      COMMAND ${CMAKE_COMMAND} -E remove ${SV_HOME}/${exe}${CMAKE_EXECUTABLE_SUFFIX}
      COMMAND ${CMAKE_COMMAND} -E copy ${EXTERNALEXE_${_EXE}} ${SV_HOME}/${exe}${CMAKE_EXECUTABLE_SUFFIX}
      COMMENT "Copying ${exe} to ${OUTBIN_DIR}/${exe}"
      )

endif()
install(PROGRAMS ${EXTERNALEXE_${_EXE}}
  DESTINATION ${SV_INSTALL_EXTERNAL_EXE_DIR}
  RENAME "${_exe}${CMAKE_EXECUTABLE_SUFFIX}" COMPONENT CoreExecutables OPTIONAL)
endforeach()
