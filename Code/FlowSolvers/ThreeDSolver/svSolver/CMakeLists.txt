##=========================================================================
 #
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
 #=========================================================================#

if(MSVC)
  string(REGEX REPLACE "STACK:10000000,10000000" "STACK:512000000,512000000" CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS}")
endif()
  if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    #set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-fbounds-check")
    set(CMAKE_Fortran_FLAGS "-ffixed-line-length-132 -cpp")
  else()
    #set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-check bounds")
    
    set(CMAKE_Fortran_FLAGS "-extend-source 132 -fpp")
  endif()
# Include c++ files
SET(CXXSRCS Cinput.cxx ValType.cxx input_fform.cxx partition.cxx main.cxx)

# Include c files
SET(CSRCS getIntPnts.c ShapeFunctions_uniformP.c shptet.c symtri.c symtet.c) # for shape functions 
SET(CSRCS ${CSRCS} write_restart_files.c) # solver io files
SET(CSRCS ${CSRCS} tmrc.c) # this is dummy timing function and should go away
	
if(NOT (LINUX AND IS64))
  SET(CSRCS ${CSRCS} missing_win32.c)
endif()

SET(FSRCS modules.f)

if(ThreeDSolver_USE_CORONARY)
  SET(FSRCS ${FSRCS} modules-coronary.f)
endif()

if (ThreeDSolver_USE_CLOSEDLOOP)
  SET(FSRCS ${FSRCS} modules-closedloop.f)
endif()

SET(FSRCS ${FSRCS} solver_subroutines.f)

SET(FSRCS ${FSRCS} addelmpvsqforsvls-closedloop-coronary.f
                   asbmfg-closedloop-varwall.f
                   asbnabi-closedloop.f
                   asbnasc-closedloop.f
                   asigmr-closedloop.f
                   elmgmr-closedloop-varwall.f
                   elmpvsq-closedloop-coronary.f
                   genbkb-varwall.f
                   genini-coronary.f
                   itrdrv-closedloop-varwall-coronary.f
                   proces-varwall.f
                   readnblk-varwall.f
                   updhistconv-coronary.f)

if(ThreeDSolver_USE_CORONARY)
  SET(FSRCS ${FSRCS} coronary_subroutines.f)
endif()

# Include files for the CLOSEDLOOP VERSION
if(ThreeDSolver_USE_CLOSEDLOOP)
  SET(FSRCS ${FSRCS} closedloop_subroutines.f)
endif()

# Include files for the VARWALL VERSION
if (ThreeDSolver_USE_VARWALL)
  SET(FSRCS ${FSRCS} varwall_subroutines.f)
endif()

# Include files for the USE_VTK VERSION
if (ThreeDSolver_USE_VTK)
  SET(CXXSRCS ${CXXSRCS} use_vtk.cxx)
endif()

if(ThreeDSolver_USE_LESLIB) 
  include_directories(${LESLIB_INCLUDE_DIR})
else()# dummy leslib
  add_library(lib_dummy_leslib leslib_dummy_prototypes.c)
  set(LESLIB_LIBRARY lib_dummy_leslib)
endif()
ADD_LIBRARY(flowsolver_fortran_lib ${FSRCS})
#SET_TARGET_PROPERTIES(flowsolver_fortran_lib PROPERTIES COMPILE_FLAGS "-extend_source 132")
TARGET_LINK_LIBRARIES(flowsolver_fortran_lib ${svMPI_EXTRA_LIBRARY} ${svMPI_Fortran_LIBRARIES})

set(HDRS Input.h ValType.h common_c.h)

simvascular_add_executable(${FLOWSOLVER_EXE} 
  SRCS ${CXXSRCS} ${CSRCS}
  DEV_SCRIPT_NAME "mysolver" 
  INSTALL_SCRIPT_NAME "svsolver"
  INSTALL_COMPONENT ModuleExecutables
  INSTALL_DESTINATION ${SIMVASCULAR_INSTALL_RUNTIME_DIR})

if(ThreeDSolver_USE_VTK)
  SET(VTKLIB ${VTK_LIBRARIES})
else()
  SET(VTKLIB "")
endif()


TARGET_LINK_LIBRARIES(${FLOWSOLVER_EXE} flowsolver_fortran_lib 
                      ${GLOBAL_LIBRARIES} ${INTELRUNTIME_LIBRARIES}
                      ${METIS_LIBRARY} ${SVLS_LIBRARY}
                      ${svMPI_EXTRA_LIBRARY} ${LESLIB_LIBRARY}
                      ${NSPCG_LIBRARY} lib_simvascular_solverio
                      ${VTKLIB}
                      )

if(SIMVASCULAR_INSTALL_LIBS)
  install(TARGETS flowsolver_fortran_lib
    RUNTIME DESTINATION ${SIMVASCULAR_INSTALL_RUNTIME_DIR} COMPONENT ModuleExecutables
    LIBRARY DESTINATION ${SIMVASCULAR_INSTALL_LIBRARY_DIR} COMPONENT ModuleLibraries
    ARCHIVE DESTINATION ${SIMVASCULAR_INSTALL_ARCHIVE_DIR} COMPONENT ModuleLibraries
    )
endif()
if(SIMVASCULAR_INSTALL_HEADERS)
  install(FILES ${HDRS}
    DESTINATION ${SIMVASCULAR_INSTALL_INCLUDE_DIR}/core COMPONENT CoreHeaders
    )
endif()



