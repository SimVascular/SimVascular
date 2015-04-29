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

# Include Flowsolver Configuration File
include(flowsolverConfig)
set(INDENTS "${INDENTS}    ")
# Include svLS Folder
#include_directories(${SVLS_INCLUDE_DIR})

set(SVLS_LIBRARY lib_simvascular_svls)
set(FLOWSOLVER_SUBDIRS ${FLOWSOLVER_SUBDIRS} IncompressibleSolver/svLS)


#-----------------------------------------------------------------------------
# Post Solver
if(Module_POSTSOLVER)
	set(FLOWSOLVER_SUBDIRS ${FLOWSOLVER_SUBDIRS} PostSolver)
	set(Module_SOLVERIO ON)
endif()

#-----------------------------------------------------------------------------
# SolverIO
if(Module_SOLVERIO)
	set(FLOWSOLVER_SUBDIRS ${FLOWSOLVER_SUBDIRS} SolverIO)
endif()
#-----------------------------------------------------------------------------
# Presolver
if(Module_PRESOLVER)
	set(FLOWSOLVER_SUBDIRS ${FLOWSOLVER_SUBDIRS} PreSolver)
	set(Module_SOLVERIO ON)
endif()

set(FLOWSOLVER_SUBDIRS ${FLOWSOLVER_SUBDIRS} IncompressibleSolver/Solver)

# Set Coronary Flag
if(FLOWSOLVER_Version_CORONARY_ACTIVATE)
  SET(VER_CORONARY 1)
else()
  SET(VER_CORONARY 0)
endif()
# Set Closed Loop Flag
if(FLOWSOLVER_Version_CLOSEDLOOP_ACTIVATE)
  SET(VER_CLOSEDLOOP 1)
else()
  SET(VER_CLOSEDLOOP 0)
endif()
# Set Variable wall Flag
if(FLOWSOLVER_Version_VARWALL_ACTIVATE)
  SET(VER_VARWALL 1)
else()
  SET(VER_VARWALL 0)
endif()
# Set Use VTK Flag
if(FLOWSOLVER_Version_USE_VTK_ACTIVATE)
  SET(VER_USE_VTK 1)
else()
  SET(VER_USE_VTK 0)
endif()

# Write include file
configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/cvFlowsolverOptions.h.in"
  "${CMAKE_CURRENT_BINARY_DIR}/cvFlowsolverOptions.h"
)

set(FLOWSOLVER_OPTIONS_HEADER_PATH ${CMAKE_CURRENT_BINARY_DIR} CACHE INTERNAL "" FORCE)
dev_message("Flowsolver Header Path:  ${FLOWSOLVER_OPTIONS_HEADER_PATH}" )

include_directories("${CMAKE_CURRENT_BINARY_DIR}")
message(STATUS "${INDENTS}ThreeDSolver Options")
if((VER_CORONARY EQUAL 0) AND (VER_CLOSEDLOOP EQUAL 0) AND (VER_VARWALL EQUAL 0) AND (VER_USE_VTK EQUAL 0))
  MESSAGE(STATUS "${INDENTS}    Compiling baseline version")
endif()
if(VER_CORONARY EQUAL 1)
  MESSAGE(STATUS "${INDENTS}    Activating coronary bc feature")
endif()
if(VER_CLOSEDLOOP EQUAL 1)
  MESSAGE(STATUS "${INDENTS}    Activating closed loop bc feature")
endif()
if(VER_VARWALL EQUAL 1)
  MESSAGE(STATUS "${INDENTS}    Activating variable wall feature")
endif()
if(VER_USE_VTK EQUAL 1)
  MESSAGE(STATUS "${INDENTS}    Activating using VTK feature")
endif()

foreach(dir ${FLOWSOLVER_SUBDIRS})
	include_directories(${CMAKE_CURRENT_SOURCE_DIR}/${dir})
endforeach()

message(STATUS "${INDENTS}ThreeDSolver Components")
foreach(dir ${FLOWSOLVER_SUBDIRS})
	message(STATUS "${INDENTS}    ${dir}")
	add_subdirectory(${dir})
endforeach()


