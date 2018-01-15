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
# ThreeD Solver
if(SV_USE_THREEDSOLVER)
	option(SV_USE_SPARSE "Use sparse Library" ON)
	option(SV_USE_METIS "Use metis Library" ON)
	option(SV_USE_NSPCG "Use nspcg Library" ON)
	set(SV_USE_FORTRAN ON)
	set(SV_THREEDSOLVER_USE_SVLS ON)
	set(SV_USE_MPI ON)
	set(SV_THREEDSOLVER_USE_SOLVERIO ON)
	#set(FLOWSOLVER_CONFIG_PATHS
	#	"${SV_SOURCE_DIR}/Modules/ThreeDSolver/IncompressibleSolver/Solver" CACHE TYPE PATH)
	# find config file
	#set(FLOWSOLVER_CONFIG_FILE "${SV_SOURCE_DIR}/Modules/ThreeDSolver/IncompressibleSolver/Solver/default.inp")
	#set(FLOWSOLVER_CONFIG_PATH ${OUTBIN_DIR})
	#file(COPY ${FLOWSOLVER_CONFIG_FILE} DESTINATION ${FLOWSOLVER_CONFIG_PATH})
	#install(FILES ${FLOWSOLVER_CONFIG_FILE} DESTINATION ${SV_INSTALL_RUNTIME_DIR})
endif()

#-----------------------------------------------------------------------------
# Solver IO
if(SV_THREEDSOLVER_SOLVERIO_REDIRECT)
	set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DBUILD_WITH_FLOWSOLVER_STDOUT_STDERR_REDIRECT")
endif()

#-----------------------------------------------------------------------------
#  SVLS
# svLS depends on the THREEDSOLVER build state so it must be here.
if(SV_THREEDSOLVER_USE_SVLS)
	set(SVLS_BUILD_TYPE "Source")
	#simvascular_external(svls SVEXTERN_DEFAULT)
	set(SV_USE_FORTRAN ON)
	set(SV_USE_MPI ON)
endif()

if(SV_THREEDSOLVER_USE_LESLIB)
	find_package(LESLIB REQUIRED)
endif()

#-----------------------------------------------------------------------------
# Remaining optional dependencies
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Enable Fortran
if(SV_USE_FORTRAN)
	enable_language(Fortran)
	if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
		#set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-fbounds-check")
		set(CMAKE_Fortran_FLAGS "-ffixed-line-length-132 -cpp")
	else()
		#set(CMAKE_Fortran_FLAGS_RELWITHDEBINFO "-check bounds")
		set(CMAKE_Fortran_FLAGS "-132 -fpp")
	endif()
endif()

#-----------------------------------------------------------------------------
# Enable MPI
if(SV_USE_MPI)
	if (NOT SV_USE_DUMMY_MPICH2)
		simvascular_external(MPI)
		if(MPI_FOUND)
			get_filename_component(MPI_LIBRARY_DIR ${MPI_LIBRARY} PATH)
		endif()
		if(WIN32)
			find_library(MPI_fmpich2_LIBRARY NAMES fmpich2 HINTS ${MPI_LIBRARY_DIR})
			set(MPI_EXTRA_LIBRARY ${MPI_EXTRA_LIBRARY} ${MPI_fmpich2_LIBRARY} ${MPI_CXX_LIBRARIES})
			#message("${MPI_EXTRA_LIBRARY}")
		endif()

		# include_directories(${MPI_Fortran_INCLUDE_PATH})
		if(SV_DEVELOPER_OUTPUT)
			#getListOfVarsPrefix("MPI" _VARLIST)
			#print_vars(_VARLIST)
		endif()
		if(SV_USE_MSMPI)
			# TODO(jmerkow): Change this.
			set(SV_MPI_DIR "${CMAKE_CURRENT_SOURCE_DIR}/ThirdParty/msmpi/")
			set(SV_MPI_LIB_DIR  "${SV_MPI_DIR}/Lib/x64/")
			set(SV_MPI_INCLUDE_PATH "${SV_MPI_DIR}/Include/;${SV_MPI_DIR}/Include/x64/")
			set(SV_MPI_EXTRA_LIBRARY "")
			set(SV_MPI_Fortran_LIBRARIES "${SV_MPI_LIB_DIR}/msmpi.lib;${SV_MPI_LIB_DIR}/msmpifmc.lib;${SV_MPI_LIB_DIR}/msmpifec.lib")
		else()
			set(SV_MPI_EXTRA_LIBRARY ${MPI_EXTRA_LIBRARY})
			set(SV_MPI_Fortran_LIBRARIES ${MPI_Fortran_LIBRARIES})
			set(SV_MPI_INCLUDE_PATH ${MPI_Fortran_INCLUDE_PATH})
		endif()
		include_directories(${SV_MPI_INCLUDE_PATH})
	else()
		set(SV_MPI_EXTRA_LIBRARY lib_extra_simvascular_dummympi)
		set(SV_MPI_Fortran_LIBRARIES lib_fortran_simvascular_dummympi)
	endif()
endif()

