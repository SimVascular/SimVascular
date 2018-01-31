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

# Licensed Package Additions
#-----------------------------------------------------------------------------
# Parasolid
if(SV_USE_PARASOLID)
	if(EXISTS ${SV_SOURCE_DIR}/Licensed/ParasolidSolidModel/CMakeLists.txt)
		simvascular_external(PARASOLID SYSTEM_DEFAULT SHARED_LIB)
		if(WIN32)
			set(SV_EXTRA_PATHS ${SV_EXTRA_PATHS} ${PARASOLID_DLL_PATH})
		endif()
		set(USE_PARASOLID ON)
		set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_USE_PARASOLID")
		option(SV_USE_PARASOLID_SHARED_LIBRARIES "Build Parasolid as shared libraries" OFF)
	else()
		message("Parasolid requires an extra license.")
	endif()
  # Find parasolid dll on windows to definitions
	if(PARASOLID_FOUND)
		include_directories(${PARASOLID_INCLUDE_DIR})
	endif()
	if (SV_USE_PARASOLID_SHARED_LIBRARIES)
		set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_USE_PARASOLID_SHARED")
	endif()
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# MeshSim
unset(MESHSIM_MODELER)
if(SV_USE_MESHSIM OR SV_USE_MESHSIM_DISCRETE_MODEL)
	unset(MESHSIM_COMP)
	if(APPLE)
		message(FATAL_ERROR "MeshSim not available on macos")
	endif()
	if(SV_USE_MESHSIM_DISCRETE_MODEL)
		set(USE_DISCRETE_MODEL ON)
		set(MESHSIM_COMP SimDiscrete)
	  set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_USE_MESHSIM_DISCRETE_MODEL")
	endif()
	if(SV_USE_PARASOLID)
		set(MESHSIM_MODELER "parasolid")
	endif()
	if(MESHSIM_MODELER MATCHES "parasolid")
    set(MESHSIM_SIMPARASOLID_VERSION SimParasolid290 CACHE STRING "When using parasolid, you need to specify which veriosn of the bridge library to use.")
    set(MESHSIM_ACCEPTED_SIMPARASOLID_VERSIONS SimParasolid290 SimParasolid280 SimParasolid271 SimParasolid270 SimParasolid260 SimParasolid251 SimParasolid250 SimParasolid241)
		set_property(CACHE MESHSIM_SIMPARASOLID_VERSION PROPERTY STRINGS ${MESHSIM_ACCEPTED_SIMPARASOLID_VERSIONS})
	endif()
	simvascular_external(MESHSIM COMPONENTS ${MESHSIM_COMP} ${MESHSIM_SIMPARASOLID_VERSION} SHARED_LIB SYSTEM_DEFAULT)
	if(MESHSIM_USE_LICENSE_FILE)
		set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DMESHSIM_USE_LICENSE_FILE")
		find_file(MESHSIM_LICENSE_FILE meshsim-license.dat PATHS ${MESHSIM_LICENSE_DIR})
		if(NOT MESHSIM_LICENSE_FILE)
			message(STATUS "")
			message(STATUS "MeshSim license NOT FOUND or specified.  Build will continue with a placeholder")
			message(STATUS "You will need to copy the license file into the build after compilation to use MeshSim functionality.")

			set(MESHSIM_LICENSE_FILE meshsim-license.dat)
		else()
			message(STATUS "MeshSim License: ${MESHSIM_LICENSE_FILE}")
		endif()
	else()
		unset(MESHSIM_LICENSE_FILE)
	endif()
	if(MESHSIM_EMBED_LICENSE_KEYS)
		set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DMESHSIM_EMBED_LICENSE_KEYS")
	endif()
	set(USE_MESHSIM ON)
  # If meshsim found
	if(MESHSIM_FOUND)
		include_directories(${MESHSIM_INCLUDE_DIR})
	endif()
	if (SV_USE_MESHSIM_SHARED_LIBRARIES)
		set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_USE_MESHSIM_SHARED")
	endif()
	if (SV_USE_MESHSIM_DISCRETE_SHARED_LIBRARIES)
		set(GLOBAL_DEFINES "${GLOBAL_DEFINES} -DSV_USE_MESHSIM_DISCRETE_MODEL_SHARED")
	endif()
endif()
message(STATUS "")
#-----------------------------------------------------------------------------
