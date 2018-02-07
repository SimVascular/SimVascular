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
# ThirdParty!
#-----------------------------------------------------------------------------
# ZLIB
if(SV_USE_ZLIB)
	SET(USE_ZLIB ON)
  SET(SV_USE_ZLIB ON)
	simvascular_third_party(zlib)
	if(NOT SV_USE_SYSTEM_ZLIB)
    set(ZLIB_LIBRARY ${SV_LIB_THIRDPARTY_ZLIB_NAME})
	else()
		simvascular_external(ZLIB)
	endif()
else()
	unset(ZLIB_LIBRARY CACHE)
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Tetgen
if(SV_USE_TETGEN)
	SET(USE_TETGEN ON)
	simvascular_third_party(tetgen)
  set(TETGEN_VERSION "1.5.1")
  set(TETGEN_LIBRARY ${SV_LIB_THIRDPARTY_TETGEN_NAME})

	if(TETGEN_VERSION MATCHES "1.5.1")
		set(TETGEN151 ON)
	elseif(TETGEN_VERSION MATCHES "1.5.0")
		set(TETGEN150 ON)
	elseif(TETGEN_VERSION MATCHES "1.4.3")
		set(TETGEN143 ON)
	else()
		message(FATAL_ERROR "Unknown Tetgen versions, please specify!")
	endif()
else()
	unset(TETGEN_LIBRARY CACHE)
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# VMTK
if(SV_USE_VMTK)
  simvascular_third_party(vmtk)
  if(NOT SV_USE_SYSTEM_VMTK)
    set(VMTK_LIBRARY ${SV_LIB_THIRDPARTY_VMTK_NAME})
  else()
    message(FATAL_ERROR "Cannot currently use system VMTK")
  endif()
else()
  unset(VMTK_LIBRARY)
endif()
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# SolverIO
if(SV_USE_SOLVERIO)
  simvascular_third_party(SolverIO)
  if(NOT SV_USE_SYSTEM_SOLVERIO)
    set(SOLVERIO_LIBRARY ${SV_LIB_THIRDPARTY_SOLVERIO_NAME})
  else()
    message(FATAL_ERROR "Cannot currently use system SolverIO")
  endif()
else()
  unset(SOLVERIO_LIBRARY)
endif()

#-----------------------------------------------------------------------------
