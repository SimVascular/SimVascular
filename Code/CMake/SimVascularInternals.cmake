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
# Core libs
set(SV_LIBS
  ADAPTOR
  GEOM
  GLOBALS
  IMAGE
  ITK_LSET
  LSET
  MATH
  PATH
  COMMON
  MESH
  MESHSIM_MESH
  MESHSIM_ADAPTOR
  MESHSIM_DISCRETE_SOLID
  MESHSIM_SOLID
  MMG_MESH
  OpenCASCADE_SOLID
  PARASOLID_SOLID
  POLYDATA_SOLID
  POST
  REPOSITORY
  PYTHON_API
  SOLID
  SEGMENTATION
  SVFSI
  TCL_INTERP
  TETGEN_MESH
  TETGEN_ADAPTOR
  UTILS
  VMTK_UTILS)

# Thirdparty libs
set(SV_LIBS ${SV_LIBS}
  THIRDPARTY_METIS
  THIRDPARTY_NSPCG
  THIRDPARTY_SPARSE
  THIRDPARTY_TETGEN
  THIRDPARTY_VMTK
  THIRDPARTY_ZLIB
  THIRDPARTY_TINYXML
  THIRDPARTY_SOLVERIO)

# Module libs
set(SV_LIBS ${SV_LIBS}
  MODULE_COMMON
  MODULE_IMAGEPROCESSING
  MODULE_MESH
  MODULE_MESHSIM
  MODULE_ML
  MODULE_MODEL
  MODULE_MODEL_OCCT
  MODULE_MODEL_PARASOLID
  MODULE_PATH
  MODULE_PROJECTMANAGEMENT
  MODULE_QTWIDGETS
  MODULE_SEGMENTATION
  MODULE_SIMULATION
  MODULE_SIMULATION1D
  MODULE_SVFSI)

#if (SV_USE_ONEDSOLVER)
#  set(SV_LIBS ${SV_LIBS} MODULE_SIMULATION1D)
#endif()

# Solver libs
set(SV_LIBS ${SV_LIBS}
  PRESOLVER
  THREEDSOLVER_DUMMY_LESLIB
  THREEDSOLVER_FORTRAN
  THREEDSOLVER_WRITE_RESTART
  THREEDSOLVER_VTK
  POSTSOLVER
  SVLS)

# vtkSV Libraries
set(SV_LIBS ${SV_LIBS}
  VTKSVBOOLEAN
  VTKSVCOMMON
  VTKSVIO
  VTKSVMISC
  VTKSVGEOMETRY
  VTKSVNURBS
  VTKSVPARAMETERIZATION
  VTKSVSEGMENTATION)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Make library name
set(SV_LIB_NAMES )
foreach(lib ${SV_LIBS})
  if(WIN32)
    string(TOLOWER "lib_SIMVASCULAR_${lib}" SV_LIB_${lib}_NAME)
  else()
    string(TOLOWER "_SIMVASCULAR_${lib}" SV_LIB_${lib}_NAME)
  endif()
  list(APPEND SV_LIB_NAMES "${SV_LIB_${lib}_NAME}")
endforeach()
#-----------------------------------------------------------------------------
