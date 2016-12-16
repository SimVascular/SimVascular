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
  PYTHON_INTERP
  REPOSITORY
  SOLID
  TCL_INTERP
  TETGEN_MESH
  TETGEN_ADAPTOR
  UTILS)

# Thirdparty libs
set(SV_LIBS ${SV_LIBS}
  THIRDPARTY_METIS
  THIRDPARTY_NSPCG
  THIRDPARTY_SPARSE
  THIRDPARTY_TETGEN
  THIRDPARTY_VMTK
  THIRDPARTY_ZLIB)

# Module libs
set(SV_LIBS ${SV_LIBS}
  MODULE_COMMON
  MODULE_MESH
  MODULE_MODEL
  MODULE_PATH
  MODULE_PROJECTMANAGEMENT
  MODULE_QTWIDGETS
  MODULE_SEGMENTATION
  MODULE_SIMULATION)

# Solver libs
set(SV_LIBS ${SV_LIBS}
  PRESOLVER
  THREEDSOLVER_DUMMY_LESLIB
  THREEDSOLVER_FORTRAN
  THREEDSOLVER_WRITE_RESTART
  THREEDSOLVER_VTK
  POSTSOLVER
  SVLS
  SOLVERIO)
#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# Make library name
foreach(lib ${SV_LIBS})
  string(TOLOWER "_SIMVASCULAR_${lib}" SV_LIB_${lib}_NAME)
endforeach()
#-----------------------------------------------------------------------------
