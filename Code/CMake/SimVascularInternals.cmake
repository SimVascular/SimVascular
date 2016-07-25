set(SV_LIBS ADAPTOR
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
            UTILS
            THIRDPARTY_METIS
            THIRDPARTY_NSPCG
            THIRDPARTY_SPARSE
            THIRDPARTY_TETGEN
            THIRDPARTY_VMTK
            THIRDPARTY_ZLIB
            PRESOLVER
            THREEDSOLVER_DUMMY_LESLIB
            THREEDSOLVER_FORTRAN
            THREEDSOLVER_WRITE_RESTART
            THREEDSOLVER_VTK
            POSTSOLVER
            SVLS
            SOLVERIO)

foreach(lib ${SV_LIBS})
  string(TOLOWER "SIMVASCULAR_${lib}" SV_LIB_${lib}_NAME)
  mark_as_superbuild(SV_LIB_${lib}_NAME)
  #set(SV_LIB_${lib}_TYPE "STATIC" CACHE STRING "Options are STATIC or SHARED")
  #if("${SV_LIBRARY_TYPE}" STREQUAL "ALL_STATIC")
  #  set(SV_LIB_${lib}_TYPE "STATIC" CACHE STRING "Options are STATIC or SHARED" FORCE)
  #endif()
  #if("${SV_LIBRARY_TYPE}" STREQUAL "ALL_SHARED")
  #  set(SV_LIB_${lib}_TYPE "SHARED" CACHE STRING "Options are STATIC or SHARED" FORCE)
  #endif()
  #set_property(CACHE SV_LIB_${lib}_TYPE PROPERTY STRINGS STATIC SHARED)
  #mark_as_superbuild(SV_LIB_${lib}_TYPE)
  #mark_as_advanced(SV_LIB_${lib}_TYPE)
endforeach()



                
