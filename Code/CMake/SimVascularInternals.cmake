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
            SOLVERIO
            QTGUI_PLUGIN_IMAGE
            QTGUI_PLUGIN_MITKSEGMENTATION
            QTGUI_PLUGIN_GENERAL
            QTGUI_PLUGIN_PATHPLANNING
            QTGUI_PLUGIN_MODELING
            QTGUI_PLUGIN_SEGMENTATION
            QTGUI_PLUGIN_TEST
            QTGUI_MODULE_COMMON
            QTGUI_MODULE_MODEL
            QTGUI_MODULE_PATH
            QTGUI_MODULE_PROJECTMANAGEMENT
            QTGUI_MODULE_APPBASE
            QTGUI_MODULE_QTWIDGETS
            QTGUI_SVWORKBENCH
            QTGUI_MODULE_SEGMENTATION)

foreach(lib ${SV_LIBS})
  string(TOLOWER "_SIMVASCULAR_${lib}" SV_LIB_${lib}_NAME)
endforeach()




