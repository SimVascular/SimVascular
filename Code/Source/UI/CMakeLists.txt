
SET(CXXSRCS main.cxx gdscInit.cxx cv_VTK_init.cxx)

UNSET(OPTLIBS)

if(NOT BUILD_SHARED_LIBS)
	SET(CXXSRCS ${CXXSRCS} 
		 ${SimVascular_SOURCE_DIR}/Source/Common/Geometry/cv_geom_init.cxx
                 ${SimVascular_SOURCE_DIR}/Source/PostProcessing/cv_post_init.cxx
                 ${SimVascular_SOURCE_DIR}/Source/ImageProcessing/cv_image_init.cxx
                 ${SimVascular_SOURCE_DIR}/Source/Common/Repository/cv_repos_init.cxx
                 ${SimVascular_SOURCE_DIR}/Source/Model/SolidModel/cv_solid_init.cxx
                 ${SimVascular_SOURCE_DIR}/Source/Legacy/LevelSet/cv_LsetCore_init.cxx
                 ${SimVascular_SOURCE_DIR}/Source/Legacy/LevelSet/cv_LsetV_init.cxx
                 ${SimVascular_SOURCE_DIR}/Source/Common/Utils/cv_get_tcl_interp_init.cxx
                 ${SimVascular_SOURCE_DIR}/Source/Mesh/MeshObject/cv_mesh_init.cxx
                 ${SimVascular_SOURCE_DIR}/Source/Common/Utils/cv_math_init.cxx
		 ${SimVascular_SOURCE_DIR}/Source/Model/PolyDataSolidModel/cv_polydatasolid_init.cxx)
endif()

if(Plugin_TETGEN)
	include_directories(${TETGEN_INCLUDE_DIR})
	SET(CXXSRCS ${CXXSRCS}
	${SimVascular_SOURCE_DIR}/Source/Mesh/TetGenMeshObject/cv_tetgen_mesh_init.cxx)
	SET(OPTLIBS ${OPTLIBS} lib_simvascular_tetgen_mesh)
endif()

if(Licensed_MESHSIM)
	SET(CXXSRCS ${CXXSRCS} 
	${SimVascular_SOURCE_DIR}/Source/Mesh/MeshSimMeshObject/cv_meshsim_mesh_init.cxx)
	SET(OPTLIBS ${OPTLIBS} lib_simvascular_meshsim_mesh)
endif()

if(Licensed_MESHSIM_DISCRETE_SOLIDMODEL)
	SET(CXXSRCS ${CXXSRCS} 
	${SimVascular_SOURCE_DIR}/Source/Model/MeshSimDiscreteSolidModel/cv_discrete_init.cxx)
	SET(OPTLIBS ${OPTLIBS} lib_simvascular_discrete)
endif()

if(Licensed_PARASOLID)
	SET(CXXSRCS ${CXXSRCS} 
	${SimVascular_SOURCE_DIR}/Licensed/ParasolidSolidModel/cv_parasolid_solid_init.cxx)
	SET(OPTLIBS ${OPTLIBS} lib_simvascular_parasolid)
endif()

if(Plugin_ITK)
	include_directories(${SimVascular_SOURCE_DIR}/Source/Segmentation/ITK/Include)
	include_directories(${SimVascular_SOURCE_DIR}/Source/Segmentation/ITK/Util)
	SET(CXXSRCS ${CXXSRCS} ${SimVascular_SOURCE_DIR}/Source/Segmentation/ITK/cv_ITKLset2d_init.cxx
							${SimVascular_SOURCE_DIR}/Source/Segmentation/ITK/Util/cv_ITKUtils_init.cxx)
	SET(OPTLIBS ${OPTLIBS} lib_simvascular_cvitk lib_simvascular_itkutils)
  endif()

if(Plugin_TET_ADAPTOR)
	SET(CXXSRCS ${CXXSRCS} 
	${SimVascular_SOURCE_DIR}/Source/Mesh/AdaptObject/cv_adapt_init.cxx
	${SimVascular_SOURCE_DIR}/Source/Mesh/TetGenAdapt/cv_tetgen_adapt_init.cxx)
	SET(OPTLIBS ${OPTLIBS} lib_simvascular_adaptor)
	SET(OPTLIBS ${OPTLIBS} lib_simvascular_tet_adaptor)
endif()

if (Licensed_ADAPTOR)
	set(CXXSRCS ${CXXSRCS}
	${SimVascular_SOURCE_DIR}/Source/Mesh/AdaptObject/cv_adapt_init.cxx
	${SimVascular_SOURCE_DIR}/Source/Mesh/MeshSimAdapt/cv_meshsim_adapt_init.cxx)
	SET(OPTLIBS ${OPTLIBS} lib_simvascular_adaptor)
	SET(OPTLIBS ${OPTLIBS} lib_simvascular_meshsim_adaptor)
endif()

simvascular_add_executable(${SIMVASCULAR_EXE} 
	SRCS ${CXXSRCS} 
	DEV_SCRIPT_NAME "mysim" 
	INSTALL_SCRIPT_NAME "simvascular"
	INSTALL_COMPONENT CoreExecutables
	INSTALL_DESTINATION ${SIMVASCULAR_INSTALL_RUNTIME_DIR})
#message("OPTLIBS ${OPTLIBS}")

target_link_libraries(${SIMVASCULAR_EXE}
			lib_simvascular_post lib_simvascular_image
			lib_simvascular_repository lib_simvascular_lset
			lib_simvascular_solid lib_simvascular_mesh
			lib_simvascular_sysgeom lib_simvascular_utils
			lib_simvascular_globals lib_simvascular_polydatasolid)

target_link_libraries(${SIMVASCULAR_EXE} ${OPTLIBS}
			${VTK_LIBRARIES} ${TCL_LIBRARY} ${TK_LIBRARY}
			${PARASOLID_LIBRARIES} ${MESHSIM_LIBRARIES}
			${ZLIB_LIBRARY} ${ITK_LIBRARIES}
			${INTELRUNTIME_LIBRARIES}
			${GLOBAL_LIBRARIES})
