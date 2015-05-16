
SET(CXXSRCS main.cxx gdscInit.cxx cv_VTK_init.cxx)

UNSET(OPTLIBS)

if(NOT BUILD_SHARED_LIBS)
	SET(CXXSRCS ${CXXSRCS} 
		 ${SimVascular_SOURCE_DIR}/Core/Common/Geometry/cv_geom_init.cxx
                 ${SimVascular_SOURCE_DIR}/Core/Common/PostProcessing/cv_post_init.cxx
                 ${SimVascular_SOURCE_DIR}/Core/Common/Image/cv_image_init.cxx
                 ${SimVascular_SOURCE_DIR}/Core/Common/Repository/cv_repos_init.cxx
                 ${SimVascular_SOURCE_DIR}/Core/Model/SolidModel/cv_solid_init.cxx
                 ${SimVascular_SOURCE_DIR}/Core/Common/LevelSet/cv_LsetCore_init.cxx
                 ${SimVascular_SOURCE_DIR}/Core/Common/LevelSet/cv_LsetV_init.cxx
                 ${SimVascular_SOURCE_DIR}/Core/Common/Utils/cv_get_tcl_interp_init.cxx
                 ${SimVascular_SOURCE_DIR}/Core/Mesh/MeshObject/cv_mesh_init.cxx
                 ${SimVascular_SOURCE_DIR}/Core/Common/Utils/cv_math_init.cxx
		 ${SimVascular_SOURCE_DIR}/Core/Model/PolyDataSolid/cv_polydatasolid_init.cxx)
endif()

if(Plugin_TETGEN)
	include_directories(${TETGEN_INCLUDE_DIR})
	SET(CXXSRCS ${CXXSRCS}
	${SimVascular_SOURCE_DIR}/Core/Mesh/TetGenMeshObject/cv_tetgen_mesh_init.cxx)
	SET(OPTLIBS ${OPTLIBS} lib_simvascular_tetgen_mesh)
endif()

if(Licensed_MESHSIM)
	SET(CXXSRCS ${CXXSRCS} 
	${SimVascular_SOURCE_DIR}/Core/Mesh/MeshSimMeshObject/cv_meshsim_mesh_init.cxx)
	SET(OPTLIBS ${OPTLIBS} lib_simvascular_meshsim_mesh)
endif()

if(Licensed_MESHSIM_DISCRETE_SOLIDMODEL)
	SET(CXXSRCS ${CXXSRCS} 
	${SimVascular_SOURCE_DIR}/Core/Model/MeshSimDiscreteSolidModel/cv_discrete_init.cxx)
	SET(OPTLIBS ${OPTLIBS} lib_simvascular_discrete)
endif()

if(Licensed_PARASOLID)
	SET(CXXSRCS ${CXXSRCS} 
	${SimVascular_SOURCE_DIR}/Licensed/ParasolidSolidModel/cv_parasolid_solid_init.cxx)
	SET(OPTLIBS ${OPTLIBS} lib_simvascular_parasolid)
endif()

if(Plugin_ITK)
	include_directories(${SimVascular_SOURCE_DIR}/Core/Common/ITK/Include)
	include_directories(${SimVascular_SOURCE_DIR}/Core/Common/ITK/Util)
	SET(CXXSRCS ${CXXSRCS} ${SimVascular_SOURCE_DIR}/Core/Common/ITK/cv_ITKLset2d_init.cxx
							${SimVascular_SOURCE_DIR}/Core/Common/ITK/Util/cv_ITKUtils_init.cxx)
	SET(OPTLIBS ${OPTLIBS} lib_simvascular_cvitk lib_simvascular_itkutils)
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