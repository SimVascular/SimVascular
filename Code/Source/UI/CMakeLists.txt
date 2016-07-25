
SET(CXXSRCS main.cxx SimVascular_Init.cxx)

UNSET(OPTLIBS)

if(NOT ${USE_SYSTEM_TCL})
	include_directories(${TCL_INCLUDE_PATH})
	include_directories(${TK_INCLUDE_PATH})
endif()

if(SV_USE_TETGEN)
	SET(OPTLIBS ${OPTLIBS} lib_simvascular_tetgen_mesh)
endif()
#
if(SV_USE_MMG)
	SET(OPTLIBS ${OPTLIBS} lib_simvascular_mmg_mesh)
endif()

if(SV_USE_MESHSIM)
	if (NOT SV_USE_MESHSIM_SHARED_LIBRARIES)
	  SET(OPTLIBS ${OPTLIBS} lib_simvascular_meshsim_mesh)
        endif()
endif()

if(SV_USE_MESHSIM_DISCRETE_MODEL)
	if (NOT SV_USE_MESHSIM_DISCRETE_SHARED_LIBRARIES)
	  SET(OPTLIBS ${OPTLIBS} lib_simvascular_discrete)
        endif()
endif()

if(SV_USE_PARASOLID)
	if (NOT SV_USE_PARASOLID_SHARED_LIBRARIES)
	  SET(OPTLIBS ${OPTLIBS} lib_simvascular_parasolid)
        endif()
endif()

if(SV_USE_OpenCASCADE)
	SET(OPTLIBS ${OPTLIBS} lib_simvascular_opencascade)
endif()

if(SV_USE_ITK)
	SET(OPTLIBS ${OPTLIBS} lib_simvascular_itklset)
  endif()

if(SV_USE_TET_ADAPTOR)
	SET(OPTLIBS ${OPTLIBS} lib_simvascular_adaptor)
	SET(OPTLIBS ${OPTLIBS} lib_simvascular_tet_adaptor)
endif()

if (SV_USE_MESHSIM_ADAPTOR)
	if(NOT SV_USE_MESHSIM_SHARED_LIBRARIES)
	  SET(OPTLIBS ${OPTLIBS} lib_simvascular_adaptor)
	  SET(OPTLIBS ${OPTLIBS} lib_simvascular_meshsim_adaptor)
        endif()
endif()

if (SV_USE_PYTHON)
	SET(OPTLIBS ${OPTLIBS} lib_simvascular_tclpython)
endif()


simvascular_add_executable(${SV_EXE}
	SRCS ${CXXSRCS}
	DEV_SCRIPT_NAME "mysim"
	INSTALL_SCRIPT_NAME "simvascular"
	INSTALL_COMPONENT CoreExecutables
	INSTALL_DESTINATION ${SV_INSTALL_RUNTIME_DIR})
#message("OPTLIBS ${OPTLIBS}")

target_link_libraries(${SV_EXE}
			lib_simvascular_post lib_simvascular_image
			lib_simvascular_repository lib_simvascular_lset
			lib_simvascular_solid lib_simvascular_mesh
			lib_simvascular_sysgeom lib_simvascular_utils
			lib_simvascular_globals lib_simvascular_polydatasolid)

target_link_libraries(${SV_EXE} ${OPTLIBS}
			${VTK_LIBRARIES} ${TCL_LIBRARY} ${TK_LIBRARY}
			${PARASOLID_LIBRARIES} ${MESHSIM_LIBRARIES}
			${OpenCASCADE_LIBRARIES}
			${ZLIB_LIBRARY} ${ITK_LIBRARIES}
			${INTELRUNTIME_LIBRARIES}
			${GLOBAL_LIBRARIES}
			${MMG_LIBRARY}
			${FREETYPE_LIBRARY}
                        ${PYTHON_LIBRARY})

