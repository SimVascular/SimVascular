#-----------------------------------------------------------------------------
# ThirdParty!
#-----------------------------------------------------------------------------
# ZLIB
if(SV_USE_ZLIB)
	SET(USE_ZLIB ON)
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
	if(NOT SV_USE_SYSTEM_TETGEN)
		set(TETGEN_VERSION "1.5.1")
    set(TETGEN_LIBRARY ${SV_LIB_THIRDPARTY_TETGEN_NAME})
	else()
		find_package(TETGEN)
	endif()
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
# Add libraries for flowsolver
# SPARSE
if(SV_USE_SPARSE)
	set(USE_SPARSE ON)
    simvascular_third_party(sparse SOLVER_DEPEND)
	if(NOT SV_USE_SYSTEM_SPARSE)
    set(SPARSE_LIBRARY ${SV_LIB_THIRDPARTY_SPARSE_NAME})
	else()
		find_package(SPARSE)
	endif()
endif()

# METIS
if(SV_USE_METIS)
	set(USE_METIS ON)
    simvascular_third_party(metis SOLVER_DEPEND)
	if(NOT SV_USE_SYSTEM_METIS)
    set(METIS_LIBRARY ${SV_LIB_THIRDPARTY_METIS_NAME})
	else()
		find_package(METIS)
	endif()
endif()

# NSPCG
if(SV_USE_NSPCG)
	set(USE_NSPCG ON)
    simvascular_third_party(nspcg SOLVER_DEPEND)
	if(NOT SV_USE_SYSTEM_NSPCG)
    set(NSPCG_LIBRARY ${SV_LIB_THIRDPARTY_NSPCG_NAME})
	else()
		find_package(NSPCG)
	endif()
endif()

