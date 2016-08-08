set(lib ${SVLS_LIBRARY})


SET(FSRCS ADDBCMUL.f
		BC.f
		#BICGS.f
		CGRAD.f
		COMMU.f
		CPUT.f
		DOT.f
		GE.f
		GMRES.f
		INCOMMU.f
		LHS.f
		LS.f
		NORM.f
		NSSOLVER.f
		#OMPLA.f
		PRECOND.f
		SOLVE.f
		SPARMUL.f)


if(SV_USE_THREEDSOLVER_SHARED_LIBRARIES)
  add_library(${lib} SHARED ${FSRCS})
else()
  add_library(${lib} STATIC ${FSRCS})
endif()

target_link_libraries(${lib} ${MPI_LIBRARY} ${MPI_Fortran_LIBRARIES})

if(SV_INSTALL_LIBS)
	install(TARGETS ${lib}
		RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR} COMPONENT ModuleExecutables
		LIBRARY DESTINATION ${SV_INSTALL_LIBRARY_DIR} COMPONENT ModuleLibraries
		ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR} COMPONENT ModuleLibraries
		)
endif()
if(SV_INSTALL_HEADERS)
	install(FILES ${HDRS}
		DESTINATION ${SV_INSTALL_INCLUDE_DIR}/core COMPONENT CoreHeaders
		)
endif()



