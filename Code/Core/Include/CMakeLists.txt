#-----------------------------------------------------------------------------
# Include
#-----------------------------------------------------------------------------

getListOfVarsPrefix(USE_ _VARLIST)
dev_print_vars(_VARLIST)

configure_file(simvascular_version.h.in ${CMAKE_CURRENT_BINARY_DIR}/simvascular_version.h)

configure_file(simvascular_options.h.in ${CMAKE_CURRENT_BINARY_DIR}/simvascular_options.h)

SET(HDRS cvIOstream.h cvVTK.h SimVascular.h
	)
if(SIMVASCULAR_INSTALL_HEADERS)
	install(FILES ${HDRS}
		DESTINATION ${SIMVASCULAR_INSTALL_INCLUDE_DIR} COMPONENT CoreHeaders
		)
endif()

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/simvascular_options.h ${CMAKE_CURRENT_BINARY_DIR}/simvascular_version.h
	DESTINATION ${SIMVASCULAR_INSTALL_INCLUDE_DIR} COMPONENT CoreHeaders
	)