#-----------------------------------------------------------------------------
# Include
#-----------------------------------------------------------------------------

getListOfVarsPrefix(USE_ _VARLIST)
dev_print_vars(_VARLIST)

configure_file(simvascular_version.h.in ${CMAKE_CURRENT_BINARY_DIR}/simvascular_version.h)

configure_file(simvascular_options.h.in ${CMAKE_CURRENT_BINARY_DIR}/simvascular_options.h)

set(HDRS cvIOstream.h cvVTK.h SimVascular.h)
if(SV_INSTALL_HEADERS)
	install(FILES ${HDRS}
		DESTINATION ${SV_INSTALL_INCLUDE_DIR} COMPONENT CoreHeaders
		)
endif()

install(FILES ${CMAKE_CURRENT_BINARY_DIR}/simvascular_options.h ${CMAKE_CURRENT_BINARY_DIR}/simvascular_version.h
	DESTINATION ${SV_INSTALL_INCLUDE_DIR} COMPONENT CoreHeaders
	)
