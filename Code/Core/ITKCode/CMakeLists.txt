
SET(CXXSRCS )


add_library(lib_simvascular_itkcode ${CXXSRCS})

target_link_libraries(lib_simvascular_itkcode
			${ITK_LIBRARIES} ${VTK_LIBRARIES} ${TCL_LIBRARY} ${TK_LIBRARY}
			${GLOBAL_LIBRARIES}
			lib_simvascular_globals)
