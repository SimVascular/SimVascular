cmake_minimum_required(VERSION 2.8.8)

Project(SPARSE)
if (NOT SPARSE_LIBRARY_NAME)
    set(SPARSE_LIBRARY_NAME sparse)
endif()

set(HDRS spConfig.h spDefs.h spMatrix.h)
set(CSRCS spAllocate.c spBuild.c spFactor.c spOutput.c spSolve.c spUtils.c spFortran.c)

add_library(${SPARSE_LIBRARY_NAME} ${SV_LIBRARY_TYPE} ${CSRCS})

if(SV_INSTALL_LIBS)
	install(TARGETS ${SPARSE_LIBRARY_NAME}
		RUNTIME DESTINATION ${SV_INSTALL_RUNTIME_DIR} COMPONENT PluginExecutables
		LIBRARY DESTINATION ${SV_INSTALL_LIBRARY_DIR} COMPONENT PluginLibraries
		ARCHIVE DESTINATION ${SV_INSTALL_ARCHIVE_DIR} COMPONENT PluginLibraries
		)
endif()
if(SV_INSTALL_HEADERS)
	install(FILES ${HDRS}
		DESTINATION ${SV_INSTALL_INCLUDE_DIR}/thirdparty/sparse COMPONENT PluginHeaders
		)
endif()
