SET(HDRS itkVascularPhaseOneLevelSetFunction.hxx
	itkVascularPhaseOneLevelSetImageFilter.hxx
	itkVascularPhaseTwoLevelSetFunction.hxx
	itkVascularPhaseTwoLevelSetImageFilter.hxx
	VascularLevelSetObserver.h)
if(SIMVASCULAR_INSTALL_HEADERS)
	install(FILES ${HDRS}
		DESTINATION ${SIMVASCULAR_INSTALL_INCLUDE_DIR}/plugins/itk/levelset COMPONENT PluginHeaders
		)
endif()

