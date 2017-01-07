set(HDRS itkVascularPhaseOneLevelSetFunction.hxx
	itkVascularPhaseOneLevelSetImageFilter.hxx
	itkVascularPhaseTwoLevelSetFunction.hxx
	itkVascularPhaseTwoLevelSetImageFilter.hxx
	VascularLevelSetObserver.h)

add_definitions("-DSV_EXPORT_SEGITKUTILS_COMPILE")

if(SV_INSTALL_HEADERS)
	install(FILES ${HDRS}
		DESTINATION ${SV_INSTALL_INCLUDE_DIR}/plugins/itk/levelset COMPONENT PluginHeaders
		)
endif()

