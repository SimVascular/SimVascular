set(H_FILES
    svModelUtils.h
    svModelElement.h
    svModelElementPolyData.h
    svModelOperation.h
    svModel.h
    svModelVtkMapper2D.h
    svModelVtkMapper3D.h
    svModelDataInteractor.h
    svModelIO.h
    svModelLegacyIO.h
    svModelObjectFactory.h
)

if(SV_USE_OpenCASCADE)
  set(H_FILES
      ${H_FILES}
      svModelElementOCCT.h
  )
endif()

set(CPP_FILES
    svModelUtils.cxx
    svModelElement.cxx
    svModelElementPolyData.cxx
    svModelOperation.cxx
    svModel.cxx
    svModelVtkMapper2D.cxx
    svModelVtkMapper3D.cxx
    svModelDataInteractor.cxx
    svModelIO.cxx
    svModelLegacyIO.cxx
    svModelObjectFactory.cxx
)

if(SV_USE_OpenCASCADE)
  set(CPP_FILES
      ${CPP_FILES}
      svModelElementOCCT.cxx
  )
endif()

set(RESOURCE_FILES
    Interactions/svModelInteraction.xml
    Interactions/svModelConfig.xml
)
