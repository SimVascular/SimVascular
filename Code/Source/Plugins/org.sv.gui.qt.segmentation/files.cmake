set(SRC_CPP_FILES

)

set(INTERNAL_CPP_FILES
    svLoftingUtils.cxx
    svContourGroupCreate.cxx
    svContourGroupCreateAction.cxx
    svSegmentationLegacyLoadAction.cxx
    svSegmentationLegacySaveAction.cxx
    svSegmentationLoadAction.cxx
    svLevelSet2DWidget.cxx
    svLoftParamWidget.cxx
    svSeg2DEdit.cxx
    svContourGroupPoint2DSizeAction.cxx
    svContourGroupPoint3DSizeAction.cxx
    svSeg3DCreateAction.cxx
    svSeg3DEdit.cxx
    svSegmentationPluginActivator.cxx
)

set(MOC_H_FILES
    src/internal/svLoftingUtils.h
    src/internal/svContourGroupCreate.h
    src/internal/svContourGroupCreateAction.h
    src/internal/svSegmentationLegacyLoadAction.h
    src/internal/svSegmentationLegacySaveAction.h
    src/internal/svSegmentationLoadAction.h
    src/internal/svLevelSet2DWidget.h
    src/internal/svLoftParamWidget.h
    src/internal/svSeg2DEdit.h
    src/internal/svContourGroupPoint2DSizeAction.h
    src/internal/svContourGroupPoint3DSizeAction.h
    src/internal/svSeg3DCreateAction.h
    src/internal/svSeg3DEdit.h
    src/internal/svSegmentationPluginActivator.h
)

set(UI_FILES
    src/internal/svContourGroupCreate.ui
    src/internal/svLevelSet2DWidget.ui
    src/internal/svLoftParamWidget.ui
    src/internal/svSeg2DEdit.ui
    src/internal/svSeg3DEdit.ui
)

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/contourgroup.png
  resources/svseg3d.png
)

set(QRC_FILES

)

set(CPP_FILES )

foreach(file ${SRC_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/${file})
endforeach(file ${SRC_CPP_FILES})

foreach(file ${INTERNAL_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/internal/${file})
endforeach(file ${INTERNAL_CPP_FILES})

