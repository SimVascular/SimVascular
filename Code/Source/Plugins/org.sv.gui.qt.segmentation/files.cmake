set(SRC_CPP_FILES

)

set(INTERNAL_CPP_FILES
    svContourGroupCreate.cxx
    svContourGroupCreateAction.cxx
    svSegmentationLegacyLoadAction.cxx
    svSegmentationLegacySaveAction.cxx
    svLevelSet2DWidget.cxx
    svLoftParamWidget.cxx
    svSegmentation2D.cxx
    svSegmentationPluginActivator.cxx
)

set(MOC_H_FILES
    src/internal/svContourGroupCreate.h
    src/internal/svContourGroupCreateAction.h
    src/internal/svSegmentationLegacyLoadAction.h
    src/internal/svSegmentationLegacySaveAction.h
    src/internal/svLevelSet2DWidget.h
    src/internal/svLoftParamWidget.h
    src/internal/svSegmentation2D.h
    src/internal/svSegmentationPluginActivator.h
)

set(UI_FILES
    src/internal/svContourGroupCreate.ui
    src/internal/svLevelSet2DWidget.ui
    src/internal/svLoftParamWidget.ui
    src/internal/svSegmentation2D.ui
)

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/contourgroup.png
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

