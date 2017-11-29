set(SRC_CPP_FILES

)

set(INTERNAL_CPP_FILES
    svModelCreate.cxx
    svModelCreateAction.cxx
    svModelLoadAction.cxx
    svModelLegacySaveAction.cxx
    svModelExtractPathsAction.cxx
    svSegSelectionWidget.cxx
    svModelEdit.cxx
    svFaceListDelegate.cxx
    svModelFaceInfoExportAction.cxx
    svModelingPluginActivator.cxx
    svLoftingPreferencePage.cxx
    svCapSelectionWidget.cxx
)

set(MOC_H_FILES
    src/internal/svModelCreate.h
    src/internal/svModelCreateAction.h
    src/internal/svModelLoadAction.h
    src/internal/svModelLegacySaveAction.h
    src/internal/svModelExtractPathsAction.h
    src/internal/svSegSelectionWidget.h
    src/internal/svModelEdit.h
    src/internal/svFaceListDelegate.h
    src/internal/svModelFaceInfoExportAction.h
    src/internal/svModelingPluginActivator.h
    src/internal/svLoftingPreferencePage.h
    src/internal/svCapSelectionWidget.h
)

set(UI_FILES
    src/internal/svModelCreate.ui
    src/internal/svSegSelectionWidget.ui
    src/internal/svModelEdit.ui
    src/internal/svCapSelectionWidget.ui
)

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/model.png
)

set(QRC_FILES
  resources/modeling.qrc
)

set(CPP_FILES )

foreach(file ${SRC_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/${file})
endforeach(file ${SRC_CPP_FILES})

foreach(file ${INTERNAL_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/internal/${file})
endforeach(file ${INTERNAL_CPP_FILES})

