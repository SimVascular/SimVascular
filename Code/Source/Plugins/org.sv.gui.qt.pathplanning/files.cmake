set(SRC_CPP_FILES

)

set(INTERNAL_CPP_FILES
    svPathCreate.cxx
    svPathCreateAction.cxx
    svPathLegacyLoadAction.cxx
    svPathLegacySaveAction.cxx
    svPathSmooth.cxx
    svPathEdit.cxx
    svPathPlanningPluginActivator.cxx
)

set(MOC_H_FILES
    src/internal/svPathCreate.h
    src/internal/svPathCreateAction.h
    src/internal/svPathLegacyLoadAction.h
    src/internal/svPathLegacySaveAction.h
    src/internal/svPathSmooth.h
    src/internal/svPathEdit.h
    src/internal/svPathPlanningPluginActivator.h
)

set(UI_FILES
    src/internal/svPathCreate.ui
    src/internal/svPathSmooth.ui
    src/internal/svPathEdit.ui
)

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/pathedit.png
)

set(QRC_FILES
  resources/pathplanning.qrc
)

set(CPP_FILES )

foreach(file ${SRC_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/${file})
endforeach(file ${SRC_CPP_FILES})

foreach(file ${INTERNAL_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/internal/${file})
endforeach(file ${INTERNAL_CPP_FILES})

