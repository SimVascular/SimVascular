set(SRC_CPP_FILES
    svPathCreate.cxx
    svPathSmooth.cxx
    svPathEdit.cxx
    svPathPlanningPluginActivator.cxx
)

set(INTERNAL_CPP_FILES
)

set(MOC_H_FILES
    src/svPathCreate.h
    src/svPathSmooth.h
    src/svPathEdit.h
    src/svPathPlanningPluginActivator.h
)

set(UI_FILES
    src/svPathCreate.ui
    src/svPathSmooth.ui
    src/svPathEdit.ui
)

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/pathedit.png
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

