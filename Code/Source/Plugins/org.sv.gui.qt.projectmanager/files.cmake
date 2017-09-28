set(SRC_CPP_FILES

)

set(INTERNAL_CPP_FILES
  svProjectManagerPluginActivator.cxx
  svProjectAddImageAction.cxx
  svProjectCloseAction.cxx
  svProjectSaveAction.cxx
  svProjectDuplicateAction.cxx
  svProjectShowModelEdgesAction.cxx
  svProjectShowModelFullAction.cxx
)

set(MOC_H_FILES
  src/internal/svProjectManagerPluginActivator.h
  src/internal/svProjectAddImageAction.h
  src/internal/svProjectCloseAction.h
  src/internal/svProjectSaveAction.h
  src/internal/svProjectDuplicateAction.h
  src/internal/svProjectShowModelEdgesAction.h
  src/internal/svProjectShowModelFullAction.h
)

set(UI_FILES
)

set(CACHED_RESOURCE_FILES
  resources/icon.xpm
  plugin.xml
)

set(QRC_FILES
  resources/projectmanager.qrc
)

set(CPP_FILES )

foreach(file ${SRC_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/${file})
endforeach(file ${SRC_CPP_FILES})

foreach(file ${INTERNAL_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/internal/${file})
endforeach(file ${INTERNAL_CPP_FILES})

