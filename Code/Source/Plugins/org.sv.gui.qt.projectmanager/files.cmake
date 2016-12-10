set(SRC_CPP_FILES
  
)

set(INTERNAL_CPP_FILES
  svProjectManagerPluginActivator.cxx
#  svProjectCreate.cxx
#  svProjectManagerView.cxx
  svProjectAddImageAction.cxx
  svProjectCloseAction.cxx
  svProjectShowModelEdgesAction.cxx
)

set(MOC_H_FILES
  src/internal/svProjectManagerPluginActivator.h
#  src/internal/svProjectCreate.h
#  src/internal/svProjectManagerView.h
  src/internal/svProjectAddImageAction.h
  src/internal/svProjectCloseAction.h
  src/internal/svProjectShowModelEdgesAction.h
)

set(UI_FILES
#  src/internal/svProjectCreate.ui
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

