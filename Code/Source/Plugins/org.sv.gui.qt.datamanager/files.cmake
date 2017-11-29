set(INTERNAL_CPP_FILES
  svberrySingleNodeSelection.cxx
  svQmitkDataManagerView.cxx
  svQmitkDataManagerPreferencePage.cxx
  svQmitkDataManagerHotkeysPrefPage.cxx
  svDataManagerPluginActivator.cxx
  svQmitkNodeTableViewKeyFilter.cxx
  svQmitkInfoDialog.cxx
  svQmitkDataManagerItemDelegate.cxx
  svQmitkFunctionality.cxx
)

set(MOC_H_FILES
  src/internal/svQmitkDataManagerView.h
  src/internal/svQmitkDataManagerPreferencePage.h
  src/internal/svQmitkDataManagerHotkeysPrefPage.h
  src/internal/svQmitkNodeTableViewKeyFilter.h
  src/internal/svQmitkInfoDialog.h
  src/internal/svQmitkDataManagerItemDelegate.h
  src/internal/svDataManagerPluginActivator.h
  src/internal/svmitkIContextMenuAction.h
  src/internal/svQmitkFunctionality.h
)

set(CPP_FILES )

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/DataManager_48.png
)

set(QRC_FILES
  resources/svdatamanager.qrc
)

foreach(file ${SRC_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/${file})
endforeach(file ${SRC_CPP_FILES})

foreach(file ${INTERNAL_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/internal/${file})
endforeach(file ${INTERNAL_CPP_FILES})
