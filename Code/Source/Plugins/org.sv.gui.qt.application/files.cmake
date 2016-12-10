set(SRC_CPP_FILES
)

set(INTERNAL_CPP_FILES
  svApplication.cxx
  svApplicationPluginActivator.cxx
  svFileCreateProjectAction.cxx
  svFileOpenProjectAction.cxx
  svFileSaveProjectAction.cxx
  svProjectCreate.cxx
  svWorkbenchWindowAdvisor.cxx
  svAppWorkbenchAdvisor.cxx
  svAboutDialog.cxx
  svWorkbenchIntroPart.cxx
  svDefaultPerspective.cxx
)

set(MOC_H_FILES
  src/internal/svApplication.h
  src/internal/svApplicationPluginActivator.h
  src/internal/svFileCreateProjectAction.h
  src/internal/svFileOpenProjectAction.h
  src/internal/svFileSaveProjectAction.h
  src/internal/svProjectCreate.h
  src/internal/svWorkbenchWindowAdvisor.h
  src/internal/svWorkbenchWindowAdvisorHack.h
  src/internal/svAboutDialog.h
  src/internal/svWorkbenchIntroPart.h
  src/internal/svDefaultPerspective.h
)

set(UI_FILES
  src/internal/svProjectCreate.ui
  src/internal/svAboutDialog.ui
  src/internal/svWelcomeScreenViewControls.ui
)

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/icon.png
)

set(QRC_FILES
resources/svApplication.qrc
resources/welcome/svWelcomeScreenView.qrc
)

set(CPP_FILES )

foreach(file ${SRC_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/${file})
endforeach(file ${SRC_CPP_FILES})

foreach(file ${INTERNAL_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/internal/${file})
endforeach(file ${INTERNAL_CPP_FILES})

