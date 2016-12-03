set(SRC_CPP_FILES

)

set(INTERNAL_CPP_FILES
    svSimJobCreate.cxx
    svSimJobCreateAction.cxx
    svCapBCWidget.cxx
    svTableCapDelegate.cxx
    svTableSolverDelegate.cxx
    svSimulationView.cxx
    svSimulationPreferencePage.cxx
    svSimulationPluginActivator.cxx
)

set(MOC_H_FILES
    src/internal/svSimJobCreate.h
    src/internal/svSimJobCreateAction.h
    src/internal/svCapBCWidget.h
    src/internal/svTableCapDelegate.h
    src/internal/svTableSolverDelegate.h
    src/internal/svSimulationView.h
    src/internal/svSimulationPreferencePage.h
    src/internal/svSimulationPluginActivator.h
)

set(UI_FILES
    src/internal/svSimJobCreate.ui
    src/internal/svCapBCWidget.ui
    src/internal/svSimulationPreferencePage.ui
    src/internal/svSimulationView.ui
)

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/simulation.png
)

set(QRC_FILES
  resources/simulation.qrc
)

set(CPP_FILES )

foreach(file ${SRC_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/${file})
endforeach(file ${SRC_CPP_FILES})

foreach(file ${INTERNAL_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/internal/${file})
endforeach(file ${INTERNAL_CPP_FILES})

