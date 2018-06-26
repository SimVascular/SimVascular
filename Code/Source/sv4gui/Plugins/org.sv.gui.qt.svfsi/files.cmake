set(SRC_CPP_FILES

)

set(INTERNAL_CPP_FILES
    sv4gui_svFSIJobCreateAction.cxx
    sv4gui_svFSIView.cxx
    sv4gui_svFSIBCWidget.cxx
    sv4gui_svFSIPreferencePage.cxx
    sv4gui_svFSIPluginActivator.cxx
)

set(MOC_H_FILES
    sv4gui_svFSIJobCreateAction.h
    sv4gui_svFSIUtil.h
    sv4gui_svFSIView.h
    sv4gui_svFSIBCWidget.h
    sv4gui_svFSIPreferencePage.h
    sv4gui_svFSIPluginActivator.h
)

set(UI_FILES
    sv4gui_svFSIView.ui
    sv4gui_svFSIBCWidget.ui
    sv4gui_svFSIPreferencePage.ui
)

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/svFSI.png
)

set(QRC_FILES
  resources/datanode.qrc
)

set(CPP_FILES )

foreach(file ${SRC_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} ${file})
endforeach(file ${SRC_CPP_FILES})

foreach(file ${INTERNAL_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} ${file})
endforeach(file ${INTERNAL_CPP_FILES})
