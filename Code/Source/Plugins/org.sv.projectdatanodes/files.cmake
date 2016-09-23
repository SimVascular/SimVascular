set(SRC_CPP_FILES
  
)

set(INTERNAL_CPP_FILES
  svProjectDataNodesPluginActivator.cxx
)

set(MOC_H_FILES
  src/internal/svProjectDataNodesPluginActivator.h
)

set(QRC_FILES
  resources/projectdatanodes.qrc
)

set(CPP_FILES )

foreach(file ${SRC_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/${file})
endforeach(file ${SRC_CPP_FILES})

foreach(file ${INTERNAL_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} src/internal/${file})
endforeach(file ${INTERNAL_CPP_FILES})

