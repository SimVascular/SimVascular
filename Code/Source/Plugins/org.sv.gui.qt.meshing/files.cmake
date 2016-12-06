set(SRC_CPP_FILES

)

set(INTERNAL_CPP_FILES
    svMeshCreate.cxx
    svMeshCreateAction.cxx
    svMeshEdit.cxx
    svVtkMeshSphereWidget.cxx
    svMeshLegacySaveAction.cxx
    svMeshLoadSurfaceAction.cxx
    svMeshLoadVolumeAction.cxx
    svMeshingPluginActivator.cxx
)

set(MOC_H_FILES
    src/internal/svMeshCreate.h
    src/internal/svMeshCreateAction.h
    src/internal/svMeshEdit.h
    src/internal/svMeshLegacySaveAction.h
    src/internal/svMeshLoadSurfaceAction.h
    src/internal/svMeshLoadVolumeAction.h
    src/internal/svMeshingPluginActivator.h
)

set(UI_FILES
    src/internal/svMeshCreate.ui
    src/internal/svMeshEdit.ui
)

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/mesh.png
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

