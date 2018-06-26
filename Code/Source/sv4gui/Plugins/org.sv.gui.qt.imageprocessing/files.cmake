set(SRC_CPP_FILES

)

set(INTERNAL_CPP_FILES
    sv4gui_ImageProcessing.cxx
    sv4gui_ImageSeedContainer.cxx
    sv4gui_ImageSeedMapper.cxx
    sv4gui_ImageSeedInteractor.cxx
    sv4gui_ImageProcessingPluginActivator.cxx
)

set(MOC_H_FILES
    sv4gui_ImageProcessing.h
    sv4gui_ImageSeedContainer.h
    sv4gui_ImageSeedMapper.h
    sv4gui_ImageSeedInteractor.h
    sv4gui_ImageProcessingPluginActivator.h
)

set(UI_FILES
    sv4gui_ImageProcessing.ui
)

set(CACHED_RESOURCE_FILES
  plugin.xml
  resources/image_processing.png
)

set(QRC_FILES

)

set(CPP_FILES )

foreach(file ${SRC_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} ${file})
endforeach(file ${SRC_CPP_FILES})

foreach(file ${INTERNAL_CPP_FILES})
  set(CPP_FILES ${CPP_FILES} ${file})
endforeach(file ${INTERNAL_CPP_FILES})
