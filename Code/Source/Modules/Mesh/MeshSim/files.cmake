set(H_FILES
    svMeshSim.h
)

if(SV_USE_MESHSIM_ADAPTOR)
  set(H_FILES ${H_FILES}
    svMeshSimAdaptor.h)
endif()

set(CPP_FILES
    svMeshSim.cxx
    svRegisterMeshSimFunction.cxx
)

if(SV_USE_MESHSIM_ADAPTOR)
  set(CPP_FILES ${CPP_FILES}
    svMeshSimAdaptor.cxx)
endif()

set(RESOURCE_FILES

)
