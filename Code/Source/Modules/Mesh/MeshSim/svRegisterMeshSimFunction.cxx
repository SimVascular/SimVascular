#include <svMeshSimExports.h>

#include "svMeshFactory.h"
#include "svMeshSim.h"
#ifdef SV_USE_MESHSIM_ADAPTOR
  #include "svMeshSimAdaptor.h"
#endif
#include "cv_meshsim_mesh_init.h"

struct SVMESHSIM_EXPORT svRegisterMeshSimFunction{

    svRegisterMeshSimFunction()
    {
        svMeshSim* tempmesh=new svMeshSim();
        std::string type=tempmesh->GetType();
        svMeshFactory::RegisterCreationFunction(type, &svMeshSim::CreateMesh);
        svMeshFactory::RegisterFileExtensions(type, tempmesh->GetFileExtensions());
#ifdef SV_USE_MESHSIM_ADAPTOR
        svMeshFactory::RegisterAdaptorFunction(type, &svMeshSimAdaptor::CreateAdaptor);
#endif
        delete tempmesh;

        Meshsimmesh_Init(NULL);
    }

    virtual ~svRegisterMeshSimFunction(){}
};

static svRegisterMeshSimFunction registerMeshSimFunction;
