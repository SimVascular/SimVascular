#include <svMeshSimExports.h>

#include "svMeshFactory.h"
#include "svMeshSim.h"
//#include "svMeshSimAdaptor.h"

struct SVMESHSIM_EXPORT svRegisterMeshSimFunction{

    svRegisterMeshSimFunction()
    {
        svMeshSim* tempmesh=new svMeshSim();
        std::string type=tempmesh->GetType();
        svMeshFactory::RegisterCreationFunction(type, &svMeshSim::CreateMesh);
//        svMeshFactory::RegisterFileExtensions(type, tempmesh->GetFileExtensions());
//        svMeshFactory::RegisterAdaptorFunction(type, &svMesSimAdaptor::CreateAdaptor);
        delete tempmesh;
    }

    virtual ~svRegisterMeshSimFunction(){}
};

static svRegisterMeshSimFunction registerMeshSimFunction;
