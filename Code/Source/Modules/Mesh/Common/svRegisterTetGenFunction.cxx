#include <svMeshExports.h>

#include "svMeshFactory.h"
#include "svMeshTetGen.h"
#include "svMeshTetGenAdaptor.h"

struct SVMESH_EXPORT svRegisterTetGenFunction{

    svRegisterTetGenFunction()
    {
        svMeshTetGen* tempmesh=new svMeshTetGen();
        std::string type=tempmesh->GetType();
        svMeshFactory::RegisterCreationFunction(type, &svMeshTetGen::CreateMesh);
        svMeshFactory::RegisterFileExtensions(type, tempmesh->GetFileExtensions());
        svMeshFactory::RegisterAdaptorFunction(type, &svMeshTetGenAdaptor::CreateAdaptor);
        delete tempmesh;
    }

    virtual ~svRegisterTetGenFunction(){}
};

static svRegisterTetGenFunction registerTetGenFunction;
