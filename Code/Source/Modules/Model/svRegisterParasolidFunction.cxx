#include <svModelExports.h>

#include <svModelElementFactory.h>
#include <svModelElementParasolid.h>

struct SVMODEL_EXPORT svRegisterParasolidFunction{

    svRegisterParasolidFunction()
    {
        svModelElementFactory::RegisterCreationFunction("Parasolid", &svModelElementParasolid::CreateModelElement);
    }

    virtual ~svRegisterParasolidFunction(){}
};

static svRegisterParasolidFunction registerParasolidFunction;
