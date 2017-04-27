#include <svModelExports.h>

#include <svModelElementFactory.h>
#include <svModelElementParasolid.h>

#include "cv_parasolid_utils.h"

#include <iostream>

struct SVMODEL_EXPORT svRegisterParasolidFunction{

    svRegisterParasolidFunction()
    {
        svModelElementParasolid* tempme=new svModelElementParasolid();
        std::string type=tempme->GetType();
        svModelElementFactory::RegisterCreationFunction(type, &svModelElementParasolid::CreateModelElement);
        svModelElementFactory::RegisterFileExtensions(type, tempme->GetFileExtensions());
        delete tempme;

        if (PsdUtils_Init() != SV_OK)
            std::cerr<<"Parasolid Uitls init error."<<std::endl;
    }

    virtual ~svRegisterParasolidFunction(){}
};

static svRegisterParasolidFunction registerParasolidFunction;
