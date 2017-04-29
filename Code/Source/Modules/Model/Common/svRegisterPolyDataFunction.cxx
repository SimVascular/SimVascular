#include <svModelExports.h>

#include "svModelElementFactory.h"
#include "svModelElementPolyData.h"

struct SVMODEL_EXPORT svRegisterPolyDataFunction{

    svRegisterPolyDataFunction()
    {
        svModelElementPolyData* tempme=new svModelElementPolyData();
        std::string type=tempme->GetType();
        svModelElementFactory::RegisterCreationFunction(type, &svModelElementPolyData::CreateModelElement);
        svModelElementFactory::RegisterFileExtensions(type, tempme->GetFileExtensions());
        delete tempme;
    }

    virtual ~svRegisterPolyDataFunction(){}
};

static svRegisterPolyDataFunction registerPolyDataFunction;
