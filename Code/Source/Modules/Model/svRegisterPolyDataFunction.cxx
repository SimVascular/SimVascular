#include <svModelExports.h>

#include <svModelElementFactory.h>
#include <svModelElementPolyData.h>

struct SVMODEL_EXPORT svRegisterPolyDataFunction{

    svRegisterPolyDataFunction()
    {
        svModelElementFactory::RegisterCreationFunction("PolyData", &svModelElementPolyData::CreateModelElement);
        svModelElementFactory::RegisterFileExtension("PolyData", "vtp");
    }

    virtual ~svRegisterPolyDataFunction(){}
};

static svRegisterPolyDataFunction registerPolyDataFunction;
