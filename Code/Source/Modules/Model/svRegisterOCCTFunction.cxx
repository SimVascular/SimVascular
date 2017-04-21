#include <svModelExports.h>

#include <svModelElementFactory.h>
#include <svModelElementOCCT.h>

struct SVMODEL_EXPORT svRegisterOCCTFunction{

    svRegisterOCCTFunction()
    {
        svModelElementFactory::RegisterCreationFunction("OpenCASCADE", &svModelElementOCCT::CreateModelElement);
    }

    virtual ~svRegisterOCCTFunction(){}
};

static svRegisterOCCTFunction registerOCCTFunction;
