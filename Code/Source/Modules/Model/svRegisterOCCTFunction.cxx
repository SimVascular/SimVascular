#include <svModelExports.h>

#include <svModelElementFactory.h>
#include <svModelElementOCCT.h>

struct SVMODEL_EXPORT svRegisterOCCTFunction{

    svRegisterOCCTFunction()
    {
        svModelElementFactory::RegisterCreationFunction("OpenCASCADE", &svModelElementOCCT::CreateModelElement);
        svModelElementFactory::RegisterFileExtension("OpenCASCADE", "brep");
    }

    virtual ~svRegisterOCCTFunction(){}
};

static svRegisterOCCTFunction registerOCCTFunction;
