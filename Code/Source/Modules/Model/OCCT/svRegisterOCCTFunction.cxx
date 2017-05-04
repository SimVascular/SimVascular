#include <svModelOCCTExports.h>

#include "cv_globals.h"

#include "svModelElementFactory.h"
#include "svModelElementOCCT.h"

#include <TDocStd_Document.hxx>
#include <XCAFDoc_DocumentTool.hxx>
#include <XCAFApp_Application.hxx>

#include <iostream>

struct SVMODELOCCT_EXPORT svRegisterOCCTFunction{

    svRegisterOCCTFunction()
    {
        svModelElementOCCT* tempme=new svModelElementOCCT();
        std::string type=tempme->GetType();
        svModelElementFactory::RegisterCreationFunction(type, &svModelElementOCCT::CreateModelElement);
        svModelElementFactory::RegisterFileExtensions(type, tempme->GetFileExtensions());
        delete tempme;

        Handle(XCAFApp_Application) OCCTManager = static_cast<XCAFApp_Application*>(gOCCTManager);
        OCCTManager = XCAFApp_Application::GetApplication();
        Handle(TDocStd_Document) doc;
        OCCTManager->NewDocument("MDTV-XCAF",doc);
        if ( !XCAFDoc_DocumentTool::IsXCAFDocument(doc))
        {
          std::cerr<<"OCCT XDE is not setup correctly, file i/o and register of solid will not work correctly"<<std::endl;
        }
    }

    virtual ~svRegisterOCCTFunction(){}
};

static svRegisterOCCTFunction registerOCCTFunction;
