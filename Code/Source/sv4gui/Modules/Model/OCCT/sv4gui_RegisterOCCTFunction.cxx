/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "sv4gui_RegisterOCCTFunction.h"

#include <sv4guiModuleModelOCCTExports.h>

#include "sv2_globals.h"

#include "sv4gui_ModelElementFactory.h"
#include "sv4gui_ModelElementOCCT.h"

#include <TDocStd_Document.hxx>
#include <XCAFDoc_DocumentTool.hxx>
#include <XCAFApp_Application.hxx>

#include <iostream>

sv4guiRegisterOCCTFunction::sv4guiRegisterOCCTFunction()
    {
        sv4guiModelElementOCCT* tempme=new sv4guiModelElementOCCT();
        std::string type=tempme->GetType();
        sv4guiModelElementFactory::RegisterCreationFunction(type, &sv4guiModelElementOCCT::CreateModelElement);
        sv4guiModelElementFactory::RegisterFileExtensions(type, tempme->GetFileExtensions());
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

sv4guiRegisterOCCTFunction::~sv4guiRegisterOCCTFunction() {}

static sv4guiRegisterOCCTFunction registerOCCTFunction = sv4guiRegisterOCCTFunction();
