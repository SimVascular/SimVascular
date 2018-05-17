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

#ifndef SV4GUI_MODELELEMENTOCCT_H
#define SV4GUI_MODELELEMENTOCCT_H

#include <sv4guiModuleModelOCCTExports.h>

#include "sv4gui_ModelElement.h"
#include "sv4gui_ModelElementAnalytic.h"

#include "sv_OCCTSolidModel.h"

class SV4GUIMODULEMODELOCCT_EXPORT sv4guiModelElementOCCT : public sv4guiModelElementAnalytic
{
public:

    sv4guiModelElementOCCT();

    sv4guiModelElementOCCT(const sv4guiModelElementOCCT &other);

    virtual ~sv4guiModelElementOCCT();

    virtual sv4guiModelElementOCCT* Clone() override;

    static sv4guiModelElement* CreateModelElement();

    virtual sv4guiModelElement* CreateModelElement(std::vector<mitk::DataNode::Pointer> segNodes
                                    , int numSamplingPts
                                    , svLoftingParam *param
                                    , int* stats = NULL
                                    , double maxDist = 20.0
                                    , int noInterOut = 1
                                    , double tol = 1e-6
                                    , unsigned int t = 0) override;

    virtual sv4guiModelElement* CreateModelElementByBlend(std::vector<sv4guiModelElement::svBlendParamRadius*> blendRadii
                                                      , sv4guiModelElement::svBlendParam* param) override;

    virtual bool ReadFile(std::string filePath) override;

    virtual bool WriteFile(std::string filePath) override;
};

#endif // SV4GUI_MODELELEMENTOCCT_H
