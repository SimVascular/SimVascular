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

#ifndef SVMODELUTILSOCCT_H
#define SVMODELUTILSOCCT_H

#include <svModelOCCTExports.h>

#include "svContour.h"
#include "svContourGroup.h"
#include "svModelElement.h"
#include "svModelElementOCCT.h"

class SVMODELOCCT_EXPORT svModelUtilsOCCT
{

public:

    static cvOCCTSolidModel* CreateLoftSurfaceOCCT(std::vector<svContour*> contourSet, std::string groupName, int numSamplingPts, svLoftingParam *param, int vecFlag, int addCaps);

    static svModelElementOCCT* CreateModelElementOCCT(std::vector<mitk::DataNode::Pointer> segNodes, int numSamplingPts, svLoftingParam *param, double maxDist = 20.0, unsigned int t = 0);

    static svModelElementOCCT* CreateModelElementOCCTByBlend(svModelElementOCCT* meocctsrc, std::vector<svModelElement::svBlendParamRadius*> blendRadii);
};

#endif /* SVMODELUTILSOCCT_H */
