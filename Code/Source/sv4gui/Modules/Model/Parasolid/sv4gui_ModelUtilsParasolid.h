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

#ifndef SV4GUI_MODELUTILSPARASOLID_H
#define SV4GUI_MODELUTILSPARASOLID_H

#include <svModelParasolidExports.h>

#include "sv4gui_Contour.h"
#include "sv4gui_ContourGroup.h"
#include "sv4gui_ModelElement.h"
#include "sv4gui_ModelElementParasolid.h"

class SVMODELPARASOLID_EXPORT sv4guiModelUtilsParasolid
{

public:

    static cvParasolidSolidModel* CreateLoftSurfaceParasolid(std::vector<sv4guiContour*> contourSet, std::string groupName, int numSamplingPts, int vecFlag, int addCaps);

    static sv4guiModelElementParasolid* CreateModelElementParasolid(std::vector<mitk::DataNode::Pointer> segNodes, int numSamplingPts, double maxDist = 1.0, unsigned int t = 0);

    static sv4guiModelElementParasolid* CreateModelElementParasolidByBlend(sv4guiModelElementParasolid* mepssrc, std::vector<sv4guiModelElement::svBlendParamRadius*> blendRadii);
};

#endif
