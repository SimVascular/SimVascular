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
