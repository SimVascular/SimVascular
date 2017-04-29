#ifndef SVMODELUTILSPARASOLID_H
#define SVMODELUTILSPARASOLID_H

#include <svModelParasolidExports.h>

#include "svContour.h"
#include "svContourGroup.h"
#include "svModelElement.h"
#include "svModelElementParasolid.h"

class SVMODELPARASOLID_EXPORT svModelUtilsParasolid
{

public:

    static cvParasolidSolidModel* CreateLoftSurfaceParasolid(std::vector<svContour*> contourSet, std::string groupName, int numSamplingPts, int vecFlag, int addCaps);

    static svModelElementParasolid* CreateModelElementParasolid(std::vector<mitk::DataNode::Pointer> segNodes, int numSamplingPts, double maxDist = 1.0, unsigned int t = 0);

    static svModelElementParasolid* CreateModelElementParasolidByBlend(svModelElementParasolid* mepssrc, std::vector<svModelElement::svBlendParamRadius*> blendRadii);
};

#endif
