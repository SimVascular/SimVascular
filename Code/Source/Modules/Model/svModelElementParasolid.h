#ifndef SVMODELELEMENTPARASOLID_H
#define SVMODELELEMENTPARASOLID_H

#include <svModelExports.h>

#include "svModelElement.h"
#include "svModelElementAnalytic.h"

#include "cvParasolidSolidModel.h"

class SVMODEL_EXPORT svModelElementParasolid : public svModelElementAnalytic
{
public:

    svModelElementParasolid();

    svModelElementParasolid(const svModelElementParasolid &other);

    virtual ~svModelElementParasolid();

    virtual svModelElementParasolid* Clone() override;

    static svModelElement* CreateModelElement();

    virtual svModelElement* CreateModelElement(std::vector<mitk::DataNode::Pointer> segNodes
                                    , int numSamplingPts
                                    , svModelElement::svNURBSLoftParam *nurbsParam
                                    , int* stats = NULL
                                    , double maxDist = 1.0
                                    , int noInterOut = 1
                                    , double tol = 1e-6
                                    , unsigned int t = 0) override;

    virtual svModelElement* CreateModelElementByBlend(std::vector<svModelElement::svBlendParamRadius*> blendRadii
                                                      , svModelElement::svBlendParam* param) override;
};

#endif // SVMODELELEMENTPARASOLID_H
