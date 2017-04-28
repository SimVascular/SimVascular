#ifndef SVMODELELEMENTOCCT_H
#define SVMODELELEMENTOCCT_H

#include <svModelOCCTExports.h>

#include "svModelElement.h"
#include "svModelElementAnalytic.h"

#include "cvOCCTSolidModel.h"

class SVMODELOCCT_EXPORT svModelElementOCCT : public svModelElementAnalytic
{
public:

    svModelElementOCCT();

    svModelElementOCCT(const svModelElementOCCT &other);

    virtual ~svModelElementOCCT();

    virtual svModelElementOCCT* Clone() override;

    static svModelElement* CreateModelElement();

    virtual svModelElement* CreateModelElement(std::vector<mitk::DataNode::Pointer> segNodes
                                    , int numSamplingPts
                                    , svModelElement::svNURBSLoftParam *nurbsParam
                                    , int* stats = NULL
                                    , double maxDist = 20.0
                                    , int noInterOut = 1
                                    , double tol = 1e-6
                                    , unsigned int t = 0) override;

    virtual svModelElement* CreateModelElementByBlend(std::vector<svModelElement::svBlendParamRadius*> blendRadii
                                                      , svModelElement::svBlendParam* param) override;

    virtual bool ReadFile(std::string filePath) override;

    virtual bool WriteFile(std::string filePath) override;
};

#endif // SVMODELELEMENTOCCT_H
