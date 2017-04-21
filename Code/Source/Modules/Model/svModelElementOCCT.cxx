#include "svModelElementOCCT.h"
#include "svModelUtils.h"

svModelElementOCCT::svModelElementOCCT()
{
    m_Type="OpenCASCADE";
    m_MaxDist=20.0;
}

svModelElementOCCT::svModelElementOCCT(const svModelElementOCCT &other)
    : svModelElementAnalytic(other)
{
    m_InnerSolid=new cvOCCTSolidModel();
    m_InnerSolid->Copy(*(other.m_InnerSolid));
}

svModelElementOCCT::~svModelElementOCCT()
{
}

svModelElementOCCT* svModelElementOCCT::Clone()
{
    return new svModelElementOCCT(*this);
}

svModelElement* svModelElementOCCT::CreateModelElement()
{
    return new svModelElementOCCT();
}

svModelElement* svModelElementOCCT::CreateModelElement(std::vector<mitk::DataNode::Pointer> segNodes
                                , int numSamplingPts
                                , svModelElement::svNURBSLoftParam *nurbsParam
                                , int* stats
                                , double maxDist
                                , int noInterOut
                                , double tol
                                , unsigned int t)
{
    return svModelUtils::CreateModelElementOCCT(segNodes,numSamplingPts,nurbsParam,maxDist,t);
}

svModelElement* svModelElementOCCT::CreateModelElementByBlend(std::vector<svModelElement::svBlendParamRadius*> blendRadii
                                                  , svModelElement::svBlendParam* param)
{
    return svModelUtils::CreateModelElementOCCTByBlend(this,blendRadii);
}
