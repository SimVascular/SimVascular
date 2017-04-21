#include "svModelElementParasolid.h"
#include "svModelUtils.h"

svModelElementParasolid::svModelElementParasolid()
{
    m_Type="Parasolid";
    m_MaxDist=1.0;
}

svModelElementParasolid::svModelElementParasolid(const svModelElementParasolid &other)
    : svModelElementAnalytic(other)
{
    m_InnerSolid=new cvParasolidSolidModel();
    m_InnerSolid->Copy(*(other.m_InnerSolid));
}

svModelElementParasolid::~svModelElementParasolid()
{
}

svModelElementParasolid* svModelElementParasolid::Clone()
{
    return new svModelElementParasolid(*this);
}

svModelElement* svModelElementParasolid::CreateModelElement()
{
    return new svModelElementParasolid();
}

svModelElement* svModelElementParasolid::CreateModelElement(std::vector<mitk::DataNode::Pointer> segNodes
                                , int numSamplingPts
                                , svModelElement::svNURBSLoftParam *nurbsParam
                                , int* stats
                                , double maxDist
                                , int noInterOut
                                , double tol
                                , unsigned int t)
{
    return svModelUtils::CreateModelElementParasolid(segNodes,numSamplingPts,maxDist,t);
}

svModelElement* svModelElementParasolid::CreateModelElementByBlend(std::vector<svModelElement::svBlendParamRadius*> blendRadii
                                                  , svModelElement::svBlendParam* param)
{
    return svModelUtils::CreateModelElementParasolidByBlend(this,blendRadii);
}
