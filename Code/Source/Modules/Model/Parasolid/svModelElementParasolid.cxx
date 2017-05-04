#include "svModelElementParasolid.h"
#include "svModelUtilsParasolid.h"

#include <iostream>

svModelElementParasolid::svModelElementParasolid()
{
    m_Type="Parasolid";
    m_MaxDist=1.0;
    std::vector<std::string> exts={"xmt_txt"};
    m_FileExtensions=exts;
}

svModelElementParasolid::svModelElementParasolid(const svModelElementParasolid &other)
    : svModelElementAnalytic(other)
{
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
    return svModelUtilsParasolid::CreateModelElementParasolid(segNodes,numSamplingPts,maxDist,t);
}

svModelElement* svModelElementParasolid::CreateModelElementByBlend(std::vector<svModelElement::svBlendParamRadius*> blendRadii
                                                  , svModelElement::svBlendParam* param)
{
    return svModelUtilsParasolid::CreateModelElementParasolidByBlend(this,blendRadii);
}

bool svModelElementParasolid::ReadFile(std::string filePath)
{
    cvParasolidSolidModel* parasolid=new cvParasolidSolidModel();
    char* df=const_cast<char*>(filePath.c_str());
    m_InnerSolid=parasolid;
    if(m_InnerSolid->ReadNative(df)==SV_OK)
        return true;
    else
        return false;
}

bool svModelElementParasolid::WriteFile(std::string filePath)
{
    if(m_InnerSolid)
    {
        char* df=const_cast<char*>(filePath.c_str());
        if (m_InnerSolid->WriteNative(0,df)!=SV_OK )
            return false;
    }

    return true;
}

std::string svModelElementParasolid::GetFaceIdentifierFromInnerSolid(std::string faceName)
{
    int faceID=GetFaceIDFromInnerSolid(faceName);
    char* value;
    m_InnerSolid->GetFaceAttribute("identifier",faceID,&value);
    std::string idtf(value);

    return idtf;
}
