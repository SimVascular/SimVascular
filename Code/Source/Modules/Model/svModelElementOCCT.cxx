#include "svModelElementOCCT.h"

#include "svModelUtils.h"

#include <iostream>
using namespace std;

svModelElementOCCT::svModelElementOCCT()
{
    m_Type="OpenCASCADE";
    m_OCCTSolid=NULL;
    m_MaxDist=20.0;
}

svModelElementOCCT::svModelElementOCCT(const svModelElementOCCT &other)
    : svModelElement(other)
    , m_MaxDist(other.m_MaxDist)
{
    m_OCCTSolid=new cvOCCTSolidModel();
    m_OCCTSolid->Copy(*(other.m_OCCTSolid));
}

svModelElementOCCT::~svModelElementOCCT()
{
    if(m_OCCTSolid)
        delete m_OCCTSolid;
}

svModelElementOCCT* svModelElementOCCT::Clone()
{
    return new svModelElementOCCT(*this);
}

vtkSmartPointer<vtkPolyData> svModelElementOCCT::CreateFaceVtkPolyData(int id)
{
    if(m_OCCTSolid==NULL)
        return NULL;

    cvPolyData* cvfacevpd=m_OCCTSolid->GetFacePolyData(id,1,m_MaxDist);
    if(cvfacevpd==NULL)
        return NULL;

    return cvfacevpd->GetVtkPolyData();

//    vtkSmartPointer<vtkPolyData> fpd
//      = vtkSmartPointer<vtkPolyData>::Take(facepd);

//    return fpd;
}

vtkSmartPointer<vtkPolyData> svModelElementOCCT::CreateWholeVtkPolyData()
{
    if(m_OCCTSolid==NULL)
        return NULL;

    cvPolyData* cvwholevpd=m_OCCTSolid->GetPolyData(1,m_MaxDist);
    if(cvwholevpd==NULL)
        return NULL;

    return cvwholevpd->GetVtkPolyData();
}

double svModelElementOCCT::GetMaxDist()
{
    return m_MaxDist;
}

void svModelElementOCCT::SetMaxDist(double maxDist)
{
    m_MaxDist=maxDist;
}

cvOCCTSolidModel* svModelElementOCCT::GetOCCTSolid()
{
    return m_OCCTSolid;
}

void svModelElementOCCT::SetOCCTSolid(cvOCCTSolidModel* occtSolid)
{
    m_OCCTSolid=occtSolid;
}

int svModelElementOCCT::GetFaceIDFromInnerSolid(std::string faceName)
{
    int id=-1;

    if(m_OCCTSolid==NULL)
        return id;

    int numFaces;
    int *ids;
    int status=m_OCCTSolid->GetFaceIds( &numFaces, &ids);
    if(status!=CV_OK)
        return id;

    for(int i=0;i<numFaces;i++)
    {
        char *value;
        m_OCCTSolid->GetFaceAttribute("gdscName",ids[i],&value);
        std::string name(value);
        if(name==faceName)
            return ids[i];
    }

    return id;
}

void svModelElementOCCT::AddBlendRadii(std::vector<svBlendParamRadius*> moreBlendRadii)
{
    for(int i=0;i<moreBlendRadii.size();i++)
    {
        svBlendParamRadius*  newParamRadius=moreBlendRadii[i];
        svBlendParamRadius* existingParamRadius=GetBlendParamRadius(newParamRadius->faceName1, newParamRadius->faceName2);
        if(existingParamRadius)
        {
            existingParamRadius->radius=newParamRadius->radius;
            delete newParamRadius;
        }
        else
        {
            m_BlendRadii.push_back(newParamRadius);
        }
    }

    //update faceids since ids in m_OCCTSolid changed after blending
    for(int i=0;i<m_BlendRadii.size();i++)
    {
        if(m_BlendRadii[i])
        {
            int faceID1=GetFaceIDFromInnerSolid(m_BlendRadii[i]->faceName1);
            int faceID2=GetFaceIDFromInnerSolid(m_BlendRadii[i]->faceName2);
            m_BlendRadii[i]->faceID1=faceID1;
            m_BlendRadii[i]->faceID2=faceID2;
        }
    }

}

void svModelElementOCCT::SetFaceName(std::string name, int id)
{
    int index=GetFaceIndex(id);
    if(index>-1)
    {
        m_Faces[index]->name=name;
        if(m_OCCTSolid)
        {
            char* nc=const_cast<char*>(name.c_str());
            m_OCCTSolid->SetFaceAttribute("gdscName",id,nc);
        }

    }
}

svModelElementPolyData* svModelElementOCCT::ConverToPolyDataModel()
{
    svModelElementPolyData* mepd=new svModelElementPolyData();
    mepd->SetSegNames(GetSegNames());
    mepd->SetWholeVtkPolyData(CreateWholeVtkPolyData());

}
