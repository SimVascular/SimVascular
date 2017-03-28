#include "svModelElementParasolid.h"

#include "svModelUtils.h"

#include "cv_sys_geom.h"

#include <iostream>
using namespace std;

svModelElementParasolid::svModelElementParasolid()
{
    m_Type="Parasolid";
    m_InnerSolid=NULL;
    m_MaxDist=1.0;
}

svModelElementParasolid::svModelElementParasolid(const svModelElementParasolid &other)
    : svModelElement(other)
    , m_MaxDist(other.m_MaxDist)
{
    m_InnerSolid=new cvParasolidSolidModel();
    m_InnerSolid->Copy(*(other.m_InnerSolid));
}

svModelElementParasolid::~svModelElementParasolid()
{
    if(m_InnerSolid)
        delete m_InnerSolid;
}

svModelElementParasolid* svModelElementParasolid::Clone()
{
    return new svModelElementParasolid(*this);
}

vtkSmartPointer<vtkPolyData> svModelElementParasolid::CreateFaceVtkPolyData(int id)
{
    if(m_InnerSolid==NULL)
        return NULL;

    cvPolyData* cvfacevpd=m_InnerSolid->GetFacePolyData(id,1,m_MaxDist);
    if(cvfacevpd==NULL)
        return NULL;

    return cvfacevpd->GetVtkPolyData();

//    vtkSmartPointer<vtkPolyData> fpd
//      = vtkSmartPointer<vtkPolyData>::Take(facepd);

//    return fpd;
}

vtkSmartPointer<vtkPolyData> svModelElementParasolid::CreateWholeVtkPolyData()
{
    if(m_InnerSolid==NULL)
        return NULL;

    cvPolyData* cvwholevpd=m_InnerSolid->GetPolyData(1,m_MaxDist);
    if(cvwholevpd==NULL)
        return NULL;

    return cvwholevpd->GetVtkPolyData();
}

double svModelElementParasolid::GetMaxDist()
{
    return m_MaxDist;
}

void svModelElementParasolid::SetMaxDist(double maxDist)
{
    m_MaxDist=maxDist;
}

cvParasolidSolidModel* svModelElementParasolid::GetInnerSolid()
{
    return m_InnerSolid;
}

void svModelElementParasolid::SetInnerSolid(cvParasolidSolidModel* innerSolid)
{
    m_InnerSolid=innerSolid;
}

int svModelElementParasolid::GetFaceIDFromInnerSolid(std::string faceName)
{
    int id=-1;

    if(m_InnerSolid==NULL)
        return id;

    int numFaces;
    int *ids;
    int status=m_InnerSolid->GetFaceIds( &numFaces, &ids);
    if(status!=SV_OK)
        return id;

    for(int i=0;i<numFaces;i++)
    {
        char *value;
        m_InnerSolid->GetFaceAttribute("gdscName",ids[i],&value);
        std::string name(value);
        if(name==faceName)
            return ids[i];
    }

    return id;
}

void svModelElementParasolid::AddBlendRadii(std::vector<svBlendParamRadius*> moreBlendRadii)
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

    //update faceids since ids in m_InnerSolid changed after blending
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

void svModelElementParasolid::SetFaceName(std::string name, int id)
{
    int index=GetFaceIndex(id);
    if(index>-1)
    {
        m_Faces[index]->name=name;
        if(m_InnerSolid)
        {
            char* nc=const_cast<char*>(name.c_str());
            m_InnerSolid->SetFaceAttribute("gdscName",id,nc);
        }

    }
}

svModelElementPolyData* svModelElementParasolid::ConverToPolyDataModel()
{
    svModelElementPolyData* mepd=new svModelElementPolyData();
    mepd->SetSegNames(GetSegNames());

    vtkSmartPointer<vtkPolyData> wholevpd=NULL;
    if(GetWholeVtkPolyData())
    {
        wholevpd=vtkSmartPointer<vtkPolyData>::New();
        wholevpd->DeepCopy(GetWholeVtkPolyData());
    }

    if(wholevpd==NULL)
        return NULL;

    cvPolyData* src=new cvPolyData(wholevpd);

    std::vector<svModelElement::svFace*> oldFaces=GetFaces();
    std::vector<svModelElement::svFace*> faces;
    int numFaces=oldFaces.size();
    int* ids=new int[numFaces];
    cvPolyData **facevpds=new cvPolyData*[numFaces];

    for(int i=0;i<numFaces;i++)
    {
        ids[i]=oldFaces[i]->id;
        facevpds[i]=new cvPolyData(oldFaces[i]->vpd);

        svModelElement::svFace* face=new svModelElement::svFace(*(oldFaces[i]),false);

        faces.push_back(face);
    }

    cvPolyData *dst=NULL;
    if ( sys_geom_assign_ids_based_on_faces(src,facevpds,numFaces,ids,&dst ) != SV_OK ) {
        if(dst!=NULL)
            delete dst;

        delete [] ids;
        return NULL;
    }

    mepd->SetWholeVtkPolyData(dst->GetVtkPolyData());

    for(int i=0;i<numFaces;i++)
    {
        faces[i]->vpd=mepd->CreateFaceVtkPolyData(faces[i]->id);
    }

    mepd->SetFaces(faces);

    return mepd;
}
