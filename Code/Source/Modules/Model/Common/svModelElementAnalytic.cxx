#include "svModelElementAnalytic.h"

#include "cv_sys_geom.h"

svModelElementAnalytic::svModelElementAnalytic()
    : m_MaxDist(0)
{
}

svModelElementAnalytic::svModelElementAnalytic(const svModelElementAnalytic &other)
    : svModelElement(other)
    , m_MaxDist(other.m_MaxDist)
{
}

svModelElementAnalytic::~svModelElementAnalytic()
{
}

svModelElementAnalytic* svModelElementAnalytic::Clone()
{
    return new svModelElementAnalytic(*this);
}

vtkSmartPointer<vtkPolyData> svModelElementAnalytic::CreateFaceVtkPolyData(int id)
{
    if(m_InnerSolid==NULL)
        return NULL;

    cvPolyData* cvfacevpd=m_InnerSolid->GetFacePolyData(id,1,m_MaxDist);
    if(cvfacevpd==NULL)
        return NULL;

    return cvfacevpd->GetVtkPolyData();
}

vtkSmartPointer<vtkPolyData> svModelElementAnalytic::CreateWholeVtkPolyData()
{
    if(m_InnerSolid==NULL)
        return NULL;

    cvPolyData* cvwholevpd=m_InnerSolid->GetPolyData(1,m_MaxDist);
    if(cvwholevpd==NULL)
        return NULL;

    return cvwholevpd->GetVtkPolyData();
}

double svModelElementAnalytic::GetMaxDist()
{
    return m_MaxDist;
}

void svModelElementAnalytic::SetMaxDist(double maxDist)
{
    m_MaxDist=maxDist;
}

void svModelElementAnalytic::AddBlendRadii(std::vector<svBlendParamRadius*> moreBlendRadii)
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

svModelElementPolyData* svModelElementAnalytic::ConverToPolyDataModel()
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
