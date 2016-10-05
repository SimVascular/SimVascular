#include "svModelElementPolyData.h"

#include "cv_polydatasolid_utils.h"

svModelElementPolyData::svModelElementPolyData()
{
    m_Type="PolyData";
    m_BlendParam=new svBlendParam();
}

svModelElementPolyData::svModelElementPolyData(const svModelElementPolyData &other)
    : svModelElement(other)
{
    m_BlendParam=new svBlendParam(*(other.m_BlendParam));
    for(int i=0;i<other.m_BlendRadius;i++)
    {
        m_BlendRadius.push_back(new svBlendParamRadius(*(other.m_BlendRadius[i])));
    }
}

svModelElementPolyData::~svModelElementPolyData()
{
}

svModelElementPolyData* svModelElementPolyData::Clone()
{
    return new svModelElementPolyData(*this);
}

vtkSmartPointer<vtkPolyData> svModelElementPolyData::CreateFaceVtkPolyData(int id)
{
    vtkPolyData* facepd=NULL;

    if(m_WholeVtkPolyData)
    {
        facepd = vtkPolyData::New();
//        PlyDtaUtils_GetFacePolyData(m_SolidModel, &id, facepd);
        PlyDtaUtils_GetFacePolyData(m_WholeVtkPolyData.GetPointer(), &id, facepd);
    }

    vtkSmartPointer<vtkPolyData> fpd
      = vtkSmartPointer<vtkPolyData>::Take(facepd);

    return fpd;
}

//vtkPolyData* svModelElementPolyData::GetSolidModel() const
//{
//    return m_SolidModel;
//}

//void svModelElementPolyData::SetSolidModel(vtkPolyData* solidModel)
//{
//    m_SolidModel=solidModel;
//    m_WholeVtkPolyData=solidModel;
//}

svModelElementPolyData::svBlendParam* svModelElementPolyData::GetBlendParam()
{
    return m_BlendParam;
}

std::vector<svModelElementPolyData::svBlendParamRadius*> svModelElementPolyData::GetBlendRadius()
{
    return m_BlendRadius;
}

void svModelElementPolyData::SetBlendRadius(std::vector<svModelElementPolyData::svBlendParamRadius*> blendRadius)
{
    m_BlendRadius=blendRadius;
}
