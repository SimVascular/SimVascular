#include "svModelElementPolyData.h"

#include "cv_polydatasolid_utils.h"

svModelElementPolyData::svModelElementPolyData()
    : m_Type("PolyData")
    , m_SolidModelPolyData(NULL)
{
}

svModelElementPolyData::svModelElementPolyData(const svModelElementPolyData &other)
    : svModelElement(other)
    , m_SolidModelPolyData(other.m_SolidModelPolyData)
{
}

svModelElementPolyData::~svModelElementPolyData()
{
}

svModelElementPolyData* svModelElementPolyData::Clone()
{
    return new svModelElementPolyData(*this);
}

vtkPolyData* svModelElementPolyData::CreateFaceVtkPolyData(int id)
{
    vtkPolyData *facepd=NULL;

    if(m_SolidModel)
    {
        facepd = vtkPolyData::New();
        PlyDtaUtils_GetFacePolyData(m_SolidModel, &id, facepd);
    }

    return facepd;
}

vtkPolyData* svModelElementPolyData::GetSolidModel() const
{
    return m_SolidModel;
}

void svModelElementPolyData::SetSolidModel(vtkPolyData* solidModel)
{
    m_SolidModel=solidModel;
    m_WholeVtkPolyData=solidModel;
}


