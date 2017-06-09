#include "svSeg3D.h"

svSeg3D::svSeg3D()
    : m_Vpd(NULL)
{
}

svSeg3D::svSeg3D(const svSeg3D &other, bool copyVpd)
    : m_Param(other.m_Param)
    , m_Vpd(NULL)
{
    if(copyVpd && other.m_Vpd!=NULL)
    {
        m_Vpd=vtkSmartPointer<vtkPolyData>::New();
        m_Vpd->DeepCopy(other.m_Vpd);
    }
}

svSeg3D::~svSeg3D()
{
}

svSeg3D* svSeg3D::Clone()
{
    return new svSeg3D(*this);
}

svSeg3DParam& svSeg3D::GetParam()
{
    return m_Param;
}

