#include "svMitkSeg3D.h"
#include "svMitkSeg3DOperation.h"

svMitkSeg3D::svMitkSeg3D()
    : mitk::Surface()
    , m_Seg3D(NULL)
    , m_DataModified(false)
{
}

svMitkSeg3D::svMitkSeg3D(const svMitkSeg3D &other)
    : mitk::Surface(other)
    , m_DataModified(true)
{
    m_Seg3D=new svSeg3D(*(other.m_Seg3D),false);
    m_Seg3D->SetVtkPolyData(GetVtkPolyData());
}

svMitkSeg3D::~svMitkSeg3D()
{
    if(m_Seg3D)
        delete m_Seg3D;
}

bool svMitkSeg3D::IsEmptyTimeStep(unsigned int t) const
{
    return !IsInitialized();
}

void svMitkSeg3D::SetSeg3D(svSeg3D* seg3D)
{
    m_Seg3D=seg3D;

    SetVtkPolyData(seg3D->GetVtkPolyData());
    CalculateBoundingBox();
    Modified();

    this->InvokeEvent( svMitkSeg3DSetEvent() );
}

void svMitkSeg3D::ExecuteOperation( mitk::Operation* operation )
{

    svMitkSeg3DOperation* seg3DOperation = dynamic_cast<svMitkSeg3DOperation*>(operation);

    if(seg3DOperation==NULL)
    {
        MITK_ERROR << "No valid Operation for svMitkSeg3D" << std::endl;
        return;
    }

    svSeg3D* seg3D=seg3DOperation->GetSeg3D();

    if(seg3DOperation->GetOperationType()==svMitkSeg3DOperation::OpSETSEG3D)
    {
        m_DataModified=true;
        SetSeg3D(seg3D);
    }

    // mitk::OperationEndEvent endevent(operation);
    // ((const itk::Object*)this)->InvokeEvent(endevent);

}

