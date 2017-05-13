#include "svMeshTetGenAdaptor.h"


svMeshTetGenAdaptor::svMeshTetGenAdaptor()
    : m_cvTetGetMesh(new cvTetGenMeshObject(NULL))
    , m_cvTetGenAdaptor(new cvTetGenAdapt())
{
    m_Type="TetGen";
    m_cvTetGenAdaptor->SetMeshObject(m_cvTetGetMesh);
}

svMeshTetGenAdaptor::~svMeshTetGenAdaptor()
{
    if(m_cvTetGetMesh!=NULL)
        delete m_cvTetGetMesh;

    if(m_cvTetGenAdaptor!=NULL)
    {
        m_cvTetGenAdaptor->SetMeshObject(NULL);
        delete m_cvTetGenAdaptor;
    }
}

bool svMeshTetGenAdaptor::SetModelElement(svModelElement *modelElement)
{
    if(m_cvTetGenAdaptor && modelElement && modelElement->GetWholeVtkPolyData())
        if(m_cvTetGenAdaptor->LoadModel(modelElement->GetWholeVtkPolyData())==SV_OK)
            return true;

    return false;
}

bool svMeshTetGenAdaptor::LoadMesh(std::string filePath)
{
    return (m_cvTetGenAdaptor && m_cvTetGenAdaptor->LoadMesh(const_cast<char*>(filePath.c_str()))==SV_OK);
}

bool svMeshTetGenAdaptor::SetAdaptOptions(std::string flag, double value)
{
    return m_cvTetGenAdaptor && m_cvTetGenAdaptor->SetAdaptOptions(const_cast<char*>(flag.c_str()),value)==SV_OK;
}

bool svMeshTetGenAdaptor::Adapt()
{
    return m_cvTetGenAdaptor
            && m_cvTetGenAdaptor->SetMetric(NULL,-1,-1)==SV_OK
            && m_cvTetGenAdaptor->SetupMesh()==SV_OK
            && m_cvTetGenAdaptor->RunAdaptor()==SV_OK
            && m_cvTetGenAdaptor->GetAdaptedMesh()==SV_OK
            && m_cvTetGenAdaptor->TransferSolution()==SV_OK;
}

bool svMeshTetGenAdaptor::WriteAdaptedSolution(std::string filePath)
{
    return m_cvTetGenAdaptor && m_cvTetGenAdaptor->WriteAdaptedSolution(const_cast<char*>(filePath.c_str()))==SV_OK;
}

svMeshTetGen* svMeshTetGenAdaptor::GetAdaptedMesh()
{
    if(m_cvTetGetMesh)
    {
        vtkPolyData* surfaceMesh=m_cvTetGetMesh->GetPolyData()->GetVtkPolyData();
        vtkUnstructuredGrid* volumeMesh=m_cvTetGetMesh->GetUnstructuredGrid()->GetVtkUnstructuredGrid();
        if(surfaceMesh && volumeMesh)
        {
            vtkSmartPointer<vtkPolyData> surf=vtkSmartPointer<vtkPolyData>::New();
            vtkSmartPointer<vtkUnstructuredGrid> vol=vtkSmartPointer<vtkUnstructuredGrid>::New();
            surf->DeepCopy(surfaceMesh);
            vol->DeepCopy(volumeMesh);

            svMeshTetGen* adaptedMesh=new svMeshTetGen();
            adaptedMesh->SetSurfaceMesh(surf);
            adaptedMesh->SetVolumeMesh(vol);

            return adaptedMesh;
        }
    }

    return NULL;
}

bool svMeshTetGenAdaptor::WriteAdaptedMesh(std::string filePath)
{
    return m_cvTetGenAdaptor && m_cvTetGenAdaptor->WriteAdaptedMesh(const_cast<char*>(filePath.c_str()))==SV_OK;
}

svMeshAdaptor* svMeshTetGenAdaptor::CreateAdaptor()
{
    return new svMeshTetGenAdaptor();
}
