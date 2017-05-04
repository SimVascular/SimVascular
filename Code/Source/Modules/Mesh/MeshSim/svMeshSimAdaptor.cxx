#include "svMeshSimAdaptor.h"


svMeshSimAdaptor::svMeshSimAdaptor()
    : m_cvTetGetMesh(new cvTetGenMeshObject(NULL))
    , m_cvTetGenAdaptor(new cvTetGenAdapt())
{
    m_Type="TetGen";
    m_cvTetGenAdaptor->SetMeshObject(m_cvTetGetMesh);
}

svMeshSimAdaptor::~svMeshSimAdaptor()
{
    if(m_cvTetGetMesh!=NULL)
        delete m_cvTetGetMesh;

    if(m_cvTetGenAdaptor!=NULL)
    {
        m_cvTetGenAdaptor->SetMeshObject(NULL);
        delete m_cvTetGenAdaptor;
    }
}

bool svMeshSimAdaptor::SetModelElement(svModelElement *modelElement)
{
    if(m_cvTetGenAdaptor && modelElement && modelElement->GetWholeVtkPolyData())
    {
        m_cvTetGenAdaptor->LoadModel(modelElement->GetWholeVtkPolyData());
        return true;
    }else
        return false;
}

//bool svMeshSimAdaptor::SetResultMesh(vtkSmartPointer<vtkUnstructuredGrid> mesh)
//{
//    if(m_cvTetGenAdaptor && mesh && m_cvTetGenAdaptor->LoadMesh(mesh)==SV_OK)
//        return true;
//    else
//        return false;
//}

bool svMeshSimAdaptor::LoadMeshFromResultVTUFile(std::string filePath)
{
    return (m_cvTetGenAdaptor && m_cvTetGenAdaptor->LoadMesh(const_cast<char*>(filePath.c_str()))==SV_OK);
}

bool svMeshSimAdaptor::SetAdaptOptions(std::string flag, double value)
{
    return m_cvTetGenAdaptor && m_cvTetGenAdaptor->SetAdaptOptions(const_cast<char*>(flag.c_str()),value)==SV_OK;
}

bool svMeshSimAdaptor::Adapt()
{
    return m_cvTetGenAdaptor
            && m_cvTetGenAdaptor->SetMetric(NULL,-1,-1)==SV_OK
            && m_cvTetGenAdaptor->SetupMesh()==SV_OK
            && m_cvTetGenAdaptor->RunAdaptor()==SV_OK
            && m_cvTetGenAdaptor->GetAdaptedMesh()==SV_OK
            && m_cvTetGenAdaptor->TransferSolution()==SV_OK;
}

bool svMeshSimAdaptor::WriteAdaptedSolution(std::string filePath)
{
    return m_cvTetGenAdaptor && m_cvTetGenAdaptor->WriteAdaptedSolution(const_cast<char*>(filePath.c_str()))==SV_OK;
}

svMeshSim* svMeshSimAdaptor::GetAdaptedMesh()
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

            svMeshSim* adaptedMesh=new svMeshSim();
            adaptedMesh->SetSurfaceMesh(surf);
            adaptedMesh->SetVolumeMesh(vol);

            return adaptedMesh;
        }
    }

    return NULL;
}

svMeshAdaptor* svMeshSimAdaptor::CreateAdaptor()
{
    return new svMeshSimAdaptor();
}
