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
    {
        m_cvTetGenAdaptor->LoadModel(modelElement->GetWholeVtkPolyData());
        return true;
    }else
        return false;
}

//bool svMeshTetGenAdaptor::SetResultMesh(vtkSmartPointer<vtkUnstructuredGrid> mesh)
//{
//    if(m_cvTetGenAdaptor && mesh && m_cvTetGenAdaptor->LoadMesh(mesh)==CV_OK)
//        return true;
//    else
//        return false;
//}

bool svMeshTetGenAdaptor::LoadMeshFromResultVTUFile(std::string filePath)
{
    return (m_cvTetGenAdaptor && m_cvTetGenAdaptor->LoadMesh(filePath.c_str())==CV_OK);
}

bool svMeshTetGenAdaptor::SetAdaptOptions(std::string flag, double value)
{
    return m_cvTetGenAdaptor && m_cvTetGenAdaptor->SetAdaptOptions(flag.c_str(),value)==CV_OK;
}

bool svMeshTetGenAdaptor::Adapt()
{
    return m_cvTetGenAdaptor
            && m_cvTetGenAdaptor->SetMetric(NULL,-1,-1)==CV_OK
            && m_cvTetGenAdaptor->SetupMesh()==CV_OK
            && m_cvTetGenAdaptor->RunAdaptor()==CV_OK
            && m_cvTetGenAdaptor->GetAdaptedMesh()==CV_OK
            && m_cvTetGenAdaptor->TransferSolution()==CV_OK;
}

bool svMeshTetGenAdaptor::WriteAdaptedSolution(std::string filePath)
{
    return m_cvTetGenAdaptor && m_cvTetGenAdaptor->WriteAdaptedSolution(filePath.c_str())==CV_OK;
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


