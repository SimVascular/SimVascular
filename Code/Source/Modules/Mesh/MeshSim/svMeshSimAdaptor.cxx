#include "svMeshSimAdaptor.h"


svMeshSimAdaptor::svMeshSimAdaptor()
    : m_cvMeshSimMesh(new cvMeshSimMeshObject(NULL))
    , m_cvMeshSimAdaptor(new cvMeshSimAdapt())
{
    m_Type="MeshSim";
    m_cvMeshSimAdaptor->SetMeshObject(m_cvMeshSimMesh);
}

svMeshSimAdaptor::~svMeshSimAdaptor()
{
    if(m_cvMeshSimMesh!=NULL)
        delete m_cvMeshSimMesh;

    if(m_cvMeshSimAdaptor!=NULL)
    {
        m_cvMeshSimAdaptor->SetMeshObject(NULL);
        delete m_cvMeshSimAdaptor;
    }
}

bool svMeshSimAdaptor::SetModelElement(svModelElement *modelElement)
{
    m_ModelElement=modelElement;
    if(m_cvMeshSimMesh && modelElement && modelElement->GetInnerSolid())
        if(m_cvMeshSimMesh->LoadModel(modelElement->GetInnerSolid())==SV_OK)
            return true;

    return false;
}

bool svMeshSimAdaptor::LoadMesh(std::string filePath)
{
    return (m_cvMeshSimAdaptor && m_cvMeshSimAdaptor->LoadMesh(const_cast<char*>(filePath.c_str()))==SV_OK);
}

bool svMeshSimAdaptor::SetAdaptOptions(std::string flag, double value)
{
    return m_cvMeshSimAdaptor && m_cvMeshSimAdaptor->SetAdaptOptions(const_cast<char*>(flag.c_str()),value)==SV_OK;
}

bool svMeshSimAdaptor::Adapt()
{
    return m_cvMeshSimAdaptor
            && m_cvMeshSimAdaptor->SetMetric(NULL,-1,-1)==SV_OK
            && m_cvMeshSimAdaptor->SetupMesh()==SV_OK
            && m_cvMeshSimAdaptor->RunAdaptor()==SV_OK
            && m_cvMeshSimAdaptor->GetAdaptedMesh()==SV_OK
            && m_cvMeshSimAdaptor->TransferSolution()==SV_OK;
}

bool svMeshSimAdaptor::WriteAdaptedSolution(std::string filePath)
{
    return m_cvMeshSimAdaptor && m_cvMeshSimAdaptor->WriteAdaptedSolution(const_cast<char*>(filePath.c_str()))==SV_OK;
}

svMeshSim* svMeshSimAdaptor::GetAdaptedMesh()
{
    if(m_cvMeshSimMesh)
    {
        cvUnstructuredGrid* cvug=m_cvMeshSimMesh->GetUnstructuredGrid();
        vtkUnstructuredGrid* volumeMesh=NULL;
        if(cvug)
            volumeMesh=cvug->GetVtkUnstructuredGrid();

        if(volumeMesh)
        {
            vtkSmartPointer<vtkPolyData> surf=svMeshSim::CreateSurfaceMeshContainingModelFaceIDs(m_ModelElement,m_cvMeshSimMesh);
            vtkSmartPointer<vtkUnstructuredGrid> vol=vtkSmartPointer<vtkUnstructuredGrid>::New();
            vol->DeepCopy(volumeMesh);

            svMeshSim* adaptedMesh=new svMeshSim();
            adaptedMesh->SetSurfaceMesh(surf);
            adaptedMesh->SetVolumeMesh(vol);

            delete cvug;

            return adaptedMesh;
        }
    }

    return NULL;
}

bool svMeshSimAdaptor::WriteAdaptedMesh(std::string filePath)
{
    return m_cvMeshSimAdaptor && m_cvMeshSimAdaptor->WriteAdaptedMesh(const_cast<char*>(filePath.c_str()))==SV_OK;
}

svMeshAdaptor* svMeshSimAdaptor::CreateAdaptor()
{
    return new svMeshSimAdaptor();
}
