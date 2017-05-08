#include "svMesh.h"

#include "svStringUtils.h"

#include <vtkXMLPolyDataWriter.h>
#include <vtkSmartPointer.h>
#include <vtkErrorCode.h>
#include <vtkXMLUnstructuredGridWriter.h>
#include <vtkXMLPolyDataReader.h>
#include <vtkXMLUnstructuredGridReader.h>

svMesh::svMesh()
    : m_Type("")
//    , m_ModelName("")
    , m_ModelElement(NULL)
    , m_SurfaceMesh(NULL)
    , m_VolumeMesh(NULL)
{
}

svMesh::svMesh(const svMesh &other)
    : m_Type(other.m_Type)
//    , m_ModelName(other.m_ModelName)
    , m_ModelElement(other.m_ModelElement)
    , m_CommandHistory(other.m_CommandHistory)
{
    m_SurfaceMesh=NULL;
    if(other.m_SurfaceMesh)
    {
        m_SurfaceMesh=vtkSmartPointer<vtkPolyData>::New();
        m_SurfaceMesh->DeepCopy(other.m_SurfaceMesh);
    }

    m_VolumeMesh=NULL;
    if(other.m_VolumeMesh)
    {
        m_VolumeMesh=vtkSmartPointer<vtkUnstructuredGrid>::New();
        m_VolumeMesh->DeepCopy(other.m_VolumeMesh);
    }
}

svMesh::~svMesh()
{
}

svMesh* svMesh::Clone()
{
    return new svMesh(*this);
}

std::string svMesh::GetType() const
{
    return m_Type;
}

//std::string svMesh::GetModelName() const
//{
//    return m_ModelName;
//}

//void svMesh::SetModelName(std::string name)
//{
//    m_ModelName=name;
//}

svModelElement* svMesh::GetModelElement() const
{
    return m_ModelElement;
}

bool svMesh::SetModelElement(svModelElement* modelElement)
{
    SetModelElementOnly(modelElement);
    return true;
}

void svMesh::SetModelElementOnly(svModelElement* modelElement)
{
    m_ModelElement=modelElement;
}

void svMesh::CalculateBoundingBox(double *bounds)
{
    bounds[0]=0;
    bounds[1]=0;
    bounds[2]=0;
    bounds[3]=0;
    bounds[4]=0;
    bounds[5]=0;

    if (m_SurfaceMesh != nullptr && m_SurfaceMesh->GetNumberOfPoints() > 0)
    {
      m_SurfaceMesh->ComputeBounds();
      m_SurfaceMesh->GetBounds(bounds);
    }

}

bool svMesh::ExecuteCommand(std::string cmd, std::string& msg)
{
    std::string flag="";
    double values[20]={0};
    std::string strValues[5]={""};
    bool option=false;

    if(!ParseCommand(cmd, flag, values, strValues, option, msg))
        return false;

    if(!Execute(flag, values, strValues, option, msg))
        return false;

    return true;
}

bool svMesh::ExecuteCommands(std::vector<std::string> cmds, std::string& msg)
{
    for(int i=0;i<cmds.size();i++)
    {
        std::string cmd=svStringUtils_trim(cmds[i]);

        if(cmd=="")
            continue;

        if(!ExecuteCommand(cmd, msg))
            return false;
    }

    return true;
}

std::vector<std::string> svMesh::GetCommandHistory() const
{
    return m_CommandHistory;
}

void svMesh::SetCommandHistory(std::vector<std::string> history)
{
    m_CommandHistory=history;
}

bool svMesh::ExecuteCommandHistory(std::string& msg)
{
    if(!ExecuteCommands(m_CommandHistory, msg)) {
            return false;
    }
    return true;
}

vtkSmartPointer<vtkPolyData> svMesh::GetSurfaceMesh()
{
    return m_SurfaceMesh;
}

vtkSmartPointer<vtkUnstructuredGrid> svMesh::GetVolumeMesh()
{
    return m_VolumeMesh;
}

void svMesh::SetSurfaceMesh(vtkSmartPointer<vtkPolyData> surfaceMesh)
{
    m_SurfaceMesh=surfaceMesh;
}

void svMesh::SetVolumeMesh(vtkSmartPointer<vtkUnstructuredGrid> volumeMesh)
{
    m_VolumeMesh=volumeMesh;
}

bool svMesh::WriteSurfaceFile(std::string filePath)
{
    if(m_SurfaceMesh)
    {
        vtkSmartPointer<vtkXMLPolyDataWriter> writer = vtkSmartPointer<vtkXMLPolyDataWriter>::New();
        writer->SetFileName(filePath.c_str());
        writer->SetInputData(m_SurfaceMesh);
        if (writer->Write() == 0 || writer->GetErrorCode() != 0 )
        {
            std::cerr << "vtkXMLPolyDataWriter error: " << vtkErrorCode::GetStringFromErrorCode(writer->GetErrorCode())<<std::endl;
            return false;
        }
    }

    return true;
}

bool svMesh::WriteVolumeFile(std::string filePath)
{
    if(m_VolumeMesh)
    {
        vtkSmartPointer<vtkXMLUnstructuredGridWriter> writer = vtkSmartPointer<vtkXMLUnstructuredGridWriter>::New();
        writer->SetFileName(filePath.c_str());
        writer->SetInputData(m_VolumeMesh);
        if (writer->Write() == 0 || writer->GetErrorCode() != 0 )
        {
            std::cerr << "vtkXMLUnstructuredGridWriter error: " << vtkErrorCode::GetStringFromErrorCode(writer->GetErrorCode())<<std::endl;
            return false;
        }
    }

    return true;
}

bool svMesh::ReadSurfaceFile(std::string filePath)
{
    m_SurfaceMesh=CreateSurfaceMeshFromFile(filePath);

    return true;
}

bool svMesh::ReadVolumeFile(std::string filePath)
{
    m_VolumeMesh=CreateVolumeMeshFromFile(filePath);

    return true;
}

vtkSmartPointer<vtkPolyData> svMesh::CreateSurfaceMeshFromFile(std::string filePath)
{
    vtkSmartPointer<vtkPolyData> surfaceMesh=NULL;
    std::ifstream surfaceFile(filePath);
    if (surfaceFile) {
        vtkSmartPointer<vtkXMLPolyDataReader> reader = vtkSmartPointer<vtkXMLPolyDataReader>::New();

        reader->SetFileName(filePath.c_str());
        reader->Update();
        surfaceMesh=reader->GetOutput();
    }

    return surfaceMesh;
}

vtkSmartPointer<vtkUnstructuredGrid> svMesh::CreateVolumeMeshFromFile(std::string filePath)
{
    vtkSmartPointer<vtkUnstructuredGrid> volumeMesh=NULL;
    std::ifstream volumeFile(filePath);
    if (volumeFile) {
        vtkSmartPointer<vtkXMLUnstructuredGridReader> reader = vtkSmartPointer<vtkXMLUnstructuredGridReader>::New();

        reader->SetFileName(filePath.c_str());
        reader->Update();
        volumeMesh=reader->GetOutput();
    }

    return volumeMesh;
}
