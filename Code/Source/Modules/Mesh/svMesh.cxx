#include "svMesh.h"

// can't get this to resolve properly on windows!
//#include "svStringUtils.h"
#include <string>
#include <vector>
#include <sstream>
#include <algorithm>
#include <functional>
#include <cctype>
#include <locale>

static std::vector<std::string> svStringUtils__split(const std::string &s, char delim)
{
    std::stringstream ss(s);
    std::string item;
    std::vector<std::string> elems;
    while (std::getline(ss, item, delim)) {
        if (item.length() > 0) {
            elems.push_back(item);
        }
    }
    return elems;
}

static std::string svStringUtils__ltrim(std::string s) {
    s.erase(s.begin(), std::find_if(s.begin(), s.end(),
            std::not1(std::ptr_fun<int, int>(std::isspace))));
    return s;
}

static std::string svStringUtils__rtrim(std::string s) {
    s.erase(std::find_if(s.rbegin(), s.rend(),
            std::not1(std::ptr_fun<int, int>(std::isspace))).base(), s.end());
    return s;
}

static std::string svStringUtils__trim(std::string s) {
    return svStringUtils__ltrim(svStringUtils__rtrim(s));
}

static std::string svStringUtils__lower(std::string s) {
    std::transform(s.begin(), s.end(), s.begin(), ::tolower);
    return s;
}

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

    if(!ParseCommandInternal(cmd, flag, values, strValues, option, msg))
        return false;

    if(!Execute(flag, values, strValues, option, msg))
        return false;

    return true;
}

bool svMesh::ExecuteCommands(std::vector<std::string> cmds, std::string& msg)
{
    for(int i=0;i<cmds.size();i++)
    {
        std::string cmd=svStringUtils__trim(cmds[i]);

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
