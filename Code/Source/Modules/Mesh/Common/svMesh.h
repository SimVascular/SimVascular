#ifndef SVMESH_H
#define SVMESH_H

#include <svMeshExports.h>

#include "svModelElement.h"

#include <vtkSmartPointer.h>

#include <vtkPolyData.h>
#include <vtkUnstructuredGrid.h>

class SVMESH_EXPORT svMesh
{

public:

    svMesh();

    svMesh(const svMesh &other);

    virtual ~svMesh();

    virtual svMesh* Clone();

    std::string GetType() const;

    virtual void InitNewMesher() {}

//    std::string GetModelName() const;

//    void SetModelName(std::string name);

    svModelElement* GetModelElement() const;

    virtual bool SetModelElement(svModelElement* modelElement);

    void SetModelElementOnly(svModelElement* modelElement);

    void CalculateBoundingBox(double *bounds);

    bool ExecuteCommand(std::string cmd, std::string& msg);

    virtual bool Execute(std::string flag, double values[20], std::string strValues[5], bool option, std::string& msg) {return false;}

    virtual bool ParseCommand(std::string cmd, std::string& flag, double values[20], std::string strValues[5], bool& option, std::string& msg) {return false;}

    bool ExecuteCommands(std::vector<std::string> cmds, std::string& msg);

//    bool ExecuteCommandFile(std::string filePath);

    std::vector<std::string> GetCommandHistory() const;

    void SetCommandHistory(std::vector<std::string> history);

    bool ExecuteCommandHistory(std::string& msg);

    vtkSmartPointer<vtkPolyData> GetSurfaceMesh();

    vtkSmartPointer<vtkUnstructuredGrid> GetVolumeMesh();

    void SetSurfaceMesh(vtkSmartPointer<vtkPolyData> surfaceMesh);

    void SetVolumeMesh(vtkSmartPointer<vtkUnstructuredGrid> volumeMesh);

    std::vector<std::string> GetFileExtensions(){return m_FileExtensions;}

    virtual bool WriteSurfaceFile(std::string filePath);

    virtual bool WriteVolumeFile(std::string filePath);

    virtual bool ReadSurfaceFile(std::string filePath);

    virtual bool ReadVolumeFile(std::string filePath);

    virtual vtkSmartPointer<vtkPolyData> CreateSurfaceMeshFromFile(std::string filePath);

    virtual vtkSmartPointer<vtkUnstructuredGrid> CreateVolumeMeshFromFile(std::string filePath);

//    virtual bool WriteMeshComplete(vtkSmartPointer<vtkPolyData> surfaceMesh, vtkSmartPointer<vtkUnstructuredGrid> volumeMesh, svModelElement* modelElement, std::string meshDir) {return false;}

//    virtual bool WriteMeshComplete(std::string meshDir);

  protected:

//    std::string m_ModelName;

    svModelElement* m_ModelElement;

    std::string m_Type;

    vtkSmartPointer<vtkPolyData> m_SurfaceMesh;

    vtkSmartPointer<vtkUnstructuredGrid> m_VolumeMesh;

    std::vector<std::string> m_CommandHistory;

    std::vector<std::string> m_FileExtensions;

  };

#endif // SVMESH_H
