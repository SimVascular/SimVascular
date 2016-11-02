#ifndef SVMESH_H
#define SVMESH_H

#include <svMeshExports.h>

#include <svModelElement.h>

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

    svModelElement* GetModelElement() const;

    virtual bool SetModelElement(svModelElement* modelElement);

    void CalculateBoundingBox(double *bounds);

    virtual bool ExecuteCommand(std::string cmd) {}

    bool ExecuteCommands(std::vector<std::string> cmds);

//    bool ExecuteCommandFile(std::string filePath);

    std::vector<std::string> GetCommandHistory() const;

    void SetCommandHistory(std::vector<std::string> history);

    bool ExcuteCommandHistory();

  protected:

    std::string m_ModelName;

    svModelElement* m_ModelElement;

    std::string m_Type;

    vtkSmartPointer<vtkPolyData> m_SurfaceMesh;

    vtkSmartPointer<vtkUnstructuredGrid> m_VolumeMesh;

    std::vector<std::string> m_CommandHistory;

  };


#endif // SVMESH_H
