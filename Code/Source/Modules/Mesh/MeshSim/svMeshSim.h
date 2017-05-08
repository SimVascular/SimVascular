#ifndef SVMESHSIM_H
#define SVMESHSIM_H

#include <svMeshSimExports.h>

#include "svMesh.h"

#include "cvMeshSimMeshObject.h"

class SVMESHSIM_EXPORT svMeshSim : public svMesh
{
public:

    svMeshSim();

    svMeshSim(const svMeshSim &other);

    virtual ~svMeshSim();

    svMeshSim* Clone() override;

    void InitNewMesher() override;

    bool SetModelElement(svModelElement* modelElement) override;

    bool Execute(std::string flag, double values[20], std::string strValues[5], bool option, std::string& msg) override;

    bool ParseCommand(std::string cmd, std::string& flag, double values[20], std::string strValues[5], bool& option, std::string& msg) override;

    cvMeshSimMeshObject* GetMesher();

    static svMesh* CreateMesh();

    bool WriteSurfaceFile(std::string filePath) override;

    bool WriteVolumeFile(std::string filePath) override;

    bool ReadSurfaceFile(std::string filePath) override;

    bool ReadVolumeFile(std::string filePath) override;

    vtkSmartPointer<vtkPolyData> CreateSurfaceMeshFromFile(std::string filePath) override;

    vtkSmartPointer<vtkUnstructuredGrid> CreateVolumeMeshFromFile(std::string filePath) override;

//    bool WriteMeshComplete(vtkSmartPointer<vtkPolyData> surfaceMesh, vtkSmartPointer<vtkUnstructuredGrid> volumeMesh, svModelElement* modelElement, std::string meshDir) override;

//    bool WriteMeshComplete(std::string meshDir) override;

    vtkSmartPointer<vtkPolyData> CreateSurfaceMeshContainingModelFaceIDs();

  protected:

    cvMeshSimMeshObject* m_cvMeshSimMesh;

  };


#endif // SVMESHSIM_H
