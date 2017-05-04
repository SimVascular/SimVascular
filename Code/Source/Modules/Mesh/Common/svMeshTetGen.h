#ifndef SVMESHTETGEN_H
#define SVMESHTETGEN_H

#include <svMeshExports.h>

#include <svMesh.h>


#include <cvTetGenMeshObject.h>

class SVMESH_EXPORT svMeshTetGen : public svMesh
{
public:

    svMeshTetGen();

    svMeshTetGen(const svMeshTetGen &other);

    virtual ~svMeshTetGen();

    svMeshTetGen* Clone() override;

    void InitNewMesher() override;

    bool SetModelElement(svModelElement* modelElement) override;

    bool Execute(std::string flag, double values[20], std::string strValues[5], bool option, std::string& msg) override;

    bool ParseCommand(std::string cmd, std::string& flag, double values[20], std::string strValues[5], bool& option, std::string& msg) override;

    cvTetGenMeshObject* GetMesher();

    static svMesh* CreateMesh();

    bool WriteSurfaceFile(std::string filePath) override;

    bool WriteVolumeFile(std::string filePath) override;

    bool ReadSurfaceFile(std::string filePath) override;

    bool ReadVolumeFile(std::string filePath) override;

    vtkSmartPointer<vtkPolyData> CreateSurfaceMeshFromFile(std::string filePath) override;

    vtkSmartPointer<vtkUnstructuredGrid> CreateVolumeMeshFromFile(std::string filePath) override;

//    bool WriteMeshComplete(vtkSmartPointer<vtkPolyData> surfaceMesh, vtkSmartPointer<vtkUnstructuredGrid> volumeMesh, svModelElement* modelElement, std::string meshDir) override;

//    bool WriteMeshComplete(std::string meshDir) override;

  protected:

    cvTetGenMeshObject* m_cvTetGenMesh;

  };


#endif // svMeshTetGen_H
