#ifndef SVMESHSIMADAPTOR_H
#define SVMESHSIMADAPTOR_H

#include <svMeshExports.h>

#include "svMeshAdaptor.h"
#include "svMeshSim.h"

#include <cvTetGenAdapt.h>

class SVMESH_EXPORT svMeshSimAdaptor : public svMeshAdaptor
{

public:

    svMeshSimAdaptor();

    virtual ~svMeshSimAdaptor();

    virtual bool SetModelElement(svModelElement *modelElement) override;

//    virtual bool SetResultMesh(vtkSmartPointer<vtkUnstructuredGrid> mesh) override;
    virtual bool LoadMeshFromResultVTUFile(std::string filePath) override;

    virtual bool SetAdaptOptions(std::string flag, double value) override;

    virtual bool Adapt() override;

    virtual bool WriteAdaptedSolution(std::string filePath) override;

    svMeshSim* GetAdaptedMesh() override;

    static svMeshAdaptor* CreateAdaptor();

protected:

  cvTetGenMeshObject* m_cvTetGetMesh;
  cvTetGenAdapt* m_cvTetGenAdaptor;

  };

#endif // SVMESHSIMADAPTOR_H
