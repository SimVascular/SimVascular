#ifndef SVMESHSIMADAPTOR_H
#define SVMESHSIMADAPTOR_H

#include <svMeshExports.h>

#include "svMeshAdaptor.h"
#include "svMeshSim.h"

#include <cvMeshSimAdapt.h>

class SVMESH_EXPORT svMeshSimAdaptor : public svMeshAdaptor
{

public:

    svMeshSimAdaptor();

    virtual ~svMeshSimAdaptor();

    virtual bool SetModelElement(svModelElement *modelElement) override;

    virtual bool LoadMesh(std::string filePath) override;

    virtual bool SetAdaptOptions(std::string flag, double value) override;

    virtual bool Adapt() override;

    virtual bool WriteAdaptedSolution(std::string filePath) override;

    svMeshSim* GetAdaptedMesh() override;

    virtual bool WriteAdaptedMesh(std::string filePath) override;

    static svMeshAdaptor* CreateAdaptor();

protected:

    cvMeshSimMeshObject* m_cvMeshSimMesh;
    cvMeshSimAdapt* m_cvMeshSimAdaptor;
    svModelElement* m_ModelElement;

  };

#endif // SVMESHSIMADAPTOR_H
