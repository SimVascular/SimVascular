#ifndef SVMESHTETGENADAPTOR_H
#define SVMESHTETGENADAPTOR_H

#include <svMeshExports.h>

#include <svMeshAdaptor.h>
#include <svMeshTetGen.h>

#include <cvTetGenAdapt.h>

class SVMESH_EXPORT svMeshTetGenAdaptor : public svMeshAdaptor
{

public:

    svMeshTetGenAdaptor();

    virtual ~svMeshTetGenAdaptor();

    virtual bool SetModelElement(svModelElement *modelElement) override;

    virtual bool LoadMesh(std::string filePath) override;

    virtual bool SetAdaptOptions(std::string flag, double value) override;

    virtual bool Adapt() override;

    virtual bool WriteAdaptedSolution(std::string filePath) override;

    svMeshTetGen* GetAdaptedMesh() override;

    virtual bool WriteAdaptedMesh(std::string filePath) override;

    static svMeshAdaptor* CreateAdaptor();

protected:

  cvTetGenMeshObject* m_cvTetGetMesh;
  cvTetGenAdapt* m_cvTetGenAdaptor;

  };

#endif // SVMESHTETGENADAPTOR_H
