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

    bool SetModelElement(svModelElement* modelElement) override;

    bool ExecuteCommand(std::string cmd) override;

  protected:

    cvTetGenMeshObject* m_cvTetGetMesh;

  };


#endif // svMeshTetGen_H
