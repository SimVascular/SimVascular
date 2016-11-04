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

    bool ParseCommandInternal(std::string cmd, std::string& flag, double values[20], std::string strValues[5], bool& option, std::string& msg) override;

    static bool ParseCommand(std::string cmd, std::string& flag, double values[20], std::string strValues[5], bool& option, std::string& msg);

    cvTetGenMeshObject* GetMesher();

  protected:

    cvTetGenMeshObject* m_cvTetGetMesh;

  };


#endif // svMeshTetGen_H
