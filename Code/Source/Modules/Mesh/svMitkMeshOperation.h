#ifndef SVMITKMESHOPERATION_H
#define SVMITKMESHOPERATION_H

#include <svMeshExports.h>

#include "mitkOperation.h"
#include "svMesh.h"

class SVMESH_EXPORT svMitkMeshOperation : public mitk::Operation
{
public:

    enum ModelOperationType {OpSETMESH};

    svMitkMeshOperation(mitk::OperationType operationType, svMesh* mesh);

    svMitkMeshOperation(mitk::OperationType operationType, unsigned int timeStep, svMesh* mesh);

    virtual ~svMitkMeshOperation();

    svMesh* GetMesh();

    unsigned int GetTimeStep() const;

private:

    svMesh* m_Mesh;

    unsigned int m_TimeStep;

};

#endif // SVMITKMESHOPERATION_H
