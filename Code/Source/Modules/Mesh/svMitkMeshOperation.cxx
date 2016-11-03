#include "svMitkMeshOperation.h"

svMitkMeshOperation::svMitkMeshOperation(mitk::OperationType operationType, svMesh* mesh)
    : mitk::Operation(operationType)
    , m_Mesh(mesh)
{
}

svMitkMeshOperation::svMitkMeshOperation(mitk::OperationType operationType, unsigned int timeStep, svMesh* mesh)
    : mitk::Operation(operationType)
    , m_TimeStep(timeStep)
    , m_Mesh(mesh)
{
}

svMitkMeshOperation::~svMitkMeshOperation()
{
}

svMesh* svMitkMeshOperation::GetMesh()
{
    return m_Mesh;
}

unsigned int svMitkMeshOperation::GetTimeStep() const
{
    return m_TimeStep;
}
