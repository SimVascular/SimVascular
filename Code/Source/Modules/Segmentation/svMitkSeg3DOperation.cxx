#include "svMitkSeg3DOperation.h"

svMitkSeg3DOperation::svMitkSeg3DOperation(mitk::OperationType operationType,  svSeg3D* seg3D)
    : mitk::Operation(operationType)
    , m_Seg3D(seg3D)
{
}

svMitkSeg3DOperation::~svMitkSeg3DOperation()
{
}

svSeg3D* svMitkSeg3DOperation::GetSeg3D()
{
    return m_Seg3D;
}

