#include "svPathOperation.h"

svPathOperation::svPathOperation(mitk::OperationType operationType)
    : mitk::Operation(operationType)
{
}

svPathOperation::svPathOperation(mitk::OperationType operationType, unsigned int timeStep)
    : mitk::Operation(operationType)
    , m_TimeStep(timeStep)
{
}

svPathOperation::svPathOperation(mitk::OperationType operationType, mitk::Point3D point, int index)
    : mitk::Operation(operationType)
    , m_Point(point)
    , m_Index(index)
{
}

svPathOperation::svPathOperation(mitk::OperationType operationType, unsigned int timeStep, mitk::Point3D point, int index)
    : mitk::Operation(operationType)
    , m_TimeStep(timeStep)
    , m_Point(point)
    , m_Index(index)
{
}

//svPathOperation::svPathOperation(mitk::OperationType operationType, int index,  bool selected)
//    : mitk::Operation(operationType)
//    , m_Index(index)
//    , m_Selected(selected)
//{
//}

svPathOperation::svPathOperation(mitk::OperationType operationType, unsigned int timeStep, int index, bool selected)
    : mitk::Operation(operationType)
    , m_TimeStep(timeStep)
    , m_Index(index)
    , m_Selected(selected)
{
}

svPathOperation::svPathOperation(mitk::OperationType operationType, svPathElement* pathElement)
    : mitk::Operation(operationType)
    , m_PathElement(pathElement)
{
}

svPathOperation::svPathOperation(mitk::OperationType operationType, unsigned int timeStep, svPathElement* pathElement)
    : mitk::Operation(operationType)
    , m_TimeStep(timeStep)
    , m_PathElement(pathElement)
{
}

svPathOperation::~svPathOperation()
{
}

mitk::Point3D svPathOperation::GetPoint()
{
    return m_Point;
}

svPathElement* svPathOperation::GetPathElement()
{
    return m_PathElement;
}

int svPathOperation::GetIndex()
{
    return m_Index;
}

unsigned int svPathOperation::GetTimeStep() const
{
    return m_TimeStep;
}

bool svPathOperation::GetSelected()
{
    return m_Selected;
}
