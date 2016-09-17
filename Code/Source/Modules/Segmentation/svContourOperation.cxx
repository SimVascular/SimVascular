#include "svContourOperation.h"

svContourOperation::svContourOperation(mitk::OperationType operationType, mitk::Point3D point, int contourIndex, int index)
    : mitk::Operation(operationType)
    , m_Point(point)
    , m_ContourIndex(contourIndex)
    , m_Index(index)
{
}

svContourOperation::svContourOperation(mitk::OperationType operationType, unsigned int timeStep, mitk::Point3D point, int contourIndex, int index)
    : mitk::Operation(operationType)
    , m_TimeStep(timeStep)
    , m_Point(point)
    , m_ContourIndex(contourIndex)
    , m_Index(index)
{
}

svContourOperation::svContourOperation(mitk::OperationType operationType, svContour* contour, int contourIndex)
    : mitk::Operation(operationType)
    , m_Contour(contour)
    , m_ContourIndex(contourIndex)
{
}

svContourOperation::svContourOperation(mitk::OperationType operationType, unsigned int timeStep, svContour* contour, int contourIndex)
    : mitk::Operation(operationType)
    , m_TimeStep(timeStep)
    , m_Contour(contour)
    , m_ContourIndex(contourIndex)
{
}

svContourOperation::svContourOperation(mitk::OperationType operationType, mitk::Point3D point, int index)
    : mitk::Operation(operationType)
    , m_Point(point)
    , m_Index(index)
{
   }

svContourOperation::svContourOperation(mitk::OperationType operationType, unsigned int timeStep, mitk::Point3D point, int index)
    : mitk::Operation(operationType)
    , m_TimeStep(timeStep)
    , m_Point(point)
    , m_Index(index)
{
}

svContourOperation::svContourOperation(mitk::OperationType operationType, svContour* contour)
    : mitk::Operation(operationType)
    , m_Contour(contour)
{
}

svContourOperation::svContourOperation(mitk::OperationType operationType, unsigned int timeStep, svContour* contour)
    : mitk::Operation(operationType)
    , m_TimeStep(timeStep)
    , m_Contour(contour)
{
}

svContourOperation::~svContourOperation()
{
}

mitk::Point3D svContourOperation::GetPoint()
{
    return m_Point;
}

svContour* svContourOperation::GetContour()
{
    return m_Contour;
}

int svContourOperation::GetContourIndex()
{
    return m_ContourIndex;
}

int svContourOperation::GetIndex()
{
    return m_Index;
}

unsigned int svContourOperation::GetTimeStep() const
{
    return m_TimeStep;
}
