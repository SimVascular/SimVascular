#include "svModelOperation.h"

svModelOperation::svModelOperation(mitk::OperationType operationType, svModelElement* modelElement)
    : mitk::Operation(operationType)
    , m_ModelElement(modelElement)
{
}

svModelOperation::svModelOperation(mitk::OperationType operationType, unsigned int timeStep, svModelElement* modelElement)
    : mitk::Operation(operationType)
    , m_TimeStep(timeStep)
    , m_ModelElement(modelElement)
{
}

svModelOperation::~svModelOperation()
{
}

svModelElement* svModelOperation::GetModelElement()
{
    return m_ModelElement;
}

unsigned int svModelOperation::GetTimeStep() const
{
    return m_TimeStep;
}
