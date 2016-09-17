#ifndef SVMODELOPERATION_H
#define SVMODELOPERATION_H

#include <svModelExports.h>

#include "mitkOperation.h"
#include "svModelElement.h"

class SVMODEL_EXPORT svModelOperation : public mitk::Operation
{
public:

    enum ModelOperationType {OpINSERTMODELELEMENT, OpREMOVEMODELELEMENT, OpSETMODELELEMENT};

    svModelOperation(mitk::OperationType operationType, svModelElement* modelElement);

    svModelOperation(mitk::OperationType operationType, unsigned int timeStep, svModelElement* modelElement);

    virtual ~svModelOperation();

    svModelElement* GetModelElement();

    unsigned int GetTimeStep() const;

private:

    svModelElement* m_ModelElement;

    unsigned int m_TimeStep;

};

#endif // SVMODELOPERATION_H
