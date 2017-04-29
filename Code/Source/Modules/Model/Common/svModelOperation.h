#ifndef SVMODELOPERATION_H
#define SVMODELOPERATION_H

#include <svModelExports.h>

#include "mitkOperation.h"
#include "svModelElement.h"

class SVMODEL_EXPORT svModelOperation : public mitk::Operation
{
public:

    enum ModelOperationType {OpINSERTMODELELEMENT, OpREMOVEMODELELEMENT, OpSETMODELELEMENT, OpSETVTKPOLYDATA};

    svModelOperation(mitk::OperationType operationType, svModelElement* modelElement);

    svModelOperation(mitk::OperationType operationType, unsigned int timeStep, svModelElement* modelElement);

    svModelOperation(mitk::OperationType operationType, vtkPolyData* vpd);

    svModelOperation(mitk::OperationType operationType, unsigned int timeStep, vtkSmartPointer<vtkPolyData> vpd);

    virtual ~svModelOperation();

    svModelElement* GetModelElement();

    unsigned int GetTimeStep() const;

    vtkSmartPointer<vtkPolyData> GetVtkPolyData();

private:

    svModelElement* m_ModelElement;

    unsigned int m_TimeStep;

    vtkSmartPointer<vtkPolyData> m_vpd;

};

#endif // SVMODELOPERATION_H
