#ifndef SVPATHOPERATION_H
#define SVPATHOPERATION_H

#include "SimVascular.h"

#include <svPathExports.h>

#include "mitkOperation.h"
#include "svPathElement.h"

class SVPATH_EXPORT svPathOperation : public mitk::Operation
{
public:

    enum PathOperationType {OpINSERTCONTROLPOINT, OpREMOVECONTROLPOINT, OpMOVECONTROLPOINT, OpSELECTCONTROLPOINT, OpDESELECTALL, OpINSERTPATHELEMENT, OpREMOVEPATHELEMENT, OpSETPATHELEMENT};

    svPathOperation(mitk::OperationType operationType);

    svPathOperation(mitk::OperationType operationType, unsigned int timeStep);

    svPathOperation(mitk::OperationType operationType, mitk::Point3D point, int index);

    svPathOperation(mitk::OperationType operationType, unsigned int timeStep, mitk::Point3D point, int index);

//    svPathOperation(mitk::OperationType operationType, int index, bool selected);

    svPathOperation(mitk::OperationType operationType, unsigned int timeStep, int index, bool selected);

    svPathOperation(mitk::OperationType operationType, svPathElement* pathElement);

    svPathOperation(mitk::OperationType operationType, unsigned int timeStep, svPathElement* pathElement);

    virtual ~svPathOperation();

    mitk::Point3D GetPoint();

    svPathElement* GetPathElement();

    int GetIndex();

    unsigned int GetTimeStep() const;

    bool GetSelected();

private:

    mitk::Point3D m_Point;

    svPathElement* m_PathElement;

    int m_Index;

    unsigned int m_TimeStep;

    bool m_Selected;

};

#endif // SVPATHOPERATION_H
