#ifndef SVCONTOUROPERATION_H
#define SVCONTOUROPERATION_H

#include "SimVascular.h"

#include <svSegmentationExports.h>

#include "mitkOperation.h"
#include "svContour.h"

class SVSEGMENTATION_EXPORT svContourOperation : public mitk::Operation
{
public:

    enum ContourOperationType {OpINSERTCONTROLPOINT, OpREMOVECONTROLPOINT, OpMOVECONTROLPOINT, OpINSERTCONTOUR, OpREMOVECONTOUR, OpSETCONTOUR};

    svContourOperation(mitk::OperationType operationType, mitk::Point3D point, int coutourIndex, int index);

    svContourOperation(mitk::OperationType operationType, unsigned int timeStep, mitk::Point3D point, int coutourIndex, int index);

    svContourOperation(mitk::OperationType operationType, svContour* contour, int coutourIndex);

    svContourOperation(mitk::OperationType operationType, unsigned int timeStep, svContour* contour, int coutourIndex);

    svContourOperation(mitk::OperationType operationType, mitk::Point3D point, int index);

    svContourOperation(mitk::OperationType operationType, unsigned int timeStep, mitk::Point3D point, int index);

    svContourOperation(mitk::OperationType operationType, svContour* contour);

    svContourOperation(mitk::OperationType operationType, unsigned int timeStep, svContour* contour);

    virtual ~svContourOperation();

    mitk::Point3D GetPoint();

    svContour* GetContour();

    int GetContourIndex();

    int GetIndex();

    unsigned int GetTimeStep() const;

private:

    mitk::Point3D m_Point;

    svContour* m_Contour;

    int m_ContourIndex;

    int m_Index;

    unsigned int m_TimeStep;

};

#endif // SVCONTOUROPERATION_H
