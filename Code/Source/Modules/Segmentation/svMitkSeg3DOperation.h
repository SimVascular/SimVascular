#ifndef SVMITKSEG3DOPERATION_H
#define SVMITKSEG3DOPERATION_H

#include "SimVascular.h"

#include <svSegmentationExports.h>

#include "mitkOperation.h"
#include "svSeg3D.h"

class SVSEGMENTATION_EXPORT svMitkSeg3DOperation : public mitk::Operation
{
public:

    enum Seg3DOperationType {OpSETSEG3D};

    svMitkSeg3DOperation(mitk::OperationType operationType, svSeg3D* seg3D);

    virtual ~svMitkSeg3DOperation();

    svSeg3D* GetSeg3D();

private:

    svSeg3D* m_Seg3D;

};

#endif // SVMITKSEG3DOPERATION_H
