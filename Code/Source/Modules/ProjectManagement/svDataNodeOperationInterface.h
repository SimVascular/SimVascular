
#ifndef SVDATANODEOPERATIONINTERFACE_H
#define SVDATANODEOPERATIONINTERFACE_H

#include "SimVascular.h"

#include <svProjectManagementExports.h>

#include "itkObject.h"

#include "mitkOperation.h"
#include "mitkOperationActor.h"


class SVPROJECTMANAGEMENT_EXPORT svDataNodeOperationInterface : public itk::Object, public mitk::OperationActor
{

public:

    svDataNodeOperationInterface();
    ~svDataNodeOperationInterface();

    virtual void  ExecuteOperation(mitk::Operation* op) override;

private:


};


#endif // SVDATANODEOPERATIONINTERFACE_H
