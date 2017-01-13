
#include "svDataNodeOperationInterface.h"

#include "svProjectManager.h"
#include "svDataNodeOperation.h"

svDataNodeOperationInterface::svDataNodeOperationInterface()
{
}

svDataNodeOperationInterface::~svDataNodeOperationInterface()
{
}

void svDataNodeOperationInterface::ExecuteOperation(mitk::Operation* op)
{
    svDataNodeOperation* operation = dynamic_cast<svDataNodeOperation*>(op);

    if ( operation==NULL )
    {
        MITK_ERROR << "No valid svDataNodeOperation" << std::endl;
        return;
    }

    mitk::DataStorage::Pointer dataStorage=operation->GetDataStorage();
    mitk::DataNode::Pointer dataNode=operation->GetDataNode();
    mitk::DataNode::Pointer parentNode=operation->GetParentNode();

    switch (operation->GetOperationType())
    {

    case svDataNodeOperation::OpADDDATANODE:
    {
        svProjectManager::AddDataNode(dataStorage,dataNode,parentNode);
    }
        break;

    case svDataNodeOperation::OpREMOVEDATANODE:
    {
        svProjectManager::RemoveDataNode(dataStorage,dataNode,parentNode);
    }
        break;

    default:
//        itkWarningMacro("svDataNodeOperationInterface could not understrand the operation. Please check!");
        break;
    }

    mitk::OperationEndEvent endevent(operation);
    ((const itk::Object*)this)->InvokeEvent(endevent);

}
