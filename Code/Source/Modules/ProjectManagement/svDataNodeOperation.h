#ifndef SVDATANODEOPERATION_H
#define SVDATANODEOPERATION_H

#include "SimVascular.h"

#include <svProjectManagementExports.h>

#include <mitkOperation.h>
#include <mitkDataNode.h>
#include <mitkDataStorage.h>


class SVPROJECTMANAGEMENT_EXPORT svDataNodeOperation : public mitk::Operation
{
public:

    enum ModelOperationType {OpADDDATANODE, OpREMOVEDATANODE};

    svDataNodeOperation(mitk::OperationType operationType, mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer dataNode, mitk::DataNode::Pointer parentNode = NULL);

    virtual ~svDataNodeOperation();

    mitk::DataStorage::Pointer GetDataStorage();

    mitk::DataNode::Pointer GetDataNode();

    mitk::DataNode::Pointer GetParentNode();

private:

    mitk::DataStorage::Pointer m_DataStorage;
    mitk::DataNode::Pointer m_DataNode;
    mitk::DataNode::Pointer m_ParentNode;

};

#endif // SVDATANODEOPERATION_H
