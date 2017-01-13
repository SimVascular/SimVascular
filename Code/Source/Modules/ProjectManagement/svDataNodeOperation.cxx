#include "svDataNodeOperation.h"

svDataNodeOperation::svDataNodeOperation(mitk::OperationType operationType, mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer dataNode, mitk::DataNode::Pointer parentNode )
    : mitk::Operation(operationType)
    , m_DataStorage(dataStorage)
    , m_DataNode(dataNode)
    , m_ParentNode(parentNode)
{
}

svDataNodeOperation::~svDataNodeOperation()
{
}

mitk::DataStorage::Pointer svDataNodeOperation::GetDataStorage()
{
    return m_DataStorage;
}

mitk::DataNode::Pointer svDataNodeOperation::GetDataNode()
{
    return m_DataNode;
}

mitk::DataNode::Pointer svDataNodeOperation::GetParentNode()
{
    return m_ParentNode;
}

