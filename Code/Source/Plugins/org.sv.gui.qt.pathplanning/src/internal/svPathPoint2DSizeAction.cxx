#include "svPathPoint2DSizeAction.h"

#include "svPath.h"

#include <mitkNodePredicateDataType.h>

#include <QInputDialog>

svPathPoint2DSizeAction::svPathPoint2DSizeAction()
    : m_Functionality(NULL)
{
}

svPathPoint2DSizeAction::~svPathPoint2DSizeAction()
{
}

void svPathPoint2DSizeAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isPath = mitk::NodePredicateDataType::New("svPath");

    float initialValue=10.0f;
    if(isPath->CheckNode(selectedNode))
        selectedNode->GetFloatProperty ("point 2D display size", initialValue);

    bool ok;
    float size=QInputDialog::getDouble(NULL, "Point 2D Display Size", "Size:", initialValue, 0.1, 100.0, 1, &ok);

    if(!ok)
        return;

    mitk::DataNode::Pointer pathFolderNode=NULL;
    if(selectedNode.IsNotNull())
    {
        mitk::DataStorage::SetOfObjects::ConstPointer rs = m_DataStorage->GetSources(selectedNode);
        if(rs->size()>0){
            pathFolderNode=rs->GetElement(0);
            if(pathFolderNode.IsNotNull())
                pathFolderNode->SetFloatProperty("point 2D display size",size);
        }
    }

    for(int i=0;i<selectedNodes.size();i++)
    {
        mitk::DataNode::Pointer node = selectedNodes[i];
        if(!isPath->CheckNode(node))
            continue;

        node->SetFloatProperty("point 2D display size",size);
        svPath* path=dynamic_cast<svPath*>(node->GetData());
        if(path)
        {
            path->SetProp("point 2D display size",QString::number(size).toStdString());
            path->SetDataModified();
        }
    }

}

void svPathPoint2DSizeAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

void svPathPoint2DSizeAction::SetFunctionality(berry::QtViewPart *functionality)
{
    m_Functionality=functionality;
}

