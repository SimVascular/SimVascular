#include "svPathPoint3DSizeAction.h"

#include "svPath.h"

#include <mitkNodePredicateDataType.h>

#include <QInputDialog>

svPathPoint3DSizeAction::svPathPoint3DSizeAction()
    : m_Functionality(NULL)
{
}

svPathPoint3DSizeAction::~svPathPoint3DSizeAction()
{
}

void svPathPoint3DSizeAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isPath = mitk::NodePredicateDataType::New("svPath");

    float initialValue=1.0f;
    if(isPath->CheckNode(selectedNode))
        selectedNode->GetFloatProperty ("point size", initialValue);

    bool ok;
    float size=QInputDialog::getDouble(NULL, "Point 3D Size", "Size:", initialValue, 0.01, 50.0, 2, &ok);

    if(!ok)
        return;

    for(int i=0;i<selectedNodes.size();i++)
    {
        mitk::DataNode::Pointer node = selectedNodes[i];
        if(!isPath->CheckNode(node))
            continue;

        node->SetFloatProperty("point size",size);
        svPath* path=dynamic_cast<svPath*>(node->GetData());
        if(path)
        {
            path->SetProp("point size",QString::number(size).toStdString());
            path->SetDataModified();
        }
    }

}

void svPathPoint3DSizeAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

void svPathPoint3DSizeAction::SetFunctionality(berry::QtViewPart *functionality)
{
    m_Functionality=functionality;
}

