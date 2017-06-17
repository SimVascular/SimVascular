#include "svContourGroupPoint3DSizeAction.h"

#include "svContourGroup.h"

#include <mitkNodePredicateDataType.h>

#include <QInputDialog>

svContourGroupPoint3DSizeAction::svContourGroupPoint3DSizeAction()
    : m_Functionality(NULL)
{
}

svContourGroupPoint3DSizeAction::~svContourGroupPoint3DSizeAction()
{
}

void svContourGroupPoint3DSizeAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isGroup = mitk::NodePredicateDataType::New("svContourGroup");

    float initialValue=0.1f;
    if(isGroup->CheckNode(selectedNode))
        selectedNode->GetFloatProperty ("point.3dsize", initialValue);

    bool ok;
    float size=QInputDialog::getDouble(NULL, "Point 3D Size", "Size:", initialValue, 0.01, 50.0, 2, &ok);

    if(!ok)
        return;

    mitk::DataNode::Pointer segFolderNode=NULL;
    if(selectedNode.IsNotNull())
    {
        mitk::DataStorage::SetOfObjects::ConstPointer rs = m_DataStorage->GetSources(selectedNode);
        if(rs->size()>0){
            segFolderNode=rs->GetElement(0);
            if(segFolderNode.IsNotNull())
                segFolderNode->SetFloatProperty("point.3dsize",size);
        }
    }

    for(int i=0;i<selectedNodes.size();i++)
    {
        mitk::DataNode::Pointer node = selectedNodes[i];
        if(!isGroup->CheckNode(node))
            continue;

        node->SetFloatProperty("point.3dsize",size);
        svContourGroup* group=dynamic_cast<svContourGroup*>(node->GetData());
        if(group)
        {
            group->SetProp("point size",QString::number(size).toStdString());
            group->SetDataModified();
        }
    }

}

void svContourGroupPoint3DSizeAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

void svContourGroupPoint3DSizeAction::SetFunctionality(berry::QtViewPart *functionality)
{
    m_Functionality=functionality;
}

