#include "svPathLegacySaveAction.h"

#include "svPathLegacyIO.h"
#include <mitkNodePredicateDataType.h>

#include <QFileDialog>

svPathLegacySaveAction::svPathLegacySaveAction()
{
}

svPathLegacySaveAction::~svPathLegacySaveAction()
{
}

void svPathLegacySaveAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isPathFolder = mitk::NodePredicateDataType::New("svPathFolder");

    if(!isPathFolder->CheckNode(selectedNode))
    {
        return;
    }

    try
    {
        QString fileName = QFileDialog::getSaveFileName(NULL
                                                        ,tr("Save as Legacy Paths")
                                                        ,QDir::homePath()
                                                        ,tr("SimVascular Legacy Paths (*.paths)")
                                                        ,NULL
                                                        ,QFileDialog::DontUseNativeDialog
                                                        );

        if(!fileName.isEmpty()){
            if ( fileName.right(6) != ".paths" ) fileName += ".paths";

            svPathLegacyIO::WriteFile(m_DataStorage->GetDerivations (selectedNode),fileName);
        }

    }
    catch(...)
    {
        MITK_ERROR << "Legacy Paths Saving Error!";
    }
}

void svPathLegacySaveAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}
