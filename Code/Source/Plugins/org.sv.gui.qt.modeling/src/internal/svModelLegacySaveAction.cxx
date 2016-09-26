#include "svModelLegacySaveAction.h"

#include "svModelLegacyIO.h"
#include <mitkNodePredicateDataType.h>

#include <QFileDialog>

svModelLegacySaveAction::svModelLegacySaveAction()
{
}

svModelLegacySaveAction::~svModelLegacySaveAction()
{
}

void svModelLegacySaveAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isModelFolder = mitk::NodePredicateDataType::New("svModelFolder");

    if(!isModelFolder->CheckNode(selectedNode))
    {
        return;
    }

    try
    {
        QString modelDir = QFileDialog::getExistingDirectory(NULL, tr("Choose Directory"),
                                                             QDir::homePath(),
                                                             QFileDialog::ShowDirsOnly
                                                             | QFileDialog::DontResolveSymlinks
                                                             | QFileDialog::DontUseNativeDialog
                                                             );

        if(modelDir.trimmed().isEmpty()) return;

        mitk::DataStorage::SetOfObjects::ConstPointer rsModel=m_DataStorage->GetDerivations(selectedNode,mitk::NodePredicateDataType::New("svModel"));

        svModelLegacyIO::WriteFiles(rsModel, modelDir);
    }
    catch(...)
    {
        MITK_ERROR << "Legacy Model Saving Error!";
    }
}

void svModelLegacySaveAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}
