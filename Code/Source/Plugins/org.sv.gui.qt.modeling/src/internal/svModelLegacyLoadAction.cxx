#include "svModelLegacyLoadAction.h"

#include "svModelLegacyIO.h"
#include <mitkNodePredicateDataType.h>

#include <QFileDialog>

svModelLegacyLoadAction::svModelLegacyLoadAction()
{
}

svModelLegacyLoadAction::~svModelLegacyLoadAction()
{
}

void svModelLegacyLoadAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
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

        std::vector<mitk::DataNode::Pointer> modelNodes=svModelLegacyIO::ReadFiles(modelDir);

        for(int i=0;i<modelNodes.size();i++)
            m_DataStorage->Add(modelNodes[i],selectedNode);

    }
    catch(...)
    {
        MITK_ERROR << "Legacy Model Loading Error!";
    }
}

void svModelLegacyLoadAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

