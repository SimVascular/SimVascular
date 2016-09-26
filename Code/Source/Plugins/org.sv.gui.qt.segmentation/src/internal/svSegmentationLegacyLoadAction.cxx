#include "svSegmentationLegacyLoadAction.h"

#include "svSegmentationLegacyIO.h"
#include <mitkNodePredicateDataType.h>

#include <QFileDialog>

svSegmentationLegacyLoadAction::svSegmentationLegacyLoadAction()
{
}

svSegmentationLegacyLoadAction::~svSegmentationLegacyLoadAction()
{
}

void svSegmentationLegacyLoadAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isSegFolder = mitk::NodePredicateDataType::New("svSegmentationFolder");

    if(!isSegFolder->CheckNode(selectedNode))
    {
        return;
    }

    try
    {
        QString segDir = QFileDialog::getExistingDirectory(NULL, tr("Choose Directory"),
                                                             QDir::homePath(),
                                                             QFileDialog::ShowDirsOnly
                                                             | QFileDialog::DontResolveSymlinks
                                                             | QFileDialog::DontUseNativeDialog
                                                             );

        if(segDir.trimmed().isEmpty()) return;

        std::vector<mitk::DataNode::Pointer> segNodes=svSegmentationLegacyIO::ReadFiles(segDir);

        for(int i=0;i<segNodes.size();i++)
            m_DataStorage->Add(segNodes[i],selectedNode);

    }
    catch(...)
    {
        MITK_ERROR << "Legacy Segmentations Loading Error!";
    }
}

void svSegmentationLegacyLoadAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

