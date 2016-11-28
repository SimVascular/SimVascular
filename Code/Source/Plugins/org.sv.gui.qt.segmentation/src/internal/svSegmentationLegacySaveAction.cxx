#include "svSegmentationLegacySaveAction.h"

#include "svSegmentationLegacyIO.h"
#include <mitkNodePredicateDataType.h>

#include <QFileDialog>

svSegmentationLegacySaveAction::svSegmentationLegacySaveAction()
{
}

svSegmentationLegacySaveAction::~svSegmentationLegacySaveAction()
{
}

void svSegmentationLegacySaveAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
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

        mitk::DataStorage::SetOfObjects::ConstPointer rsContourGroup=m_DataStorage->GetDerivations(selectedNode,mitk::NodePredicateDataType::New("svContourGroup"));

        mitk::DataStorage::SetOfObjects::ConstPointer rsSeg3D=m_DataStorage->GetDerivations(selectedNode,mitk::NodePredicateDataType::New("svSeg3D"));

        svSegmentationLegacyIO::WriteFiles(rsContourGroup, rsSeg3D, segDir);
    }
    catch(...)
    {
        MITK_ERROR << "Legacy Segmentations Saving Error!";
    }
}

void svSegmentationLegacySaveAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}
