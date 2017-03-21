#include "svSegmentationLegacySaveAction.h"

#include "svSegmentationLegacyIO.h"
#include <mitkNodePredicateDataType.h>

#include <berryPlatform.h>
#include <berryIPreferences.h>
#include <berryIPreferencesService.h>

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
        berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
        berry::IPreferences::Pointer prefs;
        if (prefService)
        {
            prefs = prefService->GetSystemPreferences()->Node("/General");
        }
        else
        {
            prefs = berry::IPreferences::Pointer(0);
        }

        QString lastFilePath="";
        if(prefs.IsNotNull())
        {
            lastFilePath = prefs->Get("LastFileSavePath", "");
        }
        if(lastFilePath=="")
            lastFilePath=QDir::homePath();


        QString segDir = QFileDialog::getExistingDirectory(NULL, tr("Export as Legacy Segmentations (Choose Directory)"),
                                                             lastFilePath,
                                                             QFileDialog::DontResolveSymlinks
                                                             | QFileDialog::DontUseNativeDialog
                                                             );

        segDir=segDir.trimmed();
        if(segDir.isEmpty()) return;

        mitk::DataStorage::SetOfObjects::ConstPointer rsContourGroup=m_DataStorage->GetDerivations(selectedNode,mitk::NodePredicateDataType::New("svContourGroup"));

        mitk::DataStorage::SetOfObjects::ConstPointer rsSeg3D=m_DataStorage->GetDerivations(selectedNode,mitk::NodePredicateDataType::New("svSeg3D"));

        svSegmentationLegacyIO::WriteFiles(rsContourGroup, rsSeg3D, segDir);

        if(prefs.IsNotNull())
         {
             prefs->Put("LastFileSavePath", segDir);
             prefs->Flush();
         }
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
