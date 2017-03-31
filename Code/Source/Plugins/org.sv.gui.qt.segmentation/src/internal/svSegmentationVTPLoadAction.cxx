#include "svSegmentationVTPLoadAction.h"

#include "svSegmentationLegacyIO.h"
#include "svContourGroup.h"

#include <mitkNodePredicateDataType.h>
#include <berryPlatform.h>
#include <berryIPreferences.h>
#include <berryIPreferencesService.h>

#include <mitkIOUtil.h>

#include <QFileDialog>

svSegmentationVTPLoadAction::svSegmentationVTPLoadAction()
{
}

svSegmentationVTPLoadAction::~svSegmentationVTPLoadAction()
{
}

void svSegmentationVTPLoadAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
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
            lastFilePath = prefs->Get("LastFileOpenPath", "");
        }
        if(lastFilePath=="")
            lastFilePath=QDir::homePath();

        QString filePath = QFileDialog::getOpenFileName(NULL, "Choose Vtp File", lastFilePath, tr("VTP Files (*.vtp)")
                                                              ,NULL,QFileDialog::DontUseNativeDialog);

        filePath=filePath.trimmed();
        if(filePath.isEmpty())
            return;

        QFileInfo fi(filePath);
        QString baseName=fi.baseName();

        mitk::Surface::Pointer surface =mitk::IOUtil::LoadSurface(filePath.toStdString());
        if(surface.IsNotNull())
        {
            mitk::DataNode::Pointer surfaceNode=mitk::DataNode::New();
            surfaceNode->SetData(surface);
            surfaceNode->SetName(baseName.toStdString());
            m_DataStorage->Add(surfaceNode,selectedNode);
        }

        if(prefs.IsNotNull())
         {
             prefs->Put("LastFileOpenPath", filePath);
             prefs->Flush();
         }
    }
    catch(...)
    {
        MITK_ERROR << "Vtp File Loading Error!";
    }
}

void svSegmentationVTPLoadAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

