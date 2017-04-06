#include "svSegmentationLoadAction.h"

#include "svSegmentationLegacyIO.h"
#include "svContourGroup.h"
#include "svContourGroupIO.h"

#include <mitkNodePredicateDataType.h>
#include <berryPlatform.h>
#include <berryIPreferences.h>
#include <berryIPreferencesService.h>

#include <mitkIOUtil.h>

#include <QFileDialog>

svSegmentationLoadAction::svSegmentationLoadAction()
{
}

svSegmentationLoadAction::~svSegmentationLoadAction()
{
}

void svSegmentationLoadAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
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

        QString filePath = QFileDialog::getOpenFileName(NULL, "Import Segmentation (Choose File)", lastFilePath, tr("SimVascular Contour Group (*.ctgr);;VTP Files (*.vtp)")
                                                              ,NULL,QFileDialog::DontUseNativeDialog);

        filePath=filePath.trimmed();
        if(filePath.isEmpty())
            return;

        QFileInfo fi(filePath);
        std::string baseName=fi.baseName().toStdString();

        if(filePath.endsWith("vtp"))
        {
            mitk::Surface::Pointer surface =mitk::IOUtil::LoadSurface(filePath.toStdString());
            if(surface.IsNotNull())
            {
                mitk::DataNode::Pointer surfaceNode=mitk::DataNode::New();
                surfaceNode->SetData(surface);
                surfaceNode->SetName(baseName);
                m_DataStorage->Add(surfaceNode,selectedNode);
            }
        }
        else if(filePath.endsWith(".ctgr"))
        {
            std::vector<mitk::BaseData::Pointer> nodedata=svContourGroupIO::ReadFile(filePath.toStdString());

            if(nodedata.size()>0)
            {
                mitk::BaseData::Pointer groupdata=nodedata[0];
                if(groupdata.IsNotNull())
                {
                    mitk::DataNode::Pointer groupNode = mitk::DataNode::New();
                    groupNode->SetData(groupdata);
                    groupNode->SetName(baseName);

                    m_DataStorage->Add(groupNode,selectedNode);
                }
            }
        }

        if(prefs.IsNotNull())
         {
             prefs->Put("LastFileOpenPath", filePath);
             prefs->Flush();
         }
    }
    catch(...)
    {
        MITK_ERROR << "Segmentation File Loading Error!";
    }
}

void svSegmentationLoadAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

