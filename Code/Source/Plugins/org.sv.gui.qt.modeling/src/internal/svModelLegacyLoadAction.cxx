#include "svModelLegacyLoadAction.h"
#include "simvascular_options.h"

#include "svModelLegacyIO.h"
#include <mitkNodePredicateDataType.h>

#include <berryIPreferencesService.h>
#include <berryIPreferences.h>
#include <berryPlatform.h>

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

        QString lastFileOpenPath=QString();
        if(prefs.IsNotNull())
        {
            lastFileOpenPath = prefs->Get("LastFileOpenPath", "");
        }

        QString filter="Legacy Models (*.vtp";

#ifdef SV_USE_OpenCASCADE
        filter=filter+" *.brep *.step *.stl *.iges";
#endif

        filter=filter+")";

        QString modelFilePath = QFileDialog::getOpenFileName(NULL, tr("Load Model")
                                                             , lastFileOpenPath
                                                             , tr(filter.toStdString().c_str())
                                                             , NULL
                                                             , QFileDialog::DontUseNativeDialog);

        if(modelFilePath.trimmed().isEmpty()) return;
        mitk::DataNode::Pointer modelNode=svModelLegacyIO::ReadFile(modelFilePath);
        if(modelNode.IsNotNull())
            m_DataStorage->Add(modelNode,selectedNode);

        if(prefs.IsNotNull())
        {
            prefs->Put("LastFileOpenPath", modelFilePath);
            prefs->Flush();
        }

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

