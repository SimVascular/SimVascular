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

        QString lastFileOpenPath="";
        if(prefs.IsNotNull())
        {
            lastFileOpenPath = prefs->Get("LastFileOpenPath", "");
        }

        if(lastFileOpenPath=="")
            lastFileOpenPath=QDir::homePath();

        QString filter="Legacy Models (*.vtp";

#ifdef SV_USE_OpenCASCADE_QT_GUI
        filter=filter+" *.brep *.step *.stl *.iges";
#endif
#ifdef SV_USE_PARASOLID_QT_GUI
        filter=filter+" *.xmt_txt";
#endif

        filter=filter+")";

        QString modelFilePath = QFileDialog::getOpenFileName(NULL, tr("Load Legacy Model")
                                                             , lastFileOpenPath
                                                             , tr(filter.toStdString().c_str())
                                                             , NULL
                                                             , QFileDialog::DontUseNativeDialog);

        modelFilePath=modelFilePath.trimmed();
        if(modelFilePath.isEmpty())
            return;

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

