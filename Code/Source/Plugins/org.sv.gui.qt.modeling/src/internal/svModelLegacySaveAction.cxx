#include "svModelLegacySaveAction.h"

#include "svModel.h"
#include "svModelLegacyIO.h"
#include "svModelElementFactory.h"

#include <mitkNodePredicateDataType.h>

#include <berryIPreferencesService.h>
#include <berryIPreferences.h>
#include <berryPlatform.h>

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

    mitk::NodePredicateDataType::Pointer isModel = mitk::NodePredicateDataType::New("svModel");

    svModel* model=dynamic_cast<svModel*>(selectedNode->GetData());
    if(!model) return;

    svModelElement* modelElement=model->GetModelElement();
    if(!modelElement) return;

    QString fileFilter="";
    std::string modelType=model->GetType();
    auto exts=svModelElementFactory::GetFileExtensions(modelType);

    if(exts.size()>0)
    {
        fileFilter+="All (*);; Supported Formats (";
        for(int i=0;i<exts.size();i++)
            fileFilter=fileFilter+" *."+ QString::fromStdString(exts[i]);

        fileFilter+=")";
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

        QString lastFileSavePath="";
        if(prefs.IsNotNull())
        {
            lastFileSavePath = prefs->Get("LastFileSavePath", "");
        }

        if(lastFileSavePath=="")
            lastFileSavePath=QDir::homePath();

        QString fileName = QFileDialog::getSaveFileName(NULL
                                                        ,tr("Export as Legacy Model")
                                                        ,lastFileSavePath
                                                        ,fileFilter
                                                        ,NULL
                                                        ,QFileDialog::DontUseNativeDialog
                                                        );

        fileName=fileName.trimmed();
        if(fileName.isEmpty()) return;

        bool hasExt=false;

        if(exts.size()>0)
        {
            for(int i=0;i<exts.size();i++)
            {
                if(fileName.endsWith(QString::fromStdString("."+exts[i])))
                {
                    hasExt=true;
                    break;
                }

            }

            if(!hasExt)
                fileName=fileName+"."+QString::fromStdString(exts[0]);
        }

        fileName=fileName.remove(".xmt_txt");

        svModelLegacyIO::WriteFile(selectedNode, fileName);

        if(prefs.IsNotNull())
        {
            prefs->Put("LastFileSavePath", fileName);
            prefs->Flush();
        }
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
