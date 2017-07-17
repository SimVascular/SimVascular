#include "svModelFaceInfoExportAction.h"

#include "svModel.h"
#include "svModelLegacyIO.h"
#include <mitkNodePredicateDataType.h>

#include <berryIPreferencesService.h>
#include <berryIPreferences.h>
#include <berryPlatform.h>

#include <QFileDialog>

svModelFaceInfoExportAction::svModelFaceInfoExportAction()
{
}

svModelFaceInfoExportAction::~svModelFaceInfoExportAction()
{
}

void svModelFaceInfoExportAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isModel = mitk::NodePredicateDataType::New("svModel");

    svModel* model=dynamic_cast<svModel*>(selectedNode->GetData());
    if(!model) return;

    svModelElement* modelElement=model->GetModelElement();
    if(!modelElement) return;

    std::string modelName=selectedNode->GetName();

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
                                                        ,tr("Export Cap Info")
                                                        ,lastFileSavePath
                                                        ,tr("All Files (*)"));

        fileName=fileName.trimmed();
        if(fileName.isEmpty()) return;

        QFile infoFile(fileName);
        if(infoFile.open(QIODevice::WriteOnly | QIODevice::Text))
        {
            QTextStream out(&infoFile);
            out<<"#Face_Name\tArea\n";

            std::vector<int> capIDs=modelElement->GetCapFaceIDs();
            std::map<std::string,int> capMap;
            for(int i=0;i<capIDs.size();i++)
                capMap[modelElement->GetFaceName(capIDs[i])]=capIDs[i];

            auto it=capMap.begin();
            while(it!=capMap.end())
            {
                out<<it->first.c_str()<<"\t"<<QString::number(modelElement->GetFaceArea(it->second))<<"\n";
                it++;
            }

            infoFile.close();
        }

        if(prefs.IsNotNull())
        {
            prefs->Put("LastFileSavePath", fileName);
            prefs->Flush();
        }
    }
    catch(...)
    {
        MITK_ERROR << "Faied to expor face info for model "<<modelName.c_str() <<"!";
    }
}

void svModelFaceInfoExportAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}
