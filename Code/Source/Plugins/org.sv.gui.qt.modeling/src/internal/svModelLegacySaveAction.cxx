#include "svModelLegacySaveAction.h"

#include "svModel.h"
#include "svModelLegacyIO.h"
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

    QString fileFilter;
    std::string modelType=model->GetType();
    if(modelType=="PolyData")
        fileFilter=tr("VTK PolyData (*.vtp)");
    else if(modelType=="OpenCASCADE")
//        fileFilter=tr("OpenCASCADE (*.brep); STEP (*.step); STL (*.stl); IGES (*.iges)");
        fileFilter=tr("OpenCASCADE (*.brep *.step *.stl *.iges)");
    else if(modelType=="Parasolid")
        fileFilter=tr("Parasolid (*.xmt_txt)");

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

        QString lastFileSavePath=QString();
        if(prefs.IsNotNull())
        {
            lastFileSavePath = prefs->Get("LastFileSavePath", "");
        }

        QString fileName = QFileDialog::getSaveFileName(NULL
                                                        ,tr("Save Model")
                                                        ,lastFileSavePath
                                                        ,fileFilter
                                                        ,NULL
                                                        ,QFileDialog::DontUseNativeDialog
                                                        );
        if(fileName.trimmed().isEmpty()) return;

        if(modelType=="PolyData")
        {
            if(!fileName.endsWith(".vtp"))
                fileName=fileName+".vtp";
        }
        else if(modelType=="OpenCASCADE")
        {
            if(!fileName.endsWith(".brep") && !fileName.endsWith(".step") && !fileName.endsWith(".stl") && !fileName.endsWith(".iges"))
                fileName=fileName+".brep";
        }
        else if(modelType=="Parasolid")
        {
            if(!fileName.endsWith(".xmt_txt"))
                fileName=fileName+".xmt_txt";
        }
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
