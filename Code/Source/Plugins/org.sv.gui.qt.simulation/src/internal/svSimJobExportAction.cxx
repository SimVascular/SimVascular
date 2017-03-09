#include "svSimJobExportAction.h"

#include <mitkNodePredicateDataType.h>

#include "svSimulationView.h"

#include <berryIPreferencesService.h>
#include <berryIPreferences.h>
#include <berryPlatform.h>

#include <QFileDialog>

svSimJobExportAction::svSimJobExportAction()
    : m_Functionality(NULL)
{
}

svSimJobExportAction::~svSimJobExportAction()
{
}

void svSimJobExportAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isMitkSimJob = mitk::NodePredicateDataType::New("svMitkSimJob");

    if(selectedNode.IsNull() || !isMitkSimJob->CheckNode(selectedNode))
    {
        return;
    }

    try
    {
//        if(!m_Functionality)
//            return;

//        QmitkDataManagerView* dmView=dynamic_cast<QmitkDataManagerView*>(m_Functionality);

//        if(!dmView)
//            return;

//        mitk::IRenderWindowPart* renderWindowPart = dmView->GetRenderWindowPart();

//        if(!renderWindowPart)
//            return;

//        mitk::SliceNavigationController* timeNavigationController=renderWindowPart->GetTimeNavigationController();
//        int timeStep=0;
//        if(timeNavigationController)
//        {
//            timeStep=timeNavigationController->GetTime()->GetPos();
//        }

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

        QString dir = QFileDialog::getExistingDirectory(NULL
                                                        , tr("Choose Directory")
                                                        , lastFileSavePath
                                                        , QFileDialog::DontResolveSymlinks
                                                        | QFileDialog::DontUseNativeDialog
                                                        );

        if(dir.isEmpty()) return;


        mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
        mitk::DataStorage::SetOfObjects::ConstPointer rs=m_DataStorage->GetSources (selectedNode,isProjFolder,false);

        std::string projPath="";
        std::string simFolderName="";
        std::string jobName=selectedNode->GetName();

        if(rs->size()>0)
        {
            mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);
            projFolderNode->GetStringProperty("project path", projPath);

            rs=m_DataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svSimulationFolder"));
            if (rs->size()>0)
            {
                mitk::DataNode::Pointer simFolderNode=rs->GetElement(0);
                simFolderName=simFolderNode->GetName();
            }
        }

        if(projPath=="" || simFolderName=="")
        {
            QMessageBox::warning(NULL,"Warning","No project or simualtion folder are found.");
            return;
        }

        QString jobPath=QString::fromStdString(projPath+"/"+simFolderName+"/"+selectedNode->GetName());
        if(!QDir(jobPath).exists())
        {
            QMessageBox::warning(NULL,"Warning","Make sure data files have been created for the job.");
            return;
        }

        QStringList fileList, fileListRequired;
        fileList<<"bct.dat"<<"bct.vtp";
        fileListRequired<<"geombc.dat.1"<<"restart.0.1"<<"solver.inp";
        QString exportDir=dir+"/"+QString::fromStdString(jobName)+"-sim-files";
        QDir().mkpath(exportDir);

        for(int i=0;i<fileListRequired.size();i++)
        {
            QString filePath=jobPath+"/"+fileListRequired[i];
            if(!QFile(filePath).exists())
            {
                QMessageBox::warning(NULL,"Missing File","Missing: "+ filePath);
                return;
            }

            QString filePath2=exportDir+"/"+fileListRequired[i];
            QFile::copy(filePath,filePath2);
        }

        for(int i=0;i<fileList.size();i++)
        {
            QString filePath=jobPath+"/"+fileList[i];
            if(!QFile(filePath).exists())
                continue;

            QString filePath2=exportDir+"/"+fileList[i];
            QFile::copy(filePath,filePath2);
        }

        QFile numStartFile(exportDir+"/numstart.dat");
        if(numStartFile.open(QIODevice::WriteOnly | QIODevice::Text))
        {
            QTextStream out(&numStartFile);
            out<<QString::fromStdString("0\n");
            numStartFile.close();
        }
        else
        {
            QMessageBox::warning(NULL,"Missing File","Failed to create numstart.dat");
            return;
        }

    }
    catch(...)
    {
        MITK_ERROR << "Error during stopping job!";
    }
}


void svSimJobExportAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

void svSimJobExportAction::SetFunctionality(berry::QtViewPart *functionality)
{
    m_Functionality=functionality;
}

