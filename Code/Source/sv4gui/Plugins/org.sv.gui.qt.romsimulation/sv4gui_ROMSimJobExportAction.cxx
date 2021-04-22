/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "sv4gui_ROMSimJobExportAction.h"

#include <mitkNodePredicateDataType.h>

#include "sv4gui_ROMSimulationView.h"

#include <berryIPreferencesService.h>
#include <berryIPreferences.h>
#include <berryPlatform.h>

#include <QFileDialog>

sv4guiROMSimJobExportAction::sv4guiROMSimJobExportAction()
    : m_Functionality(NULL)
{
}

sv4guiROMSimJobExportAction::~sv4guiROMSimJobExportAction()
{
}

void sv4guiROMSimJobExportAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isMitkSimJob = mitk::NodePredicateDataType::New("sv4guiMitkSimJob");

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

        QString lastFileSavePath="";
        if(prefs.IsNotNull())
        {
            lastFileSavePath = prefs->Get("LastFileSavePath", "");
        }
        if(lastFileSavePath=="")
            lastFileSavePath=QDir::homePath();

        QString dir = QFileDialog::getExistingDirectory(NULL
                                                        , tr("Export Simulation Data Files (Choose Directory)")
                                                        , lastFileSavePath);

        dir=dir.trimmed();
        if(dir.isEmpty()) return;
        if(prefs.IsNotNull())
         {
             prefs->Put("LastFileSavePath", dir);
             prefs->Flush();
         }

        mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
        mitk::DataStorage::SetOfObjects::ConstPointer rs=m_DataStorage->GetSources (selectedNode,isProjFolder,false);

        std::string projPath="";
        std::string simFolderName="";
        std::string jobName=selectedNode->GetName();

        if(rs->size()>0)
        {
            mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);
            projFolderNode->GetStringProperty("project path", projPath);

            rs=m_DataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiROMSimulationFolder"));
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
        fileList<<"bct.dat"<<"bct.vtp"<<"rcrt.dat"<<"cort.dat";
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
        }

    }
    catch(...)
    {
        MITK_ERROR << "Error during stopping job!";
    }
}


void sv4guiROMSimJobExportAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

void sv4guiROMSimJobExportAction::SetFunctionality(berry::QtViewPart *functionality)
{
    m_Functionality=functionality;
}

