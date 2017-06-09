#include "svSegmentationLoadAction.h"

#include "svContourGroupIO.h"
#include "svMitkSeg3D.h"
#include "svMitkSeg3DIO.h"

#include <mitkNodePredicateDataType.h>
#include <berryPlatform.h>
#include <berryIPreferences.h>
#include <berryIPreferencesService.h>

#include <mitkIOUtil.h>

#include <vtkXMLPolyDataReader.h>

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

        QString filePath = QFileDialog::getOpenFileName(NULL, "Import Segmentation (Choose File)", lastFilePath, tr("SimVascular Segmentations (*.ctgr *.s3d);;VTP Files (*.vtp)")
                                                              ,NULL,QFileDialog::DontUseNativeDialog);

        filePath=filePath.trimmed();
        if(filePath.isEmpty())
            return;

        QFileInfo fi(filePath);
        std::string baseName=fi.baseName().toStdString();


        if(filePath.endsWith(".ctgr"))
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
        else if(filePath.endsWith(".s3d"))
        {
            std::vector<mitk::BaseData::Pointer> nodedata=svMitkSeg3DIO::ReadFile(filePath.toStdString());

            if(nodedata.size()>0)
            {
                mitk::BaseData::Pointer seg3Ddata=nodedata[0];
                if(seg3Ddata.IsNotNull())
                {
                    mitk::DataNode::Pointer seg3DNode = mitk::DataNode::New();
                    seg3DNode->SetData(seg3Ddata);
                    seg3DNode->SetName(baseName);

                    m_DataStorage->Add(seg3DNode,selectedNode);
                }
            }
        }
        else if(filePath.endsWith("vtp"))
        {
            vtkSmartPointer<vtkXMLPolyDataReader> reader = vtkSmartPointer<vtkXMLPolyDataReader>::New();
            reader->SetFileName(filePath.toStdString().c_str());
            reader->Update();
            vtkSmartPointer<vtkPolyData> vpd=reader->GetOutput();
            if(vpd)
            {
                svSeg3D* seg3D=new svSeg3D();
                seg3D->SetVtkPolyData(vpd);

                svMitkSeg3D::Pointer mitkSeg3D=svMitkSeg3D::New();
                mitkSeg3D->SetSeg3D(seg3D);

                mitk::DataNode::Pointer seg3DNode = mitk::DataNode::New();
                seg3DNode->SetData(mitkSeg3D);
                seg3DNode->SetName(baseName);

                m_DataStorage->Add(seg3DNode,selectedNode);
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

