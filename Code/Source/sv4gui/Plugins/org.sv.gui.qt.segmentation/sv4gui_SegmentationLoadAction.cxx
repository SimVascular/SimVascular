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

#include "sv4gui_SegmentationLoadAction.h"

#include "sv4gui_ContourGroupIO.h"
#include "sv4gui_MitkSeg3D.h"
#include "sv4gui_MitkSeg3DIO.h"

#include <mitkNodePredicateDataType.h>
#include <berryPlatform.h>
#include <berryIPreferences.h>
#include <berryIPreferencesService.h>

#include <mitkIOUtil.h>

#include <vtkXMLPolyDataReader.h>

#include <QFileDialog>
#include <QMessageBox>
#include <QInputDialog>

sv4guiSegmentationLoadAction::sv4guiSegmentationLoadAction()
{
}

sv4guiSegmentationLoadAction::~sv4guiSegmentationLoadAction()
{
}

void sv4guiSegmentationLoadAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isSegFolder = mitk::NodePredicateDataType::New("sv4guiSegmentationFolder");

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

        QString filePath = QFileDialog::getOpenFileName(NULL, "Import Segmentation (Choose File)", lastFilePath, tr("SimVascular Segmentations (*.ctgr *.s3d);;VTP Files (*.vtp)"));

        filePath=filePath.trimmed();
        if(filePath.isEmpty())
            return;

        // QFileInfo fi(filePath);
        // std::string baseName=fi.baseName().toStdString();
        
        bool ok;
        QString text = QInputDialog::getText(NULL, tr("Segmentation Name"),
                                        tr("Please give a segmentation name:"), QLineEdit::Normal,
                                        "", &ok);
        std::string baseName=text.trimmed().toStdString();
        
        if(baseName==""){
            QMessageBox::warning(NULL,"Segmentation Empty","Please give a segmentation name!");
            return;
        }
    
        mitk::DataNode::Pointer exitingNode=m_DataStorage->GetNamedDerivedNode(baseName.c_str(),selectedNode);
        if(exitingNode){
            QMessageBox::warning(NULL,"Segmentation Already Created","Please use a different name!");
            return;
        }


        if(filePath.endsWith(".ctgr"))
        {
            std::vector<mitk::BaseData::Pointer> nodedata=sv4guiContourGroupIO::ReadFile(filePath.toStdString());

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
            std::vector<mitk::BaseData::Pointer> nodedata=sv4guiMitkSeg3DIO::ReadFile(filePath.toStdString());

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
                sv4guiSeg3D* seg3D=new sv4guiSeg3D();
                seg3D->SetVtkPolyData(vpd);

                sv4guiMitkSeg3D::Pointer mitkSeg3D=sv4guiMitkSeg3D::New();
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

void sv4guiSegmentationLoadAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

