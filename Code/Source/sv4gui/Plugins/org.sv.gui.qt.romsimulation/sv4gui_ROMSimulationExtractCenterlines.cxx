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

#include "sv4gui_ROMSimulationExtractCenterlines.h"
#include "sv4gui_ROMSimulationView.h"

#include "sv4gui_ModelLegacyIO.h"
#include "sv4gui_ModelUtils.h"
#include "sv4gui_Model.h"
#include "sv4gui_Path.h"
#include "sv4gui_DataNodeOperation.h"

#include <mitkNodePredicateDataType.h>
#include <mitkStatusBar.h>
#include <mitkSurface.h>
#include <mitkOperationEvent.h>
#include <mitkUndoController.h>

#include <QMessageBox>

#include <vtkXMLPolyDataWriter.h>

//--------------------------------------
// sv4guiROMSimulationExtractCenterlines
//--------------------------------------
//
sv4guiROMSimulationExtractCenterlines::sv4guiROMSimulationExtractCenterlines()
{
    m_Thread = NULL;
    m_Interface = new sv4guiDataNodeOperationInterface;
}

//---------------------------------------
// ~sv4guiROMSimulationExtractCenterlines
//---------------------------------------
//
sv4guiROMSimulationExtractCenterlines::~sv4guiROMSimulationExtractCenterlines()
{
}

//--------------
// UpdateStatus
//--------------
//
void sv4guiROMSimulationExtractCenterlines::UpdateStatus()
{
    auto msg = "[sv4guiROMSimulationExtractCenterlines::UpdateStatus]";
    MITK_INFO << msg;
    MITK_INFO << msg << "---------- UpdateStatus ----------";
    MITK_INFO << msg << "Write centerlines to '" << m_CenterlinesFileName << "'"; 
 
    mitk::OperationEvent::IncCurrObjectEventId();

    // Write the centerline geometry to a file.
    m_CenterlineGeometry = m_Thread->m_CenterlineGeometry; 
    std::string fileName = m_CenterlinesFileName.toStdString();
    vtkSmartPointer<vtkXMLPolyDataWriter> writer = vtkSmartPointer<vtkXMLPolyDataWriter>::New();
    writer->SetFileName(fileName.c_str());
    writer->SetInputData(m_CenterlineGeometry);
    writer->Write();

    // Add model for centerlines.
    mitk::DataNode::Pointer centerlinesModelNode = m_DataStorage->GetNamedDerivedNode("Full_Centerlines", 
      m_Thread->GetSelectedNode());

    if (centerlinesModelNode.IsNull()) {
        if (m_Thread->m_JobNode == nullptr) {
            m_DataStorage->Add(m_Thread->GetCenterlinesModelNode(), m_Thread->m_JobNode);
         }
    }

    m_ProjFolderNode->SetBoolProperty("thread running",false);
    mitk::StatusBar::GetInstance()->DisplayText(m_Thread->GetStatus().toStdString().c_str());

    // Update centerlines information in the view.
    sv4guiROMSimulationView* view = dynamic_cast<sv4guiROMSimulationView*>(m_View);
    view->UpdateCenterlines();
}

//-----------------
// SetSourceCapIds
//-----------------
//
void sv4guiROMSimulationExtractCenterlines::SetSourceCapIds(std::vector<int> sourceCapIds)
{
  this->m_SourceCapIds.clear();

  for (int i=0; i< sourceCapIds.size(); i++) {
      this->m_SourceCapIds.push_back(sourceCapIds[i]);
  }
}

//-----
// Run
//-----
//
void sv4guiROMSimulationExtractCenterlines::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    auto msg = "[sv4guiROMSimulationExtractCenterlines::Run]";
    MITK_INFO << msg;
    MITK_INFO << msg << "---------- Run ----------";
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    sv4guiModel* model = dynamic_cast<sv4guiModel*>(selectedNode->GetData());
    if (!model) {
        return;
    }

    sv4guiModelElement* modelElement = model->GetModelElement();
    if(!modelElement) {
        return;
    }

    vtkSmartPointer<vtkPolyData> vpd = modelElement->GetWholeVtkPolyData();
    if(!vpd) { 
        return;
    }

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs = m_DataStorage->GetSources (selectedNode,isProjFolder,false);

    if(rs->size()==0) {
        return;
    }

    m_ProjFolderNode = rs->GetElement(0);
    bool threadRunning = false;
    m_ProjFolderNode->GetBoolProperty("thread running",threadRunning);

    if(threadRunning) {
        QMessageBox::warning(NULL,"Project is Busy","A work thread is running in the project!");
        return;
    }

    MITK_INFO << msg << "Extracting centerlines"; 
    mitk::StatusBar::GetInstance()->DisplayText("Extracting centerlines from the surface model.");

    m_Thread = new WorkThread(m_DataStorage, selectedNode, m_SourceCapIds);
    m_Thread->m_JobNode = m_JobNode; 
    m_Thread->m_CenterlinesFileName = m_CenterlinesFileName; 

    connect(m_Thread, SIGNAL(finished()), this, SLOT(UpdateStatus()));

    m_ProjFolderNode->SetBoolProperty("thread running",true);
    m_Thread->start();
}

//----------------
// SetDataStorage
//----------------
//
void sv4guiROMSimulationExtractCenterlines::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

//------------------------
// WorkThread::WorkThread
//------------------------
//
sv4guiROMSimulationExtractCenterlines::WorkThread::WorkThread(mitk::DataStorage::Pointer dataStorage, 
    mitk::DataNode::Pointer selectedNode, std::vector<int> sourceCapIds)
{
    this->mm_DataStorage = dataStorage;
    this->m_SelectedNode = selectedNode;
    this->mm_SourceCapIds.clear();
    this->m_JobNode = nullptr;

    for (int i=0; i<sourceCapIds.size(); i++) {
      this->mm_SourceCapIds.push_back(sourceCapIds[i]);
    }
}


vtkSmartPointer<vtkPolyData> sv4guiROMSimulationExtractCenterlines::GetCenterlineGeometry()
{
    return m_CenterlineGeometry;
}

//-----------------
// WorkThread::run
//-----------------
// Execute the worker thread to calculate centerlines.
//
void sv4guiROMSimulationExtractCenterlines::WorkThread::run()
{
    m_PathNodes.clear();
    m_Status = "No valid data!";

    mitk::DataNode::Pointer selectedNode = m_SelectedNode;

    sv4guiModel* model = dynamic_cast<sv4guiModel*>(selectedNode->GetData());

    if (!model) {
        return;
    }

    sv4guiModelElement* modelElement = model->GetModelElement();
    if (!modelElement) {
        return;
    }

    vtkSmartPointer<vtkPolyData> vpd=modelElement->GetWholeVtkPolyData();

    if (!vpd) { 
        return;
    }

    try {
        vtkSmartPointer<vtkIdList> sourceCapIds = vtkSmartPointer<vtkIdList>::New();

        for (int i=0; i<this->mm_SourceCapIds.size(); i++) {
          sourceCapIds->InsertNextId(this->mm_SourceCapIds[i]);
        }

        m_CenterlineGeometry = sv4guiModelUtils::CreateCenterlines(modelElement, sourceCapIds, true);

        // Add Centerlines Data Node.
        //
        m_CenterlinesModelNode = mm_DataStorage->GetNamedDerivedNode("Full_Centerlines", m_SelectedNode);
        mitk::Surface::Pointer centerlinesSurface;

        if (m_CenterlinesModelNode.IsNull()) {
            centerlinesSurface = mitk::Surface::New();
            m_CenterlinesModelNode = mitk::DataNode::New();
            m_CenterlinesModelNode->SetData(centerlinesSurface);
            m_CenterlinesModelNode->SetName("Centerlines");
        } else {
            centerlinesSurface = dynamic_cast<mitk::Surface*>(m_CenterlinesModelNode->GetData());
        }
        centerlinesSurface->SetVtkPolyData(m_CenterlineGeometry, 0);

        m_Status = "Extract centerlines done.";

    } catch(...) {
        MITK_ERROR << "Model Paths Extracting Error!";
        m_Status = "Model Paths Extracting Error!";
    }
}
