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

#include "sv4gui_ModelExtractPathsAction.h"

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

sv4guiModelExtractPathsAction::sv4guiModelExtractPathsAction()
{
    m_Thread=NULL;
    m_Interface=new sv4guiDataNodeOperationInterface;
}

sv4guiModelExtractPathsAction::~sv4guiModelExtractPathsAction()
{
}

void sv4guiModelExtractPathsAction::UpdateStatus()
{
    std::vector<mitk::DataNode::Pointer> pathNodes=m_Thread->GetPathNodes();
//    for(int i=0;i<pathNodes.size();i++)
//        m_DataStorage->Add(pathNodes[i],m_Thread->GetPathFolderNode());

    mitk::OperationEvent::IncCurrObjectEventId();

    bool undoEnabled=true;
    for(int i=0;i<pathNodes.size();i++)
    {
        sv4guiDataNodeOperation* doOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpADDDATANODE,m_DataStorage,pathNodes[i],m_Thread->GetPathFolderNode());
        if(undoEnabled)
        {
            sv4guiDataNodeOperation* undoOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpREMOVEDATANODE,m_DataStorage,pathNodes[i],m_Thread->GetPathFolderNode());
            mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Interface, doOp, undoOp, "Add DataNode");
            mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
        }
        m_Interface->ExecuteOperation(doOp);
    }

    // Now add model for whole centerlines pd
    mitk::DataNode::Pointer centerlinesModelNode = m_DataStorage->GetNamedDerivedNode("Full_Centerlines", m_Thread->GetSelectedNode());

    if (centerlinesModelNode.IsNull())
      m_DataStorage->Add(m_Thread->GetCenterlinesModelNode(), m_Thread->GetSelectedNode());
    // Added model

    // Now add model for merged centerlines pd
    #ifdef sv4guiModelExtractPathsAction_extract_paths 
    mitk::DataNode::Pointer mergedCenterlinesModelNode = m_DataStorage->GetNamedDerivedNode("Merged_Centerlines", m_Thread->GetSelectedNode());

    if (mergedCenterlinesModelNode.IsNull())
      m_DataStorage->Add(m_Thread->GetMergedCenterlinesModelNode(), m_Thread->GetSelectedNode());
    #endif

    // Added model

    m_ProjFolderNode->SetBoolProperty("thread running",false);
    mitk::StatusBar::GetInstance()->DisplayText(m_Thread->GetStatus().toStdString().c_str());
}

void sv4guiModelExtractPathsAction::SetSourceCapIds(std::vector<int> sourceCapIds)
{
  this->m_SourceCapIds.clear();
  for (int i=0; i<sourceCapIds.size(); i++)
    this->m_SourceCapIds.push_back(sourceCapIds[i]);
}

void sv4guiModelExtractPathsAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{

    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    sv4guiModel* model=dynamic_cast<sv4guiModel*>(selectedNode->GetData());
    if(!model) return;

    sv4guiModelElement* modelElement=model->GetModelElement();
    if(!modelElement) return;

    vtkSmartPointer<vtkPolyData> vpd=modelElement->GetWholeVtkPolyData();
    if(!vpd) return;

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=m_DataStorage->GetSources (selectedNode,isProjFolder,false);

    if(rs->size()==0)
        return;

    m_ProjFolderNode=rs->GetElement(0);

    bool threadRunning=false;
    m_ProjFolderNode->GetBoolProperty("thread running",threadRunning);

    if(threadRunning)
    {
        QMessageBox::warning(NULL,"Project is Busy","A work thread is running in the project!");
        return;
    }

    mitk::StatusBar::GetInstance()->DisplayText("Extracting paths from the model... ( may take several minutes or more)");
    m_Thread=new WorkThread(m_DataStorage,selectedNode,m_SourceCapIds);

    connect(m_Thread, SIGNAL(finished()), this, SLOT(UpdateStatus()));

    m_ProjFolderNode->SetBoolProperty("thread running",true);

    m_Thread->start();
}

void sv4guiModelExtractPathsAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

sv4guiModelExtractPathsAction::WorkThread::WorkThread(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer selectedNode, std::vector<int> sourceCapIds)
{
    this->mm_DataStorage=dataStorage;
    this->m_SelectedNode=selectedNode;
    this->mm_SourceCapIds.clear();
    for (int i=0; i<sourceCapIds.size(); i++)
      this->mm_SourceCapIds.push_back(sourceCapIds[i]);
}

void sv4guiModelExtractPathsAction::WorkThread::run()
{
    std::cout << "========== sv4guiModelExtractPathsAction::WorkThread::run ==========" << std::endl;

    m_PathNodes.clear();
    m_Status="No valid data!";

    mitk::DataNode::Pointer selectedNode = m_SelectedNode;

    sv4guiModel* model=dynamic_cast<sv4guiModel*>(selectedNode->GetData());
    if(!model)
        return;

    sv4guiModelElement* modelElement=model->GetModelElement();
    if(!modelElement) return;

    vtkSmartPointer<vtkPolyData> vpd=modelElement->GetWholeVtkPolyData();
    if(!vpd) return;

    try
    {
        vtkSmartPointer<vtkIdList> sourceCapIds = vtkSmartPointer<vtkIdList>::New();
        for (int i=0; i<this->mm_SourceCapIds.size(); i++)
          sourceCapIds->InsertNextId(this->mm_SourceCapIds[i]);

        vtkSmartPointer<vtkPolyData> centerlinesPd = sv4guiModelUtils::CreateCenterlines(modelElement, sourceCapIds);

        #ifdef sv4guiModelExtractPathsAction_extract_paths 

        vtkSmartPointer<vtkPolyData> mergedCenterlinesPD = sv4guiModelUtils::MergeCenterlines(centerlinesPd);

        std::vector<sv4guiPathElement*> pathElements=sv4guiModelUtils::CreatePathElements(modelElement, mergedCenterlinesPD);

        if(pathElements.size()==0)
            return;

        mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
        mitk::DataStorage::SetOfObjects::ConstPointer rs=mm_DataStorage->GetSources (selectedNode,isProjFolder,false);

        if(rs->size()==0)
            return;

        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        rs=mm_DataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiPathFolder"));

        if(rs->size()==0)
            return;

        m_PathFolderNode=rs->GetElement(0);

        int maxPathID=sv4guiPath::GetMaxPathID(mm_DataStorage->GetDerivations(m_PathFolderNode));

        for(int i=0;i<pathElements.size();i++)
        {
            sv4guiPath::Pointer path=sv4guiPath::New();
            maxPathID++;
            path->SetPathID(maxPathID);
            path->SetMethod(pathElements[i]->GetMethod());
            path->SetCalculationNumber(pathElements[i]->GetCalculationNumber());
            path->SetPathElement(pathElements[i]);
            path->SetDataModified();

            mitk::DataNode::Pointer pathNode = mitk::DataNode::New();
            pathNode->SetData(path);
            pathNode->SetName(selectedNode->GetName()+"_centerline_"+std::to_string(i+1));

            m_PathNodes.push_back(pathNode);
        }
        #endif

        // Now add model for whole centerlines pd
        std::cout << "[run] Add Full_Centerlines ... " << std::endl;
        m_CenterlinesModelNode = mm_DataStorage->GetNamedDerivedNode("Full_Centerlines", m_SelectedNode);
        mitk::Surface::Pointer centerlinesSurface;

        if (m_CenterlinesModelNode.IsNull())
        {
          centerlinesSurface = mitk::Surface::New();

          m_CenterlinesModelNode = mitk::DataNode::New();
          m_CenterlinesModelNode->SetData(centerlinesSurface);
          m_CenterlinesModelNode->SetName("Full_Centerlines");
        }
        else
          centerlinesSurface = dynamic_cast<mitk::Surface*>(m_CenterlinesModelNode->GetData());
        centerlinesSurface->SetVtkPolyData(centerlinesPd, 0);
        std::cout << "[run] Done. " << std::endl;

        // Added model

        // Add model for merged centerlines pd
        #ifdef sv4guiModelExtractPathsAction_extract_paths 
        m_MergedCenterlinesModelNode = mm_DataStorage->GetNamedDerivedNode("Merged_Centerlines", m_SelectedNode);
        mitk::Surface::Pointer mergedCenterlinesSurface;

        if (m_MergedCenterlinesModelNode.IsNull())
        {
          mergedCenterlinesSurface = mitk::Surface::New();

          m_MergedCenterlinesModelNode = mitk::DataNode::New();
          m_MergedCenterlinesModelNode->SetData(mergedCenterlinesSurface);
          m_MergedCenterlinesModelNode->SetName("Merged_Centerlines");
        }
        else
          mergedCenterlinesSurface = dynamic_cast<mitk::Surface*>(m_MergedCenterlinesModelNode->GetData());
        mergedCenterlinesSurface->SetVtkPolyData(mergedCenterlinesPD, 0);
        #endif

        // Added model

        m_Status="Paths extracting done.";
    }
    catch(...)
    {
        MITK_ERROR << "Model Paths Extracting Error!";
        m_Status="Model Paths Extracting Error!";
    }
}
