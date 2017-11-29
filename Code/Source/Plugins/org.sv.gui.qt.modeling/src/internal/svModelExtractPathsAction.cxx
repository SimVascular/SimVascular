#include "svModelExtractPathsAction.h"

#include "svModelLegacyIO.h"
#include "svModelUtils.h"
#include "svModel.h"
#include "svPath.h"
#include "svDataNodeOperation.h"

#include <mitkNodePredicateDataType.h>
#include <mitkStatusBar.h>
#include <mitkSurface.h>
#include <mitkOperationEvent.h>
#include <mitkUndoController.h>

#include <QMessageBox>

svModelExtractPathsAction::svModelExtractPathsAction()
{
    m_Thread=NULL;
    m_Interface=new svDataNodeOperationInterface;
}

svModelExtractPathsAction::~svModelExtractPathsAction()
{
}

void svModelExtractPathsAction::UpdateStatus()
{
    std::vector<mitk::DataNode::Pointer> pathNodes=m_Thread->GetPathNodes();
//    for(int i=0;i<pathNodes.size();i++)
//        m_DataStorage->Add(pathNodes[i],m_Thread->GetPathFolderNode());

    mitk::OperationEvent::IncCurrObjectEventId();

    bool undoEnabled=true;
    for(int i=0;i<pathNodes.size();i++)
    {
        svDataNodeOperation* doOp = new svDataNodeOperation(svDataNodeOperation::OpADDDATANODE,m_DataStorage,pathNodes[i],m_Thread->GetPathFolderNode());
        if(undoEnabled)
        {
            svDataNodeOperation* undoOp = new svDataNodeOperation(svDataNodeOperation::OpREMOVEDATANODE,m_DataStorage,pathNodes[i],m_Thread->GetPathFolderNode());
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
    mitk::DataNode::Pointer mergedCenterlinesModelNode = m_DataStorage->GetNamedDerivedNode("Merged_Centerlines", m_Thread->GetSelectedNode());

    if (mergedCenterlinesModelNode.IsNull())
      m_DataStorage->Add(m_Thread->GetMergedCenterlinesModelNode(), m_Thread->GetSelectedNode());
    // Added model

    m_ProjFolderNode->SetBoolProperty("thread running",false);
    mitk::StatusBar::GetInstance()->DisplayText(m_Thread->GetStatus().toStdString().c_str());
}

void svModelExtractPathsAction::SetSourceCapIds(std::vector<int> sourceCapIds)
{
  this->m_SourceCapIds.clear();
  for (int i=0; i<sourceCapIds.size(); i++)
    this->m_SourceCapIds.push_back(sourceCapIds[i]);
}

void svModelExtractPathsAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{

    mitk::DataNode::Pointer selectedNode = selectedNodes[0];
    fprintf(stdout,"AFT RUN NAME: %s\n", selectedNode->GetName().c_str());

    svModel* model=dynamic_cast<svModel*>(selectedNode->GetData());
    if(!model) return;

    svModelElement* modelElement=model->GetModelElement();
    if(!modelElement) return;

    vtkSmartPointer<vtkPolyData> vpd=modelElement->GetWholeVtkPolyData();
    if(!vpd) return;

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
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

void svModelExtractPathsAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

svModelExtractPathsAction::WorkThread::WorkThread(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer selectedNode, std::vector<int> sourceCapIds)
{
    this->mm_DataStorage=dataStorage;
    this->m_SelectedNode=selectedNode;
    this->mm_SourceCapIds.clear();
    for (int i=0; i<sourceCapIds.size(); i++)
      this->mm_SourceCapIds.push_back(sourceCapIds[i]);
}

void svModelExtractPathsAction::WorkThread::run()
{
    m_PathNodes.clear();
    m_Status="No valid data!";

    mitk::DataNode::Pointer selectedNode = m_SelectedNode;

    svModel* model=dynamic_cast<svModel*>(selectedNode->GetData());
    if(!model)
        return;

    svModelElement* modelElement=model->GetModelElement();
    if(!modelElement) return;

    vtkSmartPointer<vtkPolyData> vpd=modelElement->GetWholeVtkPolyData();
    if(!vpd) return;

    try
    {
        vtkSmartPointer<vtkIdList> sourceCapIds = vtkSmartPointer<vtkIdList>::New();
        for (int i=0; i<this->mm_SourceCapIds.size(); i++)
          sourceCapIds->InsertNextId(this->mm_SourceCapIds[i]);

        vtkSmartPointer<vtkPolyData> centerlinesPd = svModelUtils::CreateCenterlines(modelElement, sourceCapIds);
        vtkSmartPointer<vtkPolyData> mergedCenterlinesPD = svModelUtils::MergeCenterlines(centerlinesPd);

        std::vector<svPathElement*> pathElements=svModelUtils::CreatePathElements(modelElement, mergedCenterlinesPD);

        if(pathElements.size()==0)
            return;

        mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
        mitk::DataStorage::SetOfObjects::ConstPointer rs=mm_DataStorage->GetSources (selectedNode,isProjFolder,false);

        if(rs->size()==0)
            return;

        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        rs=mm_DataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svPathFolder"));

        if(rs->size()==0)
            return;

        m_PathFolderNode=rs->GetElement(0);

        int maxPathID=svPath::GetMaxPathID(mm_DataStorage->GetDerivations(m_PathFolderNode));

        for(int i=0;i<pathElements.size();i++)
        {
            svPath::Pointer path=svPath::New();
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

        // Now add model for whole centerlines pd
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
        // Added model

        // Add model for merged centerlines pd
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
        // Added model

        m_Status="Paths extracting done.";
    }
    catch(...)
    {
        MITK_ERROR << "Model Paths Extracting Error!";
        m_Status="Model Paths Extracting Error!";
    }
}
