#include "svModelExtractPathsAction.h"

#include "svModelLegacyIO.h"
#include "svModelUtils.h"
#include "svModel.h"
#include "svPath.h"

#include <mitkNodePredicateDataType.h>
#include <mitkStatusBar.h>

#include <QMessageBox>

svModelExtractPathsAction::svModelExtractPathsAction()
{
    m_Thread=NULL;
}

svModelExtractPathsAction::~svModelExtractPathsAction()
{
}

void svModelExtractPathsAction::UpdateStatus()
{
    std::vector<mitk::DataNode::Pointer> pathNodes=m_Thread->GetPathNodes();
    for(int i=0;i<pathNodes.size();i++)
        m_DataStorage->Add(pathNodes[i],m_Thread->GetPathFolderNode());

    m_ProjFolderNode->SetBoolProperty("thread running",false);
    mitk::StatusBar::GetInstance()->DisplayText(m_Thread->GetStatus().toStdString().c_str());
}

void svModelExtractPathsAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{

    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

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
    m_Thread=new WorkThread(m_DataStorage,selectedNode);

    connect(m_Thread, SIGNAL(finished()), this, SLOT(UpdateStatus()));

    m_ProjFolderNode->SetBoolProperty("thread running",true);

    m_Thread->start();
}

void svModelExtractPathsAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

svModelExtractPathsAction::WorkThread::WorkThread(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer selectedNode)
{
    mm_DataStorage=dataStorage;
    m_SelectedNode=selectedNode;
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
        std::vector<svPathElement*> pathElements=svModelUtils::CreatePathElements(modelElement);

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

        m_Status="Paths extracting done.";
    }
    catch(...)
    {
        MITK_ERROR << "Model Paths Extracting Error!";
        m_Status="Model Paths Extracting Error!";
    }
}
