#include "svProjectDataNodesPluginActivator.h"
#include "svPathObjectFactory.h"
#include "svSegmentationObjectFactory.h"
#include "svModelObjectFactory.h"
#include "svMitkMeshObjectFactory.h"
#include "svMitkSimulationObjectFactory.h"
#include "svDataFolder.h"
#include "svDataNodeOperation.h"
#include "svProjectManager.h"
#include "svPath.h"
#include "svContourGroup.h"
#include "svModel.h"
#include "svMitkMesh.h"
#include "svMitkSimJob.h"

#include <mitkOperationEvent.h>
#include <mitkUndoController.h>
#include <QmitkNodeDescriptorManager.h>
#include <mitkNodePredicateDataType.h>
#include <mitkIDataStorageService.h>
#include <mitkDataNodeSelection.h>

#include <berryPlatformUI.h>
#include <berryISelectionService.h>
#include <berryIWorkbenchPage.h>
#include <berryIWorkbench.h>
//#include <QmitkDataManagerView.h>

#include <QMessageBox>
#include <QInputDialog>
#include <QLineEdit>
#include <QTreeView>

static RegistersvPathObjectFactory registersvPathObjectFactory;
static RegistersvSegmentationObjectFactory registersvSegmentationObjectFactory;
static RegistersvModelObjectFactory registersvModelObjectFactory;
static RegistersvMitkMeshObjectFactory registersvMitkMeshObjectFactory;
static RegistersvMitkSimulationObjectFactory registersvMitkSimulationObjectFactory;

ctkPluginContext* svProjectDataNodesPluginActivator::m_Context = nullptr;

svProjectDataNodesPluginActivator::svProjectDataNodesPluginActivator()
{
    m_UndoEnabled=true;
    m_Interface=new svDataNodeOperationInterface;
    m_CopyDataNode=NULL;
}

svProjectDataNodesPluginActivator::~svProjectDataNodesPluginActivator()
{
    //for (std::vector< std::pair< QmitkNodeDescriptor*, QAction* > >::iterator it = m_DescriptorActionList.begin();it != m_DescriptorActionList.end(); it++)
    //{
    //    (it->first)->RemoveAction(it->second);
    //}
}

void svProjectDataNodesPluginActivator::start(ctkPluginContext* context)
{
    this->m_Context = context;

    QmitkNodeDescriptorManager* descriptorManager = QmitkNodeDescriptorManager::GetInstance();

    mitk::NodePredicateDataType::Pointer isProjectFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("svProjectFolder"), QString(":svprojectfolder.png"), isProjectFolder, descriptorManager));

    mitk::NodePredicateDataType::Pointer isImageFolder = mitk::NodePredicateDataType::New("svImageFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("svImageFolder"), QString(":svimagefolder.png"), isImageFolder, descriptorManager));

    mitk::NodePredicateDataType::Pointer isPathFolder = mitk::NodePredicateDataType::New("svPathFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("svPathFolder"), QString(":svpathfolder.png"), isPathFolder, descriptorManager));

    mitk::NodePredicateDataType::Pointer isPath = mitk::NodePredicateDataType::New("svPath");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("svPath"), QString(":svpath.png"), isPath, descriptorManager));

    mitk::NodePredicateDataType::Pointer isSegmentationFolder = mitk::NodePredicateDataType::New("svSegmentationFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("svSegmentationFolder"), QString(":svsegfolder.png"), isSegmentationFolder, descriptorManager));

    mitk::NodePredicateDataType::Pointer isContourGroup = mitk::NodePredicateDataType::New("svContourGroup");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("svContourGroup"), QString(":svcontourgroup.png"), isContourGroup, descriptorManager));

    mitk::NodePredicateDataType::Pointer isModelFolder = mitk::NodePredicateDataType::New("svModelFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("svModelFolder"), QString(":svmodelfolder.png"), isModelFolder, descriptorManager));

    mitk::NodePredicateDataType::Pointer isModel = mitk::NodePredicateDataType::New("svModel");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("svModel"), QString(":svmodel.png"), isModel, descriptorManager));

    mitk::NodePredicateDataType::Pointer isMeshFolder = mitk::NodePredicateDataType::New("svMeshFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("svMeshFolder"), QString(":svmeshfolder.png"), isMeshFolder, descriptorManager));

    mitk::NodePredicateDataType::Pointer isMesh = mitk::NodePredicateDataType::New("svMitkMesh");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("svMitkMesh"), QString(":svmitkmesh.png"), isMesh, descriptorManager));

    mitk::NodePredicateDataType::Pointer isSimulationFolder = mitk::NodePredicateDataType::New("svSimulationFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("svSimulationFolder"), QString(":svsimfolder.png"), isSimulationFolder, descriptorManager));

    mitk::NodePredicateDataType::Pointer isSimJob = mitk::NodePredicateDataType::New("svMitkSimJob");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("svMitkSimJob"), QString(":svsimjob.png"), isSimJob, descriptorManager));

    QmitkNodeDescriptor* unknownDataNodeDescriptor =  QmitkNodeDescriptorManager::GetInstance()->GetUnknownDataNodeDescriptor();
    QAction* removeAction = new QAction(QIcon(":remove.png"), "Remove", this);
    QObject::connect( removeAction, SIGNAL( triggered(bool) ) , this, SLOT( RemoveSelectedNodes(bool) ) );
    unknownDataNodeDescriptor->AddAction(removeAction);
    m_DescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(unknownDataNodeDescriptor,removeAction));

    QAction* renameAction = new QAction(QIcon(":rename.png"), "Rename", this);
    QObject::connect( renameAction, SIGNAL( triggered(bool) ) , this, SLOT( RenameSelectedNode(bool) ) );
    unknownDataNodeDescriptor->AddAction(renameAction,false);
    m_DescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(unknownDataNodeDescriptor,renameAction));

    QAction* copyAction = new QAction(QIcon(":copy.png"), "Copy", this);
    QObject::connect( copyAction, SIGNAL( triggered(bool) ) , this, SLOT( CopyDataNode(bool) ) );
    unknownDataNodeDescriptor->AddAction(copyAction,false);
    m_DescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(unknownDataNodeDescriptor,copyAction));

    QAction* pasteAction = new QAction(QIcon(":paste.png"), "Paste", this);
    QObject::connect( pasteAction, SIGNAL( triggered(bool) ) , this, SLOT( PasteDataNode(bool) ) );
    unknownDataNodeDescriptor->AddAction(pasteAction,false);
    m_DescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(unknownDataNodeDescriptor,pasteAction));


//    SetupDataManagerDoubleClick();
}

void svProjectDataNodesPluginActivator::stop(ctkPluginContext* context)
{
}

mitk::DataStorage::Pointer svProjectDataNodesPluginActivator::GetDataStorage()
{
    mitk::IDataStorageReference::Pointer dsRef;

    mitk::IDataStorageService* dss = nullptr;
    ctkServiceReference dsServiceRef = m_Context->getServiceReference<mitk::IDataStorageService>();
    if (dsServiceRef)
    {
        dss = m_Context->getService<mitk::IDataStorageService>(dsServiceRef);
    }

    if (!dss)
    {
        QString msg = "IDataStorageService service not available. Unable to save sv projects.";
        MITK_WARN << msg.toStdString();
        return NULL;
    }

    // Get the active data storage (or the default one, if none is active)
    dsRef = dss->GetDataStorage();
    m_Context->ungetService(dsServiceRef);

    if(dsRef.IsNull())
        return NULL;

    return dsRef->GetDataStorage();
}

std::list< mitk::DataNode::Pointer > svProjectDataNodesPluginActivator::GetSelectedDataNodes()
{
    std::list< mitk::DataNode::Pointer > selectedList;

    berry::IWorkbenchWindow::Pointer window=berry::PlatformUI::GetWorkbench()->GetActiveWorkbenchWindow();

    if(window.IsNull())
        return selectedList;

    berry::ISelectionService* selectionService =window->GetSelectionService();
    if(selectionService==NULL)
        return selectedList;

    mitk::DataNodeSelection::ConstPointer nodeSelection = selectionService->GetSelection().Cast<const mitk::DataNodeSelection>();
    if(nodeSelection.IsNull())
        return selectedList;

    return nodeSelection->GetSelectedDataNodes();
}

void svProjectDataNodesPluginActivator::RemoveSelectedNodes( bool )
{
    mitk::DataStorage::Pointer dataStorage=GetDataStorage();
    if(dataStorage.IsNull())
        return;

    std::list< mitk::DataNode::Pointer > list=GetSelectedDataNodes();
    if(list.size()==0)
        return;

    QList<mitk::DataNode::Pointer> nodes=QList<mitk::DataNode::Pointer>::fromStdList(list);

    if(nodes.size() < 1)
    {
        return;
    }

    mitk::DataNode::Pointer node = 0;

    std::vector<mitk::DataNode::Pointer> selectedNodes;

    QString question = tr("Do you really want to remove ");

    mitk::TNodePredicateDataType<svDataFolder>::Pointer isDataFolder= mitk::TNodePredicateDataType<svDataFolder>::New();

    for (int i=0;i<nodes.size();i++)
    {
        node = nodes[i];
        if ( node.IsNotNull() && !isDataFolder->CheckNode(node) )
        {
            selectedNodes.push_back(node);
            question.append(QString::fromStdString(node->GetName()));
            question.append(", ");
        }
    }
    // remove the last two characters = ", "
    question = question.remove(question.size()-2, 2);
    question.append(" from data storage?");

    if(selectedNodes.size()==0)
    {
        return;
    }

    QMessageBox::StandardButton answerButton = QMessageBox::question( NULL
                                                                      , tr("DataManager")
                                                                      , question
                                                                      , QMessageBox::Yes | QMessageBox::No, QMessageBox::Yes);

    if(answerButton == QMessageBox::Yes)
    {

        bool incCurrEventId=false;
        for (std::vector<mitk::DataNode::Pointer>::iterator it = selectedNodes.begin()
             ; it != selectedNodes.end(); it++)
        {
            node = *it;
            if( !isDataFolder->CheckNode(node))
            {
//                dataStorage->Remove(node);
                if(!incCurrEventId&&m_UndoEnabled)
                {
                    mitk::OperationEvent::IncCurrObjectEventId();
                    incCurrEventId=true;
                }

                mitk::DataNode::Pointer parentNode=NULL;
                mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSources (node);
                if(rs.IsNotNull()&&rs->size()>0)
                    parentNode=rs->GetElement(0);

                svDataNodeOperation* doOp = new svDataNodeOperation(svDataNodeOperation::OpREMOVEDATANODE,dataStorage,node,parentNode);
                if(m_UndoEnabled)
                {
                    svDataNodeOperation* undoOp = new svDataNodeOperation(svDataNodeOperation::OpADDDATANODE,dataStorage,node,parentNode);
                    mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Interface, doOp, undoOp, "Remove DataNode");
                    mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
                }

                m_Interface->ExecuteOperation(doOp);
            }
            //      if (m_GlobalReinitOnNodeDelete)
            //          this->GlobalReinit(false);
        }
    }
}

void svProjectDataNodesPluginActivator::RenameSelectedNode( bool )
{
    mitk::DataStorage::Pointer dataStorage=GetDataStorage();
    if(dataStorage.IsNull())
        return;

    std::list< mitk::DataNode::Pointer > list=GetSelectedDataNodes();
    if(list.size()==0)
        return;

    QList<mitk::DataNode::Pointer> nodes=QList<mitk::DataNode::Pointer>::fromStdList(list);

    if(nodes.size() < 1)
    {
        return;
    }

    mitk::DataNode::Pointer node = nodes.front();

    if ( node.IsNotNull())
    {
        mitk::TNodePredicateDataType<svDataFolder>::Pointer isDataFolder= mitk::TNodePredicateDataType<svDataFolder>::New();
        if( isDataFolder->CheckNode(node))
        {
            return;
        }

        bool ok;
        QString text = QInputDialog::getText(NULL, tr("Rename"),
                                             tr("New Name:"), QLineEdit::Normal,
                                             QString::fromStdString(node->GetName()), &ok);
        QString newName=text.trimmed();
        if (ok && !newName.isEmpty())
        {
            mitk::DataNode::Pointer parentNode=NULL;
            mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSources(node);
            if(rs.IsNotNull()&&rs->size()>0)
                parentNode=rs->GetElement(0);


            bool alreadyExists=false;
            if(parentNode.IsNull() && dataStorage->GetNamedNode(newName.toStdString()))
                alreadyExists=true;
            else if(parentNode.IsNotNull() && dataStorage->GetNamedDerivedNode(newName.toStdString().c_str(),parentNode))
                alreadyExists=true;

            if(alreadyExists)
            {
                QMessageBox::warning(NULL,"Name Conflict","Please use a name different from other existing nodes under the parent node.");
                return;
            }

            svProjectManager::RenameDataNode(dataStorage,node,newName.toStdString());

        }
    }
}

void svProjectDataNodesPluginActivator::CopyDataNode( bool )
{
    mitk::DataStorage::Pointer dataStorage=GetDataStorage();
    if(dataStorage.IsNull())
        return;

    std::list< mitk::DataNode::Pointer > list=GetSelectedDataNodes();
    if(list.size()==0)
        return;

    QList<mitk::DataNode::Pointer> nodes=QList<mitk::DataNode::Pointer>::fromStdList(list);

    if(nodes.size() < 1)
    {
        return;
    }

    mitk::DataNode::Pointer node = nodes.front();

    m_CopyDataNode=node;
}

void svProjectDataNodesPluginActivator::PasteDataNode( bool )
{
    if(m_CopyDataNode.IsNull())
        return;

    mitk::DataStorage::Pointer dataStorage=GetDataStorage();
    if(dataStorage.IsNull())
        return;

    std::list< mitk::DataNode::Pointer > list=GetSelectedDataNodes();
    if(list.size()==0)
        return;

    QList<mitk::DataNode::Pointer> nodes=QList<mitk::DataNode::Pointer>::fromStdList(list);

    if(nodes.size() < 1)
    {
        return;
    }

    mitk::DataNode::Pointer node = nodes.front();

    mitk::NodePredicateDataType::Pointer isPathFolder = mitk::NodePredicateDataType::New("svPathFolder");
    mitk::NodePredicateDataType::Pointer isSegFolder = mitk::NodePredicateDataType::New("svSegmentationFolder");
    mitk::NodePredicateDataType::Pointer isModelFolder = mitk::NodePredicateDataType::New("svModelFolder");
    mitk::NodePredicateDataType::Pointer isMeshFolder = mitk::NodePredicateDataType::New("svMeshFolder");
    mitk::NodePredicateDataType::Pointer isSimFolder = mitk::NodePredicateDataType::New("svSimulationFolder");

    mitk::NodePredicateDataType::Pointer isPath = mitk::NodePredicateDataType::New("svPath");
    mitk::NodePredicateDataType::Pointer isContourGroup = mitk::NodePredicateDataType::New("svContourGroup");
    mitk::NodePredicateDataType::Pointer isModel = mitk::NodePredicateDataType::New("svModel");
    mitk::NodePredicateDataType::Pointer isMesh = mitk::NodePredicateDataType::New("svMitkMesh");
    mitk::NodePredicateDataType::Pointer isSimJob = mitk::NodePredicateDataType::New("svMitkSimJob");

    mitk::DataNode::Pointer parentNode=NULL;

    if(isPath->CheckNode(m_CopyDataNode))
    {
        if(isPathFolder->CheckNode(node))
            parentNode=node;
        else if(isPath->CheckNode(node))
        {
            mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSources(node);
            if(rs->size()>0)
                parentNode=rs->GetElement(0);
        }
        else
            return;
    }
    else if(isContourGroup->CheckNode(m_CopyDataNode))
    {
        if(isSegFolder->CheckNode(node))
            parentNode=node;
        else if(isContourGroup->CheckNode(node))
        {
            mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSources(node);
            if(rs->size()>0)
                parentNode=rs->GetElement(0);
        }
        else
            return;
    }
    else if(isModel->CheckNode(m_CopyDataNode))
    {
        if(isModelFolder->CheckNode(node))
            parentNode=node;
        else if(isModel->CheckNode(node))
        {
            mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSources(node);
            if(rs->size()>0)
                parentNode=rs->GetElement(0);
        }
        else
            return;
    }
    else if(isMesh->CheckNode(m_CopyDataNode))
    {
        if(isMeshFolder->CheckNode(node))
            parentNode=node;
        else if(isMesh->CheckNode(node))
        {
            mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSources(node);
            if(rs->size()>0)
                parentNode=rs->GetElement(0);
        }
        else
            return;
    }
    else if(isSimJob->CheckNode(m_CopyDataNode))
    {
        if(isSimFolder->CheckNode(node))
            parentNode=node;
        else if(isSimJob->CheckNode(node))
        {
            mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSources(node);
            if(rs->size()>0)
                parentNode=rs->GetElement(0);
        }
        else
            return;
    }
    else
    {
        return;
    }

    mitk::DataNode::Pointer newNode = mitk::DataNode::New();

    svPath* path=dynamic_cast<svPath*>(m_CopyDataNode->GetData());
    if(path)
    {
        newNode->SetData(path->Clone());
    }

    svContourGroup* group=dynamic_cast<svContourGroup*>(m_CopyDataNode->GetData());
    if(group)
    {
        newNode->SetData(group->Clone());
    }

    svModel* model=dynamic_cast<svModel*>(m_CopyDataNode->GetData());
    if(model)
    {
        newNode->SetData(model->Clone());
    }

    svMitkMesh* mesh=dynamic_cast<svMitkMesh*>(m_CopyDataNode->GetData());
    if(mesh)
    {
        newNode->SetData(mesh->Clone());
    }

    svMitkSimJob* simJob=dynamic_cast<svMitkSimJob*>(m_CopyDataNode->GetData());
    if(simJob)
    {
        svMitkSimJob::Pointer copyJob=simJob->Clone();
        copyJob->SetStatus("No Data Files");
        newNode->SetData(copyJob);
    }

    std::string copyName=m_CopyDataNode->GetName()+"_copy";
    int i=1;
    while(i<10)
    {
        if(parentNode.IsNull() && !dataStorage->GetNamedNode(copyName))
            break;
        else if(parentNode.IsNotNull() && !dataStorage->GetNamedDerivedNode(copyName.c_str(),parentNode))
            break;

        i++;
        copyName=m_CopyDataNode->GetName()+"_copy"+std::to_string(i);
    }

    newNode->SetName(copyName);

    mitk::OperationEvent::IncCurrObjectEventId();

    svDataNodeOperation* doOp = new svDataNodeOperation(svDataNodeOperation::OpADDDATANODE,dataStorage,newNode,parentNode);
    if(m_UndoEnabled)
    {
        svDataNodeOperation* undoOp = new svDataNodeOperation(svDataNodeOperation::OpREMOVEDATANODE,dataStorage,newNode,parentNode);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Interface, doOp, undoOp, "Paste DataNode");
        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
    }
    m_Interface->ExecuteOperation(doOp);
}

//berry::PlatformUI::GetWorkbench() doesn't work, this function not used.
//void svProjectDataNodesPluginActivator::SetupDataManagerDoubleClick()
//{
//    berry::IWorkbench* workbench=berry::PlatformUI::GetWorkbench();
//    if(workbench==NULL)
//        return;

//    berry::IWorkbenchWindow::Pointer window=workbench->GetActiveWorkbenchWindow();
//    if(window.IsNull())
//        return;

//    berry::IWorkbenchPage::Pointer page = window->GetActivePage();
//    if(page.IsNull())
//        return;

//    berry::IViewPart::Pointer dataManagerView = window->GetActivePage()->FindView("org.mitk.views.datamanager");
//    if(dataManagerView.IsNull())
//        return;

//    QmitkDataManagerView* dataManager=dynamic_cast<QmitkDataManagerView*>(dataManagerView.GetPointer());
//    QTreeView* treeView=dataManager->GetTreeView();

//    QObject::connect(treeView, SIGNAL(doubleClicked(const QModelIndex &)), this, SLOT(ShowSVView()));
//}

//void svProjectDataNodesPluginActivator::ShowSVView()
//{
//    berry::IWorkbenchWindow::Pointer window=berry::PlatformUI::GetWorkbench()->GetActiveWorkbenchWindow();

//    if(window.IsNull())
//        return;

//    berry::IWorkbenchPage::Pointer page = window->GetActivePage();
//    if(page.IsNull())
//        return;

//    std::list< mitk::DataNode::Pointer > list=GetSelectedDataNodes();
//    if(list.size()==0)
//        return;

//    QList<mitk::DataNode::Pointer> nodes=QList<mitk::DataNode::Pointer>::fromStdList(list);

//    if(nodes.size() < 1)
//    {
//        return;
//    }

//    mitk::DataNode::Pointer selectedNode = nodes.front();

//    mitk::NodePredicateDataType::Pointer isPath = mitk::NodePredicateDataType::New("svPath");
//    mitk::NodePredicateDataType::Pointer isContourGroup = mitk::NodePredicateDataType::New("svContourGroup");
//    mitk::NodePredicateDataType::Pointer isModel = mitk::NodePredicateDataType::New("svModel");
//    mitk::NodePredicateDataType::Pointer isMesh = mitk::NodePredicateDataType::New("svMitkMesh");
//    mitk::NodePredicateDataType::Pointer isSimJob = mitk::NodePredicateDataType::New("svMitkSimJob");

//    if(isPath->CheckNode(selectedNode))
//    {
//       page->ShowView("org.sv.views.pathplanning");
//    }
//    else if(isContourGroup->CheckNode(selectedNode))
//    {
//       page->ShowView("org.sv.views.segmentation2d");
//    }
//    else if(isModel->CheckNode(selectedNode))
//    {
//       page->ShowView("org.sv.views.modeling");
//    }
//    else if(isMesh->CheckNode(selectedNode))
//    {
//       page->ShowView("org.sv.views.meshing");
//    }
//    else if(isSimJob->CheckNode(selectedNode))
//    {
//       page->ShowView("org.sv.views.simulation");
//    }

//}
