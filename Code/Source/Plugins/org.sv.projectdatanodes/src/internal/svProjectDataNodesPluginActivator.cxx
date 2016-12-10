#include "svProjectDataNodesPluginActivator.h"
#include "svPathObjectFactory.h"
#include "svSegmentationObjectFactory.h"
#include "svModelObjectFactory.h"
#include "svMitkMeshObjectFactory.h"
#include "svMitkSimulationObjectFactory.h"
#include "svDataFolder.h"

#include <QmitkNodeDescriptorManager.h>
#include <mitkNodePredicateDataType.h>
#include <mitkIDataStorageService.h>
#include <mitkDataNodeSelection.h>

#include <berryPlatformUI.h>
#include <berryISelectionService.h>
#include <berryIWorkbenchPage.h>
#include <berryIWorkbench.h>
#include <QmitkDataManagerView.h>

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

}

svProjectDataNodesPluginActivator::~svProjectDataNodesPluginActivator()
{
    for (std::vector< std::pair< QmitkNodeDescriptor*, QAction* > >::iterator it = m_DescriptorActionList.begin();it != m_DescriptorActionList.end(); it++)
    {
        (it->first)->RemoveAction(it->second);
    }
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


        for (std::vector<mitk::DataNode::Pointer>::iterator it = selectedNodes.begin()
             ; it != selectedNodes.end(); it++)
        {
            node = *it;
            if( !isDataFolder->CheckNode(node))
            {
                dataStorage->Remove(node);
            }
            //      if (m_GlobalReinitOnNodeDelete)
            //          this->GlobalReinit(false);
        }
    }
}

void svProjectDataNodesPluginActivator::RenameSelectedNode( bool )
{
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
            node->SetName(newName.toStdString());

        }
    }
}

//berry::PlatformUI::GetWorkbench() doesn't work, this function not used.
void svProjectDataNodesPluginActivator::SetupDataManagerDoubleClick()
{
    berry::IWorkbench* workbench=berry::PlatformUI::GetWorkbench();
    if(workbench==NULL)
        return;

    berry::IWorkbenchWindow::Pointer window=workbench->GetActiveWorkbenchWindow();
    if(window.IsNull())
        return;

    berry::IWorkbenchPage::Pointer page = window->GetActivePage();
    if(page.IsNull())
        return;

    berry::IViewPart::Pointer dataManagerView = window->GetActivePage()->FindView("org.mitk.views.datamanager");
    if(dataManagerView.IsNull())
        return;

    QmitkDataManagerView* dataManager=dynamic_cast<QmitkDataManagerView*>(dataManagerView.GetPointer());
    QTreeView* treeView=dataManager->GetTreeView();

    QObject::connect(treeView, SIGNAL(doubleClicked(const QModelIndex &)), this, SLOT(ShowSVView()));
}

void svProjectDataNodesPluginActivator::ShowSVView()
{
    berry::IWorkbenchWindow::Pointer window=berry::PlatformUI::GetWorkbench()->GetActiveWorkbenchWindow();

    if(window.IsNull())
        return;

    berry::IWorkbenchPage::Pointer page = window->GetActivePage();
    if(page.IsNull())
        return;

    std::list< mitk::DataNode::Pointer > list=GetSelectedDataNodes();
    if(list.size()==0)
        return;

    QList<mitk::DataNode::Pointer> nodes=QList<mitk::DataNode::Pointer>::fromStdList(list);

    if(nodes.size() < 1)
    {
        return;
    }

    mitk::DataNode::Pointer selectedNode = nodes.front();

    mitk::NodePredicateDataType::Pointer isPath = mitk::NodePredicateDataType::New("svPath");
    mitk::NodePredicateDataType::Pointer isContourGroup = mitk::NodePredicateDataType::New("svContourGroup");
    mitk::NodePredicateDataType::Pointer isModel = mitk::NodePredicateDataType::New("svModel");
    mitk::NodePredicateDataType::Pointer isMesh = mitk::NodePredicateDataType::New("svMitkMesh");
    mitk::NodePredicateDataType::Pointer isSimJob = mitk::NodePredicateDataType::New("svMitkSimJob");

    if(isPath->CheckNode(selectedNode))
    {
       page->ShowView("org.sv.views.pathplanning");
    }
    else if(isContourGroup->CheckNode(selectedNode))
    {
       page->ShowView("org.sv.views.segmentation2d");
    }
    else if(isModel->CheckNode(selectedNode))
    {
       page->ShowView("org.sv.views.modeling");
    }
    else if(isMesh->CheckNode(selectedNode))
    {
       page->ShowView("org.sv.views.meshing");
    }
    else if(isSimJob->CheckNode(selectedNode))
    {
       page->ShowView("org.sv.views.simulation");
    }

}
