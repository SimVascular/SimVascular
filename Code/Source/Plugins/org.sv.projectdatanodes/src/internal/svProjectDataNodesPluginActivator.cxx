#include "svProjectDataNodesPluginActivator.h"
#include "svPathObjectFactory.h"
#include "svSegmentationObjectFactory.h"
#include "svModelObjectFactory.h"
#include "svMitkMeshObjectFactory.h"
#include "svMitkSimulationObjectFactory.h"

#include <QmitkNodeDescriptorManager.h>
#include <mitkNodePredicateDataType.h>

#include <QLibrary>

static RegistersvPathObjectFactory registersvPathObjectFactory;
static RegistersvSegmentationObjectFactory registersvSegmentationObjectFactory;
static RegistersvModelObjectFactory registersvModelObjectFactory;
static RegistersvMitkMeshObjectFactory registersvMitkMeshObjectFactory;
static RegistersvMitkSimulationObjectFactory registersvMitkSimulationObjectFactory;

//ctkPluginContext* svProjectDataNodesPluginActivator::m_Context = nullptr;

svProjectDataNodesPluginActivator::svProjectDataNodesPluginActivator()
{
}

svProjectDataNodesPluginActivator::~svProjectDataNodesPluginActivator()
{
}

void svProjectDataNodesPluginActivator::start(ctkPluginContext* context)
{
//    this->m_Context = context;

    //Load optional libs: occt, parasolid, meshsim
    QLibrary occtlib("lib_simvascular_module_model_occt");
    if(occtlib.load())
        MITK_INFO<<"OpenCASCADE module loaded.";
    else
        MITK_INFO<<"OpenCASCADE module not loaded.";

    QLibrary parasolidlib("lib_simvascular_module_model_parasolid");
    if(parasolidlib.load())
        MITK_INFO<<"Parasolid module loaded.";
    else
        MITK_INFO<<"Parasolid module not loaded.";

    QLibrary meshsimlib("lib_simvascular_module_meshsim");
    if(meshsimlib.load())
        MITK_INFO<<"MeshSim module loaded.";
    else
        MITK_INFO<<"MeshSim module not loaded.";

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

//    SetupDataManagerDoubleClick();
}

void svProjectDataNodesPluginActivator::stop(ctkPluginContext* context)
{
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
