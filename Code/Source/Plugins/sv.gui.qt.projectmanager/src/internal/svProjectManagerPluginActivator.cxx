#include "svProjectManagerPluginActivator.h"
//#include <QtPlugin>
#include "svProjectManagerView.h"
#include "QmitkNodeDescriptorManager.h"
#include "mitkNodePredicateDataType.h"

using namespace mitk;

ctkPluginContext* svProjectManagerPluginActivator::m_Context = nullptr;

void svProjectManagerPluginActivator::start(ctkPluginContext* context)
{
    BERRY_REGISTER_EXTENSION_CLASS(svProjectManagerView, context)

    m_Context=context;

    QmitkNodeDescriptorManager* descriptorManager = QmitkNodeDescriptorManager::GetInstance();

    mitk::NodePredicateDataType::Pointer isProjectFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("svProjectFolder"), QString(":svprojectfolder.png"), isProjectFolder, this));

    mitk::NodePredicateDataType::Pointer isImageFolder = mitk::NodePredicateDataType::New("svImageFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("svImageFolder"), QString(":svimagefolder.png"), isImageFolder, this));

    mitk::NodePredicateDataType::Pointer isPathFolder = mitk::NodePredicateDataType::New("svPathFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("svPathFolder"), QString(":svpathfolder.png"), isPathFolder, this));

    mitk::NodePredicateDataType::Pointer isPath = mitk::NodePredicateDataType::New("svPath");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("svPath"), QString(":svpath.png"), isPath, this));

    mitk::NodePredicateDataType::Pointer isSegmentationFolder = mitk::NodePredicateDataType::New("svSegmentationFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("svSegmentationFolder"), QString(":svsegfolder.png"), isSegmentationFolder, this));

    mitk::NodePredicateDataType::Pointer isContourGroup = mitk::NodePredicateDataType::New("svContourGroup");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("svContourGroup"), QString(":svcontourgroup.png"), isContourGroup, this));

    mitk::NodePredicateDataType::Pointer isModelFolder = mitk::NodePredicateDataType::New("svModelFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("svModelFolder"), QString(":svmodelfolder.png"), isModelFolder, this));

    mitk::NodePredicateDataType::Pointer isModel = mitk::NodePredicateDataType::New("svModel");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("svModel"), QString(":svmodel.png"), isModel, this));

    mitk::NodePredicateDataType::Pointer isMeshFolder = mitk::NodePredicateDataType::New("svMeshFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("svMeshFolder"), QString(":svmeshfolder.png"), isMeshFolder, this));

    mitk::NodePredicateDataType::Pointer isSimulationFolder = mitk::NodePredicateDataType::New("svSimulationFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("svSimulationFolder"), QString(":svsimfolder.png"), isSimulationFolder, this));

    QmitkNodeDescriptor* projectNodeDescriptor = descriptorManager->GetDescriptor("svProjectFolder");
    QAction* closeAction = new QAction(QIcon(":closeproject.png"), "Close Project", this);
    QObject::connect( closeAction, SIGNAL( triggered(bool) ) , this, SLOT( CloseProject(bool) ) );
    projectNodeDescriptor->AddAction(closeAction,false);
    m_DescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(projectNodeDescriptor, closeAction));

    QmitkNodeDescriptor* folderNodeDescriptor = descriptorManager->GetDescriptor("svImageFolder");
    QAction* action = new QAction(QIcon(":addimage.png"), "Add Image", this);
    QObject::connect( action, SIGNAL( triggered(bool) ) , this, SLOT( AddImage(bool) ) );
    folderNodeDescriptor->AddAction(action,false);
    m_DescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(folderNodeDescriptor, action));

}

void svProjectManagerPluginActivator::stop(ctkPluginContext* context)
{
    Q_UNUSED(context)
    m_Context=nullptr;
}

ctkPluginContext* svProjectManagerPluginActivator::GetContext()
{
  return m_Context;
}

void svProjectManagerPluginActivator::AddImage(bool)
{
//    ctkServiceReference ref = GetContext()->getServiceReference<mitk::IDataStorageService>();
//    assert(ref == true);

//    mitk::IDataStorageService* service = mitk::PluginActivator::getContext()->getService<mitk::IDataStorageService>(ref);

//    assert(service);

//    return service->GetDefaultDataStorage()->GetDataStorage();


//    QList<mitk::DataNode::Pointer> nodes=GetCurrentSelection();
//    mitk::NodePredicateDataType::Pointer isImageFolder = mitk::NodePredicateDataType::New("svImageFolder");

//    if(nodes.size()>0 && isImageFolder->CheckNode(nodes.front()))
//    {
//        mitk::DataNode::Pointer imageFolderNode=nodes.front();

//        QString imageFilePath = QFileDialog::getOpenFileName(this, tr("Open File"),
//                                                             QDir::homePath(),
//                                                             tr("Image Files (*.vti *.nrrd *.dcm)"),NULL,QFileDialog::DontUseNativeDialog);

//        if(!imageFilePath.isEmpty())
//        {
//            svProjectManager::AddImage(GetDataStorage(), imageFilePath, imageFolderNode);
//        }

//    }

}

void svProjectManagerPluginActivator::CloseProject(bool)
{
//    QList<mitk::DataNode::Pointer> nodes=GetCurrentSelection();
//    mitk::NodePredicateDataType::Pointer isProjectFolder = mitk::NodePredicateDataType::New("svProjectFolder");

//    if(nodes.size()>0 && isProjectFolder->CheckNode(nodes.front()))
//    {

//        QString msg = "Are you sure that you want to close the project "+QString::fromStdString(nodes.front()->GetName())+"?";
//        if (QMessageBox::question(NULL, "Close Project", msg,
//                                  QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
//        {
//            return;
//        }

//        try
//        {
//            mitk::DataStorage::Pointer dataStorage = GetDataStorage();

//            mitk::DataStorage::SetOfObjects::ConstPointer nodesToRemove=dataStorage->GetDerivations(nodes.front(),nullptr,false);

//            if( !nodesToRemove->empty())
//            {
//                dataStorage->Remove(nodesToRemove);
//            }

//            dataStorage->Remove(nodes.front());

//        }
//        catch (std::exception& e)
//        {
//            MITK_ERROR << "Exception caught during closing project: " << e.what();
//            QMessageBox::warning(NULL, "Error", QString("An error occurred during Close Project: %1").arg(e.what()));
//        }

//    }

}

//Q_EXPORT_PLUGIN2(my_demoproject_exampleplugin, mitk::PluginActivator)
