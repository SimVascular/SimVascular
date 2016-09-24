#include "svProjectDataNodesPluginActivator.h"
#include "svPathObjectFactory.h"

#include "QmitkNodeDescriptorManager.h"
#include "mitkNodePredicateDataType.h"

void svProjectDataNodesPluginActivator::start(ctkPluginContext* context)
{
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

    mitk::NodePredicateDataType::Pointer isSimulationFolder = mitk::NodePredicateDataType::New("svSimulationFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("svSimulationFolder"), QString(":svsimfolder.png"), isSimulationFolder, descriptorManager));

}

void svProjectDataNodesPluginActivator::stop(ctkPluginContext* context)
{
}

static RegistersvPathObjectFactory registersvPathObjectFactory;
