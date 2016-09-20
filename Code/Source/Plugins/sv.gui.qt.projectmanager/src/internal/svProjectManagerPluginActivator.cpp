#include "svProjectManagerPluginActivator.h"
//#include <QtPlugin>
#include "svProjectManagerView.h"
#include "QmitkNodeDescriptorManager.h"
#include "mitkNodePredicateDataType.h"

namespace mitk {

void svProjectManagerPluginActivator::start(ctkPluginContext* context)
{
  BERRY_REGISTER_EXTENSION_CLASS(svProjectManagerView, context)

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

}

void svProjectManagerPluginActivator::stop(ctkPluginContext* context)
{
  Q_UNUSED(context)
}

}

//Q_EXPORT_PLUGIN2(my_demoproject_exampleplugin, mitk::PluginActivator)
