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

#include "sv4gui_ProjectDataNodesPluginActivator.h"
#include "sv4gui_PathObjectFactory.h"
#include "sv4gui_SegmentationObjectFactory.h"
#include "sv4gui_ModelObjectFactory.h"
#include "sv4gui_MitkMeshObjectFactory.h"
#include "sv4gui_MitkSimulationObjectFactory.h"
#include "sv4gui_MitksvFSIObjectFactory.h"
#include "sv4gui_MitkROMSimulationObjectFactory.h"

#include <QmitkNodeDescriptorManager.h>
#include <mitkNodePredicateDataType.h>

#include <QLibrary>

static Registersv4guiPathObjectFactory registersv4guiPathObjectFactory;
static Registersv4guiSegmentationObjectFactory registersv4guiSegmentationObjectFactory;
static Registersv4guiModelObjectFactory registersv4guiModelObjectFactory;
static Registersv4guiMitkMeshObjectFactory registersv4guiMitkMeshObjectFactory;
static Registersv4guiMitkSimulationObjectFactory registersv4guiMitkSimulationObjectFactory;
static Registersv4guiMitksvFSIObjectFactory registersv4guiMitksvFSIObjectFactory;
static Registersv4guiMitkROMSimulationObjectFactory registersv4guiMitkROMSimulationObjectFactory;

//ctkPluginContext* sv4guiProjectDataNodesPluginActivator::m_Context = nullptr;

sv4guiProjectDataNodesPluginActivator::sv4guiProjectDataNodesPluginActivator()
{
}

sv4guiProjectDataNodesPluginActivator::~sv4guiProjectDataNodesPluginActivator()
{
}

// needed for Windows
#ifdef LoadLibrary
#undef LoadLibrary
#endif

void sv4guiProjectDataNodesPluginActivator::LoadLibrary(QString name, QString libFileName)
{
    QLibrary extraLib(libFileName);
    if(extraLib.load())
        MITK_INFO<< name.toStdString() + " module loaded.";
//    else
//    {
//        MITK_INFO<< name.toStdString() + " module not loaded.";
//        MITK_INFO<< extraLib.errorString().toStdString();
//    }
}

void sv4guiProjectDataNodesPluginActivator::LoadModules()
{
    LoadLibrary("OpenCASCADE", "lib_simvascular_module_model_occt");

    LoadLibrary("Parasolid", "lib_simvascular_module_model_parasolid");

    LoadLibrary("MeshSim", "lib_simvascular_module_meshsim");
}

void sv4guiProjectDataNodesPluginActivator::start(ctkPluginContext* context)
{
//    this->m_Context = context;

    LoadModules();

    QmitkNodeDescriptorManager* descriptorManager = QmitkNodeDescriptorManager::GetInstance();

    mitk::NodePredicateDataType::Pointer isProjectFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("sv4guiProjectFolder"), QString(":svprojectfolder.png"), isProjectFolder, descriptorManager));

    mitk::NodePredicateDataType::Pointer isImageFolder = mitk::NodePredicateDataType::New("sv4guiImageFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("sv4guiImageFolder"), QString(":svimagefolder.png"), isImageFolder, descriptorManager));

    mitk::NodePredicateDataType::Pointer isPathFolder = mitk::NodePredicateDataType::New("sv4guiPathFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("sv4guiPathFolder"), QString(":svpathfolder.png"), isPathFolder, descriptorManager));

    mitk::NodePredicateDataType::Pointer isPath = mitk::NodePredicateDataType::New("sv4guiPath");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("sv4guiPath"), QString(":svpath.png"), isPath, descriptorManager));

    mitk::NodePredicateDataType::Pointer isSegmentationFolder = mitk::NodePredicateDataType::New("sv4guiSegmentationFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("sv4guiSegmentationFolder"), QString(":svsegfolder.png"), isSegmentationFolder, descriptorManager));

    mitk::NodePredicateDataType::Pointer isContourGroup = mitk::NodePredicateDataType::New("sv4guiContourGroup");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("sv4guiContourGroup"), QString(":svcontourgroup.png"), isContourGroup, descriptorManager));

    mitk::NodePredicateDataType::Pointer isSeg3D = mitk::NodePredicateDataType::New("sv4guiMitkSeg3D");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("sv4guiMitkSeg3D"), QString(":svseg3d.png"), isSeg3D, descriptorManager));
#ifdef SV_USE_MITK_SEGMENTATION
    mitk::NodePredicateDataType::Pointer isSeg3DMITK = mitk::NodePredicateDataType::New("sv4guiMitkSeg3DMITK");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("sv4guiMitkSeg3DMITK"), QString(":svseg3d.png"), isSeg3DMITK, descriptorManager));
#endif
    mitk::NodePredicateDataType::Pointer isModelFolder = mitk::NodePredicateDataType::New("sv4guiModelFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("sv4guiModelFolder"), QString(":svmodelfolder.png"), isModelFolder, descriptorManager));

    mitk::NodePredicateDataType::Pointer isModel = mitk::NodePredicateDataType::New("sv4guiModel");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("sv4guiModel"), QString(":svmodel.png"), isModel, descriptorManager));

    mitk::NodePredicateDataType::Pointer isMeshFolder = mitk::NodePredicateDataType::New("sv4guiMeshFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("sv4guiMeshFolder"), QString(":svmeshfolder.png"), isMeshFolder, descriptorManager));

    mitk::NodePredicateDataType::Pointer isMesh = mitk::NodePredicateDataType::New("sv4guiMitkMesh");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("sv4guiMitkMesh"), QString(":svmitkmesh.png"), isMesh, descriptorManager));

    // Simulation
    mitk::NodePredicateDataType::Pointer isSimulationFolder = mitk::NodePredicateDataType::New("sv4guiSimulationFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("sv4guiSimulationFolder"), QString(":svsimfolder.png"), isSimulationFolder, descriptorManager));

    mitk::NodePredicateDataType::Pointer isSimJob = mitk::NodePredicateDataType::New("sv4guiMitkSimJob");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("sv4guiMitkSimJob"), QString(":svsimjob.png"), isSimJob, descriptorManager));


    // svFSI
    mitk::NodePredicateDataType::Pointer issvFSIFolder = mitk::NodePredicateDataType::New("sv4guisvFSIFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("sv4guisvFSIFolder"), QString(":svfsifolder.png"), issvFSIFolder, descriptorManager));

    mitk::NodePredicateDataType::Pointer issvFSIJob = mitk::NodePredicateDataType::New("sv4guiMitksvFSIJob");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("sv4guiMitksvFSIJob"), QString(":svfsijob.png"), issvFSIJob, descriptorManager));


    // ROM Simulation
    auto isROMSimulationFolder = mitk::NodePredicateDataType::New("sv4guiROMSimulationFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("sv4guiROMSimulationFolder"), QString(":svromsimfolder.png"), isROMSimulationFolder, descriptorManager));

    mitk::NodePredicateDataType::Pointer isROMSimJob = mitk::NodePredicateDataType::New("sv4guiMitkROMSimJob");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("sv4guiMitkROMSimJob"), QString(":svromsimjob.png"), isROMSimJob, descriptorManager));

    // Repository
    mitk::NodePredicateDataType::Pointer isRepositoryFolder = mitk::NodePredicateDataType::New("sv4guiRepositoryFolder");
    descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("sv4guiRepositoryFolder"), QString(":svrepositoryfolder.png"), isRepositoryFolder, descriptorManager));

//    SetupDataManagerDoubleClick();
}

void sv4guiProjectDataNodesPluginActivator::stop(ctkPluginContext* context)
{
}

//berry::PlatformUI::GetWorkbench() doesn't work, this function not used.
//void sv4guiProjectDataNodesPluginActivator::SetupDataManagerDoubleClick()
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

//void sv4guiProjectDataNodesPluginActivator::ShowSVView()
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

//    mitk::NodePredicateDataType::Pointer isPath = mitk::NodePredicateDataType::New("sv4guiPath");
//    mitk::NodePredicateDataType::Pointer isContourGroup = mitk::NodePredicateDataType::New("sv4guiContourGroup");
//    mitk::NodePredicateDataType::Pointer isModel = mitk::NodePredicateDataType::New("sv4guiModel");
//    mitk::NodePredicateDataType::Pointer isMesh = mitk::NodePredicateDataType::New("sv4guiMitkMesh");
//    mitk::NodePredicateDataType::Pointer isSimJob = mitk::NodePredicateDataType::New("sv4guiMitkSimJob");

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
