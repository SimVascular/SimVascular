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

#include "sv4gui_ImageProcessing.h"
#include "sv4gui_ImageProcessingUtils.h"
#include "ui_sv4gui_ImageProcessing.h"
#include "sv4gui_VtkUtils.h"
#include <sv4gui_ImageSeedMapper.h>
#include <sv4gui_ImageSeedMapper2D.h>
#include <sv4gui_ImageSeedInteractor.h>
#include <sv4gui_Seg3D.h>
#include <sv4gui_MitkSeg3D.h>

#include <vtkImageData.h>
#include <vtkSmartPointer.h>

#include <mitkDataStorage.h>
#include <mitkDataNode.h>
#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>
#include <mitkImage.h>
#include <mitkImageCast.h>

#include "sv_PolyData.h"
#include "sv_vmtk_utils_init.h"
#include "sv_vmtk_utils.h"
#include <vtkXMLPolyDataWriter.h>
#include <vtkXMLPolyDataReader.h>

#include <usModuleRegistry.h>
#include <usGetModuleContext.h>
#include <usModule.h>
#include <usModuleContext.h>
#include <QInputDialog>
#include <QMessageBox>
#include <QDir>

#include <array>

const QString sv4guiImageProcessing::EXTENSION_ID = "org.sv.views.imageprocessing";

// Set the names used to create for SV Data Manager nodes.
const std::string sv4guiImageProcessing::CENTERLINES_NODE_NAME = "centerlines";
const std::string sv4guiImageProcessing::COLLIDING_FRONTS_NODE_NAME = "colliding-fronts";
const std::string sv4guiImageProcessing::SEED_POINTS_NODE_NAME = "seed-points";
const std::string sv4guiImageProcessing::SURFACE_NODE_NAME = "surface";

//-----------------------
// sv4guiImageProcessing
//-----------------------
//
sv4guiImageProcessing::sv4guiImageProcessing() : ui(new Ui::sv4guiImageProcessing)
{

}

sv4guiImageProcessing::~sv4guiImageProcessing()
{
  delete ui;
}

//-----------------------
// SetLineEditValidFloat
//-----------------------
// Set float validator for line edit widgets.
//
// This restricts the line edit widget to valid float values.
//
// Note: The QDoubleValidator does not work that well when specifying
// a range, does not prevent typing in an invalid values.
//
void sv4guiImageProcessing::SetLineEditValidFloat(QLineEdit* lineEdit)
{
  QDoubleValidator* dvalid = new QDoubleValidator(this);
  dvalid->setNotation(QDoubleValidator::StandardNotation);
  lineEdit->setValidator(dvalid);
}

//--------------------
// GetOutputDirectory
//--------------------
//
QString sv4guiImageProcessing::GetOutputDirectory()
{
  QString outputDir = "";
  if (m_CollidingFrontsNode.IsNull()) {
      return outputDir;
  }

  mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
  mitk::DataStorage::SetOfObjects::ConstPointer rs = GetDataStorage()->GetSources (m_CollidingFrontsNode, isProjFolder, false);

  std::string projPath = "";
  std::string folderName = "";

  if (rs->size() > 0) {
      mitk::DataNode::Pointer projFolderNode = rs->GetElement(0);
      projFolderNode->GetStringProperty("project path", projPath);
      rs = GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiImageFolder"));

      if (rs->size() > 0) {
          mitk::DataNode::Pointer folderNode = rs->GetElement(0);
          folderName = folderNode->GetName();
          outputDir = QString::fromStdString(projPath+"/"+folderName+"/"+m_CollidingFrontsNode->GetName());
      }
  }

  return outputDir;
}

//---------------------
// CreateQtPartControl
//---------------------
// Initialize GUI widgets and create SV Manager Data nodes.
//
void sv4guiImageProcessing::CreateQtPartControl(QWidget *parent)
{
  //std::cout << "========== sv4guiImageProcessing::CreateQtPartControl ========== " << std::endl;
  m_Parent = parent;
  ui->setupUi(parent);

  // Get access to the four-window widget in the centre of the application.
  m_DisplayWidget = GetActiveStdMultiWidget();

  if (m_DisplayWidget == NULL) {
      parent->setEnabled(false);
      MITK_ERROR << "Plugin ImageProcessing Init Error: No QmitkStdMultiWidget!";
      return;
  }

  // The plugin panel is displayed when SV starts so grey 
  // out the panel if no project is defined.
  m_DataStorage = GetDataStorage();
  if (m_DataStorage == nullptr) {
    return;
  }
  mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
  mitk::DataNode::Pointer projFolderNode = m_DataStorage->GetNode(isProjFolder);
  if (projFolderNode == nullptr) {
    parent->setEnabled(false);
    return;
  }
  parent->setEnabled(true);

  // Hide edge image widgets.
  ui->inputLabel2->setVisible(false);
  ui->edgeImageComboBox->setVisible(false);

  // Set callbacks for seed widgets.
  connect(ui->SeedSizeLineEdit, SIGNAL(editingFinished()), this, SLOT(SeedSize()));
  connect(ui->seedCheckBox, SIGNAL(clicked(bool)), this, SLOT(displaySeeds(bool)));
  connect(ui->AddStartButton, SIGNAL(clicked(bool)), this, SLOT(AddStartSeed()));
  connect(ui->AddEndButton, SIGNAL(clicked(bool)), this, SLOT(AddEndSeed()));
  connect(ui->ClearButton, SIGNAL(clicked(bool)), this, SLOT(ClearSeeds()));

  // Set callbacks for centerlines widgets.
  connect(ui->ComputeCenterlinesButton, SIGNAL(clicked(bool)), this, SLOT(ComputeCenterlines()));

  // Add mouse key shortcuts for adding and deleting a seed points.
  ui->AddStartButton->setShortcut(QKeySequence("S"));
  ui->AddEndButton->setShortcut(QKeySequence("E"));

  // Full pipelines tab widgets. 
  //
  // Some of these are defined using DoubleSpinBoxes so their range can be set.
  //
  ui->fullCFGradientLineEdit->setMinimum(0.0);
  SetLineEditValidFloat(ui->fullCFUpperThresholdLineEdit);
  SetLineEditValidFloat(ui->fullCFLowerThresholdLineEdit);
  ui->fullCFPropagationLineEdit->setRange(0.0, 1.0);
  ui->fullCFAdvectionLineEdit->setRange(0.0, 1.0);
  ui->fullCFCurvatureLineEdit->setRange(0.0, 1.0);
  auto numItersValidator = new QIntValidator(this);
  numItersValidator->setBottom(10);
  ui->fullCFIterationsLineEdit->setValidator(numItersValidator);
  SetLineEditValidFloat(ui->fullCFIsoValueLineEdit);
  connect(ui->runCollidingFrontsButton, SIGNAL(clicked()), this, SLOT(runFullCollidingFronts()));

  // Cropping/Editing toolbox.
  //
  // [DaveP] Hide for now.
  /*
  ui->imageEditingPage->setVisible(false);
  ui->imageEditingToolbox->setVisible(false);
  */

  // Filtering toolbox.
  //
  // [DaveP] Hide for now.
  /*
  ui->filteringPage->setVisible(false);
  ui->filteringToolbox->setVisible(false);
  */

  // Segmentation toolbox.
  //
  // [DaveP] Hide for now.
  /*
  ui->segmentationPage->setVisible(false);
  ui->segmentationToolbox->setVisible(false);
  connect(ui->thresholdButton, SIGNAL(clicked()), this, SLOT(runThreshold()));
  connect(ui->binaryThresholdButton, SIGNAL(clicked()), this, SLOT(runBinaryThreshold()));
  connect(ui->editImageButton, SIGNAL(clicked()), this, SLOT(runEditImage()));
  connect(ui->cropImageButton, SIGNAL(clicked()), this, SLOT(runCropImage()));
  connect(ui->resampleImageButton, SIGNAL(clicked()), this, SLOT(runResampleImage()));
  connect(ui->zeroLevelButton, SIGNAL(clicked()), this, SLOT(runZeroLevel()));
  connect(ui->CFButton, SIGNAL(clicked()), this, SLOT(runCollidingFronts()));

  connect(ui->marchingCubesButton, SIGNAL(clicked()), this, SLOT(runIsovalue()));
  connect(ui->gradientMagnitudeButton, SIGNAL(clicked()), this, SLOT(runGradientMagnitude()));
  connect(ui->smoothButton, SIGNAL(clicked()), this, SLOT(runSmoothing()));
  connect(ui->anisotropicButton, SIGNAL(clicked()), this, SLOT(runAnisotropic()));
  connect(ui->levelSetButton, SIGNAL(clicked()), this, SLOT(runGeodesicLevelSet()));
  */

  m_Interface = new sv4guiDataNodeOperationInterface();

  // Create objects for storing and processing data created by the pluginn; seed points, centerlines, etc.
  //
  if (!m_PluginInitialized){
    mitk::DataNode::Pointer imageNode = GetDataStorage()->GetNamedNode("Images");
    if (imageNode.IsNull()) {
        return;
    }

    // Create main colliding fronts node for storing all data created.
    m_CollidingFrontsNode = mitk::DataNode::New();
    m_CollidingFrontsNode->SetName(COLLIDING_FRONTS_NODE_NAME);
    m_CollidingFrontsNode->SetVisibility(true);
    // [TODO:DaveP] nodes must have data so add something.
    auto seedContainer = sv4guiImageSeedContainer::New();
    m_CollidingFrontsNode->SetData(seedContainer);
    GetDataStorage()->Add(m_CollidingFrontsNode, imageNode);
    m_PluginInitialized = true;

    // Create the 'colliding-fronts' directory used to store surfaces, centerlines, etc.
    m_PluginOutputDirectory = GetOutputDirectory();
    QDir dir(m_PluginOutputDirectory);
    if (!dir.exists()){
        dir.mkpath(".");
    }

    // Create a node for storing seed points.
    m_SeedNode = mitk::DataNode::New();
    m_SeedNode->SetName(SEED_POINTS_NODE_NAME);
    m_SeedContainer = sv4guiImageSeedContainer::New();
    m_SeedNode->SetData(m_SeedContainer);
    GetDataStorage()->Add(m_SeedNode, m_CollidingFrontsNode);

    // Create 2D and 3D mappers for displaying seeds as spheres.
    m_SeedMapper = sv4guiImageSeedMapper::New();
    m_SeedMapper->SetDataNode(m_SeedNode);
    m_SeedMapper->m_box = false;
    m_SeedNode->SetMapper(mitk::BaseRenderer::Standard3D, m_SeedMapper);

    m_SeedMapper2D = sv4guiImageSeedMapper2D::New();
    m_SeedMapper2D->SetDataNode(m_SeedNode);
    m_SeedNode->SetMapper(mitk::BaseRenderer::Standard2D, m_SeedMapper2D);

    // Create a node used to process mouse events for selecting seeds. 
    m_SeedInteractor = sv4guiImageSeedInteractor::New();
    m_SeedInteractor->LoadStateMachine("seedInteraction.xml", us::ModuleRegistry::GetModule("sv4guiModuleImageProcessing"));
    m_SeedInteractor->SetEventConfig("seedConfig.xml", us::ModuleRegistry::GetModule("sv4guiModuleImageProcessing"));
    m_SeedInteractor->SetDataNode(m_SeedNode);

    if (imageNode) {
        imageNode->SetVisibility(true);
    }

    m_SeedNode->SetVisibility(false);
    mitk::RenderingManager::GetInstance()->RequestUpdateAll();

    m_SeedNode->SetVisibility(true);
    mitk::RenderingManager::GetInstance()->RequestUpdateAll();

    displaySeeds(true);

   readData();
  }
}

//----------
// readData
//----------
//
void sv4guiImageProcessing::readData()
{
  std::cout << "=========== sv4guiImageProcessing::readData ===========" << std::endl;

  auto dirPath = m_PluginOutputDirectory.toStdString();
  std::string fileName = dirPath + "/centerlines.vtp"; 
  auto *centerlines = vtkPolyData::New();

  std::cout << "[readData] dirPath: " << dirPath << std::endl;
  vtkSmartPointer<vtkXMLPolyDataReader> reader = vtkSmartPointer<vtkXMLPolyDataReader>::New();
  reader->SetFileName(fileName.c_str());
  reader->Update();
  centerlines->DeepCopy(reader->GetOutput());
  std::cout << "[readData] centerlines num points: " << centerlines->GetNumberOfPoints() << std::endl;
  std::cout << "[readData] centerlines num cells: " << centerlines->GetNumberOfCells() << std::endl;

  // Create centerlines data nodes, setup interactor, etc.
  InitializeCenterlines();

  // Centerline geometry is stored in the container.
  m_CenterlinesContainer->SetLines(centerlines);

  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

//--------------------
// ComputeCenterlines 
//--------------------
// Compute centerlines for the current surface.
//
void sv4guiImageProcessing::ComputeCenterlines()
{
  //std::cout << "========== sv4guiImageProcessing::ComputeCenterlines ========== " << std::endl;

  // A start seed must be defined. 
  int numStartSeeds = m_SeedContainer->GetNumStartSeeds();
  if (numStartSeeds < 1) {
    QMessageBox::warning(NULL,"","No start seeds have been defined.");
    return;
  }

  // An end seed must be defined. 
  int numEndSeeds = m_SeedContainer->GetNumEndSeeds(0);
  if (numEndSeeds < 1) {
    QMessageBox::warning(NULL,"","No end seeds have been defined.");
    return;
  } 

  // Check if there is a segmentation surface.
  if (m_CollidingFrontsSurface.IsNull()) {
    QMessageBox::warning(NULL, "", "No segmentation surface has been created.");
    return;
  }

  auto segPolyData = m_CollidingFrontsSurface->GetVtkPolyData();
  int numCells = segPolyData->GetNumberOfCells();
  int numPoints = segPolyData->GetNumberOfPoints();
  auto points = segPolyData->GetPoints();
  //std::cout << "[ComputeCenterlines] Segmentation polydata:" << std::endl;
  //std::cout << "[ComputeCenterlines]   Number of points: " << numPoints << std::endl;

  // Find the closest node to the start seed.
  std::vector<int> sourceIDs;
  std::vector<int> targetIDs;

  for (int s = 0; s < numStartSeeds; s++) {
      auto seedPoint = m_SeedContainer->GetStartSeed(s);
      int min_id = FindClosesetPoint(segPolyData, seedPoint);
      sourceIDs.push_back(min_id);

      int numEndSeeds = m_SeedContainer->GetNumEndSeeds(s);
      if (numEndSeeds == 0) {
        break;
      }

      for (int e = 0; e < numEndSeeds; e++){
          auto seedPoint = m_SeedContainer->GetEndSeed(s,e);
          int min_id = FindClosesetPoint(segPolyData, seedPoint);
          targetIDs.push_back(min_id);
      }
    }

  cvPolyData* linesDst = nullptr;
  cvPolyData* voronoiDst = nullptr;
  cvPolyData cvSurfPolydata(segPolyData);

  if (sys_geom_centerlines(&cvSurfPolydata, sourceIDs.data(), sourceIDs.size(), targetIDs.data(), targetIDs.size(), 
        &linesDst, &voronoiDst) != SV_OK) {
    QMessageBox::critical(NULL, "", "The centerline extraction computation has failed."); 
    return;
  }

  auto lines = linesDst->GetVtkPolyData();
  //std::cout << "[ComputeCenterlines] Centerlines:" << std::endl;
  //std::cout << "[ComputeCenterlines]   Number of points: " << lines->GetNumberOfPoints() << std::endl;

  // Create centerlines data nodes, setup interactor, etc.
  InitializeCenterlines();

  // Centerline geometry is stored in the container.
  m_CenterlinesContainer->SetLines(lines);

  // Write the centerlines to a file.
  auto dirPath = m_PluginOutputDirectory.toStdString();
  std::string fileName = dirPath + "/centerlines.vtp"; 
  WritePolydata(fileName, lines);

  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

//-----------------------
// InitializeCenterlines
//-----------------------
//
void sv4guiImageProcessing::InitializeCenterlines()
{
  std::cout << "========== sv4guiImageProcessing::InitializeCenterlines ==========" << std::endl;

  // Create a node and container for storing centerline geometry. 
  if (m_CenterlinesNode.IsNull()) { 
      m_CenterlinesNode = mitk::DataNode::New();
      m_CenterlinesNode->SetName(CENTERLINES_NODE_NAME);
      m_CenterlinesContainer = sv4guiImageLinesContainer::New();
      m_CenterlinesNode->SetData(m_CenterlinesContainer);
      GetDataStorage()->Add(m_CenterlinesNode, m_CollidingFrontsNode);
  }

  // Create mapper to display the centerlines.
  if (m_CenterlinesMapper.IsNull()) { 
      m_CenterlinesMapper = sv4guiImageLinesMapper::New();
      m_CenterlinesMapper->SetDataNode(m_CenterlinesNode);
      m_CenterlinesMapper->m_box = false;
      m_CenterlinesMapper->SetColor(0.0, 1.0, 0.0);
      m_CenterlinesNode->SetMapper(mitk::BaseRenderer::Standard3D, m_CenterlinesMapper);
  }

  // Create a node used to process mouse events for selecting centerlines. 
  if (m_CenterlineInteractor.IsNull()) { 
      std::cout << "[InitializeCenterlines] Create m_CenterlineInteractor" << std::endl;
      m_CenterlineInteractor = sv4guiImageCenterlineInteractor::New();
      m_CenterlineInteractor->LoadStateMachine("centerlineInteraction.xml", us::ModuleRegistry::GetModule("sv4guiModuleImageProcessing"));
      m_CenterlineInteractor->SetEventConfig("seedConfig.xml", us::ModuleRegistry::GetModule("sv4guiModuleImageProcessing"));
      m_CenterlineInteractor->SetDataNode(m_CenterlinesNode);
    }

    // This is needed so m_CenterlineInteractor receives events.
    m_CenterlinesNode->SetVisibility(true);
    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

//-------------------
// FindClosesetPoint
//-------------------
// Find the closest point in a vtkPolyData object to a given point.
//
// Returns the index of the closest point into the vtkPolyData points array.
//
int sv4guiImageProcessing::FindClosesetPoint(vtkPolyData* polyData, std::array<double,3>& testPoint)
{
  int numPoints = polyData->GetNumberOfPoints();
  auto points = polyData->GetPoints();
  double min_d = 1e6;
  int min_id = -1;
  double point[3];

  for (int i = 0; i < numPoints; i++) {
      int id = i;
      points->GetPoint(id, point);
      auto dx = point[0] - testPoint[0];
      auto dy = point[1] - testPoint[1];
      auto dz = point[2] - testPoint[2];
      auto d = dx*dx + dy*dy + dz*dz;
      if (d < min_d) {
          min_d = d;
          min_id = id;
      }
  }

  return min_id;
}

//--------------
// AddStartSeed 
//--------------
// Add the position of a start seed to m_SeedContainer.
//
void sv4guiImageProcessing::AddStartSeed()
{
  //std::cout << "========== sv4guiImageProcessing::AddStartSeed ========== " << std::endl;
  mitk::Point3D point = m_DisplayWidget->GetCrossPosition();
  //std::cout << "[AddStartSeed] Point: " << point[0] << "  " << point[1] << "  " << point[2] << std::endl;

  int numStartSeeds = m_SeedContainer->GetNumStartSeeds();
  //std::cout << "[AddStartSeed] numStartSeeds: " << numStartSeeds << std::endl;
  m_SeedContainer->AddStartSeed(point[0], point[1], point[2]);

  // TODO:DaveP] Do we need to updata all?
  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}


//------------
// AddEndSeed 
//------------
// Add the position of an end seed to m_SeedContainer.
//
void sv4guiImageProcessing::AddEndSeed()
{
  //std::cout << "========== sv4guiImageProcessing::AddEndSeed ========== " << std::endl;
  mitk::Point3D point = m_DisplayWidget->GetCrossPosition();
  //std::cout << "[AddEndSeed] Point: " << point[0] << "  " << point[1] << "  " << point[2] << std::endl;

  // A start seed must have been selected.
  int numStartSeeds = m_SeedContainer->GetNumStartSeeds();
  if (numStartSeeds < 1) { 
    QMessageBox::warning(NULL,"","No start seeds have been defined.");
    return;
  }

  // Add add an end seed to the current start seed.
  m_SeedContainer->AddEndSeed(point[0], point[1], point[2]);

  // TODO:DaveP] Do we need to updata all?
  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

//------------
// ClearSeeds 
//------------
// Clear all start and end seeds. 
//
void sv4guiImageProcessing::ClearSeeds()
{
  m_SeedContainer->ClearSeeds();
  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

//--------------
// displaySeeds
//--------------
//
void sv4guiImageProcessing::displaySeeds(bool state)
{
  if (!state){
    GetDataStorage()->Remove(m_SeedNode);
  } else {
    auto imageNode = GetDataStorage()->GetNamedNode("Images");
    if (imageNode){
      //GetDataStorage()->Add(m_SeedNode, imageNode);
    }
  }
  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

//-------------------------
// imageEditingTabSelected
//-------------------------
//
void sv4guiImageProcessing::imageEditingTabSelected()
{
  ui->edgeImageComboBox->setEnabled(false);

/*
  switch(ui->imageEditingToolbox->currentIndex()){
    case 0:
      m_SeedMapper->m_box = true;

      ui->helpLabel->setText("Edit image:\n"
        "Place start and end seed points to make a box\n"
        "pixels in the box will have their value set to Replace Value");
        break;
    case 1:
      m_SeedMapper->m_box = false;

      ui->helpLabel->setText("Crop image:\n"
        "Enter two corners of the box to crop image to");
        break;
    case 2:
      m_SeedMapper->m_box = false;

      ui->helpLabel->setText("Resample Image:\n"
        "Enter the length/pixel to convert the image to");
        break;
  }
  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
*/

}

void sv4guiImageProcessing::filteringTabSelected()
{
  ui->edgeImageComboBox->setEnabled(false);
  m_SeedMapper->m_box = false;

/*
  switch(ui->filteringToolbox->currentIndex()){
    case 0:
      ui->helpLabel->setText("Smooth:\n"
        "Smoothing scale: size of smoothing filter, larger scale means more smoothing");
        break;
    case 1:
      ui->helpLabel->setText("Anisotropic Smooth:\n"
        "Edge preserving smoothing.\n"
        "Iterations: number of smoothing steps (higher = more smoothing)\n"
        "Time step: amount of smoothing per iteration, smaller = less smoothing\n"
        "Conductance: Smoothing strength, higher means more smoothing");
        break;
    case 2:
      ui->helpLabel->setText("Gradient Magnitude:\n"
        "Create new image where every pixel value is the magnitude of the gradient of the original image at that pixel\n"
        "Gradient length scale indicates length to use when calculating the gradient");
        break;
  }
  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
*/

}

void sv4guiImageProcessing::segmentationTabSelected()
{
  ui->edgeImageComboBox->setEnabled(true);
  m_SeedMapper->m_box = false;

/*

  switch(ui->segmentationToolbox->currentIndex()){
      case 0:
          ui->helpLabel->setText("Zero Level:\n"
          "Multiply image by -1 and add isovalue\n"
          "Makes all pixels that had value isovalue now have value 0\n"
          "Useful for level set intialization");
          break;
      case 1:
        ui->helpLabel->setText("Threshold:\n"
          "Set all pixels with values outside upper value and lower value to 0");
          break;
      case 2:
        ui->helpLabel->setText("Binary Threshold:\n"
          "Set all pixels with values outside upper value and lower value to\n"
          "outside value and those inside to inside value");
          break;
      case 3:
        ui->helpLabel->setText("Colliding Fronts:\n"
          "Place multiple start (red) and end (green) seed points\n"
          "Segments all pixels with values inside upper threshold and lower threshold, that are connected to seed points");
          break;
      case 4:
        ui->helpLabel->setText("IsoSurface:\n"
          "Extract an isosurface based on an isovalue\n"
          "Will created a 3D segmentation object in the segmentations folder");
          break;
      case 5:
        ui->helpLabel->setText("Level Set:\n"
          "Extract a segmentation based on a supplied initial level set and edge image (all scaling parameters should be between 0 and 1)\n"
          "Propagation scaling: The balloon force, pushes front outwards\n"
          "Advection scaling: Attracts front to edges\n"
          "curvature scaling: Smooths and shrinks front\n");
          break;
  }
  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
*/

}

void sv4guiImageProcessing::pipelinesTabSelected()
{
  ui->edgeImageComboBox->setEnabled(false);
  m_SeedMapper->m_box = false;
  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

//----------
// SeedSize
//----------
//
void sv4guiImageProcessing::SeedSize()
{
  double seedSize = std::stod(ui->SeedSizeLineEdit->text().toStdString());

  m_SeedMapper->m_seedRadius = seedSize;

  m_SeedInteractor->m_seedRadius = seedSize;
  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

//--------------------
// OnSelectionChanged
//--------------------
//
void sv4guiImageProcessing::OnSelectionChanged(std::vector<mitk::DataNode*> nodes)
{
  m_DataStorage = GetDataStorage();

  if (m_DataStorage == nullptr) {
      return;
  }

  UpdateImageList();
}

//--------------
// getImageName
//--------------
//
std::string sv4guiImageProcessing::getImageName(int imageIndex)
{
  QString imageName;
  if (imageIndex == 0){
    imageName = ui->inputImageComboBox->currentText();
  }else if (imageIndex == 1){
    imageName = ui->edgeImageComboBox->currentText();
  }

  if (imageName.length() == 0){
    MITK_ERROR << "No image "<< imageIndex <<" selected, please select an image" << std::endl;
    return "";
  }

  std::string image_str = imageName.toStdString();
  return image_str;
}

//----------
// getImage
//----------
//
mitk::Image::Pointer sv4guiImageProcessing::getImage(std::string image_name)
{
  mitk::DataNode::Pointer folder_node = GetDataStorage()->GetNamedNode("Images");
  mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetDerivations(folder_node,mitk::NodePredicateDataType::New("Image"));

  bool found = false;
  auto image_node = folder_node;

  for (int i = 0; i < rs->size(); i++){
    auto node = rs->GetElement(i);

    if (node->GetName() == image_name){
      image_node = node;
      found = true;
    }
  }

  if (!found){
    MITK_ERROR << "Image with name "<< image_name <<" not found\n";
    return NULL;
  }

  mitk::Image::Pointer image_data = dynamic_cast<mitk::Image*>(image_node->GetData());

  if (!image_data){
    MITK_ERROR << "Image data is null for image "<< image_name <<"\n";
    return NULL;
  }
  return image_data;
}

//-------------
// getItkImage
//-------------
//
sv4guiImageProcessingUtils::itkImPoint 
sv4guiImageProcessing::getItkImage(int index)
{
  std::string image_name = getImageName(index);
  if (image_name.empty()){
    MITK_ERROR << "No image found";
    return NULL;
  }

  mitk::Image::Pointer image = getImage(image_name);

  if (!image){
    MITK_ERROR << "Image data is null";
    return NULL;
  }

  image->GetScalarValueMin();
  sv4guiImageProcessingUtils::itkImPoint itkImage = sv4guiImageProcessingUtils::itkImageType::New();
  mitk::CastToItkImage(image, itkImage);

  return itkImage;
}

void sv4guiImageProcessing::storeImage(sv4guiImageProcessingUtils::itkImPoint image){
  bool ok;
  QString new_image_name = QInputDialog::getText(m_Parent, tr("New Image Name"),
                                       tr("Enter a name for the new image"), QLineEdit::Normal,
                                       "", &ok);

  mitk::DataNode::Pointer image_folder_node = GetDataStorage()->GetNamedNode("Images");

  if (!image_folder_node){
    MITK_ERROR << "No image folder found\n";
    return;
  }

  mitk::DataNode::Pointer newImageNode =
    GetDataStorage()->GetNamedNode(new_image_name.toStdString());

  if (newImageNode){
    QMessageBox::warning(NULL,"Image Already exists","Please use a different image name!");
    return;
  }
  if(!ok){
    return;
  }

  newImageNode = mitk::DataNode::New();

  newImageNode->SetName(new_image_name.toStdString());

  mitk::Image::Pointer mitkImage;

  std::string image_name = getImageName(0);
  mitkImage = getImage(image_name)->Clone();

  mitk::CastToMitkImage(image, mitkImage);
  newImageNode->SetData(mitkImage);

  addNode(newImageNode, image_folder_node);

  UpdateImageList();
}

//---------------
// storePolyData
//---------------
// Store a vtkPolyData object under an the SV Data Manager 'Segmentations' node.
//
void sv4guiImageProcessing::storePolyData(vtkSmartPointer<vtkPolyData>& surface)
{
  #ifdef davep_storePolyData_old_way
  bool ok;
  QString newNodeName = QInputDialog::getText(m_Parent, tr("New 3D Segmentation Name"), tr("Enter a name for the new 3D Segmentation"), 
      QLineEdit::Normal, "", &ok);

  if (!ok) {
    return;
  }

  // Get the Segmentations node.
  mitk::DataNode::Pointer segFolderNode = GetDataStorage()->GetNamedNode("Segmentations");
  //mitk::DataNode::Pointer segFolderNode = GetDataStorage()->GetNamedNode("Images");
  if (!segFolderNode) {
    MITK_ERROR << "No Segmentations folder found\n";
    QMessageBox::critical(NULL, "", "No Segmentations folder was found in the SV Data Manager."); 
    return;
  }

  // Check for a duplicate node name.
  mitk::DataNode::Pointer newPdNode = GetDataStorage()->GetNamedNode(newNodeName.toStdString());
  if (newPdNode) {
    QMessageBox::warning(NULL, "", "The segmentation node named '" + newNodeName + "' already exists.");
    return;
  }

  // Create a new node.
  newPdNode = mitk::DataNode::New();
  newPdNode->SetName(newNodeName.toStdString());

  // Create segmentation objects to store in the node.
  //
  sv4guiSeg3D* newSeg3D = new sv4guiSeg3D();
  newSeg3D->SetVtkPolyData(vtkPd);

  sv4guiMitkSeg3D::Pointer mitkSeg3D = sv4guiMitkSeg3D::New();
  mitkSeg3D->SetSeg3D(newSeg3D);
  mitkSeg3D->SetDataModified();
  newPdNode->SetData(mitkSeg3D);

  // Add the new node to the Segmentations folder.
  addNode(newPdNode, segFolderNode);

  // [TODO:DaveP] this is a hack! replace later.
  m_LastSegmentationNodeName = newNodeName;
  #endif

  // Create a node for the colliding fronts surface.
  if (m_CollidingFrontsSurfaceNode.IsNull()) {
    m_CollidingFrontsSurfaceNode = mitk::DataNode::New();
    m_CollidingFrontsSurfaceNode->SetName(SURFACE_NODE_NAME);
    GetDataStorage()->Add(m_CollidingFrontsSurfaceNode, m_CollidingFrontsNode);
  }

  // Create an mitk surface to store the vtkPolyData surface. 
  m_CollidingFrontsSurface = mitk::Surface::New();
  m_CollidingFrontsSurfaceNode->SetData(m_CollidingFrontsSurface);
  m_CollidingFrontsSurface->SetVtkPolyData(surface);

  mitk::RenderingManager::GetInstance()->RequestUpdateAll();

  // Write the surface to a file.
  auto dirPath = m_PluginOutputDirectory.toStdString();
  std::string fileName = dirPath + "/surface.vtp"; 
  WritePolydata(fileName, surface);
}

//---------------
// WritePolydata
//---------------
//
void sv4guiImageProcessing::WritePolydata(std::string& fileName, vtkPolyData* polydata)
{
  vtkSmartPointer<vtkXMLPolyDataWriter> writer = vtkSmartPointer<vtkXMLPolyDataWriter>::New();
  writer->SetFileName(fileName.c_str());
  writer->SetInputData(polydata);
  writer->Update();
  writer->Write();
}

//---------
// addNode
//---------
//
void sv4guiImageProcessing::addNode(mitk::DataNode::Pointer child_node, mitk::DataNode::Pointer parent_node)
{
  mitk::OperationEvent::IncCurrObjectEventId();

  bool undoEnabled=true;
  sv4guiDataNodeOperation* doOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpADDDATANODE,
    GetDataStorage(),child_node, parent_node);

  if(undoEnabled)
  {
      sv4guiDataNodeOperation* undoOp = new sv4guiDataNodeOperation(
        sv4guiDataNodeOperation::OpREMOVEDATANODE,GetDataStorage(),child_node, parent_node);
      mitk::OperationEvent *operationEvent = new mitk::OperationEvent(
        m_Interface, doOp, undoOp, "Add DataNode");
      mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
  }
  m_Interface->ExecuteOperation(doOp);
}

//-----------------
// UpdateImageList
//-----------------
//
void sv4guiImageProcessing::UpdateImageList()
{
  auto typeCondition = mitk::NodePredicateDataType::New("Image");
  auto rs = GetDataStorage()->GetSubset(typeCondition);

  if (rs->size() == 0){
    return ;
  }

  ui->inputImageComboBox->clear();
  ui->edgeImageComboBox->clear();

  for (int i = 0; i < rs->size(); i++){
    mitk::DataNode::Pointer Node=rs->GetElement(i);
    if ((Node->GetName() != SEED_POINTS_NODE_NAME)){
      ui->inputImageComboBox->addItem(Node->GetName().c_str());
      ui->edgeImageComboBox->addItem(Node->GetName().c_str());
    }
  }
}

void sv4guiImageProcessing::runGeodesicLevelSet()
{
#ifdef use_sv4guiImageProcessing_runGeodesicLevelSet
  double propagation = std::stod(ui->propagationLineEdit->text().toStdString());
  double advection = std::stod(ui->advectionLineEdit->text().toStdString());
  double curvature = std::stod(ui->curvatureLineEdit->text().toStdString());
  double iterations = std::stod(ui->iterationsLineEdit->text().toStdString());

  sv4guiImageProcessingUtils::itkImPoint initialization = getItkImage(0);
  sv4guiImageProcessingUtils::itkImPoint edgeImage = getItkImage(1);

  if (!initialization || !edgeImage){
    MITK_ERROR << "No image 1 or 2 selected, please select an image 1\n";
    return;
  }

  auto itkImage = sv4guiImageProcessingUtils::geodesicLevelSet(initialization, edgeImage, propagation, advection, curvature, iterations);

  std::cout << "Storing image\n";
  storeImage(itkImage);
#endif
}

//--------------
// runThreshold
//--------------
//
void sv4guiImageProcessing::runThreshold()
{
#ifdef use_sv4guiImageProcessing_runThreshold()
  std::cout << " Threshold button clicked\n";
  double upperThreshold = std::stod(ui->thresholdUpperLineEdit->text().toStdString());
  double lowerThreshold = std::stod(ui->thresholdLowerLineEdit->text().toStdString());
  std::cout << "upper,lower threshold: " << upperThreshold << ", " << lowerThreshold << "\n";

  sv4guiImageProcessingUtils::itkImPoint itkImage = getItkImage(0);

  if (!itkImage){
    MITK_ERROR << "No image 1 selected, please select an image 1\n";
    return;
  }
  std::cout << "Running threshold\n";
  itkImage = sv4guiImageProcessingUtils::threshold(itkImage, lowerThreshold, upperThreshold);

  std::cout << "Storing image\n";
  storeImage(itkImage);
#endif
}

void sv4guiImageProcessing::runBinaryThreshold()
{
#ifdef use_sv4guiImageProcessing_runBinaryThreshold
  std::cout << " binary Threshold button clicked\n";
  double upperThreshold =
    std::stod(ui->binaryThresholdUpperLineEdit->text().toStdString());
  double lowerThreshold =
    std::stod(ui->binaryThresholdLowerLineEdit->text().toStdString());
  double outsideValue =
    std::stod(ui->binaryThresholdOutsideLineEdit->text().toStdString());
  double insideValue =
    std::stod(ui->binaryThresholdInsideLineEdit->text().toStdString());

  std::cout << "upper,lower threshold: " << upperThreshold << ", " << lowerThreshold << "\n";

  sv4guiImageProcessingUtils::itkImPoint itkImage = getItkImage(0);

  if (!itkImage){
    MITK_ERROR << "No image 1 selected, please select an image 1\n";
    return;
  }
  std::cout << "Running threshold\n";
  itkImage = sv4guiImageProcessingUtils::binaryThreshold(itkImage, lowerThreshold, upperThreshold, insideValue, outsideValue);

  std::cout << "Storing image\n";
  storeImage(itkImage);
#endif
}

void sv4guiImageProcessing::runEditImage()
{
#ifdef sv4guiImageProcessing_runEditImage
  std::cout << " Edit Image button clicked\n";

  sv4guiImageProcessingUtils::itkImPoint itkImage = getItkImage(0);

  if (!itkImage){
    MITK_ERROR << "No image 1 selected, please select an image 1\n";
    return;
  }
  double replaceValue = std::stod(ui->editImageReplaceValueLineEdit->text().toStdString());

  int startSeeds = m_SeedContainer->GetNumStartSeeds();
  if (startSeeds == 0) return;

  for (int s = 0; s < startSeeds; s++){
    int endSeeds = m_SeedContainer->GetNumEndSeeds(s);
    if (endSeeds == 0) break;

    auto v_start = m_SeedContainer->GetStartSeed(s);

    for (int e = 0; e < endSeeds; e++){
      std::cout << "seed " << s << ", " << e << "\n";
      auto v_end = m_SeedContainer->GetEndSeed(s,e);

      auto s_index = sv4guiImageProcessingUtils::physicalPointToIndex(
        itkImage, v_start[0], v_start[1], v_start[2]);
      auto e_index = sv4guiImageProcessingUtils::physicalPointToIndex(
        itkImage, v_end[0], v_end[1], v_end[2]);

      int ox = (s_index[0]+e_index[0])/2;
      int oy = (s_index[1]+e_index[1])/2;
      int oz = (s_index[2]+e_index[2])/2;

      int l = abs(s_index[0]-e_index[0]);
      int w = abs(s_index[1]-e_index[1]);
      int h = abs(s_index[2]-e_index[2]);

      std::cout << "Running edit image\n";
      itkImage = sv4guiImageProcessingUtils::editImage(itkImage, ox, oy, oz, l, w, h, replaceValue);
    }
  }
  std::cout << "Storing image\n";
  storeImage(itkImage);
#endif
}

void sv4guiImageProcessing::runCropImage()
{
#ifdef use_sv4guiImageProcessing_runCropImage
  std::cout << " Crop Image button clicked\n";

  //point 1
  int x1 =
    std::stoi(ui->cropImageX1LineEdit->text().toStdString());
  int y1 =
    std::stoi(ui->cropImageY1LineEdit->text().toStdString());
  int z1 =
    std::stoi(ui->cropImageZ1LineEdit->text().toStdString());

  //point 2
  int x2 =
    std::stoi(ui->cropImageX2LineEdit->text().toStdString());
  int y2 =
    std::stoi(ui->cropImageY2LineEdit->text().toStdString());
  int z2 =
    std::stoi(ui->cropImageZ2LineEdit->text().toStdString());

  int ox = (x1+x2)/2;
  int oy = (y1+y2)/2;
  int oz = (z1+z2)/2;

  int l = abs(x1-x2);
  int w = abs(y1-y2);
  int h = abs(z1-z2);

  sv4guiImageProcessingUtils::itkImPoint itkImage = getItkImage(0);

  sv4guiImageProcessingUtils::itkImageType::RegionType region = itkImage->GetLargestPossibleRegion();

  sv4guiImageProcessingUtils::itkImageType::SizeType size = region.GetSize();

  std::cout << size << std::endl;

  if (!itkImage){
    MITK_ERROR << "No image 1 selected, please select an image 1\n";
    return;
  }
  std::cout << "Crop image\n";

  itkImage = sv4guiImageProcessingUtils::cropImage(itkImage, ox, oy, oz, l, w, h);

  std::cout << "Storing image\n";
  storeImage(itkImage);
#endif
}

//-------------------------
// CombinedCollidingFronts
//-------------------------
// Compute a new image using colliding fronts and combine it with 
// the imput image. 
//
// Returns an mikt::itkImgeType. 
//
sv4guiImageProcessingUtils::itkImPoint 
sv4guiImageProcessing::CombinedCollidingFronts(sv4guiImageProcessingUtils::itkImPoint itkImage, double lower, double upper)
{
  bool min_init = false;
  auto minImage = sv4guiImageProcessingUtils::copyImage(itkImage);

  int startSeeds = m_SeedContainer->GetNumStartSeeds();
  if (startSeeds == 0) return NULL;

  for (int i = 0; i < startSeeds; i++) {
    int endSeeds = m_SeedContainer->GetNumEndSeeds(i);
    if (endSeeds == 0) {
      break;
    }

    auto startPoint = m_SeedContainer->GetStartSeed(i);

    for (int j = 0; j < endSeeds; j++){
      auto endPoint = m_SeedContainer->GetEndSeed(i, j);
      auto startIndex = sv4guiImageProcessingUtils::physicalPointToIndex(itkImage, startPoint[0], startPoint[1], startPoint[2]);
      auto endIndex = sv4guiImageProcessingUtils::physicalPointToIndex(itkImage, endPoint[0], endPoint[1], endPoint[2]);
      auto temp_im = sv4guiImageProcessingUtils::collidingFronts(itkImage, startIndex[0], startIndex[1], startIndex[2],
                       endIndex[0], endIndex[1], endIndex[2], lower, upper);

      if (min_init) {
        minImage = sv4guiImageProcessingUtils::elementwiseMinimum(minImage, temp_im);
      } else {
        min_init = true;
        minImage = temp_im;
      }
    }
  }

  if (min_init) {
    return minImage;
  } else {
    return NULL;
  }
}

//--------------------
// runCollidingFronts
//--------------------
//
void sv4guiImageProcessing::runCollidingFronts()
{

#ifdef use_sv4guiImageProcessing_runCollidingFronts()
  std::cout << "========== sv4guiImageProcessing::runCollidingFronts ========== " << std::endl;

  int startSeeds = m_SeedContainer->GetNumStartSeeds();
  std::cout << "[runCollidingFronts] Number of start seeds: " << startSeeds << std::endl;

  if (startSeeds == 0) {
    return;
  }

  //threshold
  double lower = std::stod(ui->CFLowerLineEdit->text().toStdString());
  double upper = std::stod(ui->CFUpperLineEdit->text().toStdString());
  std::cout << "[runCollidingFronts] Lower threshold: " << lower << std::endl;
  std::cout << "[runCollidingFronts] Upper threshold: " << upper << std::endl;

  sv4guiImageProcessingUtils::itkImPoint itkImage = getItkImage(0);

  if (!itkImage){
    MITK_ERROR << "No image 1 selected, please select an image 1\n";
    return;
  }
  std::cout << "colliding fronts\n";

  auto minImage = CombinedCollidingFronts(itkImage,lower,upper);


  std::cout << "Storing image\n";
  storeImage(minImage);
#endif
}

//------------------------
// runFullCollidingFronts
//------------------------
//
void sv4guiImageProcessing::runFullCollidingFronts()
{
  #ifdef dbg_sv4guiImageProcessing_runFullCollidingFronts
  std::cout << "========== sv4guiImageProcessing::runFullCollidingFronts ========== " << std::endl;
  #endif

  int startSeeds = m_SeedContainer->GetNumStartSeeds();
  #ifdef dbg_sv4guiImageProcessing_runFullCollidingFronts
  std::cout << "[runCollidingFronts] Number of start seeds: " << startSeeds << std::endl;
  #endif
  if (startSeeds == 0) {
    QMessageBox::warning(NULL, "", "No seeds have been selected.");
    return;
  }

  // Get threshold values. 
  //
  auto lowerStr = ui->fullCFLowerThresholdLineEdit->text().toStdString();
  auto upperStr = ui->fullCFUpperThresholdLineEdit->text().toStdString();

  if (lowerStr == "") {
    QMessageBox::warning(NULL, "", "A lower threshold value must be specified.");
    return;
  }

  if (upperStr == "") {
    QMessageBox::warning(NULL, "", "An upper threshold value must be specified.");
    return;
  }

  double lower = std::stod(lowerStr);
  double upper = std::stod(upperStr);

  if (lower >= upper) {
    QMessageBox::warning(NULL, "", "The upper threshold value must be larger than the lower threshold value.");
    return;
  }

  sv4guiImageProcessingUtils::itkImPoint itkImage = getItkImage(0);

  if (!itkImage){
    MITK_ERROR << "No image 1 selected, please select an image 1\n";
    return;
  }

  // Initialize the level set image.
  auto minImage = CombinedCollidingFronts(itkImage, lower, upper);

  // Compute the magnitude of the image gradient and transform the image intensities in [0.0, 1.0].
  double sigma = ui->fullCFGradientLineEdit->value();
  auto gradImage = sv4guiImageProcessingUtils::gradientMagnitude(itkImage, sigma);

  double propagation = ui->fullCFPropagationLineEdit->value();
  double advection = ui->fullCFAdvectionLineEdit->value();
  double curvature = ui->fullCFCurvatureLineEdit->value();
  int numIterations = std::stoi(ui->fullCFIterationsLineEdit->text().toStdString());

  if (!gradImage || !minImage){
    MITK_ERROR << "Error in gradient image or colliding fronts image\n";
    QMessageBox::critical(NULL, "", "The colliding fronts computation has failed.");
    return;
  }

  // Compute the level set segmentation.
  auto lsImage = sv4guiImageProcessingUtils::geodesicLevelSet(minImage, gradImage, propagation, advection, 
      curvature, numIterations);

  // Extract an isosurface.
  double isovalue = std::stod(ui->fullCFIsoValueLineEdit->text().toStdString());
  vtkSmartPointer<vtkImageData> vtkImage = sv4guiImageProcessingUtils::itkImageToVtkImage(lsImage);
  vtkSmartPointer<vtkPolyData> vtkPd = sv4guiImageProcessingUtils::marchingCubes(vtkImage, isovalue, false);

  // Save the isosurface.
  storePolyData(vtkPd);
}

//------------------
// runResampleImage
//------------------
//
void sv4guiImageProcessing::runResampleImage()
{
#ifdef use_sv4guiImageProcessing_runResampleImage
  std::cout << " Resample Image button clicked\n";

  double space_x =
    std::stod(ui->resampleImageXLineEdit->text().toStdString());
  double space_y =
    std::stod(ui->resampleImageYLineEdit->text().toStdString());
  double space_z =
    std::stod(ui->resampleImageZLineEdit->text().toStdString());

  sv4guiImageProcessingUtils::itkImPoint itkImage = getItkImage(0);

  if (!itkImage){
    MITK_ERROR << "No image 1 selected, please select an image 1\n";
    return;
  }
  std::cout << "Running resample image\n";
  itkImage = sv4guiImageProcessingUtils::resampleImage(itkImage, space_x, space_y, space_z);

  std::cout << "Storing image\n";
  storeImage(itkImage);
#endif
}

void sv4guiImageProcessing::runZeroLevel()
{
#ifdef use_sv4guiImageProcessing_runZeroLevel
  std::cout << " ZeroLevel button clicked\n";

  double pixelValue =
    std::stod(ui->zeroLevelLineEdit->text().toStdString());

  sv4guiImageProcessingUtils::itkImPoint itkImage = getItkImage(0);

  if (!itkImage){
    MITK_ERROR << "No image 1 selected, please select an image 1\n";
    return;
  }
  std::cout << "Running zero level\n";
  itkImage = sv4guiImageProcessingUtils::zeroLevel(itkImage, pixelValue);

  std::cout << "Storing image\n";
  storeImage(itkImage);
#endif
}

void sv4guiImageProcessing::runGradientMagnitude()
{
#ifdef use_sv4guiImageProcessing_runGradientMagnitude()
  std::cout << " GradientMagnitude button clicked\n";

  double sigma =
    std::stod(ui->gradientMagnitudeLineEdit->text().toStdString());

  sv4guiImageProcessingUtils::itkImPoint itkImage = getItkImage(0);

  if (!itkImage){
    MITK_ERROR << "No image 1 selected, please select an image 1\n";
    return;
  }
  std::cout << "Running gradient magnitude\n";
  itkImage = sv4guiImageProcessingUtils::gradientMagnitude(itkImage, sigma);

  std::cout << "Storing image\n";
  storeImage(itkImage);
#endif
}

void sv4guiImageProcessing::runSmoothing()
{
#ifdef use_sv4guiImageProcessing_runSmoothing
  std::cout << " smoothin button clicked\n";

  double sigma = std::stod(ui->smoothLineEdit->text().toStdString());

  sv4guiImageProcessingUtils::itkImPoint itkImage = getItkImage(0);

  if (!itkImage){
    MITK_ERROR << "No image 1 selected, please select an image 1\n";
    return;
  }
  std::cout << "Running smoothing\n";
  itkImage = sv4guiImageProcessingUtils::smooth(itkImage, sigma);

  std::cout << "Storing image\n";
  storeImage(itkImage);
#endif
}

void sv4guiImageProcessing::runAnisotropic()
{
#ifdef use_sv4guiImageProcessing_runAnisotropic
  std::cout << " anisotropic button clicked\n";

  int iterations =
    std::stod(ui->anisotropicIterationsLineEdit->text().toStdString());
  double timeStep =
    std::stod(ui->anisotropicTimeStepLineEdit->text().toStdString());
  double conductance =
    std::stod(ui->anisotropicConductanceLineEdit->text().toStdString());

  sv4guiImageProcessingUtils::itkImPoint itkImage = getItkImage(0);

  if (!itkImage){
    MITK_ERROR << "No image 1 selected, please select an image 1\n";
    return;
  }
  std::cout << "Running anisotropic\n";
  itkImage = sv4guiImageProcessingUtils::anisotropicSmooth(itkImage, iterations, timeStep, conductance);

  std::cout << "Storing image\n";
  storeImage(itkImage);
#endif
}

//-------------
// runIsovalue
//-------------
//
void sv4guiImageProcessing::runIsovalue()
{
#ifdef use_sv4guiImageProcessing_runIsovalue
  std::cout << "Extracting IsoValue\n";
  double isovalue =
    std::stod(ui->marchingCubesLineEdit->text().toStdString());

  bool largest_cc = ui->isoCheckBox->isChecked();

  auto itkImage = getItkImage(0);
  vtkSmartPointer<vtkImageData> vtkImage = sv4guiImageProcessingUtils::itkImageToVtkImage(itkImage);

  vtkSmartPointer<vtkPolyData> vtkPd     = sv4guiImageProcessingUtils::marchingCubes(vtkImage, isovalue, largest_cc);
  storePolyData(vtkPd);
#endif
}
