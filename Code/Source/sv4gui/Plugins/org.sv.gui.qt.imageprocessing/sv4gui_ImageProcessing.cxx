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

// [DaveP] There was a lot of functionality here to do a bunch of unknown operations
// that did not seem important. Thus all other functionality besides the colliding fronts 
// level set computation has been disabled.
//
// Some of the functionality may turn out to be useful in future so leave it in for now.

#include "SimVascular.h"

#include "math.h"
#ifndef M_PI
    #define M_PI 3.14159265358979323846
#endif

#include "sv4gui_ImageProcessing.h"
#include "sv4gui_ImageProcessingUtils.h"
#include "ui_sv4gui_ImageProcessing.h"
#include "sv4gui_VtkUtils.h"
#include <sv4gui_ImageSeedMapper.h>
#include <sv4gui_ImageSeedMapper2D.h>
#include <sv4gui_ImageSeedInteractor.h>
#include <sv4gui_PathElement.h>
#include <sv4gui_Seg3D.h>
#include <sv4gui_MitkSeg3D.h>

#include <sv3_PathGroup.h>
#include <sv3_PathIO.h>
#include <sv3_PathUtils.h>

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
#include <vtkTransformPolyDataFilter.h>
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
#ifndef WIN32
  #include <dirent.h>
#endif
#include<regex>

// Set debugging directives.
#ifdef DEBUG 
  #define dbg_sv4guiImageProcessing_ExectuteLevelSet
#endif

// Set reading paths and centerline data for development. 
//#define READ_DATA 

const QString sv4guiImageProcessing::EXTENSION_ID = "org.sv.views.imageprocessing";

// Set the names used to create for SV Data Manager nodes.
const std::string sv4guiImageProcessing::CENTERLINES_NODE_NAME = "centerlines";
const std::string sv4guiImageProcessing::LEVEL_SET_NODE_NAME = "level-set";
const std::string sv4guiImageProcessing::PATHS_NODE_NAME = "paths";
const std::string sv4guiImageProcessing::SEED_POINTS_NODE_NAME = "seed-points";
const std::string sv4guiImageProcessing::SURFACE_NODE_NAME = "surface";

const std::string sv4guiImageProcessing::CENTERLINES_FILE_NAME = "centerlines.vtp";
const std::string sv4guiImageProcessing::PATH_FILE_NAME = "path_";
const std::string sv4guiImageProcessing::PATH_FILE_EXTENSION = ".pth";
const std::string sv4guiImageProcessing::PATH_FILE_NAME_PATTERN = "path_[0-9]*.pth";   // regex used for reading path files.
const std::string sv4guiImageProcessing::SURFACE_FILE_NAME = "surface.vtp";

// Keyboard short cuts.
const std::string sv4guiImageProcessing::ADD_START_SEED_SHORT_CUT = "S";
const std::string sv4guiImageProcessing::ADD_END_SEED_SHORT_CUT = "E";

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
// Return the full path to the SV project Image/colliding-fronts
// directory.
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

  // Seed widgets.
  //
  // Set size.
  connect(ui->SD_Size_DoubleSpinBox, SIGNAL(editingFinished()), this, SLOT(SeedSize()));
  ui->SD_Size_DoubleSpinBox->setRange(0.0, 10.0);
  ui->SD_Size_DoubleSpinBox->setValue(1.0);

  connect(ui->SD_AddStart_PushButton, SIGNAL(clicked(bool)), this, SLOT(AddStartSeed()));
  connect(ui->SD_AddEnd_PushButton, SIGNAL(clicked(bool)), this, SLOT(AddEndSeed()));
  connect(ui->SD_Clear_PushButton, SIGNAL(clicked(bool)), this, SLOT(ClearSeeds()));

  // Add mouse key shortcuts for adding and deleting a seed points.
  ui->SD_AddStart_PushButton->setShortcut(QKeySequence(QString(ADD_START_SEED_SHORT_CUT.c_str())));
  ui->SD_AddEnd_PushButton->setShortcut(QKeySequence(QString(ADD_END_SEED_SHORT_CUT.c_str())));

  // Level set widgets. 
  //
  ui->LS_GradMag_DoubleSpinBox->setMinimum(0.0);
  ui->LS_PropScale_DoubleSpinBox->setRange(0.0, 1.0);
  ui->LS_AdvScale_DoubleSpinBox->setRange(0.0, 1.0);
  ui->LS_CurvScale_DoubleSpinBox->setRange(0.0, 1.0);

  auto numItersValidator = new QIntValidator(this);
  numItersValidator->setBottom(10);
  ui->LS_NumItLineEdit->setValidator(numItersValidator);

  connect(ui->LS_Execute_PushButton, SIGNAL(clicked()), this, SLOT(ExectuteLevelSet()));

  // Centerlines widgets.
  //
  connect(ui->SU_ComputeCenterlines_PushButton, SIGNAL(clicked(bool)), this, SLOT(ComputeCenterlines()));

  // Path widgets.
  auto distMultValidator = new QIntValidator(this);
  distMultValidator->setBottom(2);
  ui->PT_DistMult_LineEdit->setValidator(distMultValidator);
  ui->PT_DistMult_LineEdit->setText("10");
  ui->PT_Tangent_DoubleSpinBox->setRange(35.0, 60.0);
  connect(ui->PT_Extract_PushButton, SIGNAL(clicked()), this, SLOT(ExtractPaths()));

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
    m_CollidingFrontsNode->SetName(LEVEL_SET_NODE_NAME);
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

//--------------
// ExtractPaths
//--------------
// Extract paths from centerlines geometry.
//
// Sections of centerlines geometry are extract using centerlines and group
// ID data generated by the centerline computation.
//
// Path control points are then created for each section based on the number of
// desired control points in each section and curvature (i.e. the change in tangent
// along the section).
//
// Paths are then written to SV .pth XML format files.
//
void sv4guiImageProcessing::ExtractPaths()
{
  // Check if there is are centerlines.
  if (m_CenterlinesContainer.IsNull()) {
    QMessageBox::warning(NULL, "", "No centerlines have been created.");
    return;
  }

  // Get the image voxel dimensions.
  auto imageSpacing = GetImageSpacing();
  m_PathsContainer->SetImageSpacing(imageSpacing);
  double distMeasure = *std::max_element(imageSpacing.begin(), imageSpacing.end());

  // Create paths data nodes, m_PathsContainer, etc.
  InitializePaths();
  m_PathsContainer->ClearPathElements();

  // Set the mapper to update geometry.
  m_PathsMapper->SetUpdate(true);

  // Get input parameters and centerline geometry.
  int distMult = std::stoi(ui->PT_DistMult_LineEdit->text().toStdString());
  double tangentChangeDeg = ui->PT_Tangent_DoubleSpinBox->value();
  double tangentChange = cos((M_PI/180.0)*tangentChangeDeg);

  auto lines = m_CenterlinesContainer->GetLines();
  auto centerlines = vtkSmartPointer<vtkPolyData>::New();
  centerlines->DeepCopy(lines);
  
  // Extract disjoint sections from the centerlines based on centerline and group IDs.
  auto sections = sv3::PathUtils::ExtractCenterlinesSections(centerlines);

  // Remove old .path files.
  auto dirPath = m_PluginOutputDirectory.toStdString();
  QDir dir(m_PluginOutputDirectory, {"*.pth"});
  for (const QString & filename: dir.entryList()){
      dir.remove(filename);
  }

  // Extract path control points from the sections, create SV path elements from them
  // and write the path to an XML .pth file.
  int pathID = 1;
  std::vector<sv3::PathElement*> pathElements;
  for (auto& section : sections) {
      auto pathPoints = sv3::PathUtils::SampleLinePoints(section, distMult, tangentChange, distMeasure);

      // Create a path element used to store path geometry.
      auto pathElem = new sv3::PathElement();
      pathElem->SetMethod(sv3::PathElement::CONSTANT_TOTAL_NUMBER);
      pathElem->SetCalculationNumber(100);

      std::vector<std::array<double,3>> points;
      for (auto point : pathPoints) {
          points.push_back(point);
          //points.push_back(mitk::Point3D(pt[0], pt[1], pt[2]));
      }
      bool update = true;
      pathElem->SetControlPoints(points, update);
      //pathElempe->SetPathPoints(ConvertToPathPoints(posPoints));

      // Create a path group for writing a path file.
      int time = 0;
      sv3::PathGroup pathGroup;
      pathGroup.SetPathID(pathID);
      pathGroup.SetPathElement(pathElem, time);

      std::string fileName = dirPath + "/" + PATH_FILE_NAME + std::to_string(pathID) + PATH_FILE_EXTENSION; 
      sv3::PathIO().Write(fileName, &pathGroup);

      m_PathsContainer->AddPathElement(pathElem);
      pathID += 1;
  }

  m_PathsContainer->ComputeDistanceMeasure();

  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

//----------
// readData
//----------
//
void sv4guiImageProcessing::readData()
{
  // Create centerlines data nodes, setup interactor, etc.
  InitializeCenterlines();
  #ifdef READ_DATA 
      readCenterlines();
  #endif

  // Create paths data nodes.
  InitializePaths();
  #ifdef READ_DATA 
      readPaths();
  #endif

  // Read surface.
  //
  /* [DaveP] Not sure if I need to do this.
  std::string surfFileName = dirPath + "/surface.vtp";
  auto *surface = vtkPolyData::New();
  vtkSmartPointer<vtkXMLPolyDataReader> surfReader = vtkSmartPointer<vtkXMLPolyDataReader>::New();
  surfReader->SetFileName(surfFileName.c_str());
  surfReader->Update();
  surface->DeepCopy(reader->GetOutput());
  std::cout << "[readData] surface num points: " << surface->GetNumberOfPoints() << std::endl;
  std::cout << "[readData] surface num cells: " << surface->GetNumberOfCells() << std::endl;
  */

  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

//-----------
// readPaths 
//-----------
// Read in path files.
//
// [TODO:DaveP] not sure if we need this.
//
void sv4guiImageProcessing::readPaths()
{
#ifndef WIN32
  auto dirPath = m_PluginOutputDirectory.toStdString();
  std::string fileName = dirPath + "/" + PATH_FILE_NAME;

  DIR *dir = opendir(dirPath.c_str()); 
  struct dirent *dirEntry; 
  const std::regex pathRegex(PATH_FILE_NAME_PATTERN);

  while ((dirEntry = readdir(dir)) != nullptr) {
      std::string fileName(dirEntry->d_name);
  }
  
  closedir(dir);
#else
  std::cerr << "ERROR!!!! sv4guiImageProcessing::readPaths NOT DEFINED ON WIN32"; 
#endif
}

//-----------------
// readCenterlines 
//-----------------
// Read a centerlines file and set the m_CenterlinesContainer geometry.
//
void sv4guiImageProcessing::readCenterlines()
{
  auto dirPath = m_PluginOutputDirectory.toStdString();
  std::string fileName = dirPath + "/" + CENTERLINES_FILE_NAME;

  if (FILE *file = fopen(fileName.c_str(), "r")) {
      fclose(file);
  } else {
      return;
  }

  auto *centerlines = vtkPolyData::New();
  vtkSmartPointer<vtkXMLPolyDataReader> reader = vtkSmartPointer<vtkXMLPolyDataReader>::New();
  reader->SetFileName(fileName.c_str());
  reader->Update();
  centerlines->DeepCopy(reader->GetOutput());

  // Centerline geometry is stored in the container.
  m_CenterlinesContainer->SetLines(centerlines);
}

//--------------------
// ComputeCenterlines 
//--------------------
// Compute centerlines for the current surface.
//
void sv4guiImageProcessing::ComputeCenterlines()
{
  // A start seed must be defined. 
  int numStartSeeds = m_SeedContainer->GetNumStartSeeds();
  if (numStartSeeds < 1) {
    QMessageBox::warning(NULL,"","No start seeds have been defined.");
    return;
  }

  // An end seed must be defined. 
  if (m_SeedContainer->GetNumEndSeeds() == 0) { 
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

  // Find the closest node to the start seed.
  std::vector<int> sourceIDs;
  std::vector<int> targetIDs;

  auto startSeeds = m_SeedContainer->GetStartSeeds();
  for (auto& seed : startSeeds) { 
      auto startSeed = std::get<0>(seed.second);
      auto endSeeds = std::get<1>(seed.second);
      auto seedPoint = startSeed.point; 
      int min_id = FindClosesetPoint(segPolyData, seedPoint);
      sourceIDs.push_back(min_id);

      if (endSeeds.size() == 0) {
        break;
      }

      for (auto& endSeed : endSeeds) { 
          auto seedPoint = endSeed.point; 
          int min_id = FindClosesetPoint(segPolyData, seedPoint);
          targetIDs.push_back(min_id);
      }
    }

  // Compute centerlines.
  cvPolyData* centerlines = nullptr;
  cvPolyData* voronoiDst = nullptr;
  cvPolyData cvSurfPolydata(segPolyData);
  if (sys_geom_centerlines(&cvSurfPolydata, sourceIDs.data(), sourceIDs.size(), targetIDs.data(), targetIDs.size(), 
        &centerlines, &voronoiDst) != SV_OK) {
    QMessageBox::critical(NULL, "", "The centerline extraction computation has failed."); 
    return;
  }

  // Compute separate sections. 
  cvPolyData* separateCenterlines = nullptr;
  if (sys_geom_separatecenterlines(centerlines, &separateCenterlines) != SV_OK) {
    QMessageBox::critical(NULL, "", "The centerline extraction computation has failed."); 
    return;
  }

  // Merge separate sections. 
  cvPolyData* mergedCenterlines = nullptr;
  int mergeblanked = 1;
  if (sys_geom_mergecenterlines(separateCenterlines, mergeblanked, &mergedCenterlines) != SV_OK ) {
    QMessageBox::critical(NULL, "", "The centerline extraction computation has failed."); 
    return;
  }
  auto lines = mergedCenterlines->GetVtkPolyData();

  // Create centerlines data nodes, setup interactor, etc.
  InitializeCenterlines();

  // Centerline geometry is stored in the container.
  m_CenterlinesContainer->SetLines(lines);

  // Write the centerlines to a file.
  auto dirPath = m_PluginOutputDirectory.toStdString();
  std::string fileName = dirPath + "/" + CENTERLINES_FILE_NAME; 
  WritePolydata(fileName, lines);

  // [TODO:DaveP] something is crashing SV here.
  /*
  delete linesDst; 
  delete voronoiDst;
  delete splitCenterlines; 
  delete surfGrouped; 
  delete sections;
  delete splitCenterlines;
  */

  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

//-----------------------
// InitializeCenterlines
//-----------------------
//
void sv4guiImageProcessing::InitializeCenterlines()
{
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
      m_CenterlineInteractor = sv4guiImageCenterlineInteractor::New();
      m_CenterlineInteractor->LoadStateMachine("centerlineInteraction.xml", us::ModuleRegistry::GetModule("sv4guiModuleImageProcessing"));
      m_CenterlineInteractor->SetEventConfig("seedConfig.xml", us::ModuleRegistry::GetModule("sv4guiModuleImageProcessing"));
      m_CenterlineInteractor->SetDataNode(m_CenterlinesNode);
    }

    // This is needed so m_CenterlineInteractor receives events.
    m_CenterlinesNode->SetVisibility(true);
    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

//-----------------
// InitializePaths 
//-----------------
//
void sv4guiImageProcessing::InitializePaths()
{
  // Create a node and container for storing centerline geometry. 
  if (m_PathsNode.IsNull()) { 
      m_PathsNode = mitk::DataNode::New();
      m_PathsNode->SetName(PATHS_NODE_NAME);
      m_PathsContainer = sv4guiImagePathsContainer::New();
      m_PathsNode->SetData(m_PathsContainer);
      GetDataStorage()->Add(m_PathsNode, m_CollidingFrontsNode);
  }

  // Create mapper to display the centerlines.
  if (m_PathsMapper.IsNull()) { 
      m_PathsMapper = sv4guiImagePathsMapper::New();
      m_PathsMapper->SetDataNode(m_PathsNode);
      m_PathsMapper->SetColor(1.0, 1.0, 0.0);
      m_PathsNode->SetMapper(mitk::BaseRenderer::Standard3D, m_PathsMapper);
  }

    m_PathsNode->SetVisibility(true);
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
  mitk::Point3D point = m_DisplayWidget->GetCrossPosition();
  int numStartSeeds = m_SeedContainer->GetNumStartSeeds();
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
  mitk::Point3D point = m_DisplayWidget->GetCrossPosition();

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
/*
  ui->edgeImageComboBox->setEnabled(false);

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
/*
  ui->edgeImageComboBox->setEnabled(false);
  m_SeedMapper->m_box = false;

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
/*
  ui->edgeImageComboBox->setEnabled(true);
  m_SeedMapper->m_box = false;

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
  //ui->edgeImageComboBox->setEnabled(false);
  //m_SeedMapper->m_box = false;
  //mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

//----------
// SeedSize
//----------
//
void sv4guiImageProcessing::SeedSize()
{
  double seedSize = ui->SD_Size_DoubleSpinBox->value();
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
    imageName = ui->InputImage_ComboBox->currentText();
  }else if (imageIndex == 1){
    ///imageName = ui->edgeImageComboBox->currentText();
  }

  if (imageName.length() == 0){
    MITK_ERROR << "No image "<< imageIndex <<" selected, please select an image" << std::endl;
    return "";
  }

  std::string image_str = imageName.toStdString();
  return image_str;
}

//-----------------
// GetImageSpacing
//-----------------
//
std::array<double,3> 
sv4guiImageProcessing::GetImageSpacing()
{
  std::string image_name = getImageName(0);
  auto mitkImage = getImage(image_name);
  auto spacing = mitkImage->GetGeometry()->GetSpacing();
  return std::array<double,3> {spacing[0], spacing[1], spacing[2]};
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

  ui->InputImage_ComboBox->clear();
  //ui->edgeImageComboBox->clear();

  for (int i = 0; i < rs->size(); i++){
    mitk::DataNode::Pointer Node=rs->GetElement(i);
    if ((Node->GetName() != SEED_POINTS_NODE_NAME)){
      ui->InputImage_ComboBox->addItem(Node->GetName().c_str());
      //ui->edgeImageComboBox->addItem(Node->GetName().c_str());
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

  auto startSeeds = m_SeedContainer->GetStartSeeds();
  for (auto& seed : startSeeds) { 
    auto startSeed = std::get<0>(seed.second);
    auto endSeeds = std::get<1>(seed.second);
    if (endSeeds.size() == 0) {
      break;
    }

    auto startPoint = startSeed.point; 
    for (auto& endSeed : endSeeds) { 
      auto endPoint = endSeed.point; 
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
// Execute the colliding fronts level set computation.
//
void sv4guiImageProcessing::runCollidingFronts()
{

#ifdef use_sv4guiImageProcessing_runCollidingFronts
  std::cout << "========== sv4guiImageProcessing::runCollidingFronts ========== " << std::endl;

  int startSeeds = m_SeedContainer->GetNumStartSeeds();
  std::cout << "[runCollidingFronts] Number of start seeds: " << startSeeds << std::endl;

  if (startSeeds == 0) {
    QMessageBox::warning(NULL,"","No start seeds have been defined.");
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
// ExectuteLevelSet
//------------------------
// Execute the colliding fronts level set computation.
//
// The image may be transformed in sv4guiProjectManager::setTransformFromImageHeaderFile().
// This transforms image geometry (e.g. image slices) from its original coordinate
// system for display. 
//
// The image data does have direction cosines that match the image transformation but 
// vtkMarchingCubes() does not take this into account. The level set surface is thus
// in the original coordinate system so it must also be transformed by the image 
// transformation.
//
void sv4guiImageProcessing::ExectuteLevelSet()
{
  #ifdef dbg_sv4guiImageProcessing_ExectuteLevelSet
  std::cout << "========== sv4guiImageProcessing::ExectuteLevelSet ========== " << std::endl;
  #endif

  int startSeeds = m_SeedContainer->GetNumStartSeeds();
  #ifdef dbg_sv4guiImageProcessing_ExectuteLevelSet
  std::cout << "[runCollidingFronts] Number of start seeds: " << startSeeds << std::endl;
  #endif
  if (startSeeds == 0) {
    QMessageBox::warning(NULL, "", "No start seeds have been defined.");
    return;
  }

  // An end seed must be defined. 
  if (m_SeedContainer->GetNumEndSeeds() == 0) { 
    QMessageBox::warning(NULL,"","No end seeds have been defined.");
    return;
  } 

  // Get threshold values. 
  //
  auto upperThreshold = ui->LS_UpThresh_DoubleSpinBox->value();
  auto lowerThreshold = ui->LS_LowThresh_DoubleSpinBox->value();
  if (lowerThreshold >= upperThreshold) {
    QMessageBox::warning(NULL, "", "The upper threshold value must be larger than the lower threshold value.");
    return;
  }

  sv4guiImageProcessingUtils::itkImPoint itkImage = getItkImage(0);

  if (!itkImage){
    MITK_ERROR << "No image selected, please select an image.\n";
    return;
  }

  // Initialize the level set image.
  auto minImage = CombinedCollidingFronts(itkImage, lowerThreshold, upperThreshold);

  // Compute the magnitude of the image gradient and transform the image intensities in [0.0, 1.0].
  double sigma = ui->LS_GradMag_DoubleSpinBox->value();
  auto gradImage = sv4guiImageProcessingUtils::gradientMagnitude(itkImage, sigma);

  double propagation = ui->LS_PropScale_DoubleSpinBox->value();
  double advection = ui->LS_AdvScale_DoubleSpinBox->value();
  double curvature = ui->LS_CurvScale_DoubleSpinBox->value();
  int numIterations = std::stoi(ui->LS_NumItLineEdit->text().toStdString());

  if (!gradImage || !minImage){
    MITK_ERROR << "Error in gradient image or colliding fronts image\n";
    QMessageBox::critical(NULL, "", "The colliding fronts computation has failed.");
    return;
  }

  // Compute the level set segmentation.
  auto lsImage = sv4guiImageProcessingUtils::geodesicLevelSet(minImage, gradImage, propagation, advection, 
      curvature, numIterations);

  // Extract an isosurface.
  double isovalue = ui->LS_IsoLevel_DoubleSpinBox->value();
  auto vtkImage = sv4guiImageProcessingUtils::itkImageToVtkImage(lsImage);
  auto vtkPd = sv4guiImageProcessingUtils::marchingCubes(vtkImage, isovalue, false);

  // Transform the isosurface by the image transformation. 
  //
  auto direction_cos = lsImage->GetDirection();
  auto origin = lsImage->GetOrigin();
  auto matrix = vtkSmartPointer<vtkMatrix4x4>::New();
  matrix->Identity();
  for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 3; j++) {
        matrix->SetElement(i, j, direction_cos[i][j]);
    }
  }

  auto transform = vtkSmartPointer<vtkTransform>::New(); 
  transform->Translate(origin[0], origin[1], origin[2]);
  transform->Concatenate(matrix); 
  transform->Translate(-origin[0], -origin[1], -origin[2]);

  auto transformFilter = vtkSmartPointer<vtkTransformPolyDataFilter>::New(); 
  transformFilter->SetInputData(vtkPd); 
  transformFilter->SetTransform(transform); 
  transformFilter->Update(); 
  vtkSmartPointer<vtkPolyData> xform_vtkPd = transformFilter->GetOutput(); 

  // Save the isosurface.
  storePolyData(xform_vtkPd);
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
