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


#include <usModuleRegistry.h>
#include <usGetModuleContext.h>
#include <usModule.h>
#include <usModuleContext.h>
#include <QInputDialog>
#include <QMessageBox>

const QString sv4guiImageProcessing::EXTENSION_ID = "org.sv.views.imageprocessing";

sv4guiImageProcessing::sv4guiImageProcessing() :
  ui(new Ui::sv4guiImageProcessing)
{

}

sv4guiImageProcessing::~sv4guiImageProcessing(){
  delete ui;
}

void sv4guiImageProcessing::CreateQtPartControl(QWidget *parent){
  m_Parent=parent;
  ui->setupUi(parent);

  m_DisplayWidget=GetActiveStdMultiWidget();

  if(m_DisplayWidget==NULL)
  {
      parent->setEnabled(false);
      MITK_ERROR << "Plugin ImageProcessing Init Error: No QmitkStdMultiWidget!";
      return;
  }

  connect(ui->seedLineEdit, SIGNAL(editingFinished()), this, SLOT(seedSize()));

  //more display stuff
  connect(ui->imageEditingToolbox, SIGNAL(currentChanged(int)),
    this, SLOT(imageEditingTabSelected()));
  connect(ui->filteringToolbox, SIGNAL(currentChanged(int)),
    this, SLOT(filteringTabSelected()));
  connect(ui->segmentationToolbox, SIGNAL(currentChanged(int)),
    this, SLOT(segmentationTabSelected()));
  connect(ui->pipelinesToolbox, SIGNAL(currentChanged(int)),
    this, SLOT(pipelinesTabSelected()));

  // getText();
  connect(ui->thresholdButton, SIGNAL(clicked()), this, SLOT(runThreshold()));
  connect(ui->binaryThresholdButton, SIGNAL(clicked()), this, SLOT(runBinaryThreshold()));
  connect(ui->editImageButton, SIGNAL(clicked()), this, SLOT(runEditImage()));
  connect(ui->cropImageButton, SIGNAL(clicked()), this, SLOT(runCropImage()));
  connect(ui->resampleImageButton, SIGNAL(clicked()), this, SLOT(runResampleImage()));
  connect(ui->zeroLevelButton, SIGNAL(clicked()), this, SLOT(runZeroLevel()));
  connect(ui->CFButton, SIGNAL(clicked()), this, SLOT(runCollidingFronts()));
  connect(ui->fullCFButton, SIGNAL(clicked()), this, SLOT(runFullCollidingFronts()));

  connect(ui->marchingCubesButton, SIGNAL(clicked()), this, SLOT(runIsovalue()));
  connect(ui->gradientMagnitudeButton, SIGNAL(clicked()), this, SLOT(runGradientMagnitude()));
  connect(ui->smoothButton, SIGNAL(clicked()), this, SLOT(runSmoothing()));
  connect(ui->anisotropicButton, SIGNAL(clicked()), this, SLOT(runAnisotropic()));
  connect(ui->levelSetButton, SIGNAL(clicked()), this, SLOT(runGeodesicLevelSet()));

  connect(ui->guideCheckBox, SIGNAL(clicked(bool)), this,
    SLOT(displayGuide(bool)));

  connect(ui->seedCheckBox, SIGNAL(clicked(bool)), this,
      SLOT(displaySeeds(bool)));

  m_Interface= new sv4guiDataNodeOperationInterface();

  if (m_init){
    std::cout << "Making seed node\n";
    m_init = false;
    m_SeedContainer = sv4guiImageSeedContainer::New();

    auto seedNode = mitk::DataNode::New();

    mitk::DataNode::Pointer image_folder_node =
      GetDataStorage()->GetNamedNode("Images");

    seedNode->SetData(m_SeedContainer);

    m_SeedMapper = sv4guiImageSeedMapper::New();
    m_SeedMapper->SetDataNode(seedNode);
    m_SeedMapper->m_box = false;

    seedNode->SetMapper(mitk::BaseRenderer::Standard3D, m_SeedMapper);
    //seedNode->SetMapper(mitk::BaseRenderer::Standard2D, seedMapper);

    m_SeedInteractor = sv4guiImageSeedInteractor::New();
    m_SeedInteractor->LoadStateMachine("seedInteraction.xml",
        us::ModuleRegistry::GetModule("sv4guiModuleImageProcessing"));
    m_SeedInteractor->SetEventConfig("seedConfig.xml",
        us::ModuleRegistry::GetModule("sv4guiModuleImageProcessing"));
    m_SeedInteractor->SetDataNode(seedNode);

    if (image_folder_node) image_folder_node->SetVisibility(true);

    seedNode->SetVisibility(false);
    mitk::RenderingManager::GetInstance()->RequestUpdateAll();

    seedNode->SetVisibility(true);
    mitk::RenderingManager::GetInstance()->RequestUpdateAll();

    m_SeedNode = seedNode;
    m_SeedNode->SetName("seeds");

  }
}

void sv4guiImageProcessing::displaySeeds(bool state){
  if (!state){
    GetDataStorage()->Remove(m_SeedNode);
  }else{
    auto image_folder_node = GetDataStorage()->GetNamedNode("Images");
    if(image_folder_node){
      GetDataStorage()->Add(m_SeedNode, image_folder_node);
    }
  }
  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void sv4guiImageProcessing::displayGuide(bool state){
  if (!state){
    ui->guideWidget->hide();
  }else{
    ui->guideWidget->show();
  }
}
/*******************************************************
* Display Buttons
********************************************************/
void sv4guiImageProcessing::imageEditingTabSelected(){
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

}

void sv4guiImageProcessing::filteringTabSelected(){
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

}

void sv4guiImageProcessing::segmentationTabSelected(){
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

}

void sv4guiImageProcessing::pipelinesTabSelected(){
  ui->edgeImageComboBox->setEnabled(false);
  m_SeedMapper->m_box = false;
  mitk::RenderingManager::GetInstance()->RequestUpdateAll();

}

void sv4guiImageProcessing::seedSize(){
  double seedSize =
    std::stod(ui->seedLineEdit->text().toStdString());

  m_SeedMapper->m_seedRadius = seedSize;

  m_SeedInteractor->m_seedRadius = seedSize;
  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}
/*******************************************************
* Everything Else
********************************************************/
void sv4guiImageProcessing::OnSelectionChanged(std::vector<mitk::DataNode*> nodes){
  UpdateImageList();
}

std::string sv4guiImageProcessing::getImageName(int imageIndex){
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

mitk::Image::Pointer sv4guiImageProcessing::getImage(std::string image_name){

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

  std::cout << "Got node with name " << image_node->GetName() << "\n";

  mitk::Image::Pointer image_data = dynamic_cast<mitk::Image*>(image_node->GetData());

  if (!image_data){
    MITK_ERROR << "Image data is null for image "<< image_name <<"\n";
    return NULL;
  }
  return image_data;
}

sv4guiImageProcessingUtils::itkImPoint sv4guiImageProcessing::getItkImage(int index){
  std::cout << "Running Image processing\n";

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
  std::cout << "done pixel type\n";

  std::cout << image_name << " selected\n";
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

  std::cout << "Casting to mitk image\n";
  std::string image_name = getImageName(0);
  mitkImage = getImage(image_name)->Clone();

  mitk::CastToMitkImage(image, mitkImage);
  newImageNode->SetData(mitkImage);

  std::cout << "Adding node\n";
  addNode(newImageNode, image_folder_node);

  UpdateImageList();
}

void sv4guiImageProcessing::storePolyData(vtkSmartPointer<vtkPolyData> vtkPd){
  bool ok;
  QString new_polydata_name = QInputDialog::getText(m_Parent, tr("New 3D Segmentation Name"),
                                       tr("Enter a name for the new 3D Segmentation"), QLineEdit::Normal,
                                       "", &ok);

  mitk::DataNode::Pointer polydata_folder_node = GetDataStorage()->GetNamedNode("Segmentations");

  if (!polydata_folder_node){
    MITK_ERROR << "No image folder found\n";
    return;
  }

  mitk::DataNode::Pointer newPdNode =
    GetDataStorage()->GetNamedNode(new_polydata_name.toStdString());

  if (newPdNode){
    QMessageBox::warning(NULL,"Segmentation Already exists","Please use a different segmentation name!");
    return;
  }
  if(!ok){
    return;
  }

  newPdNode = mitk::DataNode::New();

  newPdNode->SetName(new_polydata_name.toStdString());

  sv4guiSeg3D* newSeg3D = new sv4guiSeg3D();
  newSeg3D->SetVtkPolyData(vtkPd);

  sv4guiMitkSeg3D::Pointer mitkSeg3D = sv4guiMitkSeg3D::New();
  mitkSeg3D->SetSeg3D(newSeg3D);
  mitkSeg3D->SetDataModified();

  newPdNode->SetData(mitkSeg3D);

  std::cout << "Adding node\n";
  addNode(newPdNode, polydata_folder_node);
}

void sv4guiImageProcessing::addNode(mitk::DataNode::Pointer child_node, mitk::DataNode::Pointer parent_node){

  mitk::OperationEvent::IncCurrObjectEventId();

  bool undoEnabled=true;
  std::cout << "creating do op\n";
  sv4guiDataNodeOperation* doOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpADDDATANODE,
    GetDataStorage(),child_node, parent_node);

  if(undoEnabled)
  {
      std::cout << "creating undo op\n";
      sv4guiDataNodeOperation* undoOp = new sv4guiDataNodeOperation(
        sv4guiDataNodeOperation::OpREMOVEDATANODE,GetDataStorage(),child_node, parent_node);
      mitk::OperationEvent *operationEvent = new mitk::OperationEvent(
        m_Interface, doOp, undoOp, "Add DataNode");
      mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
  }
  std::cout << "Executin operationg\n";
  m_Interface->ExecuteOperation(doOp);
}

void sv4guiImageProcessing::UpdateImageList(){
  mitk::NodePredicateDataType::Pointer TypeCondition = mitk::NodePredicateDataType::New("Image");

  mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSubset(TypeCondition);

  if (rs->size() == 0){
    std::cout << "No images found, cannot create mask\n";
    return ;
  }
  ui->inputImageComboBox->clear();
  ui->edgeImageComboBox->clear();

  for (int i = 0; i < rs->size(); i++){
    mitk::DataNode::Pointer Node=rs->GetElement(i);
    std::cout << i << ": " << Node->GetName() << "\n";
    if ((Node->GetName() != "seeds")){
      ui->inputImageComboBox->addItem(Node->GetName().c_str());
      ui->edgeImageComboBox->addItem(Node->GetName().c_str());
    }
  }
}

void sv4guiImageProcessing::runGeodesicLevelSet(){

  double propagation =
    std::stod(ui->propagationLineEdit->text().toStdString());
  double advection =
    std::stod(ui->advectionLineEdit->text().toStdString());
  double curvature =
    std::stod(ui->curvatureLineEdit->text().toStdString());
  double iterations =
    std::stod(ui->iterationsLineEdit->text().toStdString());

  sv4guiImageProcessingUtils::itkImPoint initialization = getItkImage(0);
  sv4guiImageProcessingUtils::itkImPoint edgeImage = getItkImage(1);

  if (!initialization || !edgeImage){
    MITK_ERROR << "No image 1 or 2 selected, please select an image 1\n";
    return;
  }

  auto itkImage = sv4guiImageProcessingUtils::geodesicLevelSet(initialization, edgeImage, propagation, advection, curvature, iterations);

  std::cout << "Storing image\n";
  storeImage(itkImage);
}

void sv4guiImageProcessing::runThreshold(){
  std::cout << " Threshold button clicked\n";
  double upperThreshold =
    std::stod(ui->thresholdUpperLineEdit->text().toStdString());
  double lowerThreshold =
    std::stod(ui->thresholdLowerLineEdit->text().toStdString());
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
}

void sv4guiImageProcessing::runBinaryThreshold(){
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
}

void sv4guiImageProcessing::runEditImage(){
  std::cout << " Edit Image button clicked\n";

  sv4guiImageProcessingUtils::itkImPoint itkImage = getItkImage(0);

  if (!itkImage){
    MITK_ERROR << "No image 1 selected, please select an image 1\n";
    return;
  }
  double replaceValue = std::stod(ui->editImageReplaceValueLineEdit->text().toStdString());

  int startSeeds = m_SeedContainer->getNumStartSeeds();
  if (startSeeds == 0) return;

  for (int s = 0; s < startSeeds; s++){
    int endSeeds = m_SeedContainer->getNumEndSeeds(s);
    if (endSeeds == 0) break;

    auto v_start = m_SeedContainer->getStartSeed(s);

    for (int e = 0; e < endSeeds; e++){
      std::cout << "seed " << s << ", " << e << "\n";
      auto v_end = m_SeedContainer->getEndSeed(s,e);

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
}

void sv4guiImageProcessing::runCropImage(){
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
}

sv4guiImageProcessingUtils::itkImPoint sv4guiImageProcessing::CombinedCollidingFronts(sv4guiImageProcessingUtils::itkImPoint itkImage,
double lower, double upper){

  bool min_init = false;
  auto minImage = sv4guiImageProcessingUtils::copyImage(itkImage);

  int startSeeds = m_SeedContainer->getNumStartSeeds();
  if (startSeeds == 0) return NULL;

  for (int s = 0; s < startSeeds; s++){
    int endSeeds = m_SeedContainer->getNumEndSeeds(s);
    if (endSeeds == 0) break;

    auto v_start = m_SeedContainer->getStartSeed(s);

    for (int e = 0; e < endSeeds; e++){
      std::cout << "seed " << s << ", " << e << "\n";
      auto v_end = m_SeedContainer->getEndSeed(s,e);

      std::cout << "executing colliding fronts\n";
      auto s_index = sv4guiImageProcessingUtils::physicalPointToIndex(
        itkImage, v_start[0], v_start[1], v_start[2]);
      auto e_index = sv4guiImageProcessingUtils::physicalPointToIndex(
        itkImage, v_end[0], v_end[1], v_end[2]);

      std::cout << s_index[0] << ", " << e_index[0] << "\n";

      auto temp_im = sv4guiImageProcessingUtils::collidingFronts(itkImage,
        s_index[0], s_index[1], s_index[2],
        e_index[0], e_index[1], e_index[2],
        lower, upper);

      std::cout << "taking image minimum\n";
      if (min_init){
        minImage = sv4guiImageProcessingUtils::elementwiseMinimum(minImage, temp_im);
      }else {
        min_init = true;
        minImage = temp_im;
      }
    }
  }
  if (min_init){
    return minImage;
  }else{
    return NULL;
  }
}

void sv4guiImageProcessing::runCollidingFronts(){
  std::cout << " Colliding fronts button clicked\n";

  int startSeeds = m_SeedContainer->getNumStartSeeds();
  if (startSeeds == 0) return;

  //threshold
  double lower = std::stod(ui->CFLowerLineEdit->text().toStdString());
  double upper = std::stod(ui->CFUpperLineEdit->text().toStdString());

  sv4guiImageProcessingUtils::itkImPoint itkImage = getItkImage(0);

  if (!itkImage){
    MITK_ERROR << "No image 1 selected, please select an image 1\n";
    return;
  }
  std::cout << "colliding fronts\n";

  auto minImage = CombinedCollidingFronts(itkImage,lower,upper);


  std::cout << "Storing image\n";
  storeImage(minImage);
}

void sv4guiImageProcessing::runFullCollidingFronts(){
  std::cout << " Colliding fronts button clicked\n";

  /*****************
  colliding fronts
  *****************/
  int startSeeds = m_SeedContainer->getNumStartSeeds();
  if (startSeeds == 0) return;

  //threshold
  double lower = std::stod(ui->fullCFLowerThresholdLineEdit->text().toStdString());
  double upper = std::stod(ui->fullCFUpperThresholdLineEdit->text().toStdString());

  sv4guiImageProcessingUtils::itkImPoint itkImage = getItkImage(0);

  if (!itkImage){
    MITK_ERROR << "No image 1 selected, please select an image 1\n";
    return;
  }
  std::cout << "colliding fronts\n";

  auto minImage = CombinedCollidingFronts(itkImage,lower,upper);

  /*****************
  gradient magnitude
  *****************/
  double sigma =
    std::stod(ui->fullCFGradientLineEdit->text().toStdString());

  std::cout << "Running gradient magnitude\n";
  auto gradImage = sv4guiImageProcessingUtils::gradientMagnitude(itkImage, sigma);

  /****************
  Level Set
  *****************/
  double propagation =
    std::stod(ui->fullCFPropagationLineEdit->text().toStdString());
  double advection =
    std::stod(ui->fullCFAdvectionLineEdit->text().toStdString());
  double curvature =
    std::stod(ui->fullCFCurvatureLineEdit->text().toStdString());
  double iterations =
    std::stod(ui->fullCFIterationsLineEdit->text().toStdString());

  if (!gradImage || !minImage){
    MITK_ERROR << "Error in gradient image or colliding fronts image\n";
    return;
  }

  auto lsImage = sv4guiImageProcessingUtils::geodesicLevelSet(minImage, gradImage,
     propagation, advection, curvature, iterations);

  /****************
  IsoValue
  *****************/
  double isovalue =
    std::stod(ui->fullCFIsoValueLineEdit->text().toStdString());

  vtkSmartPointer<vtkImageData> vtkImage =
    sv4guiImageProcessingUtils::itkImageToVtkImage(lsImage);
  vtkSmartPointer<vtkPolyData> vtkPd     =
    sv4guiImageProcessingUtils::marchingCubes(vtkImage, isovalue, false);
  storePolyData(vtkPd);
}

void sv4guiImageProcessing::runResampleImage(){
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
}

void sv4guiImageProcessing::runZeroLevel(){
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
}

void sv4guiImageProcessing::runGradientMagnitude(){
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
}

void sv4guiImageProcessing::runSmoothing(){
  std::cout << " smoothin button clicked\n";

  double sigma =
    std::stod(ui->smoothLineEdit->text().toStdString());

  sv4guiImageProcessingUtils::itkImPoint itkImage = getItkImage(0);

  if (!itkImage){
    MITK_ERROR << "No image 1 selected, please select an image 1\n";
    return;
  }
  std::cout << "Running smoothing\n";
  itkImage = sv4guiImageProcessingUtils::smooth(itkImage, sigma);

  std::cout << "Storing image\n";
  storeImage(itkImage);
}

void sv4guiImageProcessing::runAnisotropic(){
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
}

void sv4guiImageProcessing::runIsovalue(){
  std::cout << "Extracting IsoValue\n";
  double isovalue =
    std::stod(ui->marchingCubesLineEdit->text().toStdString());

  bool largest_cc = ui->isoCheckBox->isChecked();

  auto itkImage = getItkImage(0);
  vtkSmartPointer<vtkImageData> vtkImage = sv4guiImageProcessingUtils::itkImageToVtkImage(itkImage);

  vtkSmartPointer<vtkPolyData> vtkPd     = sv4guiImageProcessingUtils::marchingCubes(vtkImage, isovalue, largest_cc);
  storePolyData(vtkPd);
}
