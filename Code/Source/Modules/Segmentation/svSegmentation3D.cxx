#include <svSegmentation3D.h>
#include <iostream>
#include <itkImage.h>
#include <itkVTKImageToImageFilter.h>
#include <itkThresholdImageFilter.h>
#include <itkRescaleIntensityImageFilter.h>
#include <itkCollidingFrontsImageFilter.h>
#include <itkImageFileReader.h>
#include <itkImageFileWriter.h>
#include <itkCastImageFilter.h>
#include <itkImageToVTKImageFilter.h>
#include <itkImageFileWriter.h>
#include <vtkMarchingCubes.h>
#include <vtkPoints.h>
#include <vtkPolyData.h>
#include <vtkPolyDataWriter.h>
#include <vtkImageData.h>
#include <vtkMetaImageReader.h>
#include <vtkImageCast.h>
#include <fstream>
#include <vector>
#include <vtkSmartPointer.h>

svSegmentation3D::svSegmentation3D()
{
}

svSegmentation3D::~svSegmentation3D()
{
}


vtkSmartPointer<vtkPolyData> svSegmentation3D::collidingFronts(vtkImageData* volumeImage,
  std::vector<std::vector<int>>& seeds1,
  std::vector<std::vector<int>>& seeds2,
  int lowerThreshold, int upperThreshold)
{

  typedef itk::Image<short int,3> InputType;
  typedef itk::Image<float,3> CFImageType;

  auto caster = vtkSmartPointer<vtkImageCast>::New();
  caster->SetInputData(volumeImage);
  caster->SetOutputScalarTypeToFloat();
  caster->Update();


  //Colliding fronts needs an itk image, so we first convert vtk image to itk
  auto VTKFilter = itk::VTKImageToImageFilter<CFImageType>::New();
  VTKFilter->SetInput(caster->GetOutput());
  VTKFilter->Update();

  auto itkImage = VTKFilter->GetOutput();

  auto thresh = itk::ThresholdImageFilter<CFImageType>::New();

  //check if we need to threshold the image
  if ((lowerThreshold != upperThreshold) && (lowerThreshold < upperThreshold)){

    thresh->SetInput(itkImage);
    thresh->ThresholdOutside(lowerThreshold,upperThreshold);
    thresh->SetOutsideValue(0.0);
    thresh->Update();
    itkImage = thresh->GetOutput();
  }

  //finally CF needs pixel values to be between 0 and 1, so we rescale the image
  auto scaler = itk::RescaleIntensityImageFilter<CFImageType,CFImageType>::New();
  scaler->SetInput(itkImage);
  scaler->SetOutputMinimum(0.0);
  scaler->SetOutputMaximum(1.0);
  scaler->Update();

  //now construct collidingfronts filter
  typedef itk::CollidingFrontsImageFilter<CFImageType,CFImageType> CFType;
  auto CF = CFType::New();

  //Colliding fronts expects seeds as level set nodes in a node container,
  //so we convert the input seed vectors to this type
  typedef CFType::NodeContainer NodeContainer;
  typedef CFType::NodeType NodeType;

  //create node container for starting seed points
  auto seedContainer1 = NodeContainer::New();
  seedContainer1->Initialize();
  for (int i = 0; i < seeds1.size(); i++){

    auto p = seeds1[i];
    CFImageType::IndexType index;

    for (int j = 0; j < 3; j++){
      index[j] = p[j];
    }

    NodeType n;
    n.SetIndex(index);
    n.SetValue(0.0);
    seedContainer1->InsertElement(i,n);
  }

  //create node container for end seed points
  auto seedContainer2 = NodeContainer::New();
  seedContainer2->Initialize();
  for (int i = 0; i < seeds2.size(); i++){

    auto p = seeds2[i];
    CFImageType::IndexType index;

    for (int j = 0; j < 3; j++){
      index[j] = p[j];
    }

    NodeType n;
    n.SetIndex(index);
    n.SetValue(0.0);
    seedContainer2->InsertElement(i,n);
  }

  //now set the inputs for the colliding fronts filter
  CF->SetInput(scaler->GetOutput());
  CF->SetSeedPoints1(seedContainer1);
  CF->SetSeedPoints2(seedContainer2);
  // CF->ApplyConnectivityOn();
  CF->StopOnTargetsOn();
  CF->Update();

  //convert CF segmentation to binary image
  //CF sets everything inside to negative, everything outside to 0
  thresh->SetInput(CF->GetOutput());
  thresh->ThresholdBelow(0);
  thresh->SetOutsideValue(1.0);
  thresh->Update();

  //now convert back to vtk image
  auto itk_to_vtk = itk::ImageToVTKImageFilter<CFImageType>::New();
  itk_to_vtk->SetInput(thresh->GetOutput());
  itk_to_vtk->Update();

  //Extract a segmentation from the image
  auto MC = vtkSmartPointer<vtkMarchingCubes>::New();
  MC->SetInputData(itk_to_vtk->GetOutput());
  MC->SetValue(0,0.5);
  MC->Update();

  auto pd = MC->GetOutput();
  return pd;
}
