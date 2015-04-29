/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson, Ken Wang.
 *
 * See SimVascular Acknowledgements file for additional
 * contributors to the source code. 
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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

// default includes
#include "SimVascular.h"

// itk includes

#ifdef ThreadInfoStruct
#undef ThreadInfoStruct
#endif

#include "itkImage.h"
// needed for random include for "ThreadInfo"
#include "itkExpNegativeImageFilter.h" 
#include "itkFastMarchingImageFilter.h"
#include "itkShapeDetectionLevelSetImageFilter.h"
#include "itkRegionOfInterestImageFilter.h"
#include "itkResampleImageFilter.h"
#include "itkAffineTransform.h"
#include "itkLinearInterpolateImageFunction.h"
#include "itkMinMaxCurvatureFlowImageFilter.h"
#include "itkSigmoidImageFilter.h"
#include "itkThresholdImageFilter.h"
#include "itkThresholdSegmentationLevelSetImageFilter.h"
#include "itkVTKImageImport.h"
#include "itkVTKImageExport.h"

// vtk includes
#include "vtkStructuredPoints.h"
#include "vtkImageExport.h"
#include "vtkImageImport.h"

// aspire2 includes
//#include "a2itk.h"

/**
 * This function will connect the given itk::VTKImageExport filter to
 * the given vtkImageImport filter.
 */
template <typename ITK_Exporter, typename VTK_Importer>
void ConnectPipelines(ITK_Exporter exporter, VTK_Importer* importer)
{
  importer->SetUpdateInformationCallback(exporter->GetUpdateInformationCallback());
  importer->SetPipelineModifiedCallback(exporter->GetPipelineModifiedCallback());
  importer->SetWholeExtentCallback(exporter->GetWholeExtentCallback());
  importer->SetSpacingCallback(exporter->GetSpacingCallback());
  importer->SetOriginCallback(exporter->GetOriginCallback());
  importer->SetScalarTypeCallback(exporter->GetScalarTypeCallback());
  importer->SetNumberOfComponentsCallback(exporter->GetNumberOfComponentsCallback());
  importer->SetPropagateUpdateExtentCallback(exporter->GetPropagateUpdateExtentCallback());
  importer->SetUpdateDataCallback(exporter->GetUpdateDataCallback());
  importer->SetDataExtentCallback(exporter->GetDataExtentCallback());
  importer->SetBufferPointerCallback(exporter->GetBufferPointerCallback());
  importer->SetCallbackUserData(exporter->GetCallbackUserData());
}


/**
 * This function will connect the given vtkImageExport filter to
 * the given itk::VTKImageImport filter.
 */
template <typename VTK_Exporter, typename ITK_Importer>
void ConnectPipelines(VTK_Exporter* exporter, ITK_Importer importer)
{
  importer->SetUpdateInformationCallback(exporter->GetUpdateInformationCallback());
  importer->SetPipelineModifiedCallback(exporter->GetPipelineModifiedCallback());
  importer->SetWholeExtentCallback(exporter->GetWholeExtentCallback());
  importer->SetSpacingCallback(exporter->GetSpacingCallback());
  importer->SetOriginCallback(exporter->GetOriginCallback());
  importer->SetScalarTypeCallback(exporter->GetScalarTypeCallback());
  importer->SetNumberOfComponentsCallback(exporter->GetNumberOfComponentsCallback());
  importer->SetPropagateUpdateExtentCallback(exporter->GetPropagateUpdateExtentCallback());
  importer->SetUpdateDataCallback(exporter->GetUpdateDataCallback());
  importer->SetDataExtentCallback(exporter->GetDataExtentCallback());
  importer->SetBufferPointerCallback(exporter->GetBufferPointerCallback());
  importer->SetCallbackUserData(exporter->GetCallbackUserData());
}

template <unsigned int Ta2itkDim, typename InputPixelType1, typename InputPixelType2, typename OutputPixelType>
a2itk<Ta2itkDim, InputPixelType1, typename InputPixelType2, typename OutputPixelType>::a2itk() {
    dimension_ = NULL;
    input1_  = NULL;
    input2_  = NULL;
    output1_ = NULL;
}

template <unsigned int Ta2itkDim, typename InputPixelType1, typename InputPixelType2, typename OutputPixelType>
a2itk<Ta2itkDim, InputPixelType1, typename InputPixelType2, typename OutputPixelType>::~a2itk() {
    // call destructors here??
    //input1_  = NULL;
    //input2_  = NULL;
    //output1_ = NULL;
}

template <unsigned int Ta2itkDim, typename InputPixelType1, typename InputPixelType2, typename OutputPixelType>
int a2itk<Ta2itkDim, InputPixelType1, typename InputPixelType2, typename OutputPixelType>::setInput1(vtkStructuredPoints *sp) {

  typedef itk::Image< InputPixelType1, Ta2itkDim >  InternalImageType;
  typedef itk::Image<InputPixelType1, Ta2itkDim> ImageType;
  typedef itk::VTKImageImport<ImageType> ImageImportType;
 
  vtkImageExport* vtkExporter = vtkImageExport::New();
  vtkExporter->SetInput(sp);

  ImageImportType::Pointer itkImporter = ImageImportType::New();
  ConnectPipelines(vtkExporter, itkImporter);

  itkImporter->Update();
  input1_ = itkImporter->GetOutput();

  return CV_OK;
}

template <unsigned int Ta2itkDim, typename InputPixelType1, typename InputPixelType2, typename OutputPixelType>
int a2itk<Ta2itkDim, InputPixelType1, typename InputPixelType2, typename OutputPixelType>::setInput2(vtkStructuredPoints *sp) {

  typedef itk::Image<InputPixelType2, Ta2itkDim> ImageType;
  typedef itk::VTKImageImport<ImageType> ImageImportType;
 
  vtkImageExport* vtkExporter = vtkImageExport::New();
  vtkExporter->SetInput(sp);

  ImageImportType::Pointer itkImporter = ImageImportType::New();
  ConnectPipelines(vtkExporter, itkImporter);

  itkImporter->Update();
  input2_ = itkImporter->GetOutput();

  return CV_OK;
}

template <unsigned int Ta2itkDim, typename InputPixelType1, typename InputPixelType2, typename OutputPixelType>
vtkStructuredPoints* a2itk<Ta2itkDim, InputPixelType1, typename InputPixelType2, typename OutputPixelType>::getOutput() {

  typedef itk::Image<typename OutputPixelType, Ta2itkDim> ImageType;
  typedef itk::VTKImageExport<ImageType> ImageExportType;

  ImageExportType::Pointer exporter = ImageExportType::New();

  exporter->SetInput( output1_ );

  vtkImageImport* importer = vtkImageImport::New();  
  ConnectPipelines(exporter, importer);

  vtkStructuredPoints *img;
  img = vtkStructuredPoints::New();
  importer->Update();
  img->DeepCopy(importer->GetOutput());

  // need to shift the origin like what used to be done
  // in vtkImageToStructuredPoints class

  int whole[6];
  double *tspacing, torigin[3];

  (importer->GetOutput())->GetWholeExtent(whole);
  tspacing = (importer->GetOutput())->GetSpacing();
  (importer->GetOutput())->GetOrigin(torigin);

  // slide min extent to 0,0,0 (I Hate this !!!!)
  //  this->Translate[0] = whole[0];
  //  this->Translate[1] = whole[2];
  //  this->Translate[2] = whole[4];
  
  torigin[0] += tspacing[0] * whole[0];
  torigin[1] += tspacing[1] * whole[2];
  torigin[2] += tspacing[2] * whole[4];
  whole[1] -= whole[0];
  whole[3] -= whole[2];
  whole[5] -= whole[4];
  whole[0] = whole[2] = whole[4] = 0;
  
  //std::cout << "whole: " << whole[0] << " " << whole [1] << " " <<
  //                          whole[2] << " " << whole [3] << " " <<
  //                          whole[4] << " " << whole [5] << " " << std::endl;

  img->SetWholeExtent(whole);
  // Now should Origin and Spacing really be part of information?
  // How about xyx arrays in RectilinearGrid of Points in StructuredGrid?
  img->SetOrigin(torigin);
  img->SetSpacing(tspacing);
  img->GetPointData()->GetScalars()->SetName("Scalars_");
  return img;
}

template <unsigned int Ta2itkDim, typename InputPixelType1, typename InputPixelType2, typename OutputPixelType>
int a2itk<Ta2itkDim, InputPixelType1, typename InputPixelType2, typename OutputPixelType>::expNegativeImageFilter (double factor) {

  typedef itk::Image<InputPixelType1, Ta2itkDim> ImageType;
  typedef itk::ExpNegativeImageFilter<ImageType,ImageType> NegExpFilter;

  NegExpFilter::Pointer filter = NegExpFilter::New();
  filter->SetInput(input1_);
  filter->SetFactor(factor);
  filter->Update();
  output1_ = filter->GetOutput();

  return CV_OK;

}

template <unsigned int Ta2itkDim, typename InputPixelType1, typename InputPixelType2, typename OutputPixelType>
int a2itk<Ta2itkDim, InputPixelType1, typename InputPixelType2, typename OutputPixelType>::createSignedDistanceImageWithFMM (int numSeeds, double* pts,
                                                double origin[3],double spacing[3], 
                                                int dims[3], int start[3],
                                               double time) {

  int i,j;

  typedef itk::Image< InputPixelType1, Ta2itkDim >  InternalImageType;

  typedef  itk::FastMarchingImageFilter< InternalImageType, InternalImageType >
    FastMarchingFilterType;

  // create object
  FastMarchingFilterType::Pointer  fastMarching = FastMarchingFilterType::New();

  typedef FastMarchingFilterType::NodeContainer           NodeContainer;
  typedef FastMarchingFilterType::NodeType                NodeType;

  NodeContainer::Pointer seeds = NodeContainer::New();
  seeds->Initialize();

  NodeType node;
  node.SetValue( time );

  InternalImageType::IndexType  seedPosition;

  // we create a dummy ITK image data (without data) to convert from 2-D
  // points in space to indexes inside of the image

  typedef itk::Image < InputPixelType1 , Ta2itkDim > ImageType;

  ImageType::Pointer image = ImageType::New();
  ImageType::IndexType istart;
  ImageType::SizeType size;
  ImageType::RegionType region;
 
  double sp [ ImageType::ImageDimension ];
  double org [ ImageType::ImageDimension ];

  for (j = 0; j < Ta2itkDim; j++) {
    istart[j]=start[j];
    size[j]=dims[j];
    sp[j]=spacing[j];
    org[j]=origin[j]; 
  }
  region.SetSize(size);
  region.SetIndex(istart);
  
  image->SetRegions( region );
  image->Allocate();
  image->SetSpacing(sp);
  image->SetOrigin(org);

  typedef itk::Point< double, ImageType::ImageDimension > PointType;
  PointType pt;

  int numPtsInside = 0;
  for (i = 0; i < numSeeds; ++i) {
    for (j = 0; j < Ta2itkDim; j++) {
        pt[j] = pts[3*i+j];
    }
    bool isInside = image->TransformPhysicalPointToIndex ( pt, seedPosition );
    if (isInside) {
      node.SetIndex( seedPosition );
      std::cout << "seedPos: ";
      for (j = 0; j < Ta2itkDim; j++) {
        std::cout << seedPosition[j] << " ";
      }
      std::cout<<std::endl;
      seeds->InsertElement( numPtsInside, node );
      numPtsInside++;       
    } else {
        std::cout << "seed not in range: ";
      for (j = 0; j < Ta2itkDim; j++) {
        std::cout << pt[j] << " ";
      }
      std::cout<<std::endl;
    } 
  }

  fastMarching->SetOutputSpacing(sp);
  fastMarching->SetOutputOrigin(org);
  fastMarching->SetOutputRegion(region);

  fastMarching->SetTrialPoints(  seeds  );
  fastMarching->SetSpeedConstant( 1.0 ); 
  fastMarching->SetOutputSize( size );

  fastMarching->UpdateLargestPossibleRegion();

  output1_ = fastMarching->GetOutput();

  /* debugging code
  for (int j = 0; j < dims[0]; j++) {
      for (int k = 0; k < dims[1]; k++) {
          InternalImageType::IndexType pixelIndex;
          pixelIndex[0]=j;pixelIndex[1]=k;
          std::cout << fastMarching->GetOutput()->GetPixel(pixelIndex) << " ";
      }
      std::cout << std::endl;
  }
  std::cout << std::endl;
  */

  return CV_OK;

}

template <unsigned int Ta2itkDim, typename InputPixelType1, typename InputPixelType2, typename OutputPixelType>
int a2itk<Ta2itkDim, InputPixelType1, typename InputPixelType2, typename OutputPixelType>::shapeDetectionLevelSet (double propagationScaling, 
                                      double curvatureScaling,
                                     double maxRMSerror, int maxNumIters) {

  typedef itk::Image< InputPixelType1, Ta2itkDim >  InternalImageType;

  typedef  itk::ShapeDetectionLevelSetImageFilter< InternalImageType, 
                              InternalImageType >    ShapeDetectionFilterType;
  ShapeDetectionFilterType::Pointer shapeDetection = ShapeDetectionFilterType::New();

  shapeDetection->SetInput( input1_ );
  shapeDetection->SetFeatureImage( input2_ );
  shapeDetection->SetPropagationScaling(  propagationScaling );
  shapeDetection->SetCurvatureScaling( curvatureScaling ); 
  shapeDetection->SetMaximumRMSError( maxRMSerror );
  shapeDetection->SetNumberOfIterations( maxNumIters );
  shapeDetection->Update();

  // Print out some useful information 
  std::cout << std::endl;
  std::cout << "Max. no. iterations: " << shapeDetection->GetNumberOfIterations() << std::endl;
  std::cout << "Max. RMS error: " << shapeDetection->GetMaximumRMSError() << std::endl;
  std::cout << std::endl;
  std::cout << "No. elpased iterations: " << shapeDetection->GetElapsedIterations() << std::endl;
  std::cout << "RMS change: " << shapeDetection->GetRMSChange() << std::endl;

  output1_ = shapeDetection->GetOutput();

  return CV_OK;

}


template <unsigned int Ta2itkDim, typename InputPixelType1, typename InputPixelType2, typename OutputPixelType>
int a2itk<Ta2itkDim, InputPixelType1, typename InputPixelType2, typename OutputPixelType>::itkRegionOfInterestFilter(int starting[], int dimensions[]){
  typedef itk::Image< InputPixelType1, Ta2itkDim >  InternalImageType;

  typedef itk::RegionOfInterestImageFilter< InternalImageType, InternalImageType > FilterType;

  FilterType::Pointer filter = FilterType::New();

  InternalImageType::IndexType start;
  start[0] = starting[0];
  start[1] = starting[1];
  start[2] = starting[2];

  InternalImageType::SizeType size;
  size[0] = dimensions[0];
  size[1] = dimensions[1];
  size[2] = dimensions[2];

  InternalImageType::RegionType desiredRegion;
  desiredRegion.SetSize(  size  );
  desiredRegion.SetIndex( start );
 
  filter->SetRegionOfInterest( desiredRegion );

  // Update the filter
 
  filter->SetInput( input1_ );
 
  filter->Update();

  output1_ = filter->GetOutput();
  


  return CV_OK;
}

template <unsigned int Ta2itkDim, typename InputPixelType1, typename InputPixelType2, typename OutputPixelType>
int a2itk<Ta2itkDim, InputPixelType1, typename InputPixelType2, typename OutputPixelType>::itkResampleFilter(double spacingParam, int dimensions[] ){
  typedef itk::Image< InputPixelType1, Ta2itkDim >  InternalImageType;

  typedef itk::ResampleImageFilter<InternalImageType, InternalImageType> ResampleFilterType;
  typedef itk::AffineTransform< double, Ta2itkDim >  TransformType;

  ResampleFilterType::Pointer resample_filter = ResampleFilterType::New();
  TransformType::Pointer transform = TransformType::New();

  transform->SetIdentity();
  resample_filter->SetTransform( transform );

  typedef itk::LinearInterpolateImageFunction< 
                       InternalImageType, double >  InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();
  resample_filter->SetInterpolator( interpolator );
  resample_filter->SetDefaultPixelValue( 0 );

  double spacing[ Ta2itkDim ];

  spacing[0] = spacingParam; // pixel spacing in millimeters along X
  spacing[1] = spacingParam; // pixel spacing in millimeters along Y
  spacing[2] = spacingParam; // pixel spacing in millimeters along Z
  
  resample_filter->SetOutputSpacing( spacing );

// eriks original -  modified by nate
//  const double *origin;
//  const double *orig_spacing;
//  origin = filter->GetOutput()->GetOrigin(); // Use same origin as in VOI
//  orig_spacing = filter->GetOutput()->GetSpacing(); // Spacing in original image

  double origin[3];
  double orig_spacing[3];

  for (int i = 0; i < 3; i++) {
    origin[i] = input1_->GetOrigin()[i]; // Use same origin as in VOI
    orig_spacing[i] = input1_->GetSpacing()[i]; // Spacing in original image
  }

  InternalImageType::SizeType size;
  size[0] = dimensions[0];
  size[1] = dimensions[1];
  size[2] = dimensions[2];  

  InternalImageType::SizeType resamp_size;
  resamp_size[0] = size[0]*orig_spacing[0]/spacing[0];
  resamp_size[1] = size[1]*orig_spacing[1]/spacing[1];
  resamp_size[2] = size[2]*orig_spacing[2]/spacing[2];

  
  resample_filter->SetSize( resamp_size );  
  resample_filter->SetOutputOrigin( origin );

  resample_filter->SetInput( input1_ );

  resample_filter->Update(); 

  output1_ = resample_filter->GetOutput();
  return CV_OK;
}

template <unsigned int Ta2itkDim, typename InputPixelType1, typename InputPixelType2, typename OutputPixelType>
int a2itk<Ta2itkDim, InputPixelType1, typename InputPixelType2, typename OutputPixelType>::itkMinMaxCurvFlowFilter(const unsigned int numIter, 
			    const long radius, const double timeStep){
  typedef itk::Image< InputPixelType1, Ta2itkDim >  InternalImageType;

  typedef itk::MinMaxCurvatureFlowImageFilter<InternalImageType, InternalImageType > FilterType;
  FilterType::Pointer filter = FilterType::New();

  filter->SetInput( input1_ );

  typedef FilterType::RadiusValueType RadiusType;

  filter->SetTimeStep( timeStep );
  filter->SetNumberOfIterations( numIter );
  filter->SetStencilRadius( RadiusType(radius) );
  filter->Update();

  output1_ = filter->GetOutput();

  return CV_OK;
}

template <unsigned int Ta2itkDim, typename InputPixelType1, typename InputPixelType2, typename OutputPixelType>
int a2itk<Ta2itkDim, InputPixelType1, typename InputPixelType2, typename OutputPixelType>::itkSigMapFilter(const double min, const double max, const double alpha, const double beta){

  typedef itk::Image< InputPixelType1, Ta2itkDim >  InputImageType;
  typedef itk::Image< OutputPixelType, Ta2itkDim > OutputImageType;

  typedef itk::SigmoidImageFilter<
               InputImageType, OutputImageType >  SigmoidFilterType;
  SigmoidFilterType::Pointer sigmoidFilter = SigmoidFilterType::New();

  if(max<min) throw itk::ExceptionObject(__FILE__, __LINE__, "Max should be less than min!");

  const InputPixelType1 outputMinimum = min;
  const InputPixelType1 outputMaximum = max;

  sigmoidFilter->SetOutputMinimum(   outputMinimum  );
  sigmoidFilter->SetOutputMaximum(   outputMaximum  );

  sigmoidFilter->SetAlpha(  alpha  );
  sigmoidFilter->SetBeta(   beta   );


  sigmoidFilter->SetInput( input1_ );

  sigmoidFilter->Update();

  output1_ = sigmoidFilter->GetOutput();

  return CV_OK;
}

template <unsigned int Ta2itkDim, typename InputPixelType1, typename InputPixelType2, typename OutputPixelType>
int a2itk<Ta2itkDim, InputPixelType1, typename InputPixelType2, typename OutputPixelType>::itkFmmFilter(const double inputSeeds[SEEDS_ARRAY_MAX][3], int numSeeds, const double start, const double stop){
  const double STOP_MULTIPLIER=1.25;

  typedef itk::Image< InputPixelType1, Ta2itkDim >  InternalImageType;

  // Instantiate FMM
  typedef  itk::FastMarchingImageFilter< InternalImageType, InternalImageType > FastMarchingFilterType;

  FastMarchingFilterType::Pointer fastMarching = FastMarchingFilterType::New();

  // Instantiate scale filter
  typedef  itk::ThresholdImageFilter< InternalImageType >    FilterType;

  FilterType::Pointer thresholdFilter = FilterType::New();
  thresholdFilter->ThresholdOutside(start, stop);
  thresholdFilter->SetOutsideValue( STOP_MULTIPLIER* stop );

  fastMarching->SetInput( input1_ );
  thresholdFilter->SetInput( fastMarching->GetOutput() );

 // Assign seed points
  typedef FastMarchingFilterType::NodeContainer           NodeContainer;
  typedef FastMarchingFilterType::NodeType                NodeType;
  NodeContainer::Pointer seeds = NodeContainer::New();
  InternalImageType::IndexType  seedPosition;
  NodeType node;
  const double seedValue = start;

  seeds->Initialize();
  for(int i = 0;i<numSeeds;i++){
    for(int j=0;j<3;j++){
      seedPosition[j] = inputSeeds[i][j];
    }
    node.SetValue( seedValue );
    node.SetIndex( seedPosition );
    seeds->InsertElement( i , node );
  }
  fastMarching->SetTrialPoints(  seeds  );

  fastMarching->SetOutputSize( 
			      input1_->GetLargestPossibleRegion().GetSize() );

  // Assign stopping time for FMM

  fastMarching->SetStoppingValue(  stop  );
  thresholdFilter->Update();

  output1_ = thresholdFilter->GetOutput();

  return CV_OK;
}

template <unsigned int Ta2itkDim, typename InputPixelType1, typename InputPixelType2, typename OutputPixelType>
int a2itk<Ta2itkDim, InputPixelType1, typename InputPixelType2, typename OutputPixelType>::itkThresholdLevelSet( const double upper, const double lower, const double propagationScaling,
					    const double curvatureScaling, const int numIterations){
  const double MAX_RMS_ERROR = 0.001;

  typedef itk::Image< InputPixelType1, Ta2itkDim >  InternalLevelSetType;
  typedef itk::Image< InputPixelType2, Ta2itkDim >  InternalImageType;

  typedef  itk::ThresholdSegmentationLevelSetImageFilter< InternalLevelSetType, 
	  InternalImageType, OutputPixelType > ThresholdSegmentationLevelSetImageFilterType;
  ThresholdSegmentationLevelSetImageFilterType::Pointer thresholdSegmentation =
	  ThresholdSegmentationLevelSetImageFilterType::New();


  thresholdSegmentation->SetPropagationScaling( propagationScaling );
  thresholdSegmentation->SetCurvatureScaling( curvatureScaling );

  thresholdSegmentation->SetMaximumRMSError( MAX_RMS_ERROR );
  thresholdSegmentation->SetNumberOfIterations( numIterations );
  thresholdSegmentation->SetInput( input1_ );
  thresholdSegmentation->SetFeatureImage( input2_ );

  thresholdSegmentation->SetUpperThreshold( upper );
  thresholdSegmentation->SetLowerThreshold( lower );
  thresholdSegmentation->SetIsoSurfaceValue(0.0);
  thresholdSegmentation->Update();
  output1_ = thresholdSegmentation->GetOutput();


  return CV_OK;
}
