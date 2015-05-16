/*=========================================================================
 *
 * Copyright (c) 2014 The Regents of the University of California.
 * All Rights Reserved.
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
 *
 *=========================================================================*/
/*
 * cvITKLevelSet.cxx
 *
 *  Created on: Dec 12, 2013
 *      Author: jmerkow
 */

#include "cvITKLevelSet.h"


//Filters needed to generate seed image in ITK
#include "itkBinaryThresholdImageFilter.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkBinaryDilateImageFilter.h"
#include "itkSignedMaurerDistanceMapImageFilter.h"

//Baseline
#include "itkGeodesicActiveContourLevelSetImageFilter.h"
#include "itkExpNegativeImageFilter.h"

//Ken's Filters
#include "itkVascularPhaseOneLevelSetImageFilter.h"
#include "itkVascularPhaseTwoLevelSetImageFilter.h"

//Recast
#include "itkCastImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkChangeInformationImageFilter.h"
#include "itkStatisticsImageFilter.h"

//debug + Overlay
#include "itkBinaryContourImageFilter.h"
#include "itkBinaryImageToLabelMapFilter.h"
#include "itkImageRegionIterator.h"
#include "itkLabelMapToLabelImageFilter.h"
#include "itkLabelOverlayImageFilter.h"
#include "itkRGBPixel.h"

//VTK Stuff
#include <vtkPolyDataToImageStencil.h>
#include <vtkImageStencil.h>
#include <vtkImageChangeInformation.h>
#include <vtkImageCast.h>
#include <vtkTransformPolyDataFilter.h>
#include <vtkContourFilter.h>



cvITKLevelSet::cvITKLevelSet(){

	m_MaxRMSError = .01;
	m_MaxIterations = 100;
	m_AdvectionScaling = 0;
	m_CurvatureScaling = 1.0;
	m_PropagationScaling = 0.0;
	m_SigmaFeature = 0;
	m_SigmaAdvection = 0;


	m_cvInputImage = NULL;
	m_cvSeedImage = NULL;
	m_cvSeed = NULL;

	m_Debug = false;
	m_UseInputImageAsFeature = false;

	m_vtkFrontPolyData = vtkSmartPointer<vtkPolyData>::New();
	m_vtkFrontImage = vtkSmartPointer<vtkStructuredPoints>::New();
	m_vtkFeatureImage = vtkSmartPointer<vtkStructuredPoints>::New();

	m_itkFrontImage = ITKInternalImageType::New();
	m_itkFeatureImage = ITKInternalImageType::New();
	m_itkSeedImage = ITKInternalImageType::New();
}



// ---
// Feature IO and deallocation
// ---
void	cvITKLevelSet::DeallocateFeatureObjs()
{
	if ( m_cvInputImage != NULL ) {
		delete m_cvInputImage;
		m_cvInputImage = NULL;
	}

}
int cvITKLevelSet::SetInputImage(cvStrPts *s)
{
	DeallocateFeatureObjs();

	ExternalImgInfo = ImgInfo(s->GetVtkStructuredPoints());
	InternalImgInfo.SetExtent(ExternalImgInfo.GetExtent());

	//ExternalImgInfo.Print(std::cout);
	//InternalImgInfo.Print(std::cout);

	m_cvInputImage = new cvStrPts( s->GetVtkStructuredPoints() );
	m_cvInputImage->SetName( s->GetName() );

	return 1;
}
int cvITKLevelSet::SetFeatureImage(cvStrPts *s)
{
	DeallocateFeatureObjs();

	ExternalImgInfo = ImgInfo(s->GetVtkStructuredPoints());
	InternalImgInfo.SetExtent(ExternalImgInfo.GetExtent());

	//m_cvInputImage = new cvStrPts( s->GetVtkStructuredPoints() );
	//m_cvInputImage->SetName( s->GetName() );

	return 1;
}
int cvITKLevelSet::GetFeatureImage( cvStrPts **s)
{
	if ( m_cvInputImage == NULL ) {
		return 0;
	} else {
		*s = m_cvInputImage;
		return 1;
	}
}

cvStrPts* cvITKLevelSet::GetVelocityImage()
{
	cvStrPts* vimg = new cvStrPts(m_vtkFeatureImage);
	if ( vimg == NULL ) {
		return 0;
	} else {

		return vimg;
	}
}

cvStrPts* cvITKLevelSet::GetFrontImage()
{
	cvStrPts* vimg = new cvStrPts(m_vtkFrontImage);
	if ( vimg == NULL ) {
		return 0;
	} else {

		return vimg;
	}
}


// ---
// Front IO and deallocation
// ---
void	cvITKLevelSet::DeallocateFrontObjs()
{

}

int cvITKLevelSet::GetFront(cvPolyData** front)
{

	*front = this->GetFront();
	if(front != 0)
		return 1;
	else
		return 0;
}

cvPolyData* cvITKLevelSet::GetFront()
{
	cvPolyData *front;
	front = new cvPolyData(m_vtkFrontPolyData);
	if ( front == NULL ) {
		return 0;
	} else {

		return front;
	}
}

// ---
// Seed IO and deallocation
// ---
void cvITKLevelSet::DeallocateSeedObjs()
{

	if ( m_cvSeedImage != NULL ) {
		delete m_cvSeedImage;
		m_cvSeedImage = NULL;
	}

}


//These functions perform individual level sets
int cvITKLevelSet::ComputeGACLevelSet(float expFactorRising)
{

	typedef itk::GeodesicActiveContourLevelSetImageFilter<ITKInternalImageType,ITKInternalImageType>
	LevelSetFilterType;

	this->GenerateFeatureImage();
	this->GenerateSeedImage();

	cvITKNewMacro(LevelSetFilterType,levelSetFilter);

	ITKInternalImageType::Pointer expImage;

	if(this->GetDebug()){
		std::cout << std::endl << "GAC Levelset" << " expFactorRising "  << expFactorRising << std::endl;
	}
	try
	{


		typedef itk::ExpNegativeImageFilter<ITKInternalImageType,ITKInternalImageType>
		ExpFilterType;
		cvITKNewMacro(ExpFilterType,expFilter);
		expFilter->SetInput(m_itkFeatureImage);
		expFilter->Update();

		levelSetFilter->SetDebug(0);
		levelSetFilter->UseImageSpacingOff();

		levelSetFilter->SetFeatureImage(expFilter->GetOutput());
		levelSetFilter->SetInitialImage(m_itkSeedImage);

		levelSetFilter->SetPropagationScaling(m_PropagationScaling);
		levelSetFilter->SetCurvatureScaling(m_CurvatureScaling);
		levelSetFilter->SetAdvectionScaling(m_AdvectionScaling);

		levelSetFilter->SetMaximumRMSError(m_MaxRMSError);
		levelSetFilter->SetNumberOfIterations(m_MaxIterations);

		levelSetFilter->Update();

		if(1){
			std::cout << "LevelSet Completed in: " << levelSetFilter->GetElapsedIterations() << " iterations with " <<
					levelSetFilter->GetRMSChange() << "RMS Change at end"<< std::endl;
		}
	}
	catch( itk::ExceptionObject & err )
	{
		std::cerr << "ExceptionObject caught !" << std::endl;
		std::cerr << "LevelSet Error! :"<<err << std::endl;
		return TCL_ERROR;
	}
	catch(const std::exception& ex)
	{
		std::cerr << "std exception caught !" << std::endl;
		std::cerr << "LevelSet Error! :"<<ex.what() << std::endl;
		return TCL_ERROR;

	}
	catch(...)
	{
		std::cerr << "Unknown Error!" << std::endl;
		return TCL_ERROR;

	}

	DeallocateFrontObjs();

	//save itkseed
	try
	{
		cvITKLSUtil::itkDeepCopy(levelSetFilter->GetOutput(),m_itkFrontImage);
	}
	catch( itk::ExceptionObject & err )
	{
		std::cerr << "ExceptionObject caught !" << std::endl;
		std::cerr << "LevelSet Error! :"<<err << std::endl;
		return TCL_ERROR;
	}

	//copy seed to VTK
	cvITKLSUtil::CopyITKtoVTK(levelSetFilter->GetOutput(),
			m_vtkFrontImage);

	//copy speed image to VTK
	cvITKLSUtil::CopyITKtoVTK(levelSetFilter->GetSpeedImage(), m_vtkFeatureImage);

	//perform contouring in vtk and save i in the front
	vtkSmartPointer<vtkContourFilter> contour = vtkSmartPointer<vtkContourFilter>::New();
	contour->SetInputData(m_vtkFrontImage);
	contour->SetValue( 0, 0 );
	contour->Update();
	m_vtkFrontPolyData = contour->GetOutput();


	//We're done, but there is some debug info I might want
	if(this->GetDebug())
		std::cout << "NumPts " << contour->GetOutput()->GetNumberOfPoints() <<std::endl;
	if(this->GetDebug())
	{
		typedef unsigned char UCPixelType;
		typedef itk::Image<UCPixelType,2> UCImageType;
		typedef itk::RGBPixel<UCPixelType> RGBPixelType;
		typedef itk::Image<RGBPixelType> RGBImageType;
		//Labeling
		typedef itk::BinaryThresholdImageFilter<ITKInternalImageType,ITKInternalImageType> ThresholdFilterType;
		typedef itk::BinaryContourImageFilter<ITKInternalImageType,ITKInternalImageType> ContourFilterType;
		typedef itk::RescaleIntensityImageFilter<ITKInternalImageType,UCImageType> RecastFilterType;
		typedef itk::BinaryImageToLabelMapFilter<UCImageType> BinaryToLabelMapFilterType;
		typedef itk::LabelMapToLabelImageFilter<BinaryToLabelMapFilterType::OutputImageType, UCImageType> LabelMapToLabelImageFilterType;
		typedef itk::LabelOverlayImageFilter<ITKInternalImageType, UCImageType, RGBImageType> LabelOverlayImageFilterType;
		typedef itk::RescaleIntensityImageFilter<ITKInternalImageType,ITKInternalImageType> RescaleFilterType;


		BinaryToLabelMapFilterType::Pointer binaryToLabelMap = BinaryToLabelMapFilterType::New();
		LabelMapToLabelImageFilterType::Pointer labelMaptoImage = LabelMapToLabelImageFilterType::New();
		LabelOverlayImageFilterType::Pointer labelOverlay = LabelOverlayImageFilterType::New();
		ContourFilterType::Pointer contour = ContourFilterType::New();
		RecastFilterType::Pointer recastUChar = RecastFilterType::New();


		cvITKNewMacro(ThresholdFilterType,thresholdFilter);

		thresholdFilter->SetInput(levelSetFilter->GetOutput());
		thresholdFilter->SetLowerThreshold(-1000);
		thresholdFilter->SetUpperThreshold(0);
		thresholdFilter->SetInsideValue(1);

		contour->SetInput(thresholdFilter->GetOutput());
		contour->SetForegroundValue(1);
		contour->SetBackgroundValue(0);
		contour->FullyConnectedOn();

		recastUChar->SetInput(contour->GetOutput());
		binaryToLabelMap->SetInput(recastUChar->GetOutput());
		labelMaptoImage->SetInput(binaryToLabelMap->GetOutput());
		labelOverlay->SetLabelImage(labelMaptoImage->GetOutput());
		labelOverlay->SetOpacity(.3);
		labelOverlay->ResetColors();
		labelOverlay->AddColor(255,0,1.0);

		RescaleFilterType::Pointer rescale = RescaleFilterType::New();
		rescale->SetOutputMaximum(255);
		rescale->SetOutputMinimum(0);

		QuickView viewer;
		bool flip = false;

		viewer.AddImage(m_itkFeatureImage.GetPointer(),
				flip,
				"Input");
		viewer.AddImage(m_itkSeedImage.GetPointer(),
				flip,
				"Seed");
		rescale->SetInput(levelSetFilter->GetSpeedImage());
		labelOverlay->SetInput(rescale->GetOutput());
		viewer.AddImage(labelOverlay->GetOutput(),
				flip,
				"Gradient Output");
		viewer.AddImage(levelSetFilter->GetOutput(),
				flip,
				"VascOneLevelSet Output");
		viewer.Visualize();
	}
	return TCL_OK;


}
int cvITKLevelSet::ComputePhaseOneLevelSet(float kc,
		float expFactorRising,float expFactorFalling)
{

	typedef itk::VascularPhaseOneLevelSetImageFilter
			<ITKInternalImageType,ITKInternalImageType> LevelSetFilterType;

	cvITKNewMacro(LevelSetFilterType,levelSetFilter);

	this->GenerateFeatureImage();
	this->GenerateSeedImage();

	if(this->GetDebug()){
		std::cout << std::endl << "Vasc Phase One " << "sigmaSpeed " << m_SigmaFeature << " sigmaAdv "  << m_SigmaAdvection << std::endl;
		std::cout << "expFactorRising " << expFactorRising << " expFactorFalling " << expFactorFalling <<std::endl;
		std::cout << "propScale " << m_PropagationScaling << " curveScale " <<m_CurvatureScaling << " advScale " << m_AdvectionScaling <<std::endl;
	}
	try
	{
		levelSetFilter->SetFeatureImage(m_itkFeatureImage);
		levelSetFilter->SetInitialImage(m_itkSeedImage);
		levelSetFilter->SetDebug(0);
		levelSetFilter->UseImageSpacingOff();

		//levelSetFilter->SetSpeedDerivativeSigma(m_SigmaFeature);

		levelSetFilter->SetAdvectionDerivativeSigma(m_SigmaAdvection);
		levelSetFilter->SetRisingVelocityDecayModifier(expFactorRising);
		levelSetFilter->SetFallingVelocityDecayModifier(expFactorFalling);
		levelSetFilter->SetPropagationScaling(m_PropagationScaling);
		levelSetFilter->SetCurvatureScaling(m_CurvatureScaling);
		levelSetFilter->SetAdvectionScaling(m_AdvectionScaling);
		levelSetFilter->SetEquilibriumCurvature(kc);
		levelSetFilter->SetMaximumRMSError(m_MaxRMSError);
		levelSetFilter->SetNumberOfIterations(m_MaxIterations);

		levelSetFilter->Update();

		if(1){
			std::cout << "LevelSet Completed in: " << levelSetFilter->GetElapsedIterations() << " iterations with " <<
					levelSetFilter->GetRMSChange() << "RMS Change at end"<< std::endl;
		}
	}
	catch( itk::ExceptionObject & err )
	{
		std::cerr << "ExceptionObject caught !" << std::endl;
		std::cerr << "LevelSet Error! :"<<err << std::endl;
		return TCL_ERROR;
	}
	catch(const std::exception& ex)
	{
		std::cerr << "std exception caught !" << std::endl;
		std::cerr << "LevelSet Error! :"<<ex.what() << std::endl;
		return TCL_ERROR;

	}
	catch(...)
	{
		std::cerr << "Unknown Error!" << std::endl;
		return TCL_ERROR;

	}
	DeallocateFrontObjs();

	//save itkseed
	try
	{
		cvITKLSUtil::itkDeepCopy<ITKInternalImageType>(levelSetFilter->GetOutput(),m_itkFrontImage);
	}
	catch( itk::ExceptionObject & err )
	{
		std::cerr << "ExceptionObject caught !" << std::endl;
		std::cerr << "LevelSet Error! :"<<err << std::endl;
		return TCL_ERROR;
	}

	//copy seed to VTK
	cvITKLSUtil::itk2vtkRecast<ITKInternalImageType>((ITKInternalImageType*)levelSetFilter->GetOutput(),m_vtkFrontImage,&ExternalImgInfo);
	//copy speed image to VTK

	cvITKLSUtil::itk2vtkRecastAndRescale<ITKInternalImageType,ITKExternalImageType>((ITKInternalImageType*)levelSetFilter->GetSpeedImage(),m_vtkFeatureImage,&ExternalImgInfo);

	//perform contouring in vtk and save i in the front
	vtkSmartPointer<vtkContourFilter> contour = vtkSmartPointer<vtkContourFilter>::New();
	contour->SetInputData(m_vtkFrontImage);
	contour->SetValue( 0, 0 );
	contour->Update();
	m_vtkFrontPolyData = contour->GetOutput();


	//We're done, but there is some debug info I might want
	if(this->GetDebug())
		std::cout << "NumPts " << contour->GetOutput()->GetNumberOfPoints() <<std::endl;

	if(this->GetDebug())
	{
		typedef unsigned char UCPixelType;
		typedef itk::Image<UCPixelType,2> UCImageType;
		typedef itk::RGBPixel<UCPixelType> RGBPixelType;
		typedef itk::Image<RGBPixelType> RGBImageType;
		//Labeling
		typedef itk::BinaryThresholdImageFilter<ITKInternalImageType,ITKInternalImageType> ThresholdFilterType;
		typedef itk::BinaryContourImageFilter<ITKInternalImageType,ITKInternalImageType> ContourFilterType;
		typedef itk::RescaleIntensityImageFilter<ITKInternalImageType,UCImageType> RecastFilterType;
		typedef itk::BinaryImageToLabelMapFilter<UCImageType> BinaryToLabelMapFilterType;
		typedef itk::LabelMapToLabelImageFilter<BinaryToLabelMapFilterType::OutputImageType, UCImageType> LabelMapToLabelImageFilterType;
		typedef itk::LabelOverlayImageFilter<ITKInternalImageType, UCImageType, RGBImageType> LabelOverlayImageFilterType;
		typedef itk::RescaleIntensityImageFilter<ITKInternalImageType,ITKInternalImageType> RescaleFilterType;


		cvITKNewMacro(BinaryToLabelMapFilterType,binaryToLabelMap);
		cvITKNewMacro(LabelMapToLabelImageFilterType,labelMaptoImage);
		cvITKNewMacro(LabelOverlayImageFilterType,labelOverlay);
		cvITKNewMacro(ContourFilterType,contour);
		cvITKNewMacro(RecastFilterType,recastUChar);
		cvITKNewMacro(ThresholdFilterType,thresholdFilter);

		thresholdFilter->SetInput(levelSetFilter->GetOutput());
		thresholdFilter->SetLowerThreshold(-1000);
		thresholdFilter->SetUpperThreshold(0);
		thresholdFilter->SetInsideValue(1);

		contour->SetInput(thresholdFilter->GetOutput());
		contour->SetForegroundValue(1);
		contour->SetBackgroundValue(0);
		contour->FullyConnectedOn();

		recastUChar->SetInput(contour->GetOutput());
		binaryToLabelMap->SetInput(recastUChar->GetOutput());
		labelMaptoImage->SetInput(binaryToLabelMap->GetOutput());
		labelOverlay->SetLabelImage(labelMaptoImage->GetOutput());
		labelOverlay->SetOpacity(.3);
		labelOverlay->ResetColors();
		labelOverlay->AddColor(255,0,1.0);

		RescaleFilterType::Pointer rescale = RescaleFilterType::New();
		rescale->SetOutputMaximum(255);
		rescale->SetOutputMinimum(0);

		QuickView viewer;
		bool flip = false;

		viewer.AddImage(m_itkFeatureImage.GetPointer(),
				flip,
				"itkFeatureImage");
		viewer.AddImage(m_itkSeedImage.GetPointer(),
				flip,
				"Seed");
		rescale->SetInput(levelSetFilter->GetSpeedImage());
		labelOverlay->SetInput(rescale->GetOutput());
		viewer.AddImage(labelOverlay->GetOutput(),
				flip,
				"Gradient Output");
		viewer.AddImage(levelSetFilter->GetOutput(),
				flip,
				"VascOneLevelSet Output");
		viewer.Visualize();
	}

	return TCL_OK;
}
int cvITKLevelSet::ComputePhaseTwoLevelSet(float kupp,float klow)
{
	typedef itk::VascularPhaseTwoLevelSetImageFilter
			<ITKInternalImageType,ITKInternalImageType> LevelSetFilterType;

	cvITKNewMacro(LevelSetFilterType,levelSetFilter);

	this->GenerateFeatureImage();
	this->GenerateSeedImage();

	if(this->GetDebug()){
		std::cout << std::endl << "Vasc Phase Two " << "Kupp " << kupp << " Klow "  << klow << std::endl;
		std::cout << "propScale " << m_PropagationScaling << " curveScale " <<m_CurvatureScaling << " advScale " << m_AdvectionScaling <<std::endl;
	}

	try{

		levelSetFilter->SetFeatureImage(m_itkFeatureImage);
		levelSetFilter->SetInitialImage(m_itkSeedImage);
		levelSetFilter->SetDebug(0);
		levelSetFilter->UseImageSpacingOff();

		levelSetFilter->SetAdvectionDerivativeSigma(m_SigmaAdvection);
		levelSetFilter->SetPropagationScaling(m_PropagationScaling);
		levelSetFilter->SetCurvatureScaling(m_CurvatureScaling);
		levelSetFilter->SetAdvectionScaling(m_AdvectionScaling);

		levelSetFilter->SetCurvataureLowerThreshold(klow);
		levelSetFilter->SetCurvataureUpperThreshold(kupp);

		levelSetFilter->SetMaximumRMSError(m_MaxRMSError);
		levelSetFilter->SetNumberOfIterations(m_MaxIterations);

		levelSetFilter->Update();

		if(1){
			std::cout << "LevelSet Completed in: " << levelSetFilter->GetElapsedIterations() << " iterations with " <<
					levelSetFilter->GetRMSChange() << "RMS Change at end"<< std::endl;
		}
	}
	catch( itk::ExceptionObject & err )
	{
		std::cerr << "ExceptionObject caught !" << std::endl;
		std::cerr << "LevelSet Error! :"<<err << std::endl;
		return TCL_ERROR;
	}
	catch(const std::exception& ex)
	{
		std::cerr << "std exception caught !" << std::endl;
		std::cerr << "LevelSet Error! :"<<ex.what() << std::endl;
		return TCL_ERROR;

	}
	catch(...)
	{
		std::cerr << "Unknown Error!" << std::endl;
		return TCL_ERROR;

	}

	//copy seed to VTK
	cvITKLSUtil::itk2vtkRecast((ITKInternalImageType*)levelSetFilter->GetOutput(),m_vtkFrontImage,&ExternalImgInfo);
	//copy speed image to VTK

	cvITKLSUtil::itk2vtkRecastAndRescale<ITKInternalImageType,ITKExternalImageType>((ITKInternalImageType*)levelSetFilter->GetSpeedImage(),m_vtkFeatureImage,&ExternalImgInfo);

	//perform contouring in vtk and save i in the front
	vtkSmartPointer<vtkContourFilter> contour = vtkSmartPointer<vtkContourFilter>::New();
	contour->SetInputData(m_vtkFrontImage);
	contour->SetValue( 0, 0 );
	contour->Update();
	m_vtkFrontPolyData = contour->GetOutput();


	//We're done, but there is some debug info I might want
	if(this->GetDebug())
		std::cout << "NumPts " << contour->GetOutput()->GetNumberOfPoints() <<std::endl;

	if(this->GetDebug())
	{
		typedef unsigned char UCPixelType;
		typedef itk::Image<UCPixelType,2> UCImageType;
		typedef itk::RGBPixel<UCPixelType> RGBPixelType;
		typedef itk::Image<RGBPixelType> RGBImageType;
		//Labeling
		typedef itk::BinaryThresholdImageFilter<ITKInternalImageType,ITKInternalImageType> ThresholdFilterType;
		typedef itk::BinaryContourImageFilter<ITKInternalImageType,ITKInternalImageType> ContourFilterType;
		typedef itk::RescaleIntensityImageFilter<ITKInternalImageType,UCImageType> RecastFilterType;
		typedef itk::BinaryImageToLabelMapFilter<UCImageType> BinaryToLabelMapFilterType;
		typedef itk::LabelMapToLabelImageFilter<BinaryToLabelMapFilterType::OutputImageType, UCImageType> LabelMapToLabelImageFilterType;
		typedef itk::LabelOverlayImageFilter<ITKInternalImageType, UCImageType, RGBImageType> LabelOverlayImageFilterType;
		typedef itk::RescaleIntensityImageFilter<ITKInternalImageType,ITKInternalImageType> RescaleFilterType;


		BinaryToLabelMapFilterType::Pointer binaryToLabelMap = BinaryToLabelMapFilterType::New();
		LabelMapToLabelImageFilterType::Pointer labelMaptoImage = LabelMapToLabelImageFilterType::New();
		LabelOverlayImageFilterType::Pointer labelOverlay = LabelOverlayImageFilterType::New();
		ContourFilterType::Pointer contour = ContourFilterType::New();
		RecastFilterType::Pointer recastUChar = RecastFilterType::New();


		cvITKNewMacro(ThresholdFilterType,thresholdFilter);

		thresholdFilter->SetInput(levelSetFilter->GetOutput());
		thresholdFilter->SetLowerThreshold(-1000);
		thresholdFilter->SetUpperThreshold(0);
		thresholdFilter->SetInsideValue(1);

		contour->SetInput(thresholdFilter->GetOutput());
		contour->SetForegroundValue(1);
		contour->SetBackgroundValue(0);
		contour->FullyConnectedOn();

		recastUChar->SetInput(contour->GetOutput());
		binaryToLabelMap->SetInput(recastUChar->GetOutput());
		labelMaptoImage->SetInput(binaryToLabelMap->GetOutput());
		labelOverlay->SetLabelImage(labelMaptoImage->GetOutput());
		labelOverlay->SetOpacity(.3);
		labelOverlay->ResetColors();
		labelOverlay->AddColor(255,0,1.0);

		RescaleFilterType::Pointer rescale = RescaleFilterType::New();
		rescale->SetOutputMaximum(255);
		rescale->SetOutputMinimum(0);

		QuickView viewer;
		bool flip = false;

		viewer.AddImage(m_itkFeatureImage.GetPointer(),
				flip,
				"Input");
		viewer.AddImage(m_itkSeedImage.GetPointer(),
				flip,
				"Seed");
		rescale->SetInput(levelSetFilter->GetSpeedImage());
		labelOverlay->SetInput(rescale->GetOutput());
		viewer.AddImage(labelOverlay->GetOutput(),
				flip,
				"Gradient Output");
		viewer.AddImage(levelSetFilter->GetOutput(),
				flip,
				"VascOneLevelSet Output");
		viewer.Visualize();
	}
	return TCL_OK;


}

int cvITKLevelSet::GenerateFeatureImage()
{
	ITKInternalImageType::Pointer tempImg = ITKInternalImageType::New();
	cvITKLSUtil::vtk2itkRecastAndRescale<ITKInternalImageType,ITKExternalImageType>(m_cvInputImage->GetVtkStructuredPoints(),tempImg,
			&InternalImgInfo);
	if (m_UseInputImageAsFeature)
	{
		cvITKLSUtil::itkGenerateFeatureImageNoGrad<ITKInternalImageType>(tempImg, m_itkFeatureImage, m_SigmaFeature);
	}
	else
	{
		cvITKLSUtil::itkGenerateFeatureImage<ITKInternalImageType>(tempImg, m_itkFeatureImage, m_SigmaFeature);
	}
	//cvITKLSUtil::WriteImage2(m_itkFeatureImage.GetPointer(),"featureimage.mha");


	// Debug only, vtk image not used
	cvITKLSUtil::itk2vtkRecastAndRescale<ITKInternalImageType,ITKExternalImageType>(m_itkFeatureImage,m_vtkFeatureImage,&ExternalImgInfo);

	return 1;
}



int cvITKLevelSet::GenerateSeedImage()
{
	//ExternalImgInfo.Print(std::cout);
	vtkStructuredPoints* vtkImg = vtkStructuredPoints::New();
	cvITKLSUtil::vtkPolyDataTo2DImage(m_cvSeed->GetVtkPolyData(),vtkImg,&ExternalImgInfo);
	cvITKLSUtil::vtk2itkBinaryImageToSeedImage
	<ITKInternalImageType,ITKExternalImageType>(vtkImg,m_itkSeedImage,&InternalImgInfo);
	//cvITKLSUtil::WriteImage2(m_itkSeedImage.GetPointer(),"seedimage.mha");
	vtkImg->Delete();

	return 1;
}



/* Vtk Methods */

/* Specialized VTK<->ITK Methods */

/**
 * vtk2itkRecastAndRescale: This function take in a vtkImage, and itkImage, and
 * ImgInfo.  It converts the vtk image to itk, then rescales the itk image to
 * fit the refInfo min/max.  This image is copied into the itkImage.
 */


/**
 * itk2vtkRecastAndRescale: This function readies an ITK image for vtk.  Right
 * now this function does not rescale at all.
 */


/** itk Methods */
/**
 * itkGenerateGradientImage: This function applies a gradient to the input image
 */

/* General VTK<->ITK Helper Methods */

