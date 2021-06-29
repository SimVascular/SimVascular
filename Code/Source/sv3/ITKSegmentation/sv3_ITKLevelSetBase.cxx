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

#ifndef CVITKLEVELSETBASE_HXX_
#define CVITKLEVELSETBASE_HXX_

#include "sv3_ITKLevelSetBase.h"
#include <stdio.h>

#include "sv3_ITKLset_ITKUtils.h"

using namespace std;


//Filters needed to generate seed image in ITK
#include "itkBinaryThresholdImageFilter.h"
#include "itkBinaryBallStructuringElement.h"
#include "itkBinaryDilateImageFilter.h"
#include "itkSignedMaurerDistanceMapImageFilter.h"

//Baseline
#include "itkLaplacianSegmentationLevelSetImageFilter.h"
#include "itkGeodesicActiveContourLevelSetImageFilter.h"
#include "itkExpNegativeImageFilter.h"

//Ken's Filters
#include "sv3_VascularPhaseOneLevelSetImageFilter.h"
#include "sv3_VascularPhaseTwoLevelSetImageFilter.h"

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
#include "itkStatisticsImageFilter.h"

//VTK Stuff
#include <vtkPolyDataToImageStencil.h>
#include <vtkImageStencil.h>
#include <vtkImageChangeInformation.h>
#include <vtkImageCast.h>
#include <vtkTransformPolyDataFilter.h>
#include <vtkContourFilter.h>

#include "sv3_ITKLset_EdgeRemapImageFilter.h"
#include "sv3_GeodesicActiveContourLaplacianSmoothLevelSetImageFilter.h"


template<typename TInputImage,
typename TInternalPixelType>
cvITKLevelSetBase<TInputImage, TInternalPixelType>
::cvITKLevelSetBase(){

	m_MaxRMSError = .01;
	m_MaxIterations = 100;
	m_AdvectionScaling = 1.0;
	m_LaplacianScaling = 0;
	m_CurvatureScaling = 1.0;
	m_PropagationScaling = 1.0;
	m_SigmaFeature = 0;
	m_SigmaAdvection = 0;


	m_cvInputImage = NULL;
	m_cvSeedImage = NULL;
	m_cvSeed = NULL;

	m_Debug = false;

	m_vtkFrontPolyData = vtkSmartPointer<vtkPolyData>::New();
	m_vtkFrontImage = vtkSmartPointer<vtkStructuredPoints>::New();
	m_vtkFeatureImage = vtkSmartPointer<vtkStructuredPoints>::New();

	m_itkFrontImage = ITKInternalImageType::New();
	m_itkFeatureImage = ITKInternalImageType::New();
	m_itkSeedImage = ITKInternalImageType::New();
	m_UseNormalVectorCurvature = true;
	m_UseMeanCurvature = false;
	m_UseMinimalCurvature = false;
	m_BinarySeed = true;
}



// ---
// Feature IO and deallocation
// ---
template<typename TInputImage,
typename TInternalPixelType>
void	cvITKLevelSetBase<TInputImage, TInternalPixelType>
::DeallocateFeatureObjs()
 {
	if ( m_cvInputImage != NULL ) {
		delete m_cvInputImage;
		m_cvInputImage = NULL;
	}

 }
template<typename TInputImage,
typename TInternalPixelType>
int cvITKLevelSetBase<TInputImage, TInternalPixelType>
::SetInputImage(cvStrPts *s)
 {
	DeallocateFeatureObjs();

	ExternalImgInfo = ImgInfo(s->GetVtkStructuredPoints());
	if(ImageDimension == 2)
		InternalImgInfo.SetExtent(ExternalImgInfo.GetExtent());
	else
	{
		InternalImgInfo = ImgInfo(s->GetVtkStructuredPoints());
		InternalImgInfo.SetMaxValue(255);
		InternalImgInfo.SetMinValue(0);
		//		InternalImgInfo.SetExtent(ExternalImgInfo.GetExtent());

	}

	//ExternalImgInfo.Print(std::cout);
	InternalImgInfo.Print(std::cout);

	m_cvInputImage = new cvStrPts( s->GetVtkStructuredPoints() );
	m_cvInputImage->SetName( s->GetName() );

        return SV_OK;
 }
template<typename TInputImage,
typename TInternalPixelType>
int cvITKLevelSetBase<TInputImage, TInternalPixelType>
::SetFeatureImage(cvStrPts *s)
 {
	DeallocateFeatureObjs();

	ExternalImgInfo = ImgInfo(s->GetVtkStructuredPoints());
	if(ImageDimension == 2)
		InternalImgInfo.SetExtent(ExternalImgInfo.GetExtent());
	else
	{
		InternalImgInfo.SetExtent(ExternalImgInfo.GetExtent());
		InternalImgInfo.SetMaxValue(255);
		InternalImgInfo.SetMinValue(0);
	}


	//m_cvInputImage = new cvStrPts( s->GetVtkStructuredPoints() );
	//m_cvInputImage->SetName( s->GetName() );

        return SV_OK;
 }
template<typename TInputImage,
typename TInternalPixelType>
int cvITKLevelSetBase<TInputImage, TInternalPixelType>
::GetFeatureImage( cvStrPts **s)
 {
	if ( m_cvInputImage == NULL ) {
                return SV_ERROR;
	} else {
		*s = m_cvInputImage;
                return SV_OK;
	}
 }

template<typename TInputImage,
typename TInternalPixelType>
cvStrPts* cvITKLevelSetBase<TInputImage, TInternalPixelType>
::GetVelocityImage()
 {
	cvStrPts* vimg = new cvStrPts(m_vtkFeatureImage);
	if ( vimg == NULL ) {
		return 0;
	} else {

		return vimg;
	}
 }

template<typename TInputImage,
typename TInternalPixelType>
cvStrPts* cvITKLevelSetBase<TInputImage, TInternalPixelType>
::GetFrontImage()
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
template<typename TInputImage,
typename TInternalPixelType>
void	cvITKLevelSetBase<TInputImage, TInternalPixelType>
::DeallocateFrontObjs()
 {

 }

template<typename TInputImage,
typename TInternalPixelType>
int cvITKLevelSetBase<TInputImage, TInternalPixelType>
::GetFront(cvPolyData** front)
 {

	*front = this->GetFront();
	if(front != 0)
                return SV_OK;
	else
                return SV_ERROR;
 }

template<typename TInputImage,
typename TInternalPixelType>
cvPolyData* cvITKLevelSetBase<TInputImage, TInternalPixelType>
::GetFront()
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
template<typename TInputImage,
typename TInternalPixelType>
void cvITKLevelSetBase<TInputImage, TInternalPixelType>
::DeallocateSeedObjs()
 {

	if ( m_cvSeedImage != NULL ) {
		delete m_cvSeedImage;
		m_cvSeedImage = NULL;
	}

 }


//These functions perform individual level sets
template<typename TInputImage,
typename TInternalPixelType>
int cvITKLevelSetBase<TInputImage, TInternalPixelType>
::ComputeGACLevelSet(float exponent,float kappa,float iso)
 {
	typedef itk::GeodesicActiveContourLaplacianSmoothLevelSetImageFilter<ITKInternalImageType,ITKInternalImageType,float>
	LevelSetFilterType;

	this->GenerateFeatureImage();
	this->GenerateSeedImage();

	//cvITKTemplateNewMacro(LevelSetFilterType,levelSetFilter);
	typename LevelSetFilterType::Pointer levelSetFilter = LevelSetFilterType::New();

	typename ITKInternalImageType::Pointer expImage;

	if(this->GetDebug()){
		std::cout << std::endl << "GAC Levelset" << " expFactorRising "  << exponent << std::endl;
	}
	try
	{

		typedef itk::RescaleIntensityImageFilter<ITKInternalImageType,ITKInternalImageType>
		RescaleFilterType;
		cvITKTemplateNewMacro(RescaleFilterType,rescaleInputFilter);
		rescaleInputFilter->SetOutputMinimum(0);
		rescaleInputFilter->SetOutputMaximum(1);
		rescaleInputFilter->SetInput(m_itkFeatureImage);
		rescaleInputFilter->UpdateLargestPossibleRegion();

		typedef EdgeRemapImageFilter<ITKInternalImageType> EdgeRemapFilterType;
		cvITKTemplateNewMacro(EdgeRemapFilterType,remapper);
		remapper->SetInput(rescaleInputFilter->GetOutput());

		std::cout << "Edge processing Values" << std::endl;
		std::cout << std::endl << "kappa: " << kappa << " exponent " << exponent << std::endl;
		std::cout << std::endl << "sigmaSpeed " << m_SigmaFeature << std::endl;
		remapper->SetKappa(kappa);
		remapper->SetExponent(exponent);

		remapper->Update();

		levelSetFilter->SetDebug(0);

		typedef itk::StatisticsImageFilter<ITKInternalImageType> StatisticsImageFilterType;
		cvITKTemplateNewMacro(StatisticsImageFilterType,statisticsImageFilter);


		statisticsImageFilter->SetInput(remapper->GetOutput());
		statisticsImageFilter->Update();

		std::cout << "Feature Image Stats" << std::endl;
		std::cout << "Mean: " << statisticsImageFilter->GetMean() << std::endl;
		std::cout << "Std.: " << statisticsImageFilter->GetSigma() << std::endl;
		std::cout << "Min: " << statisticsImageFilter->GetMinimum() << std::endl;
		std::cout << "Max: " << statisticsImageFilter->GetMaximum() << std::endl;

		cvITKLSUtil::WriteImage2(m_itkFeatureImage.GetPointer(),"featureimage.mha");
		cvITKLSUtil::WriteImage2(remapper->GetOutput(),"edgeimage.mha");
		cvITKLSUtil::WriteImage2(m_itkSeedImage.GetPointer(),"seedimage.mha");

		std::cout << "Iso Value: " << iso << std::endl;
		levelSetFilter->SetFeatureImage(m_itkFeatureImage);
		levelSetFilter->SetInitialImage(m_itkSeedImage);
		levelSetFilter->SetIsoSurfaceValue(iso);
		std::cout << "Level Set Scaling:" << std::endl;
		std::cout << "Prop Scaling: " << m_PropagationScaling << " Curve Scaling: " << m_CurvatureScaling << std::endl <<
				" Advect. Scaling " << m_AdvectionScaling << " Lap. Scaling " << m_LaplacianScaling << std::endl;

		levelSetFilter->SetPropagationScaling(m_PropagationScaling);
		levelSetFilter->SetCurvatureScaling(m_CurvatureScaling);
		levelSetFilter->SetAdvectionScaling(m_AdvectionScaling);
		levelSetFilter->SetLaplacianSmoothingWeight(m_LaplacianScaling);

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
		std::cerr << "LevelSet Error! :"<< ex.what() << std::endl;
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
	cvITKLSUtil::WriteImage2(levelSetFilter->GetOutput(),"frontImage.mha");

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

	return TCL_OK;


 }
template<typename TInputImage,
typename TInternalPixelType>
int cvITKLevelSetBase<TInputImage, TInternalPixelType>
::ComputeLaplacianLevelSet(float exponent,float kappa,float iso)
 {
	typedef itk::LaplacianSegmentationLevelSetImageFilter<ITKInternalImageType,ITKInternalImageType>
	LevelSetFilterType;

	this->GenerateFeatureImage();
	this->GenerateSeedImage();

	cvITKTemplateNewMacro(LevelSetFilterType,levelSetFilter);

	typename ITKInternalImageType::Pointer expImage;

	if(this->GetDebug()){
		std::cout << std::endl << "GAC Levelset" << " expFactorRising "  << exponent << std::endl;
	}
	try
	{

		typedef itk::RescaleIntensityImageFilter<ITKInternalImageType,ITKInternalImageType>
		RescaleFilterType;
		cvITKTemplateNewMacro(RescaleFilterType,rescaleInputFilter);
		rescaleInputFilter->SetOutputMinimum(0);
		rescaleInputFilter->SetOutputMaximum(1);
		rescaleInputFilter->SetInput(m_itkFeatureImage);
		rescaleInputFilter->UpdateLargestPossibleRegion();

		typedef EdgeRemapImageFilter<ITKInternalImageType> EdgeRemapFilterType;
		cvITKTemplateNewMacro(EdgeRemapFilterType,remapper);
		remapper->SetInput(rescaleInputFilter->GetOutput());

		std::cout << "Edge processing Values" << std::endl;
		std::cout << std::endl << "kappa: " << kappa << " exponent " << exponent << std::endl;
		std::cout << std::endl << "sigmaSpeed " << m_SigmaFeature << std::endl;
		remapper->SetKappa(kappa);
		remapper->SetExponent(exponent);

		remapper->Update();

		levelSetFilter->SetDebug(0);

		typedef itk::StatisticsImageFilter<ITKInternalImageType> StatisticsImageFilterType;
		cvITKTemplateNewMacro(StatisticsImageFilterType,statisticsImageFilter);


		statisticsImageFilter->SetInput(remapper->GetOutput());
		statisticsImageFilter->Update();

		std::cout << "Feature Image Stats" << std::endl;
		std::cout << "Mean: " << statisticsImageFilter->GetMean() << std::endl;
		std::cout << "Std.: " << statisticsImageFilter->GetSigma() << std::endl;
		std::cout << "Min: " << statisticsImageFilter->GetMinimum() << std::endl;
		std::cout << "Max: " << statisticsImageFilter->GetMaximum() << std::endl;

		cvITKLSUtil::WriteImage2(m_itkFeatureImage.GetPointer(),"featureimage.mha");
		cvITKLSUtil::WriteImage2(remapper->GetOutput(),"edgeimage.mha");
		cvITKLSUtil::WriteImage2(m_itkSeedImage.GetPointer(),"seedimage.mha");

		std::cout << "Iso Value: " << iso << std::endl;
		levelSetFilter->SetFeatureImage(m_itkFeatureImage);
		levelSetFilter->SetInitialImage(m_itkSeedImage);
		levelSetFilter->SetIsoSurfaceValue(iso);
		std::cout << "Level Set Scaling:" << std::endl;
		std::cout << "Prop Scaling: " << m_PropagationScaling << " Curve Scaling: " << m_CurvatureScaling << " Advect. Scaling " << m_AdvectionScaling << std::endl;

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
		std::cerr << "LevelSet Error! :"<< ex.what() << std::endl;
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
	cvITKLSUtil::WriteImage2(levelSetFilter->GetOutput(),"frontImage.mha");

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

	return TCL_OK;


 }


template<typename TInputImage,
typename TInternalPixelType>
int cvITKLevelSetBase<TInputImage, TInternalPixelType>
::ComputePhaseOneLevelSet(float kc,
		float expFactorRising,float expFactorFalling)
		{

	typedef itk::VascularPhaseOneLevelSetImageFilter
			<ITKInternalImageType,ITKInternalImageType> LevelSetFilterType;

	cvITKTemplateNewMacro(LevelSetFilterType,levelSetFilter);

	this->GenerateFeatureImage();
	this->GenerateSeedImage();

	cvITKLSUtil::WriteImage2(m_itkFeatureImage.GetPointer(),"featureimage.mha");
	cvITKLSUtil::WriteImage2(m_itkSeedImage.GetPointer(),"seedimage.mha");



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
		//levelSetFilter->SetUseNormalVectorCurvature(m_UseNormalVectorCurvature);

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

	}

	return TCL_OK;
		}
template<typename TInputImage,
typename TInternalPixelType>
int cvITKLevelSetBase<TInputImage, TInternalPixelType>
::ComputePhaseTwoLevelSet(float kupp,float klow)
 {
	typedef itk::VascularPhaseTwoLevelSetImageFilter
			<ITKInternalImageType,ITKInternalImageType> LevelSetFilterType;

	cvITKTemplateNewMacro(LevelSetFilterType,levelSetFilter);

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
		//levelSetFilter->SetUseNormalVectorCurvature(m_UseNormalVectorCurvature);

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

	return TCL_OK;


 }

template<typename TInputImage,
typename TInternalPixelType>
int cvITKLevelSetBase<TInputImage, TInternalPixelType>
::GenerateFeatureImage()
 {
	typename ITKInternalImageType::Pointer tempImg = ITKInternalImageType::New();

	cvITKLSUtil::vtk2itkRecastAndRescale<ITKInternalImageType,ITKExternalImageType>(m_cvInputImage->GetVtkStructuredPoints(),tempImg,
			&InternalImgInfo);
	cvITKLSUtil::WriteImage2(tempImg.GetPointer(),"inputimage.mha");
	cvITKLSUtil::itkGenerateFeatureImage<ITKInternalImageType>(tempImg, m_itkFeatureImage, m_SigmaFeature);

	// Debug only, vtk image not used
	cvITKLSUtil::itk2vtkRecastAndRescale<ITKInternalImageType,ITKExternalImageType>(m_itkFeatureImage,m_vtkFeatureImage,&ExternalImgInfo);

        return SV_OK;
 }



template<typename TInputImage,
typename TInternalPixelType>
int cvITKLevelSetBase<TInputImage, TInternalPixelType>
::GenerateSeedImage()
 {
	//ExternalImgInfo.Print(std::cout);

	if(m_BinarySeed) {
		vtkStructuredPoints* vtkImg = vtkStructuredPoints::New();
		if(ImageDimension == 2)
			cvITKLSUtil::vtkPolyDataTo2DImage(m_cvSeed->GetVtkPolyData(),vtkImg,&ExternalImgInfo);
		else
		{
			std::cout << "3d Image" << std::endl;
			cvITKLSUtil::vtkPolyDataToVolume(m_cvSeed->GetVtkPolyData(),vtkImg,&ExternalImgInfo);
		}
		cvITKLSUtil::vtkWriteImage2(vtkImg,"binaryimage.mha");

		cvITKLSUtil::vtk2itkBinaryImageToSeedImage
		<ITKInternalImageType,ITKExternalImageType>(vtkImg,m_itkSeedImage,&InternalImgInfo);
		vtkImg->Delete();
	}
	else
	{
		// Do something?
	}
        return SV_OK;
 }

#ifdef USE_QUICKVIEW_DEBUG
template<typename TInputImage,
typename TInternalPixelType>
template<typename TLevelSetFilterType>

void cvITKLevelSetBase<TInputImage, TInternalPixelType>
::ShowDebug(typename TLevelSetFilterType::Pointer levelSetFilter)
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


	cvITKTemplateNewMacro(BinaryToLabelMapFilterType,binaryToLabelMap);
	cvITKTemplateNewMacro(LabelMapToLabelImageFilterType,labelMaptoImage);
	cvITKTemplateNewMacro(LabelOverlayImageFilterType,labelOverlay);
	cvITKTemplateNewMacro(ContourFilterType,contour);
	cvITKTemplateNewMacro(RecastFilterType,recastUChar);
	cvITKTemplateNewMacro(ThresholdFilterType,thresholdFilter);

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

	cvITKTemplateNewMacro(RescaleFilterType,rescale);
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
#endif

template<typename TInputImage,
typename TInternalPixelType>
int cvITKLevelSetBase<TInputImage, TInternalPixelType>
::CopyFrontToSeed()
 {
	try{
		typedef itk::BinaryThresholdImageFilter<
				ITKInternalImageType,
				ITKExternalImageType    >       ThresholdingFilterType;
		typename ThresholdingFilterType::Pointer thresholder = ThresholdingFilterType::New();
		thresholder->SetInput(m_itkFrontImage);
		thresholder->SetLowerThreshold( -1000.0 );
		thresholder->SetUpperThreshold(     0.0 );
		thresholder->SetOutsideValue(  0  );
		thresholder->SetInsideValue(  255 );
		vtkStructuredPoints* vtkImg = vtkStructuredPoints::New();
		cvITKLSUtil::CopyITKtoVTK<ITKExternalImageType>(thresholder->GetOutput(), vtkImg);

		cvITKLSUtil::vtkWriteImage2(vtkImg,"binaryimage-reseed.mha");

		cvITKLSUtil::vtk2itkBinaryImageToSeedImage
		<ITKInternalImageType,ITKExternalImageType>(vtkImg,m_itkSeedImage,&InternalImgInfo);
		vtkImg->Delete();



		m_BinarySeed = false;

	}
	catch( itk::ExceptionObject & err )
	{
		std::cerr << "ExceptionObject caught !" << std::endl;
		std::cerr << "Copy Seed Error! :"<<err << std::endl;
		return TCL_ERROR;
	}
	catch(const std::exception& ex)
	{
		std::cerr << "std exception caught !" << std::endl;
		std::cerr << "Copy Seed Error! :"<<ex.what() << std::endl;
		return TCL_ERROR;

	}
	catch(...)
	{
		std::cerr << "Unknown Error!" << std::endl;
		return TCL_ERROR;

	}

   return TCL_OK;
 }


#endif //CVITKLEVELSETBASE_HXX_
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

