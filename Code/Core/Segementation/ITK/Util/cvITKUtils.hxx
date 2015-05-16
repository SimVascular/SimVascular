/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
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
 * cvITKUtils.hxx
 *
 *  Created on: May 22, 2014
 *      Author: jmerkow
 */

#ifndef CVLEVELSETUTILS_HXX_
#define CVLEVELSETUTILS_HXX_

#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkGradientMagnitudeImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkDiscreteGaussianImageFilter.h"
#include "itkSignedMaurerDistanceMapImageFilter.h"
#include "itkDanielssonDistanceMapImageFilter.h"
#include "./ITKCode/EdgeRemapImageFilter.h"
#include <vtkImageChangeInformation.h>
#include "itkInvertIntensityImageFilter.h"
#include <itkAbsImageFilter.h>
#include "itkCastImageFilter.h"

#include "itkBinaryContourImageFilter.h"
#include "itkBinaryThresholdImageFilter.h"
#include "itkMultiplyImageFilter.h"

#include <vtkImageCast.h>
#include <vtkTypeTraits.h>
namespace cvITKLSUtil {

/* ITK Only Methods */
template <typename TImageType>
void itkGenerateEdgeProxImage(TImageType* itkInputImage,
		typename TImageType::Pointer outputImage,
		double sigma,double kappa, double exponent)
{
	typedef itk::GradientMagnitudeRecursiveGaussianImageFilter<TImageType,TImageType>
	GradientFilterType;
	typedef itk::RescaleIntensityImageFilter<TImageType,TImageType>
	RescaleFilterType;
	typedef EdgeRemapImageFilter<TImageType> EdgeRemapFilterType;
	cvITKTemplateNewMacro(EdgeRemapFilterType,remapper);
	cvITKTemplateNewMacro(RescaleFilterType,rescaleInputFilter);

	rescaleInputFilter->SetOutputMinimum(0);
	rescaleInputFilter->SetOutputMaximum(1);
	remapper->SetKappa(kappa);
	remapper->SetExponent(exponent);

	try
	{
		// do gradient outside of the filter
		if(sigma > 0){
			typedef itk::GradientMagnitudeRecursiveGaussianImageFilter<TImageType,TImageType>
			GradientFilterType;

			cvITKTemplateNewMacro(GradientFilterType,gradFilter);
			gradFilter->SetInput(itkInputImage);
			gradFilter->SetSigma(sigma);
			rescaleInputFilter->SetInput(gradFilter->GetOutput());
			remapper->SetInput(rescaleInputFilter->GetOutput());
			remapper->Update();
			itkDeepCopy<TImageType>(remapper->GetOutput(),outputImage);
		}
		else if(sigma == 0)
		{
			typedef itk::GradientMagnitudeImageFilter<TImageType,TImageType>
			GradientFilterType;
			cvITKTemplateNewMacro(GradientFilterType,gradFilter);
			gradFilter->SetInput(itkInputImage);
			rescaleInputFilter->SetInput(gradFilter->GetOutput());
			remapper->SetInput(rescaleInputFilter->GetOutput());
			remapper->Update();
			itkDeepCopy<TImageType>(remapper->GetOutput(),outputImage);
		}
	}
	catch( itk::ExceptionObject & err )
	{
		std::cerr << "ExceptionObject caught !" << std::endl;
		std::cerr << "LevelSet Error! :"<<err << std::endl;
	}
	catch(const std::exception& ex)
	{
		std::cerr << "std exception caught !" << std::endl;
		std::cerr << "LevelSet Error! :"<<ex.what() << std::endl;
	}
	catch(...)
	{
		std::cerr << "Unknown Error!" << std::endl;
	}
}

template <typename TImageType>
void itkGenerateFeatureImage(TImageType* itkInputImage,
		typename TImageType::Pointer gradientImage, double sigma)
{
	try
	{
		// do gradient outside of the filter
		if(sigma > 0){

			typedef itk::GradientMagnitudeRecursiveGaussianImageFilter<TImageType,TImageType>
			GradientFilterType;

			cvITKTemplateNewMacro(GradientFilterType,gradFilter);
			gradFilter->SetInput(itkInputImage);
			gradFilter->SetSigma(sigma);
			gradFilter->Update();
			itkDeepCopy<TImageType>(gradFilter->GetOutput(),gradientImage);
		}
		else if(sigma == 0)
		{
			typedef itk::GradientMagnitudeImageFilter<TImageType,TImageType>
			GradientFilterType;
			cvITKTemplateNewMacro(GradientFilterType,gradFilter);
			gradFilter->SetInput(itkInputImage);
			gradFilter->Update();
			itkDeepCopy<TImageType>(gradFilter->GetOutput(),gradientImage);
		}
	}
	catch( itk::ExceptionObject & err )
	{
		std::cerr << "ExceptionObject caught !" << std::endl;
		std::cerr << "LevelSet Error! :"<<err << std::endl;
	}
	catch(const std::exception& ex)
	{
		std::cerr << "std exception caught !" << std::endl;
		std::cerr << "LevelSet Error! :"<<ex.what() << std::endl;
	}
	catch(...)
	{
		std::cerr << "Unknown Error!" << std::endl;
	}
}

template <typename TImageType>
void itkGenerateFeatureImageNoGrad(TImageType* itkInputImage,
		typename TImageType::Pointer outImage, double sigma)
{
	typedef itk::RescaleIntensityImageFilter<TImageType,TImageType>
	RescaleInputFilterType;
	try
	{
		// do gradient outside of the filter
		if(sigma > 0){

			typedef itk::DiscreteGaussianImageFilter<TImageType,TImageType>
			BlurImageFilterType;

			cvITKTemplateNewMacro(BlurImageFilterType,blurFilter);

			blurFilter->SetInput(itkInputImage);
			blurFilter->SetVariance(sigma);
			blurFilter->Update();
			cvITKTemplateNewMacro(RescaleInputFilterType,rescaleFilter);
			rescaleFilter->SetInput(blurFilter->GetOutput());
			rescaleFilter->SetOutputMinimum(-255);
			rescaleFilter->SetOutputMaximum(255);
			rescaleFilter->Update();
			itkDeepCopy<TImageType>(blurFilter->GetOutput(),outImage);
		}
		else if(sigma == 0)
		{
			cvITKTemplateNewMacro(RescaleInputFilterType,rescaleFilter);
			rescaleFilter->SetInput(itkInputImage);
			rescaleFilter->SetOutputMinimum(-255);
			rescaleFilter->SetOutputMaximum(255);
			rescaleFilter->Update();
			itkDeepCopy<TImageType>(itkInputImage,outImage);	
		}
	}
	catch( itk::ExceptionObject & err )
	{
		std::cerr << "ExceptionObject caught !" << std::endl;
		std::cerr << "LevelSet Error! :"<<err << std::endl;
	}
	catch(const std::exception& ex)
	{
		std::cerr << "std exception caught !" << std::endl;
		std::cerr << "LevelSet Error! :"<<ex.what() << std::endl;
	}
	catch(...)
	{
		std::cerr << "Unknown Error!" << std::endl;
	}
}

template <typename TImageType>
void itkGenerateFeatureImageDistance(TImageType* itkInputImage,
		typename TImageType::Pointer outImage, double thres){
	typedef itk::Image<float,TImageType::ImageDimension> floatImageType;
	typedef itk::SignedMaurerDistanceMapImageFilter<TImageType,TImageType> DistanceFilterType;
	typedef itk::RescaleIntensityImageFilter<TImageType,TImageType> RescaleInputFilterType;
	typedef itk::RescaleIntensityImageFilter<floatImageType,floatImageType> RescaleInputFilterType2;
	typedef itk::InvertIntensityImageFilter<TImageType,TImageType> InvertFilterType;
	typedef itk::AbsImageFilter<TImageType,TImageType> AbsFilterType;
	typedef itk::BinaryThresholdImageFilter<TImageType,TImageType> ThresholdFilterType;
	typedef itk::BinaryContourImageFilter<TImageType,TImageType> ContourFilterType;
	

	typedef itk::CastImageFilter<TImageType,floatImageType> InputCastType;
	typedef itk::CastImageFilter<floatImageType,TImageType> OutputCastType;

	cvITKTemplateNewMacro(RescaleInputFilterType,rescale1);
	rescale1->SetInput(itkInputImage);
	rescale1->SetOutputMinimum(0);
	rescale1->SetOutputMaximum(1);
	rescale1->Update();

	if (thres > 1)
		thres = .5;

	cvITKTemplateNewMacro(ThresholdFilterType,thresholdFilter);
	thresholdFilter->SetInput(rescale1->GetOutput());
	thresholdFilter->SetLowerThreshold(thres);
	thresholdFilter->SetUpperThreshold(1);
	thresholdFilter->Update();

	//cvITKLSUtil::WriteImage2(thresholdFilter->GetOutput(),"featbinary-image.mha");

	cvITKTemplateNewMacro(DistanceFilterType,distFilter);
	distFilter->SetInput(thresholdFilter->GetOutput());
	distFilter->SetInsideIsPositive(false);
	distFilter->Update();

	//cvITKLSUtil::WriteImage2(distFilter->GetOutput(),"dtform-image.mha");


	cvITKTemplateNewMacro(RescaleInputFilterType,rescaleFilter);
	rescaleFilter->SetInput(distFilter->GetOutput());
	rescaleFilter->SetOutputMinimum(0);
	rescaleFilter->SetOutputMaximum(255);
	rescaleFilter->Update();

	//cvITKLSUtil::WriteImage2(rescaleFilter->GetOutput(),"dtformrescale-image.mha");


	cvITKTemplateNewMacro(InvertFilterType,invertFilter);
	invertFilter->SetInput(rescaleFilter->GetOutput());
	invertFilter->SetMaximum(255);
	invertFilter->Update();

	//cvITKLSUtil::WriteImage2(invertFilter->GetOutput(),"dtinvert-image.mha");

	cvITKTemplateNewMacro(RescaleInputFilterType,rescale3);
	rescale3->SetInput(invertFilter->GetOutput());
	rescale3->SetOutputMinimum(0);
	rescale3->SetOutputMaximum(255);
	rescale3->Update();

	itkDeepCopy<TImageType>(rescale3->GetOutput(),outImage);
}

template <typename TImageType>
void itkGenerateFeatureImageThreshold(TImageType* itkInputImage,
		typename TImageType::Pointer outImage,double thres)
{
	typedef itk::RescaleIntensityImageFilter<TImageType,TImageType> RescaleInputFilterType;
	typedef itk::BinaryThresholdImageFilter<TImageType,TImageType> ThresholdFilterType;
	typedef itk::MultiplyImageFilter <TImageType, TImageType > MultiplyImageFilterType;

	cvITKTemplateNewMacro(RescaleInputFilterType,rescale1);
	rescale1->SetInput(itkInputImage);
	rescale1->SetOutputMinimum(0);
	rescale1->SetOutputMaximum(1);
	rescale1->Update();

	if (thres > 1)
		thres = .5;

	cvITKTemplateNewMacro(ThresholdFilterType,thresholdFilter);
	thresholdFilter->SetInput(rescale1->GetOutput());
	thresholdFilter->SetLowerThreshold(thres);
	thresholdFilter->SetUpperThreshold(1);
	thresholdFilter->Update();

	cvITKTemplateNewMacro(MultiplyImageFilterType,multiplyFilter);
	multiplyFilter->SetInput1(thresholdFilter->GetOutput());
	multiplyFilter->SetInput2(itkInputImage);
	multiplyFilter->Update();


	itkDeepCopy<TImageType>(thresholdFilter->GetOutput(),outImage);

}

/* ITK <-> VTK Methods*/
template < typename TImageType , typename TExternalImageType >
void vtk2itkRecastAndRescale(vtkStructuredPoints* vtkImage,
		typename TImageType::Pointer itkImage,
		ImgInfo* refInfo)
{
	typedef itk::RescaleIntensityImageFilter<TImageType,TImageType>
	RescaleFilterType;
	typedef itk::RescaleIntensityImageFilter<TExternalImageType,TImageType>
	RescaleInputFilterType;
	typedef typename TImageType::SpacingType SpacingType;

	typedef vtkTypeTraits<short> vtkExternalPixelType;
	vtkSmartPointer<vtkImageCast> vtkTypeGuard = vtkSmartPointer<vtkImageCast>::New();
	vtkTypeGuard->SetOutputScalarType(vtkExternalPixelType::VTKTypeID());
	vtkTypeGuard->SetInputData(vtkImage);
	vtkTypeGuard->Update();

	if(Debug){
		std::cout << "type: " <<
				vtkImage->GetScalarTypeAsString() << std::endl;
		std::cout << "type after guard: " <<
				vtkTypeGuard->GetOutput()->GetScalarTypeAsString() << std::endl;

	}
	if(Debug){
		double vtkSpacing[3];
		vtkImage->GetSpacing(vtkSpacing);
		std::cout << "feature: " ;
	}
	vtkSmartPointer<vtkImageChangeInformation> changer =
			vtkSmartPointer<vtkImageChangeInformation>::New();
	changer->SetInputData(vtkTypeGuard->GetOutput());
	changer->SetOutputOrigin(refInfo->GetOrigin());
	changer->SetOutputSpacing(refInfo->GetSpacing());
	changer->Update();

	MyUtGenerateVTK2ITKTemplateBridge(TExternalImageType,Ext2InFeat);
	vtkExporterExt2InFeat->SetInputData(changer->GetOutput());
	vtkExporterExt2InFeat->Update();
	itkImporterExt2InFeat->Update();

	cvITKTemplateNewMacro(RescaleInputFilterType,rescaleInputFilter);
	rescaleInputFilter->SetOutputMinimum(refInfo->GetMinValue());
	rescaleInputFilter->SetOutputMaximum(refInfo->GetMaxValue());
	rescaleInputFilter->SetInput(itkImporterExt2InFeat->GetOutput());

	rescaleInputFilter->UpdateLargestPossibleRegion();
	itkDeepCopy(rescaleInputFilter->GetOutput(),itkImage);

}

template < typename TImageType , typename TExternalImageType >
void itk2vtkRecastAndRescale(TImageType* itkImage,
		vtkStructuredPoints* vtkImage,
		ImgInfo* refInfo)
{
	typedef itk::RescaleIntensityImageFilter<TImageType,TExternalImageType>
	RescaleOutputFilterType;
	cvITKTemplateNewMacro(RescaleOutputFilterType,rescaleOutputFilter);
	rescaleOutputFilter->SetOutputMinimum(refInfo->GetMinValue());
	rescaleOutputFilter->SetOutputMaximum(refInfo->GetMaxValue());
	rescaleOutputFilter->SetInput(itkImage);

	MyUtGenerateITKTemplate2VTKBridge(TExternalImageType,In2ExtFeat)
	itkExporterIn2ExtFeat->SetInput(rescaleOutputFilter->GetOutput());
	itkExporterIn2ExtFeat->Update();
	vtkImporterIn2ExtFeat->Update();

	vtkSmartPointer<vtkImageChangeInformation> changer =
			vtkSmartPointer<vtkImageChangeInformation>::New();
	changer->SetInputData(vtkImporterIn2ExtFeat->GetOutput());
	changer->SetOutputOrigin(refInfo->GetOrigin());
	changer->SetOutputSpacing(refInfo->GetSpacing());
	changer->Update();


	vtkImage->DeepCopy(changer->GetOutput());

}

template <typename TImageType,typename TExternalImageType>
int vtkGenerateFeatureImage(vtkStructuredPoints* vtkInputImage,
		vtkStructuredPoints* vtkOuputImage,
		ImgInfo* refInfo,double sigma)
{
	typename TImageType::Pointer tempImg =  TImageType::New();
	vtk2itkRecastAndRescale<TImageType,TExternalImageType>(vtkInputImage,tempImg,
			refInfo);

	typename TImageType::Pointer tempImg2 = TImageType::New();
	itkGenerateFeatureImage<TImageType>(tempImg, tempImg2, sigma);

	// Debug only, vtk image not used
	ImgInfo outInfo = ImgInfo(vtkInputImage);
	itk2vtkRecastAndRescale<TImageType,TExternalImageType>(tempImg2,vtkOuputImage,&outInfo);

	return 1;
}

template <typename TImageType,typename TExternalImageType>
int vtkGenerateFeatureImageNoGrad(vtkStructuredPoints* vtkInputImage,
		vtkStructuredPoints* vtkOuputImage,
		ImgInfo* refInfo,double sigma)
{
	typename TImageType::Pointer tempImg =  TImageType::New();
	vtk2itkRecastAndRescale<TImageType,TExternalImageType>(vtkInputImage,tempImg,
			refInfo);

	typename TImageType::Pointer tempImg2 = TImageType::New();
	itkGenerateFeatureImageNoGrad<TImageType>(tempImg, tempImg2, sigma);

	// Debug only, vtk image not used
	ImgInfo outInfo = ImgInfo(vtkInputImage);
	itk2vtkRecastAndRescale<TImageType,TExternalImageType>(tempImg2,vtkOuputImage,&outInfo);

	return 1;
}

template <typename TImageType,typename TExternalImageType>
int vtkGenerateFeatureImageDistance(vtkStructuredPoints* vtkInputImage,
		vtkStructuredPoints* vtkOuputImage,
		ImgInfo* refInfo,double thres)
{
	typename TImageType::Pointer tempImg =  TImageType::New();
	vtk2itkRecastAndRescale<TImageType,TExternalImageType>(vtkInputImage,tempImg,
			refInfo);

	typename TImageType::Pointer tempImg2 = TImageType::New();
	itkGenerateFeatureImageDistance<TImageType>(tempImg, tempImg2,thres);

	// Debug only, vtk image not used
	ImgInfo outInfo = ImgInfo(vtkInputImage);
	itk2vtkRecastAndRescale<TImageType,TExternalImageType>(tempImg2,vtkOuputImage,&outInfo);

	return 1;
}

template <typename TImageType,typename TExternalImageType>
int vtkGenerateEdgeProxImage(vtkStructuredPoints* vtkInputImage,
		vtkStructuredPoints* vtkOuputImage,
		ImgInfo* refInfo,double sigma,double kappa, double exponent)
{
	typename TImageType::Pointer tempImg =  TImageType::New();
	vtk2itkRecastAndRescale<TImageType,TExternalImageType>(vtkInputImage,tempImg,
			refInfo);

	typename TImageType::Pointer tempImg2 = TImageType::New();
	itkGenerateEdgeProxImage<TImageType>(tempImg, tempImg2, sigma,kappa,exponent);

	// Debug only, vtk image not used
	ImgInfo outInfo = ImgInfo(vtkInputImage);
	itk2vtkRecastAndRescale<TImageType,TExternalImageType>(tempImg2,vtkOuputImage,&outInfo);

	return 1;
}

template <typename TImageType,typename TExternalImageType>
int vtkGenerateFeatureImageThreshold(vtkStructuredPoints* vtkInputImage,
		vtkStructuredPoints* vtkOuputImage,
		ImgInfo* refInfo,double thres)
{
	typename TImageType::Pointer tempImg =  TImageType::New();
	vtk2itkRecastAndRescale<TImageType,TExternalImageType>(vtkInputImage,tempImg,
			refInfo);

	typename TImageType::Pointer tempImg2 = TImageType::New();
	itkGenerateFeatureImageThreshold<TImageType>(tempImg, tempImg2, thres);

	// Debug only, vtk image not used
	ImgInfo outInfo = ImgInfo(vtkInputImage);
	itk2vtkRecastAndRescale<TImageType,TExternalImageType>(tempImg2,vtkOuputImage,&outInfo);

	return 1;
}

template < typename TImageType, typename TExternalImageType >
void vtk2itkBinaryImageToSeedImage(vtkStructuredPoints* vtkImg,
		typename TImageType::Pointer itkImage,ImgInfo* internalInfo)
{
	// 	ImgInfo extInfo = ImgInfo(vtkImg);
	//Self::vtkPolyDataToImage(vtkPd,vtkImg,extInfo);
	typedef itk::SignedMaurerDistanceMapImageFilter<TExternalImageType,
			TImageType> DistanceFilterType;

	vtkSmartPointer<vtkImageChangeInformation> changer =
			vtkSmartPointer<vtkImageChangeInformation>::New();
	changer->SetInputData(vtkImg);
	changer->SetOutputOrigin(internalInfo->GetOrigin());
	changer->SetOutputSpacing(internalInfo->GetSpacing());
	changer->Update();


	MyUtGenerateVTK2ITKTemplateBridge(TExternalImageType,);
	vtkExporter->SetInputData(changer->GetOutput());
	vtkExporter->Update();
	itkImporter->Update();

	cvITKTemplateNewMacro(DistanceFilterType,initialContour);
	initialContour->SetUseImageSpacing(false);
	initialContour->SetInsideIsPositive(false);
	initialContour->SetInput(itkImporter->GetOutput());
	initialContour->Update();

	if(TImageType::ImageDimension == 2){
		if(Debug){
			QuickView viewer;
			bool flip = false;

			viewer.AddImage(itkImporter->GetOutput(),
					flip,
					"itkImporter");
			viewer.AddImage(initialContour->GetOutput(),
					flip,
					"initialContour");
			viewer.Visualize();
		}
	}


	//copy seed to Output Image
	// TODO: Try this without copying the image!
	itkDeepCopy(initialContour->GetOutput(),itkImage);
}




template<typename ITKImageType>
void CreateImage(typename ITKImageType::Pointer image,
		typename ITKImageType::SizeType size,
		typename ITKImageType::SpacingType spacing,
		typename ITKImageType::PointType origin,
		typename ITKImageType::PixelType value)
{
	typename ITKImageType::IndexType start;
	start.Fill(0);

	typename ITKImageType::RegionType region(start,size);

	image->SetRegions(region);
	image->Allocate();
	image->FillBuffer(value);

	image->SetSpacing(spacing);
	image->SetOrigin(origin);
}

template <typename TImage>
void itkDeepCopy(typename TImage::Pointer input,
		typename TImage::Pointer output)
{
	output->SetRegions(input->GetLargestPossibleRegion());
	output->Allocate();

	itk::ImageRegionConstIterator<TImage> inputIterator(input,
			input->GetLargestPossibleRegion());
	itk::ImageRegionIterator<TImage> outputIterator(output,
			output->GetLargestPossibleRegion());

	while(!inputIterator.IsAtEnd())
	{
		outputIterator.Set(inputIterator.Get());
		++inputIterator;
		++outputIterator;
	}
	output->CopyInformation(input);
}

template <typename TImageType>
void itkDeepCopy(const TImageType* input,
		typename TImageType::Pointer output)
{
	output->SetRegions(input->GetLargestPossibleRegion());
	output->Allocate();

	itk::ImageRegionConstIterator<TImageType> inputIterator(input,
			input->GetLargestPossibleRegion());
	itk::ImageRegionIterator<TImageType> outputIterator(output,
			output->GetLargestPossibleRegion());

	while(!inputIterator.IsAtEnd())
	{
		outputIterator.Set(inputIterator.Get());
		++inputIterator;
		++outputIterator;
	}

	output->CopyInformation(input);

}
template <typename TImageType>
void itkDeepCopy(const TImageType* input,TImageType* output)
{
	output->SetRegions(input->GetLargestPossibleRegion());
	output->Allocate();

	itk::ImageRegionConstIterator<TImageType> inputIterator(input,
			input->GetLargestPossibleRegion());
	itk::ImageRegionIterator<TImageType> outputIterator(output,
			output->GetLargestPossibleRegion());

	while(!inputIterator.IsAtEnd())
	{
		outputIterator.Set(inputIterator.Get());
		++inputIterator;
		++outputIterator;
	}
	output->CopyInformation(input);

}

template <typename TImageType>
void itk2vtkRecast(TImageType* itkImage,
		vtkStructuredPoints* vtkImage,
		ImgInfo* refInfo)
{
	vtkSmartPointer<vtkStructuredPoints> vtkTemp =
			vtkSmartPointer<vtkStructuredPoints>::New();
	CopyITKtoVTK< TImageType >(itkImage, vtkTemp);

	vtkSmartPointer<vtkImageChangeInformation> changer =
			vtkSmartPointer<vtkImageChangeInformation>::New();
	changer->SetInputData(vtkTemp);
	changer->SetOutputOrigin(refInfo->GetOrigin());
	changer->SetOutputSpacing(refInfo->GetSpacing());
	changer->Update();


	vtkImage->DeepCopy(changer->GetOutput());
}




template <typename TImageType>
void CopyITKtoVTK(const TImageType* in,vtkStructuredPoints* out)
{

	MyUtGenerateITKTemplate2VTKBridge(TImageType,);
	itkExporter->SetInput(in);
	itkExporter->Update();
	vtkImporter->Update();
	out->DeepCopy(vtkImporter->GetOutput());

}


template <typename TImage>
void WriteImage(const TImage* input,string FilenameBase)
{
	typedef typename itk::ImageFileWriter<TImage> WriterType;
	typename WriterType::Pointer writer = WriterType::New();
	std::stringstream sstm;
	sstm  << FilenameBase;
	sstm  << "-itk";
	sstm << "." << "tiff";
	writer->SetInput(input);
	writer->SetFileName(sstm.str());
	writer->Update();
}

template <typename TImage>
void WriteImage2(const TImage* input,string FilenameBase)
{
	typedef typename itk::ImageFileWriter<TImage> WriterType;
	typename WriterType::Pointer writer = WriterType::New();
	std::stringstream sstm;
	sstm  << FilenameBase;
	writer->SetInput(input);
	writer->SetFileName(sstm.str());
	writer->Update();
}

template <typename TImage>
static void WritePNGImage(const TImage* input,string FilenameBase)
{
	try{
		typedef typename itk::ImageFileWriter<TImage> WriterType;
		typename WriterType::Pointer writer = WriterType::New();
		std::stringstream sstm;
		sstm  << FilenameBase;
		sstm << "." << "png";
		writer->SetInput(input);
		writer->SetFileName(sstm.str());
		writer->Update();
	}
	catch( itk::ExceptionObject & err )
	{
		std::cerr << "ExceptionObject caught !" << std::endl;
		std::cerr << "Write Error! :"<<err << std::endl;
	}
}


} // namespace
#endif /* CVLEVELSETUTILS_HXX_ */
