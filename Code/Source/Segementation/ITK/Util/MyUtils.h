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
 * MyUtils.h
 *
 *  Created on: Nov 7, 2013
 *      Author: jmerkow
 */

#ifndef _MYUTILS_H_
#define _MYUTILS_H_

#include <stdio.h>
#include <string.h>
#include <iostream>
#include <sstream>
#include <fstream>
#include <algorithm>
#include <iterator>
#include <iomanip>

#include "itkImage.h"
#include "itkImageRegionIterator.h"

#include "itkTileImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkAbsImageFilter.h"


#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"

#include "itkAbsImageFilter.h"
//Extract Slice Includes
#include "itkExtractImageFilter.h"
//Level Boundary Stuff
#include "itkBinaryThresholdImageFilter.h"
#include "itkBinaryContourImageFilter.h"

//Overlay
#include "itkBinaryImageToLabelMapFilter.h"
#include "itkImageRegionIterator.h"
#include "itkLabelMapToLabelImageFilter.h"
#include "itkLabelOverlayImageFilter.h"
#include "itkRGBPixel.h"

//Initial Image
#include "itkBinaryBallStructuringElement.h"
#include "itkBinaryDilateImageFilter.h"

#include "itkChangeInformationImageFilter.h"

#include "ConnectVTKITK.h"
#include "itkVTKImageExport.h"
#include "itkVTKImageImport.h"
#include "vtkImageImport.h"
#include "vtkImageExport.h"

#include "vtkSmartPointer.h"
#include "vtkStructuredPoints.h"


#include <itkImage.h>
#include <itkImageToVTKImageFilter.h>

#include "vtkVersion.h"
#include "vtkImageViewer.h"
#include "vtkSmartPointer.h"
#include "vtkProperty.h"

#include "vtkPoints.h"
#include "vtkParametricSpline.h"
#include <vtkCardinalSpline.h>
#include <vtkPolyData.h>
#include <vtkPolyDataMapper.h>

#include "vtkRenderer.h"
#include "vtkRenderWindow.h"
#include "vtkRenderWindowInteractor.h"

#include <vtkTubeFilter.h>

#include "vtkActor.h"

#include "vtkSphereSource.h"
#include "vtkGlyph3D.h"
#include "vtkPolyDataMapper.h"
#include "vtkCellArray.h"

#include <vtkMath.h>
#include "../Include/cvVTKMacros.h"


#include "vtkImageReader2.h"
#include "vtkMatrix4x4.h"

#include "vtkImageReslice.h"
#include "vtkLookupTable.h"

#include "vtkImageImport.h"




namespace MyUt{
typedef itk::Image<double,2> Double2dITKImageType;
typedef itk::Image<float,2> Float2dITKImageType;

template<typename TImage>
void CreateImage(typename TImage::Pointer image,typename TImage::SizeType size,
		typename TImage::SpacingType spacing, typename TImage::PointType origin,
		typename TImage::PixelType value);

template<typename TVectorImage>
void CreateImageVectorImage(typename TVectorImage::Pointer image,typename TVectorImage::PixelType value,typename TVectorImage::SizeType size,
		typename TVectorImage::SpacingType spacing, typename TVectorImage::PointType origin);

template<typename TVectorImage>
void CreateImageVectorImage(typename TVectorImage::Pointer image,typename TVectorImage::PixelType value);

template<typename TImage>
void SetImagePropoties(typename TImage::Pointer image,
		typename TImage::SpacingType spacing, typename TImage::PointType origin);

template<typename TImage>
void DeepCopy(const TImage* input, typename TImage::Pointer output);

template<typename TImage>
void DeepCopy(typename TImage::Pointer input, typename TImage::Pointer output);

template <class T>
std::vector<T> &split(const std::string &s, char delim, std::vector<T> &elems);

template <class T>
std::vector<T> split(const std::string &s, char delim);

//template<typename T>
//void print_vector(typename std::vector<std::vector<T> > &vec,const char* end);
//
//template<typename T>
//void print_vector(typename std::vector<T> &vec,const char* end);

template<typename T>
void printvec(typename std::vector<T> vec);

template<typename T>
void printArray(T *arr,unsigned int length);

void getParams(float params[6],const char* filename);

std::vector<std::vector<float> > getParams(const char* filename);

std::vector<std::vector<float> > parsePathsFile(const char* filename);

bool isInteger(const std::string & s);

bool isNumber(const char* s);

template<typename T>
void make2DSeriesTo3D(std::string FilenameBase,
		std::string FilenameExt,std::string out_filename, int start,int end);

template<typename TvtkObject>
void vtkResliceImage(vtkSmartPointer<TvtkObject> reader,
		vtkSmartPointer<vtkImageReslice> reslice,double axes[16]);


void DeepCopy(Double2dITKImageType::Pointer input,Double2dITKImageType::Pointer output);
void CopyVTKtoITK(vtkStructuredPoints* in,Double2dITKImageType* out);
void CopyITKtoVTK(const Double2dITKImageType* in,vtkStructuredPoints* out);

/** VTK Utils***/

template<typename TvtkObject>
void vtkResliceImage(vtkSmartPointer<TvtkObject>,
		vtkSmartPointer<vtkImageReslice> reslice);

void vtkResliceImage(vtkSmartPointer<vtkImageImport> importer,
		vtkSmartPointer<vtkImageReslice> reslice,double axes[16]);

void vtkSetupResliceImage(vtkSmartPointer<vtkImageReslice> reslice,
		vtkSmartPointer<vtkPoints> path,
		int ii);

void vtkVecToPoints(vtkSmartPointer<vtkPoints> points, std::vector<std::vector<float> > pts);

vtkSmartPointer<vtkPoints> vtkInterpolatePath(vtkSmartPointer<vtkPoints> points, int numberOfOutputPoints);

void vtkInterpolatePath(vtkSmartPointer<vtkPoints> points,vtkSmartPointer<vtkPoints> path,int numberOfOutputPoints);

void RenderPathSetup(vtkSmartPointer<vtkPoints> points,vtkSmartPointer<vtkPoints> path,
		vtkSmartPointer<vtkActor> profile,vtkSmartPointer<vtkActor> glyph,double ball[3], float tubearg=0.5);

void QuickRenderPolyData(vtkPolyData* front,float radius = .5);


/** Inline methods **/
int inline recast_string(std::string str,int m)
{return atoi(str.c_str());}

float inline recast_string(std::string str, float m){
	return atof(str.c_str());
}

inline void printArray(int *arr,unsigned int length)
{printArray<int>(arr,length);}

inline void printArray(double *arr,unsigned int length)
{printArray<double>(arr,length);}

inline void RenderPathSetup(vtkSmartPointer<vtkPoints> points,vtkSmartPointer<vtkPoints> path,
		vtkSmartPointer<vtkActor> profile,vtkSmartPointer<vtkActor> glyph, float tubearg=0.5)
{
	double ball[3] = {.8,10,10};
	RenderPathSetup(points,path,profile, glyph,ball,tubearg);
}

inline void cross( float x[3],  float y[3], float z[3])
{
	float Zx = x[1] * y[2] - x[2] * y[1];
	float Zy = x[2] * y[0] - x[0] * y[2];
	float Zz = x[0] * y[1] - x[1] * y[0];
	z[0] = Zx; z[1] = Zy; z[2] = Zz;
}

inline void cross(double x[3],  double y[3], double z[3])
{
	float Zx = x[1] * y[2] - x[2] * y[1];
	float Zy = x[2] * y[0] - x[0] * y[2];
	float Zz = x[0] * y[1] - x[1] * y[0];
	z[0] = Zx; z[1] = Zy; z[2] = Zz;
}

inline void MakeResliceAxes(double n[3],double p[3],double axes[16]){

	vtkMath::Normalize(n);
	double v1[3];// = {n[0],-n[1],0};
	double v2[3];
	double pi2 = (vtkMath::Pi()*.5);
	vtkMath::Perpendiculars(n,v1,v2,pi2);
	vtkMath::Normalize(v1);

	double dot = v1[0] * v2[0] + v1[1] * v2[1] + v1[2] * v2[2];

	std::cout << "dot: " << dot <<std::endl;

	//cross(v1,n,v2);
	vtkMath::Normalize(v2);

	std::cout << "v1 :" ;
	MyUt::printArray(v1,3);
	std::cout << "v2 :" ;
	MyUt::printArray(v2,3);
	std::cout << "n :" ;
	MyUt::printArray(n,3);

	axes[0] =v2[0]; axes[1] = -v1[0]; axes[2]  = -n[0];	axes[3] = p[0];
	axes[4] =v2[1]; axes[5] = -v1[1]; axes[6]  = -n[1];	axes[7] = p[1];
	axes[8] =v2[2]; axes[9] = -v1[2]; axes[10] = -n[2];	axes[11] = p[2];
	axes[12] =	0;	axes[13] =	0;	 axes[14] =		0;	axes[15] = 	  1;
}

inline void MakeResliceAxesFromPoints(double p1[3],double p2[3],double axes[16]){

	double n[3];
	vtkMath::Subtract(p2,p1,n);
	vtkMath::Normalize(n);
	MakeResliceAxes(n,p1, axes);


}

inline void printAxes(double axes[16]){
	std::cout << "\t\t" << "Axes:"<< std::endl;
	std::cout << "\t\t" << std::setprecision(4) << axes[0] << "\t\t" << axes[1] << "\t\t"  << axes[2] << "\t\t"  << axes[3] << std::endl;
	std::cout << "\t\t" << std::setprecision(4) << axes[4] << "\t\t" << axes[5] << "\t\t"  << axes[6] << "\t\t"  << axes[7] << std::endl;
	std::cout << "\t\t" << std::setprecision(4) << axes[8] << "\t\t" << axes[9] << "\t\t"  << axes[10] << "\t\t"  << axes[11] << std::endl;
	std::cout << "\t\t" << std::setprecision(4) << axes[12] << "\t\t" << axes[13] << "\t\t"  << axes[14] << "\t\t"  << axes[15] << std::endl;
}

inline void SetupRender(vtkSmartPointer<vtkRenderer> renderer,
		vtkSmartPointer<vtkRenderWindow> renderWindow,
		vtkSmartPointer<vtkRenderWindowInteractor> renderWindowInteractor)
{

}

inline void printArrays(int extent[6],double spacing[3],double origin[3],double center[3])
{
	std::cout << "extent: ";
	MyUt::printArray(extent,6);
	std::cout << "spacing: ";
	MyUt::printArray(spacing,3);
	std::cout << "origin: ";
	MyUt::printArray(origin,3);
	center[0] = origin[0] + spacing[0] * 0.5 * (extent[0] + extent[1]);
	center[1] = origin[1] + spacing[1] * 0.5 * (extent[2] + extent[3]);
	center[2] = origin[2] + spacing[2] * 0.5 * (extent[4] + extent[5]);

	std::cout << "center: ";
	MyUt::printArray(center,3);

}

inline void printArrays(double bounds[6],double spacing[3],double origin[3])
{
	std::cout << "bounds: ";
	MyUt::printArray(bounds,6);
	std::cout << "spacing: ";
	MyUt::printArray(spacing,3);
	std::cout << "origin: ";
	MyUt::printArray(origin,3);


}

inline void TransformIndexToPhysical(double spacing[3],double origin[3],
		double in[3], double out[3])
{
	out[0] = origin[0] + spacing[0] * in[0];
	out[1] = origin[1] + spacing[1] * in[1];
	out[2] = origin[2] + spacing[2] * in[2];
}

inline void TransformPhysicalToIndex(double spacing[3],double origin[3],
		double in[3], int out[3])
{
	out[0] = vtkMath::Round((in[0] - origin[0])/spacing[0]);
	out[1] = vtkMath::Round((in[1] - origin[1])/spacing[1]);
	out[2] = vtkMath::Round((in[2] - origin[2])/spacing[2]);
}

}//namespace

namespace MyUt{

template <class T>
std::vector<T> &split(const std::string &s, char delim, std::vector<T> &elems) {
	std::stringstream ss(s);
	std::string item;
	T item_casted;
	while (std::getline(ss, item, delim)) {
		elems.push_back(recast_string(item,item_casted));
	}
	return elems;
}

template <class T>
std::vector<T> split(const std::string &s, char delim) {
	std::vector<T> elems;
	split<T>(s, delim, elems); return elems;
}

template<typename T>
void printvec(std::vector<T> vec){
	for( typename std::vector<T>::const_iterator i =vec.begin();
			i != vec.end();
			++i)
		std::cout << *i << ' ';
}

template<typename T>
void printArray(T *arr,unsigned int length)
{
	for(int i = 0;i<length;i++)
		std::cout << arr[i] << " ";

	std::cout <<std::endl;
}

template<typename TImage>
void DeepCopy(typename TImage::Pointer input, typename TImage::Pointer output)
{
	output->SetRegions(input->GetLargestPossibleRegion());
	output->Allocate();

	itk::ImageRegionConstIterator<TImage> inputIterator(input, input->GetLargestPossibleRegion());
	itk::ImageRegionIterator<TImage> outputIterator(output, output->GetLargestPossibleRegion());

	while(!inputIterator.IsAtEnd())
	{
		outputIterator.Set(inputIterator.Get());
		++inputIterator;
		++outputIterator;
	}
}

template<typename TImage>
void DeepCopy(const TImage* input, typename TImage::Pointer output)
{
	output->SetRegions(input->GetLargestPossibleRegion());
	output->Allocate();

	itk::ImageRegionConstIterator<TImage> inputIterator(input, input->GetLargestPossibleRegion());
	itk::ImageRegionIterator<TImage> outputIterator(output, output->GetLargestPossibleRegion());

	while(!inputIterator.IsAtEnd())
	{
		outputIterator.Set(inputIterator.Get());
		++inputIterator;
		++outputIterator;
	}
}

template<typename TImage>
void CreateImage(typename TImage::Pointer image,typename TImage::SizeType size,
		typename TImage::SpacingType spacing, typename TImage::PointType origin,
		typename TImage::PixelType value)
{
	typename TImage::IndexType start;
	start.Fill(0);


	typename TImage::RegionType region(start,size);

	image->SetRegions(region);
	image->Allocate();
	image->FillBuffer(value);

	SetImagePropoties<TImage>(image,spacing,origin);
}

template<typename TVectorImage>
void CreateImageVectorImage(typename TVectorImage::Pointer image,typename TVectorImage::PixelType value,typename TVectorImage::SizeType size,
		typename TVectorImage::SpacingType spacing, typename TVectorImage::PointType origin)
{

	typename TVectorImage::IndexType start;
	start.Fill(0);


	typename TVectorImage::RegionType region;
	region.SetSize(size);
	region.SetIndex(start);

	image->SetRegions(region);
	image->SetNumberOfComponentsPerPixel(2);
	image->Allocate();

	typename TVectorImage::PixelType variableLengthVector;

	for(int i = 0; i <variableLengthVector.Size();i++)
		variableLengthVector[i] = value[i];

	itk::ImageRegionIterator<TVectorImage> imageIterator(image,image->GetLargestPossibleRegion());


	while(!imageIterator.IsAtEnd())
	{
		imageIterator.Set(variableLengthVector);

		++imageIterator;
	}

	SetImagePropoties<TVectorImage>(image,spacing,origin);
}

template<typename TVectorImage>
void CreateImageVectorImage(typename TVectorImage::Pointer image,typename TVectorImage::PixelType value)
{

	typename TVectorImage::SizeType size;
	size.Fill(2);

	typename TVectorImage::SpacingType spacing;
	spacing.Fill(1);

	typename TVectorImage::PointType origin;
	origin.Fill(0);

	CreateImageVectorImage<TVectorImage>(image,value,size,spacing,origin);

}

template<typename TImage>
void SetImagePropoties(typename TImage::Pointer image,
		typename TImage::SpacingType spacing, typename TImage::PointType origin)
{
	image->SetSpacing(spacing);
	image->SetOrigin(origin);
}
/*
 * try{
		gradientMagFilter->SetInput(itkImporter->GetOutput());
		gradientMagFilter->SetSigma(1.0);
		gradientMagFilter->Update();
		MyUt::WriteImage<itkImportType::OutputImageType>(gradientMagFilter->GetOutput(),"vtkSlices/vtkSliceGradient",".tiff",pathId);
		std::cout << "Gradient Written" << std::endl;
		}
		catch(itk::ExceptionObject &e)
		{
			std::cerr << e.what() << std::endl;
			return 0;
		}
 *
 */

template<typename T>
void make2DSeriesTo3DGradient(std::string FilenameBase,
		std::string FilenameExt,std::string out_filename, int start,int end){
	typedef T PixelType;
	typedef itk::Image<T,2> InternalImageType;
	typedef itk::Image<T,3> ThreeDImageType;
	typedef itk::ImageFileReader<InternalImageType > ImageReaderType2d;
	typedef itk::TileImageFilter<InternalImageType, ThreeDImageType> TilerType;
	typedef itk::ImageFileWriter<ThreeDImageType> WriterType2;
	typedef itk::GradientMagnitudeRecursiveGaussianImageFilter<InternalImageType,InternalImageType> GradientMagFilterType;

	typename TilerType::Pointer tiler = TilerType::New();
	typename GradientMagFilterType::Pointer gradientMagFilter = GradientMagFilterType::New();
	itk::FixedArray< unsigned int, 3 > layout;
	layout[0] = 1;
	layout[1] = 1;
	layout[2] = 0;
	tiler->SetLayout( layout );
	unsigned int inputImageNumber = 0;
	typename ImageReaderType2d::Pointer reader2d = ImageReaderType2d::New();




	typename InternalImageType::Pointer inputImageTile;
	//	std::cout << Observer->GetCurrentIteration() << std::endl;
	for (int i = start; i < end; i++)
	{
		std::stringstream sstm;

		sstm  << FilenameBase;
		sstm << std::setw(5) << std::setfill('0') << i << FilenameExt;
		std::cout <<sstm.str() << std::endl;



		reader2d->SetFileName( sstm.str() );
		reader2d->UpdateLargestPossibleRegion();



		std::cout << reader2d->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;

		gradientMagFilter->SetInput(reader2d->GetOutput());
		gradientMagFilter->SetSigma(1.0);
		gradientMagFilter->Update();

		inputImageTile = gradientMagFilter->GetOutput();
		inputImageTile->DisconnectPipeline();
		tiler->SetInput( i, inputImageTile );
	}
	PixelType filler = 0;
	tiler->SetDefaultPixelValue( filler );
	tiler->Update();
	typename WriterType2::Pointer writer3d = WriterType2::New();
	writer3d->SetFileName(out_filename.c_str());
	writer3d->SetInput(tiler->GetOutput());
	writer3d->Update();
}

template<typename T>
void make2DSeriesTo3D(std::string FilenameBase,
		std::string FilenameExt,std::string out_filename, int start,int end){
	typedef T PixelType;
	typedef itk::Image<T,3> InternalImageType;
	typedef itk::Image<T,3> ThreeDImageType;
	typedef itk::ImageFileReader<InternalImageType > ImageReaderType2d;
	typedef itk::TileImageFilter<InternalImageType, ThreeDImageType > TilerType;
	typedef itk::ImageFileWriter<ThreeDImageType> WriterType2;
	typename TilerType::Pointer tiler = TilerType::New();
	itk::FixedArray< unsigned int, 3 > layout;
	layout[0] = 1;
	layout[1] = 1;
	layout[2] = 0;
	tiler->SetLayout( layout );
	unsigned int inputImageNumber = 0;
	typename ImageReaderType2d::Pointer reader2d = ImageReaderType2d::New();
	typename InternalImageType::Pointer inputImageTile;
	//	std::cout << Observer->GetCurrentIteration() << std::endl;
	for (int i = start; i < end; i++)
	{
		std::stringstream sstm;

		sstm  << FilenameBase;
		sstm << std::setw(5) << std::setfill('0') << i << FilenameExt;
		//	std::cout <<sstm.str() << std::endl;



		reader2d->SetFileName( sstm.str() );
		reader2d->UpdateLargestPossibleRegion();
		inputImageTile = reader2d->GetOutput();
		inputImageTile->DisconnectPipeline();
		tiler->SetInput( i, inputImageTile );
	}
	PixelType filler = 0;
	tiler->SetDefaultPixelValue( filler );
	tiler->Update();
	typename WriterType2::Pointer writer3d = WriterType2::New();
	writer3d->SetFileName(out_filename.c_str());
	writer3d->SetInput(tiler->GetOutput());
	writer3d->Update();
}

template<typename TInternalImageType>
void WriteImage(const TInternalImageType * image,
		std::string FilenameBase,
		std::string FilenameExt,
		int FileId){
	typedef TInternalImageType InternalImageType;
	typedef typename itk::ImageFileWriter<InternalImageType> WriterType;
	std::stringstream sstm;
	sstm  << FilenameBase;
	sstm << std::setw(5) << std::setfill('0') << FileId ;
	sstm<< FilenameExt;
	typename WriterType::Pointer writer = WriterType::New();
	writer->SetFileName(sstm.str());

	typename InternalImageType::Pointer inputImage =
			InternalImageType::New();

	//MyUt::DeepCopy<InternalImageType>(image,inputImage);
	//inputImage->DisconnectPipeline();
	writer->SetInput(image);
	writer->Update();
}



// Templated Definitions
template<typename TInternalImageType>
void make2DSeriesTo3DAbsOverlay(TInternalImageType * image, std::string FilenameBase,
		std::string FilenameExt,std::string out_filename, int start,int end){

	typedef unsigned char UCPixelType;
	typedef itk::Image<UCPixelType,2> UCImageType;

	typedef itk::RGBPixel<UCPixelType> RGBPixelType;
	typedef itk::Image<RGBPixelType,2> RGBImageType2d;
	typedef itk::Image<RGBPixelType,3> RGBImageType3d;

	typedef itk::RGBPixel<UCPixelType> RGBPixelType;
	typedef itk::Image<RGBPixelType,2> RGBImageType2d;
	typedef itk::Image<RGBPixelType,3> RGBImageType3d;


	typedef TInternalImageType ImageType2d;
	typedef typename ImageType2d::PixelType PixelType;

	typedef itk::ChangeInformationImageFilter<ImageType2d>  CenterFilterType;


	typedef typename itk::ImageFileReader<ImageType2d > ImageReaderType2d;
	typedef typename itk::TileImageFilter<RGBImageType2d, RGBImageType3d> TilerType;

	typedef typename itk::AbsImageFilter <ImageType2d, ImageType2d> AbsImageFilterType;
	typedef typename itk::BinaryThresholdImageFilter<ImageType2d,ImageType2d> thresholdFilterType;
	typedef typename itk::BinaryContourImageFilter<ImageType2d,ImageType2d> ContourFilterType;

	//	typedef typename itk::BinaryBallStructuringElement<ImageType2d> StructuringElementType;
	//	typedef typename itk::BinaryDilateImageFilter<ImageType2d,ImageType2d,StructuringElementType> BinaryDilateImageFilterType;

	typedef typename itk::RescaleIntensityImageFilter<ImageType2d,UCImageType> RecastFilterType;
	typedef typename itk::BinaryImageToLabelMapFilter<UCImageType> BinaryToLabelMapFilterType;
	typedef typename itk::LabelMapToLabelImageFilter<BinaryToLabelMapFilterType::OutputImageType, UCImageType> LabelMapToLabelImageFilterType;
	typedef typename itk::LabelOverlayImageFilter<ImageType2d, UCImageType, RGBImageType2d> LabelOverlayImageFilterType;
	typedef typename itk::RescaleIntensityImageFilter<ImageType2d,ImageType2d> RescaleFilterType;

	typedef typename itk::ExtractImageFilter<ImageType2d,ImageType2d> ExtractImageFilterType;
	typedef typename itk::ImageFileWriter<RGBImageType3d> RGBWriterType;


	typename thresholdFilterType::Pointer thresholdFilter = thresholdFilterType::New();
	thresholdFilter->SetLowerThreshold(-1000);
	thresholdFilter->SetUpperThreshold(0);
	thresholdFilter->SetInsideValue(1);

	typename RecastFilterType::Pointer recastUChar = RecastFilterType::New();
	recastUChar->SetOutputMinimum(0);
	recastUChar->SetOutputMaximum(255);

	typename BinaryToLabelMapFilterType::Pointer binaryToLabelMap = BinaryToLabelMapFilterType::New();
	typename LabelMapToLabelImageFilterType::Pointer labelMaptoImage = LabelMapToLabelImageFilterType::New();
	typename LabelOverlayImageFilterType::Pointer labelOverlay = LabelOverlayImageFilterType::New();
	//	typename ContourFilterType::Pointer contour = ContourFilterType::New();
	//
	//	contour->SetForegroundValue(1);
	//	contour->SetBackgroundValue(0);
	//	contour->FullyConnectedOn();
	//	typename StructuringElementType structuringElement2;
	//	structuringElement2.SetRadius(.5);
	//	structuringElement2.CreateStructuringElement();
	//
	//
	//	typename BinaryDilateImageFilterType::Pointer dilateFilter2 = BinaryDilateImageFilterType::New();
	//
	//	dilateFilter2->SetKernel(structuringElement2);
	//	dilateFilter2->SetBackgroundValue(0);
	//	dilateFilter2->SetForegroundValue(1);






	labelOverlay->SetOpacity(.3);
	labelOverlay->ResetColors();
	labelOverlay->AddColor(255,0,1.0);

	typename RescaleFilterType::Pointer rescale = RescaleFilterType::New();
	rescale->SetOutputMaximum(255);
	rescale->SetOutputMinimum(0);

	typename TilerType::Pointer tiler = TilerType::New();

	itk::FixedArray< unsigned int, 3 > layout;
	layout[0] = 1;
	layout[1] = 1;
	layout[2] = 0;
	tiler->SetLayout( layout );
	unsigned int inputImageNumber = 0;
	typename ImageReaderType2d::Pointer reader2d = ImageReaderType2d::New();
	typename AbsImageFilterType::Pointer absFilter = AbsImageFilterType::New();
	typename CenterFilterType::Pointer center = CenterFilterType::New();
	center->ChangeAll();
	center->SetReferenceImage(image);
	center->UseReferenceImageOn();


	center->SetInput(reader2d->GetOutput());
	absFilter->SetInput(center->GetOutput());
	thresholdFilter->SetInput(absFilter->GetOutput());
	recastUChar->SetInput(thresholdFilter->GetOutput());
	binaryToLabelMap->SetInput(recastUChar->GetOutput());
	labelMaptoImage->SetInput(binaryToLabelMap->GetOutput());
	labelOverlay->SetLabelImage(labelMaptoImage->GetOutput());


	rescale->SetInput(image);
	labelOverlay->SetInput(rescale->GetOutput());



	//	dilateFilter2->SetInput(contour->GetOutput());
	//	recastUChar->SetInput(dilateFilter2->GetOutput());


	typedef typename itk::ImageFileWriter<RGBImageType2d> RGBWriterType2d;
	RGBWriterType2d::Pointer writer2 = RGBWriterType2d::New();
	writer2->SetInput(labelOverlay->GetOutput());
	writer2->SetFileName("TestRGB.tiff");

	typename LabelOverlayImageFilterType::OutputImageType::Pointer inputImageTile;
	for (int i = start; i < end; i++)
	{
		std::stringstream sstm;

		sstm  << FilenameBase;
		sstm << std::setw(5) << std::setfill('0') << i << FilenameExt;




		reader2d->SetFileName( sstm.str() );
		//image->CopyInformation(labelOverlay->GetLabelImage());
		labelOverlay->UpdateLargestPossibleRegion();

		if (i == start){
			std::cout <<sstm.str() << std::endl;
			std::cout << "image" << std::endl;
			std::cout << "Origin " << image->GetOrigin() << std::endl;
			std::cout << "Spacing " << image->GetSpacing() << std::endl;
			std::cout << "Size " << image->GetLargestPossibleRegion().GetSize() << std::endl;

			std::cout << "reader" << std::endl;
			std::cout << "Origin " << reader2d->GetOutput()->GetOrigin() << std::endl;
			std::cout << "Spacing " << reader2d->GetOutput()->GetSpacing() << std::endl;
			std::cout << "Size" << reader2d->GetOutput()->GetLargestPossibleRegion().GetSize() << std::endl;
			writer2->Update();
		}
		inputImageTile = labelOverlay->GetOutput();

		//inputImageTile->CopyInformation()

		inputImageTile->DisconnectPipeline();
		tiler->SetInput( i, inputImageTile );
	}
	PixelType filler = 0;
	tiler->SetDefaultPixelValue( filler );
	tiler->Update();
	typename RGBWriterType::Pointer writer3d = RGBWriterType::New();
	writer3d->SetFileName(out_filename.c_str());
	writer3d->SetInput(tiler->GetOutput());
	writer3d->Update();
}


template<typename TvtkObject>
void vtkResliceImage(vtkSmartPointer<TvtkObject> reader,
		vtkSmartPointer<vtkImageReslice> reslice,double axes[16])
{
	reader->UpdateWholeExtent();
	reader->Update();

	int extent[6];
	double spacing[3];
	double origin[3];

	reader->GetOutput()->GetSpacing(spacing);
	reader->GetOutput()->GetOrigin(origin);


	MyUt::printArray<int>(extent,6);
	MyUt::printArray<double>(spacing,3);
	MyUt::printArray<double>(spacing,3);
}


}//namespace




#endif /* _MYUTILS_H_ */




