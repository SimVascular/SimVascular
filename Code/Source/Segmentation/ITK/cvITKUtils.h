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
 * cvLevelSetUtils.h
 *
 *  Created on: May 21, 2014
 *      Author: jmerkow
 */

#ifndef CVLEVELSETUTILS_H_
#define CVLEVELSETUTILS_H_

#include "SimVascular.h"
#include "cvPolyData.h"
#include "cvStrPts.h"

#ifndef cvStructuredPoints
#define cvStructuredPoints cvStrPts
#endif

#include "itkImage.h"
#include "itkImageRegionIterator.h"

#include "Include/cvITKMacros.h"
#include "Include/cvMacros.h"

#include "Include/ConnectVTKITK.h"
#include "itkVTKImageExport.h"
#include "itkVTKImageImport.h"

#include "vtkImageImport.h"
#include "vtkImageExport.h"

#include "vtkSmartPointer.h"
#include "vtkStructuredPoints.h"
#include "vtkImageData.h"
#include "vtkPolyData.h"

#ifdef USE_QUICKVIEW_DEBUG
#include "QuickView.h"
#endif

#include "itkImageFileWriter.h"
#include "vtkTIFFWriter.h"
#include "ImgInfo.h"

typedef std::string string;

namespace cvITKLSUtil {

extern ImgInfo DefaultImgInfo;
extern bool Debug;

typedef itk::Image<short,2> ITKShort2DImageType;
typedef itk::Image<float,2> ITKFloat2DImageType;
typedef itk::Image<float,2> ITKDefaultImageType;
typedef itk::Image<short,3> ITKShort3DImageType;
typedef itk::Image<float,3> ITKFloat3DImageType;
typedef itk::Image<float,3> ITKDefault3DImageType;


/** vtk/itk input process **
 * vtkImage -> ChangeInfo -> RecastRescale -> itk
 **/
/** vtk/itk output process **
 * itk ->  RecastRescale -> ChangeInfo -> vtk
 **/

// Vtk methods
void vtkChangeImageInformation(vtkStructuredPoints* vtkImage,
		ImgInfo* refInfo);
void vtkPolyDataTo2DImage(vtkPolyData* pd,vtkStructuredPoints* vtkImg,
		ImgInfo* extInfo);
void vtkPolyDataToVolume(vtkPolyData* pd,vtkStructuredPoints* vtkImg,
		ImgInfo* extInfo);
void vtkGenerateCircle(double radius,double center[3],int numPoints,
		vtkPolyData* circle);
void WriteImage(vtkStructuredPoints* image,string FilenameBase);
void vtkWriteImage2(vtkStructuredPoints* image,string FilenameBase);
void WritePerciseVtkImage(vtkStructuredPoints* image,string FilenameBase);


/* itk templated methods */
template <typename TImageType,typename TExternalImageType>
SV_EXPORT_SEGITK int vtkGenerateFeatureImage(vtkStructuredPoints* vtkInputImage,
		vtkStructuredPoints* vtkOuputImage, ImgInfo* refInfo,double sigma);

template < typename TImageType , typename TExternalImageType >
SV_EXPORT_SEGITK void vtk2itkRecastAndRescale(vtkStructuredPoints* vtkImage,
		typename TImageType::Pointer itkImage, ImgInfo* refInfo);

template < typename TImageType >
SV_EXPORT_SEGITK void itk2vtkRecast(TImageType* itkImage, vtkStructuredPoints* vtkImage,
		ImgInfo* refInfo);

template < typename TImageType >
SV_EXPORT_SEGITK void itk2vtkRecastAndRescale(TImageType* itkImage,
		vtkStructuredPoints* vtkImage, ImgInfo* refInfo);

template < typename TImageType >
SV_EXPORT_SEGITK void itk2vtkRecast(TImageType* itkImage, vtkStructuredPoints* vtkImage,
		ImgInfo* refInfo);

template< typename TImageType >
SV_EXPORT_SEGITK void itkGenerateFeatureImage(TImageType* itkInputImage,
		typename TImageType::Pointer featureImage, double sigma);

template <typename TImageType>
SV_EXPORT_SEGITK void itkGenerateFeatureImageNoGrad(TImageType* itkInputImage,
		typename TImageType::Pointer outImage, double sigma);

template <typename TImageType>
SV_EXPORT_SEGITK void itkGenerateFeatureImageDistance(TImageType* itkInputImage,
		typename TImageType::Pointer outImage,double thres=.5);

template <typename TImageType>
SV_EXPORT_SEGITK void itkGenerateFeatureImageThreshold(TImageType* itkInputImage,
		typename TImageType::Pointer outImage,double thres);

template < typename TImageType , typename TExternalImageType >
SV_EXPORT_SEGITK void vtk2itkBinaryImageToSeedImage(vtkStructuredPoints* vtkImg,
		typename TImageType::Pointer itkImage,ImgInfo* intInfo);

template < typename TImageType >
SV_EXPORT_SEGITK void itkDeepCopy(const TImageType* input,
		typename TImageType::Pointer output);

template < typename TImageType >
SV_EXPORT_SEGITK void itkDeepCopy(typename TImageType::Pointer input,
		typename TImageType::Pointer output);

template < typename TImageType >
SV_EXPORT_SEGITK void itkDeepCopy(const TImageType* input,TImageType* output);

template < typename TImageType >
SV_EXPORT_SEGITK void CopyVTKtoITK(vtkStructuredPoints* in,TImageType* out);

template < typename TImageType >
SV_EXPORT_SEGITK void CopyITKtoVTK(const TImageType* in,vtkStructuredPoints* out);

template < typename TImageType >
SV_EXPORT_SEGITK void CopyITKtoVTK(const TImageType* in,vtkStructuredPoints* out);

template < typename TImageType >
SV_EXPORT_SEGITK void WriteImage(const TImageType* input,string FilenameBase);

template < typename TImageType >
SV_EXPORT_SEGITK void WriteImage2(const TImageType* input,string FilenameBase);

template < typename TImageType >
static void WritePNGImage(const TImageType* input,string FilenameBase);

template<typename ITKImageType>
SV_EXPORT_SEGITK void CreateImage(typename ITKImageType::Pointer image,
		typename ITKImageType::SizeType size,
		typename ITKImageType::SpacingType spacing,
		typename ITKImageType::PointType origin,
		typename ITKImageType::PixelType value);

/* CV convienience methods */
SV_EXPORT_SEGITK void inline vtkPolyDataTo2DImage(vtkPolyData* pd,cvStructuredPoints** result,
		ImgInfo* refInfo)
{
	vtkStructuredPoints* out = vtkStructuredPoints::New();
	vtkPolyDataTo2DImage(pd,out,refInfo);
	(*result) = new cvStructuredPoints(out);
}

SV_EXPORT_SEGITK void inline vtkPolyDataToVolume(vtkPolyData* pd,cvStructuredPoints** result,
		ImgInfo* refInfo)
{
	vtkStructuredPoints* out = vtkStructuredPoints::New();
	vtkPolyDataToVolume(pd,out,refInfo);
	(*result) = new cvStructuredPoints(out);
}

SV_EXPORT_SEGITK void inline vtkGenerateCircle(double radius,double center[3],
		int numPoints,cvPolyData** circle)
{
	vtkPolyData* out = vtkPolyData::New();
	vtkGenerateCircle(radius,center,numPoints,out);
	(*circle) = new cvPolyData(out);
}

} // namespace


// Below contains the template function body
#include "cvITKUtils.hxx"


#endif /* CVLEVELSETUTILS_H_ */
