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
 * svLevelSet.h
 *
 *  Created on: Dec 11, 2013
 *      Author: jameson
 *
 *      The purpose of this is an interface for the level sets developed in ITK and the tcl code currently in simvascular
 *
 *      This class should contain the code to generate, setup, and execute the ITK pipeline to obtain the level sets
 *      In addition, it should include helper functions to convert data between the two systems.
 */

#ifndef CVITKLEVELSETBASE_H_
#define CVITKLEVELSETBASE_H_

#include "SimVascular.h"
#include "cvSolidModel.h"
#include "cvPolyData.h"
#include "cvStrPts.h"

#ifndef cvStructuredPoints
#define cvStructuredPoints cvStrPts
#endif

#include "Include/cvITKMacros.h"
#include "Include/cvMacros.h"
#include "Include/ConnectVTKITK.h"
#include "Util/MyUtils.h"

#include "itkVTKImageExport.h"
#include "itkVTKImageImport.h"
#include "itkImage.h"

#include "vtkImageImport.h"
#include "vtkImageExport.h"

#include "vtkSmartPointer.h"
#include "vtkStructuredPoints.h"
#include "vtkImageData.h"
#include "vtkPolyData.h"

#include "itkImageFileWriter.h"
#include "vtkTIFFWriter.h"
#include "ImgInfo.h"

#include "Util/cvITKUtils.h"

#ifndef NULL
#define NULL   ((void *) 0)
#endif


template<typename TInputImage = itk::Image<short,2>,
typename TInternalPixelType = float>
class cvITKLevelSetBase
{

public:

	/** Typedefs */
	// Depending on image size, this might be better with float...
	typedef cvITKLevelSetBase Self;
	typedef TInputImage ITKExternalImageType;
	typedef itk::Image<TInternalPixelType,
			ITKExternalImageType::ImageDimension> ITKInternalImageType;
	enum { ImageDimension = TInputImage::ImageDimension };


	//typedef itk::Image<short,N> ITKExternalImageType;
	//typedef itk::Image<float,N> ITKInternalImageType;

	typedef itk::VTKImageImport<ITKExternalImageType> itkExternalImportType;
	typedef itk::VTKImageExport<ITKExternalImageType> itkExternalExportType;

	typedef itk::VTKImageImport<ITKInternalImageType> itkInternalImportType;
	typedef itk::VTKImageExport<ITKInternalImageType> itkInternalExportType;

	virtual const char *GetNameOfClass() const
	{
		return "cvITKLevelSet";
	}

	//Feature Image stuff
	void	DeallocateFeatureObjs();
	int SetFeatureImage( cvStrPts *s);
	int GetFeatureImage( cvStrPts **s);

	//	int GetVelocityImage( cvStrPts **s);
	cvStrPts* GetVelocityImage();

	//Seed Image Stuff
	void DeallocateSeedObjs();

	int ComputePhaseOneLevelSet(float kc,
			float expFactorRising,float expFactorFalling);
	int ComputePhaseTwoLevelSet(float kupp,float klow);
	int ComputeGACLevelSet(float exponent,float kappa=5,float iso = .5);
	int ComputeLaplacianLevelSet(float exponent,float kappa=5,float iso = .5);

	int GenerateFeatureImage();
	int GenerateSeedImage();
	int CopyFrontToSeed();


	//Front Image Stuff:
	void DeallocateFrontObjs();
	int GetFront(cvPolyData** front);
	cvPolyData* GetFront();
	cvStrPts* GetFrontImage();

	//Get and Set Properties
	cvSetMacro(MaxIterations,int);
	cvGetMacro(MaxIterations,int);

	cvSetMacro(MaxRMSError,double);
	cvGetMacro(MaxRMSError,double);

	cvSetMacro(AdvectionScaling,double);
	cvGetMacro(AdvectionScaling,double);

	cvSetMacro(LaplacianScaling,double);
	cvGetMacro(LaplacianScaling,double);

	cvSetMacro(PropagationScaling,double);
	cvGetMacro(PropagationScaling,double);

	cvSetMacro(CurvatureScaling,double);
	cvGetMacro(CurvatureScaling,double);

	cvSetMacro(SigmaFeature,double);
	cvGetMacro(SigmaFeature,double);

	cvSetMacro(SigmaAdvection,double);
	cvGetMacro(SigmaAdvection,double);

	cvSetRepoObjMacro(Seed,cvPolyData,vtkPolyData);
	cvGetRepoObjMacro(Seed,cvPolyData);

	cvSetMacro(Debug,bool);
	cvGetMacro(Debug,bool);

	cvSetMacro(UseNormalVectorCurvature,bool);
	cvGetMacro(UseNormalVectorCurvature,bool);

	cvSetMacro(UseMeanCurvature,bool);
	cvGetMacro(UseMeanCurvature,bool);

	cvSetMacro(UseMinimalCurvature,bool);
	cvGetMacro(UseMinimalCurvature,bool);

	cvSetMacro(BinarySeed,bool);
	cvGetMacro(BinarySeed,bool);

	//cvSetRepoObjMacro(InputImage,cvStrPts,vtkStructuredPoints);
	int SetInputImage(cvStrPts *s);
	cvGetRepoObjMacro(InputImage,cvStrPts);

	//Image Info
	inline void SetInternalImgInfo(vtkStructuredPoints* vtksp)
	{

	}

	//some debug stuff
	void ViewITKFront()
	{
		QuickView viewer;
		bool flip = false;

		viewer.AddImage(m_itkFrontImage.GetPointer(),
				flip,
				"ITKFront");

		viewer.Visualize();

	}
	template<typename TLevelSetFilterType>
	void ShowDebug(typename TLevelSetFilterType::Pointer levelSetFilter);

	void WriteFrontImages()
	{
		std::stringstream filenameBase;
		filenameBase << "frontImage";
		cvITKLSUtil::WriteImage(m_itkFrontImage.GetPointer(),filenameBase.str());
		cvITKLSUtil::WriteImage(m_vtkFrontImage,filenameBase.str());

	}

	cvITKLevelSetBase();
	virtual ~cvITKLevelSetBase()
	{
		if ( m_cvInputImage != NULL ) {
			delete m_cvInputImage;
			m_cvInputImage = NULL;
		}
		if ( m_cvSeedImage != NULL ) {
			delete m_cvSeedImage;
			m_cvSeedImage = NULL;
		}

	};

	// To facilitate use with Tcl hash tables:
	char tclName_[CV_STRLEN];
protected:

	//base class
	void GenerateData(); //Might need a helper function
	//I do not want the compiler to auto-generate these methods.
	cvITKLevelSetBase(const cvITKLevelSetBase &); // purposely not implemented
	void operator=(const cvITKLevelSetBase &); // purposely not implemented
	//void


private:

	ImgInfo InternalImgInfo;
	ImgInfo ExternalImgInfo;
	//Input images
	cvStructuredPoints* m_cvInputImage;
	cvPolyData* m_cvSeed;

	typename ITKInternalImageType::Pointer m_itkFeatureImage;
	typename ITKInternalImageType::Pointer m_itkSeedImage;

	//output images
	vtkSmartPointer<vtkPolyData> m_vtkFrontPolyData;
	vtkSmartPointer<vtkStructuredPoints> m_vtkFrontImage;

	//Helper and debug output images
	vtkSmartPointer<vtkStructuredPoints> m_vtkFeatureImage;
	typename ITKInternalImageType::Pointer m_itkFrontImage;
	cvStructuredPoints* m_cvSeedImage;

	//Level Set Parameters
	double m_SigmaFeature;
	double m_SigmaAdvection;

	//Level Set Advanced Parameters
	double m_MaxRMSError;
	int m_MaxIterations;
	double m_PropagationScaling;
	double m_CurvatureScaling;
	double m_AdvectionScaling;
	double m_LaplacianScaling;

	//curvature type
	bool m_UseNormalVectorCurvature;
	bool m_UseMeanCurvature;
	bool m_UseMinimalCurvature;
	bool m_BinarySeed;

	//ITK Stuff
	bool m_Debug;

};

#include "cvITKLevelSetBase.cxx"

//throw e_; /* Explicit naming to work around Intel compiler bug.  */					\


#endif /* CVITKLEVELSET_H_ */

