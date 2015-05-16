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
 * cv_ITKUtils_init.cxx
 *
 *  Created on: May 29, 2014
 *      Author: jmerkow
 */

#include "SimVascular.h"
#include "cvTCLMacros.h"
#include <stdio.h>
#include <string.h>

#include "cvITKLevelSet.h"
#include "cvLevelSetVelocity.h"
#include "cvRepository.h"
#include "cvSolidModel.h"
#include "cv_misc_utils.h"
#include "cv_arg.h"
#include "itkVersion.h"

#include "cvITKUtils.h"

#include "cv_ITKUtils_init.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

//!  C++/TCL bindings for ITK Utils.
/*!
  This file provides C++/Tcl bindings for itk Utils
*/

// Globals:
// --------

#include "cv_globals.h"

// -----------
// Prototypes:
// -----------

//! Generate a polydata cirlce with a specified radius and location
/*! (Requires VTK Only) A more elaborate description... TBD
*/
static int itkutils_GenerateCircleCmd( CXX_TCL_STDARGS );

//! Converts a polydata to a 2D image
/*! (Requires VTK Only) A more elaborate description... TBD
*/
static int itkutils_PdToImgCmd( CXX_TCL_STDARGS );

//! Converts a  3D polydata to a 3D image volume
/*! (Requires VTK Only) A more elaborate description... TBD
*/
static int itkutils_PdToVolCmd( CXX_TCL_STDARGS );

//! Write an image to vtk format
/*! (Requires VTK Only) A more elaborate description... TBD
*/
static int itkutils_WriteImageCmd( CXX_TCL_STDARGS );

//! Computer the gradient magnitude of a vtk image, after a gaussian blur
/*! (Requires VTK and ITK) A more elaborate description... TBD
*/
static int itkutils_GradientMagnitudeGaussianCmd( CXX_TCL_STDARGS );

static int itkutils_GaussianCmd( CXX_TCL_STDARGS );
static int itkutils_DistanceImageCmd( CXX_TCL_STDARGS );
static int itkutils_ThresholdImageCmd( CXX_TCL_STDARGS );

//static int itkutils_SetExternalImgInfoCmd( CXX_TCL_STDARGS );

static int itkutils_FractEdgeProximity3DCmd( CXX_TCL_STDARGS );

// -------------
// itkutils_Init
// -------------
//! Initializes C++/Tcl bindings
/*! A more elaborate description... TBD
*/
int itkutils_Init( Tcl_Interp *interp ){

	printf("  %-12s %s\n","itk:", itk::Version::GetITKVersion());

	//Usage: CVTCLFunctionInit(Prefix,FunctionName,TclName)
	CVTCLFunctionInit(itkutils,GenerateCircleCmd,itkutils_GenerateCircle);
	CVTCLFunctionInit(itkutils,PdToImgCmd,itkutils_PdToImg);
	CVTCLFunctionInit(itkutils,PdToVolCmd,itkutils_PdToVol);
	CVTCLFunctionInit(itkutils,WriteImageCmd,itkutils_WriteImage);
	CVTCLFunctionInit(itkutils,GradientMagnitudeGaussianCmd,itkutils_GradientMagnitudeGaussian);
	CVTCLFunctionInit(itkutils,GaussianCmd,itkutils_GaussianBlur);
	CVTCLFunctionInit(itkutils,FractEdgeProximity3DCmd,itkutils_FractEdgeProximity3D);
	CVTCLFunctionInit(itkutils,DistanceImageCmd,itkutils_DistanceImage);
	CVTCLFunctionInit(itkutils,ThresholdImageCmd,itkutils_ThresholdImage);
//	CVTCLFunctionInit(itkutils,SetExternalImgInfoCmd,itkutils_SetExternalImgInfo);
	return TCL_OK;

}

static int itkutils_GenerateCircleCmd( CXX_TCL_STDARGS )
{
	char *usage;
	char *result;
	double r=1.0, x, y, z;

	int table_size = 5;
	ARG_Entry arg_table[] = {
			{ "-result", STRING_Type, &result, NULL, REQUIRED, 0, { 0 } },
			{ "-r", DOUBLE_Type, &r, NULL, REQUIRED, 0, { 0 } },
			{ "-x", DOUBLE_Type, &x, NULL, REQUIRED, 0, { 0 } },
			{ "-y", DOUBLE_Type, &y, NULL, REQUIRED, 0, { 0 } },
			{ "-z", DOUBLE_Type, &z, NULL, REQUIRED, 0, { 0 } },
	};

	usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );

	CVTCLTestArgsMacro(1)
	CVTCLParseTclStrMacro(1)
	// Make sure the specified result object does not exist:
	CVTCLRepositoryExistsMacro(result)

	cvPolyData *obj = NULL;
	int loc[3];
	loc[0] = x;
	loc[1] = y;
	loc[2] = z;
	double center[3];
	center[0] = x;
	center[1] = y;
	center[2] = z;
	// Do work of command
	cvITKLSUtil::vtkGenerateCircle(r,center,50,&obj);

	//Save Result
	obj->SetName( result );
	if ( !( gRepository->Register( obj->GetName(), obj ) ) ) {
		Tcl_AppendResult( interp, "error registering obj ", result,
				" in repository", (char *)NULL );
		delete obj;
		return TCL_ERROR;
	}

	Tcl_SetResult( interp, obj->GetName(), TCL_VOLATILE );
	return TCL_OK;
}

static int itkutils_PdToImgCmd( CXX_TCL_STDARGS )
{
	//std::cout << "Command: PdToImg" << std::endl;
	char *usage;
	char *inputPdName;
	char *result;

	int table_size = 2;
	ARG_Entry arg_table[] = {
			{ "-src", STRING_Type, &inputPdName, NULL, REQUIRED, 0, { 0 } },
			{ "-dst", STRING_Type, &result, NULL, REQUIRED, 0, { 0 } },
	};
	usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );

	CVTCLTestArgsMacro(1)
	CVTCLParseTclStrMacro(1)

	//Get the input image from the repository
	RepositoryDataT type;
	cvRepositoryData *pd;
	vtkPolyData *vtkpd;
	if (inputPdName != NULL) {
		// Look up given image object:
		pd = gRepository->GetObject( inputPdName );
		if ( pd == NULL ) {
			Tcl_AppendResult( interp, "couldn't find object ", inputPdName, (char *)NULL );
			return TCL_ERROR;
		}
		// Make sure image is of type STRUCTURED_PTS_T:
		type = pd->GetType();
		if ( type != POLY_DATA_T ) {
			Tcl_AppendResult( interp, "error: object ", inputPdName,
					"not of type PolyData", (char *)NULL );
			return TCL_ERROR;
		}
		// Retrive geometric information:
		vtkpd = ((cvPolyData*)pd)->GetVtkPolyData();
	}

	// Make sure the specified result object does not exist:
	CVTCLRepositoryExistsMacro(result)

	// Do work of command
	cvStrPts *obj = NULL;
	ImgInfo tempInfo;

	cvITKLSUtil::DefaultImgInfo.Print(std::cout);

	//tempInfo.Print(std::cout);
	cvITKLSUtil::vtkPolyDataTo2DImage(vtkpd,&obj,&tempInfo);

	//Save Result
	obj->SetName( result );
	if ( !( gRepository->Register( obj->GetName(), obj ) ) ) {
		Tcl_AppendResult( interp, "error registering obj ", result,
				" in repository", (char *)NULL );
		delete obj;
		return TCL_ERROR;
	}

	// Return name
	Tcl_SetResult( interp, obj->GetName(), TCL_VOLATILE );
	return TCL_OK;
}


static int itkutils_PdToVolCmd( CXX_TCL_STDARGS )
{
	//std::cout << "Command: PdToVol" << std::endl;
	char *usage;
	char *inputPdName;
	char *refName;
	char *result;

	int table_size = 3;
	ARG_Entry arg_table[] = {
			{ "-src", STRING_Type, &inputPdName, NULL, REQUIRED, 0, { 0 } },
			{ "-dst", STRING_Type, &result, NULL, REQUIRED, 0, { 0 } },
			{ "-ref", STRING_Type, &refName, NULL, REQUIRED, 0, { 0 } },
	};

	usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );

	CVTCLTestArgsMacro(1)
	CVTCLParseTclStrMacro(1)

	//Get the input image from the repository
	RepositoryDataT type;
	cvRepositoryData *pd;
	vtkPolyData *vtkpd;
	if (inputPdName != NULL) {
		// Look up given image object:
		pd = gRepository->GetObject( inputPdName );
		if ( pd == NULL ) {
			Tcl_AppendResult( interp, "couldn't find object ", inputPdName, (char *)NULL );
			return TCL_ERROR;
		}
		// Make sure image is of type STRUCTURED_PTS_T:
		type = pd->GetType();
		if ( type != POLY_DATA_T ) {
			Tcl_AppendResult( interp, "error: object ", inputPdName,
					"not of type PolyData", (char *)NULL );
			return TCL_ERROR;
		}
		// Retrive geometric information:
		vtkpd = ((cvPolyData*)pd)->GetVtkPolyData();
	}

	cvRepositoryData *ref;
	vtkStructuredPoints *vtkref;
	if (refName != NULL) {
		// Look up given image object:
		ref = gRepository->GetObject( refName );
		if ( ref == NULL ) {
			Tcl_AppendResult( interp, "couldn't find object ", refName, (char *)NULL );
			return TCL_ERROR;
		}
		// Make sure image is of type STRUCTURED_PTS_T:
		type = ref->GetType();
		if ( type != STRUCTURED_PTS_T ) {
			Tcl_AppendResult( interp, "error: object ", refName,
					"not of type StructuredPts", (char *)NULL );
			return TCL_ERROR;
		}
		// Retrive geometric information:
		vtkref = (vtkStructuredPoints*)((cvStrPts*)ref)->GetVtkPtr();
	}

	// Make sure the specified result object does not exist:
	CVTCLRepositoryExistsMacro(result)

	// Do work of command
	cvStrPts *obj = NULL;
	ImgInfo tempInfo = ImgInfo(vtkref);
	vtkref->Delete();
	//tempInfo.Print(std::cout);
	cvITKLSUtil::vtkPolyDataToVolume(vtkpd,&obj,&tempInfo);

	//Save Result
	obj->SetName( result );
	if ( !( gRepository->Register( obj->GetName(), obj ) ) ) {
		Tcl_AppendResult( interp, "error registering obj ", result,
				" in repository", (char *)NULL );
		delete obj;
		return TCL_ERROR;
	}

	//Return Name
	Tcl_SetResult( interp, obj->GetName(), TCL_VOLATILE );
	return TCL_OK;

}

static int itkutils_WriteImageCmd(CXX_TCL_STDARGS)
{
	std::cout << "Command: WriteImage" << std::endl;
	char *usage;
	char *inputImgName;
	char *fname;

	int table_size = 2;
	ARG_Entry arg_table[] = {
			{ "-src", STRING_Type, &inputImgName, NULL, REQUIRED, 0, { 0 } },
			{ "-fname", STRING_Type, &fname, NULL, REQUIRED, 0, { 0 } },
	};
	usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );

	CVTCLTestArgsMacro(1)
	CVTCLParseTclStrMacro(1)

	//Get the input image from the repository
	RepositoryDataT type;
	cvRepositoryData *img;
	vtkStructuredPoints *vtksp;
	if (inputImgName != NULL) {
		// Look up given image object:
		img = gRepository->GetObject( inputImgName );
		if ( img == NULL ) {
			Tcl_AppendResult( interp, "couldn't find object ", inputImgName, (char *)NULL );
			return TCL_ERROR;
		}
		// Make sure image is of type STRUCTURED_PTS_T:
		type = img->GetType();
		if ( type != STRUCTURED_PTS_T ) {
			Tcl_AppendResult( interp, "error: object ", inputImgName,
					"not of type StructuredPts", (char *)NULL );
			return TCL_ERROR;
		}
		// Retrive geometric information:
		vtksp = ((cvStrPts*)img)->GetVtkStructuredPoints();
	}

	// Do work of command
	std::string sfname(fname);
	cvITKLSUtil::WritePerciseVtkImage(vtksp,sfname);

	Tcl_SetResult( interp, fname, TCL_VOLATILE );

	return TCL_OK;

}

int itkutils_GradientMagnitudeGaussianCmd( CXX_TCL_STDARGS )
{
	//std::cout << "Command: GradientMagnitudeGaussian" << std::endl;
	char *usage;
	char *inputImgName;
	char *result;
	double sigma = 1;

	int table_size = 3;
	ARG_Entry arg_table[] = {
			{ "-src", STRING_Type, &inputImgName, NULL, REQUIRED, 0, { 0 } },
			{ "-dst", STRING_Type, &result, NULL, REQUIRED, 0, { 0 } },
			{ "-sigma", DOUBLE_Type, &sigma, NULL, REQUIRED, 0, { 0 } },
	};
	usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );

	CVTCLTestArgsMacro(1)
	CVTCLParseTclStrMacro(1)


	// Get the input image from the repository
	RepositoryDataT type;
	cvRepositoryData *img;
	vtkStructuredPoints *vtksp;
	if (inputImgName != NULL) {
		// Look up given image object:
		img = gRepository->GetObject( inputImgName );
		if ( img == NULL ) {
			Tcl_AppendResult( interp, "couldn't find object ", inputImgName, (char *)NULL );
			return TCL_ERROR;
		}

		// Make sure image is of type STRUCTURED_PTS_T:
		type = img->GetType();
		if ( type != STRUCTURED_PTS_T ) {
			Tcl_AppendResult( interp, "error: object ", inputImgName,
					"not of type StructuredPts", (char *)NULL );
			return TCL_ERROR;
		}
		// Retrive geometric information:
		vtksp = ((cvStrPts*)img)->GetVtkStructuredPoints();
	}

	// Make sure the specified result object does not exist:
	CVTCLRepositoryExistsMacro(result)

	//std::cout << "Sigma: " << sigma <<std::endl;
	// Do work of command
	vtkStructuredPoints* vtkout = vtkStructuredPoints::New();
	ImgInfo itkinfo;
	itkinfo.SetExtent(vtksp->GetExtent());
	//itkinfo.Print(std::cout);
	cvITKLSUtil::vtkGenerateFeatureImage
	<cvITKLSUtil::ITKFloat2DImageType,cvITKLSUtil::ITKShort2DImageType>(vtksp,vtkout,&itkinfo,sigma);

	// Save Result
	cvStrPts* obj = new cvStrPts(vtkout);
	obj->SetName( result );
	if ( !( gRepository->Register( obj->GetName(), obj ) ) ) {
		Tcl_AppendResult( interp, "error registering obj ", result,
				" in repository", (char *)NULL );
		delete obj;
		return TCL_ERROR;
	}

	// Return Name
	Tcl_SetResult( interp, obj->GetName(), TCL_VOLATILE );
	return TCL_OK;

}

int itkutils_GaussianCmd( CXX_TCL_STDARGS )
{
	//std::cout << "Command: GradientMagnitudeGaussian" << std::endl;
	char *usage;
	char *inputImgName;
	char *result;
	double sigma = 1;

	int table_size = 3;
	ARG_Entry arg_table[] = {
			{ "-src", STRING_Type, &inputImgName, NULL, REQUIRED, 0, { 0 } },
			{ "-dst", STRING_Type, &result, NULL, REQUIRED, 0, { 0 } },
			{ "-sigma", DOUBLE_Type, &sigma, NULL, REQUIRED, 0, { 0 } },
	};
	usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );

	CVTCLTestArgsMacro(1)
	CVTCLParseTclStrMacro(1)


	// Get the input image from the repository
	RepositoryDataT type;
	cvRepositoryData *img;
	vtkStructuredPoints *vtksp;
	if (inputImgName != NULL) {
		// Look up given image object:
		img = gRepository->GetObject( inputImgName );
		if ( img == NULL ) {
			Tcl_AppendResult( interp, "couldn't find object ", inputImgName, (char *)NULL );
			return TCL_ERROR;
		}

		// Make sure image is of type STRUCTURED_PTS_T:
		type = img->GetType();
		if ( type != STRUCTURED_PTS_T ) {
			Tcl_AppendResult( interp, "error: object ", inputImgName,
					"not of type StructuredPts", (char *)NULL );
			return TCL_ERROR;
		}
		// Retrive geometric information:
		vtksp = ((cvStrPts*)img)->GetVtkStructuredPoints();
	}

	// Make sure the specified result object does not exist:
	CVTCLRepositoryExistsMacro(result)

	//std::cout << "Sigma: " << sigma <<std::endl;
	// Do work of command
	vtkStructuredPoints* vtkout = vtkStructuredPoints::New();
	ImgInfo itkinfo;
	itkinfo.SetExtent(vtksp->GetExtent());
	//itkinfo.Print(std::cout);
	cvITKLSUtil::vtkGenerateFeatureImageNoGrad
	<cvITKLSUtil::ITKFloat2DImageType,cvITKLSUtil::ITKShort2DImageType>(vtksp,vtkout,&itkinfo,sigma);

	// Save Result
	cvStrPts* obj = new cvStrPts(vtkout);
	obj->SetName( result );
	if ( !( gRepository->Register( obj->GetName(), obj ) ) ) {
		Tcl_AppendResult( interp, "error registering obj ", result,
				" in repository", (char *)NULL );
		delete obj;
		return TCL_ERROR;
	}

	// Return Name
	Tcl_SetResult( interp, obj->GetName(), TCL_VOLATILE );
	return TCL_OK;

}

int itkutils_DistanceImageCmd( CXX_TCL_STDARGS )
{
	//std::cout << "Command: GradientMagnitudeGaussian" << std::endl;
	char *usage;
	char *inputImgName;
	char *result;
	double thres = .5;

	int table_size = 3;
	ARG_Entry arg_table[] = {
			{ "-src", STRING_Type, &inputImgName, NULL, REQUIRED, 0, { 0 } },
			{ "-dst", STRING_Type, &result, NULL, REQUIRED, 0, { 0 } },
			{ "-thres", DOUBLE_Type, &thres, NULL, REQUIRED, 0, { 0 } },
	};
	usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );

	CVTCLTestArgsMacro(1)
	CVTCLParseTclStrMacro(1)


	// Get the input image from the repository
	RepositoryDataT type;
	cvRepositoryData *img;
	vtkStructuredPoints *vtksp;
	if (inputImgName != NULL) {
		// Look up given image object:
		img = gRepository->GetObject( inputImgName );
		if ( img == NULL ) {
			Tcl_AppendResult( interp, "couldn't find object ", inputImgName, (char *)NULL );
			return TCL_ERROR;
		}

		// Make sure image is of type STRUCTURED_PTS_T:
		type = img->GetType();
		if ( type != STRUCTURED_PTS_T ) {
			Tcl_AppendResult( interp, "error: object ", inputImgName,
					"not of type StructuredPts", (char *)NULL );
			return TCL_ERROR;
		}
		// Retrive geometric information:
		vtksp = ((cvStrPts*)img)->GetVtkStructuredPoints();
	}

	// Make sure the specified result object does not exist:
	CVTCLRepositoryExistsMacro(result)

	//std::cout << "Sigma: " << sigma <<std::endl;
	// Do work of command
	vtkStructuredPoints* vtkout = vtkStructuredPoints::New();
	ImgInfo itkinfo;
	itkinfo.SetExtent(vtksp->GetExtent());
	//itkinfo.Print(std::cout);
	cvITKLSUtil::vtkGenerateFeatureImageDistance
	<cvITKLSUtil::ITKFloat2DImageType,cvITKLSUtil::ITKShort2DImageType>(vtksp,vtkout,&itkinfo,thres);

	// Save Result
	cvStrPts* obj = new cvStrPts(vtkout);
	obj->SetName( result );
	if ( !( gRepository->Register( obj->GetName(), obj ) ) ) {
		Tcl_AppendResult( interp, "error registering obj ", result,
				" in repository", (char *)NULL );
		delete obj;
		return TCL_ERROR;
	}

	// Return Name
	Tcl_SetResult( interp, obj->GetName(), TCL_VOLATILE );
	return TCL_OK;

}


int itkutils_ThresholdImageCmd( CXX_TCL_STDARGS )
{
	//std::cout << "Command: GradientMagnitudeGaussian" << std::endl;
	char *usage;
	char *inputImgName;
	char *result;
	double thres = .5;

	int table_size = 3;
	ARG_Entry arg_table[] = {
			{ "-src", STRING_Type, &inputImgName, NULL, REQUIRED, 0, { 0 } },
			{ "-dst", STRING_Type, &result, NULL, REQUIRED, 0, { 0 } },
			{ "-thres", DOUBLE_Type, &thres, NULL, REQUIRED, 0, { 0 } },
	};
	usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );

	CVTCLTestArgsMacro(1)
	CVTCLParseTclStrMacro(1)


	// Get the input image from the repository
	RepositoryDataT type;
	cvRepositoryData *img;
	vtkStructuredPoints *vtksp;
	if (inputImgName != NULL) {
		// Look up given image object:
		img = gRepository->GetObject( inputImgName );
		if ( img == NULL ) {
			Tcl_AppendResult( interp, "couldn't find object ", inputImgName, (char *)NULL );
			return TCL_ERROR;
		}

		// Make sure image is of type STRUCTURED_PTS_T:
		type = img->GetType();
		if ( type != STRUCTURED_PTS_T ) {
			Tcl_AppendResult( interp, "error: object ", inputImgName,
					"not of type StructuredPts", (char *)NULL );
			return TCL_ERROR;
		}
		// Retrive geometric information:
		vtksp = ((cvStrPts*)img)->GetVtkStructuredPoints();
	}

	// Make sure the specified result object does not exist:
	CVTCLRepositoryExistsMacro(result)

	//std::cout << "Sigma: " << sigma <<std::endl;
	// Do work of command
	vtkStructuredPoints* vtkout = vtkStructuredPoints::New();
	ImgInfo itkinfo;
	itkinfo.SetExtent(vtksp->GetExtent());
	//itkinfo.Print(std::cout);
	cvITKLSUtil::vtkGenerateFeatureImageThreshold
	<cvITKLSUtil::ITKFloat2DImageType,cvITKLSUtil::ITKShort2DImageType>(vtksp,vtkout,&itkinfo,thres);

	// Save Result
	cvStrPts* obj = new cvStrPts(vtkout);
	obj->SetName( result );
	if ( !( gRepository->Register( obj->GetName(), obj ) ) ) {
		Tcl_AppendResult( interp, "error registering obj ", result,
				" in repository", (char *)NULL );
		delete obj;
		return TCL_ERROR;
	}

	// Return Name
	Tcl_SetResult( interp, obj->GetName(), TCL_VOLATILE );
	return TCL_OK;

}

//TODO: Template this better. Replace with redesign.
int itkutils_FractEdgeProximity3DCmd( CXX_TCL_STDARGS )
{
	//std::cout << "Command: GradientMagnitudeGaussian" << std::endl;
	char *usage;
	char *inputImgName;
	char *result;
	double sigma = 1, kappa=5, exponent=5;

	int table_size = 5;
	ARG_Entry arg_table[] = {
			{ "-src", STRING_Type, &inputImgName, NULL, REQUIRED, 0, { 0 } },
			{ "-dst", STRING_Type, &result, NULL, REQUIRED, 0, { 0 } },
			{ "-sigma", DOUBLE_Type, &sigma, NULL, REQUIRED, 0, { 0 } },
			{ "-kappa", DOUBLE_Type, &kappa, NULL, REQUIRED, 0, { 0 } },
			{ "-exponent", DOUBLE_Type, &exponent, NULL, REQUIRED, 0, { 0 } }
	};
	usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );

	CVTCLTestArgsMacro(1)
	CVTCLParseTclStrMacro(1)


	// Get the input image from the repository
	RepositoryDataT type;
	cvRepositoryData *img;
	vtkStructuredPoints *vtksp;
	if (inputImgName != NULL) {
		// Look up given image object:
		img = gRepository->GetObject( inputImgName );
		if ( img == NULL ) {
			Tcl_AppendResult( interp, "couldn't find object ", inputImgName, (char *)NULL );
			return TCL_ERROR;
		}

		// Make sure image is of type STRUCTURED_PTS_T:
		type = img->GetType();
		if ( type != STRUCTURED_PTS_T ) {
			Tcl_AppendResult( interp, "error: object ", inputImgName,
					"not of type StructuredPts", (char *)NULL );
			return TCL_ERROR;
		}
		// Retrive geometric information:
		vtksp = ((cvStrPts*)img)->GetVtkStructuredPoints();
	}

	// Make sure the specified result object does not exist:
	CVTCLRepositoryExistsMacro(result)

	//std::cout << "Sigma: " << sigma <<std::endl;
	// Do work of command
	vtkStructuredPoints* vtkout = vtkStructuredPoints::New();
	ImgInfo itkinfo = ImgInfo(vtksp);
	itkinfo.SetExtent(vtksp->GetExtent());
	itkinfo.SetMaxValue(255);
	itkinfo.SetMinValue(0);
	//itkinfo.Print(std::cout);
	typedef cvITKLSUtil::ITKFloat3DImageType IT1;
	typedef cvITKLSUtil::ITKShort3DImageType IT2;
	cvITKLSUtil::vtkGenerateEdgeProxImage<IT1,IT2>(vtksp,vtkout,&itkinfo,sigma,kappa,exponent);

	// Save Result
	cvStrPts* obj = new cvStrPts(vtkout);
	obj->SetName( result );
	if ( !( gRepository->Register( obj->GetName(), obj ) ) ) {
		Tcl_AppendResult( interp, "error registering obj ", result,
				" in repository", (char *)NULL );
		delete obj;
		return TCL_ERROR;
	}

	// Return Name
	Tcl_SetResult( interp, obj->GetName(), TCL_VOLATILE );
	return TCL_OK;

}
