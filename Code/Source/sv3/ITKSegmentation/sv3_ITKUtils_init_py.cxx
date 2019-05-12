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
/*
 * sv3_ITKUtils_init.cxx
 *
 *  Created on: May 29, 2014
 *      Author: jmerkow
 */

#include "SimVascular.h"
#include "Python.h"
#include "vtkPythonUtil.h"
#include "sv3_ITKLSet_PYTHON_Macros.h"
#include <stdio.h>
#include <string.h>

#include "sv3_ITKLevelSet.h"
#include "sv2_LevelSetVelocity.h"
#include "sv_Repository.h"
#include "sv_SolidModel.h"
#include "sv_misc_utils.h"
#include "sv_arg.h"
#include "itkVersion.h"

#include "sv3_ITKLset_ITKUtils.h"

#include "sv3_ITKUtils_init_py.h"

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

#include "sv2_globals.h"

// -----------
// Prototypes:
// -----------

//! Generate a polydata cirlce with a specified radius and location
/*! (Requires VTK Only) A more elaborate description... TBD
*/
static PyObject* itkutils_GenerateCircleCmd( CXX_PYTHON_STDARGS );

//! Converts a polydata to a 2D image
/*! (Requires VTK Only) A more elaborate description... TBD
*/
static PyObject* itkutils_PdToImgCmd( CXX_PYTHON_STDARGS );

//! Converts a  3D polydata to a 3D image volume
/*! (Requires VTK Only) A more elaborate description... TBD
*/
static PyObject* itkutils_PdToVolCmd( CXX_PYTHON_STDARGS );

//! Write an image to vtk format
/*! (Requires VTK Only) A more elaborate description... TBD
*/
static PyObject* itkutils_WriteImageCmd( CXX_PYTHON_STDARGS );

//! Computer the gradient magnitude of a vtk image, after a gaussian blur
/*! (Requires VTK and ITK) A more elaborate description... TBD
*/
static PyObject* itkutils_GradientMagnitudeGaussianCmd( CXX_PYTHON_STDARGS );

static PyObject* itkutils_GaussianCmd( CXX_PYTHON_STDARGS );
static PyObject* itkutils_DistanceImageCmd( CXX_PYTHON_STDARGS );
static PyObject* itkutils_ThresholdImageCmd( CXX_PYTHON_STDARGS );

//static PyObject* itkutils_SetExternalImgInfoCmd( CXX_PYTHON_STDARGS );

static PyObject* itkutils_FractEdgeProximity3DCmd( CXX_PYTHON_STDARGS );
PyObject *PyRunTimeErrU;
// -------------
// Itkutils_Init
// -------------
//! Initializes C++/Tcl bindings
/*! A more elaborate description... TBD
*/
PyMethodDef Itkutils_methods[] = {
	{"GenerateCircle", itkutils_GenerateCircleCmd, METH_VARARGS,NULL},
	{"PdToImg", itkutils_PdToImgCmd, METH_VARARGS,NULL},
	{"PdToVol", itkutils_PdToVolCmd, METH_VARARGS,NULL},
	{"WriteImage", itkutils_WriteImageCmd, METH_VARARGS,NULL},
	{"GradientMagnitudeGaussian",itkutils_GradientMagnitudeGaussianCmd,METH_VARARGS,NULL},
	{"GaussianBlur", itkutils_GaussianCmd, METH_VARARGS,NULL},
	{"DistanceImage",itkutils_DistanceImageCmd,METH_VARARGS,NULL},
	{"ThresholdImage",itkutils_ThresholdImageCmd,METH_VARARGS,NULL},
	{"FractEdgeProximity3D",itkutils_FractEdgeProximity3DCmd,METH_VARARGS,NULL},
	{NULL, NULL,0,NULL},
	};

#if PYTHON_MAJOR_VERSION == 3
static struct PyModuleDef Itkutilsmodule = {
   PyModuleDef_HEAD_INIT,
   "Itkutils",   /* name of module */
   "", /* module documentation, may be NULL */
   -1,       /* size of per-interpreter state of the module,
                or -1 if the module keeps state in global variables. */
   Itkutils_methods
};
#endif

PyObject*  Itkutils_pyInit( ){

	printf("  %-12s %s\n","Itk:", itk::Version::GetITKVersion());

    PyObject *pyItklsUtils;
#if PYTHON_MAJOR_VERSION == 2
    pyItklsUtils= Py_InitModule("Itkutils",Itkutils_methods);
#elif PYTHON_MAJOR_VERSION == 3
    pyItklsUtils= PyModule_Create(&Itkutilsmodule);
#endif
    PyRunTimeErrU = PyErr_NewException("Itkutils.error",NULL,NULL);
    PyModule_AddObject(pyItklsUtils,"error",PyRunTimeErrU);

	return pyItklsUtils;

}


static PyObject* itkutils_GenerateCircleCmd( CXX_PYTHON_STDARGS )
{
	char *usage;
	char *result;
	double r=1.0, x, y, z;

 /*	int table_size = 5;
	ARG_Entry arg_table[] = {
			{ "-result", STRING_Type, &result, NULL, REQUIRED, 0, { 0 } },
			{ "-r", DOUBLE_Type, &r, NULL, REQUIRED, 0, { 0 } },
			{ "-x", DOUBLE_Type, &x, NULL, REQUIRED, 0, { 0 } },
			{ "-y", DOUBLE_Type, &y, NULL, REQUIRED, 0, { 0 } },
			{ "-z", DOUBLE_Type, &z, NULL, REQUIRED, 0, { 0 } },
	};

	usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );

	CVPYTHONTestArgsMacro(1)
	CVPYTHONParseTclStrMacro(1)*/
	if (!PyArg_ParseTuple(args,"sdddd",&result,&r,&x,&y,&z))
	{
		PyErr_SetString(PyRunTimeErrU,"Could not import 1 char and 4 doubles");
		
	}
	// Make sure the specified result object does not exist:
	CVPYTHONRepositoryExistsMacro(result,PyRunTimeErrU)

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

    if (obj == NULL) {
        PyErr_SetString( PyRunTimeErrU, "Problem generating circle" );
        
    }
	//Save Result
	obj->SetName( result );
	if ( !( gRepository->Register( obj->GetName(), obj ) ) ) {
		PyErr_SetString(PyRunTimeErrU,"Error registering obj in repository.");

		delete obj;
		
	}

    vtkSmartPointer<vtkPolyData> polydataObj =
    vtkSmartPointer<vtkPolyData>::New();
    polydataObj = obj->GetVtkPolyData();
    //instead of exporting the object name, output the vtkPolydata object
    PyObject* pyVtkObj=vtkPythonUtil::GetObjectFromPointer(polydataObj);
    return pyVtkObj;
}

static PyObject* itkutils_PdToImgCmd( CXX_PYTHON_STDARGS )
{
	//std::cout << "Command: PdToImg" << std::endl;
	char *inputPdName;
	char *result;

	if (!PyArg_ParseTuple(args,"ss",&inputPdName,&result))
	{
		PyErr_SetString(PyRunTimeErrU,"Could not import 2 chars");
		
	}
	//Get the input image from the repository
	RepositoryDataT type;
	cvRepositoryData *pd;
	vtkPolyData *vtkpd;
	if (inputPdName != NULL) {
		// Look up given image object:
		pd = gRepository->GetObject( inputPdName );
		if ( pd == NULL ) {

			PyErr_SetString(PyRunTimeErrU,"couldn't find object." );
			
		}
		// Make sure image is of type STRUCTURED_PTS_T:
		type = pd->GetType();
		if ( type != POLY_DATA_T ) {
			PyErr_SetString(PyRunTimeErrU,"error: object not of type PolyData");
			
		}
		// Retrive geometric information:
		vtkpd = ((cvPolyData*)pd)->GetVtkPolyData();
	}

	// Make sure the specified result object does not exist:
	CVPYTHONRepositoryExistsMacro(result,PyRunTimeErrU)

	// Do work of command
	cvStrPts *obj = NULL;
	ImgInfo tempInfo;

	cvITKLSUtil::DefaultImgInfo.Print(std::cout);

	//tempInfo.Print(std::cout);
	cvITKLSUtil::vtkPolyDataTo2DImage(vtkpd,&obj,&tempInfo);

	//Save Result
	obj->SetName( result );
	if ( !( gRepository->Register( obj->GetName(), obj ) ) ) {
		PyErr_SetString(PyRunTimeErrU,"error registering obj in repository");
		delete obj;
		
	}

	// Return name
	return Py_BuildValue("s", obj->GetName());
}


static PyObject* itkutils_PdToVolCmd( CXX_PYTHON_STDARGS )
{
	//std::cout << "Command: PdToVol" << std::endl;
	char *usage;
	char *inputPdName;
	char *refName;
	char *result;
	if (!PyArg_ParseTuple(args,"sss",&inputPdName,&result,&refName))
    {
		PyErr_SetString(PyRunTimeErrU,"Could not import 3 chars");
		
	}
	//Get the input image from the repository
	RepositoryDataT type;
	cvRepositoryData *pd;
	vtkPolyData *vtkpd;
	if (inputPdName != NULL) {
		// Look up given image object:
		pd = gRepository->GetObject( inputPdName );
		if ( pd == NULL ) {

			PyErr_SetString(PyRunTimeErrU,"couldn't find object." );
			
		}
		// Make sure image is of type STRUCTURED_PTS_T:
		type = pd->GetType();
		if ( type != POLY_DATA_T ) {
			 PyErr_SetString(PyRunTimeErrU,"error: object not of type PolyData");
			
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
			PyErr_SetString(PyRunTimeErrU,"couldn't find object ");
			
		}
		// Make sure image is of type STRUCTURED_PTS_T:
		type = ref->GetType();
		if ( type != STRUCTURED_PTS_T ) {
			PyErr_SetString(PyRunTimeErrU, "error: object not of type StructuredPts");
			
		}
		// Retrive geometric information:
		vtkref = (vtkStructuredPoints*)((cvStrPts*)ref)->GetVtkPtr();
	}

	// Make sure the specified result object does not exist:
	CVPYTHONRepositoryExistsMacro(result,PyRunTimeErrU)

	// Do work of command
	cvStrPts *obj = NULL;
	ImgInfo tempInfo = ImgInfo(vtkref);
	vtkref->Delete();
	//tempInfo.Print(std::cout);
	cvITKLSUtil::vtkPolyDataToVolume(vtkpd,&obj,&tempInfo);

	//Save Result
	obj->SetName( result );
	if ( !( gRepository->Register( obj->GetName(), obj ) ) ) {
		PyErr_SetString(PyRunTimeErrU,"error registering obj in repository");
		delete obj;
		
	}

	//Return Name
	return Py_BuildValue("s",obj->GetName());

}

static PyObject* itkutils_WriteImageCmd(CXX_PYTHON_STDARGS)
{
	std::cout << "Command: WriteImage" << std::endl;
	char *usage;
	char *inputImgName;
	char *fname;

	if (!PyArg_ParseTuple(args,"ss",&inputImgName,&fname))
	{
		PyErr_SetString(PyRunTimeErrU,"Could not import 2 chars");
		
	}

	/*
	int table_size = 2;
	ARG_Entry arg_table[] = {
			{ "-src", STRING_Type, &inputImgName, NULL, REQUIRED, 0, { 0 } },
			{ "-fname", STRING_Type, &fname, NULL, REQUIRED, 0, { 0 } },
	};
	usage = ARG_GenSyntaxStr( 1, argv, table_size, arg_table );

	CVPYTHONTestArgsMacro(1)
	CVPYTHONParseTclStrMacro(1)
	*/
	//Get the input image from the repository
	RepositoryDataT type;
	cvRepositoryData *img;
	vtkStructuredPoints *vtksp;
	if (inputImgName != NULL) {
		// Look up given image object:
		img = gRepository->GetObject( inputImgName );
		if ( img == NULL ) {
			PyErr_SetString(PyRunTimeErrU, "couldn't find object ");
			
		}
		// Make sure image is of type STRUCTURED_PTS_T:
		type = img->GetType();
		if ( type != STRUCTURED_PTS_T ) {
			PyErr_SetString(PyRunTimeErrU, "error: object not of type StructuredPts" );
			
		}
		// Retrive geometric information:
		vtksp = ((cvStrPts*)img)->GetVtkStructuredPoints();
	}

	// Do work of command
	std::string sfname(fname);
	cvITKLSUtil::WritePerciseVtkImage(vtksp,sfname);


	return Py_BuildValue("s",fname);

}

PyObject* itkutils_GradientMagnitudeGaussianCmd( CXX_PYTHON_STDARGS )
{
	//std::cout << "Command: GradientMagnitudeGaussian" << std::endl;
	char *usage;
	char *inputImgName;
	char *result;
	double sigma = 1;

	if (!PyArg_ParseTuple(args,"sss",&inputImgName,&result,&sigma))
	{
		PyErr_SetString(PyRunTimeErrU,"Could not import 3 chars");
		
	}


	// Get the input image from the repository
	RepositoryDataT type;
	cvRepositoryData *img;
	vtkStructuredPoints *vtksp;
	if (inputImgName != NULL) {
		// Look up given image object:
		img = gRepository->GetObject( inputImgName );
		if ( img == NULL ) {
			PyErr_SetString(PyRunTimeErrU, "couldn't find object " );
			
		}

		// Make sure image is of type STRUCTURED_PTS_T:
		type = img->GetType();
		if ( type != STRUCTURED_PTS_T ) {
			PyErr_SetString(PyRunTimeErrU, "error: object not of type StructuredPts" );
			
		}
		// Retrive geometric information:
		vtksp = ((cvStrPts*)img)->GetVtkStructuredPoints();
	}

	// Make sure the specified result object does not exist:
	CVPYTHONRepositoryExistsMacro(result,PyRunTimeErrU)

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
		PyErr_SetString(PyRunTimeErrU, "error registering obj in repository");
		delete obj;
		
	}

	// Return Name
	return Py_BuildValue("s",obj->GetName());

}

PyObject* itkutils_GaussianCmd( CXX_PYTHON_STDARGS )
{
	//std::cout << "Command: GradientMagnitudeGaussian" << std::endl;
	char *usage;
	char *inputImgName;
	char *result;
	double sigma = 1;

	if (!PyArg_ParseTuple(args,"sss",&inputImgName,&result,&sigma))
	{
		PyErr_SetString(PyRunTimeErrU,"Could not import 3 chars");
		
	}


	// Get the input image from the repository
	RepositoryDataT type;
	cvRepositoryData *img;
	vtkStructuredPoints *vtksp;
	if (inputImgName != NULL) {
		// Look up given image object:
		img = gRepository->GetObject( inputImgName );
		if ( img == NULL ) {
			PyErr_SetString(PyRunTimeErrU, "couldn't find object ");
			
		}

		// Make sure image is of type STRUCTURED_PTS_T:
		type = img->GetType();
		if ( type != STRUCTURED_PTS_T ) {
			PyErr_SetString(PyRunTimeErrU, "error: object not of type StructuredPts");
			
		}
		// Retrive geometric information:
		vtksp = ((cvStrPts*)img)->GetVtkStructuredPoints();
	}

	// Make sure the specified result object does not exist:
	CVPYTHONRepositoryExistsMacro(result,PyRunTimeErrU)

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
		PyErr_SetString(PyRunTimeErrU, "error registering obj in repository" );
		delete obj;
		
	}

	// Return Name
	return Py_BuildValue("s",obj->GetName());

}

PyObject* itkutils_DistanceImageCmd( CXX_PYTHON_STDARGS )
{
	//std::cout << "Command: GradientMagnitudeGaussian" << std::endl;
	char *usage;
	char *inputImgName;
	char *result;
	double thres = .5;

	if (!PyArg_ParseTuple(args,"sss",&inputImgName,&result,&thres))
	{
		PyErr_SetString(PyRunTimeErrU,"Could not import 3 chars");
		
	}


	// Get the input image from the repository
	RepositoryDataT type;
	cvRepositoryData *img;
	vtkStructuredPoints *vtksp;
	if (inputImgName != NULL) {
		// Look up given image object:
		img = gRepository->GetObject( inputImgName );
		if ( img == NULL ) {
			PyErr_SetString(PyRunTimeErrU, "couldn't find object ");
			
		}

		// Make sure image is of type STRUCTURED_PTS_T:
		type = img->GetType();
		if ( type != STRUCTURED_PTS_T ) {
			PyErr_SetString(PyRunTimeErrU, "error: object not of type StructuredPts");
			
		}
		// Retrive geometric information:
		vtksp = ((cvStrPts*)img)->GetVtkStructuredPoints();
	}

	// Make sure the specified result object does not exist:
	CVPYTHONRepositoryExistsMacro(result,PyRunTimeErrU)

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
		PyErr_SetString(PyRunTimeErrU, "error registering obj in repository");
		delete obj;
		
	}

	// Return Name
	return Py_BuildValue("s",obj->GetName());

}


PyObject* itkutils_ThresholdImageCmd( CXX_PYTHON_STDARGS )
{
	//std::cout << "Command: GradientMagnitudeGaussian" << std::endl;
	char *usage;
	char *inputImgName;
	char *result;
	double thres = .5;


	if (!PyArg_ParseTuple(args,"sss",&inputImgName,&result,&thres))
	{
		PyErr_SetString(PyRunTimeErrU,"Could not import 3 chars");
		
	}

	// Get the input image from the repository
	RepositoryDataT type;
	cvRepositoryData *img;
	vtkStructuredPoints *vtksp;
	if (inputImgName != NULL) {
		// Look up given image object:
		img = gRepository->GetObject( inputImgName );
		if ( img == NULL ) {
			PyErr_SetString(PyRunTimeErrU, "couldn't find object " );
			
		}

		// Make sure image is of type STRUCTURED_PTS_T:
		type = img->GetType();
		if ( type != STRUCTURED_PTS_T ) {
			PyErr_SetString(PyRunTimeErrU, "error: object not of type StructuredPts");
			
		}
		// Retrive geometric information:
		vtksp = ((cvStrPts*)img)->GetVtkStructuredPoints();
	}

	// Make sure the specified result object does not exist:
	CVPYTHONRepositoryExistsMacro(result,PyRunTimeErrU)

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
		PyErr_SetString(PyRunTimeErrU, "error registering obj in repository");
		delete obj;
		
	}

	// Return Name
	return Py_BuildValue("s",obj->GetName());

}

//TODO: Template this better. Replace with redesign.
PyObject* itkutils_FractEdgeProximity3DCmd( CXX_PYTHON_STDARGS )
{
	//std::cout << "Command: GradientMagnitudeGaussian" << std::endl;
	char *usage;
	char *inputImgName;
	char *result;
	double sigma = 1, kappa=5, exponent=5;

	if (!PyArg_ParseTuple(args,"ssddd",&inputImgName,&result,&sigma,&kappa,&exponent))
	{
		PyErr_SetString(PyRunTimeErrU,"Could not import 2 chars and 3 doubles");
		
	}


	// Get the input image from the repository
	RepositoryDataT type;
	cvRepositoryData *img;
	vtkStructuredPoints *vtksp;
	if (inputImgName != NULL) {
		// Look up given image object:
		img = gRepository->GetObject( inputImgName );
		if ( img == NULL ) {
			PyErr_SetString(PyRunTimeErrU, "couldn't find object ");
			
		}

		// Make sure image is of type STRUCTURED_PTS_T:
		type = img->GetType();
		if ( type != STRUCTURED_PTS_T ) {
			PyErr_SetString(PyRunTimeErrU, "error: object not of type StructuredPts" );
			
		}
		// Retrive geometric information:
		vtksp = ((cvStrPts*)img)->GetVtkStructuredPoints();
	}

	// Make sure the specified result object does not exist:
	CVPYTHONRepositoryExistsMacro(result,PyRunTimeErrU)

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
		PyErr_SetString(PyRunTimeErrU, "error registering obj in repository");
		delete obj;
		
	}

	// Return Name
	return Py_BuildValue("s",obj->GetName());

}
