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
 * cv_itkls3d_init.cxx
 *
 *  Created on: Feb 12, 2014
 *      Author: Jameson Merkow
 */

#include "SimVascular.h"
#include "Python.h"
#include "cvPYTHONMacros.h"

#include "cv_LsetCore_init.h"
#include "cvITKLevelSet.h"
#include "cvITKLevelSetBase.h"
#include "cvLevelSetVelocity.h"
#include "cvRepository.h"
#include "cvSolidModel.h"
#include "cv_misc_utils.h"
#include "cv_arg.h"

#include "cvITKUtils.h"
#include "cv_globals.h"
#include "cv_ITKLset3d_init_py.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#include "cv_globals.h"

// Helper fns
// ----------

// NewName: Checks if the name is already used
static int NewName( CONST84 char *name );
static void PrintMethods();

// Prototypes:
// -----------
typedef itk::Image<short,3> ImageType;
// cvITKLevelSets object methods
static PyObject* itkls3d_ObjectCmd( CXX_PYTHON_STDARGS );
static PyObject* itkls3d_SetInputsMtd( CXX_PYTHON_STDARGS );
static PyObject* itkls3d_PhaseOneLevelSetMtd( CXX_PYTHON_STDARGS );
static PyObject* itkls3d_PhaseTwoLevelSetMtd( CXX_PYTHON_STDARGS );
static PyObject* itkls3d_GACLevelSetMtd( CXX_PYTHON_STDARGS );
static PyObject* itkls3d_LaplacianLevelSetMtd( CXX_PYTHON_STDARGS );
static PyObject* itkls3d_CopyFrontToSeedMtd( CXX_PYTHON_STDARGS );
PyObject *PyRunTimeErr3d;
CVPYTHONMtdDeclareMacro(itkls3d,GetFront);
CVPYTHONMtdDeclareMacro(itkls3d,GetFrontImage);
CVPYTHONMtdDeclareMacro(itkls3d,GetVelocityImage);
CVPYTHONMtdDeclareMacro(itkls3d,WriteFront);
CVPYTHONMtdDeclareMacro(itkls3d,SetMaxIterations);
CVPYTHONMtdDeclareMacro(itkls3d,SetMaxRMSError);
CVPYTHONMtdDeclareMacro(itkls3d,SetDebug);
CVPYTHONMtdDeclareMacro(itkls3d,SetPropagationScaling);
CVPYTHONMtdDeclareMacro(itkls3d,SetLaplacianScaling);
CVPYTHONMtdDeclareMacro(itkls3d,SetAdvectionScaling);
CVPYTHONMtdDeclareMacro(itkls3d,SetCurvatureScaling);

CVPYTHONMtdDeclareMacro(itkls3d,SetUseNormalVectorCurvature);
CVPYTHONMtdDeclareMacro(itkls3d,SetUseMeanCurvature);
CVPYTHONMtdDeclareMacro(itkls3d,SetUseMinimalCurvature);
CVPYTHONMtdDeclareMacro(itkls3d,SetBinarySeed);


// cvITKLevelSet Properties
CVPYTHONObjMemberGetObjPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,Front,cvPolyData,PyRunTimeErr3d);
CVPYTHONObjMemberGetObjPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,VelocityImage,cvStrPts,PyRunTimeErr3d);
CVPYTHONObjMemberGetObjPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,FrontImage,cvStrPts,PyRunTimeErr3d);

CVPYTHONObjMemberSetPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,MaxIterations,int,"i",PyRunTimeErr3d);
CVPYTHONObjMemberSetPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,MaxRMSError,double,"d",PyRunTimeErr3d);
CVPYTHONObjMemberSetPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,PropagationScaling,double,"d",PyRunTimeErr3d);
CVPYTHONObjMemberSetPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,CurvatureScaling,double,"d",PyRunTimeErr3d);
CVPYTHONObjMemberSetPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,AdvectionScaling,double,"d",PyRunTimeErr3d);
CVPYTHONObjMemberSetPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,LaplacianScaling,double,"d",PyRunTimeErr3d);
CVPYTHONObjMemberSetPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,Debug,bool,"i",PyRunTimeErr3d);
CVPYTHONObjMemberSetPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,UseNormalVectorCurvature,bool,"i",PyRunTimeErr3d);
CVPYTHONObjMemberSetPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,UseMeanCurvature,bool,"i",PyRunTimeErr3d);
CVPYTHONObjMemberSetPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,UseMinimalCurvature,bool,"i",PyRunTimeErr3d);
CVPYTHONObjMemberSetPropertyMacro(itkls3d,cvITKLevelSetBase<ImageType>,BinarySeed,bool,"i",PyRunTimeErr3d);



PyMethodDef Itkls3d_methods[] = {
    {"SetInputs", itkls3d_SetInputsMtd, METH_VARARGS,NULL},
    {"PhaseOneLevelSet", itkls3d_PhaseOneLevelSetMtd, METH_VARARGS,NULL},
    {"PhaseTwoLevelSet", itkls3d_PhaseTwoLevelSetMtd, METH_VARARGS,NULL},
    {"GACLevelSet", itkls3d_GACLevelSetMtd, METH_VARARGS,NULL},
    {"LaplacianLevelSet",itkls3d_LaplacianLevelSetMtd,METH_VARARGS,NULL},
    {"WriteFront", itkls3d_WriteFrontMtd, METH_VARARGS,NULL},
    {"CopyFrontToSeed",itkls3d_CopyFrontToSeedMtd,METH_VARARGS,NULL},
    {NULL, NULL,0,NULL},
};


// -------------
// Itkls3d_Init
// -------------
PyObject* Itkls3d_pyInit(){

	//Usage: CVPYTHONFunctionInit(Prefix,FunctionName,TclName)

    PyObject *pyItkls3D;

    pyItkls3D = Py_InitModule("Itkls3d",Itkls3d_methods);
    PyRunTimeErr3d = PyErr_NewException("Itkls3d.error",NULL,NULL);
    PyModule_AddObject(pyItkls3D,"error",PyRunTimeErr3d);

    return pyItkls3D;

}


/*// Creates the rest of the commands
int itkls3d_ObjectCmd( CXX_PYTHON_STDARGS )
{
	////std::cout << "Method: " << argv[1] << std::endl;
	if ( argc == 1 ) {
		PrintMethods();
		return TCL_OK;
	}

	if ( Tcl_StringMatch( argv[1], "GetClassName" ) ) {
		Tcl_SetResult( interp, "ITKLevelSet3D", TCL_STATIC );
		return TCL_OK;
	}

	CVPYTHONObjMethodInit(itkls3d,SetInputsMtd,SetInputs)
	CVPYTHONObjMethodInit(itkls3d,PhaseOneLevelSetMtd,PhaseOneLevelSet)
	CVPYTHONObjMethodInit(itkls3d,PhaseTwoLevelSetMtd,PhaseTwoLevelSet)
	CVPYTHONObjMethodInit(itkls3d,GACLevelSetMtd,GACLevelSet)
	CVPYTHONObjMethodInit(itkls3d,LaplacianLevelSetMtd,LaplacianLevelSet)
	CVPYTHONObjMethodInit(itkls3d,GetFrontMtd,GetFront)
	CVPYTHONObjMethodInit(itkls3d,GetVelocityImageMtd,GetVelocityImage)
	CVPYTHONObjMethodInit(itkls3d,GetFrontImageMtd,GetFrontImage)
	CVPYTHONObjMethodInit(itkls3d,WriteFrontMtd,WriteFront);
	CVPYTHONObjMethodInit(itkls3d,SetMaxRMSErrorMtd,SetMaxRMSError)
	CVPYTHONObjMethodInit(itkls3d,SetMaxIterationsMtd,SetMaxIterations)
	CVPYTHONObjMethodInit(itkls3d,SetPropagationScalingMtd,SetPropagationScaling)
	CVPYTHONObjMethodInit(itkls3d,SetLaplacianScalingMtd,SetLaplacianScaling)
	CVPYTHONObjMethodInit(itkls3d,SetCurvatureScalingMtd,SetCurvatureScaling)
	CVPYTHONObjMethodInit(itkls3d,SetAdvectionScalingMtd,SetAdvectionScaling)
	CVPYTHONObjMethodInit(itkls3d,SetMaxIterationsMtd,SetMaxIterations)
	CVPYTHONObjMethodInit(itkls3d,SetDebugMtd,SetDebug)
	CVPYTHONObjMethodInit(itkls3d,CopyFrontToSeedMtd,CopyFrontToSeed)

	CVPYTHONObjMethodInit(itkls3d,SetUseNormalVectorCurvatureMtd,SetUseNormalVectorCurvature)
	CVPYTHONObjMethodInit(itkls3d,SetUseMeanCurvatureMtd,SetUseMeanCurvature)
	CVPYTHONObjMethodInit(itkls3d,SetUseMinimalCurvatureMtd,SetUseMinimalCurvature)
	CVPYTHONObjMethodInit(itkls3d,SetBinarySeedMtd,SetBinarySeed)

	Tcl_AppendResult( interp, "\"", argv[1],
			"\" not a recognized ITKLevelSet method", (char *)NULL );
	return TCL_ERROR;
}*/


//Helper functions
// NewName: Checks if the name is already used
static int NewName( CONST84 char *name )
{
	Tcl_HashEntry *entryPtr;
	entryPtr = Tcl_FindHashEntry( &gLsetCoreTable, name );
	if ( entryPtr != NULL ) {
		return 0;
	}
	return 1;
}

static void PrintMethods()
{
	printf("Im not sure yet...\n");
}



PyObject* itkls3d_SetInputsMtd( CXX_PYTHON_STDARGS )
{
	//cvITKLevelSetBase<ImageType> *ls = (cvITKLevelSetBase<ImageType> *)clientData;
	cvITKLevelSetBase<ImageType> *ls;
	char *inputImageName;
	char *seedPdName;

	if(!PyArg_ParseTuple(args, "ss",&inputImageName,&seedPdName))
	{
		PyErr_SetString(PyRunTimeErr3d,"Could not import 2 chars");
		return SV_ERROR;
	}



	RepositoryDataT typeImg1;
	char *typeImg1Str;
	typeImg1 = gRepository->GetType( inputImageName );
	typeImg1Str = RepositoryDataT_EnumToStr( typeImg1 );
	cvRepositoryData *inputImage;
	if (inputImageName != NULL) {
		// Look up given image object:
		inputImage = gRepository->GetObject( inputImageName );
		if ( inputImage == NULL ) {
			char temp[2048];
			sprintf(temp,"couldn't find object ", inputImageName, (char *)NULL );
			PyErr_SetString(PyRunTimeErr3d, temp );
			return SV_ERROR;
		}
		printf("Found Object\n");
		// Make sure image is of type STRUCTURED_PTS_T:
		typeImg1 = inputImage->GetType();
		if ( typeImg1 != STRUCTURED_PTS_T ) {
			char temp[2048];
			sprintf(temp,"error: object ", inputImageName, "not of type StructuredPts", (char *)NULL);
			PyErr_SetString(PyRunTimeErr3d, temp );
			return SV_ERROR;
		}
	}

	RepositoryDataT typeImg2;
	char *typeImg2Str;
	typeImg2 = gRepository->GetType( seedPdName );
	typeImg2Str = RepositoryDataT_EnumToStr( typeImg2 );
	cvRepositoryData *seedPolyData;

	if (seedPdName != NULL) {
		// Look up given image object:
		seedPolyData = gRepository->GetObject( seedPdName );
		if ( seedPolyData == NULL ) {
			char temp[2048];
			sprintf(temp,"couldn't find object ", seedPdName, (char *)NULL );
			PyErr_SetString(PyRunTimeErr3d, temp );
			return SV_ERROR;
		}
		printf("Found Object\n");
		// Make sure image is of type STRUCTURED_PTS_T:
		typeImg2 = seedPolyData->GetType();
		if ( typeImg2 != POLY_DATA_T) {
			char temp[2048];
			sprintf(temp,"error: object ", seedPdName, "not of type PolyData", (char *)NULL);
			PyErr_SetString(PyRunTimeErr3d, temp );
			return SV_ERROR;
		}
	}

	ls->SetInputImage((cvStrPts*)inputImage);
	ls->SetSeed((cvPolyData*)seedPolyData);


	return Py_BuildValue("s","success");
}


static PyObject* itkls3d_PhaseOneLevelSetMtd( CXX_PYTHON_STDARGS )
{
	cvITKLevelSetBase<ImageType> *ls;
	double kc, expFactorRising,expFactorFalling, advScale;

	double sigmaFeat = -1, sigmaAdv = -1;

	if (!PyArg_ParseTuple(args,"ddddd",&kc,&expFactorRising,&expFactorFalling,
	&sigmaFeat,&sigmaAdv))
    {
		PyErr_SetString(PyRunTimeErr3d,"Could not import 5 doubles");
		return SV_ERROR;
	}
	std::cout << "sigmaFeat " << sigmaFeat << std::endl;

	if(sigmaFeat >= 0)
		ls->SetSigmaFeature(sigmaFeat);
	if(sigmaAdv >= 0)
		ls->SetSigmaAdvection(sigmaAdv);

	ls->ComputePhaseOneLevelSet(kc, expFactorRising,expFactorFalling);

	return Py_BuildValue("s","success");
}

static PyObject* itkls3d_PhaseTwoLevelSetMtd( CXX_PYTHON_STDARGS )
{
	cvITKLevelSetBase<ImageType> *ls ;
	char *usage;
	double klow, kupp;
	double sigmaFeat = -1, sigmaAdv = -1;

	if (!PyArg_ParseTuple(args,"dddd",&klow,&kupp,
	&sigmaFeat,&sigmaAdv))
    {
		PyErr_SetString(PyRunTimeErr3d,"Could not import 4 doubles");
		return SV_ERROR;
    }


	//std::cout << "Entering LS" << std::endl;

	if(sigmaFeat >= 0)
		ls->SetSigmaFeature(sigmaFeat);
	if(sigmaAdv >= 0)
		ls->SetSigmaAdvection(sigmaAdv);


	ls->ComputePhaseTwoLevelSet(kupp,klow);

	return Py_BuildValue("s","success");
}
static PyObject* itkls3d_GACLevelSetMtd( CXX_PYTHON_STDARGS )
{
	cvITKLevelSetBase<ImageType> *ls ;
	char *usage;
	double sigma, expFactor,kappa,iso;
	if (!PyArg_ParseTuple(args,"dddd",&expFactor,&sigma,&kappa,&iso))
	{
		PyErr_SetString(PyRunTimeErr3d,"Could not import 4 doubles");
		return SV_ERROR;
	}

	if(sigma >= 0)
		ls->SetSigmaFeature(sigma);
	ls->ComputeGACLevelSet(expFactor,kappa,iso);

	return Py_BuildValue("s","success");
}
static PyObject* itkls3d_LaplacianLevelSetMtd( CXX_PYTHON_STDARGS )
{
	cvITKLevelSetBase<ImageType> *ls ;
	char *usage;
	double sigma, expFactor,kappa,iso;
	std::cout << "Laplacian" << std::endl;
	if (!PyArg_ParseTuple(args,"dddd",&expFactor,&sigma,&kappa,&iso))
	{
		PyErr_SetString(PyRunTimeErr3d,"Could not import 4 doubles");
		return SV_ERROR;
	}

	if(sigma >= 0)
		ls->SetSigmaFeature(sigma);
	ls->ComputeLaplacianLevelSet(expFactor,kappa,iso);


	return Py_BuildValue("s","success");
}


static PyObject* itkls3d_WriteFrontMtd( CXX_PYTHON_STDARGS )
{
	cvITKLevelSetBase<ImageType> *ls;
	ls->WriteFrontImages();
	return Py_BuildValue("s","success");
}

static PyObject* itkls3d_CopyFrontToSeedMtd(CXX_PYTHON_STDARGS)
{

	cvITKLevelSetBase<ImageType> *ls;
	ls->CopyFrontToSeed();
	return Py_BuildValue("s","success");
}




