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
 * cv_itkls2d_init.cxx
 *
 *  Created on: Feb 12, 2014
 *      Author: Jameson Merkow
 */

#include "SimVascular.h"
#include "cvPYTHONMacros.h"
#include "Python.h"

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
#include "cv_ITKLset2d_init_py.h"

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
typedef itk::Image<short,2> ImageType;
// cvITKLevelSets New and delete
// cvITKLevelSets object methods
static PyObject*  itkls2d_ObjectCmd( CXX_PYTHON_STDARGS );
static PyObject*  itkls2d_SetInputsMtd( CXX_PYTHON_STDARGS );
static PyObject*  itkls2d_PhaseOneLevelSetMtd( CXX_PYTHON_STDARGS );
static PyObject*  itkls2d_PhaseTwoLevelSetMtd( CXX_PYTHON_STDARGS );
static PyObject*  itkls2d_GACLevelSetMtd( CXX_PYTHON_STDARGS );
PyObject *PyRunTimeErr2d;
CVPYTHONMtdDeclareMacro(itkls2d,GetFront);
CVPYTHONMtdDeclareMacro(itkls2d,GetFrontImage);
CVPYTHONMtdDeclareMacro(itkls2d,GetVelocityImage);
CVPYTHONMtdDeclareMacro(itkls2d,WriteFront);
CVPYTHONMtdDeclareMacro(itkls2d,SetMaxIterations);
CVPYTHONMtdDeclareMacro(itkls2d,SetMaxRMSError);
CVPYTHONMtdDeclareMacro(itkls2d,SetDebug);
CVPYTHONMtdDeclareMacro(itkls2d,SetPropagationScaling);
CVPYTHONMtdDeclareMacro(itkls2d,SetAdvectionScaling);
CVPYTHONMtdDeclareMacro(itkls2d,SetCurvatureScaling);

CVPYTHONMtdDeclareMacro(itkls2d,SetUseNormalVectorCurvature);
CVPYTHONMtdDeclareMacro(itkls2d,SetUseMeanCurvature);
CVPYTHONMtdDeclareMacro(itkls2d,SetUseMinimalCurvature);


// cvITKLevelSet Properties
CVPYTHONObjMemberGetObjPropertyMacro(itkls2d,cvITKLevelSet,Front,cvPolyData,PyRunTimeErr2d);
CVPYTHONObjMemberGetObjPropertyMacro(itkls2d,cvITKLevelSet,VelocityImage,cvStrPts,PyRunTimeErr2d);
CVPYTHONObjMemberGetObjPropertyMacro(itkls2d,cvITKLevelSet,FrontImage,cvStrPts,PyRunTimeErr2d);

CVPYTHONObjMemberSetPropertyMacro(itkls2d,cvITKLevelSet,MaxIterations,int,"i",PyRunTimeErr2d);
CVPYTHONObjMemberSetPropertyMacro(itkls2d,cvITKLevelSet,MaxRMSError,double,"d",PyRunTimeErr2d);
CVPYTHONObjMemberSetPropertyMacro(itkls2d,cvITKLevelSet,PropagationScaling,double,"d",PyRunTimeErr2d);
CVPYTHONObjMemberSetPropertyMacro(itkls2d,cvITKLevelSet,CurvatureScaling,double,"d",PyRunTimeErr2d);
CVPYTHONObjMemberSetPropertyMacro(itkls2d,cvITKLevelSet,AdvectionScaling,double,"d",PyRunTimeErr2d);
CVPYTHONObjMemberSetPropertyMacro(itkls2d,cvITKLevelSet,Debug,bool,"i",PyRunTimeErr2d);
CVPYTHONObjMemberSetPropertyMacro(itkls2d,cvITKLevelSet,UseInputImageAsFeature,bool,"i",PyRunTimeErr2d);
CVPYTHONObjMemberSetPropertyMacro(itkls2d,cvITKLevelSet,UseInputImageDistance,bool,"i",PyRunTimeErr2d);
// CVPYTHONObjMemberSetPropertyMacro(itkls2d,cvITKLevelSet,UseNormalVectorCurvature,bool,BOOL_Type);
// CVPYTHONObjMemberSetPropertyMacro(itkls2d,cvITKLevelSet,UseMeanCurvature,bool,BOOL_Type);
// CVPYTHONObjMemberSetPropertyMacro(itkls2d,cvITKLevelSet,UseMinimalCurvature,bool,BOOL_Type);

PyMethodDef Itkls2d_methods[] = {
    {"SetInputs", itkls2d_SetInputsMtd, METH_VARARGS,NULL},
    {"PhaseOneLevelSet", itkls2d_PhaseOneLevelSetMtd, METH_VARARGS,NULL},
    {"PhaseTwoLevelSet", itkls2d_PhaseTwoLevelSetMtd, METH_VARARGS,NULL},
    {"GACLevelSet", itkls2d_GACLevelSetMtd, METH_VARARGS,NULL},
    {"WriteFront", itkls2d_WriteFrontMtd, METH_VARARGS,NULL},
    {NULL, NULL,0,NULL},
};

#ifdef SV_USE_PYTHON3
static struct PyModuleDef Itkls2dmodule = {
   PyModuleDef_HEAD_INIT,
   "Itkls2d",   /* name of module */
   "", /* module documentation, may be NULL */
   -1,       /* size of per-interpreter state of the module,
                or -1 if the module keeps state in global variables. */
   Itkls2d_methods
};
#endif

// -------------
// Itkls2d_Init
// -------------
PyObject* Itkls2d_pyInit(){

	//Usage: CVPYTHONFunctionInit(Prefix,FunctionName,TclName)

    PyObject *pyItkls2D;
#ifdef SV_USE_PYTHON2
    pyItkls2D = Py_InitModule("Itkls2d",Itkls2d_methods);
#endif
#ifdef SV_USE_PYTHON3
    pyItkls2D = PyModule_Create(&Itkls2dmodule);
#endif
    PyRunTimeErr2d = PyErr_NewException("Itkls2d.error",NULL,NULL);
    PyModule_AddObject(pyItkls2D,"error",PyRunTimeErr2d);

   // PyObject* pyItkls = PyImport_ImportModuleNoBlock("pyItkls");
    //if (pyItkls == NULL){
   //     printf("error importing module pyItkls");}
   // else{
   //     PyModule_AddObject(pyItkls,"Itkls2d",pyItkls2D);
   // }

	return pyItkls2D;

}




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

PyObject* itkls2d_SetInputsMtd( CXX_PYTHON_STDARGS )
{
	//`cvITKLevelSet *ls = (cvITKLevelSet *)clientData;
	cvITKLevelSet *ls;
	char *inputImageName;
	char *seedPdName;

	if(!PyArg_ParseTuple(args, "ss",&inputImageName,&seedPdName))
	{
		PyErr_SetString(PyRunTimeErr2d,"Could not import 2 chars");
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
			PyErr_SetString(PyRunTimeErr2d, temp );
			return SV_ERROR;
		}
		printf("Found Object\n");
		// Make sure image is of type STRUCTURED_PTS_T:
		typeImg1 = inputImage->GetType();
		if ( typeImg1 != STRUCTURED_PTS_T ) {
			char temp[2048];
			sprintf(temp,"error: object ", inputImageName, "not of type StructuredPts", (char *)NULL);
			PyErr_SetString(PyRunTimeErr2d, temp );
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
			PyErr_SetString(PyRunTimeErr2d, temp );
			return SV_ERROR;
		}
		printf("Found Object\n");
		// Make sure image is of type STRUCTURED_PTS_T:
		typeImg2 = seedPolyData->GetType();
		if ( typeImg2 != POLY_DATA_T) {
			char temp[2048];
			sprintf(temp,"error: object ", seedPdName, "not of type PolyData", (char *)NULL);
			PyErr_SetString(PyRunTimeErr2d, temp );
			return SV_ERROR;
		}
	}

	ls->SetInputImage((cvStrPts*)inputImage);
	ls->SetSeed((cvPolyData*)seedPolyData);


	return Py_BuildValue("s","success");
}

/*
 *
 *
 */

static PyObject* itkls2d_PhaseOneLevelSetMtd( CXX_PYTHON_STDARGS )
{
	//cvITKLevelSet *ls = (cvITKLevelSet *)clientData;
	cvITKLevelSet *ls ;
	double kc, expFactorRising,expFactorFalling, advScale;

	double sigmaFeat = -1, sigmaAdv = -1;


	if (!PyArg_ParseTuple(args,"ddddd",&kc,&expFactorRising,&expFactorFalling,
					&sigmaFeat,&sigmaAdv))
	{
		PyErr_SetString(PyRunTimeErr2d,"Could not import 5 doubles");
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

static PyObject* itkls2d_PhaseTwoLevelSetMtd( CXX_PYTHON_STDARGS )
{
	cvITKLevelSet *ls;

	double klow, kupp;
	double sigmaFeat = -1, sigmaAdv = -1;
	if (!PyArg_ParseTuple(args,"dddd",&klow,&kupp,
					&sigmaFeat,&sigmaAdv))
	{
		PyErr_SetString(PyRunTimeErr2d,"Could not import 4 doubles");
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
static PyObject* itkls2d_GACLevelSetMtd( CXX_PYTHON_STDARGS )
{
	cvITKLevelSet *ls;
	char *usage;
	double sigma, expFactor;

	if (!PyArg_ParseTuple(args,"dd",&expFactor,&sigma))
	{
		PyErr_SetString(PyRunTimeErr2d,"Could not import 2 doubles");
		return SV_ERROR;
	}


	ls->ComputeGACLevelSet(expFactor);


	return Py_BuildValue("s","success");
}



static PyObject*  itkls2d_WriteFrontMtd( CXX_PYTHON_STDARGS )
{
	cvITKLevelSet *ls;
	ls->WriteFrontImages();
	return Py_BuildValue("s","success");
}




