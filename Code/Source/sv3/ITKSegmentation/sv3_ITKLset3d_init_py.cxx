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
#include "SimVascular_python.h"
#include "Python.h"
#include "sv3_ITKLSet_PYTHON_Macros.h"

#include "sv2_LsetCore_init.h"
#include "sv3_ITKLevelSet.h"
#include "sv3_ITKLevelSetBase.h"
#include "sv2_LevelSetVelocity.h"
#include "sv_Repository.h"
#include "sv_SolidModel.h"
#include "sv_misc_utils.h"
#include "sv_arg.h"

#include "sv3_ITKLset_ITKUtils.h"
#include "sv2_globals.h"
#include "sv3_ITKLset3d_init_py.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#include "sv2_globals.h"

// Helper fns
// ----------

// NewName: Checks if the name is already used
static int NewName( CONST84 char *name );
static void PrintMethods();

// Prototypes:
// -----------
typedef itk::Image<short,3> ImageType;
typedef struct
{
  PyObject_HEAD
  cvITKLevelSetBase<ImageType> *ls;
}pyLevelSet3d;
// cvITKLevelSets object methods
//static PyObject* itkls3d_ObjectCmd( pyLevelSet* self, PyObject* args );
static pyLevelSet3d* itkls3d_NewCmd(  pyLevelSet3d* self, PyObject* args );
PyObject* Deleteitkls3d(pyLevelSet3d* self, PyObject* args );
static PyObject* itkls3d_SetInputsMtd( pyLevelSet3d* self, PyObject* args );
static PyObject* itkls3d_PhaseOneLevelSetMtd( pyLevelSet3d* self, PyObject* args );
static PyObject* itkls3d_PhaseTwoLevelSetMtd( pyLevelSet3d* self, PyObject* args );
static PyObject* itkls3d_GACLevelSetMtd( pyLevelSet3d* self, PyObject* args );
static PyObject* itkls3d_LaplacianLevelSetMtd( pyLevelSet3d* self, PyObject* args );
static PyObject* itkls3d_CopyFrontToSeedMtd( pyLevelSet3d* self, PyObject* args );
static PyObject* itkls3d_WriteFrontMtd( pyLevelSet3d* self, PyObject* args );
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

static int pyLevelSet3d_init(pyLevelSet3d* self, PyObject* args)
{
  fprintf(stdout,"pyLevelSet3d initialized.\n");
  return SV_OK;
}

PyMethodDef pyLevelSet3d_methods[] = {
	{"NewLevelSetObject", (PyCFunction)itkls3d_NewCmd, METH_VARARGS,NULL},
	{"DeleteLevelSetObject",(PyCFunction)Deleteitkls3d, METH_NOARGS,NULL },
    {"SetInputs", (PyCFunction)itkls3d_SetInputsMtd, METH_VARARGS,NULL},
    {"PhaseOneLevelSet", (PyCFunction)itkls3d_PhaseOneLevelSetMtd, METH_VARARGS,NULL},
    {"PhaseTwoLevelSet", (PyCFunction)itkls3d_PhaseTwoLevelSetMtd, METH_VARARGS,NULL},
    {"GACLevelSet", (PyCFunction)itkls3d_GACLevelSetMtd, METH_VARARGS,NULL},
    {"LaplacianLevelSet",(PyCFunction)itkls3d_LaplacianLevelSetMtd,METH_VARARGS,NULL},
    //{"WriteFront", (PyCFunction)itkls3d_WriteFrontMtd, METH_VARARGS,NULL},
    {"CopyFrontToSeed",(PyCFunction)itkls3d_CopyFrontToSeedMtd,METH_VARARGS,NULL},
    {NULL, NULL,0,NULL},
};
PyMethodDef Itkls3d_methods[] = {
    {NULL, NULL,0,NULL},
};

static PyTypeObject pyLevelSet3dType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  "pyLevelSet3d.pyLevelSet3d",             /* tp_name */
  sizeof(pyLevelSet3d),             /* tp_basicsize */
  0,                         /* tp_itemsize */
  0,                         /* tp_dealloc */
  0,                         /* tp_print */
  0,                         /* tp_getattr */
  0,                         /* tp_setattr */
  0,                         /* tp_compare */
  0,                         /* tp_repr */
  0,                         /* tp_as_number */
  0,                         /* tp_as_sequence */
  0,                         /* tp_as_mapping */
  0,                         /* tp_hash */
  0,                         /* tp_call */
  0,                         /* tp_str */
  0,                         /* tp_getattro */
  0,                         /* tp_setattro */
  0,                         /* tp_as_buffer */
  Py_TPFLAGS_DEFAULT |
      Py_TPFLAGS_BASETYPE,   /* tp_flags */
  "pyLevelSet3d objects",           /* tp_doc */
  0,                         /* tp_traverse */
  0,                         /* tp_clear */
  0,                         /* tp_richcompare */
  0,                         /* tp_weaklistoffset */
  0,                         /* tp_iter */
  0,                         /* tp_iternext */
  pyLevelSet3d_methods,             /* tp_methods */
  0,                         /* tp_members */
  0,                         /* tp_getset */
  0,                         /* tp_base */
  0,                         /* tp_dict */
  0,                         /* tp_descr_get */
  0,                         /* tp_descr_set */
  0,                         /* tp_dictoffset */
  (initproc)pyLevelSet3d_init,       /* tp_init */
  0,                         /* tp_alloc */
  0,                  /* tp_new */
};

#if PYTHON_MAJOR_VERSION == 3
static struct PyModuleDef Itkls3dmodule = {
   PyModuleDef_HEAD_INIT,
   "Itkls3d",   /* name of module */
   "", /* module documentation, may be NULL */
   -1,       /* size of per-interpreter state of the module,
                or -1 if the module keeps state in global variables. */
   Itkls3d_methods
};
#endif
// -------------
// Itkls3d_Init
// -------------
PyObject* Itkls3d_pyInit(){

	//Usage: CVPYTHONFunctionInit(Prefix,FunctionName,TclName)
    pyLevelSet3dType.tp_new=PyType_GenericNew;
	if (PyType_Ready(&pyLevelSet3dType)<0)
    {
      fprintf(stdout,"Error in pyLevelSet3dType\n");
    }
    PyObject *pyItkls3D;

#if PYTHON_MAJOR_VERSION == 2
    pyItkls3D = Py_InitModule("Itkls3d",Itkls3d_methods);
#elif PYTHON_MAJOR_VERSION == 3
    pyItkls3D = PyModule_Create(&Itkls3dmodule);
#endif
    PyRunTimeErr3d = PyErr_NewException("Itkls3d.error",NULL,NULL);
    PyModule_AddObject(pyItkls3D,"error",PyRunTimeErr3d);
    Py_INCREF(&pyLevelSet3dType);
    PyModule_AddObject(pyItkls3D,"pyLevelSet3d",(PyObject*)&pyLevelSet3dType);

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

// -------------
// itkls3d_NewCmd
// -------------

// itkls3d <objName> Or ITKLevelSet <objName>
static pyLevelSet3d* itkls3d_NewCmd(  pyLevelSet3d* self, PyObject* args )
{

	CONST84 char *lsName;
	cvITKLevelSetBase<ImageType> *ls;
	Tcl_HashEntry *entryPtr;
	int newEntry = 0;


	// Check syntax:
	if(!PyArg_ParseTuple(args, "s",&lsName))
	{
		PyErr_SetString(PyRunTimeErr3d,"Could not import 1 char, lsname");
		
	}

	// Make sure this is a new object name:
	if ( !NewName( lsName ) ) {
		PyErr_SetString(PyRunTimeErr3d, "ITKLevelSetCore object already exists");
		
	}

	// Allocate new cvLevelSet object:
	ls = new cvITKLevelSetBase<ImageType>;
	if ( ls == NULL ) {
		PyErr_SetString(PyRunTimeErr3d,"error allocating object");
		
	}

	strcpy( ls->tclName_, lsName );
	entryPtr = Tcl_CreateHashEntry( &gLsetCoreTable, lsName, &newEntry );
	if ( !newEntry ) {
		PyErr_SetString(PyRunTimeErr3d, "error updating cvLevelSet hash table");
		delete ls;
		
	}
	Tcl_SetHashValue( entryPtr, (ClientData)ls );
    Py_INCREF(ls);
    self->ls=ls;
    Py_DECREF(ls);
    return self;
}

// --------------
// DeleteLsetCore
// --------------
// Deletion callback invoked when the Tcl object is deleted.  Delete
// Tcl hash table entry as well as the cvITKLevelSet object itself.

PyObject* Deleteitkls3d( pyLevelSet3d* self, PyObject* args )
{

	cvITKLevelSetBase<ImageType> *ls = self->ls;
	Tcl_HashEntry *entryPtr;

	entryPtr = Tcl_FindHashEntry( &gLsetCoreTable, ls->tclName_ );
	if ( entryPtr == NULL ) {
		printf("Error looking up LsetCore object %s for deletion.\n",
				ls->tclName_);
	} else {
		Tcl_DeleteHashEntry( entryPtr );
	}
	delete ls;
	return SV_PYTHON_OK;
}

PyObject* itkls3d_SetInputsMtd( pyLevelSet3d* self, PyObject* args )
{
	//cvITKLevelSetBase<ImageType> *ls = (cvITKLevelSetBase<ImageType> *)clientData;
	cvITKLevelSetBase<ImageType> *ls=self->ls;
	char *inputImageName;
	char *seedPdName;

	if(!PyArg_ParseTuple(args, "ss",&inputImageName,&seedPdName))
	{
		PyErr_SetString(PyRunTimeErr3d,"Could not import 2 chars");
		
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
			
		}
		printf("Found Object\n");
		// Make sure image is of type STRUCTURED_PTS_T:
		typeImg1 = inputImage->GetType();
		if ( typeImg1 != STRUCTURED_PTS_T ) {
			char temp[2048];
			sprintf(temp,"error: object ", inputImageName, "not of type StructuredPts", (char *)NULL);
			PyErr_SetString(PyRunTimeErr3d, temp );
			
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
			
		}
		printf("Found Object\n");
		// Make sure image is of type STRUCTURED_PTS_T:
		typeImg2 = seedPolyData->GetType();
		if ( typeImg2 != POLY_DATA_T) {
			char temp[2048];
			sprintf(temp,"error: object ", seedPdName, "not of type PolyData", (char *)NULL);
			PyErr_SetString(PyRunTimeErr3d, temp );
			
		}
	}

	ls->SetInputImage((cvStrPts*)inputImage);
	ls->SetSeed((cvPolyData*)seedPolyData);


	return SV_PYTHON_OK;
}


static PyObject* itkls3d_PhaseOneLevelSetMtd( pyLevelSet3d* self, PyObject* args )
{
	cvITKLevelSetBase<ImageType> *ls=self->ls;
	double kc, expFactorRising,expFactorFalling, advScale;

	double sigmaFeat = -1, sigmaAdv = -1;

	if (!PyArg_ParseTuple(args,"ddd|dd",&kc,&expFactorRising,&expFactorFalling,
	&sigmaFeat,&sigmaAdv))
    {
		PyErr_SetString(PyRunTimeErr3d,"Could not import 5 doubles");
		
	}
	std::cout << "sigmaFeat " << sigmaFeat << std::endl;

	if(sigmaFeat >= 0)
		ls->SetSigmaFeature(sigmaFeat);
	if(sigmaAdv >= 0)
		ls->SetSigmaAdvection(sigmaAdv);

	ls->ComputePhaseOneLevelSet(kc, expFactorRising,expFactorFalling);

	return SV_PYTHON_OK;
}

static PyObject* itkls3d_PhaseTwoLevelSetMtd( pyLevelSet3d* self, PyObject* args )
{
	cvITKLevelSetBase<ImageType> *ls=self->ls;
	char *usage;
	double klow, kupp;
	double sigmaFeat = -1, sigmaAdv = -1;

	if (!PyArg_ParseTuple(args,"dd|dd",&klow,&kupp,
	&sigmaFeat,&sigmaAdv))
    {
		PyErr_SetString(PyRunTimeErr3d,"Could not import 4 doubles");
		
    }


	//std::cout << "Entering LS" << std::endl;

	if(sigmaFeat >= 0)
		ls->SetSigmaFeature(sigmaFeat);
	if(sigmaAdv >= 0)
		ls->SetSigmaAdvection(sigmaAdv);


	ls->ComputePhaseTwoLevelSet(kupp,klow);

	return SV_PYTHON_OK;
}
static PyObject* itkls3d_GACLevelSetMtd( pyLevelSet3d* self, PyObject* args )
{
	cvITKLevelSetBase<ImageType> *ls=self->ls;
	char *usage;
	double sigma, expFactor,kappa,iso;
	if (!PyArg_ParseTuple(args,"ddd|d",&expFactor,&kappa,&iso,&sigma))
	{
		PyErr_SetString(PyRunTimeErr3d,"Could not import 4 doubles");
		
	}

	if(sigma >= 0)
		ls->SetSigmaFeature(sigma);
	ls->ComputeGACLevelSet(expFactor,kappa,iso);

	return SV_PYTHON_OK;
}
static PyObject* itkls3d_LaplacianLevelSetMtd( pyLevelSet3d* self, PyObject* args )
{
	cvITKLevelSetBase<ImageType> *ls=self->ls;
	char *usage;
	double sigma, expFactor,kappa,iso;
	std::cout << "Laplacian" << std::endl;
	if (!PyArg_ParseTuple(args,"ddd|d",&expFactor,&kappa,&iso,&sigma))
	{
		PyErr_SetString(PyRunTimeErr3d,"Could not import 4 doubles");
		
	}

	if(sigma >= 0)
		ls->SetSigmaFeature(sigma);
	ls->ComputeLaplacianLevelSet(expFactor,kappa,iso);


	return SV_PYTHON_OK;
}


static PyObject* itkls3d_WriteFrontMtd( pyLevelSet3d* self, PyObject* args )
{
	cvITKLevelSetBase<ImageType> *ls=self->ls;
	ls->WriteFrontImages();
	return SV_PYTHON_OK;
}

static PyObject* itkls3d_CopyFrontToSeedMtd(pyLevelSet3d* self, PyObject* args)
{
	cvITKLevelSetBase<ImageType> *ls=self->ls;
	ls->CopyFrontToSeed();
	return SV_PYTHON_OK;
}




