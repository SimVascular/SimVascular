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
#include "SimVascular_python.h"
#include "sv3_ITKLSet_PYTHON_Macros.h"
#include "Python.h"

#include "sv2_LsetCore_init.h"
#include "sv3_ITKLevelSet.h"
#include "sv3_ITKLevelSetBase.h"
#include "sv2_LevelSetVelocity.h"
#include "sv_Repository.h"
#include "sv_SolidModel.h"
#include "sv_misc_utils.h"
#include "sv_arg.h"

#include "sv3_ITKLset_ITKUtils.h"
#include "sv3_ITKLset2d_init_py.h"

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
typedef itk::Image<short,2> ImageType;
typedef struct
{
  PyObject_HEAD
  cvITKLevelSet* ls;
}pyLevelSet;
// cvITKLevelSets New and delete
// cvITKLevelSets object methods
static pyLevelSet* itkls2d_NewCmd( pyLevelSet* self, PyObject* args );
PyObject* Deleteitkls2d(pyLevelSet* self, PyObject* args );
static PyObject*  itkls2d_ObjectCmd( pyLevelSet* self, PyObject* args );
static PyObject*  itkls2d_SetInputsMtd( pyLevelSet* self, PyObject* args );
static PyObject*  itkls2d_PhaseOneLevelSetMtd( pyLevelSet* self, PyObject* args );
static PyObject*  itkls2d_PhaseTwoLevelSetMtd( pyLevelSet* self, PyObject* args );
static PyObject*  itkls2d_GACLevelSetMtd( pyLevelSet* self, PyObject* args );
static PyObject*  itkls2d_WriteFrontMtd( pyLevelSet* self, PyObject* args  );

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

static int pyLevelSet_init(pyLevelSet* self, PyObject* args)
{
  fprintf(stdout,"pyLevelSet initialized.\n");
  return SV_OK;
}

PyMethodDef pyLevelSet_methods[] = {
	{"NewLevelSetObject", (PyCFunction)itkls2d_NewCmd, METH_VARARGS,NULL},
	{"DeleteLevelSetObject",(PyCFunction)Deleteitkls2d, METH_NOARGS,NULL },
    {"SetInputs", (PyCFunction)itkls2d_SetInputsMtd, METH_VARARGS,NULL},
    {"PhaseOneLevelSet", (PyCFunction)itkls2d_PhaseOneLevelSetMtd, METH_VARARGS,NULL},
    {"PhaseTwoLevelSet", (PyCFunction)itkls2d_PhaseTwoLevelSetMtd, METH_VARARGS,NULL},
//	{"WriteFront", (PyCFunction)itkls2d_WriteFrontMtd, METH_NOARGS,NULL},
    {"GACLevelSet", (PyCFunction)itkls2d_GACLevelSetMtd, METH_VARARGS,NULL},
    {NULL, NULL,0,NULL},
};

PyMethodDef Itkls2d_methods[] = {
    {NULL, NULL,0,NULL},
};

static PyTypeObject pyLevelSetType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  "pyLevelSet.pyLevelSet",             /* tp_name */
  sizeof(pyLevelSet),             /* tp_basicsize */
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
  "pyLevelSet objects",           /* tp_doc */
  0,                         /* tp_traverse */
  0,                         /* tp_clear */
  0,                         /* tp_richcompare */
  0,                         /* tp_weaklistoffset */
  0,                         /* tp_iter */
  0,                         /* tp_iternext */
  pyLevelSet_methods,             /* tp_methods */
  0,                         /* tp_members */
  0,                         /* tp_getset */
  0,                         /* tp_base */
  0,                         /* tp_dict */
  0,                         /* tp_descr_get */
  0,                         /* tp_descr_set */
  0,                         /* tp_dictoffset */
  (initproc)pyLevelSet_init,       /* tp_init */
  0,                         /* tp_alloc */
  0,                  /* tp_new */
};

#if PYTHON_MAJOR_VERSION == 3
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
    pyLevelSetType.tp_new=PyType_GenericNew;
	if (PyType_Ready(&pyLevelSetType)<0)
    {
      fprintf(stdout,"Error in pyLevelSetType\n");
    }
    
    PyObject *pyItkls2D;
#if PYTHON_MAJOR_VERSION == 2
    pyItkls2D = Py_InitModule("Itkls2d",Itkls2d_methods);
#elif PYTHON_MAJOR_VERSION == 3
    pyItkls2D = PyModule_Create(&Itkls2dmodule);
#endif
    PyRunTimeErr2d = PyErr_NewException("Itkls2d.error",NULL,NULL);
    PyModule_AddObject(pyItkls2D,"error",PyRunTimeErr2d);
    Py_INCREF(&pyLevelSetType);
    PyModule_AddObject(pyItkls2D,"pyLevelSet",(PyObject*)&pyLevelSetType);

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

// -------------
// itkls2d_NewCmd
// -------------

// itkls2d <objName> Or ITKLevelSet <objName>
static pyLevelSet* itkls2d_NewCmd( pyLevelSet* self, PyObject* args )
{

	CONST84 char *lsName;
	cvITKLevelSet *ls;
	Tcl_HashEntry *entryPtr;
	int newEntry = 0;
	if(!PyArg_ParseTuple(args, "s",&lsName))
	{
		PyErr_SetString(PyRunTimeErr2d,"Could not import 1 char, lsname");
		
	}

	// Make sure this is a new object name:
	if ( !NewName( lsName ) ) {
		PyErr_SetString(PyRunTimeErr2d, "ITKLevelSetCore object already exists");
		
	}

	// Allocate new cvLevelSet object:
	ls = new cvITKLevelSet;
	if ( ls == NULL ) {
		PyErr_SetString(PyRunTimeErr2d,"error allocating object");
		
	}

	strcpy( ls->tclName_, lsName );
	entryPtr = Tcl_CreateHashEntry( &gLsetCoreTable, lsName, &newEntry );
	if ( !newEntry ) {
		PyErr_SetString(PyRunTimeErr2d, "error updating cvLevelSet hash table");
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

PyObject* Deleteitkls2d(pyLevelSet* self, PyObject* args )
{

	cvITKLevelSet *ls = self->ls;
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

PyObject* itkls2d_SetInputsMtd( pyLevelSet* self, PyObject* args  )
{
	//`cvITKLevelSet *ls = (cvITKLevelSet *)clientData;
	cvITKLevelSet *ls=self->ls;
	char *inputImageName;
	char *seedPdName;

	if(!PyArg_ParseTuple(args, "ss",&inputImageName,&seedPdName))
	{
		PyErr_SetString(PyRunTimeErr2d,"Could not import 2 chars");
		
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
			
		}
		printf("Found Object\n");
		// Make sure image is of type STRUCTURED_PTS_T:
		typeImg1 = inputImage->GetType();
		if ( typeImg1 != STRUCTURED_PTS_T ) {
			char temp[2048];
			sprintf(temp,"error: object ", inputImageName, "not of type StructuredPts", (char *)NULL);
			PyErr_SetString(PyRunTimeErr2d, temp );
			
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
			
		}
		printf("Found Object\n");
		// Make sure image is of type STRUCTURED_PTS_T:
		typeImg2 = seedPolyData->GetType();
		if ( typeImg2 != POLY_DATA_T) {
			char temp[2048];
			sprintf(temp,"error: object ", seedPdName, "not of type PolyData", (char *)NULL);
			PyErr_SetString(PyRunTimeErr2d, temp );
			
		}
	}

	ls->SetInputImage((cvStrPts*)inputImage);
	ls->SetSeed((cvPolyData*)seedPolyData);


	return SV_PYTHON_OK;
}

/*
 *
 *
 */

static PyObject* itkls2d_PhaseOneLevelSetMtd( pyLevelSet* self, PyObject* args  )
{
	//cvITKLevelSet *ls = (cvITKLevelSet *)clientData;
	cvITKLevelSet *ls=self->ls ;
	double kc, expFactorRising,expFactorFalling, advScale;

	double sigmaFeat = -1, sigmaAdv = -1;


	if (!PyArg_ParseTuple(args,"ddd|dd",&kc,&expFactorRising,&expFactorFalling,
					&sigmaFeat,&sigmaAdv))
	{
		PyErr_SetString(PyRunTimeErr2d,"Could not import 5 doubles");
		
	}
	std::cout << "sigmaFeat " << sigmaFeat << std::endl;

	if(sigmaFeat >= 0)
		ls->SetSigmaFeature(sigmaFeat);
	if(sigmaAdv >= 0)
		ls->SetSigmaAdvection(sigmaAdv);

	ls->ComputePhaseOneLevelSet(kc, expFactorRising,expFactorFalling);

	return SV_PYTHON_OK;
}

static PyObject* itkls2d_PhaseTwoLevelSetMtd( pyLevelSet* self, PyObject* args  )
{
	cvITKLevelSet *ls=self->ls;

	double klow, kupp;
	double sigmaFeat = -1, sigmaAdv = -1;
	if (!PyArg_ParseTuple(args,"dd|dd",&klow,&kupp,
					&sigmaFeat,&sigmaAdv))
	{
		PyErr_SetString(PyRunTimeErr2d,"Could not import 4 doubles");
		
	}


	//std::cout << "Entering LS" << std::endl;

	if(sigmaFeat >= 0)
		ls->SetSigmaFeature(sigmaFeat);
	if(sigmaAdv >= 0)
		ls->SetSigmaAdvection(sigmaAdv);


	ls->ComputePhaseTwoLevelSet(kupp,klow);

	return SV_PYTHON_OK;
}
static PyObject* itkls2d_GACLevelSetMtd( pyLevelSet* self, PyObject* args  )
{
	cvITKLevelSet *ls=self->ls;
	char *usage;
	double sigma, expFactor;

	if (!PyArg_ParseTuple(args,"d|d",&expFactor,&sigma))
	{
		PyErr_SetString(PyRunTimeErr2d,"Could not import 2 doubles");
		
	}


	ls->ComputeGACLevelSet(expFactor);


	return SV_PYTHON_OK;
}

static PyObject* itkls2d_WriteFrontMtd(pyLevelSet* self, PyObject* args)
{
	cvITKLevelSet *ls=self->ls;
	ls->WriteFrontImages();
	return SV_PYTHON_OK;
}




