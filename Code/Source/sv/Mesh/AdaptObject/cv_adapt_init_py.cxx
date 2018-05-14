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

#include "SimVascular.h"

#include <stdio.h>
#include <string.h>
#include "cvRepository.h"
#include "cv_adapt_init_py.h"
#include "cvAdaptObject.h"
#include "cv_arg.h"
#include "cv_misc_utils.h"
#include "cv_vtk_utils.h"
#include "cvPolyData.h"
#include "cv_sys_geom.h"

#include "cvFactoryRegistrar.h"

#include "Python.h"
// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#include "cv_globals.h"

// Prototypes:
// -----------
typedef struct
{
  PyObject_HEAD
  cvAdaptObject* geom;
}pyAdaptObject;

static void pyAdaptObject_dealloc(pyAdaptObject* self)
{
  Py_XDECREF(self->geom);
  Py_TYPE(self)->tp_free((PyObject*)self);
}

pyAdaptObject* cvAdapt_NewObjectCmd( pyAdaptObject* self, PyObject* args);

// Adapt
// -----
PyObject* PyRunTimeErr;
PyObject* Adapt_RegistrarsListCmd(PyObject* self, PyObject* args);
PyMODINIT_FUNC initpyMeshAdapt();
static PyObject* cvAdapt_CreateInternalMeshObjectMtd( pyAdaptObject* self, PyObject* args);
static PyObject* cvAdapt_LoadModelMtd( pyAdaptObject* self, PyObject* args);
static PyObject* cvAdapt_LoadMeshMtd( pyAdaptObject* self, PyObject* args);
static PyObject* cvAdapt_LoadSolutionFromFileMtd( pyAdaptObject* self, PyObject* args);
static PyObject* cvAdapt_LoadYbarFromFileMtd( pyAdaptObject* self, PyObject* args);
static PyObject* cvAdapt_LoadAvgSpeedFromFileMtd( pyAdaptObject* self, PyObject* args);
static PyObject* cvAdapt_LoadHessianFromFileMtd( pyAdaptObject* self, PyObject* args);
static PyObject* cvAdapt_ReadSolutionFromMeshMtd( pyAdaptObject* self, PyObject* args);
static PyObject* cvAdapt_ReadYbarFromMeshMtd( pyAdaptObject* self, PyObject* args);
static PyObject* cvAdapt_ReadAvgSpeedFromMeshMtd( pyAdaptObject* self, PyObject* args);
static PyObject* cvAdapt_SetAdaptOptionsMtd( pyAdaptObject* self, PyObject* args);
static PyObject* cvAdapt_CheckOptionsMtd( pyAdaptObject* self, PyObject* args);
static PyObject* cvAdapt_SetMetricMtd( pyAdaptObject* self, PyObject* args);
static PyObject* cvAdapt_SetupMeshMtd( pyAdaptObject* self, PyObject* args);
static PyObject* cvAdapt_RunAdaptorMtd( pyAdaptObject* self, PyObject* args);
static PyObject* cvAdapt_PrintStatsMtd( pyAdaptObject* self, PyObject* args);
static PyObject* cvAdapt_GetAdaptedMeshMtd( pyAdaptObject* self, PyObject* args);
static PyObject* cvAdapt_TransferSolutionMtd( pyAdaptObject* self, PyObject* args);
static PyObject* cvAdapt_TransferRegionsMtd( pyAdaptObject* self, PyObject* args);
static PyObject* cvAdapt_WriteAdaptedModelMtd( pyAdaptObject* self, PyObject* args);
static PyObject* cvAdapt_WriteAdaptedMeshMtd( pyAdaptObject* self, PyObject* args);
static PyObject* cvAdapt_WriteAdaptedSolutionMtd( pyAdaptObject* self, PyObject* args);

// Helper functions
// ----------------

static void AdaptPrintMethods();

void DeleteAdapt( pyAdaptObject* self );


// ----------
// Adapt_Init
// ----------
int Adapt_pyInit()
{
  initpyMeshAdapt();
  return Py_OK;
}
static int pyAdaptObject_init(pyAdaptObject* self, PyObject* args)
{
  fprintf(stdout,"pyAdaptObject initialized.\n");
  return Py_OK;
}

static PyMethodDef pyAdaptObject_methods[]={
  {"adapt_newObject",(PyCFunction)cvAdapt_NewObjectCmd,METH_VARARGS,NULL},
  { "CreateInternalMeshObject",(PyCFunction)cvAdapt_CreateInternalMeshObjectMtd, METH_VARARGS,NULL},
  { "LoadModel", (PyCFunction)cvAdapt_LoadModelMtd,METH_VARARGS,NULL},
  { "LoadMesh",(PyCFunction)cvAdapt_LoadMeshMtd,METH_VARARGS,NULL},
  { "LoadSolutionFromFile",
    (PyCFunction)cvAdapt_LoadSolutionFromFileMtd,METH_VARARGS,NULL},
  { "LoadYbarFromFile",
    (PyCFunction)cvAdapt_LoadYbarFromFileMtd,METH_VARARGS,NULL},
  { "LoadAvgSpeedFromFile",
    (PyCFunction)cvAdapt_LoadAvgSpeedFromFileMtd,METH_VARARGS,NULL},
  { "LoadHessianFromFile",
    (PyCFunction)cvAdapt_LoadHessianFromFileMtd,METH_VARARGS,NULL},
  { "ReadSolutionFromMesh",
    (PyCFunction)cvAdapt_ReadSolutionFromMeshMtd,METH_VARARGS,NULL},
  { "ReadYbarFromMesh",
    (PyCFunction)cvAdapt_ReadYbarFromMeshMtd,METH_VARARGS,NULL},
  { "ReadAvgSpeedFromMesh",
    (PyCFunction)cvAdapt_ReadAvgSpeedFromMeshMtd,METH_VARARGS,NULL},
  { "SetAdaptOptions",
    (PyCFunction)cvAdapt_SetAdaptOptionsMtd,METH_VARARGS,NULL},
  { "CheckOptions",
    (PyCFunction)cvAdapt_CheckOptionsMtd,METH_VARARGS,NULL},
  { "SetMetric",
    (PyCFunction)cvAdapt_SetMetricMtd,METH_VARARGS,NULL},
  { "SetupMesh",
    (PyCFunction)cvAdapt_SetupMeshMtd,METH_VARARGS,NULL},
  { "RunAdaptor",
    (PyCFunction)cvAdapt_RunAdaptorMtd,METH_VARARGS,NULL},
  { "PrintStats",
    (PyCFunction)cvAdapt_PrintStatsMtd,METH_VARARGS,NULL},
  { "GetAdaptedMesh",
    (PyCFunction)cvAdapt_GetAdaptedMeshMtd,METH_VARARGS,NULL},
  { "TransferSolution",
    (PyCFunction)cvAdapt_TransferSolutionMtd,METH_VARARGS,NULL},
  { "TransferRegions",
    (PyCFunction)cvAdapt_TransferRegionsMtd,METH_VARARGS,NULL},
  { "WriteAdaptedModel",
    (PyCFunction)cvAdapt_WriteAdaptedModelMtd,METH_VARARGS,NULL},
  { "WriteAdaptedMesh",
    (PyCFunction)cvAdapt_WriteAdaptedMeshMtd,METH_VARARGS,NULL},
  { "WriteAdaptedSolution",
    (PyCFunction)cvAdapt_WriteAdaptedSolutionMtd,METH_VARARGS,NULL},
 {NULL}
};

static PyTypeObject pyAdaptObjectType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  "pyMeshAdapt.pyAdaptObject",             /* tp_name */
  sizeof(pyAdaptObject),             /* tp_basicsize */
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
  "pyAdaptObject  objects",           /* tp_doc */
  0,                         /* tp_traverse */
  0,                         /* tp_clear */
  0,                         /* tp_richcompare */
  0,                         /* tp_weaklistoffset */
  0,                         /* tp_iter */
  0,                         /* tp_iternext */
  pyAdaptObject_methods,             /* tp_methods */
  0,                         /* tp_members */
  0,                         /* tp_getset */
  0,                         /* tp_base */
  0,                         /* tp_dict */
  0,                         /* tp_descr_get */
  0,                         /* tp_descr_set */
  0,                         /* tp_dictoffset */
  (initproc)pyAdaptObject_init,                            /* tp_init */
  0,                         /* tp_alloc */
  0,                  /* tp_new */
};
static PyMethodDef pyAdaptMesh_methods[] = {
  {"adapt_registrars",Adapt_RegistrarsListCmd,METH_NOARGS,NULL},
  {NULL, NULL}
};


PyMODINIT_FUNC
initpyMeshAdapt()
{

  // Associate the adapt object registrar with the python interpreter
  if (gRepository==NULL)
  {
    gRepository= new cvRepository();
    fprintf(stdout,"New gRepository created from cv_adapt_init\n");
  }
  fprintf(stdout,"check\n");
  if (PySys_SetObject("AdaptObjectRegistrar",(PyObject*)&cvAdaptObject::gRegistrar)<0)
  {
    fprintf(stdout,"Unable to create AdaptObjectRegistrar\n");
    return;
  }
  fprintf(stdout,"check\n");

  // Initialize
  cvAdaptObject::gCurrentKernel = KERNEL_INVALID;

#ifdef USE_TETGEN_ADAPTOR
  cvAdaptObject::gCurrentKernel = KERNEL_TETGEN;
#endif
  fprintf(stdout,"check\n");

  pyAdaptObjectType.tp_new=PyType_GenericNew;
  if (PyType_Ready(&pyAdaptObjectType)<0)
  {
    fprintf(stdout,"Error in pyAdaptMeshType\n");
    return;
  }
  fprintf(stdout,"check\n");

  PyObject* pythonC;
  pythonC = Py_InitModule("pyMeshAdapt",pyAdaptMesh_methods);
  fprintf(stdout,"check\n");
  if(pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyMeshAdapt\n");
    return;
  }
  fprintf(stdout,"check\n");

  PyRunTimeErr = PyErr_NewException("pyMeshAdapt.error",NULL,NULL);
  PyModule_AddObject(pythonC,"error",PyRunTimeErr);
  Py_INCREF(&pyAdaptObjectType);
  PyModule_AddObject(pythonC,"pyAdaptObject",(PyObject*)&pyAdaptObjectType);
  fprintf(stdout,"check\n");
  return ;

 }
// This routine is used for debugging the registrar/factory system.
PyObject* Adapt_RegistrarsListCmd( PyObject* self, PyObject* args)
{
  cvFactoryRegistrar *adaptObjectRegistrar =
    (cvFactoryRegistrar *) PySys_GetObject( "AdaptObjectRegistrar");

  char result[255];
  sprintf( result, "Adapt object registrar ptr -> %p\n", adaptObjectRegistrar );
  PyObject* pyList=PyList_New(6);
  PyList_SetItem(pyList,0,PyString_FromFormat(result));
  for (int i = 0; i < 5; i++) {
    sprintf( result, "GetFactoryMethodPtr(%i) = %p\n",
      i, (adaptObjectRegistrar->GetFactoryMethodPtr(i)));
    PyList_SetItem(pyList,i+1,PyString_FromFormat(result));
  }

  return pyList;
}

// Now, since we're using the cvRepository mechanism (which is itself a
// Tcl_HashTable), we can use the cvRepository's lookup mechanisms to
// find those operands.  That is, we can call cvRepository's
// GetObject(name) method to get back object pointers for use inside
// Tcl object method functions.

pyAdaptObject* cvAdapt_NewObjectCmd( pyAdaptObject* self, PyObject* args)
{
  char *resultName = NULL;

  char *kernelName;
  if(!(PyArg_ParseTuple(args,"s",&resultName)))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char, resultname.");
  }

  // Do work of command:

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( resultName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    return Py_ERROR;
  }

  KernelType meshType = KERNEL_INVALID;
  kernelName = cvMeshSystem::GetCurrentKernelName();
  // Instantiate the new mesh:
  cvAdaptObject *adaptor = NULL;
  if (!strcmp(kernelName,"TetGen")) {
    meshType = KERNEL_TETGEN;
    cvAdaptObject::gCurrentKernel = KERNEL_TETGEN;
  } else if (!strcmp(kernelName,"MeshSim")) {
    meshType = KERNEL_MESHSIM;
    cvAdaptObject::gCurrentKernel = KERNEL_MESHSIM;
  } else {
    PyErr_SetString(PyRunTimeErr, "invalid kernel name");
    return Py_ERROR;
  }
  fprintf(stdout, kernelName );
  adaptor = cvAdaptObject::DefaultInstantiateAdaptObject(meshType);

  if ( adaptor == NULL ) {
    return Py_ERROR;
  }

  // Register the solid:
  if ( !( gRepository->Register( resultName, adaptor ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository" );
    delete adaptor;
    return Py_ERROR;
  }

  Py_INCREF(adaptor);
  self->geom=adaptor;
  Py_DECREF(adaptor);
  return self;
}

// -------------
// DeleteAdapt
// -------------
// This is the deletion call-back for cvAdaptObject object commands.

void DeleteAdapt( pyAdaptObject* self ) {
    cvAdaptObject *geom = self->geom;

    gRepository->UnRegister( geom->GetName() );
}

// ------------
// AdaptPrintMethods
// ------------

static void AdaptPrintMethods()
{

  PySys_WriteStdout( "CreateInternalMeshObject\n");
  PySys_WriteStdout( "LoadModel\n");
  PySys_WriteStdout( "LoadMesh\n");
  PySys_WriteStdout( "LoadSolutionFromFile\n");
  PySys_WriteStdout( "LoadYbarFromFile\n");
  PySys_WriteStdout( "LoadAvgSpeedFromFile\n");
  PySys_WriteStdout( "LoadHessianFromFile\n");
  PySys_WriteStdout( "ReadSolutionFromMesh\n");
  PySys_WriteStdout( "ReadYbarFromMesh\n");
  PySys_WriteStdout( "ReadAvgSpeedFromMesh\n");
  PySys_WriteStdout( "SetAdaptOptions\n");
  PySys_WriteStdout( "CheckOptions\n");
  PySys_WriteStdout( "SetMetric\n");
  PySys_WriteStdout( "SetupMesh\n");
  PySys_WriteStdout( "RunAdaptor\n");
  PySys_WriteStdout( "PrintStats\n");
  PySys_WriteStdout( "TransferSolution\n");
  PySys_WriteStdout( "TransferRegions\n");
  PySys_WriteStdout( "WriteAdaptedModel\n");
  PySys_WriteStdout( "WriteAdaptedMesh\n");
  PySys_WriteStdout( "WriteAdaptedSolution\n");

  return;
}

// ----------------
// cvAdapt_CreateInternalMeshObjectMtd
// ----------------
static PyObject* cvAdapt_CreateInternalMeshObjectMtd( pyAdaptObject* self, PyObject* args)
{
  char *meshFileName = NULL;
  char *solidFileName = NULL;

  if(!(PyArg_ParseTuple(args,"ss",&meshFileName,&solidFileName)))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import two chars.");
  }
  // Do work of command:

  cvAdaptObject *geom =self->geom;
  if ( geom == NULL ) {
    fprintf(stderr,"Adapt object should already be created! It is NULL\n");
    return Py_ERROR;
  }
  if (geom->CreateInternalMeshObject(meshFileName,solidFileName) != SV_OK)
  {
    fprintf(stderr,"Error in creation of internal mesh\n");
    return Py_ERROR;
  }

  return Py_BuildValue("s","success");
}

// ----------------
// cvAdapt_LoadModelMtd
// ----------------
static PyObject* cvAdapt_LoadModelMtd( pyAdaptObject* self, PyObject* args)
{
  char *solidFileName = NULL;

  if(!(PyArg_ParseTuple(args,"s",&solidFileName)))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char, soildFileName.");
  }


  // Do work of command:
  cvAdaptObject *geom = self->geom;
  if ( geom == NULL ) {
    fprintf(stderr,"Adapt object should already be created! It is NULL\n");
    return Py_ERROR;
  }
  if (geom->LoadModel(solidFileName) != SV_OK)
  {
    fprintf(stderr,"Error in loading of model\n");
    return Py_ERROR;
  }//, meshFileName, solidFileName );

  return Py_BuildValue("s","success");
}

// ----------------
// cvAdapt_LoadMeshMtd
// ----------------
static PyObject* cvAdapt_LoadMeshMtd( pyAdaptObject* self, PyObject* args)
{
  char *meshFileName = NULL;
  if(!(PyArg_ParseTuple(args,"s",&meshFileName)))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char, meshFileName");
  }


  // Do work of command:

  cvAdaptObject *geom =self->geom;
  if ( geom == NULL ) {
    fprintf(stderr,"Adapt object should already be created! It is NULL\n");
    return Py_ERROR;
  }
  if (geom->LoadMesh(meshFileName) != SV_OK)
  {
    fprintf(stderr,"Error in loading of mesh\n");
    return Py_ERROR;
  }

  return Py_BuildValue("s","success");
}

// ----------------
// cvAdapt_LoadSolutionFromFileMtd
// ----------------
static PyObject* cvAdapt_LoadSolutionFromFileMtd( pyAdaptObject* self, PyObject* args)
{
  char *fileName = NULL;


  if(!(PyArg_ParseTuple(args,"s",&fileName)))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char, filename.");
  }

  // Do work of command:

  cvAdaptObject *geom = self->geom;
  if ( geom == NULL ) {
    fprintf(stderr,"Adapt object should already be created! It is NULL\n");
    return Py_ERROR;
  }
  if (geom->LoadSolutionFromFile(fileName) != SV_OK)
  {
    fprintf(stderr,"Error in loading of solution\n");
    return Py_ERROR;
  }

  return Py_BuildValue("s","success");
}

// ----------------
// cvAdapt_LoadYbarFromFileMtd
// ----------------
static PyObject* cvAdapt_LoadYbarFromFileMtd( pyAdaptObject* self, PyObject* args)
{
  char *fileName = NULL;

  if(!(PyArg_ParseTuple(args,"s",&fileName)))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char, fileName.");
  }

  // Do work of command:

  cvAdaptObject *geom = self->geom;
  if ( geom == NULL ) {
    fprintf(stderr,"Adapt object should already be created! It is NULL\n");
    return Py_ERROR;
  }
  if (geom->LoadYbarFromFile(fileName) != SV_OK)
  {
    fprintf(stderr,"Error in loading of average speed\n");
    return Py_ERROR;
  }

  return Py_BuildValue("s","success");
}

// ----------------
// cvAdapt_LoadAvgSpeedFromFileMtd
// ----------------
static PyObject* cvAdapt_LoadAvgSpeedFromFileMtd( pyAdaptObject* self, PyObject* args)
{
  char *fileName = NULL;
  if(!(PyArg_ParseTuple(args,"s",&fileName)))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char, fileName.");
  }

  // Do work of command:

  cvAdaptObject *geom = self->geom;
  if ( geom == NULL ) {
    fprintf(stderr,"Adapt object should already be created! It is NULL\n");
    return Py_ERROR;
  }
  if (geom->LoadAvgSpeedFromFile(fileName) != SV_OK)
  {
    fprintf(stderr,"Error in loading of average speed\n");
    return Py_ERROR;
  }

  return Py_BuildValue("s","success");
}

// ----------------
// cvAdapt_LoadHessianFromFileMtd
// ----------------
static PyObject* cvAdapt_LoadHessianFromFileMtd( pyAdaptObject* self, PyObject* args)
{
  char *fileName = NULL;

  if(!(PyArg_ParseTuple(args,"s",&fileName)))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char, fileName.");
  }

  // Do work of command:

  cvAdaptObject *geom = self->geom;
  if ( geom == NULL ) {
    fprintf(stderr,"Adapt object should already be created! It is NULL\n");
    return Py_ERROR;
  }
  if (geom->LoadHessianFromFile(fileName) != SV_OK)
  {
    fprintf(stderr,"Error in loading of hessian\n");
    return Py_ERROR;
  }

  return Py_BuildValue("s","success");
}

// ----------------
// cvAdapt_ReadSolutionFromMeshMtd
// ----------------
static PyObject* cvAdapt_ReadSolutionFromMeshMtd( pyAdaptObject* self, PyObject* args)
{
  cvAdaptObject *geom = self->geom;

  if (geom->ReadSolutionFromMesh() == SV_OK) {
    return Py_BuildValue("s","success");
  } else {
    return Py_ERROR;
  }
  return Py_BuildValue("s","success");
}

// ----------------
// cvAdapt_ReadYbarFromMeshMtd
// ----------------
static PyObject* cvAdapt_ReadYbarFromMeshMtd( pyAdaptObject* self, PyObject* args)
{
  cvAdaptObject *geom = self->geom;

  if (geom->ReadYbarFromMesh() == SV_OK) {
    return Py_BuildValue("s","success");
  } else {
    return Py_ERROR;
  }
  return Py_BuildValue("s","success");
}

// ----------------
// cvAdapt_ReadAvgSpeedFromMeshMtd
// ----------------
static PyObject* cvAdapt_ReadAvgSpeedFromMeshMtd( pyAdaptObject* self, PyObject* args)
{
  cvAdaptObject *geom = self->geom;

  if (geom->ReadAvgSpeedFromMesh() == SV_OK) {
    return Py_BuildValue("s","success");
  } else {
    return Py_ERROR;
  }
  return Py_BuildValue("s","success");
}

// ----------------
// cvAdapt_SetAdaptOptionsMtd
// ----------------
static PyObject* cvAdapt_SetAdaptOptionsMtd( pyAdaptObject* self, PyObject* args)
{
  double value=0;
  char *flag = NULL;

  if(!(PyArg_ParseTuple(args,"sd",&flag,&value)))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char and one double, fileName, value");
  }

  // Do work of command:

  cvAdaptObject *geom = self->geom;
  if ( geom == NULL ) {
    fprintf(stderr,"Adapt object should already be created! It is NULL\n");
    return Py_ERROR;
  }
  if (geom->SetAdaptOptions(flag,value) != SV_OK)
  {
    fprintf(stderr,"Error in options setting\n");
    fprintf(stderr,"%s is not a valid option flag\n",flag);
    return Py_ERROR;
  }

  return Py_BuildValue("s","success");
}

// ----------------
// cvAdapt_CheckOptionsMtd
// ----------------
static PyObject* cvAdapt_CheckOptionsMtd( pyAdaptObject* self, PyObject* args)
{
  cvAdaptObject *geom = self->geom;

  if (geom->CheckOptions() == SV_OK) {
    return Py_BuildValue("s","success");
  } else {
    return Py_ERROR;
  }

  return Py_BuildValue("s","success");
}

// ----------------
// cvAdapt_SetMetricMtd
// ----------------
static PyObject* cvAdapt_SetMetricMtd( pyAdaptObject* self, PyObject* args)
{
  char *fileName = NULL;
  int option = -1;
  int strategy = -1;

  if(!(PyArg_ParseTuple(args,"s|ii",&fileName, &option,&strategy)))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char, fileName or two optional int, option stategy");
  }

  cvAdaptObject *geom = self->geom;

  if (geom->SetMetric(fileName,option,strategy) == SV_OK) {
    return Py_BuildValue("s","success");
  } else {
    return Py_ERROR;
  }

  return Py_BuildValue("s","success");
}

// ----------------
// cvAdapt_SetupMeshMtd
// ----------------
static PyObject* cvAdapt_SetupMeshMtd( pyAdaptObject* self, PyObject* args)
{
  cvAdaptObject *geom = self->geom;

  if (geom->SetupMesh() == SV_OK) {
    return Py_BuildValue("s","success");
  } else {
    return Py_ERROR;
  }

  return Py_BuildValue("s","success");
}

// ----------------
// cvAdapt_RunAdaptorMtd
// ----------------
static PyObject* cvAdapt_RunAdaptorMtd( pyAdaptObject* self, PyObject* args)
{
  cvAdaptObject *geom = self->geom;

  if (geom->RunAdaptor() == SV_OK) {
    return Py_BuildValue("s","success");
  } else {
    return Py_ERROR;
  }

  return Py_BuildValue("s","success");
}

// ----------------
// cvAdapt_PrintStatsMtd
// ----------------
static PyObject* cvAdapt_PrintStatsMtd( pyAdaptObject* self, PyObject* args)
{
  cvAdaptObject *geom = self->geom;

  if (geom->PrintStats() == SV_OK) {
    return Py_BuildValue("s","success");
  } else {
    return Py_ERROR;
  }
}

// ----------------
// cvAdapt_GetAdaptedMeshMtd
// ----------------
PyObject* cvAdapt_GetAdaptedMeshMtd( pyAdaptObject* self, PyObject* args)
{
  cvAdaptObject *geom =self->geom;
  if (geom->GetAdaptedMesh() == SV_OK) {
    return Py_BuildValue("s","success");
  } else {
    return Py_ERROR;
  }
}

// ----------------
// cvAdapt_TransferSolutionMtd
// ----------------
PyObject* cvAdapt_TransferSolutionMtd( pyAdaptObject* self, PyObject* args)
{
  cvAdaptObject *geom = self->geom;

  if (geom->TransferSolution() == SV_OK) {
    return Py_BuildValue("s","success");
  } else {
    return Py_ERROR;
  }
}

// ----------------
// cvAdapt_TransferRegionsMtd
// ----------------
PyObject* cvAdapt_TransferRegionsMtd( pyAdaptObject* self, PyObject* args)
{
  cvAdaptObject *geom = self->geom;

  if (geom->TransferRegions() == SV_OK) {
    return Py_BuildValue("s","success");
  } else {
    return Py_ERROR;
  }
}

// ----------------
// cvAdapt_WriteAdaptedModelMtd
// ----------------
static PyObject* cvAdapt_WriteAdaptedModelMtd( pyAdaptObject* self, PyObject* args)
{
  char *fileName = NULL;


  if(!(PyArg_ParseTuple(args,"s",&fileName)))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char, fileName.");
  }
  // Do work of command:

  cvAdaptObject *geom = self->geom;
  if ( geom == NULL ) {
    fprintf(stderr,"Adapt object should already be created! It is NULL\n");
    return Py_ERROR;
  }
  if (geom->WriteAdaptedModel(fileName) != SV_OK)
  {
    fprintf(stderr,"Error in writing of model\n");
    return Py_ERROR;
  }

  return Py_BuildValue("s","success");
}

// ----------------
// cvAdapt_WriteAdaptedMeshMtd
// ----------------
static PyObject* cvAdapt_WriteAdaptedMeshMtd( pyAdaptObject* self, PyObject* args)
{
  char *fileName = NULL;

  if(!(PyArg_ParseTuple(args,"s",&fileName)))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char, fileName.");
  }
  // Do work of command:

  cvAdaptObject *geom = self->geom;
  if ( geom == NULL ) {
    fprintf(stderr,"Adapt object should already be created! It is NULL\n");
    return Py_ERROR;
  }
  if (geom->WriteAdaptedMesh(fileName) != SV_OK)
  {
    fprintf(stderr,"Error in writing of mesh\n");
    return Py_ERROR;
  }

  return Py_BuildValue("s","success");
}

// ----------------
// cvAdapt_WriteAdaptedSolutionMtd
// ----------------
static PyObject* cvAdapt_WriteAdaptedSolutionMtd( pyAdaptObject* self, PyObject* args)
{
  char *fileName = NULL;

  if(!(PyArg_ParseTuple(args,"s",&fileName)))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char, fileName.");
  }
  // Do work of command:

  cvAdaptObject *geom = self->geom;
  if ( geom == NULL ) {
    fprintf(stderr,"Adapt object should already be created! It is NULL\n");
    return Py_ERROR;
  }
  if (geom->WriteAdaptedSolution(fileName) != SV_OK)
  {
    fprintf(stderr,"Error in writing of solution\n");
    return Py_ERROR;
  }

  return Py_BuildValue("s","success");
}

