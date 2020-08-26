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
#include "SimVascular_python.h"
#include "sv_MeshSystem.h"
#include "sv_MeshObject.h"
#include "sv_mesh_init_py.h"

#include <stdio.h>
#include <string.h>
#include "sv_Repository.h"
#include "sv_RepositoryData.h"
#include "sv_PolyData.h"
#include "sv_arg.h"
#include "sv_VTK.h"
#include "sv_misc_utils.h"
#include "Python.h"
// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif
#include <iostream>
// Globals:
// --------

#include "sv2_globals.h"

// Prototypes:
// -----------

static void pyMeshObject_dealloc(pyMeshObject* self)

{

  Py_XDECREF(self->geom);
  Py_TYPE(self)->tp_free((PyObject*)self);

}
#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC initpyMeshObject();
#elif PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC PyInit_pyMeshObject();
#endif
PyObject* PyRunTimeErr;
PyObject*  cvMesh_SetMeshKernelCmd( PyObject* self, PyObject* args);

PyObject* cvMesh_NewObjectCmd( pyMeshObject* self, PyObject* args);
PyObject* cvMesh_GetObjectCmd( pyMeshObject* self, PyObject* args);
PyObject* cvMesh_ListMethodsCmd( PyObject* self, PyObject* args);
PyObject* cvMesh_LogonCmd( PyObject* self, PyObject* args);
PyObject* cvMesh_LogoffCmd( PyObject* self, PyObject* args);


// Mesh methods
// --------------------
static PyObject* cvMesh_GetKernelMtd( pyMeshObject* self, PyObject* args);
static PyObject* cvMesh_PrintMtd( pyMeshObject* self, PyObject* args);
static PyObject* cvMesh_UpdateMtd( pyMeshObject* self, PyObject* args);
static PyObject* cvMesh_WriteMetisAdjacencyMtd( pyMeshObject* self, PyObject* args);
static PyObject* cvMesh_GetPolyDataMtd( pyMeshObject* self, PyObject* args);
static PyObject* cvMesh_GetSolidMtd( pyMeshObject* self, PyObject* args);
static PyObject* cvMesh_SetVtkPolyDataMtd( pyMeshObject* self, PyObject* args);
static PyObject* cvMesh_GetUnstructuredGridMtd( pyMeshObject* self, PyObject* args);
static PyObject* cvMesh_GetFacePolyDataMtd( pyMeshObject* self, PyObject* args);
static PyObject* cvMesh_GetModelFaceInfoMtd( pyMeshObject* self, PyObject* args);
static PyObject* cvMesh_GetBoundaryFacesMtd( pyMeshObject* self, PyObject* args);
/*
#ifdef SV_USE_MESHSIM_DISCRETE_MODEL
static PyObject* cvMesh_LoadDiscreteModelMtd( pyMeshObject* self, PyObject* args);
#endif
*/

static PyObject* cvMesh_LoadModelMtd( pyMeshObject* self, PyObject* args);
static PyObject* cvMesh_LoadMeshMtd( pyMeshObject* self, PyObject* args);
static PyObject* cvMesh_NewMeshMtd( pyMeshObject* self, PyObject* args);
static PyObject* cvMesh_GenerateMeshMtd( pyMeshObject* self, PyObject* args);
static PyObject* cvMesh_WriteMeshMtd( pyMeshObject* self, PyObject* args);
static PyObject* cvMesh_WriteStatsMtd( pyMeshObject* self, PyObject* args);
static PyObject* cvMesh_SetMeshOptionsMtd( pyMeshObject* self, PyObject* args);
static PyObject* cvMesh_SetCylinderRefinementMtd( pyMeshObject* self, PyObject* args);
static PyObject* cvMesh_SetSphereRefinementMtd( pyMeshObject* self, PyObject* args);
static PyObject* cvMesh_SetBoundaryLayerMtd( pyMeshObject* self, PyObject* args);
static PyObject* cvMesh_SetWallsMtd( pyMeshObject* self, PyObject* args);
static PyObject* cvMesh_SetSolidKernelMtd( pyMeshObject* self, PyObject* args);
static PyObject* cvMesh_SetSizeFunctionBasedMeshMtd( pyMeshObject* self, PyObject* args);
static PyObject* cvMesh_AdaptMtd( pyMeshObject* self, PyObject* args);


// Helper functions
// ----------------

static void MeshPrintMethods();


// ----------
// cvMesh_Init
// ----------

int Mesh_pyInit()
{
#if PYTHON_MAJOR_VERSION == 2
  initpyMeshObject();
#elif PYTHON_MAJOR_VERSION == 3
  PyInit_pyMeshObject();
#endif
  return SV_OK;
}
static int pyMeshObject_init(pyMeshObject* self, PyObject* args)
{
  fprintf(stdout,"pyMeshObject initialized.\n");
  return SV_OK;
}

//static PyMemberDef pyMeshObject_members[]={
//{NULL}
//};
static PyMethodDef pyMeshObject_methods[]={
  {"NewObject", (PyCFunction)cvMesh_NewObjectCmd,METH_VARARGS,NULL},
  {"GetMesh", (PyCFunction)cvMesh_GetObjectCmd, METH_VARARGS, NULL},
  { "LoadModel", (PyCFunction)cvMesh_LoadModelMtd,METH_VARARGS,NULL},
  { "GetBoundaryFaces",(PyCFunction)cvMesh_GetBoundaryFacesMtd,METH_VARARGS,NULL},
  { "LoadMesh", (PyCFunction)cvMesh_LoadMeshMtd,METH_VARARGS,NULL},
  { "NewMesh", (PyCFunction)cvMesh_NewMeshMtd,METH_VARARGS,NULL},
  { "SetMeshOptions", (PyCFunction)cvMesh_SetMeshOptionsMtd,METH_VARARGS,NULL},
  { "SetCylinderRefinement", (PyCFunction)cvMesh_SetCylinderRefinementMtd,METH_VARARGS,NULL},
  { "SetSphereRefinement",(PyCFunction)cvMesh_SetSphereRefinementMtd,METH_VARARGS,NULL},
  { "SetSizeFunctionBasedMesh", (PyCFunction)cvMesh_SetSizeFunctionBasedMeshMtd,METH_VARARGS,NULL},
  { "GenerateMesh", (PyCFunction)cvMesh_GenerateMeshMtd,METH_VARARGS,NULL},
  { "SetBoundaryLayer", (PyCFunction)cvMesh_SetBoundaryLayerMtd,METH_VARARGS,NULL},
  { "SetWalls", (PyCFunction)cvMesh_SetWallsMtd,METH_VARARGS,NULL},
  { "SetSolidKernel", (PyCFunction)cvMesh_SetSolidKernelMtd,METH_VARARGS,NULL},
  { "GetModelFaceInfo",(PyCFunction)cvMesh_GetModelFaceInfoMtd,METH_VARARGS,NULL},
  // The method "Update" must be called before any of the other
  // methods since it loads the mesh.  To avoid confusion, we
  // call this method directly prior to any other.
 // { "Update" ) ) {
    // ignore this call now, it is done implicitly (see above)
    //if ( (PyCFunction)cvMesh_UpdateMtd,METH_VARARGS,NULL},
  { "Print",(PyCFunction)cvMesh_PrintMtd,METH_VARARGS,NULL},
  { "GetKernel", (PyCFunction)cvMesh_GetKernelMtd,METH_VARARGS,NULL},
  { "WriteMetisAdjacency", (PyCFunction)cvMesh_WriteMetisAdjacencyMtd,METH_VARARGS,NULL},
  { "GetPolyData", (PyCFunction)cvMesh_GetPolyDataMtd,METH_VARARGS,NULL},
  { "GetSolid", (PyCFunction)cvMesh_GetSolidMtd,METH_VARARGS,NULL},
  { "SetVtkPolyData",(PyCFunction)cvMesh_SetVtkPolyDataMtd,METH_VARARGS,NULL},
  { "GetUnstructuredGrid", (PyCFunction)cvMesh_GetUnstructuredGridMtd,METH_VARARGS,NULL},
  { "GetFacePolyData", (PyCFunction)cvMesh_GetFacePolyDataMtd,METH_VARARGS,NULL},
  { "WriteMesh", (PyCFunction)cvMesh_WriteMeshMtd,METH_VARARGS,NULL},
  { "WriteStats",(PyCFunction)cvMesh_WriteStatsMtd,METH_VARARGS,NULL},
  { "Adapt",  (PyCFunction)cvMesh_AdaptMtd,METH_VARARGS,NULL},
  {NULL,NULL}
};

static PyTypeObject pyMeshObjectType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  "pyMeshObject.pyMeshObject",             /* tp_name */
  sizeof(pyMeshObject),             /* tp_basicsize */
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
  "pyMeshObject  objects",           /* tp_doc */
  0,                         /* tp_traverse */
  0,                         /* tp_clear */
  0,                         /* tp_richcompare */
  0,                         /* tp_weaklistoffset */
  0,                         /* tp_iter */
  0,                         /* tp_iternext */
  pyMeshObject_methods,             /* tp_methods */
  0,                         /* tp_members */
  0,                         /* tp_getset */
  0,                         /* tp_base */
  0,                         /* tp_dict */
  0,                         /* tp_descr_get */
  0,                         /* tp_descr_set */
  0,                         /* tp_dictoffset */
  (initproc)pyMeshObject_init,                            /* tp_init */
  0,                         /* tp_alloc */
  0,                  /* tp_new */
};
static PyMethodDef pyMeshObjectModule_methods[] =
{
  //{"mesh_newObject", (PyCFunction)cvMesh_NewObjectCmd,METH_VARARGS,NULL},
  {"ListMethods",(PyCFunction)cvMesh_ListMethodsCmd,METH_NOARGS,NULL},
  {"SetKernel", (PyCFunction)cvMesh_SetMeshKernelCmd,METH_VARARGS,NULL},
  {"Logon", (PyCFunction)cvMesh_LogonCmd,METH_VARARGS,NULL},
  {"Logoff", (PyCFunction)cvMesh_LogoffCmd,METH_NOARGS,NULL},
  {NULL, NULL}
};

#if PYTHON_MAJOR_VERSION == 3
static struct PyModuleDef pyMeshObjectmodule = {
   PyModuleDef_HEAD_INIT,
   "pyMeshObject",   /* name of module */
   "", /* module documentation, may be NULL */
   -1,       /* size of per-interpreter state of the module,
                or -1 if the module keeps state in global variables. */
   pyMeshObjectModule_methods
};
#endif
//----------------
//initpyMeshObject
//----------------
#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC initpyMeshObject()

{
  // Associate the mesh registrar with the python interpreter so it can be
  // retrieved by the DLLs.
  if (gRepository==NULL)
  {
    gRepository = new cvRepository();
    fprintf(stdout,"New gRepository created from cv_mesh_init\n");
  }
  int (*kernel)(cvMeshObject::KernelType, cvMeshSystem*)=(&cvMeshSystem::RegisterKernel);
  if (Py_BuildValue("i",kernel)==nullptr)
  {
    fprintf(stdout,"Unable to create MeshSystemRegistrar\n");
    return;

  }
  if(PySys_SetObject("MeshSystemRegistrar",Py_BuildValue("i",kernel))<0)
  {
    fprintf(stdout, "Unable to register MeshSystemRegistrar\n");
    return;

  }
  // Initialize
  cvMeshSystem::SetCurrentKernel( cvMeshObject::KERNEL_INVALID );

  pyMeshObjectType.tp_new=PyType_GenericNew;
  if (PyType_Ready(&pyMeshObjectType)<0)
  {
    fprintf(stdout,"Error in pyMeshObjectType\n");
    return;

  }
  PyObject* pythonC;
  pythonC = Py_InitModule("pyMeshObject",pyMeshObjectModule_methods);

  if(pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyMeshObject\n");
    return;

  }
  PyRunTimeErr = PyErr_NewException("pyMeshObject.error",NULL,NULL);
  PyModule_AddObject(pythonC,"error",PyRunTimeErr);
  Py_INCREF(&pyMeshObjectType);
  PyModule_AddObject(pythonC,"pyMeshObject",(PyObject*)&pyMeshObjectType);
  return;

}
#endif

#if PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC PyInit_pyMeshObject()

{
  // Associate the mesh registrar with the python interpreter so it can be
  // retrieved by the DLLs.
  if (gRepository==NULL)
  {
    gRepository = new cvRepository();
    fprintf(stdout,"New gRepository created from cv_mesh_init\n");
  }
  int (*kernel)(cvMeshObject::KernelType, cvMeshSystem*)=(&cvMeshSystem::RegisterKernel);
  if (Py_BuildValue("i",kernel)==nullptr)
  {
    fprintf(stdout,"Unable to create MeshSystemRegistrar\n");
    return SV_PYTHON_ERROR;

  }
  if(PySys_SetObject("MeshSystemRegistrar",Py_BuildValue("i",kernel))<0)
  {
    fprintf(stdout, "Unable to register MeshSystemRegistrar\n");
    return SV_PYTHON_ERROR;

  }
  // Initialize
  cvMeshSystem::SetCurrentKernel( cvMeshObject::KERNEL_INVALID );

  pyMeshObjectType.tp_new=PyType_GenericNew;
  if (PyType_Ready(&pyMeshObjectType)<0)
  {
    fprintf(stdout,"Error in pyMeshObjectType\n");
    return SV_PYTHON_ERROR;
  }
  PyObject* pythonC;

  pythonC = PyModule_Create(&pyMeshObjectmodule);
  if(pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyMeshObject\n");
    return SV_PYTHON_ERROR;
  }
  PyRunTimeErr = PyErr_NewException("pyMeshObject.error",NULL,NULL);
  PyModule_AddObject(pythonC,"error",PyRunTimeErr);
  Py_INCREF(&pyMeshObjectType);
  PyModule_AddObject(pythonC,"pyMeshObject",(PyObject*)&pyMeshObjectType);

  return pythonC;

}
#endif
//-------------------
//cvMesh_NewObjectCmd
//-------------------

PyObject* cvMesh_NewObjectCmd(pyMeshObject* self, PyObject* args)
{
  char *resultName;
  char *meshFileName = NULL;
  char *solidFileName = NULL;

  if(!PyArg_ParseTuple(args,"s|ss",&resultName,&meshFileName,&solidFileName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char and two optional chars, resultname,meshFileName, solidFileName");
    
  }

  // Do work of command:

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( resultName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists.");
    
  }

  // Instantiate the new mesh:
  cvMeshObject *geom;
  geom = cvMeshSystem::DefaultInstantiateMeshObject(meshFileName, solidFileName );
  if ( geom == NULL ) {
    PyErr_SetString(PyRunTimeErr, "geom is NULL."); 
  }

  // Register the solid:
  if ( !( gRepository->Register( resultName, geom ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete geom;
    
  }

  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  return SV_PYTHON_OK;
}

// ----------------------
// cvMesh_GetObjectCmd
// ----------------------
PyObject* cvMesh_GetObjectCmd( pyMeshObject* self, PyObject* args)
{
  char *objName=NULL;
  RepositoryDataT type;
  cvRepositoryData *rd;
  cvMeshObject *geom;

  if (!PyArg_ParseTuple(args,"s", &objName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 1 char: objName");
    
  }

  // Do work of command:

  // Retrieve source object:
  rd = gRepository->GetObject( objName );
  char r[2048];
  if ( rd == NULL )
  {
    r[0] = '\0';
    sprintf(r, "couldn't find object %s", objName);
    PyErr_SetString(PyRunTimeErr,r);
    
  }

  type = rd->GetType();

  if ( type != MESH_T )
  {
    r[0] = '\0';
    sprintf(r, "%s not a mesh object", objName);
    PyErr_SetString(PyRunTimeErr,r);
    
  }
  
  geom = dynamic_cast<cvMeshObject*> (rd);
  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  return SV_PYTHON_OK; 
  
}
    

// ----------------------
// cvMesh_ListMethodsCmd
// ----------------------

PyObject* cvMesh_ListMethodsCmd(PyObject* self, PyObject* args)
{
  MeshPrintMethods( );
  return SV_PYTHON_OK;
}

PyObject* cvMesh_SetMeshKernelCmd(PyObject* self, PyObject* args)
{

  char *kernelName;
  if(!PyArg_ParseTuple(args,"s",&kernelName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char, kernelname.");
    
  }

  // Do work of command:
  cvMeshObject::KernelType kernelType = cvMeshObject::GetKernelType( kernelName );

  if ( kernelType != cvMeshObject::KERNEL_INVALID && cvMeshSystem::SetCurrentKernel(kernelType)==SV_OK ) {
    return Py_BuildValue("s",kernelName);
  } else {
    PyErr_SetString(PyRunTimeErr, "Mesh kernel is not available");
    
  }
}

// -------------
// DeleteMesh
// -------------
// This is the deletion call-back for cvMeshObject object commands.

void DeleteMesh(pyMeshObject* self )
{
    cvMeshObject *geom =self->geom ;
    gRepository->UnRegister( geom->GetName() );
}

// -------------
// fakeDeleteMesh
// -------------
// This is the deletion call-back for cvMeshObject object commands.

void fakeDeleteMesh(pyMeshObject* self) {
  ;
}

// ------------
// MeshPrintMethods
// ------------

static void MeshPrintMethods()
{

  // Note:  I've commented out some of the currently
  // unimplemented methods in the mesh object.  Since I may
  // want these in the future, instead of removing the methods
  // from the object I just hide them from the user.  This
  // way all I have to do is bind in code in the MegaMeshObject
  // and these commands are ready to go.

  PySys_WriteStdout( "GetFacePolyData\n");
  PySys_WriteStdout( "GetKernel\n");
  PySys_WriteStdout( "GetPolyData\n");
  PySys_WriteStdout( "GetSolid\n");
  PySys_WriteStdout( "SetVtkPolyData\n");
  PySys_WriteStdout( "GetUnstructuredGrid\n");
  PySys_WriteStdout( "Print\n");
  PySys_WriteStdout( "Update\n");
  PySys_WriteStdout( "WriteMetisAdjacency\n");
  PySys_WriteStdout( "*** methods to generate meshes ***\n");
  PySys_WriteStdout( "LoadModel\n");
  /*
#ifdef SV_USE_MESHSIM_DISCRETE_MODEL
  PySys_WriteStdout( "LoadDiscreteModel\n");
#endif
  */
  PySys_WriteStdout( "LoadMesh\n");
  PySys_WriteStdout( "NewMesh\n");
  PySys_WriteStdout( "SetBoundaryLayer\n");
  PySys_WriteStdout( "SetWalls\n");
  PySys_WriteStdout( "SetMeshOptions\n");
  PySys_WriteStdout( "SetCylinderRefinement\n");
  PySys_WriteStdout( "SetSphereRefinement\n");
  PySys_WriteStdout( "SetSizeFunctionBasedMesh\n");
  PySys_WriteStdout( "GenerateMesh\n");
  PySys_WriteStdout( "WriteMesh\n");
  PySys_WriteStdout( "WriteStats\n");
  PySys_WriteStdout( "Adapt\n");
  PySys_WriteStdout( "SetSolidKernel\n");
  PySys_WriteStdout( "GetModelFaceInfo\n");

  return;
}

// ----------------
// cvMesh_GetKernelMtd
// ----------------

static PyObject*  cvMesh_GetKernelMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;
  cvMeshObject::KernelType kernelType;
  char *kernelName;

  if (geom->GetMeshLoaded() == 0)
  {
    if (geom->Update() == SV_ERROR)
    {
      PyErr_SetString(PyRunTimeErr,"error updating.");
    }
  }
  kernelType = geom->GetMeshKernel();
  std::cout<<kernelType<<std::endl;
  kernelName = cvMeshObject::GetKernelName( kernelType );

  if ( kernelType == SM_KT_INVALID ) {
    fprintf(stderr,"Invalid kernel type\n");
    PyErr_SetString(PyRunTimeErr, "Invalid kernel type");
  } else {
    return Py_BuildValue("s",kernelName);
  }
}

// ----------------
// cvMesh_PrintMtd
// ----------------

static PyObject* cvMesh_PrintMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;

  if (geom->GetMeshLoaded() == 0)
  {
    if (geom->Update() == SV_ERROR)
    {
      PyErr_SetString(PyRunTimeErr,"error update.");
    }
  }
  if (geom->pyPrint() == SV_OK) {
    return SV_PYTHON_OK;
  } else {
    PyErr_SetString(PyRunTimeErr, "error print.");
  }

}


// -----------------
// cvMesh_UpdateMtd
// -----------------

static PyObject* cvMesh_UpdateMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;

  if (geom->Update() == SV_OK) {
    return SV_PYTHON_OK;
  } else {
    PyErr_SetString(PyRunTimeErr,"error update.");
  }
}


// ------------------------
// cvMesh-SetSolidKernelMtd
// ------------------------

PyObject* cvMesh_SetSolidKernelMtd(pyMeshObject* self, PyObject* args)
{
  char *kernelName;
  SolidModel_KernelT kernel;
  cvMeshObject *geom = self->geom;
  if(!PyArg_ParseTuple(args,"s",&kernelName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char, kernelName.");
    
  }
    // Do work of command:
  kernel = SolidModel_KernelT_StrToEnum( kernelName );
  if ( kernel != SM_KT_INVALID ) {
    geom->SetSolidModelKernel(kernel);
    return Py_BuildValue("s",kernelName);
  } else {
    PyErr_SetString(PyRunTimeErr,SolidModel_KernelT_EnumToStr( SM_KT_INVALID ));
    
  }
}

// -------------------------------
// cvMesh_WriteMetisAdjacencyMtd
// -------------------------------

static PyObject* cvMesh_WriteMetisAdjacencyMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;
  char *fn;
  int status;
  if(!PyArg_ParseTuple(args,"s",&fn))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char, fn.");
    
  }

  if (geom->GetMeshLoaded() == 0)
  {
    if (geom->Update() == SV_ERROR)
    {
      PyErr_SetString(PyRunTimeErr, "error update.");
    }
  }
  // Do work of command:
  status = geom->WriteMetisAdjacency( fn );

  if ( status != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error writing object ");
    
  } else {
    return SV_PYTHON_OK;
  }
}

// ----------------------
// cvMesh_GetPolyDataMtd
// ----------------------

static PyObject* cvMesh_GetPolyDataMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;
  char *resultName;
  cvPolyData *pd;
  if(!PyArg_ParseTuple(args,"s",&resultName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char, resultName");
    
  }

  // Do work of command:

  if (geom->GetMeshLoaded() == 0)
  {
    if (geom->Update() == SV_ERROR)
    {
        
        PyErr_SetString(PyRunTimeErr, "error update.");
    }
  }
  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( resultName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  // Get the cvPolyData:
  pd = geom->GetPolyData();
  if ( pd == NULL ) {
    PyErr_SetString(PyRunTimeErr, "error getting cvPolyData" );
  }

  // Register the result:
  if ( !( gRepository->Register( resultName, pd ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete pd;
    
  }

  return SV_PYTHON_OK;
}

// ----------------------
// cvMesh_GetSolidMtd
// ----------------------

static PyObject* cvMesh_GetSolidMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;
  char *resultName;
  cvPolyData *pd;
  if(!PyArg_ParseTuple(args,"s",&resultName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char, resultName");
    
  }

  // Do work of command:

  if (geom->GetMeshLoaded() == 0)
  {
    if (geom->Update() == SV_ERROR)
    {
      PyErr_SetString(PyRunTimeErr, "error update.");
    }
  }
  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( resultName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists" );
    
  }

  // Get the cvPolyData:
  pd = geom->GetSolid();
  if ( pd == NULL ) {
    PyErr_SetString(PyRunTimeErr, "error getting cvPolyData");
  }

  // Register the result:
  if ( !( gRepository->Register( resultName, pd ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj ");
    delete pd;
    
  }

  return SV_PYTHON_OK;
}

// ----------------------
// cvMesh_SetVtkPolyDataMtd
// ----------------------

static PyObject* cvMesh_SetVtkPolyDataMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;
  char *objName;
  RepositoryDataT type;
  cvRepositoryData *obj;
  vtkPolyData *pd;
  if(!PyArg_ParseTuple(args,"s",&objName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char, objName");
    
  }

  if (geom->GetMeshLoaded() == 0)
  {
    if (geom->Update() == SV_ERROR)
    {
      PyErr_SetString(PyRunTimeErr, "error update.");
    }
  }
  // Do work of command:
  type = gRepository->GetType( objName );
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "obj must be of type cvPolyData");
    
  }

  obj = gRepository->GetObject( objName );
  switch (type) {
  case POLY_DATA_T:
    pd = ((cvPolyData *)obj)->GetVtkPolyData();
    break;
  default:
    PyErr_SetString(PyRunTimeErr, "error in SetVtkPolyData");
    
    break;
  }

  // set the vtkPolyData:
  if(!geom->SetVtkPolyDataObject(pd))
  {
    PyErr_SetString(PyRunTimeErr, "error set vtk polydate object.");
  }

  return SV_PYTHON_OK;
}

// -------------------------------
// cvMesh_GetUnstructuredGridMtd
// -------------------------------

static PyObject* cvMesh_GetUnstructuredGridMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;
  char *resultName;
  cvUnstructuredGrid *ug;
  if(!PyArg_ParseTuple(args,"s",&resultName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char, resultName");
    
  }

  // Do work of command:

  if (geom->GetMeshLoaded() == 0)
  {
    if (geom->Update() == SV_ERROR)
    {
      PyErr_SetString(PyRunTimeErr, "error update.");
    }
  }
  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( resultName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists" );
    
  }

  // Get the cvUnstructuredGrid:
  ug = geom->GetUnstructuredGrid();
  if ( ug == NULL ) {
    PyErr_SetString(PyRunTimeErr, "error getting cvPolyData" );
    
  }

  // Register the result:
  if ( !( gRepository->Register( resultName, ug ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj " );
    delete ug;
    
  }

  return SV_PYTHON_OK;
}


// --------------------------
// cvMesh_GetFacePolyDataMtd
// --------------------------

static PyObject* cvMesh_GetFacePolyDataMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;
  char *resultName;
  cvPolyData *pd;
  int face;
  if(!PyArg_ParseTuple(args,"si",&resultName,&face))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char and one int, resultName, face");
    
  }
  // Do work of command:
  if (geom->GetMeshLoaded() == 0)
  {
    if (geom->Update() == SV_ERROR)
    {
      PyErr_SetString(PyRunTimeErr, "error update.");
    }
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( resultName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists" );
    
  }

  // Get the cvPolyData:
  pd = geom->GetFacePolyData(face);
  if ( pd == NULL ) {
    PyErr_SetString(PyRunTimeErr, "error getting cvPolyData ");
    
  }

  // Register the result:
  if ( !( gRepository->Register( resultName, pd ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository" );
    delete pd;
    
  }

  return SV_PYTHON_OK;
}

//
// LogOn
//

PyObject* cvMesh_LogonCmd(PyObject* self, PyObject* args)
{

  char *logFileName;
  if(!PyArg_ParseTuple(args,"s",&logFileName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char, logFileName");
    
  }

  // Do work of command:

  cvMeshSystem* meshKernel = cvMeshSystem::GetCurrentKernel();

  // read in the results file
  if (meshKernel == NULL || meshKernel->LogOn(logFileName) == SV_ERROR) {
      PyErr_SetString(PyRunTimeErr, "error opening logfile");
      
  }

  return SV_PYTHON_OK;
}


// ------------------
// cvMesh_LogoffCmd
// ------------------

PyObject* cvMesh_LogoffCmd( PyObject* self, PyObject* args)
{
  cvMeshSystem* meshKernel = cvMeshSystem::GetCurrentKernel();

  if (meshKernel == NULL || meshKernel->LogOff() == SV_ERROR) {
      PyErr_SetString(PyRunTimeErr, "error turning off logfile ");
      
  }

  return SV_PYTHON_OK;
}

// -------------------------
// cvMesh_SetMeshOptionsMtd
// -------------------------

static PyObject* cvMesh_SetMeshOptionsMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;
  char *flags;
  PyObject* valueList;
  if(!PyArg_ParseTuple(args,"sO",&flags,&valueList))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char and one list,flags and valuelist");
    
  }
  int numValues = PyList_Size(valueList);
  double *values = new double [numValues];
  for (int j=0 ; j<numValues;j++)
  {
    values[j]=PyFloat_AsDouble(PyList_GetItem(valueList,j));
  }
  // Do work of command:
  // Get the cvPolyData:
  if ( geom->SetMeshOptions(flags,numValues,values) == SV_ERROR ) {
    PyErr_SetString(PyRunTimeErr, "error in method ");
    delete [] values;
    
  }
  delete [] values;

  return SV_PYTHON_OK;
}

//
// LoadModel
//

PyObject* cvMesh_LoadModelMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;
  if (geom==NULL)
  {
      PyErr_SetString(PyRunTimeErr,"Mesh object not registered in repository");
      
  }
  char *FileName;
  if(!PyArg_ParseTuple(args,"s",&FileName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char,FileName");
    
  }

  // Do work of command:

  // read in the results file
  fprintf(stderr,"Filename: %s\n",FileName);
  if (geom->LoadModel(FileName) == SV_ERROR) {
      PyErr_SetString(PyRunTimeErr, "error loading solid model");
      
  }

  return SV_PYTHON_OK;
}

// -------------------
// Solid_GetBoundaryFacesMtd
// -------------------
//
PyObject* cvMesh_GetBoundaryFacesMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;
  double angle = 0.0;
  if(!PyArg_ParseTuple(args,"d",&angle))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one double,angle");
    
  }

  int status = geom->GetBoundaryFaces(angle);
  if ( status == SV_OK ) {
    return SV_PYTHON_OK;
  } else {
    PyErr_SetString(PyRunTimeErr, "GetBoundaryFaces: error on object");
    
  }
}

#ifdef SV_USE_MESHSIM_DISCRETE_MODEL

/*
//
// LoadDiscreteModel
//

*/

#endif

PyObject* cvMesh_LoadMeshMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;
  char *FileName;
  char *SurfFileName = 0;
  if(!PyArg_ParseTuple(args,"s|s",&FileName, &SurfFileName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char or one optional char,FileName, SurfFileName");
    
  }

  // Do work of command:

  // read in the results file
  if (geom->LoadMesh(FileName,SurfFileName) == SV_ERROR) {
      PyErr_SetString(PyRunTimeErr, "error loading mesh ");
      
  }

  return SV_PYTHON_OK;
}

PyObject* cvMesh_WriteStatsMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;
  char *FileName;

  if(!PyArg_ParseTuple(args,"s",&FileName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char ,FileName");
    
  }

  // Do work of command:
  if (geom->GetMeshLoaded() == 0)
  {
    if (geom->Update() == SV_ERROR)
    {
      PyErr_SetString(PyRunTimeErr, "error update.");
    }
  }

  // read in the results file
  if (geom->WriteStats(FileName) == SV_ERROR) {
      PyErr_SetString(PyRunTimeErr, "error writing stats file ");
      
  }

  return SV_PYTHON_OK;
}

PyObject* cvMesh_AdaptMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;

  if (geom->GetMeshLoaded() == 0)
  {
    if (geom->Update() == SV_ERROR)
    {
      PyErr_SetString(PyRunTimeErr, "error update.");
    }
  }
  // Do work of command:
  if (geom->Adapt() == SV_OK) {
    return SV_PYTHON_OK;
  } else {
    PyErr_SetString(PyRunTimeErr, "error adapt.");
  }
}

PyObject* cvMesh_WriteMeshMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;
  char *FileName;
  int smsver = 0;
  if(!PyArg_ParseTuple(args,"s|i",&FileName,&smsver))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char and one optional int ,FileName, smsver");
    
  }

  // Do work of command:
  if (geom->GetMeshLoaded() == 0)
  {
    if (geom->Update() == SV_ERROR)
    {
      PyErr_SetString(PyRunTimeErr, "error update.");
    }
  }

  // read in the results file
  if (geom->WriteMesh(FileName,smsver) == SV_ERROR) {
      PyErr_SetString(PyRunTimeErr, "error writing mesh ");
      
  }

  return SV_PYTHON_OK;
}


// -------------------
// cvMesh_NewMeshMtd
// -------------------
PyObject* cvMesh_NewMeshMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;
  if (geom->NewMesh() == SV_ERROR)
  {
      PyErr_SetString(PyRunTimeErr, "error creating new mesh ");
      
  }

  return SV_PYTHON_OK;
}


// ------------------------
// cvMesh_GenerateMeshMtd
// ------------------------

PyObject* cvMesh_GenerateMeshMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;
  if (geom->GenerateMesh() == SV_ERROR)
  {
      PyErr_SetString(PyRunTimeErr, "Error generating mesh ");
      
  }

  return SV_PYTHON_OK;
}


// -------------------------------
// cvMesh_SetSphereRefinementMtd
// -------------------------------

static PyObject* cvMesh_SetSphereRefinementMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;
  double size;
  PyObject* ctrList;
  double ctr[3];
  double r;
  int nctr;

  if(!PyArg_ParseTuple(args,"ddO",&size,&r,&ctrList))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import two doubles and one list, size, r, ctrList");
    
  }
  nctr=PyList_Size(ctrList);
  if ( nctr != 3 )
  {
    PyErr_SetString(PyRunTimeErr,"sphere requires a 3D center coordinate");
    
  }

  // Do work of command:

  if ( geom->SetSphereRefinement(size,r,ctr) == SV_ERROR )   {
    PyErr_SetString(PyRunTimeErr, "error in method " );
    
  }

  return SV_PYTHON_OK;
}

// -------------------------------
// cvMesh_SetSizeFunctionBasedMeshMtd
// -------------------------------

static PyObject* cvMesh_SetSizeFunctionBasedMeshMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;
  char *functionName;
  double size;

  if(!PyArg_ParseTuple(args,"ds",&size,&functionName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one double and one char, size and functionName");
    
  }

  // Do work of command:

  if ( geom->SetSizeFunctionBasedMesh(size,functionName) == SV_ERROR ) {
    PyErr_SetString(PyRunTimeErr, "error in setting size function" );
    
  }

  return SV_PYTHON_OK;
}


// ---------------------------------
// cvMesh_SetCylinderRefinementMtd
// ---------------------------------

static PyObject* cvMesh_SetCylinderRefinementMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;
  double size;
  PyObject* ctrList;
  PyObject* nrmList;
  double ctr[3];
  double nrm[3];
  double r;
  int nctr;
  int nnrm;
  double length;

  if(!PyArg_ParseTuple(args,"ddOO",&size,&r, &length,&ctrList,&nrmList))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import two doubles and two lists, size, r, ctrList,nrmList.");
    
  }

  // Do work of command:
  nctr=PyList_Size(ctrList);
  nnrm=PyList_Size(nrmList);
  if ( nctr != 3 ) {
    PyErr_SetString(PyRunTimeErr,"sphere requires a 3D center coordinate");
    
  }

  if ( nnrm != 3 ) {
    PyErr_SetString(PyRunTimeErr,"norm must be 3D");
    
  }

  // Do work of command:

  if ( geom->SetCylinderRefinement(size,r,length,ctr,nrm) == SV_ERROR ) {
    PyErr_SetString(PyRunTimeErr, "error in method ");
    
  }

  return SV_PYTHON_OK;
}


// ----------------------------
// cvMesh_SetBoundaryLayerMtd
// ----------------------------

static PyObject* cvMesh_SetBoundaryLayerMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;
  int type = 0;
  int id = 0;
  int side = 0;
  int nL = 0;
  double *H = NULL;
  PyObject* Hlist;

  if(!PyArg_ParseTuple(args,"iiiiO",&type,&id,&side,&nL,&Hlist))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import four ints and one list, type, id, side, nL, Hlist.");
    
  }

  // Parse coordinate lists:
  int numH=PyList_Size(Hlist);
  H = new double [numH];
  for (int i=0; i<numH;i++)
  {
    H[i]=PyFloat_AsDouble(PyList_GetItem(Hlist,i));
  }
  // Do work of command:

  if ( geom->SetBoundaryLayer(type,id,side,nL,H) == SV_ERROR ) {
    PyErr_SetString(PyRunTimeErr, "error in method ");
    delete [] H;
    
  }

  return SV_PYTHON_OK;
}

// ----------------------------
// cvMesh_SetWallsMtd
// ----------------------------

static PyObject* cvMesh_SetWallsMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;
  PyObject* wallsList;

  if(!PyArg_ParseTuple(args,"O",&wallsList))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one list, wallsList");
    
  }


  // Parse coordinate lists:
  int numWalls=PyList_Size(wallsList);
  int *walls = new int [numWalls];
  for (int i=0;i<numWalls;i++)
  {
    walls[i]=PyLong_AsLong(PyList_GetItem(wallsList,i));
  }
  // Do work of command:

  if ( geom->SetWalls(numWalls,walls) == SV_ERROR ) {
    PyErr_SetString(PyRunTimeErr, "error in method ");
    delete [] walls;
    
  }

  return SV_PYTHON_OK;
}



// --------------------------
// cvMesh_GetModelFaceInfoMtd
// --------------------------

static PyObject* cvMesh_GetModelFaceInfoMtd( pyMeshObject* self, PyObject* args)
{
  cvMeshObject *geom = self->geom;

  char info[99999];
  //geom->GetModelFaceInfo(info);

  return Py_BuildValue("s",info);
}
