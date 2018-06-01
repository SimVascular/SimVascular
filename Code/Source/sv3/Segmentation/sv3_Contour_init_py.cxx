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
#include "sv3_PathElement.h"
#include "sv3_Contour.h"
#include "sv3_LevelSetContour.h"
#include "sv3_Contour_init_py.h"
#include "sv3_SegmentationUtils.h"
#include "vtkPythonUtil.h"

#include <stdio.h>
#include <string.h>
#include <array>
#include <iostream>
#include "sv_Repository.h"
#include "sv_RepositoryData.h"
// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif
// Globals:
// --------

#include "sv2_globals.h"

using sv3::Contour;
using sv3::PathElement;

PyMODINIT_FUNC initpyContour();
PyObject* PyRunTimeErr;
PyObject* Contour_NewObjectCmd( pyContour* self, PyObject* args);
PyObject* Contour_GetObjectCmd( pyContour* self, PyObject* args);
PyObject* Contour_CreateLSCmd( pyContour* self, PyObject* args);
PyObject* Contour_SetKernelCmd( PyObject* self, PyObject *args);
 
int Contour_pyInit()
{
  initpyContour();
  return Py_OK;
}

static PyMethodDef pyContour_methods[]={
  {"contour_newObject", (PyCFunction)Contour_NewObjectCmd,METH_VARARGS,NULL},
  {"contour_getObject", (PyCFunction)Contour_GetObjectCmd,METH_VARARGS,NULL},
  {"contour_create", (PyCFunction)Contour_CreateCmd,METH_VARARGS,NULL},
  {NULL,NULL}
};

static int pyContour_init(pyContour* self, PyObject* args)
{
  fprintf(stdout,"pyContour initialized.\n");
  return Py_OK;
}

static PyTypeObject pyContourType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  "pyContour.pyContour",             /* tp_name */
  sizeof(pyContour),             /* tp_basicsize */
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
  "pyContour  objects",           /* tp_doc */
  0,                         /* tp_traverse */
  0,                         /* tp_clear */
  0,                         /* tp_richcompare */
  0,                         /* tp_weaklistoffset */
  0,                         /* tp_iter */
  0,                         /* tp_iternext */
  pyContour_methods,             /* tp_methods */
  0,                         /* tp_members */
  0,                         /* tp_getset */
  0,                         /* tp_base */
  0,                         /* tp_dict */
  0,                         /* tp_descr_get */
  0,                         /* tp_descr_set */
  0,                         /* tp_dictoffset */
  (initproc)pyContour_init,                            /* tp_init */
  0,                         /* tp_alloc */
  0,                  /* tp_new */
};

static PyMethodDef pyContourModule_methods[] =
{
    {"SetContourKernel", (PyCFunction)Contour_SetKernelCmd, METH_VARARGS,NULL},
    {NULL,NULL}
};

//----------------
//initpyContour
//----------------
PyMODINIT_FUNC initpyContour()

{
  // Associate the mesh registrar with the python interpreter so it can be
  // retrieved by the DLLs.
  if (gRepository==NULL)
  {
    gRepository = new cvRepository();
    fprintf(stdout,"New gRepository created from sv3_Contour_init\n");
  }

  Contour::gCurrentKernel = KERNEL_INVALID;
  if (PySys_SetObject("ContourObjectRegistrar",(PyObject*)&Contour::gRegistrar)<0)
  {
    fprintf(stdout,"Unable to create ContourObjectRegistrar");
    return;
  }
  // Initialize
  pyContourType.tp_new=PyType_GenericNew;
  if (PyType_Ready(&pyContourType)<0)
  {
    fprintf(stdout,"Error in pyContourType\n");
    return;
  }
  PyObject* pythonC;
  pythonC = Py_InitModule("pyContour",pyContourModule_methods);
  if(pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyContour\n");
    return;
  }
  PyRunTimeErr = PyErr_NewException("pyContour.error",NULL,NULL);
  PyModule_AddObject(pythonC,"error",PyRunTimeErr);
  Py_INCREF(&pyContourType);
  PyModule_AddObject(pythonC,"pyContour",(PyObject*)&pyContourType);
  return ;

}

// ------------------
// Solid_SetKernelCmd
// ------------------

PyObject* Contour_SetKernelCmd( PyObject* self, PyObject *args)
{
    char *kernelName;
    KernelType kernel;
    if(!PyArg_ParseTuple(args,"s",&kernelName))
    {
        PyErr_SetString(PyRunTimeErr,"Could not import char kernelName");
        return Py_ERROR;
    }
    
    // Do work of command:
    if (strcmp( kernelName, "LevelSet" )==0 ) {
        kernel= KERNEL_LEVELSET;
    } else if (strcmp( kernelName, "Circle" )==0 ) {
        kernel= KERNEL_CIRCLE;
    } else if (strcmp( kernelName, "Polygon" )==0 ) {
        kernel= KERNEL_POLYGON;
    } else if (strcmp( kernelName, "Ellipse")==0 ) {
        kernel= KERNEL_ELLIPSE;
    } else {
        kernel= KERNEL_INVALID;
    }
    
    if ( kernel != KERNEL_INVALID ) {
        Contour::gCurrentKernel = kernel;
        return Py_BuildValue("s",kernelName);
    } else {
        PyErr_SetString(PyRunTimeErr, "contour kernel is invalid");
        return Py_ERROR;
    }
}

// --------------------
// Contour_NewObjectCmd
// --------------------
PyObject* Contour_NewObjectCmd( pyContour* self, PyObject* args)
{
    char *objName = NULL;
    char *pathName=NULL;
    vtkImageData *vtkObj;
    int index=0;
    RepositoryDataT type;
    cvRepositoryData *rd;
    PathElement *path;
    PyObject *vtkName; 
    if (!PyArg_ParseTuple(args,"ssOi", &objName, &pathName, &vtkName, &index))
    {
        PyErr_SetString(PyRunTimeErr, "Could not import 1 char, 1 vtkImgae and \
                    1 int: pathName, vtkObj abd index");
        return Py_ERROR;
    }
  
  // Make sure the specified result object does not exist:
    if ( gRepository->Exists( objName ) ) {
        PyErr_SetString(PyRunTimeErr, "object already exists.");
        return Py_ERROR;
    }
  
    // Do work of command:
    // Look up the named vtk object:
    vtkObj = (vtkImageData *)vtkPythonUtil::GetPointerFromObject( vtkName,
        "vtkImageData");
    if ( vtkObj == NULL )
    {
        PyErr_SetString(PyRunTimeErr, "error retrieving vtkImage object ");
        return Py_ERROR;
    }
    // Retrieve source object:
    rd = gRepository->GetObject( pathName );
    char r[2048];
    if ( rd == NULL )
    {
        r[0] = '\0';
        sprintf(r, "couldn't find object %s", pathName);
        PyErr_SetString(PyRunTimeErr,r);
        return Py_ERROR;
    }
    
    type = rd->GetType();
    
    if ( type != PATH_T )
    {
        r[0] = '\0';
        sprintf(r, "%s not a path object", pathName);
        PyErr_SetString(PyRunTimeErr,r);
        return Py_ERROR;
    }
    
    path = dynamic_cast<PathElement*> (rd);
    
    int numPathPts = path->GetPathPointNumber();
    if (index>=numPathPts)
    {
        PyErr_SetString(PyRunTimeErr, "Index exceed number of path points");
        return Py_ERROR;
    }

    vtkImageData*  slice = sv3::SegmentationUtils::GetSlicevtkImage(path->GetPathPoint(index),vtkObj, 5.0);
    // Instantiate the new mesh:
    Contour *geom = sv3::Contour::DefaultInstantiateContourObject(Contour::gCurrentKernel, path->GetPathPoint(index), slice );;
    
    // Register the solid:
    if ( !( gRepository->Register( objName, geom ) ) ) {
        PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
        delete geom;
        return Py_ERROR;
    }
    
    Py_INCREF(geom);
    self->geom=geom;
    Py_DECREF(geom);
    Py_RETURN_NONE; 

}

// --------------------
// Contour_GetObjectCmd
// --------------------
PyObject* Contour_GetObjectCmd( pyContour* self, PyObject* args)
{
  char *objName=NULL;
  RepositoryDataT type;
  cvRepositoryData *rd;
  Contour *contour;

  if (!PyArg_ParseTuple(args,"s", &objName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 1 char: objName");
    return Py_ERROR;
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
    return Py_ERROR;
  }

  type = rd->GetType();

  if ( type != CONTOUR_T )
  {
    r[0] = '\0';
    sprintf(r, "%s not a contour object", objName);
    PyErr_SetString(PyRunTimeErr,r);
    return Py_ERROR;
  }
  
  contour = dynamic_cast<Contour*> (rd);
  Py_INCREF(contour);
  self->geom=contour;
  Py_DECREF(contour);
  Py_RETURN_NONE; 
}

// --------------------
// Contour_CreateLSCmd
// --------------------
PyObject* Contour_CreateCmd( pyContour* self, PyObject* args)
{
    Contour* contour = self->geom;
    if (Contour::gCurrentKernel==KERNEL_LEVELSET)
    {
        sv3::levelSetContour::svLSParam paras;
        contour->SetLevelSetParas(&paras);
    }
    //std::cout<<"paras: "<<paras.radius<<std::endl;
    contour->CreateContourObject();
    if(contour->GetContourPointNumber()==0)
    {
        PyErr_SetString(PyRunTimeErr, "Error creating contour points");
        return Py_ERROR;
    }
    
    Py_INCREF(contour);
    self->geom=contour;
    Py_DECREF(contour);
    Py_RETURN_NONE; 
    
}