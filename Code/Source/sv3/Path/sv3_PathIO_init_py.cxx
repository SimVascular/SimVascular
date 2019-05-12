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
#include "Python.h"

#include "sv3_PathIO.h"
#include "sv3_PathIO_init_py.h"

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
using sv3::PathIO;
using sv3::PathGroup;
using sv3::PathElement;
#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC initpyPathIO();
#elif PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC PyInit_pyPathIO();
#endif
PyObject* PyRunTimeErrIO;
PyObject* sv4PathIO_NewObjectCmd( pyPathIO* self, PyObject* args);
PyObject* sv4PathIO_ReadPathGroupCmd( pyPathIO* self, PyObject* args);
PyObject* sv4PathIO_WritePathGroupCmd( pyPathIO* self, PyObject* args);
PyObject* sv4PathIO_WrtiePathCmd( pyPathIO* self, PyObject* args);

int PathIO_pyInit()
{
#if PYTHON_MAJOR_VERSION == 2
    initpyPathIO();
#elif PYTHON_MAJOR_VERSION == 3
    PyInit_pyPathIO();
#endif
  return SV_OK;
}

static PyMethodDef pyPathIO_methods[]={
  {"NewObject", (PyCFunction)sv4PathIO_NewObjectCmd,METH_VARARGS,NULL},
  {"ReadPathGroup",(PyCFunction)sv4PathIO_ReadPathGroupCmd,METH_VARARGS,NULL},
  {"WritePathGroup",(PyCFunction)sv4PathIO_WritePathGroupCmd, METH_VARARGS,NULL},
  {"WritePath",(PyCFunction)sv4PathIO_WrtiePathCmd,METH_VARARGS,NULL},
  {NULL,NULL}
};

static int pyPathIO_init(pyPathIO* self, PyObject* args)
{
  fprintf(stdout,"pyPathIO initialized.\n");
  return SV_OK;
}

static PyTypeObject pyPathIOType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  "pyPathIO.pyPathIO",             /* tp_name */
  sizeof(pyPathIO),             /* tp_basicsize */
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
  "pyPathIO  objects",           /* tp_doc */
  0,                         /* tp_traverse */
  0,                         /* tp_clear */
  0,                         /* tp_richcompare */
  0,                         /* tp_weaklistoffset */
  0,                         /* tp_iter */
  0,                         /* tp_iternext */
  pyPathIO_methods,             /* tp_methods */
  0,                         /* tp_members */
  0,                         /* tp_getset */
  0,                         /* tp_base */
  0,                         /* tp_dict */
  0,                         /* tp_descr_get */
  0,                         /* tp_descr_set */
  0,                         /* tp_dictoffset */
  (initproc)pyPathIO_init,                            /* tp_init */
  0,                         /* tp_alloc */
  0,                  /* tp_new */
};

static PyMethodDef pyPathIOModule_methods[] =
{
    {NULL,NULL}
};

#if PYTHON_MAJOR_VERSION == 3
static struct PyModuleDef pyPathIOModule = {
   PyModuleDef_HEAD_INIT,
   "pyPathIO",   /* name of module */
   "", /* module documentation, may be NULL */
   -1,       /* size of per-interpreter state of the module,
                or -1 if the module keeps state in global variables. */
   pyPathIOModule_methods
};
#endif

//----------------
//initpyPathIO
//----------------
#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC initpyPathIO()

{
  // Associate the mesh registrar with the python interpreter so it can be
  // retrieved by the DLLs.
  if (gRepository==NULL)
  {
    gRepository = new cvRepository();
    fprintf(stdout,"New gRepository created from cv_mesh_init\n");
  }

  // Initialize
  pyPathIOType.tp_new=PyType_GenericNew;
  if (PyType_Ready(&pyPathIOType)<0)
  {
    fprintf(stdout,"Error in pyPathIOType\n");
    return;
  }
  PyObject* pythonC;
  pythonC = Py_InitModule("pyPathIO",pyPathIOModule_methods);
  if(pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyPathIO\n");
    return;
  }
  PyRunTimeErrIO = PyErr_NewException("pyPathIO.error",NULL,NULL);
  PyModule_AddObject(pythonC,"error",PyRunTimeErrIO);
  Py_INCREF(&pyPathIOType);
  PyModule_AddObject(pythonC,"pyPathIO",(PyObject*)&pyPathIOType);
  return ;

}
#endif

#if PYTHON_MAJOR_VERSION == 3
//----------------
//PyInit_pyPathIO
//----------------
PyMODINIT_FUNC PyInit_pyPathIO()

{

  if (gRepository==NULL)
  {
    gRepository = new cvRepository();
    fprintf(stdout,"New gRepository created from sv3_PathIO_init\n");
  }

  // Initialize
  pyPathIOType.tp_new=PyType_GenericNew;
  if (PyType_Ready(&pyPathIOType)<0)
  {
    fprintf(stdout,"Error in pyPathIOType\n");
    return SV_PYTHON_ERROR;
  }
  PyObject* pythonC;
  pythonC = PyModule_Create(&pyPathIOModule);
  if(pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyPathIO\n");
    return SV_PYTHON_ERROR;
  }
  PyRunTimeErrIO = PyErr_NewException("pyPathIO.error",NULL,NULL);
  PyModule_AddObject(pythonC,"error",PyRunTimeErrIO);
  Py_INCREF(&pyPathIOType);
  PyModule_AddObject(pythonC,"pyPathIO",(PyObject*)&pyPathIOType);
  return pythonC;

}
#endif

// --------------------
// sv4PathIO_NewObjectCmd
// --------------------
PyObject* sv4PathIO_NewObjectCmd( pyPathIO* self, PyObject* args)
{
  // Instantiate the new mesh:
  PathIO *geom = new PathIO();

  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  return SV_PYTHON_OK; 

}

// --------------------
// sv4PathIO_ReadPathGroupCmd
// --------------------

PyObject* sv4PathIO_ReadPathGroupCmd( pyPathIO* self, PyObject* args)
{

    char* objName =NULL;
    char* fn=NULL;
    if(!PyArg_ParseTuple(args,"ss",&objName,&fn))
    {
        PyErr_SetString(PyRunTimeErrIO,"Could not import two chars, objName, fn");
        
    }
    
    // Make sure the specified result object does not exist:
    if ( gRepository->Exists( objName ) ) {
        PyErr_SetString(PyRunTimeErrIO, "object already exists.");
        
    }

    PathIO* pathIO = self->geom;
    if (pathIO==NULL)
    {
        PyErr_SetString(PyRunTimeErrIO,"PathIO does not exist.");
        
    }
    
    std::string str(fn);
    PathGroup* pthGrp = pathIO->ReadFile(str);
    if(pthGrp==NULL)
    {
        PyErr_SetString(PyRunTimeErrIO,"Error reading file");
        
    }

    // Register the object:
    if ( !( gRepository->Register( objName, pthGrp ) ) ) {
        PyErr_SetString(PyRunTimeErrIO, "error registering obj in repository");
        delete pthGrp;
        
    }
    
    Py_INCREF(pathIO);
    self->geom=pathIO;
    Py_DECREF(pathIO);
    return SV_PYTHON_OK; 
    
}

// --------------------
// sv4PathIO_WritePathGroupCmd
// --------------------

PyObject* sv4PathIO_WritePathGroupCmd( pyPathIO* self, PyObject* args)
{
    char* fn=NULL;
    char* objName=NULL;
    RepositoryDataT type;
    cvRepositoryData *rd;
    PathGroup *pathGrp;
    
    if(!PyArg_ParseTuple(args,"ss",&objName, &fn))
    {
        PyErr_SetString(PyRunTimeErrIO,"Could not import two chars, objName, fn");
        
    }

    // Do work of command:
    
    // Retrieve source object:
    rd = gRepository->GetObject( objName );
    char r[2048];
    if ( rd == NULL )
    {
        r[0] = '\0';
        sprintf(r, "couldn't find object %s", objName);
        PyErr_SetString(PyRunTimeErrIO,r);
        
    }
    
    type = rd->GetType();
    
    if ( type != PATHGROUP_T )
    {
        r[0] = '\0';
        sprintf(r, "%s not a path group object", objName);
        PyErr_SetString(PyRunTimeErrIO,r);
        
    }
    
    pathGrp = dynamic_cast<PathGroup*> (rd);
    
    if (self->geom==NULL)
    {
        PyErr_SetString(PyRunTimeErrIO,"PathIO does not exist.");
        
    }
    
    std::string str(fn);
    if(self->geom->Write(str,pathGrp)==SV_ERROR)
    {
        PyErr_SetString(PyRunTimeErrIO, "Could not write path.");
        
    }
    
    return SV_PYTHON_OK; 
    
}

//----------------------------
// sv4PathIO_WrtiePathCmd
//----------------------------
PyObject* sv4PathIO_WrtiePathCmd(pyPathIO* self, PyObject* args)
{
    char* fn=NULL;
    char* objName=NULL;
    RepositoryDataT type;
    cvRepositoryData *rd;
    PathElement *path;
    
    if(!PyArg_ParseTuple(args,"ss",&objName, &fn))
    {
        PyErr_SetString(PyRunTimeErrIO,"Could not import two chars, objName, fn");
        
    }

    // Do work of command:
    
    // Retrieve source object:
    rd = gRepository->GetObject( objName );
    char r[2048];
    if ( rd == NULL )
    {
        r[0] = '\0';
        sprintf(r, "couldn't find object %s", objName);
        PyErr_SetString(PyRunTimeErrIO,r);
        
    }
    
    type = rd->GetType();
    
    if ( type != PATH_T )
    {
        r[0] = '\0';
        sprintf(r, "%s not a path object", objName);
        PyErr_SetString(PyRunTimeErrIO,r);
        
    }
    
    path = dynamic_cast<PathElement*> (rd);
    
    if (self->geom==NULL)
    {
        PyErr_SetString(PyRunTimeErrIO,"PathIO does not exist.");
        
    }
    
    PathGroup* pathGrp = new PathGroup();
    pathGrp->Expand(1);
    pathGrp->SetPathElement(path);
    
    std::string str(fn);
    if(self->geom->Write(str,pathGrp)==SV_ERROR)
    {
        PyErr_SetString(PyRunTimeErrIO, "Could not write path.");
        
    }
    
    return SV_PYTHON_OK; 
} 
