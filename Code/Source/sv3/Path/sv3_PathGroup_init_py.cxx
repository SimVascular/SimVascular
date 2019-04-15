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

#include "sv3_PathGroup.h"
#include "sv3_PathGroup_init_py.h"
#include "sv3_PathElement.h"
#include "sv3_PathElement_init_py.h"

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
using sv3::PathGroup;
using sv3::PathElement;
#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC initpyPathGroup();
#elif PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC PyInit_pyPathGroup();
#endif
PyObject* PyRunTimeErrPg;
PyObject* sv4PathGroup_NewObjectCmd( pyPathGroup* self, PyObject* args);
PyObject* sv4PathGroup_GetObjectCmd( pyPathGroup* self, PyObject* args);
PyObject* sv4PathGroup_SetPathCmd( pyPathGroup* self, PyObject* args);
PyObject* sv4PathGroup_GetTimeSizeCmd( pyPathGroup* self, PyObject* args);
PyObject* sv4PathGroup_GetPathCmd( pyPathGroup* self, PyObject* args);
PyObject* sv4PathGroup_GetPathGroupIDCmd( pyPathGroup* self, PyObject* args);
PyObject* sv4PathGroup_SetPathGroupIDCmd(pyPathGroup* self, PyObject* args);
PyObject* sv4PathGroup_SetSpacingCmd(pyPathGroup* self, PyObject* args);
PyObject* sv4PathGroup_GetSpacingCmd(pyPathGroup* self, PyObject* args);
PyObject* sv4PathGroup_SetCalculationNumber(pyPathGroup* self, PyObject* args);
PyObject* sv4PathGroup_GetCalculationNumber(pyPathGroup* self, PyObject* args);
PyObject* sv4PathGroup_SetMethod(pyPathGroup* self, PyObject* args);
PyObject* sv4PathGroup_GetMethod(pyPathGroup* self, PyObject* args);


int PathGroup_pyInit()
{
#if PYTHON_MAJOR_VERSION == 2
    initpyPathGroup();
#elif PYTHON_MAJOR_VERSION == 3
    PyInit_pyPathGroup();
#endif
  return SV_OK;
}

static PyMethodDef pyPathGroup_methods[]={
  {"NewObject", (PyCFunction)sv4PathGroup_NewObjectCmd,METH_VARARGS,NULL},
  {"GetObject", (PyCFunction)sv4PathGroup_GetObjectCmd,METH_VARARGS,NULL},
  {"SetPath",(PyCFunction)sv4PathGroup_SetPathCmd,METH_VARARGS,NULL},
  {"GetTimeSize",(PyCFunction)sv4PathGroup_GetTimeSizeCmd, METH_NOARGS,NULL},
  {"GetPath",(PyCFunction)sv4PathGroup_GetPathCmd,METH_VARARGS,NULL},
  {"GetPathGroupID",(PyCFunction)sv4PathGroup_GetPathGroupIDCmd,METH_VARARGS,NULL},
  {"SetPathGroupID",(PyCFunction)sv4PathGroup_SetPathGroupIDCmd, METH_VARARGS,NULL},
  {"SetSpacing",(PyCFunction)sv4PathGroup_SetSpacingCmd, METH_NOARGS,NULL},
  {"GetSpacing",(PyCFunction)sv4PathGroup_GetSpacingCmd, METH_NOARGS, NULL},
  {"SetCalculationNumber",(PyCFunction)sv4PathGroup_SetCalculationNumber, METH_NOARGS, NULL},
  {"GetCalculationNumber",(PyCFunction)sv4PathGroup_GetCalculationNumber, METH_NOARGS, NULL},
  {"SetMethod",(PyCFunction)sv4PathGroup_SetMethod, METH_NOARGS, NULL},
  {"GetMethod",(PyCFunction)sv4PathGroup_GetMethod, METH_NOARGS, NULL},
  {NULL,NULL}
};

static int pyPathGroup_init(pyPathGroup* self, PyObject* args)
{
  fprintf(stdout,"pyPathGroup initialized.\n");
  return SV_OK;
}

static PyTypeObject pyPathGroupType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  "pyPathGroup.pyPathGroup",             /* tp_name */
  sizeof(pyPathGroup),             /* tp_basicsize */
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
  "pyPathGroup  objects",           /* tp_doc */
  0,                         /* tp_traverse */
  0,                         /* tp_clear */
  0,                         /* tp_richcompare */
  0,                         /* tp_weaklistoffset */
  0,                         /* tp_iter */
  0,                         /* tp_iternext */
  pyPathGroup_methods,             /* tp_methods */
  0,                         /* tp_members */
  0,                         /* tp_getset */
  0,                         /* tp_base */
  0,                         /* tp_dict */
  0,                         /* tp_descr_get */
  0,                         /* tp_descr_set */
  0,                         /* tp_dictoffset */
  (initproc)pyPathGroup_init,                            /* tp_init */
  0,                         /* tp_alloc */
  0,                  /* tp_new */
};

static PyMethodDef pyPathGroupModule_methods[] =
{
    {NULL,NULL}
};

#if PYTHON_MAJOR_VERSION == 3
static struct PyModuleDef pyPathGroupModule = {
   PyModuleDef_HEAD_INIT,
   "pyPathGroup",   /* name of module */
   "", /* module documentation, may be NULL */
   -1,       /* size of per-interpreter state of the module,
                or -1 if the module keeps state in global variables. */
   pyPathGroupModule_methods
};
#endif

//----------------
//initpyPathGroup
//----------------
#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC initpyPathGroup()

{
  // Associate the mesh registrar with the python interpreter so it can be
  // retrieved by the DLLs.
  if (gRepository==NULL)
  {
    gRepository = new cvRepository();
    fprintf(stdout,"New gRepository created from cv_mesh_init\n");
  }

  // Initialize
  pyPathGroupType.tp_new=PyType_GenericNew;
  if (PyType_Ready(&pyPathGroupType)<0)
  {
    fprintf(stdout,"Error in pyPathGroupType\n");
    return;
  }
  PyObject* pythonC;
  pythonC = Py_InitModule("pyPathGroup",pyPathGroupModule_methods);
  if(pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyPathGroup\n");
    return;
  }
  PyRunTimeErrPg = PyErr_NewException("pyPathGroup.error",NULL,NULL);
  PyModule_AddObject(pythonC,"error",PyRunTimeErrPg);
  Py_INCREF(&pyPathGroupType);
  PyModule_AddObject(pythonC,"pyPathGroup",(PyObject*)&pyPathGroupType);
  return ;

}
#endif

#if PYTHON_MAJOR_VERSION == 3
//----------------
//PyInit_pyPathGroup
//----------------
PyMODINIT_FUNC PyInit_pyPathGroup()

{

  if (gRepository==NULL)
  {
    gRepository = new cvRepository();
    fprintf(stdout,"New gRepository created from sv3_PathGroup_init\n");
  }

  // Initialize
  pyPathGroupType.tp_new=PyType_GenericNew;
  if (PyType_Ready(&pyPathGroupType)<0)
  {
    fprintf(stdout,"Error in pyPathGroupType\n");
    return SV_PYTHON_ERROR;
  }
  PyObject* pythonC;
  pythonC = PyModule_Create(&pyPathGroupModule);
  if(pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyPathGroup\n");
    return SV_PYTHON_ERROR;
  }
  PyRunTimeErrPg = PyErr_NewException("pyPathGroup.error",NULL,NULL);
  PyModule_AddObject(pythonC,"error",PyRunTimeErrPg);
  Py_INCREF(&pyPathGroupType);
  PyModule_AddObject(pythonC,"pyPathGroup",(PyObject*)&pyPathGroupType);
  return pythonC;

}
#endif

// --------------------
// sv4PathGroup_NewObjectCmd
// --------------------
PyObject* sv4PathGroup_NewObjectCmd( pyPathGroup* self, PyObject* args)
{
    
  char* objName;
  
  if(!PyArg_ParseTuple(args,"s",&objName))
  {
    PyErr_SetString(PyRunTimeErrPg,"Could not import one char or optional char and ints");
    
  }
  
   // Do work of command:

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    PyErr_SetString(PyRunTimeErrPg, "object already exists.");
    
  }

  // Instantiate the new mesh:
  PathGroup *geom = new PathGroup();

  // Register the solid:
  if ( !( gRepository->Register( objName, geom ) ) ) {
    PyErr_SetString(PyRunTimeErrPg, "error registering obj in repository");
    delete geom;
    
  }

  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  return SV_PYTHON_OK; 

}

// --------------------
// sv4PathGroup_GetObjectCmd
// --------------------
PyObject* sv4PathGroup_GetObjectCmd( pyPathGroup* self, PyObject* args)
{
  char *objName=NULL;
  RepositoryDataT type;
  cvRepositoryData *rd;
  PathGroup *path;

  if (!PyArg_ParseTuple(args,"s", &objName))
  {
    PyErr_SetString(PyRunTimeErrPg, "Could not import 1 char: objName");
    
  }

  // Do work of command:

  // Retrieve source object:
  rd = gRepository->GetObject( objName );
  char r[2048];
  if ( rd == NULL )
  {
    r[0] = '\0';
    sprintf(r, "couldn't find object %s", objName);
    PyErr_SetString(PyRunTimeErrPg,r);
    
  }

  type = rd->GetType();

  if ( type != PATHGROUP_T )
  {
    r[0] = '\0';
    sprintf(r, "%s not a path group object", objName);
    PyErr_SetString(PyRunTimeErrPg,r);
    
  }
  
  path = dynamic_cast<PathGroup*> (rd);
  Py_INCREF(path);
  self->geom=path;
  Py_DECREF(path);
  return SV_PYTHON_OK; 

}

// --------------------
// sv4PathGroup_SetPathCmd
// --------------------

PyObject* sv4PathGroup_SetPathCmd( pyPathGroup* self, PyObject* args)
{

    char* objName;
    int index=-2;
    cvRepositoryData *rd;
    RepositoryDataT type;
    if(!PyArg_ParseTuple(args,"si",&objName,&index))
    {
        PyErr_SetString(PyRunTimeErrPg,"Could not import path name, index");
        
    }
    
        // Retrieve source object:
    rd = gRepository->GetObject( objName );
    char r[2048];
    if ( rd == NULL )
    {
        r[0] = '\0';
        sprintf(r, "couldn't find object %s", objName);
        PyErr_SetString(PyRunTimeErrPg,r);
        
    }
    
    type = rd->GetType();
    
    if ( type != PATH_T )
    {
        r[0] = '\0';
        sprintf(r, "%s not a path object", objName);
        PyErr_SetString(PyRunTimeErrPg,r);
        
    }
    
    PathElement* path = static_cast<PathElement*> (rd);
    if (path==NULL)
    {
        PyErr_SetString(PyRunTimeErrPg,"Path does not exist.");
        
    }
    
    int timestepSize = self->geom->GetTimeSize();
    if (index+1>=timestepSize)
    {
        self->geom->Expand(index);
        self->geom->SetPathElement(path,index);
    }
    else
        self->geom->SetPathElement(path, index);
            
    return SV_PYTHON_OK; 
    
}


// --------------------
// sv4PathGroup_GetTimeSizeCmd
// --------------------

PyObject* sv4PathGroup_GetTimeSizeCmd( pyPathGroup* self, PyObject* args)
{
  // Do work of command:
    int timestepSize = self->geom->GetTimeSize();
    
    return Py_BuildValue("i",timestepSize); 
    
}

// --------------------
// sv4PathGroup_GetPathCmd
// --------------------
PyObject* sv4PathGroup_GetPathCmd( pyPathGroup* self, PyObject* args)
{

    int index;
    char* pathName=NULL;
    if(!PyArg_ParseTuple(args,"si",&pathName, &index))
    {
        PyErr_SetString(PyRunTimeErrPg,"Could not import char pathName, int index");
        
    }
        
    PathGroup* pathGrp = self->geom;
    if (pathGrp==NULL)
    {
        PyErr_SetString(PyRunTimeErrPg,"Path does not exist.");
        
    }
    
    // Make sure the specified result object does not exist:
    if ( gRepository->Exists( pathName ) ) {
        PyErr_SetString(PyRunTimeErrPg, "object already exists.");
        
    }
    
    PathElement* path;
    if (index<pathGrp->GetTimeSize())
        path = pathGrp->GetPathElement(index);
    else
    {
        PyErr_SetString(PyRunTimeErrPg, "Index out of bound.");
        
    }
    
    // Register the path:
    if ( !( gRepository->Register( pathName, path ) ) ) {
        PyErr_SetString(PyRunTimeErrPg, "error registering obj in repository");
        
    }
  
    return SV_PYTHON_OK;
    
}

// --------------------
// sv4PathGroup_GetPathGroupIDCmd
// --------------------

PyObject* sv4PathGroup_GetPathGroupIDCmd( pyPathGroup* self, PyObject* args)
{
  // Do work of command:
    int id = self->geom->GetPathID();
    
    return Py_BuildValue("i",id); 
    
}
    
// --------------------
// sv4PathGroup_SetPathGroupIDCmd
// --------------------

PyObject* sv4PathGroup_SetPathGroupIDCmd(pyPathGroup* self, PyObject* args)
{
    int id;
    if(!PyArg_ParseTuple(args,"i",&id))
    {
        PyErr_SetString(PyRunTimeErrPg,"Could not import int id");
        
    }
    
    self->geom->SetPathID(id);
    return SV_PYTHON_OK;
}

// --------------------
// sv4PathGroup_SetSpacingCmd
// --------------------
PyObject* sv4PathGroup_SetSpacingCmd(pyPathGroup* self, PyObject* args)
{
    double spacing;
    if(!PyArg_ParseTuple(args,"d",&spacing))
    {
        PyErr_SetString(PyRunTimeErrPg,"Could not import double spacing");
        
    }
    
    self->geom->SetSpacing(spacing);
    return SV_PYTHON_OK;
}

// --------------------
// sv4PathGroup_GetSpacingCmd
// --------------------
PyObject* sv4PathGroup_GetSpacingCmd(pyPathGroup* self, PyObject* args)
{
  // Do work of command:
    double spacing = self->geom->GetSpacing();
    
    return Py_BuildValue("d",spacing); 
    
}

// --------------------
// sv4PathGroup_SetMethod
// --------------------
PyObject* sv4PathGroup_SetMethod(pyPathGroup* self, PyObject* args)
{
    char* method;
    if(!PyArg_ParseTuple(args,"s",&method))
    {
        PyErr_SetString(PyRunTimeErrPg,"Could not import char method (total, subdivision or spacing)");
        
    }
    
    if(strcmp(method,"total")==0)
        self->geom->SetMethod(PathElement::CONSTANT_TOTAL_NUMBER);
    else if (strcmp(method, "subdivision")==0)
        self->geom->SetMethod(PathElement::CONSTANT_SUBDIVISION_NUMBER);
    else if(strcmp(method, "spacing")==0)
        self->geom->SetMethod(PathElement::CONSTANT_SPACING);
    else
    {
        PyErr_SetString(PyRunTimeErrPg,"Method not recognized.");
        
    }
    return SV_PYTHON_OK;
}

// --------------------
// sv4PathGroup_GetMethod
// --------------------
PyObject* sv4PathGroup_GetMethod(pyPathGroup* self, PyObject* args)
{
    PathElement::CalculationMethod method= self->geom->GetMethod();
    
    switch(method)
    {
    case PathElement::CONSTANT_TOTAL_NUMBER:
        return Py_BuildValue("s","total");
    case PathElement::CONSTANT_SUBDIVISION_NUMBER:
        return Py_BuildValue("s","subdivision");
    case PathElement::CONSTANT_SPACING:
        return Py_BuildValue("s","spacing");
    default:
        break;
    }
    
}

// --------------------
// sv4PathGroup_SetCalculationNumber
// --------------------
PyObject* sv4PathGroup_SetCalculationNumber(pyPathGroup* self, PyObject* args)
{
    int number;
    if(!PyArg_ParseTuple(args,"i",&number))
    {
        PyErr_SetString(PyRunTimeErrPg,"Could not import int number");
        
    }
    
    self->geom->SetCalculationNumber(number);
    return SV_PYTHON_OK;
}

// --------------------
// sv4PathGroup_GetCalculationNumber
// --------------------
PyObject* sv4PathGroup_GetCalculationNumber(pyPathGroup* self, PyObject* args)
{
    int number=self->geom->GetCalculationNumber();
    return Py_BuildValue("i",number);
}

