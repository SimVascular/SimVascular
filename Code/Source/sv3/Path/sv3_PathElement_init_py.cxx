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
using sv3::PathElement;
#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC initpyPath();
#elif PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC PyInit_pyPath();
#endif
PyObject* PyRunTimeErr;
PyObject* sv4Path_NewObjectCmd( pyPath* self, PyObject* args);
PyObject* sv4Path_GetObjectCmd( pyPath* self, PyObject* args);
PyObject* sv4Path_AddPointCmd( pyPath* self, PyObject* args);
PyObject* sv4Path_PrintCtrlPointCmd( pyPath* self, PyObject* args);
PyObject* sv4Path_RemovePointCmd( pyPath* self, PyObject* args);
PyObject* sv4Path_MoveCtrlPointCmd( pyPath* self, PyObject* args);
PyObject* sv4Path_SmoothPathCmd(pyPath* self, PyObject* args);
PyObject* sv4Path_CreatePathCmd(pyPath* self, PyObject* args);
PyObject* sv4Path_GetPathPtNumberCmd(pyPath* self, PyObject* args);
PyObject* sv4Path_GetPathPosPts(pyPath* self, PyObject* args);
PyObject* sv4Path_GetControlPts(pyPath* self, PyObject* args);


int Path_pyInit()
{
#if PYTHON_MAJOR_VERSION == 2
    initpyPath();
#elif PYTHON_MAJOR_VERSION == 3
    PyInit_pyPath();
#endif
  return SV_OK;
}

static PyMethodDef pyPath_methods[]={
  {"NewObject", (PyCFunction)sv4Path_NewObjectCmd,METH_VARARGS,NULL},
  {"GetObject", (PyCFunction)sv4Path_GetObjectCmd,METH_VARARGS,NULL},
  {"AddPoint",(PyCFunction)sv4Path_AddPointCmd,METH_VARARGS,NULL},
  {"PrintPoints",(PyCFunction)sv4Path_PrintCtrlPointCmd, METH_NOARGS,NULL},
  {"RemovePoint",(PyCFunction)sv4Path_RemovePointCmd,METH_VARARGS,NULL},
  {"MovePoint",(PyCFunction)sv4Path_MoveCtrlPointCmd,METH_VARARGS,NULL},
  {"Smooth",(PyCFunction)sv4Path_SmoothPathCmd, METH_VARARGS,NULL},
  {"CreatePath",(PyCFunction)sv4Path_CreatePathCmd, METH_NOARGS,NULL},
  {"GetPathPtsNum",(PyCFunction)sv4Path_GetPathPtNumberCmd, METH_NOARGS, NULL},
  {"GetPathPosPts",(PyCFunction)sv4Path_GetPathPosPts, METH_NOARGS, NULL},
  {"GetControlPts",(PyCFunction)sv4Path_GetControlPts, METH_NOARGS, NULL},
  {NULL,NULL}
};

static int pyPath_init(pyPath* self, PyObject* args)
{
  fprintf(stdout,"pyPath initialized.\n");
  return SV_OK;
}

static PyTypeObject pyPathType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  "pyPath.pyPath",             /* tp_name */
  sizeof(pyPath),             /* tp_basicsize */
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
  "pyPath  objects",           /* tp_doc */
  0,                         /* tp_traverse */
  0,                         /* tp_clear */
  0,                         /* tp_richcompare */
  0,                         /* tp_weaklistoffset */
  0,                         /* tp_iter */
  0,                         /* tp_iternext */
  pyPath_methods,             /* tp_methods */
  0,                         /* tp_members */
  0,                         /* tp_getset */
  0,                         /* tp_base */
  0,                         /* tp_dict */
  0,                         /* tp_descr_get */
  0,                         /* tp_descr_set */
  0,                         /* tp_dictoffset */
  (initproc)pyPath_init,                            /* tp_init */
  0,                         /* tp_alloc */
  0,                  /* tp_new */
};

static PyMethodDef pyPathModule_methods[] =
{
    {NULL,NULL}
};

#if PYTHON_MAJOR_VERSION == 3
static struct PyModuleDef pyPathModule = {
   PyModuleDef_HEAD_INIT,
   "pyPath",   /* name of module */
   "", /* module documentation, may be NULL */
   -1,       /* size of per-interpreter state of the module,
                or -1 if the module keeps state in global variables. */
   pyPathModule_methods
};
#endif

//----------------
//initpyPath
//----------------
#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC initpyPath()

{
  // Associate the mesh registrar with the python interpreter so it can be
  // retrieved by the DLLs.
  if (gRepository==NULL)
  {
    gRepository = new cvRepository();
    fprintf(stdout,"New gRepository created from cv_mesh_init\n");
  }

  // Initialize
  pyPathType.tp_new=PyType_GenericNew;
  if (PyType_Ready(&pyPathType)<0)
  {
    fprintf(stdout,"Error in pyPathType\n");
    return;
  }
  PyObject* pythonC;
  pythonC = Py_InitModule("pyPath",pyPathModule_methods);
  if(pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyPath\n");
    return;
  }
  PyRunTimeErr = PyErr_NewException("pyPath.error",NULL,NULL);
  PyModule_AddObject(pythonC,"error",PyRunTimeErr);
  Py_INCREF(&pyPathType);
  PyModule_AddObject(pythonC,"pyPath",(PyObject*)&pyPathType);
  return ;

}
#endif

#if PYTHON_MAJOR_VERSION == 3
//----------------
//PyInit_pyPath
//----------------
PyMODINIT_FUNC PyInit_pyPath()

{

  if (gRepository==NULL)
  {
    gRepository = new cvRepository();
    fprintf(stdout,"New gRepository created from sv3_PathElement_init\n");
  }

  // Initialize
  pyPathType.tp_new=PyType_GenericNew;
  if (PyType_Ready(&pyPathType)<0)
  {
    fprintf(stdout,"Error in pyPathType\n");
    return SV_PYTHON_ERROR;
  }
  PyObject* pythonC;
  pythonC = PyModule_Create(&pyPathModule);
  if(pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyPath\n");
    return SV_PYTHON_ERROR;
  }
  PyRunTimeErr = PyErr_NewException("pyPath.error",NULL,NULL);
  PyModule_AddObject(pythonC,"error",PyRunTimeErr);
  Py_INCREF(&pyPathType);
  PyModule_AddObject(pythonC,"pyPath",(PyObject*)&pyPathType);
  return pythonC;

}
#endif

// --------------------
// sv4Path_NewObjectCmd
// --------------------
PyObject* sv4Path_NewObjectCmd( pyPath* self, PyObject* args)
{
    
  char* objName;
  char* methodName;
  int calcNum=100, splacing=0;
  
  if(!PyArg_ParseTuple(args,"s|sii",&objName,&methodName,&calcNum,&splacing))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char or optional char and ints");
    
  }
  
   // Do work of command:

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists.");
    
  }

  // Instantiate the new mesh:
  PathElement *geom = new PathElement();

  // Register the solid:
  if ( !( gRepository->Register( objName, geom ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete geom;
    
  }

  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  return SV_PYTHON_OK; 

}

// --------------------
// sv4Path_GetObjectCmd
// --------------------
PyObject* sv4Path_GetObjectCmd( pyPath* self, PyObject* args)
{
  char *objName=NULL;
  RepositoryDataT type;
  cvRepositoryData *rd;
  PathElement *path;

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

  if ( type != PATH_T )
  {
    r[0] = '\0';
    sprintf(r, "%s not a path object", objName);
    PyErr_SetString(PyRunTimeErr,r);
    
  }
  
  path = dynamic_cast<PathElement*> (rd);
  Py_INCREF(path);
  self->geom=path;
  Py_DECREF(path);
  return SV_PYTHON_OK; 

}

// --------------------
// sv4Path_AddPointCmd
// --------------------

PyObject* sv4Path_AddPointCmd( pyPath* self, PyObject* args)
{

    PyObject* pyList;
    int index=-2;
    if(!PyArg_ParseTuple(args,"O|i",&pyList,&index))
    {
        PyErr_SetString(PyRunTimeErr,"Could not import list");
        
    }
    
    PathElement* path = self->geom;
    if (path==NULL)
    {
        PyErr_SetString(PyRunTimeErr,"Path does not exist.");
        
    }
    
    std::array<double,3> point;
    point[0] = PyFloat_AsDouble(PyList_GetItem(pyList,0));
    point[1] = PyFloat_AsDouble(PyList_GetItem(pyList,1));
    point[2] = PyFloat_AsDouble(PyList_GetItem(pyList,2));    
    
    if(path->SearchControlPoint(point,0)!=-2)
    {
        PyErr_SetString(PyRunTimeErr,"Point already exists");
        
    }

    if (index!=-2)
    {
        if(index>(path->GetControlPoints()).size())
        {
            PyErr_SetString(PyRunTimeErr,"Index exceeds path length");
            
        }
    }
    else
        index=path->GetInsertintIndexByDistance(point);
    path->InsertControlPoint(index,point);

        
    Py_INCREF(path);
    self->geom=path;
    Py_DECREF(path);
    return SV_PYTHON_OK; 
    
}

// --------------------
// sv4Path_RemovePointCmd
// --------------------

PyObject* sv4Path_RemovePointCmd( pyPath* self, PyObject* args)
{
    int index;
    if(!PyArg_ParseTuple(args,"i",&index))
    {
        PyErr_SetString(PyRunTimeErr,"Could not import int, index");
        
    }
    
    
    PathElement* path = self->geom;
    if (path==NULL)
    {
        PyErr_SetString(PyRunTimeErr,"Path does not exist.");
        
    }
    
    if(index>=(path->GetControlPoints()).size())
    {
        PyErr_SetString(PyRunTimeErr,"Index exceeds path length");
    }
    
    path->RemoveControlPoint(index);
    
    Py_INCREF(path);
    self->geom=path;
    Py_DECREF(path);
    return SV_PYTHON_OK; 
    
}

// --------------------
// sv4Path_MoveCtrlPointCmd
// --------------------
PyObject* sv4Path_MoveCtrlPointCmd( pyPath* self, PyObject* args)
{

    PyObject* pyList;
    int index;
    if(!PyArg_ParseTuple(args,"Oi",&pyList,&index))
    {
        PyErr_SetString(PyRunTimeErr,"Could not import list and index");
        
    }
    
    PathElement* path = self->geom;
    if (path==NULL)
    {
        PyErr_SetString(PyRunTimeErr,"Path does not exist.");
        
    }
    
    std::array<double,3> point;
    point[0] = PyFloat_AsDouble(PyList_GetItem(pyList,0));
    point[1] = PyFloat_AsDouble(PyList_GetItem(pyList,1));
    point[2] = PyFloat_AsDouble(PyList_GetItem(pyList,2));    
    
    if(index>=(path->GetControlPoints()).size())
    {
        PyErr_SetString(PyRunTimeErr,"Index exceeds path length");
    }


    path->SetControlPoint(index,point);

        
    Py_INCREF(path);
    self->geom=path;
    Py_DECREF(path);
    return SV_PYTHON_OK; 
    
}

// --------------------
// sv4Path_MoveCtrlPointCmd
// --------------------

PyObject* sv4Path_SmoothPathCmd( pyPath* self, PyObject* args)
{
    
    int sampleRate,numModes,controlPointsBased;
    if(!PyArg_ParseTuple(args,"iii",&sampleRate,&numModes,&controlPointsBased))
    {
        PyErr_SetString(PyRunTimeErr,"Could not import three integers \
                                sampleRate, numModes and controlPointsBased");
        
    }
    
    PathElement* path = self->geom;
    if (path==NULL)
    {
        PyErr_SetString(PyRunTimeErr,"Path does not exist.");
        
    }  
    
    bool controlPointsBasedBool=controlPointsBased==1?true:false;
    
    path = path->CreateSmoothedPathElement(sampleRate,numModes,controlPointsBasedBool);
        
    Py_INCREF(path);
    self->geom=path;
    Py_DECREF(path);
    return SV_PYTHON_OK; 
    
}

// --------------------
// sv4Path_PrintCtrlPointCmd
// --------------------

PyObject* sv4Path_PrintCtrlPointCmd( pyPath* self, PyObject* args)
{
    PathElement* path = self->geom;
    std::vector<std::array<double,3> > pts = path->GetControlPoints();
    for (int i=0;i<pts.size();i++)
    {
        std::array<double,3> pt = pts[i];
        PySys_WriteStdout("Point %i, %f, %f, %f \n",i, pt[0],pt[1],pt[2]);
    }
    
    return SV_PYTHON_OK; 
}

// --------------------
// sv4Path_CreatePathCmd
// --------------------
PyObject* sv4Path_CreatePathCmd(pyPath* self, PyObject* args)
{
    PathElement* path = self->geom;
    if (path==NULL)
    {
        PyErr_SetString(PyRunTimeErr,"Path does not exist.");
        
    }  
    path->CreatePathPoints();
    int num = (path->GetPathPoints()).size();
    if (num==0)
    {
        PyErr_SetString(PyRunTimeErr,"Error creating path from control points");
        
    }
    else
        printf("Total number of path points created is: %i \n", num);
        
    return SV_PYTHON_OK;
}

// --------------------
// sv4Path_GetPathPtNumberCmd
// --------------------
PyObject* sv4Path_GetPathPtNumberCmd(pyPath* self, PyObject* args)
{
    PathElement* path = self->geom;
    if (path==NULL)
    {
        PyErr_SetString(PyRunTimeErr,"Path does not exist.");
        
    }  
    
    int num = path->GetPathPointNumber();
    
    return Py_BuildValue("i",num);
}

//----------------------------
// sv4Path_GetPathPosPts
//----------------------------
PyObject* sv4Path_GetPathPosPts(pyPath* self, PyObject* args)
{
    PathElement* path = self->geom;
    if (path==NULL)
    {
        PyErr_SetString(PyRunTimeErr,"Path does not exist.");
        
    }  
    
    int num = path->GetPathPointNumber();
    PyObject* output = PyList_New(num);
    for (int i = 0; i<num; i++)
    {
        PyObject* tmpList = PyList_New(3);
        std::array<double,3> pos = path->GetPathPosPoint(i);
        for (int j=0; j<3; j++)
            PyList_SetItem(tmpList,j,PyFloat_FromDouble(pos[j]));
        PyList_SetItem(output,i,tmpList);
    }
    
    if(PyErr_Occurred()!=NULL)
    {
        PyErr_SetString(PyRunTimeErr, "error generating pathpospt output");
        
    }
    
     return output;
} 

//----------------------------
// sv4Path_GetControlPts
//----------------------------
PyObject* sv4Path_GetControlPts(pyPath* self, PyObject* args)
{
    PathElement* path = self->geom;
    if (path==NULL)
    {
        PyErr_SetString(PyRunTimeErr,"Path does not exist.");
    }

    int num = path->GetControlPointNumber();
    PyObject* output = PyList_New(num);
    for (int i = 0; i<num; i++)
    {
        PyObject* tmpList = PyList_New(3);
        std::array<double,3> pos = path->GetControlPoint(i);
        for (int j=0; j<3; j++)
            PyList_SetItem(tmpList,j,PyFloat_FromDouble(pos[j]));
        PyList_SetItem(output,i,tmpList);
    }

    if(PyErr_Occurred()!=NULL)
    {
        PyErr_SetString(PyRunTimeErr, "error generating pathcontrolpt output");
    }

     return output;
}


