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
#include "Python.h"

#include "sv4_PathElement.h"
#include "sv4_PathElement_init_py.h"

#include <stdio.h>
#include <string.h>
#include <array>
#include <iostream>
#include "cvRepository.h"
#include "cvRepositoryData.h"
// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif
// Globals:
// --------

#include "cv_globals.h"

PyMODINIT_FUNC initpyPath();
PyObject* PyRunTimeErr;
PyObject* sv4Path_NewObjectCmd( pyPath* self, PyObject* args);
PyObject* sv4Path_AddPointCmd( pyPath* self, PyObject* args);
PyObject* sv4Path_PrintCtrlPointCmd( pyPath* self, PyObject* args);
PyObject* sv4Path_RemovePointCmd( pyPath* self, PyObject* args);
PyObject* sv4Path_MoveCtrlPointCmd( pyPath* self, PyObject* args);
PyObject* sv4Path_SmoothPathCmd(pyPath* self, PyObject* args);
PyObject* sv4Path_CreatePathCmd(pyPath* self, PyObject* args);


int Path_pyInit()
{
  initpyPath();
  return Py_OK;
}

static PyMethodDef pyPath_methods[]={
  {"path_newObject", (PyCFunction)sv4Path_NewObjectCmd,METH_VARARGS,NULL},
  {"path_addPoint",(PyCFunction)sv4Path_AddPointCmd,METH_VARARGS,NULL},
  {"path_printPoints",(PyCFunction)sv4Path_PrintCtrlPointCmd, METH_NOARGS,NULL},
  {"path_removePoint",(PyCFunction)sv4Path_RemovePointCmd,METH_VARARGS,NULL},
  {"path_movePoint",(PyCFunction)sv4Path_MoveCtrlPointCmd,METH_VARARGS,NULL},
  {"path_smooth",(PyCFunction)sv4Path_SmoothPathCmd, METH_VARARGS,NULL},
  {"path_createPath",(PyCFunction)sv4Path_CreatePathCmd, METH_NOARGS,NULL},
  {NULL,NULL}
};

static int pyPath_init(pyPath* self, PyObject* args)
{
  fprintf(stdout,"pyPath initialized.\n");
  return Py_OK;
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

//----------------
//initpyPath
//----------------
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
    return Py_ERROR;
  }
  
   // Do work of command:

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists.");
    return Py_ERROR;
  }

  // Instantiate the new mesh:
  sv4PathElement *geom = new sv4PathElement();

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
// sv4Path_AddPointCmd
// --------------------

PyObject* sv4Path_AddPointCmd( pyPath* self, PyObject* args)
{

    PyObject* pyList;
    int index=-2;
    if(!PyArg_ParseTuple(args,"O|i",&pyList,&index))
    {
        PyErr_SetString(PyRunTimeErr,"Could not import list");
        return Py_ERROR;
    }
    
    sv4PathElement* path = self->geom;
    if (path==NULL)
    {
        PyErr_SetString(PyRunTimeErr,"Path does not exist.");
        return Py_ERROR;
    }
    
    std::array<double,3> point;
    point[0] = PyFloat_AsDouble(PyList_GetItem(pyList,0));
    point[1] = PyFloat_AsDouble(PyList_GetItem(pyList,1));
    point[2] = PyFloat_AsDouble(PyList_GetItem(pyList,2));    
    
    if(path->SearchControlPoint(point,0)!=-2)
    {
        PyErr_SetString(PyRunTimeErr,"Point already exists");
        return Py_ERROR;
    }

    if (index!=-2)
    {
        if(index>(path->GetControlPoints()).size())
        {
            PyErr_SetString(PyRunTimeErr,"Index exceeds path length");
            return Py_ERROR;
        }
    }
    else
        index=path->GetInsertintIndexByDistance(point);
    path->InsertControlPoint(index,point);

        
    Py_INCREF(path);
    self->geom=path;
    Py_DECREF(path);
    Py_RETURN_NONE; 
    
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
        return Py_ERROR;
    }
    
    
    sv4PathElement* path = self->geom;
    if (path==NULL)
    {
        PyErr_SetString(PyRunTimeErr,"Path does not exist.");
        return Py_ERROR;
    }
    
    if(index>=(path->GetControlPoints()).size())
    {
        PyErr_SetString(PyRunTimeErr,"Index exceeds path length");
        Py_ERROR;
    }
    
    path->RemoveControlPoint(index);
    
    Py_INCREF(path);
    self->geom=path;
    Py_DECREF(path);
    Py_RETURN_NONE; 
    
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
        return Py_ERROR;
    }
    
    sv4PathElement* path = self->geom;
    if (path==NULL)
    {
        PyErr_SetString(PyRunTimeErr,"Path does not exist.");
        return Py_ERROR;
    }
    
    std::array<double,3> point;
    point[0] = PyFloat_AsDouble(PyList_GetItem(pyList,0));
    point[1] = PyFloat_AsDouble(PyList_GetItem(pyList,1));
    point[2] = PyFloat_AsDouble(PyList_GetItem(pyList,2));    
    
    if(index>=(path->GetControlPoints()).size())
    {
        PyErr_SetString(PyRunTimeErr,"Index exceeds path length");
        Py_ERROR;
    }


    path->SetControlPoint(index,point);

        
    Py_INCREF(path);
    self->geom=path;
    Py_DECREF(path);
    Py_RETURN_NONE; 
    
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
        return Py_ERROR;
    }
    
    sv4PathElement* path = self->geom;
    if (path==NULL)
    {
        PyErr_SetString(PyRunTimeErr,"Path does not exist.");
        return Py_ERROR;
    }  
    
    bool controlPointsBasedBool=controlPointsBased==1?true:false;
    
    path = path->CreateSmoothedPathElement(sampleRate,numModes,controlPointsBasedBool);
        
    Py_INCREF(path);
    self->geom=path;
    Py_DECREF(path);
    Py_RETURN_NONE; 
    
}

// --------------------
// sv4Path_PrintCtrlPointCmd
// --------------------

PyObject* sv4Path_PrintCtrlPointCmd( pyPath* self, PyObject* args)
{
    sv4PathElement* path = self->geom;
    std::vector<std::array<double,3> > pts = path->GetControlPoints();
    for (int i=0;i<pts.size();i++)
    {
        std::array<double,3> pt = pts[i];
        PySys_WriteStdout("Point %i, %f, %f, %f \n",i, pt[0],pt[1],pt[2]);
    }
    
    Py_RETURN_NONE; 
}

// --------------------
// sv4Path_CreatePathCmd
// --------------------
PyObject* sv4Path_CreatePathCmd(pyPath* self, PyObject* args)
{
    sv4PathElement* path = self->geom;
    if (path==NULL)
    {
        PyErr_SetString(PyRunTimeErr,"Path does not exist.");
        return Py_ERROR;
    }  
    path->CreatePathPoints();
    int num = (path->GetPathPoints()).size();
    if (num==0)
    {
        PyErr_SetString(PyRunTimeErr,"Error creating path from control points");
        return Py_ERROR;
    }
    else
        printf("Total number of path points created is: %i \n", num);
        
    Py_RETURN_NONE;
}