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
#include "sv_PolyData.h"
#include "vtkSmartPointer.h"
// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif
// Globals:
// --------

#include "sv2_globals.h"

using sv3::Contour;
using sv3::PathElement;

#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC  initpyContour();
#endif
#if PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC  PyInit_pyContour();
#endif
PyObject* PyRunTimeErr;
PyObject* Contour_NewObjectCmd( pyContour* self, PyObject* args);
PyObject* Contour_GetObjectCmd( pyContour* self, PyObject* args);
PyObject* Contour_CreateCmd( pyContour* self, PyObject* args);
PyObject* Contour_GetAreaCmd( pyContour* self, PyObject* args);
PyObject* Contour_GetPerimeterCmd( pyContour* self, PyObject* args);
PyObject* Contour_GetCenterPointCmd(pyContour* self, PyObject* args);
PyObject* Contour_SetControlPointsCmd(pyContour* self, PyObject* args);
PyObject* Contour_SetControlPointsByRadiusCmd(pyContour* self, PyObject* args);
PyObject* Contour_SetThresholdValueCmd(pyContour* self, PyObject* args);
PyObject* Contour_SetImageCmd(pyContour* self, PyObject* args);
PyObject* Contour_GetPolyDataCmd(pyContour* self, PyObject* args);


pyContour* Contour_CreateSmoothContour(pyContour* self, PyObject* args);

PyObject* Contour_SetKernelCmd( PyObject* self, PyObject *args);

 
int Contour_pyInit()
{
#if PYTHON_MAJOR_VERSION == 2
  initpyContour();
#endif
#if PYTHON_MAJOR_VERSION == 3
  PyInit_pyContour();
#endif
  return SV_OK;
}

static PyMethodDef pyContour_methods[]={
  {"NewObject", (PyCFunction)Contour_NewObjectCmd,METH_VARARGS,NULL},
  {"GetObject", (PyCFunction)Contour_GetObjectCmd,METH_VARARGS,NULL},
  {"Create", (PyCFunction)Contour_CreateCmd,METH_NOARGS,NULL},
  {"Area", (PyCFunction)Contour_GetAreaCmd,METH_NOARGS,NULL},
  {"Perimeter", (PyCFunction)Contour_GetPerimeterCmd,METH_NOARGS,NULL},
  {"Center", (PyCFunction)Contour_GetCenterPointCmd, METH_NOARGS,NULL},
  {"SetCtrlPts", (PyCFunction)Contour_SetControlPointsCmd, METH_VARARGS, NULL},
  {"SetCtrlPtsByRadius", (PyCFunction)Contour_SetControlPointsByRadiusCmd, METH_VARARGS, NULL},
  {"SetThresholdValue", (PyCFunction)Contour_SetThresholdValueCmd, METH_VARARGS, NULL},
  {"CreateSmoothCt", (PyCFunction)Contour_CreateSmoothContour, METH_VARARGS, NULL},
  {"SetImg", (PyCFunction)Contour_SetImageCmd, METH_VARARGS,NULL},
  {"GetPolyData", (PyCFunction)Contour_GetPolyDataCmd, METH_VARARGS,NULL},
  {NULL,NULL}
};

static int pyContour_init(pyContour* self, PyObject* args)
{
  fprintf(stdout,"pyContour initialized.\n");
  return SV_OK;
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

static PyTypeObject pyContourFactoryRegistrarType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  "pyContour.pyContourFactoryRegistrar",             /* tp_name */
  sizeof(pyContourFactoryRegistrar),             /* tp_basicsize */
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
  "pyContourFactoryRegistrar wrapper  ",           /* tp_doc */
};

static PyMethodDef pyContourModule_methods[] =
{
    {"SetContourKernel", (PyCFunction)Contour_SetKernelCmd, METH_VARARGS,NULL},
    {NULL,NULL}
};

#if PYTHON_MAJOR_VERSION == 3
static struct PyModuleDef pyContourModule = {
   PyModuleDef_HEAD_INIT,
   "pyContour",   /* name of module */
   "", /* module documentation, may be NULL */
   -1,       /* size of per-interpreter state of the module,
                or -1 if the module keeps state in global variables. */
   pyContourModule_methods
};
#endif


//----------------
//initpyContour
//----------------
#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC initpyContour()

{
  // Associate the mesh registrar with the python interpreter so it can be
  // retrieved by the DLLs.
  if (gRepository==NULL)
  {
    gRepository = new cvRepository();
    fprintf(stdout,"New gRepository created from sv3_Contour_init\n");
  }

  Contour::gCurrentKernel = cKERNEL_INVALID;
  //if (PySys_SetObject("ContourObjectRegistrar",(PyObject*)&Contour::gRegistrar)<0)
  //{
  //  fprintf(stdout,"Unable to create ContourObjectRegistrar");
  //  return;
  //}
  // Initialize
  pyContourType.tp_new=PyType_GenericNew;
  pyContourFactoryRegistrarType.tp_new = PyType_GenericNew;
  if (PyType_Ready(&pyContourType)<0)
  {
    fprintf(stdout,"Error in pyContourType\n");
    return;
  }
  if (PyType_Ready(&pyContourFactoryRegistrarType)<0)
  {
    fprintf(stdout,"Error in pyContourFactoryRegistrarType\n");
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
  Py_INCREF(&pyContourFactoryRegistrarType);
  PyModule_AddObject(pythonC,"pyContour",(PyObject*)&pyContourType);
  PyModule_AddObject(pythonC, "pyContourFactoryRegistrar", (PyObject *)&pyContourFactoryRegistrarType);
  
  pyContourFactoryRegistrar* tmp = PyObject_New(pyContourFactoryRegistrar, &pyContourFactoryRegistrarType);
  tmp->registrar = (cvFactoryRegistrar *)&Contour::gRegistrar;
  PySys_SetObject("ContourObjectRegistrar", (PyObject *)tmp);
  
  return ;

}
#endif

#if PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC PyInit_pyContour()

{
  // Associate the mesh registrar with the python interpreter so it can be
  // retrieved by the DLLs.
  if (gRepository==NULL)
  {
    gRepository = new cvRepository();
    fprintf(stdout,"New gRepository created from sv3_Contour_init\n");
  }

  Contour::gCurrentKernel = cKERNEL_INVALID;
  //if (PySys_SetObject("ContourObjectRegistrar",(PyObject*)&Contour::gRegistrar)<0)
  //{
  //  fprintf(stdout,"Unable to create ContourObjectRegistrar");
  //  return;
  //}
  // Initialize
  pyContourType.tp_new=PyType_GenericNew;
  pyContourFactoryRegistrarType.tp_new = PyType_GenericNew;
  if (PyType_Ready(&pyContourType)<0)
  {
    fprintf(stdout,"Error in pyContourType\n");
    return SV_PYTHON_ERROR;
  }
  if (PyType_Ready(&pyContourFactoryRegistrarType)<0)
  {
    fprintf(stdout,"Error in pyContourFactoryRegistrarType\n");
    return SV_PYTHON_ERROR;
  }
  PyObject* pythonC;
  pythonC = PyModule_Create(&pyContourModule);
  if(pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyContour\n");
    return SV_PYTHON_ERROR;
  }
  PyRunTimeErr = PyErr_NewException("pyContour.error",NULL,NULL);
  PyModule_AddObject(pythonC,"error",PyRunTimeErr);
  Py_INCREF(&pyContourType);
  Py_INCREF(&pyContourFactoryRegistrarType);
  PyModule_AddObject(pythonC,"pyContour",(PyObject*)&pyContourType);
  PyModule_AddObject(pythonC, "pyContourFactoryRegistrar", (PyObject *)&pyContourFactoryRegistrarType);
  
  pyContourFactoryRegistrar* tmp = PyObject_New(pyContourFactoryRegistrar, &pyContourFactoryRegistrarType);
  tmp->registrar = (cvFactoryRegistrar *)&Contour::gRegistrar;
  PySys_SetObject("ContourObjectRegistrar", (PyObject *)tmp);
  
  return pythonC;

}
#endif
// ------------------
// Contour_SetKernelCmd
// ------------------

PyObject* Contour_SetKernelCmd( PyObject* self, PyObject *args)
{
    char *kernelName;
    cKernelType kernel;
    if(!PyArg_ParseTuple(args,"s",&kernelName))
    {
        PyErr_SetString(PyRunTimeErr,"Could not import char kernelName");
        
    }
    
    // Do work of command:
    if (strcmp( kernelName, "LevelSet" )==0 ) {
        kernel= cKERNEL_LEVELSET;
    } else if (strcmp( kernelName, "Threshold")==0){
        kernel=cKERNEL_THRESHOLD;
    } else if (strcmp( kernelName, "Circle" )==0 ) {
        kernel= cKERNEL_CIRCLE;
    } else if (strcmp( kernelName, "Polygon" )==0 ) {
        kernel= cKERNEL_POLYGON;
    } else if (strcmp( kernelName, "SplinePolygon" )==0 ) {
        kernel= cKERNEL_SPLINEPOLYGON;
    } else if (strcmp( kernelName, "Ellipse")==0 ) {
        kernel= cKERNEL_ELLIPSE;
    } else {
        kernel= cKERNEL_INVALID;
    }
    
    if ( kernel != cKERNEL_INVALID ) {
        Contour::gCurrentKernel = kernel;
        return Py_BuildValue("s",kernelName);
    } else {
        PyErr_SetString(PyRunTimeErr, "contour kernel is invalid");
        
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
    if (!PyArg_ParseTuple(args,"ssi", &objName, &pathName, &index))
    {
        PyErr_SetString(PyRunTimeErr, "Could not import 2 chars, and \
                    1 int: objName, pathName, and index");
        
    }
  
  // Make sure the specified result object does not exist:
    if ( gRepository->Exists( objName ) ) {
        PyErr_SetString(PyRunTimeErr, "object already exists.");
        
    }
  
    // Do work of command:
    // Retrieve source object:
    rd = gRepository->GetObject( pathName );
    char r[2048];
    if ( rd == NULL )
    {
        r[0] = '\0';
        sprintf(r, "couldn't find object %s", pathName);
        PyErr_SetString(PyRunTimeErr,r);
        
    }
    
    type = rd->GetType();
    
    if ( type != PATH_T )
    {
        r[0] = '\0';
        sprintf(r, "%s not a path object", pathName);
        PyErr_SetString(PyRunTimeErr,r);
        
    }
    
    path = dynamic_cast<PathElement*> (rd);
    
    int numPathPts = path->GetPathPointNumber();
    if (index>=numPathPts)
    {
        PyErr_SetString(PyRunTimeErr, "Index exceed number of path points");
        
    }

    // Instantiate the new contour:
    Contour *geom = sv3::Contour::DefaultInstantiateContourObject(Contour::gCurrentKernel, path->GetPathPoint(index));
    
    if(geom==NULL)
    {
        PyErr_SetString(PyRunTimeErr, "Failed to create contour object");
        
    }
    // Register the contour:
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
// Contour_SetImageCmd
// --------------------
PyObject* Contour_SetImageCmd( pyContour* self, PyObject* args)
{
    char *pathName=NULL;
    vtkImageData *vtkObj;
    int index=0;
    RepositoryDataT type;
    cvRepositoryData *rd;
    PyObject *vtkName; 
    Contour* contour = self->geom;
    if (!PyArg_ParseTuple(args,"O", &vtkName))
    {
        PyErr_SetString(PyRunTimeErr, "Could not import 1 vtkImgae");
        
    }
  
  // Make sure the specified result object does not exist:
    if ( contour==NULL ) {
        PyErr_SetString(PyRunTimeErr, "Contour object not created");
        
    }
  
    // Do work of command:
    // Look up the named vtk object:
    vtkObj = (vtkImageData *)vtkPythonUtil::GetPointerFromObject( vtkName,
        "vtkImageData");
    if ( vtkObj == NULL )
    {
        PyErr_SetString(PyRunTimeErr, "error retrieving vtkImage object ");
        
    }

    vtkImageData*  slice = sv3::SegmentationUtils::GetSlicevtkImage(contour->GetPathPoint(),vtkObj, 5.0);
    contour->SetVtkImageSlice(slice);
    
    Py_INCREF(contour);
    self->geom=contour;
    Py_DECREF(contour);
    return SV_PYTHON_OK; 

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

  if ( type != CONTOUR_T )
  {
    r[0] = '\0';
    sprintf(r, "%s not a contour object", objName);
    PyErr_SetString(PyRunTimeErr,r);
    
  }
  
  contour = dynamic_cast<Contour*> (rd);
  Py_INCREF(contour);
  self->geom=contour;
  Py_DECREF(contour);
  return SV_PYTHON_OK; 
}

// --------------------
// Contour_SetControlPointsCmd
// --------------------
PyObject* Contour_SetControlPointsCmd( pyContour* self, PyObject* args)
{
    PyObject *ptList=NULL;

    if (!PyArg_ParseTuple(args,"O", &ptList))
    {
        PyErr_SetString(PyRunTimeErr, "Could not import one list and one optional list or double\
            , center boundary and radius");
        
    }

  // Do work of command:
    int numPts = PyList_Size(ptList);
    
    if (Contour::gCurrentKernel==cKERNEL_CIRCLE)
    {
        if(numPts!=2)
        {
            PyErr_SetString(PyRunTimeErr, "Circle contour requires two points, center and boundary");
            
        }
    }
    else if (Contour::gCurrentKernel==cKERNEL_ELLIPSE)
    {
        if(numPts!=3)
        {
            PyErr_SetString(PyRunTimeErr, "Ellipse contour requires three points, center and two boudaries");
            
        }
    }
    else if (Contour::gCurrentKernel==cKERNEL_POLYGON)
    {
        if(numPts<3)
        {
            PyErr_SetString(PyRunTimeErr, "Polygon contour requires at least three points");
            
        }
    }        
    
    Contour* contour = self->geom;
    if ( contour==NULL ) {
        PyErr_SetString(PyRunTimeErr, "Contour object not created");
        
    }
    
    std::vector<std::array<double,3> > pts(numPts);
    for (int i = 0; i<numPts; i++)
    {
        PyObject* tmpList = PyList_GetItem(ptList,i);
        if(PyList_Size(tmpList)!=3)
        {
            PyErr_SetString(PyRunTimeErr, "The length of double list must be 3");
            
        }
        for (int j = 0;j<3;j++)
        {
            pts[i][j] = PyFloat_AsDouble(PyList_GetItem(tmpList,j));
        }
    }
    contour->SetControlPoints(pts);
    return SV_PYTHON_OK; 
}

PyObject* Contour_SetControlPointsByRadiusCmd(pyContour* self, PyObject* args)
{
    PyObject *center;
    double radius = -1.;
    if (Contour::gCurrentKernel==cKERNEL_CIRCLE)
    {
        if (!PyArg_ParseTuple(args,"Od", &center, &radius ))
        {
            PyErr_SetString(PyRunTimeErr, "Could not import one list and one double\
            , center and radius");
            
        }
    }
    else
    {
        PyErr_SetString(PyRunTimeErr, "Kernel method does not support this function");
        
    }
  // Do work of command:
    double ctr[3];
    if(PyList_Size(center)!=3)
    {
        PyErr_SetString(PyRunTimeErr, "The length of double list must be 3");
        
    }
    for (int i = 0;i<PyList_Size(center);i++)
    {
        ctr[i] = PyFloat_AsDouble(PyList_GetItem(center,i));
    }
    
    Contour* contour = self->geom;
    if ( contour==NULL ) {
        PyErr_SetString(PyRunTimeErr, "Contour object not created");
        
    }
    if (radius>0.)
    {
        contour->SetControlPointByRadius(radius,ctr);
    }
    else
    {
        PyErr_SetString(PyRunTimeErr, "Must provide either a point on the \
            circle or a positive radius value");
        

    }
    return SV_PYTHON_OK; 
}


// --------------------
// Contour_CreateCmd
// --------------------
PyObject* Contour_CreateCmd( pyContour* self, PyObject* args)
{
    Contour* contour = self->geom;
    if ( contour==NULL ) {
        PyErr_SetString(PyRunTimeErr, "Contour object not created");
        
    }
    if (Contour::gCurrentKernel==cKERNEL_LEVELSET)
    {
        sv3::levelSetContour::svLSParam paras;
        contour->SetLevelSetParas(&paras);
        //std::cout<<"paras: "<<paras.radius<<std::endl;
    }
    
    contour->CreateContourPoints();
    if(contour->GetContourPointNumber()==0)
    {
        PyErr_SetString(PyRunTimeErr, "Error creating contour points");
        
    }
    
    Py_INCREF(contour);
    self->geom=contour;
    Py_DECREF(contour);
    return SV_PYTHON_OK; 
    
}

// --------------------
// Contour_GetAreaCmd
// --------------------
PyObject* Contour_GetAreaCmd( pyContour* self, PyObject* args)
{
    Contour* contour = self->geom;
    if ( contour==NULL ) {
        PyErr_SetString(PyRunTimeErr, "Contour object not created");
        
    }
    double area = contour->GetArea();
    return Py_BuildValue("d",area);    
}

// --------------------
// Contour_GetPerimeterCmd
// --------------------
PyObject* Contour_GetPerimeterCmd( pyContour* self, PyObject* args)
{
    Contour* contour = self->geom;
    if ( contour==NULL ) {
        PyErr_SetString(PyRunTimeErr, "Contour object not created");
        
    }
    double perimeter = contour->GetPerimeter();
    return Py_BuildValue("d",perimeter);    
}

// --------------------
// Contour_GetPerimeterCmd
// --------------------
PyObject* Contour_GetCenterPointCmd( pyContour* self, PyObject* args)
{
    Contour* contour = self->geom;
    if ( contour==NULL ) {
        PyErr_SetString(PyRunTimeErr, "Contour object not created");
        
    }
    std::array<double,3> center = contour->GetCenterPoint();
    char output[1024];
    output[0] = '\0';
    sprintf(output,"(%.4f,%.4f,%.4f)",center[0],center[1],center[2]);
    return Py_BuildValue("s",output);    
}

// ----------------------------
// Contour_SetThresholdValueCmd
// ----------------------------
PyObject* Contour_SetThresholdValueCmd(pyContour* self, PyObject* args)
{
    double threshold = 0.;
    if (!PyArg_ParseTuple(args,"d", &threshold))
    {
        PyErr_SetString(PyRunTimeErr, "Could not import double, threshold");
        
    }
    
    if (Contour::gCurrentKernel!=cKERNEL_THRESHOLD)
    {
        PyErr_SetString(PyRunTimeErr, "Contour type is not threshold");
        
    }
    
    Contour* contour = self->geom;
    if ( contour==NULL ) {
        PyErr_SetString(PyRunTimeErr, "Contour object not created");
        
    }
    contour->SetThresholdValue(threshold);
    return SV_PYTHON_OK;
}
    
//========================
// Contour_CreateSmoothContour
//=========================
pyContour* Contour_CreateSmoothContour(pyContour* self, PyObject* args)
{
    int fourierNumber = 0;
    char* contourName;
    if (!PyArg_ParseTuple(args,"is", &fourierNumber,&contourName))
    {
        PyErr_SetString(PyRunTimeErr, "Could not import int and one char, fourierNumber, contourName");
        
    }
    
    Contour* contour = self->geom;
    if ( contour==NULL ) {
        PyErr_SetString(PyRunTimeErr, "Contour object not created");
        
    }
    
    Contour *newContour = sv3::Contour::DefaultInstantiateContourObject(Contour::gCurrentKernel, contour->GetPathPoint());
    
    newContour= contour->CreateSmoothedContour(fourierNumber);
    
    // Register the contour:
    if ( !( gRepository->Register( contourName, newContour ) ) ) {
        PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
        delete newContour;
        
    }
        
    Py_INCREF(newContour);
    pyContour* pyNewCt;
    pyNewCt = PyObject_New(pyContour, &pyContourType);
    pyNewCt->geom=newContour;
    Py_DECREF(newContour);
    return pyNewCt;
    
}

//========================
// Contour_GetPolyDataCmd
//=========================
PyObject* Contour_GetPolyDataCmd(pyContour* self, PyObject* args)
{
    char* dstName=NULL;
    if (!PyArg_ParseTuple(args,"s", &dstName))
    {
        PyErr_SetString(PyRunTimeErr, "Could not import char, dstName");
        
    }
    
    // Make sure the specified result object does not exist:
    if ( gRepository->Exists( dstName ) ) {
        PyErr_SetString(PyRunTimeErr, "object already exists");
        
    }
  
    Contour* geom = self->geom;
    if ( geom==NULL ) {
        PyErr_SetString(PyRunTimeErr, "Contour object not created");
        
    }
    vtkSmartPointer<vtkPolyData> vtkpd = geom->CreateVtkPolyDataFromContour();
    
    cvPolyData* pd = new cvPolyData(vtkpd);
    
    if (pd==NULL)
    {
        PyErr_SetString(PyRunTimeErr, "Could not get polydata from object");
        
    }
    
      // Register the result:
    if ( !( gRepository->Register( dstName, pd ) ) ) {
      PyErr_SetString(PyRunTimeErr, "error registering obj in repository" );
      delete pd;
      
    }
    
    return SV_PYTHON_OK;
}
    
