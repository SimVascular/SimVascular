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
#include "sv_Repository.h"
#include "sv_solid_init_py.h"
#include "sv_SolidModel.h"
#include "sv_arg.h"
#include "sv_misc_utils.h"
#include "sv_vtk_utils.h"
#include "sv_PolyData.h"
#include "sv_PolyDataSolid.h"
#include "sv_sys_geom.h"

#include "sv_FactoryRegistrar.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#include "sv2_globals.h"
#include "Python.h"
#include <structmember.h>
#include "vtkPythonUtil.h"
#if PYTHON_MAJOR_VERSION == 3
#include "PyVTKObject.h"
#elif PYTHON_MAJOR_VERSION == 2
#include "PyVTKClass.h"
#endif

#include "sv_occt_init_py.h"
#include "sv_polydatasolid_init_py.h"

//Python intialization functions. Called from python interpreter
#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC initpySolid2(void);
#elif PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC PyInit_pySolid2(void);
#endif
int Solid_pyInit()
{
  //Py_Initialize();
#if PYTHON_MAJOR_VERSION == 2
 initpySolid2();
#elif PYTHON_MAJOR_VERSION == 3
 PyInit_pySolid2();
#endif
  return Py_OK;
}

typedef struct
{
  PyObject_HEAD
  cvSolidModel* geom;
}pySolidModel;

static void pySolidModel_dealloc(pySolidModel* self)
{
  Py_XDECREF(self->geom);
  Py_TYPE(self)->tp_free((PyObject*)self);
}
// Prototypes:
// -----------
// Solid
// -----
PyObject* PyRunTimeErr;
PyObject* Solid_RegistrarsListCmd( PyObject* self, PyObject* args);

PyObject* Solid_GetModelCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_PolyCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_PolyPtsCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_CircleCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_EllipseCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_Box2dCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_Box3dCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_SphereCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_EllipsoidCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_CylinderCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_TruncatedConeCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_TorusCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_Poly3dSolidCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_Poly3dSurfaceCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_ExtrudeZCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_ExtrudeCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_MakeApproxCurveLoopCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_MakeInterpCurveLoopCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_MakeLoftedSurfCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_CapSurfToSolidCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_IntersectCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_UnionCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_SubtractCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_ReadNativeCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_CopyCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_ListMethodsCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_NewObjectCmd( pySolidModel* self, PyObject* args);

PyObject* Solid_SetKernelCmd( PyObject* self, PyObject* args);

PyObject* Solid_GetKernelCmd( PyObject* self, PyObject* args);

//PyObject* Solid_PrintKernelInfoCmd( pySolidModel* self, PyObject* args);

//#ifdef SV_USE_PYTHON
//PyObject* Solid_InitPyModulesCmd( PyObject* self, PyObject* args);
//#endif


// Solid object methods
// --------------------
static PyObject* Solid_GetClassNameMtd(pySolidModel* self, PyObject* args);

static PyObject* Solid_FindExtentMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_FindCentroidMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_GetTopoDimMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_GetSpatialDimMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_ClassifyPtMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_DistanceMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_GetFaceNormalMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_TranslateMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_RotateMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_ScaleMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_ReflectMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_Apply4x4Mtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_PrintMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_CheckMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_WriteNativeMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_WriteVtkPolyDataMtd( pySolidModel* self,
				      PyObject* args);

static PyObject* Solid_WriteGeomSimMtd( pySolidModel* self,
				      PyObject* args);

static PyObject* Solid_GetFacePolyDataMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_GetPolyDataMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_SetVtkPolyDataMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_GetDiscontinuitiesMtd( pySolidModel* self,
					PyObject* args);

static pySolidModel* Solid_GetAxialIsoparametricCurveMtd( pySolidModel* self,
						PyObject* args);

static PyObject* Solid_GetKernelMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_GetFaceIdsMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_GetBoundaryFacesMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_GetRegionIdsMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_GetFaceAttrMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_SetFaceAttrMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_GetRegionAttrMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_SetRegionAttrMtd( pySolidModel* self, PyObject* args);

  // Label-related methods
  // ---------------------

static PyObject* Solid_GetLabelKeysMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_GetLabelMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_SetLabelMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_ClearLabelMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_DeleteFacesMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_DeleteRegionMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_CreateEdgeBlendMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_CombineFacesMtd( pySolidModel* self, PyObject* args);

static PyObject* Solid_RemeshFaceMtd( pySolidModel* self, PyObject* args);
// Helper functions
// ----------------

static void PrintMethods();


static int pySolidModel_init(pySolidModel* self, PyObject* args)
{
  fprintf(stdout,"pySolid Model tp_init called\n");
  return Py_OK;
}

//All functions listed and initiated as pySolid_methods declared here
static PyMemberDef pySolidModel_members[]={
{NULL}
};
// --------------------
// pySolid_methods
// --------------------
static PyMethodDef pySolidModel_methods[]={
  { "solid_getModel", (PyCFunction)Solid_GetModelCmd, METH_VARARGS, NULL},
  { "solid_poly",(PyCFunction) Solid_PolyCmd,
		     METH_VARARGS,NULL},
  { "solid_polyPts", (PyCFunction)Solid_PolyPtsCmd,
		     METH_VARARGS,NULL},
  { "solid_circle", (PyCFunction)Solid_CircleCmd,
		     METH_VARARGS,NULL},
  { "solid_ellipse", (PyCFunction)Solid_EllipseCmd,
		     METH_VARARGS,NULL},
  { "solid_box2d", (PyCFunction)Solid_Box2dCmd,
		     METH_VARARGS,NULL},
  { "solid_box3d", (PyCFunction)Solid_Box3dCmd,
		     METH_VARARGS,NULL},
  { "solid_sphere", (PyCFunction)Solid_SphereCmd,
		     METH_VARARGS,NULL},
  { "solid_ellipsoid", (PyCFunction)Solid_EllipsoidCmd,
		     METH_VARARGS,NULL},
  { "solid_cylinder", (PyCFunction)Solid_CylinderCmd,
		     METH_VARARGS,NULL},
  { "solid_truncatedCone", (PyCFunction)Solid_TruncatedConeCmd,
		     METH_VARARGS,NULL},
  { "solid_torus", (PyCFunction)Solid_TorusCmd,
		     METH_VARARGS,NULL},
  { "solid_poly3dSolid", (PyCFunction)Solid_Poly3dSolidCmd,
		     METH_VARARGS,NULL},
  { "solid_poly3dSurface", (PyCFunction)Solid_Poly3dSurfaceCmd,
		     METH_VARARGS,NULL},
  { "solid_extrudeZ", (PyCFunction)Solid_ExtrudeZCmd,
		     METH_VARARGS,NULL},
  { "solid_extrude", (PyCFunction)Solid_ExtrudeCmd,
		     METH_VARARGS,NULL},
  { "solid_makeApproxCurveLoop",
		     (PyCFunction)Solid_MakeApproxCurveLoopCmd,
		     METH_VARARGS,NULL},
  { "solid_makeInterpCurveLoop",
		     (PyCFunction)Solid_MakeInterpCurveLoopCmd,
		     METH_VARARGS,NULL},
  { "solid_makeLoftedSurf", (PyCFunction)Solid_MakeLoftedSurfCmd,
		     METH_VARARGS,NULL},
  { "solid_capSurfToSolid", (PyCFunction)Solid_CapSurfToSolidCmd,
		     METH_VARARGS,NULL},
  { "solid_intersect", (PyCFunction)Solid_IntersectCmd,
		     METH_VARARGS,NULL},
  { "solid_union", (PyCFunction)Solid_UnionCmd,
		     METH_VARARGS,NULL},
  { "solid_subtract", (PyCFunction)Solid_SubtractCmd,
		     METH_VARARGS,NULL},
  { "solid_readNative", (PyCFunction)Solid_ReadNativeCmd,
		     METH_VARARGS,NULL},
  { "solid_copy", (PyCFunction)Solid_CopyCmd,
		     METH_VARARGS,NULL},
  { "solid_methods", (PyCFunction)Solid_ListMethodsCmd,
		     METH_NOARGS,NULL},
  { "solid_newObject", (PyCFunction)Solid_NewObjectCmd,
		     METH_VARARGS,NULL},
  { "GetClassName", (PyCFunction)Solid_GetClassNameMtd,
		     METH_NOARGS,NULL},
  { "FindExtent", (PyCFunction)Solid_FindExtentMtd,
		     METH_VARARGS,NULL},
  { "FindCentroid",(PyCFunction)Solid_FindCentroidMtd,
		     METH_VARARGS,NULL},
  { "GetTopoDim",(PyCFunction)Solid_GetTopoDimMtd,
		     METH_VARARGS,NULL},
  { "GetSpatialDim",(PyCFunction)Solid_GetSpatialDimMtd,
		     METH_VARARGS,NULL},
  { "ClassifyPt",(PyCFunction)Solid_ClassifyPtMtd,
		     METH_VARARGS,NULL},
  { "DeleteFaces",(PyCFunction)Solid_DeleteFacesMtd,
		     METH_VARARGS,NULL},
  { "DeleteRegion",(PyCFunction)Solid_DeleteRegionMtd,
		     METH_VARARGS,NULL},
  { "CreateEdgeBlend",(PyCFunction)Solid_CreateEdgeBlendMtd,
		     METH_VARARGS,NULL},
  { "CombineFaces",(PyCFunction)Solid_CombineFacesMtd,
		     METH_VARARGS,NULL},
  { "RemeshFace",(PyCFunction)Solid_RemeshFaceMtd,
		     METH_VARARGS,NULL},
  { "Distance",(PyCFunction)Solid_DistanceMtd,
		     METH_VARARGS,NULL},
  { "GetFaceNormal",(PyCFunction)Solid_GetFaceNormalMtd,
		     METH_VARARGS,NULL},
  { "Translate",(PyCFunction)Solid_TranslateMtd,
		     METH_VARARGS,NULL},
  { "Rotate",(PyCFunction)Solid_RotateMtd,
		     METH_VARARGS,NULL},
  { "Scale",(PyCFunction)Solid_ScaleMtd,
		     METH_VARARGS,NULL},
  { "Reflect",(PyCFunction)Solid_ReflectMtd,
		     METH_VARARGS,NULL},
  { "Apply4x4",(PyCFunction)Solid_Apply4x4Mtd,
		     METH_VARARGS,NULL},
  { "Print", (PyCFunction)Solid_PrintMtd,
		     METH_VARARGS,NULL},
  { "Check", (PyCFunction)Solid_CheckMtd,
		     METH_VARARGS,NULL},
  { "WriteNative",(PyCFunction)Solid_WriteNativeMtd,
		     METH_VARARGS,NULL},
  { "WriteVtkPolyData",(PyCFunction)Solid_WriteVtkPolyDataMtd,
		     METH_VARARGS,NULL},
  { "WriteGeomSim", (PyCFunction)Solid_WriteGeomSimMtd,
		     METH_VARARGS,NULL},
  { "GetFacePolyData",(PyCFunction)Solid_GetFacePolyDataMtd,
		     METH_VARARGS,NULL},
  { "GetPolyData",(PyCFunction)Solid_GetPolyDataMtd,
		     METH_VARARGS,NULL},
  { "SetVtkPolyData",(PyCFunction)Solid_SetVtkPolyDataMtd,
		     METH_VARARGS,NULL},
  { "GetDiscontinuities",(PyCFunction)Solid_GetDiscontinuitiesMtd,
		     METH_VARARGS,NULL},
  { "GetAxialIsoparametricCurve",(PyCFunction)Solid_GetAxialIsoparametricCurveMtd,
		     METH_VARARGS,NULL},
  { "GetKernel",(PyCFunction)Solid_GetKernelMtd,
		     METH_VARARGS,NULL},
  { "GetLabelKeys",(PyCFunction)Solid_GetLabelKeysMtd,
		     METH_VARARGS,NULL},
  { "GetLabel", (PyCFunction)Solid_GetLabelMtd,
		     METH_VARARGS,NULL},
  { "SetLabel",(PyCFunction)Solid_SetLabelMtd,
		     METH_VARARGS,NULL},
  { "ClearLabel", (PyCFunction)Solid_ClearLabelMtd,
		     METH_VARARGS,NULL},
  { "GetFaceIds", (PyCFunction)Solid_GetFaceIdsMtd,
		     METH_NOARGS,NULL},
  { "GetBoundaryFaces",(PyCFunction)Solid_GetBoundaryFacesMtd,
		     METH_VARARGS,NULL},
  { "GetRegionIds",(PyCFunction)Solid_GetRegionIdsMtd,
		     METH_VARARGS,NULL},
  { "GetFaceAttr",(PyCFunction)Solid_GetFaceAttrMtd,
		     METH_VARARGS,NULL},
  { "SetFaceAttr",(PyCFunction)Solid_SetFaceAttrMtd,
		     METH_VARARGS,NULL},
  { "GetRegionAttr",(PyCFunction)Solid_GetRegionAttrMtd,
		     METH_VARARGS,NULL},
  { "SetRegionAttr", (PyCFunction)Solid_SetRegionAttrMtd,
		     METH_VARARGS,NULL},
  {NULL,NULL}
};
static PyTypeObject pySolidModelType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  "pySolid2.pySolidModel",             /* tp_name */
  sizeof(pySolidModel),             /* tp_basicsize */
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
  "pySolidModel  objects",           /* tp_doc */
  0,                         /* tp_traverse */
  0,                         /* tp_clear */
  0,                         /* tp_richcompare */
  0,                         /* tp_weaklistoffset */
  0,                         /* tp_iter */
  0,                         /* tp_iternext */
  pySolidModel_methods,             /* tp_methods */
  0,                         /* tp_members */
  0,                         /* tp_getset */
  0,                         /* tp_base */
  0,                         /* tp_dict */
  0,                         /* tp_descr_get */
  0,                         /* tp_descr_set */
  0,                         /* tp_dictoffset */
  (initproc)pySolidModel_init,                            /* tp_init */
  0,                         /* tp_alloc */
  0,                  /* tp_new */
};
static PyMethodDef pySolid2_methods[] = {
  {"solid_registrars", (PyCFunction)Solid_RegistrarsListCmd,METH_NOARGS,NULL},
  { "solid_setKernel", (PyCFunction)Solid_SetKernelCmd,
		     METH_VARARGS,NULL},
  { "solid_getKernel", (PyCFunction)Solid_GetKernelCmd,
		     METH_NOARGS,NULL},
  {NULL, NULL}
};

static PyTypeObject pycvFactoryRegistrarType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  "pySolid2.pycvFactoryRegistrar",             /* tp_name */
  sizeof(pycvFactoryRegistrar),             /* tp_basicsize */
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
  "cvFactoryRegistrar wrapper  ",           /* tp_doc */
};

#if PYTHON_MAJOR_VERSION == 3
static struct PyModuleDef pySolid2module = {
   PyModuleDef_HEAD_INIT,
   "pySolid2",   /* name of module */
   "", /* module documentation, may be NULL */
   -1,       /* size of per-interpreter state of the module,
                or -1 if the module keeps state in global variables. */
   pySolid2_methods
};
#endif

#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC
initpySolid2(void)
{
    // Initialize-gRepository
  if (gRepository ==NULL)
  {
    gRepository=new cvRepository();
    fprintf(stdout,"New gRepository created from cv_solid_init\n");
  }
  //Initialize-gCurrentKernel
  cvSolidModel::gCurrentKernel = SM_KT_INVALID;
  #ifdef SV_USE_PARASOLID
  cvSolidModel::gCurrentKernel = SM_KT_PARASOLID;
  #endif

  pySolidModelType.tp_new=PyType_GenericNew;
  pycvFactoryRegistrarType.tp_new = PyType_GenericNew;
  if (PyType_Ready(&pySolidModelType)<0)
  {
    fprintf(stdout,"Error in pySolidModelType");
    return;
  }
  if (PyType_Ready(&pycvFactoryRegistrarType)<0)
  {
    fprintf(stdout,"Error in pySolidModelType");
    return;
  }
  //Init our defined functions
  PyObject *pythonC;
  pythonC = Py_InitModule("pySolid2", pySolid2_methods);
  if (pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pySolid");
    return;
  }

  PyRunTimeErr=PyErr_NewException("pySolid2.error",NULL,NULL);
  PyModule_AddObject(pythonC, "error",PyRunTimeErr);
  Py_INCREF(&pySolidModelType);
  Py_INCREF(&pycvFactoryRegistrarType);
  PyModule_AddObject(pythonC, "pySolidModel", (PyObject *)&pySolidModelType);
  PyModule_AddObject(pythonC, "pyCvFactoryRegistrar", (PyObject *)&pycvFactoryRegistrarType);

  pycvFactoryRegistrar* tmp = PyObject_New(pycvFactoryRegistrar, &pycvFactoryRegistrarType);
  tmp->registrar = (cvFactoryRegistrar *)&cvSolidModel::gRegistrar;
  PySys_SetObject("solidModelRegistrar", (PyObject *)tmp);


}
#endif

#if PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC
PyInit_pySolid2(void)
{
    // Initialize-gRepository
  if (gRepository ==NULL)
  {
    gRepository=new cvRepository();
    fprintf(stdout,"New gRepository created from cv_solid_init\n");
  }
  //Initialize-gCurrentKernel
  cvSolidModel::gCurrentKernel = SM_KT_INVALID;
  #ifdef SV_USE_PARASOLID
  cvSolidModel::gCurrentKernel = SM_KT_PARASOLID;
  #endif

  pySolidModelType.tp_new=PyType_GenericNew;
  pycvFactoryRegistrarType.tp_new = PyType_GenericNew;
  if (PyType_Ready(&pySolidModelType)<0)
  {
    fprintf(stdout,"Error in pySolidModelType");
    Py_RETURN_NONE;
  }
  if (PyType_Ready(&pycvFactoryRegistrarType)<0)
  {
    fprintf(stdout,"Error in pySolidModelType");
    Py_RETURN_NONE;
  }
  //Init our defined functions
  PyObject *pythonC;
  pythonC = PyModule_Create(&pySolid2module);
  if (pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pySolid");
    Py_RETURN_NONE;
  }

  PyRunTimeErr=PyErr_NewException("pySolid2.error",NULL,NULL);
  PyModule_AddObject(pythonC, "error",PyRunTimeErr);
  Py_INCREF(&pySolidModelType);
  Py_INCREF(&pycvFactoryRegistrarType);
  PyModule_AddObject(pythonC, "pySolidModel", (PyObject *)&pySolidModelType);
  PyModule_AddObject(pythonC, "pyCvFactoryRegistrar", (PyObject *)&pycvFactoryRegistrarType);

  pycvFactoryRegistrar* tmp = PyObject_New(pycvFactoryRegistrar, &pycvFactoryRegistrarType);
  tmp->registrar = (cvFactoryRegistrar *)&cvSolidModel::gRegistrar;
  PySys_SetObject("solidModelRegistrar", (PyObject *)tmp);
  return pythonC;

}

#endif
/*#ifdef SV_USE_PYTHON
//Must be called after the python interpreter is initiated and through
//the tcl interprter. i.e. PyInterprter exec {tcl.eval("initPyMods")
// --------------------
// Solid_InitPyModules
// --------------------
PyObject* Solid_InitPyModulesCmd( PyObject* self, PyObject* args)
{
  //Import vtk
  PyObject *vtkstring = PyString_FromString("vtk");
  PyObject *vtkmodule = PyImport_Import(vtkstring);
  PyModule_AddObject(PyImport_AddModule("__buildin__"), "vtk", vtkmodule);

  //Init our defined functions
  PyObject *pythonC;
  pythonC = PyImport_ImportModule("pySolid2");
  Py_INCREF(pythonC);
  PyModule_AddObject(PyImport_AddModule("__buildin__"), "pySolid2", pythonC);

  return Py_BuildValue("s","success");
}
#endif*/

// This routine is used for debugging the registrar/factory system.
PyObject* Solid_RegistrarsListCmd(PyObject* self, PyObject* args)
{
  cvFactoryRegistrar* pySolidModelRegistrar =(cvFactoryRegistrar *) PySys_GetObject("solidModelRegistrar");
  char result[255];
  PyObject* pyList=PyList_New(6);
  sprintf( result, "Solid model registrar ptr -> %p\n", pySolidModelRegistrar );
  PyList_SetItem(pyList,0,PyString_FromFormat(result));
  for (int i = 0; i < CV_MAX_FACTORY_METHOD_PTRS; i++) {
    sprintf( result, "GetFactoryMethodPtr(%i) = %p\n",
      i, (pySolidModelRegistrar->GetFactoryMethodPtr(i)));
    PyList_SetItem(pyList,i+1,PyString_FromFormat(result));
  }
  return pyList;
}

// -----------------
// Solid_GetModelCmd
// -----------------

PyObject* Solid_GetModelCmd( pySolidModel* self, PyObject* args)
{
  char *objName=NULL;
  RepositoryDataT type;
  cvRepositoryData *rd;
  cvSolidModel *geom;

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

  if ( type != SOLID_MODEL_T )
  {
    r[0] = '\0';
    sprintf(r, "%s not a model object", objName);
    PyErr_SetString(PyRunTimeErr,r);
    return Py_ERROR;
  }
  
  geom = dynamic_cast<cvSolidModel*> (rd);
  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  Py_RETURN_NONE; 
  
}
// ----------------
// Solid_PolyPtsCmd
// ----------------

// Previously (in shapesPkg.cxx) we had maintained a Tcl_HashTable of
// all solid objects.  This table was used to look up other objects
// for object methods which required not only the "this" object (whose
// pointer is retured by the clientData mechanism), but also
// additional operands which were named by their Tcl names.  These
// objects were looked-up in the Tcl_HashTable by that name to
// retrieve additional object pointers.

// Now, since we're using the cvRepository mechanism (which is itself a
// Tcl_HashTable), we can use the cvRepository's lookup mechanisms to
// find those operands.  That is, we can call cvRepository's
// GetObject(name) method to get back object pointers for use inside
// Tcl object method functions.

PyObject* Solid_PolyPtsCmd( pySolidModel* self, PyObject* args)
{
  char *srcName;
  char *dstName;
  cvRepositoryData *pd;
  RepositoryDataT type;
  cvSolidModel *geom;
  if(!PyArg_ParseTuple(args,"ss",&srcName,&dstName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import two chars.");
    return Py_ERROR;
  }
  // Do work of command:

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists" );
    return Py_ERROR;
  }

  // Retrieve cvPolyData source:
  pd = gRepository->GetObject( srcName );
  if ( pd == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object " );
    return Py_ERROR;
  }
  type = pd->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "object not of type cvPolyData" );
    return Py_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( geom == NULL ) {
    return Py_ERROR;
  }

  // Create the polygon solid:
  if ( geom->MakePoly2dPts( (cvPolyData *)pd ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "polygon solid creation error" );
    return Py_ERROR;
  }

  // Register the solid:
  if ( !( gRepository->Register( dstName, geom ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete geom;
    return Py_ERROR;
  }

  // Make a new Tcl command:
  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);

  Py_RETURN_NONE;
}


// -------------
// Solid_PolyCmd
// -------------

PyObject* Solid_PolyCmd( pySolidModel* self, PyObject* args)
{
  char *srcName;
  char *dstName;
  cvRepositoryData *pd;
  RepositoryDataT type;
  cvSolidModel *geom;
  if(!PyArg_ParseTuple(args,"ss",&srcName,&dstName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import two chars.");
    return Py_ERROR;
  }

  // Do work of command:

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    return Py_ERROR;
  }

  // Retrieve cvPolyData source:
  pd = gRepository->GetObject( srcName );
  if ( pd == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object ");
    return Py_ERROR;
  }
  type = pd->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "object not of type cvPolyData" );
    return Py_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( geom == NULL ) {
    return Py_ERROR;
  }

  // Create the polygon solid:
  if ( geom->MakePoly2d( (cvPolyData *)pd ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "polygon solid creation error" );
    return Py_ERROR;
  }

  // Register the solid:
  if ( !( gRepository->Register( dstName, geom ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete geom;
    return Py_ERROR;
  }

  // Make a new Tcl command:

  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  Py_RETURN_NONE;
}


// ---------------
// Solid_CircleCmd
// ---------------

// % solid_circle -result /some/obj/name -r <radius> -x <x_ctr> -y <y_ctr>
PyObject* Solid_CircleCmd(pySolidModel* self, PyObject* args)
{
  char *objName;
  double radius;
  double ctr[2];
  cvSolidModel* geom;
  if(!PyArg_ParseTuple(args,"sddd",&objName,&radius,&(ctr[0]),&(ctr[1])))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char and three doubles");
    return Py_ERROR;
  }
  // Do work of command:

  if ( radius <= 0.0 ) {
    PyErr_SetString(PyRunTimeErr,"radius must be positive");
    return Py_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    PyErr_SetString(PyRunTimeErr,"object already exists" );
    return Py_ERROR ;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( geom == NULL ) {
    return Py_ERROR;
  }

  if ( geom->MakeCircle( radius, ctr ) != SV_OK ) {
     PyErr_SetString(PyRunTimeErr, "circle solid creation error");
    delete geom;
    return Py_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( objName, geom) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete geom;
    return Py_ERROR;
  }
  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);

  Py_RETURN_NONE;
}

// ---------------
// Solid_SphereCmd
// ---------------
PyObject* Solid_SphereCmd( pySolidModel* self, PyObject* args)
{
  char *objName;
  PyObject* ctrList;
  double ctr[3];
  double r;
  cvSolidModel* geom;
if(!PyArg_ParseTuple(args,"sdO",&objName,&r,&ctrList))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char, one double and one list");
    return Py_ERROR;
  }

  if (PyList_Size(ctrList)!=3)
  {
    PyErr_SetString(PyRunTimeErr,"sphere requires a 3D center coordinate");
    return Py_ERROR;
  }
  for(int i=0;i<PyList_Size(ctrList);i++)
  {
    ctr[i]=PyFloat_AsDouble(PyList_GetItem(ctrList,i));
  }
  // Do work of command:

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    return Py_ERROR;
  }
  // Instantiate the new solid:
  geom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( geom == NULL ) {
    return Py_ERROR;
  }
  if ( geom->MakeSphere( r, ctr ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "sphere solid creation error");
    delete geom;
    return Py_ERROR;
  }
  // Register the new solid:
  if ( !( gRepository->Register( objName, geom) ) ) {
     PyErr_SetString(PyRunTimeErr,"error registering obj in repository" );
    delete geom;
    return Py_ERROR;
  }

  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  //self->name=objName;
  //std::cout<<"self "<<self->geom<<"geom "<<geom<<std::endl;
 // cvRepositoryData* geom2=gRepository->GetObject(self->name);
  //cvPolyData* PD2=(self->geom)->GetPolyData(0,1.0);
 // return Py_BuildValue("s",geom->GetName());
  Py_RETURN_NONE;
}
// ----------------
// Solid_EllipseCmd
// ----------------

PyObject* Solid_EllipseCmd( pySolidModel* self, PyObject* args)
{
  char *objName;
  double xr, yr;
  double ctr[2];
  cvSolidModel *geom;

  if(!PyArg_ParseTuple(args,"sdddd",&objName,&xr,&yr,&(ctr[0]),&(ctr[1])))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char and four doubles.");
    return Py_ERROR;
  }

  // Do work of command:

  if ( ( xr <= 0.0 ) || ( yr <= 0.0 ) ) {
    PyErr_SetString(PyRunTimeErr, "radii must be positive");
    return Py_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    return Py_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( geom == NULL ) {
    return Py_ERROR;
  }

  if ( geom->MakeEllipse( xr, yr, ctr ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "ellipse solid creation error" );
    delete geom;
    return Py_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( objName, geom ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository" );
    delete geom;
    return Py_ERROR;
  }

  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  Py_RETURN_NONE;
}


// --------------
// Solid_Box2dCmd
// --------------

// % solid_box2d -result /some/obj/name -h <double> -w <double> \
//       -xctr <double> -yctr <double>

PyObject* Solid_Box2dCmd( pySolidModel* self, PyObject* args)
{
  char *objName;
  double boxDims[2];
  double ctr[2];
  cvSolidModel *geom;
  if(!PyArg_ParseTuple(args,"sdddd",&objName,&(boxDims[0]),&(boxDims[1])
                       ,&(ctr[0]),&(ctr[1])))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char and four doubles.");
    return Py_ERROR;
  }
  // Do work of command:

  if ( ( boxDims[0] <= 0.0 ) || ( boxDims[1] <= 0.0 ) ) {
    PyErr_SetString(PyRunTimeErr, "height and width must be positive");
    return Py_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    return Py_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( geom == NULL ) {
    return Py_ERROR;
  }

  if ( geom->MakeBox2d( boxDims, ctr ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "box solid creation error");
    delete geom;
    return Py_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( objName, geom ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository" );
    delete geom;
    return Py_ERROR;
  }

  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  Py_RETURN_NONE;
}


// --------------
// Solid_Box3dCmd
// --------------

PyObject* Solid_Box3dCmd( pySolidModel* self, PyObject* args)
{
  char *objName;
  double dims[3];
  double ctr[3];
  PyObject* dimList;
  PyObject* ctrList;
  cvSolidModel *geom;
  if(!PyArg_ParseTuple(args,"sOO",&objName,&dimList,&ctrList))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char and two doubles.");
    return Py_ERROR;
  }

  if (PyList_Size(dimList)>3||PyList_Size(ctrList)>3)
  {
     PyErr_SetString(PyRunTimeErr,"error in list dimension");
     return Py_ERROR;
  }
  for (int i=0;i<PyList_Size(dimList);i++)
  {
    dims[i]=PyFloat_AsDouble(PyList_GetItem(dimList,i));
  }
  for (int i=0;i<PyList_Size(ctrList);i++)
  {
    ctr[i]=PyFloat_AsDouble(PyList_GetItem(ctrList,i));
  }
  // Do work of command:

  if ( ( dims[0] <= 0.0 ) || ( dims[1] <= 0.0 ) || ( dims[2] <= 0.0 ) ) {
    PyErr_SetString(PyRunTimeErr, "all dims must be positive" );
    return Py_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exist" );
    return Py_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( geom == NULL ) {
    return Py_ERROR;
  }

  if ( geom->MakeBox3d( dims, ctr ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "box solid creation error" );
    delete geom;
    return Py_ERROR;
  }

  // Register the new solid:
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


// ------------------
// Solid_EllipsoidCmd
// ------------------

PyObject* Solid_EllipsoidCmd( pySolidModel* self, PyObject* args)
{
  char *objName;
  double ctr[3];
  double r[3];
  PyObject* rList;
  PyObject* ctrList;
  cvSolidModel *geom;
  if(!PyArg_ParseTuple(args,"sOO",&objName,&rList,&ctrList))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char and two doubles.");
    return Py_ERROR;
  }
  // Do work of command:

  if (PyList_Size(ctrList)!=3)
  {
     PyErr_SetString(PyRunTimeErr,"ellipsoid requires a 3D center coordinate");
     return Py_ERROR;
  }
  if (PyList_Size(rList)!=3)
  {
     PyErr_SetString(PyRunTimeErr,"ellipsoid requires a 3D radius vector.");
    return Py_ERROR;
  }

  for (int i=0;i<3;i++)
  {
    r[i]=PyFloat_AsDouble(PyList_GetItem(rList,i));
    ctr[i]=PyFloat_AsDouble(PyList_GetItem(ctrList,i));
  }
  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists" );
    return Py_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( geom == NULL ) {
    return Py_ERROR;
  }

  if ( geom->MakeEllipsoid( r, ctr ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "sphere solid creation error" );
    delete geom;
    return Py_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( objName, geom ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository" );
    delete geom;
    return Py_ERROR;
  }

  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  Py_RETURN_NONE;
}


// -----------------
// Solid_CylinderCmd
// -----------------

PyObject* Solid_CylinderCmd( pySolidModel* self, PyObject* args)
{
  char *objName;
  double ctr[3];
  double axis[3];
  double r, l;
  int nctr, naxis=0;
  PyObject* ctrList;
  PyObject* axisList;
  cvSolidModel *geom;

  if(!PyArg_ParseTuple(args,"sddOO",&objName,&r,&l,&ctrList,&axisList))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char, two doubles and two lists");
    return Py_ERROR;
  }
  // Do work of command:

  if (PyList_Size(ctrList)!=3)
  {
     PyErr_SetString(PyRunTimeErr,"cylinder requires a 3D center coordinate");
     return Py_ERROR;
  }
  if (PyList_Size(axisList)!=3)
  {
     PyErr_SetString(PyRunTimeErr,"ellipsoid requires a 3D axis vector.");
    return Py_ERROR;
  }

  for (int i=0;i<3;i++)
  {
    axis[i]=PyFloat_AsDouble(PyList_GetItem(axisList,i));
    ctr[i]=PyFloat_AsDouble(PyList_GetItem(ctrList,i));
  }
  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists" );
    return Py_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( geom == NULL ) {
    return Py_ERROR;
  }

  if ( geom->MakeCylinder( r, l, ctr, axis ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "cylinder solid creation error" );
    delete geom;
    return Py_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( objName, geom ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository" );
    delete geom;
    return Py_ERROR;
  }

  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  Py_RETURN_NONE;
}


// ----------------------
// Solid_TruncatedConeCmd
// ----------------------

PyObject* Solid_TruncatedConeCmd( pySolidModel* self, PyObject* args)
{
  char *objName;
  double pt[3];
  double dir[3];
  double r1, r2;
  PyObject* ptList;
  PyObject* dirList;
  cvSolidModel *geom;

  if(!PyArg_ParseTuple(args,"sddOO",&objName,&r1,&r2,&ptList,&dirList))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char,two doubles and two lists.");
    return Py_ERROR;
  }
  // Do work of command:

  if (PyList_Size(ptList)!=3)
  {
     PyErr_SetString(PyRunTimeErr,"truncatedCone requires a 3D coordinate");
     return Py_ERROR;
  }
  if (PyList_Size(dirList)!=3)
  {
     PyErr_SetString(PyRunTimeErr,"truncatedCone requires a 3D direction vector.");
    return Py_ERROR;
  }

  for (int i=0;i<3;i++)
  {
    pt[i]=PyFloat_AsDouble(PyList_GetItem(ptList,i));
    dir[i]=PyFloat_AsDouble(PyList_GetItem(dirList,i));
  }
  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists" );
    return Py_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( geom == NULL ) {
    return Py_ERROR;
  }

  if ( geom->MakeTruncatedCone( pt, dir, r1, r2 ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "cylinder solid creation error" );
    delete geom;
    return Py_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( objName, geom ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository" );
    delete geom;
    return Py_ERROR;
  }
  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  Py_RETURN_NONE;
}


// --------------
// Solid_TorusCmd
// --------------

PyObject* Solid_TorusCmd( pySolidModel* self, PyObject* args)
{
  char *objName;
  double ctr[3];
  double axis[3];
  PyObject* ctrList;
  PyObject* axisList;
  double rmaj, rmin;
  int nctr, naxis=0;
  cvSolidModel *geom;
  if(!PyArg_ParseTuple(args,"sddOO",&objName,&rmaj,&rmin,&ctrList,&axisList))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one char,two doubles and two lists.");
    return Py_ERROR;
  }

  // Do work of command:
  if (PyList_Size(ctrList)!=3)
  {
     PyErr_SetString(PyRunTimeErr,"torus requires a 3D center coordinate");
     return Py_ERROR;
  }
  if (PyList_Size(axisList)!=3)
  {
     PyErr_SetString(PyRunTimeErr,"ellipsoid requires a 3D axis vector.");
    return Py_ERROR;
  }

  for (int i=0;i<3;i++)
  {
    axis[i]=PyFloat_AsDouble(PyList_GetItem(axisList,i));
    ctr[i]=PyFloat_AsDouble(PyList_GetItem(ctrList,i));
  }
  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    return Py_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( geom == NULL ) {
    return Py_ERROR;
  }

  if ( geom->MakeTorus( rmaj, rmin, ctr, axis ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "torus solid creation error" );
    delete geom;
    return Py_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( objName, geom ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository" );
    delete geom;
    return Py_ERROR;
  }

  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  Py_RETURN_NONE;
}


// --------------------
// Solid_Poly3dSolidCmd
// --------------------

PyObject* Solid_Poly3dSolidCmd( pySolidModel* self, PyObject* args)
{
  char *objName;
  char *srcName;
  char *facetMethodName;
  char *facetStr;
  SolidModel_FacetT facetMethod;
  cvRepositoryData *pd;
  RepositoryDataT type;
  cvSolidModel *geom;
  double angle = 0.0;

  if(!PyArg_ParseTuple(args,"sssd",&objName,&srcName,&facetMethodName,&angle))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import three char and one double.");
    return Py_ERROR;
  }

  facetMethod = SolidModel_FacetT_StrToEnum( facetMethodName );
  if ( facetMethod == SM_Facet_Invalid ) {
    facetStr = SolidModel_FacetT_EnumToStr( SM_Facet_Invalid );
    PyErr_SetString(PyRunTimeErr, facetStr);
    return Py_ERROR;
  }

  // Do work of command:

  // Retrieve cvPolyData source:
  pd = gRepository->GetObject( srcName );
  if ( pd == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object " );
    return Py_ERROR;
  }
  type = pd->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "object not of type cvPolyData" );
    return Py_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists" );
    return Py_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( geom == NULL ) {
    return Py_ERROR;
  }

  if ( geom->SetPoly3dFacetMethod( facetMethod ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error selecting facet method ");
    delete geom;
    return Py_ERROR;
  }
  if ( geom->MakePoly3dSolid( (cvPolyData*)pd , angle ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "polygonal solid creation error" );
    delete geom;
    return Py_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( objName, geom ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository" );
    delete geom;
    return Py_ERROR;
  }

  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  Py_RETURN_NONE;
}


// ----------------------
// Solid_Poly3dSurfaceCmd
// ----------------------

PyObject* Solid_Poly3dSurfaceCmd( pySolidModel* self, PyObject* args)
{
  char *objName;
  char *srcName;
  char *facetMethodName;
  char *facetStr;
  SolidModel_FacetT facetMethod;
  cvRepositoryData *pd;
  RepositoryDataT type;
  cvSolidModel *geom;

  if(!PyArg_ParseTuple(args,"sss",&objName,&srcName,&facetMethodName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import three chars.");
    return Py_ERROR;
  }

  facetMethod = SolidModel_FacetT_StrToEnum( facetMethodName );
  if ( facetMethod == SM_Facet_Invalid ) {
    facetStr = SolidModel_FacetT_EnumToStr( SM_Facet_Invalid );
    PyErr_SetString(PyRunTimeErr, facetStr );
    return Py_ERROR;
  }

  // Do work of command:

  // Retrieve cvPolyData source:
  pd = gRepository->GetObject( srcName );
  if ( pd == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object ");
    return Py_ERROR;
  }
  type = pd->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "object not of type cvPolyData");
    return Py_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    PyErr_SetString(PyRunTimeErr, "object  already exists" );
    return Py_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( geom == NULL ) {
    return Py_ERROR;
  }

  if ( geom->SetPoly3dFacetMethod( facetMethod ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error selecting facet method " );
    delete geom;
    return Py_ERROR;
  }
  if ( geom->MakePoly3dSurface( (cvPolyData*)pd ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "solid polygonal surface creation error");
    delete geom;
    return Py_ERROR;
  }

  // Register the new solid:
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


// -----------------
// Solid_ExtrudeZCmd
// -----------------

PyObject* Solid_ExtrudeZCmd( pySolidModel* self, PyObject* args)
{
  char *srcName;
  char *dstName;
  double dist;
  cvRepositoryData *src;
  RepositoryDataT type;
  cvSolidModel *geom;
  if(!PyArg_ParseTuple(args,"ssd",&srcName,&dstName,&dist))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import two chars and one double.");
    return Py_ERROR;
  }
  // Do work of command:

  // Retrieve cvSolidModel source:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object ");
    return Py_ERROR;
  }
  type = src->GetType();
  if ( type != SOLID_MODEL_T ) {
    PyErr_SetString(PyRunTimeErr, "object not of type cvSolidModel" );
    return Py_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    return Py_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( geom == NULL ) {
    return Py_ERROR;
  }

  if ( geom->ExtrudeZ( (cvSolidModel *)src, dist ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error in solid extrusion" );
    delete geom;
    return Py_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( dstName, geom ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository" );
    delete geom;
    return Py_ERROR;
  }

  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  Py_RETURN_NONE;
}


// ----------------
// Solid_ExtrudeCmd
// ----------------

PyObject* Solid_ExtrudeCmd( pySolidModel* self, PyObject* args)
{
  char *srcName;
  char *dstName;
  double pt1[3],pt2[3];
  PyObject* pt1List;
  PyObject* pt2List;
  double **dist;
  cvRepositoryData *src;
  RepositoryDataT type;
  cvSolidModel *geom;

  if(!PyArg_ParseTuple(args,"ssOO",&srcName,&dstName,&pt1List,&pt2List))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import two strings and two lists");
    return Py_ERROR;
  }

  // Do work of command:

  if (PyList_Size(pt1List)>3||PyList_Size(pt2List)>3)
  {
     PyErr_SetString(PyRunTimeErr,"error in list dimension ");
     return Py_ERROR;
  }

  for (int i=0;i<PyList_Size(pt1List);i++)
  {
    pt1[i]=PyFloat_AsDouble(PyList_GetItem(pt1List,i));
  }
  for (int i=0;i<PyList_Size(pt2List);i++)
  {
    pt2[i]=PyFloat_AsDouble(PyList_GetItem(pt2List,i));
  }
  // Retrieve cvSolidModel source:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object ");
    return Py_ERROR;
  }
  type = src->GetType();
  if ( type != SOLID_MODEL_T ) {
    PyErr_SetString(PyRunTimeErr, "object not of type cvSolidModel" );
    return Py_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    return Py_ERROR;
  }

  dist = new double*[2];
  dist[0] = &pt1[0];
  dist[1] = &pt2[0];

  // Instantiate the new solid:
  geom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( geom == NULL ) {
    delete dist;
    return Py_ERROR;
  }

  if ( geom->Extrude( (cvSolidModel *)src, dist ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error in solid extrusion" );
    delete geom;
    delete dist;
    return Py_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( dstName, geom ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository" );
    delete geom;
    delete dist;
    return Py_ERROR;
  }

  delete dist;
  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  Py_RETURN_NONE;
}


// ----------------------------
// Solid_MakeApproxCurveLoopCmd
// ----------------------------

PyObject* Solid_MakeApproxCurveLoopCmd( pySolidModel* self, PyObject* args)
{
  char *srcName;
  char *dstName;
  double tol;
  cvRepositoryData *src;
  RepositoryDataT type;
  cvSolidModel *geom;
  int closed = 1;

  if(!PyArg_ParseTuple(args,"ssdi",&srcName,&dstName,&tol,&closed))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import two chars, one double and one int.");
    return Py_ERROR;
  }

  // Do work of command:

  // Retrieve cvPolyData source:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object ");
    return Py_ERROR;
  }
  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "object not of type cvPolyData" );
    return Py_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object  already exists");
    return Py_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( geom == NULL ) {
    return Py_ERROR;
  }

  if ( geom->MakeApproxCurveLoop( (cvPolyData *)src, tol, closed ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error in curve loop construction" );
    delete geom;
    return Py_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( dstName, geom ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete geom;
    return Py_ERROR;
  }

  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  Py_RETURN_NONE;
}


// ----------------------------
// Solid_MakeInterpCurveLoopCmd
// ----------------------------

PyObject* Solid_MakeInterpCurveLoopCmd( pySolidModel* self, PyObject* args)
{
  char *srcName;
  char *dstName;

  cvRepositoryData *src;
  RepositoryDataT type;
  cvSolidModel *geom;
  int closed = 1;

  if(!PyArg_ParseTuple(args,"ss|i",&srcName,&dstName,&closed))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import two chars and one int");
    return Py_ERROR;
  }
  // Do work of command:

  // Retrieve cvPolyData source:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object ");
    return Py_ERROR;
  }
  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "object not of type cvPolyData");
    return Py_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    return Py_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( geom == NULL ) {
    return Py_ERROR;
  }

  if ( geom->MakeInterpCurveLoop( (cvPolyData *)src, closed ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error in curve loop construction" );
    delete geom;
    return Py_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( dstName, geom ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete geom;
    return Py_ERROR;
  }

  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  Py_RETURN_NONE;
}


// -----------------------
// Solid_MakeLoftedSurfCmd
// -----------------------

PyObject* Solid_MakeLoftedSurfCmd( pySolidModel* self, PyObject* args)
{
  char *dstName;
  cvRepositoryData *src;
  RepositoryDataT type;
  int numSrcs;
  PyObject* srcList;
  cvSolidModel **srcs;
  cvSolidModel *geom;
  int continuity=0;
  int partype=0;
  int smoothing=0;
  double w1=0.4,w2=0.2,w3=0.4;
  int i;

  if(!PyArg_ParseTuple(args,"Os|iidddi",&srcList,&dstName,&continuity,&partype,&w1,&w2,&w3,&smoothing))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one list, one string or optional three ints and three doubles.");
    return Py_ERROR;
  }
  // Do work of command:

  numSrcs = PyList_Size(srcList);

  if ( numSrcs < 2 ) {
    PyErr_SetString(PyRunTimeErr, "need >= 2 curve cvSolidModel's to loft surface");
    return Py_ERROR;
  }

  // Foreach src obj, check that it is in the repository and of the
  // correct type (i.e. cvSolidModel).  Also build up the array of
  // cvSolidModel*'s to pass to cvSolidModel::MakeLoftedSurf.

  srcs = new cvSolidModel * [numSrcs];

  for ( i = 0; i < numSrcs; i++ ) {
    src = gRepository->GetObject( PyString_AsString(PyList_GetItem(srcList,i)) );
    if ( src == NULL ) {
      PyErr_SetString(PyRunTimeErr,"Couldn't find object ");
      delete [] srcs;
      return Py_ERROR;
    }
    type = src->GetType();
    if ( type != SOLID_MODEL_T ) {
	    PyErr_SetString(PyRunTimeErr,"src not of type cvSolidModel");
      delete [] srcs;
      return Py_ERROR;
    }
    srcs[i] = (cvSolidModel *) src;
  }

  // We're done with the src object names:

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    delete [] srcs;
    return Py_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( geom == NULL ) {
    delete [] srcs;
    return Py_ERROR;
  }

  if ( geom->MakeLoftedSurf( srcs, numSrcs , dstName,
	continuity,partype,w1,w2,w3,smoothing) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error in curve loop construction" );
    delete [] srcs;
    delete geom;
    return Py_ERROR;
  }

  // We're done with the srcs array:
  delete [] srcs;

  // Register the new solid:
  if ( !( gRepository->Register( dstName, geom ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository" );
    delete geom;
    return Py_ERROR;
  }

  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  Py_RETURN_NONE;
}


// -----------------------
// Solid_CapSurfToSolidCmd
// -----------------------

PyObject* Solid_CapSurfToSolidCmd( pySolidModel* self, PyObject* args)
{
  char *srcName;
  char *dstName;
  cvRepositoryData *src;
  RepositoryDataT type;
  cvSolidModel *geom;

  if(!PyArg_ParseTuple(args,"ss",&srcName,&dstName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import two chars");
    return Py_ERROR;
  }

  // Do work of command:

  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object ");
    return Py_ERROR;
  }

  type = src->GetType();
  if ( type != SOLID_MODEL_T ) {
    PyErr_SetString(PyRunTimeErr, "object not of type cvSolidModel" );
    return Py_ERROR;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists" );
    return Py_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( geom == NULL ) {
    return Py_ERROR;
  }

  if ( geom->CapSurfToSolid( (cvSolidModel *)src ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error in cap / bound operation" );
    delete geom;
    return Py_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( dstName, geom ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete geom;
    return Py_ERROR;
  }


  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  Py_RETURN_NONE;
}


// -------------------
// Solid_ReadNativeCmd
// -------------------

PyObject* Solid_ReadNativeCmd( pySolidModel* self, PyObject* args)
{
  char *objName, *fileName;

  if(!PyArg_ParseTuple(args,"ss",&objName,&fileName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import two chars");
    return Py_ERROR;
  }
  // Do work of command:

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists" );
    return Py_ERROR;
  }

  // Instantiate the new solid:
  cvSolidModel *geom;
  if (cvSolidModel::gCurrentKernel == SM_KT_PARASOLID ||
      cvSolidModel::gCurrentKernel == SM_KT_DISCRETE ||
      cvSolidModel::gCurrentKernel == SM_KT_POLYDATA ||
      cvSolidModel::gCurrentKernel == SM_KT_OCCT ||
      cvSolidModel::gCurrentKernel == SM_KT_MESHSIMSOLID) {

	  geom = cvSolidModel::pyDefaultInstantiateSolidModel();

	  if ( geom == NULL ) {
	    return Py_ERROR;
	  }

	  if ( geom->ReadNative( fileName ) != SV_OK ) {
	    PyErr_SetString(PyRunTimeErr, "file read error" );
	    delete geom;
	    return Py_ERROR;
	  }

	  // Register the new solid:
	  if ( !( gRepository->Register( objName, geom ) ) ) {
	    PyErr_SetString(PyRunTimeErr, "error registering obj in repository" );
	    delete geom;
	    return Py_ERROR;
	  }

  }

  else {
    fprintf( stdout, "current kernel is not valid (%i)\n",cvSolidModel::gCurrentKernel);
    //PyErr_SetString(PyRunTimeErr, "current kernel is not valid" );
    return Py_ERROR;
  }
  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  Py_RETURN_NONE;
}


// -------------
// Solid_CopyCmd
// -------------

PyObject* Solid_CopyCmd( pySolidModel* self, PyObject* args)
{
  char *srcName;
  char *dstName;
  cvRepositoryData *srcGeom;
  RepositoryDataT src_t;
  cvSolidModel *dstGeom;

  if(!PyArg_ParseTuple(args,"ss",&srcName,&dstName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import two chars");
    return Py_ERROR;
  }

  // Do work of command:

  // Retrieve source:
  srcGeom = gRepository->GetObject( srcName );
  if ( srcGeom == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object ");
    return Py_ERROR;
  }
  src_t = gRepository->GetType( srcName );
  if ( src_t != SOLID_MODEL_T ) {
    PyErr_SetString(PyRunTimeErr, "object not of type cvSolidModel" );
    return Py_ERROR;
  }

  // Make sure the specified destination object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    return Py_ERROR;
  }

  // Instantiate the new solid:
  dstGeom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( dstGeom == NULL ) {
    return Py_ERROR;
  }
  if ( dstGeom->Copy( *((cvSolidModel *)srcGeom) ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "cvSolidModel copy error" );
    delete dstGeom;
    return Py_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( dstName, dstGeom ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete dstGeom;
    return Py_ERROR;
  }

  Py_INCREF(dstGeom);
  self->geom=dstGeom;
  Py_DECREF(dstGeom);
  Py_RETURN_NONE;
}


// ------------------
// Solid_IntersectCmd
// ------------------

PyObject* Solid_IntersectCmd( pySolidModel* self, PyObject* args)
{
  char *resultName;
  char *smpName=NULL;
  char *smpStr;
  char *aName;
  char *bName;
  SolidModel_SimplifyT smp = SM_Simplify_All;  // DEFAULT ARG VALUE
  RepositoryDataT aType, bType;
  cvRepositoryData *gmA;
  cvRepositoryData *gmB;
  cvSolidModel *geom;
  if(!PyArg_ParseTuple(args,"sss|s",&resultName,&aName,&bName,&smpName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import three chars or one optional char smpName");
    return Py_ERROR;
  }


  // Parse the simplification flag if given:
  if (smpName ) {
    smp = SolidModel_SimplifyT_StrToEnum( smpName );
    if ( smp == SM_Simplify_Invalid ) {
      smpStr = SolidModel_SimplifyT_EnumToStr( SM_Simplify_Invalid );
      PyErr_SetString(PyRunTimeErr, smpStr );
      return Py_ERROR;
    }
  }

  // Do work of command:

  // Retrieve cvSolidModel operands:
  gmA = gRepository->GetObject( aName );
  if ( gmA == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object ");
    return Py_ERROR;
  }
  aType = gRepository->GetType( aName );
  if ( aType != SOLID_MODEL_T ) {
    PyErr_SetString(PyRunTimeErr, "object not of type cvSolidModel");
    return Py_ERROR;
  }

  gmB = gRepository->GetObject( bName );
  if ( gmB == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object ");
    return Py_ERROR;
  }
  bType = gRepository->GetType( bName );
  if ( bType != SOLID_MODEL_T ) {
    PyErr_SetString(PyRunTimeErr, "object  not of type cvSolidModel" );
    return Py_ERROR;
  }

  // Instantiate the new solid:
  geom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( geom == NULL ) {
    return Py_ERROR;
  }

  if ( geom->Intersect( (cvSolidModel*)gmA, (cvSolidModel*)gmB, smp ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "intersection error" );
    delete geom;
    return Py_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( resultName, geom ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete geom;
    return Py_ERROR;
  }

  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  Py_RETURN_NONE;
}


// --------------
// Solid_UnionCmd
// --------------

PyObject* Solid_UnionCmd( pySolidModel* self, PyObject* args)
{
  char *resultName;
  char *smpName=NULL;
  char *smpStr;
  char *aName;
  char *bName;
  SolidModel_SimplifyT smp = SM_Simplify_All;  // DEFAULT ARG VALUE
  RepositoryDataT aType, bType;
  cvRepositoryData *gmA;
  cvRepositoryData *gmB;
  cvSolidModel *result;

  if(!PyArg_ParseTuple(args,"sss|s",&resultName,&aName,&bName,&smpName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import three chars or one optional char smpName");
    return Py_ERROR;
  }

  // Parse the simplification flag if given:
  if (smpName) {
    smp = SolidModel_SimplifyT_StrToEnum( smpName );
    if ( smp == SM_Simplify_Invalid ) {
      smpStr = SolidModel_SimplifyT_EnumToStr( SM_Simplify_Invalid );
      PyErr_SetString(PyRunTimeErr, smpStr );
      return Py_ERROR;
    }
  }

  // Do work of command:
  // Retrieve cvSolidModel operands:
  gmA = gRepository->GetObject( aName );
  if ( gmA == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object ");
    return Py_ERROR;
  }
  aType = gRepository->GetType( aName );
  if ( aType != SOLID_MODEL_T ) {
    PyErr_SetString(PyRunTimeErr, "object not of type cvSolidModel");
    return Py_ERROR;
  }

  gmB = gRepository->GetObject( bName );
  if ( gmB == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object ");
    return Py_ERROR;
  }
  bType = gRepository->GetType( bName );
  if ( bType != SOLID_MODEL_T ) {
    PyErr_SetString(PyRunTimeErr, "object not of type cvSolidModel");
    return Py_ERROR;
  }

  // Instantiate the new solid:
  result = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( result == NULL ) {
    return Py_ERROR;
  }
  if ( result->Union( (cvSolidModel*)gmA, (cvSolidModel*)gmB, smp ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "union error" );
    delete result;
    return Py_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( resultName, result ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete result;
    return Py_ERROR;
  }

  Py_INCREF(result);
  self->geom=result;
  Py_DECREF(result);
  Py_RETURN_NONE;
}


// -----------------
// Solid_SubtractCmd
// -----------------

PyObject* Solid_SubtractCmd( pySolidModel* self, PyObject* args)
{
  char *resultName;
  char *smpName=NULL;
  char *smpStr;
  char *aName;
  char *bName;
  SolidModel_SimplifyT smp = SM_Simplify_All;  // DEFAULT ARG VALUE
  RepositoryDataT aType, bType;
  cvRepositoryData *gmA;
  cvRepositoryData *gmB;
  cvSolidModel *result;

  if(!PyArg_ParseTuple(args,"sss|s",&resultName,&aName,&bName,&smpName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import three chars or one optional char smpName");
    return Py_ERROR;
  }

  // Parse the simplification flag if given:
  if (smpName) {
    smp = SolidModel_SimplifyT_StrToEnum( smpName );
    if ( smp == SM_Simplify_Invalid ) {
      smpStr = SolidModel_SimplifyT_EnumToStr( SM_Simplify_Invalid );
      PyErr_SetString(PyRunTimeErr, smpStr );
      return Py_ERROR;
    }
  }

  // Do work of command:

  // Retrieve cvSolidModel operands:
  gmA = gRepository->GetObject( aName );
  if ( gmA == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object ");
    return Py_ERROR;
  }
  aType = gRepository->GetType( aName );
  if ( aType != SOLID_MODEL_T ) {
    PyErr_SetString(PyRunTimeErr, "object not of type cvSolidModel");
    return Py_ERROR;
  }

  gmB = gRepository->GetObject( bName );
  if ( gmB == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object ");
    return Py_ERROR;
  }
  bType = gRepository->GetType( bName );
  if ( bType != SOLID_MODEL_T ) {
    PyErr_SetString(PyRunTimeErr, "object not of type cvSolidModel" );
    return Py_ERROR;
  }

  // Instantiate the new solid:
  result = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( result == NULL ) {
    return Py_ERROR;
  }

  if ( result->Subtract( (cvSolidModel*)gmA, (cvSolidModel*)gmB, smp )
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "subtract error" );
    delete result;
    return Py_ERROR;
  }

  // Register the new solid:
  if ( !( gRepository->Register( resultName, result ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete result;
    return Py_ERROR;
  }

  Py_INCREF(result);
  self->geom=result;
  Py_DECREF(result);
  Py_RETURN_NONE;
}


// --------------------
// Solid_ListMethodsCmd
// --------------------

PyObject* Solid_ListMethodsCmd( pySolidModel* self, PyObject* args)
{
  PrintMethods();
  return Py_BuildValue("s","success");
}


// ---------------
// Solid_ObjectCmd
// ---------------
PyObject* Solid_ObjectCmd(pySolidModel* self,PyObject* args )
{
  if (PyTuple_Size(args)== 0 ) {
    PrintMethods();
    return Py_BuildValue("s","sucssess");
  }
  return Py_BuildValue("s","success");
}

// -----------
// DeleteSolid
// -----------
// This is the deletion call-back for cvSolidModel object commands.

PyObject* DeleteSolid( pySolidModel* self, PyObject* args)
{
  cvSolidModel *geom = self->geom;

  gRepository->UnRegister( geom->GetName() );
  Py_INCREF(Py_None);
  return Py_None;
}

//------------------
//Solid_NewObjectCmd
//------------------
PyObject* Solid_NewObjectCmd(pySolidModel* self,PyObject *args )
{
  char *objName, *fileName;
  cvSolidModel* geom;
  if(!PyArg_ParseTuple(args,"s",&objName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import char objName");
    return Py_ERROR;
  }
  // Do work of command:
  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( objName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exist");
    return Py_ERROR;
  }
  // Instantiate the new solid:

  if (cvSolidModel::gCurrentKernel == SM_KT_PARASOLID ||
      cvSolidModel::gCurrentKernel == SM_KT_DISCRETE ||
      cvSolidModel::gCurrentKernel == SM_KT_POLYDATA ||
      cvSolidModel::gCurrentKernel == SM_KT_OCCT ||
      cvSolidModel::gCurrentKernel == SM_KT_MESHSIMSOLID) {
  geom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( geom == NULL ) {
    return Py_ERROR;
  }

   // Register the new solid:
   if ( !( gRepository->Register( objName, geom ) ) )
   {
     PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete geom;
    return Py_ERROR;
   }
  }
  else {
    fprintf( stdout, "current kernel is not valid (%i)\n",cvSolidModel::gCurrentKernel);
    //PyErr_SetString(PyRunTimeErr, "current kernel is not valid", TCL_STATIC );
    return Py_ERROR;
  }
  // Allocate new object in python
  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  Py_RETURN_NONE;
}

// ------------------
// Solid_SetKernelCmd
// ------------------

PyObject* Solid_SetKernelCmd( PyObject* self, PyObject *args)
{
char *kernelName;
SolidModel_KernelT kernel;
if(!PyArg_ParseTuple(args,"s",&kernelName))
{
  PyErr_SetString(PyRunTimeErr,"Could not import char kernelName");
  return Py_ERROR;
}

// Do work of command:
if (strcmp( kernelName, "Parasolid" )==0 ) {
  kernel= SM_KT_PARASOLID;
} else if (strcmp( kernelName, "Discrete" )==0 ) {
  kernel= SM_KT_DISCRETE;
} else if (strcmp( kernelName, "PolyData" )==0 ) {
  std::cout<<"PolyData"<<std::endl;
  kernel= SM_KT_POLYDATA;
  std::cout<<kernelName<<std::endl;

} else if (strcmp( kernelName, "OpenCASCADE")==0 ) {
  kernel= SM_KT_OCCT;
} else if (strcmp( kernelName, "MeshSimSolid" )==0 ) {
  kernel= SM_KT_MESHSIMSOLID;
} else {
  kernel= SM_KT_INVALID;
}

if ( kernel != SM_KT_INVALID ) {
cvSolidModel::gCurrentKernel = kernel;
std::cout<<cvSolidModel::gCurrentKernel<<std::endl;
return Py_BuildValue("s",kernelName);
} else {
PyErr_SetString(PyRunTimeErr, "solid kernel is invalid");
return Py_ERROR;
}
}
// ------------------
// Solid_GetKernelCmd
// ------------------

PyObject* Solid_GetKernelCmd(PyObject* self, PyObject* args)
{
  char *kernelName;

  kernelName = SolidModel_KernelT_EnumToStr( cvSolidModel::gCurrentKernel );

  return Py_BuildValue("s",kernelName);
}

// ------------
// PrintMethods
// ------------

static void PrintMethods( )
{
  PySys_WriteStdout( "Apply4x4\n");
  PySys_WriteStdout( "Check\n");
  PySys_WriteStdout( "ClassifyPt\n");
  PySys_WriteStdout( "ClearLabel\n");
  PySys_WriteStdout( "DeleteFaces\n");
  PySys_WriteStdout( "DeleteRegion\n");
  PySys_WriteStdout( "CreateEdgeBlend\n");
  PySys_WriteStdout( "CombineFaces\n");
  PySys_WriteStdout( "RemeshFace\n");
  PySys_WriteStdout( "Distance\n");
  PySys_WriteStdout( "FindCentroid\n");
  PySys_WriteStdout( "FindExtent\n");
  PySys_WriteStdout( "GetClassName\n");
  PySys_WriteStdout( "GetDiscontinuities\n");
  PySys_WriteStdout( "GetAxialIsoparametricCurve\n");
  PySys_WriteStdout( "GetFaceAttr\n");
  PySys_WriteStdout( "GetFaceIds\n");
  PySys_WriteStdout( "GetBoundaryFaces\n");
  PySys_WriteStdout( "GetFaceNormal\n");
  PySys_WriteStdout( "GetFacePolyData\n");
  PySys_WriteStdout( "GetKernel\n");
  PySys_WriteStdout( "GetLabel\n");
  PySys_WriteStdout( "GetLabelKeys\n");
  PySys_WriteStdout( "GetPolyData\n");
  PySys_WriteStdout( "SetVtkPolyData\n");
  PySys_WriteStdout( "GetRegionIds\n");
  PySys_WriteStdout( "GetSpatialDim\n");
  PySys_WriteStdout( "GetTopoDim\n");
  PySys_WriteStdout( "Print\n");
  PySys_WriteStdout( "Reflect\n");
  PySys_WriteStdout( "Rotate\n");
  PySys_WriteStdout( "Scale\n");
  PySys_WriteStdout( "SetFaceAttr\n");
  PySys_WriteStdout( "SetLabel\n");
  PySys_WriteStdout( "Translate\n");
  PySys_WriteStdout( "WriteNative\n");
  PySys_WriteStdout( "WriteVtkPolyData\n");
  PySys_WriteStdout( "WriteGeomSim\n");
  return;
}


static PyObject* Solid_GetClassNameMtd(pySolidModel* self, PyObject* args)
{
  return Py_BuildValue("s","SolidModel");
}

// -------------------
// Solid_FindExtentMtd
// -------------------

static PyObject*  Solid_FindExtentMtd( pySolidModel *self ,PyObject* args  )
{
  int status;
  double extent;
  cvSolidModel *geom =(self->geom);
  status = geom->FindExtent( &extent);
  if ( status == SV_OK ) {
    char rtnstr[255];
    rtnstr[0]='\0';
    sprintf( rtnstr, "%f", extent );
    Py_BuildValue("s",rtnstr);
    return Py_BuildValue("s","success");
  } else {
    PyErr_SetString( PyRunTimeErr, "FindExtent: error on object" );
    return Py_ERROR;
  }
}


// ---------------------
// Solid_FindCentroidMtd
// ---------------------

static PyObject*  Solid_FindCentroidMtd( pySolidModel *self ,PyObject* args )
{
  int status;
  double centroid[3];
  char tmp[CV_STRLEN];
  int tdim;
  cvSolidModel *geom =(self->geom);
  if ( geom->GetSpatialDim( &tdim ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "couldn't get spatial dim of object " );
    return Py_ERROR;
  }
  if ( ( tdim != 2 ) && ( tdim != 3 ) ) {
    sprintf( tmp, "spatial dim %d  not supported", tdim );
    PyErr_SetString(PyRunTimeErr,tmp);
    return Py_ERROR;
  }
  status = geom->FindCentroid( centroid );
  PyObject* tmpPy=PyList_New(3);
  if ( status == SV_OK ) {

    PyList_SetItem(tmpPy,0,PyFloat_FromDouble(centroid[0]));
    PyList_SetItem(tmpPy,1,PyFloat_FromDouble(centroid[1]));
    if ( tdim == 3 ) {
    PyList_SetItem(tmpPy,2,PyFloat_FromDouble(centroid[2]));
    }
    return tmpPy;
  } else {
    PyErr_SetString(PyRunTimeErr, "FindCentroid: error on object ");
    return Py_ERROR;
  }
}


// -------------------
// Solid_GetTopoDimMtd
// -------------------

static PyObject*  Solid_GetTopoDimMtd( pySolidModel *self ,PyObject* args  )
{
  int status;
  int tdim;
  cvSolidModel *geom =(self->geom);
  status = geom->GetTopoDim( &tdim );
  if ( status == SV_OK ) {
    return Py_BuildValue("d",tdim);
  } else {
    PyErr_SetString(PyRunTimeErr, "GetTopoDim: error on object ");
    return Py_ERROR;
  }
}


// ----------------------
// Solid_GetSpatialDimMtd
// ----------------------

static PyObject*  Solid_GetSpatialDimMtd( pySolidModel *self ,PyObject* args  )
{
  int status;
  int sdim;
  cvSolidModel *geom =(self->geom);
  status = geom->GetSpatialDim( &sdim );
  if ( status == SV_OK ) {
    return Py_BuildValue("d",sdim);
  } else {
    PyErr_SetString(PyRunTimeErr,"GetSpatialDim: error on object ");
    return Py_ERROR;
  }
}


// -------------------
// Solid_ClassifyPtMtd
// -------------------

static PyObject*  Solid_ClassifyPtMtd( pySolidModel *self ,PyObject* args  )
{
  double x, y, z;
  int v = 0;
  int ans;
  int status;
  int tdim, sdim;
  cvSolidModel *geom =(self->geom);
  if(!PyArg_ParseTuple(args,"dd|di",&x,&y,&z,&v))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import two doubles, x, y, or double z or int v");
    return Py_ERROR;
  }

  // Do work of command:

  geom->GetTopoDim( &tdim );
  geom->GetSpatialDim( &sdim );

  if (z) {
    status = geom->ClassifyPt( x, y, z, v, &ans );

  } else {
    if ( ( tdim == 2 ) && ( sdim == 2 ) ) {
      status = geom->ClassifyPt( x, y, v, &ans );
    } else {
      PyErr_SetString(PyRunTimeErr, "object must be of topological and spatial dimension 2" );
      return Py_ERROR;
    }
  }

  if ( status == SV_OK ) {
    return Py_BuildValue("d",ans);
  } else {
    PyErr_SetString(PyRunTimeErr,"ClassifyPt: error on object " );
    return Py_ERROR;
  }
}


// -----------------
// Solid_DistanceMtd
// -----------------

static PyObject*  Solid_DistanceMtd( pySolidModel *self ,PyObject* args  )
{
  double pos[3];
  int npos;
  double upperLimit, dist;
  int sdim;
  int status;
  PyObject *posList;
  cvSolidModel *geom =(self->geom);
  if(!PyArg_ParseTuple(args,"Od",&posList,&upperLimit))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one list and one double");
    return Py_ERROR;
  }

  if (PyList_Size(posList)>3)
  {
    PyErr_SetString(PyRunTimeErr,"posList Dimension is greater than 3!");
    return Py_ERROR;
  }
  npos = PyList_Size(posList);
  for(int i=0;i<npos;i++)
  {
    pos[i]=PyFloat_AsDouble(PyList_GetItem(posList,i));
  }
  // Check validity of given pos:
  status = geom->GetSpatialDim( &sdim );
  if ( status != SV_OK ) {
    PyErr_SetString(PyRunTimeErr,"error retrieving spatial dim of obj " );
    return Py_ERROR;
  }
  if ( ( sdim == 3 ) && ( npos != 3 ) ) {
    PyErr_SetString(PyRunTimeErr, "objects in 3 spatial dims require a 3D position");
    return Py_ERROR;
  } else if ( ( sdim == 2 ) && ( npos != 2 ) ) {
     PyErr_SetString(PyRunTimeErr,"objects in 2 spatial dims require a 2D position");
    return Py_ERROR;
  }

  status = geom->Distance( pos, upperLimit, &dist );

  if ( status == SV_OK ) {
    return Py_BuildValue("d",dist);
  } else {
    PyErr_SetString(PyRunTimeErr, "Distance: error on object ");
    return Py_ERROR;
  }
}


// ------------------
// Solid_TranslateMtd
// ------------------

static PyObject*  Solid_TranslateMtd( pySolidModel *self ,PyObject* args  )
{
  PyObject*  vecList;
  double vec[3];
  int nvec;
  int status;
  cvSolidModel *geom =(self->geom);
  if(PyArg_ParseTuple(args,"O",&vecList))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import list");
    return Py_ERROR;
  }

  if (PyList_Size(vecList)>3)
  {
    PyErr_SetString(PyRunTimeErr,"vecList Dimension is greater than 3!");
    return Py_ERROR;
  }
  nvec = PyList_Size(vecList);
  for(int i=0;i<nvec;i++)
  {
    vec[i]=PyFloat_AsDouble(PyList_GetItem(vecList,i));
  }

  status = geom->Translate( vec, nvec );

  if ( status != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "Translate: error on object " );
    return Py_ERROR;
  }

  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  return Py_BuildValue("s","success");
}


// ---------------
// Solid_RotateMtd
// ---------------

static PyObject*  Solid_RotateMtd( pySolidModel *self ,PyObject* args  )
{
  PyObject*  axisList;
  double axis[3];
  int naxis;
  double rad;
  int status;
  cvSolidModel *geom =(self->geom);
  if(!PyArg_ParseTuple(args,"Od",&axisList,&rad))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one list and one double");
    return Py_ERROR;
  }
  naxis = PyList_Size(axisList);
  if (naxis>3)
  {
    PyErr_SetString(PyRunTimeErr,"posList Dimension is greater than 3!");
    return Py_ERROR;
  }

  for(int i=0;i<naxis;i++)
  {
    axis[i]=PyFloat_AsDouble(PyList_GetItem(axisList,i));
  }
    status = geom->Rotate( axis, naxis, rad );

  if ( status != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "Rotate: error on object ");
    return Py_ERROR;
  }

  Py_RETURN_NONE;
}


// --------------
// Solid_ScaleMtd
// --------------

static PyObject*  Solid_ScaleMtd( pySolidModel *self ,PyObject* args  )
{
  double factor;
  int status;
  cvSolidModel *geom =(self->geom);
  if(!PyArg_ParseTuple(args,"d",&factor))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one double");
    return Py_ERROR;
  }

    // Do work of command:

  status = geom->Scale( factor );

  if ( status != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "Scale: error on object " );
    return Py_ERROR;
  }

  Py_RETURN_NONE;
}


// ----------------
// Solid_ReflectMtd
// ----------------

static PyObject* Solid_ReflectMtd( pySolidModel *self ,PyObject* args  )
{
  PyObject* posList;
  PyObject* nrmList;
  double pos[3];
  double nrm[3];
  int npos=0;
  int nnrm;
  int status;
  cvSolidModel *geom =(self->geom);
  if(!PyArg_ParseTuple(args,"OO",&posList,&nrmList))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import two lists");
    return Py_ERROR;
  }

  if (PyList_Size(posList)>3||PyList_Size(nrmList)>3)
  {
    PyErr_SetString(PyRunTimeErr,"List Dimension is greater than 3!");
    return Py_ERROR;
  }

  for(int i=0;i<PyList_Size(posList);i++)
  {
    pos[i]=PyFloat_AsDouble(PyList_GetItem(posList,i));
  }
  for(int i=0;i<PyList_Size(nrmList);i++)
  {
    nrm[i]=PyFloat_AsDouble(PyList_GetItem(nrmList,i));
  }

  // Do work of command:

  status = geom->Reflect( pos, nrm );

  if ( status != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "Reflect: error on object ");
    return Py_ERROR;
  }

  Py_RETURN_NONE;
}


// -----------------
// Solid_Apply4x4Mtd
// -----------------

static PyObject*  Solid_Apply4x4Mtd( pySolidModel *self ,PyObject* args  )
{
  PyObject* matList;
  PyObject* rowList;
  double mat[4][4];
  int n, i;
  int status;
  cvSolidModel *geom =(self->geom);
  if(!PyArg_ParseTuple(args,"O",&matList))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import matList");
    return Py_ERROR;
  }

  if (PyList_Size(matList)!=4)
  {
    PyErr_SetString(PyRunTimeErr,"Matrix Row  Dimension is not  4!");
    return Py_ERROR;
  }

  for(int i=0;i<PyList_Size(matList);i++)
  {
    rowList=PyList_GetItem(matList,i);
    if (PyList_Size(rowList)!=4)
    {
      PyErr_SetString(PyRunTimeErr,"Matrix Column Dimension is not  4!");
      return Py_ERROR;
    }
    for(int j=0;j<PyList_Size(rowList);j++)
    mat[i][j]=PyFloat_AsDouble(PyList_GetItem(rowList,j));
  }

  // Do work of command:

  status = geom->Apply4x4( mat );

  if ( status != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "Apply4x4: error on object"  );
    return Py_ERROR;
  }

  Py_RETURN_NONE;
}


// --------------
// Solid_PrintMtd
// --------------

static PyObject*  Solid_PrintMtd( pySolidModel *self ,PyObject* args  )
{

  // Do work of command:
  cvSolidModel *geom =(self->geom);
  geom->Print();
  return Py_BuildValue("s","success");
}


// --------------
// Solid_CheckMtd
// --------------

static PyObject*  Solid_CheckMtd( pySolidModel *self ,PyObject* args  )
{
  int nerr;

  // Do work of command:
  cvSolidModel *geom =(self->geom);
  geom->Check( &nerr );
  return Py_BuildValue("i",nerr);
}


// --------------------
// Solid_WriteNativeMtd
// --------------------

// $solid WriteNative -file foo.gm

static PyObject* Solid_WriteNativeMtd( pySolidModel *self ,PyObject* args  )
{
  char *fn;
  int status;
  int file_version = 0;
  cvSolidModel *geom =(self->geom);
  if(!PyArg_ParseTuple(args,"s|i",&fn,&file_version))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import char filename or int fileversion");
    return Py_ERROR;
  }

  // Do work of command:
  status = geom->WriteNative( file_version , fn );
  if ( status != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error writing object to file" );
    return Py_ERROR;
  } else {

    Py_INCREF(geom);
    self->geom=geom;
    Py_DECREF(geom);
    return Py_BuildValue("s","success");
  }
}


// -------------------------
// Solid_WriteVtkPolyDataMtd
// -------------------------

static PyObject*  Solid_WriteVtkPolyDataMtd( pySolidModel *self ,
				     PyObject* args  )
{
  char *fn;
  int status;
  cvSolidModel *geom =(self->geom);
  if(!PyArg_ParseTuple(args,"s",&fn))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import char filename");
    return Py_ERROR;
  }

    // Do work of command:
  status = geom->WriteVtkPolyData( fn );
  if ( status != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error writing object to file" );
    return Py_ERROR;
  } else {
    return Py_BuildValue("s","success");
  }
}


// ---------------------
// Solid_WriteGeomSimMtd
// ---------------------

static PyObject*  Solid_WriteGeomSimMtd( pySolidModel *self ,
				     PyObject* args  )
{
  char *fn;
  int status;
  cvSolidModel *geom =(self->geom);
  if(!PyArg_ParseTuple(args,"s",&fn))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import char filename");
    return Py_ERROR;
  }
   // Do work of command:
  status = geom->WriteGeomSim( fn );
  if ( status != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error writing object to file");
    return Py_ERROR;
  } else {
    return Py_BuildValue("s","success");
  }
}


// --------------------
// Solid_GetPolyDataMtd
// --------------------

static PyObject*  Solid_GetPolyDataMtd( pySolidModel *self ,PyObject* args  )
{
  char *resultName;
  cvPolyData *pd;
  double max_dist = -1.0;
  int useMaxDist = 0;
  if(!PyArg_ParseTuple(args,"s|d",&resultName,&max_dist))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import char resultName or int max_dist");
    return Py_ERROR;
  }

  // Do work of command:

  if (max_dist > 0) {
      useMaxDist = 1;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( resultName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    return Py_ERROR;
  }

  cvSolidModel *geom =(self->geom);
  // Get the cvPolyData:
  pd = geom->GetPolyData(useMaxDist, max_dist);
  //Py_DECREF(geom);
  if ( pd == NULL ) {
    PyErr_SetString(PyRunTimeErr, "error getting cvPolyData for " );
    return Py_ERROR;
  }

  // Register the result:
  if ( !( gRepository->Register( resultName, pd ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository" );
    delete pd;
    return Py_ERROR;
  }
  Py_INCREF(geom);
  self->geom=geom;
  Py_DECREF(geom);
  return Py_BuildValue("s","success");
}

// ----------------------
// Solid_SetVtkPolyDataMtd
// ----------------------

static PyObject* Solid_SetVtkPolyDataMtd( pySolidModel* self, PyObject* args)
{
  cvSolidModel *geom = (self->geom);
  char *objName;
  RepositoryDataT type;
  cvRepositoryData *obj;
  vtkPolyData *pd;

  if(!PyArg_ParseTuple(args,"s",&objName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one string");
    return Py_ERROR;
  }


  // Do work of command:
  type = gRepository->GetType( objName );
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "object must be of type cvPolyData");
    return Py_ERROR;
  }

  obj = gRepository->GetObject( objName );
  switch (type) {
  case POLY_DATA_T:
    pd = ((cvPolyData *)obj)->GetVtkPolyData();
    break;
  default:
    PyErr_SetString(PyRunTimeErr, "error in SetVtkPolyData" );
    return Py_ERROR;
    break;
  }
  // set the vtkPolyData:
  if(!geom->SetVtkPolyDataObject(pd))
  {
    return Py_ERROR;
  }

  return Py_BuildValue("s","success");
}


// Solid_GetFacePolyDataMtd
// ------------------------

static PyObject* Solid_GetFacePolyDataMtd( pySolidModel* self, PyObject* args)
{
  cvSolidModel *geom = (self->geom);
  char *resultName;
  cvPolyData *pd;
  int faceid;
  double max_dist = -1.0;
  int useMaxDist = 0;

  if(!PyArg_ParseTuple(args,"si|d",&resultName,&faceid,&max_dist))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one string, one int and one double");
    return Py_ERROR;
  }

  // Do work of command:

  if (max_dist > 0) {
      useMaxDist = 1;
  }

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( resultName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    return Py_ERROR;
  }

  // Get the cvPolyData:
  pd = geom->GetFacePolyData(faceid,useMaxDist,max_dist);
  if ( pd == NULL ) {
    PyErr_SetString(PyRunTimeErr, "error getting cvPolyData for ");
    return Py_ERROR;
  }

  // Register the result:
  if ( !( gRepository->Register( resultName, pd ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete pd;
    return Py_ERROR;
  }

  return Py_BuildValue("s","success");
}


// ----------------------
// Solid_GetFaceNormalMtd
// ----------------------

static PyObject* Solid_GetFaceNormalMtd( pySolidModel* self, PyObject* args)
{
  cvSolidModel *geom = (self->geom);
  int faceid;
  double u,v;

  if(!PyArg_ParseTuple(args,"idd",&faceid,&u,&v))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one int and two doubles");
    return Py_ERROR;
  }

  // Do work of command:

  double normal[3];

  if ( geom->GetFaceNormal(faceid,u,v,normal) == SV_ERROR ) {
    PyErr_SetString(PyRunTimeErr, "error getting Normal for face. ");
    return Py_ERROR;
  }

  char rtnstr[255];
  rtnstr[0]='\0';
  sprintf( rtnstr, "%lf %lf %lf",normal[0],normal[1],normal[2]);

  return Py_BuildValue("s",rtnstr);
}


// ---------------------------
// Solid_GetDiscontinuitiesMtd
// ---------------------------

static PyObject* Solid_GetDiscontinuitiesMtd( pySolidModel* self,
					PyObject* args)
{
  cvSolidModel *geom = (self->geom);
  char *resultName;
  cvPolyData *pd;

  if(!PyArg_ParseTuple(args,"s",&resultName))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one string");
    return Py_ERROR;
  }

  // Do work of command:

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( resultName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    return Py_ERROR;
  }

  // Get the cvPolyData:
  pd = geom->GetDiscontinuities();
  if ( pd == NULL ) {
    PyErr_SetString(PyRunTimeErr, "error getting discontinuities");
    return Py_ERROR;
  }

  // Register the result:
  if ( !( gRepository->Register( resultName, pd ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete pd;
    return Py_ERROR;
  }

  return Py_BuildValue("s","success");
}


// -----------------------------------
// Solid_GetAxialIsoparametricCurveMtd
// -----------------------------------

static pySolidModel* Solid_GetAxialIsoparametricCurveMtd( pySolidModel* self,
						PyObject* args)
{
  cvSolidModel *geom = (self->geom);
  char *resultName;
  double prm;
  cvSolidModel *curve;

  if(!PyArg_ParseTuple(args,"sd",&resultName,&prm))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one string and one double");
    return Py_ERROR;
  }


  // Do work of command:

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( resultName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    return Py_ERROR;
  }

  // Get the isoparametric curve on the given surface at the given
  // parameter value:
  if ( ( prm < 0.0 ) || ( prm > 1.0 ) ) {
    PyErr_SetString(PyRunTimeErr, "parameter value must be between 0.0 and 1.0");
    return Py_ERROR;
  }
  curve = geom->GetAxialIsoparametricCurve( prm );
  if ( curve == NULL ) {
    PyErr_SetString(PyRunTimeErr, "error getting isoparametric curve for");
    return Py_ERROR;
  }

  // Register the result:
  if ( !( gRepository->Register( resultName, curve ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete curve;
    return Py_ERROR;
  }

  PyErr_SetString(PyRunTimeErr, curve->GetName());

  Py_INCREF(curve);
  pySolidModel* newCurve;
  newCurve = PyObject_New(pySolidModel, &pySolidModelType);
  newCurve->geom=curve;
  Py_DECREF(curve);
  return newCurve;
}
// ------------------
// Solid_GetKernelMtd
// ------------------

static PyObject* Solid_GetKernelMtd( pySolidModel* self, PyObject* args)
{
  cvSolidModel *geom = (self->geom);
  SolidModel_KernelT kernelType;
  char *kernelName;

  // Do work of command:
  kernelType = geom->GetKernelT();
  kernelName = SolidModel_KernelT_EnumToStr( kernelType );


  if ( kernelType == SM_KT_INVALID ) {
    PyErr_SetString(PyRunTimeErr, kernelName);
    return Py_ERROR;
  } else {
    return Py_BuildValue("s",kernelName);
  }
}


// ---------------------
// Solid_GetLabelKeysMtd
// ---------------------

static PyObject* Solid_GetLabelKeysMtd( pySolidModel* self, PyObject* args)
{
  cvSolidModel *geom = (self->geom);
  int numKeys, i;
  char **keys;

  // Do work of command:
  geom->GetLabelKeys( &numKeys, &keys );
  PyObject* keyList=PyList_New(numKeys);
  for (i = 0; i < numKeys; i++) {
    PyList_SetItem(keyList, i, PyString_FromString(keys[i]));
  }
  delete [] keys;

  return keyList;
}


// -----------------
// Solid_GetLabelMtd
// -----------------

static PyObject* Solid_GetLabelMtd( pySolidModel* self, PyObject* args)
{
  cvSolidModel *geom = (self->geom);
  char *key, *value;

  if(!PyArg_ParseTuple(args,"s",&key))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one string");
    return Py_ERROR;
  }


  // Do work of command:

  if ( ! geom->GetLabel( key, &value ) ) {
    PyErr_SetString(PyRunTimeErr, "key not found" );
    return Py_ERROR;
  }
  return Py_BuildValue("s",value);
}


// -----------------
// Solid_SetLabelMtd
// -----------------

static PyObject* Solid_SetLabelMtd( pySolidModel* self, PyObject* args)
{
  cvSolidModel *geom = (self->geom);
  char *key, *value;

  if(!PyArg_ParseTuple(args,"ss",&key,&value))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import two strings");
    return Py_ERROR;
  }


  // Do work of command:

  if ( ! geom->SetLabel( key, value ) ) {
    if ( geom->IsLabelPresent( key ) ) {
      PyErr_SetString(PyRunTimeErr, "key already in use");
      return Py_ERROR;
    } else {
      PyErr_SetString(PyRunTimeErr, "error setting label" );
      return Py_ERROR;
    }
  }

  Py_RETURN_NONE;
}


// -------------------
// Solid_ClearLabelMtd
// -------------------

static PyObject* Solid_ClearLabelMtd( pySolidModel* self, PyObject* args)
{
  cvSolidModel *geom = (self->geom);
  char *key;

  if(!PyArg_ParseTuple(args,"s",&key))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one string");
    return Py_ERROR;
  }

  // Do work of command:

  if ( ! geom->IsLabelPresent( key ) ) {
    PyErr_SetString(PyRunTimeErr, "key not found");
    return Py_ERROR;
  }

  geom->ClearLabel( key );

  Py_RETURN_NONE;
}


// -------------------
// Solid_GetFaceIdsMtd
// -------------------

static PyObject* Solid_GetFaceIdsMtd( pySolidModel* self, PyObject* args)
{
  cvSolidModel *geom = (self->geom);
  int numFaces;
  int *faces;
  char facestring[256];
  PyObject* faceList;
  fprintf(stdout,"checkGetFaceId\n");
  int status = geom->GetFaceIds( &numFaces, &faces);
  if ( status == SV_OK ) {
    if (numFaces == 0)
    {
      Py_INCREF(Py_None);
      fprintf(stdout,"checkGetFaceId0\n");

      return Py_None;
    }
    faceList=PyList_New(numFaces);
    for (int i = 0; i < numFaces; i++) {
	  sprintf(facestring, "%i", faces[i]);
      PyList_SetItem(faceList,i,PyString_FromFormat(facestring));
	  facestring[0]='\n';
    }
    delete faces;
    fprintf(stdout,"checkGetFaceId\n");
    return faceList;
  } else {
    PyErr_SetString(PyRunTimeErr, "GetFaceIds: error on object ");
    return Py_ERROR;
  }
}

// -------------------
// Solid_GetBoundaryFacesMtd
// -------------------
//
static PyObject*  Solid_GetBoundaryFacesMtd( pySolidModel* self, PyObject* args)
{
  cvSolidModel *geom = (self->geom);
  double angle = 0.0;

  if(!PyArg_ParseTuple(args,"d",&angle))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one double");
    return Py_ERROR;
  }


  int status = geom->GetBoundaryFaces(angle);
  if ( status == SV_OK ) {
    Py_RETURN_NONE;
  } else {
    PyErr_SetString(PyRunTimeErr, "GetBoundaryFaces: error on object ");
    return Py_ERROR;
  }
}

// ---------------------
// Solid_GetRegionIdsMtd
// ---------------------

static PyObject* Solid_GetRegionIdsMtd( pySolidModel* self, PyObject* args)
{
  cvSolidModel *geom = (self->geom);
  int numRegions;
  int *regions;
  char regionstring[256];
  PyObject* regionList;

  int status = geom->GetRegionIds( &numRegions, &regions);
  if ( status == SV_OK ) {
    if (numRegions == 0)
    {
      Py_INCREF(Py_None);
      return Py_None;
    }
    regionList=PyList_New(numRegions);
    for (int i = 0; i < numRegions; i++) {
	  sprintf(regionstring, "%i", regions[i]);
      PyList_SetItem(regionList,i,PyString_FromFormat(regionstring));
	  regionstring[0]='\n';
    }
    delete regions;
    return regionList;
  } else {
    PyErr_SetString(PyRunTimeErr, "GetRegionIds: error on object ");
    return Py_ERROR;
  }
}


// --------------------
// Solid_GetFaceAttrMtd
// --------------------

static PyObject* Solid_GetFaceAttrMtd( pySolidModel* self, PyObject* args)
{
  cvSolidModel *geom = (self->geom);
  char *key, *value;
  int faceid;

  if(!PyArg_ParseTuple(args,"si",&key, &faceid))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one string and one int");
    return Py_ERROR;
  }

  // Do work of command:

  if ( ! geom->GetFaceAttribute( key, faceid, &value ) ) {
    PyErr_SetString(PyRunTimeErr, "attribute not found");
    return Py_ERROR;
  }


  return Py_BuildValue("s",value);
}


// --------------------
// Solid_SetFaceAttrMtd
// --------------------

static PyObject* Solid_SetFaceAttrMtd( pySolidModel* self, PyObject* args)
{
  cvSolidModel *geom = (self->geom);
  char *key, *value;
  int faceid;

  if(!PyArg_ParseTuple(args,"ssi",&key,&value,&faceid))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import two strings and one int");
    return Py_ERROR;
  }

  // Do work of command:

  if ( ! geom->SetFaceAttribute( key, faceid, value ) ) {
    PyErr_SetString(PyRunTimeErr, "attribute could not be set");
    return Py_ERROR;
  }

  Py_RETURN_NONE;
}


// ----------------------
// Solid_GetRegionAttrMtd
// ----------------------

static PyObject* Solid_GetRegionAttrMtd( pySolidModel* self, PyObject* args)
{
  cvSolidModel *geom = (self->geom);
  char *key, *value;
  int regionid;

  if(!PyArg_ParseTuple(args,"si",&key, &regionid))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one string and one int");
    return Py_ERROR;
  }


  // Do work of command:

  if ( ! geom->GetRegionAttribute( key, regionid, &value ) ) {
    PyErr_SetString(PyRunTimeErr, "attribute not found");
    return Py_ERROR;
  }


  return Py_BuildValue("s",value);
}


// ----------------------
// Solid_SetRegionAttrMtd
// ----------------------

static PyObject* Solid_SetRegionAttrMtd( pySolidModel* self, PyObject* args)
{
  cvSolidModel *geom = (self->geom);
  char *key, *value;
  int regionid;

  if(!PyArg_ParseTuple(args,"ssi",&key,&value,&regionid))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import two strings and one int");
    return Py_ERROR;
  }


  // Do work of command:

  if ( ! geom->SetRegionAttribute( key, regionid, value ) ) {
    PyErr_SetString(PyRunTimeErr, "attribute could not be set" );
    return Py_ERROR;
  }

  Py_RETURN_NONE;
}


// --------------------
// Solid_DeleteFacesMtd
// --------------------

static PyObject* Solid_DeleteFacesMtd( pySolidModel* self, PyObject* args)
{
  cvSolidModel *geom = (self->geom);
  int status;
  PyObject* faceList;
  if(!PyArg_ParseTuple(args,"O",&faceList))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one list");
    return Py_ERROR;
  }

  if (PyList_Size(faceList) == 0) {
      Py_RETURN_NONE;
  }

  int nfaces = 0;
  int *faces = new int[PyList_Size(faceList)];

  for (int i=0;i<PyList_Size(faceList);i++)
  {
    faces[i]=PyLong_AsLong(PyList_GetItem(faceList,i));
  }
  // Do work of command:

  status = geom->DeleteFaces( nfaces, faces );

  delete [] faces;

  if ( status != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "DeleteFaces: error on object");
    return Py_ERROR;
  }

  Py_RETURN_NONE;
}

// --------------------
// Solid_DeleteRegionMtd
// --------------------

static PyObject* Solid_DeleteRegionMtd( pySolidModel* self, PyObject* args)
{
  cvSolidModel *geom = (self->geom);
  int regionid;
  int status;

  if(!PyArg_ParseTuple(args,"i",&regionid))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one int");
    return Py_ERROR;
  }
  // Do work of command:

  status = geom->DeleteRegion( regionid );

  if ( status != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "DeleteRegion: error on object");
    return Py_ERROR;
  }

  Py_RETURN_NONE;
}


// ------------------------
// Solid_CreateEdgeBlendMtd
// ------------------------

static PyObject* Solid_CreateEdgeBlendMtd( pySolidModel* self, PyObject* args)
{
  cvSolidModel *geom = (self->geom);
  int status;
  int faceA;
  int faceB;
  int filletshape=0;
  double radius;

  if(!PyArg_ParseTuple(args,"iid|i",&faceA,&faceB,&radius,&filletshape))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import two ints, one double or one optional int");
    return Py_ERROR;
  }

  status = geom->CreateEdgeBlend( faceA, faceB, radius, filletshape );

  if ( status != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "CreateEdgeBlend: error on object ");
    return Py_ERROR;
  }

  Py_RETURN_NONE;
}

static PyObject* Solid_CombineFacesMtd( pySolidModel* self, PyObject* args)
{
  cvSolidModel *geom = (self->geom);
  int status;
  int faceid1;
  int faceid2;

  if(!PyArg_ParseTuple(args,"ii",&faceid1,&faceid2))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import two ints");
    return Py_ERROR;
  }


  status = geom->CombineFaces( faceid1, faceid2);

  if ( status != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "Combine Faces: Error");
    return Py_ERROR;
  }

  Py_RETURN_NONE;
}

static PyObject* Solid_RemeshFaceMtd( pySolidModel* self, PyObject* args)
{
  cvSolidModel *geom = (self->geom);
  int status;
  double size;
  PyObject* excludeList;
  if(!PyArg_ParseTuple(args,"Od",&excludeList,&size))
  {
    PyErr_SetString(PyRunTimeErr,"Could not import one list and one double");
    return Py_ERROR;
  }

  if (PyList_Size(excludeList) == 0) {
      Py_RETURN_NONE;
  }

  int nfaces = 0;
  int *faces = new int[PyList_Size(excludeList)];

  for (int i=0;i<PyList_Size(excludeList);i++)
  {
    faces[i]=PyLong_AsLong(PyList_GetItem(excludeList,i));
  }

  status = geom->RemeshFace( nfaces, faces, size);

  delete [] faces;

  if ( status != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "Remesh Face: Error");
    return Py_ERROR;
  }

  Py_RETURN_NONE;
}
