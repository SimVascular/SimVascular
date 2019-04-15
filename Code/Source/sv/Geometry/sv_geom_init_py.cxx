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

#include <stdio.h>
#include <string.h>
#include "sv_Repository.h"
#include "sv_RepositoryData.h"
#include "sv_PolyData.h"
#include "sv_geom_init_py.h"
#include "sv_sys_geom.h"
#include "sv_SolidModel.h"
#include "sv_solid_init_py.h"
#include "sv_integrate_surface.h"
#include "sv_arg.h"
#include "sv_misc_utils.h"
#include "sv_vtk_utils.h"
#include "vtkSmartPointer.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#include "sv2_globals.h"


// Prototypes:
// -----------
PyObject* PyRunTimeErr;

PyObject* Geom_ReduceCmd(PyObject* self, PyObject* args);

PyObject* Geom_MakePolysConsistentCmd(PyObject* self, PyObject* args);

PyObject* Geom_ReverseAllCellsCmd(PyObject* self, PyObject* args);

PyObject* Geom_NumClosedLineRegionsCmd(PyObject* self, PyObject* args);

PyObject* Geom_GetClosedLineRegionCmd(PyObject* self, PyObject* args);

PyObject* Geom_PickCmd(PyObject* self, PyObject* args);

PyObject* Geom_OrientProfileCmd(PyObject* self, PyObject* args);

PyObject* Geom_DisorientProfileCmd(PyObject* self, PyObject* args);

PyObject* Geom_AlignProfileCmd(PyObject* self, PyObject* args);

PyObject* Geom_TranslateCmd(PyObject* self, PyObject* args);

PyObject* Geom_ScaleAvgCmd(PyObject* self, PyObject* args);

PyObject* Geom_GetOrderedPtsCmd(PyObject* self, PyObject* args);

PyObject* Geom_WriteOrderedPtsCmd(PyObject* self, PyObject* args);

PyObject* Geom_WriteLinesCmd(PyObject* self, PyObject* args);

PyObject* Geom_PolysClosedCmd(PyObject* self, PyObject* args);

PyObject* Geom_SurfAreaCmd(PyObject* self, PyObject* args);

PyObject* Geom_GetPolyCentroidCmd(PyObject* self, PyObject* args);

PyObject* Geom_PrintTriStatsCmd(PyObject* self, PyObject* args);

PyObject* Geom_PrintSmallPolysCmd(PyObject* self, PyObject* args);

PyObject* Geom_RmSmallPolysCmd(PyObject* self, PyObject* args);

PyObject* Geom_BBoxCmd(PyObject* self, PyObject* args);

PyObject* Geom_ClassifyCmd(PyObject* self, PyObject* args);

PyObject* Geom_PtInPolyCmd(PyObject* self, PyObject* args);

PyObject* Geom_MergePtsCmd(PyObject* self, PyObject* args);

PyObject* Geom_Warp3dPtsCmd(PyObject* self, PyObject* args);

PyObject* Geom_NumPtsCmd(PyObject* self, PyObject* args);

PyObject* Geom_sampleLoopCmd(PyObject* self, PyObject* args);

PyObject* Geom_loftSolidCmd(PyObject* self, PyObject* args);

PyObject* Geom_loftSolidWithNURBSCmd(PyObject* self, PyObject* args);

PyObject* Geom_2dWindingNumCmd(PyObject* self, PyObject* args);

PyObject* Geom_PolygonNormCmd(PyObject* self, PyObject* args);

PyObject* Geom_AvgPtCmd(PyObject* self, PyObject* args);

PyObject* Geom_CopyCmd(PyObject* self, PyObject* args);

PyObject* Geom_ProjectCmd(PyObject* self, PyObject* args);

PyObject* Geom_ReorderPgnCmd(PyObject* self, PyObject* args);

PyObject* Geom_SplinePtsToPathPlanCmd(PyObject* self, PyObject* args);

PyObject* Geom_IntegrateSurfaceCmd(PyObject* self, PyObject* args);

PyObject* Geom_IntegrateSurface2Cmd(PyObject* self, PyObject* args);

PyObject* Geom_IntegrateEnergyCmd(PyObject* self, PyObject* args);

PyObject* Geom_FindDistanceCmd(PyObject* self, PyObject* args);

PyObject* Geom_InterpolateScalarCmd(PyObject* self, PyObject* args);

PyObject* Geom_InterpolateVectorCmd(PyObject* self, PyObject* args);

PyObject* Geom_IntersectWithLineCmd(PyObject* self, PyObject* args);

PyObject* Geom_AddPointDataCmd(PyObject* self, PyObject* args);

PyObject* Geom_SubtractPointDataCmd(PyObject* self, PyObject* args);

PyObject* Geom_MultiplyPointDataCmd(PyObject* self, PyObject* args);

PyObject* Geom_DividePointDataCmd(PyObject* self, PyObject* args);

PyObject* Geom_IntegrateScalarSurfCmd(PyObject* self, PyObject* args);

PyObject* Geom_IntegrateScalarThreshCmd(PyObject* self, PyObject* args);

PyObject* Geom_ReplacePointDataCmd(PyObject* self, PyObject* args);

PyObject* Geom_UnionCmd(PyObject* self, PyObject* args);

PyObject* Geom_IntersectCmd(PyObject* self, PyObject* args);

PyObject* Geom_SubtractCmd(PyObject* self, PyObject* args);

PyObject* Geom_CheckSurfaceCmd(PyObject* self, PyObject* args);

PyObject* Geom_CleanCmd(PyObject* self, PyObject* args);

PyObject* Geom_CapIdSetCmd(PyObject* self, PyObject* args);

PyObject* Geom_LocalDecimationCmd(PyObject* self, PyObject* args);

PyObject* Geom_LocalLaplacianSmoothCmd(PyObject* self, PyObject* args);

PyObject* Geom_LocalConstrainSmoothCmd(PyObject* self, PyObject* args);

PyObject* Geom_LocalLinearSubdivisionCmd(PyObject* self, PyObject* args);

PyObject* Geom_LocalButterflySubdivisionCmd(PyObject* self, PyObject* args);

PyObject* Geom_LocalLoopSubdivisionCmd(PyObject* self, PyObject* args);

PyObject* Geom_LocalBlendCmd(PyObject* self, PyObject* args);

PyObject* Geom_SetArrayForLocalOp_SphereCmd(PyObject* self, PyObject* args);

PyObject* Geom_SetArrayForLocalOp_FaceCmd(PyObject* self, PyObject* args);

PyObject* Geom_SetArrayForLocalOp_CellsCmd(PyObject* self, PyObject* args);

PyObject* Geom_SetArrayForLocalOp_BlendCmd(PyObject* self, PyObject* args);

PyObject* Geom_All_UnionCmd(PyObject* self, PyObject* args);

PyObject* Geom_Convert_NURBS_To_PolyCmd(PyObject* self, PyObject* args);

#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC initpyGeom(void);
#endif
#if PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC PyInit_pyGeom(void);
#endif

// Helper functions
// ----------------


// ----------
// Geom_Methods
// ----------
PyMethodDef pyGeom_methods[] =
{
  {"Reduce", Geom_ReduceCmd, METH_VARARGS, NULL},
  {"MakePolysConsistent",
		     Geom_MakePolysConsistentCmd, METH_VARARGS, NULL},
  {"ReverseAllCells",
		     Geom_ReverseAllCellsCmd, METH_VARARGS, NULL},
  {"NumClosedLineRegions",
		     Geom_NumClosedLineRegionsCmd, METH_VARARGS, NULL},
  {"GetClosedLineRegion",
		     Geom_GetClosedLineRegionCmd, METH_VARARGS, NULL},
  {"Pick", Geom_PickCmd, METH_VARARGS, NULL},
  {"OrientProfile", Geom_OrientProfileCmd, METH_VARARGS, NULL},
  {"DisorientProfile", Geom_DisorientProfileCmd, METH_VARARGS, NULL},
  {"AlignProfile", Geom_AlignProfileCmd, METH_VARARGS, NULL},
  {"Translate", Geom_TranslateCmd, METH_VARARGS, NULL},
  {"ScaleAvg", Geom_ScaleAvgCmd, METH_VARARGS, NULL},
  {"GetOrderedPts", Geom_GetOrderedPtsCmd, METH_VARARGS, NULL},
  {"WriteOrderedPts",
		     Geom_WriteOrderedPtsCmd, METH_VARARGS, NULL},
  {"WriteLines", Geom_WriteLinesCmd, METH_VARARGS, NULL},
  {"PolysClosed", Geom_PolysClosedCmd, METH_VARARGS, NULL},
  {"SurfArea", Geom_SurfAreaCmd, METH_VARARGS, NULL},
  {"GetPolyCentroid", Geom_GetPolyCentroidCmd, METH_VARARGS, NULL},
  {"PrintTriStats", Geom_PrintTriStatsCmd, METH_VARARGS, NULL},
  {"PrintSmallPolys", Geom_PrintSmallPolysCmd, METH_VARARGS, NULL},
  {"RmSmallPolys", Geom_RmSmallPolysCmd, METH_VARARGS, NULL},
  {"Bbox", Geom_BBoxCmd, METH_VARARGS, NULL},
  {"Classify", Geom_ClassifyCmd, METH_VARARGS, NULL},
  {"PtInPoly", Geom_PtInPolyCmd, METH_VARARGS, NULL},
  {"MergePts", Geom_MergePtsCmd, METH_VARARGS, NULL},
  {"Warp3dPts", Geom_Warp3dPtsCmd, METH_VARARGS, NULL},
  {"NumPts", Geom_NumPtsCmd, METH_VARARGS, NULL},
  {"SampleLoop", Geom_sampleLoopCmd, METH_VARARGS, NULL},
  {"LoftSolid", Geom_loftSolidCmd, METH_VARARGS, NULL},
  {"LoftSolidWithNURBS", Geom_loftSolidWithNURBSCmd, METH_VARARGS, NULL},
  {"2dWindingNum", Geom_2dWindingNumCmd, METH_VARARGS, NULL},
  {"PolygonNorm", Geom_PolygonNormCmd, METH_VARARGS, NULL},
  {"AvgPt", Geom_AvgPtCmd, METH_VARARGS, NULL},
  {"Copy", Geom_CopyCmd, METH_VARARGS, NULL},
  {"Project", Geom_ProjectCmd, METH_VARARGS, NULL},
  {"ReorderPgn", Geom_ReorderPgnCmd, METH_VARARGS, NULL},
  {"SplinePtsToPathPlan", Geom_SplinePtsToPathPlanCmd, METH_VARARGS, NULL},
  {"IntegrateSurfaceFlux", Geom_IntegrateSurfaceCmd, METH_VARARGS, NULL},
  {"IntegrateSurface2", Geom_IntegrateSurface2Cmd, METH_VARARGS, NULL},
  {"IntegrateEnergy", Geom_IntegrateEnergyCmd, METH_VARARGS, NULL},
  {"FindDistance", Geom_FindDistanceCmd, METH_VARARGS, NULL},
  {"InterpolateScalar", Geom_InterpolateScalarCmd, METH_VARARGS, NULL},
  {"InterpolateVector", Geom_InterpolateVectorCmd, METH_VARARGS, NULL},
  {"IntersectWithLine", Geom_IntersectWithLineCmd, METH_VARARGS, NULL},
  {"AddPointData", Geom_AddPointDataCmd, METH_VARARGS, NULL},
  {"SubtractPointData", Geom_SubtractPointDataCmd, METH_VARARGS, NULL},
  {"MultiplyPointData", Geom_MultiplyPointDataCmd, METH_VARARGS, NULL},
  {"DividePointData", Geom_DividePointDataCmd, METH_VARARGS, NULL},
  {"IntegrateScalarSurf", Geom_IntegrateScalarSurfCmd, METH_VARARGS, NULL},
  {"IntegrateScalarThresh", Geom_IntegrateScalarThreshCmd, METH_VARARGS, NULL},
  {"ReplacePointData", Geom_ReplacePointDataCmd, METH_VARARGS, NULL},
 {"Union", Geom_UnionCmd, METH_VARARGS, NULL},
 {"Intersect", Geom_IntersectCmd, METH_VARARGS, NULL},
 {"Subtract", Geom_SubtractCmd, METH_VARARGS, NULL},
 {"Checksurface", Geom_CheckSurfaceCmd, METH_VARARGS, NULL},
 {"Clean", Geom_CleanCmd, METH_VARARGS, NULL},
 {"Set_ids_for_caps", Geom_CapIdSetCmd, METH_VARARGS, NULL},
 {"Local_decimation", Geom_LocalDecimationCmd, METH_VARARGS, NULL},
 {"Local_laplacian_smooth", Geom_LocalLaplacianSmoothCmd, METH_VARARGS, NULL},
 {"Local_constrain_smooth", Geom_LocalConstrainSmoothCmd, METH_VARARGS, NULL},
 {"Local_linear_subdivision", Geom_LocalLinearSubdivisionCmd, METH_VARARGS, NULL},
 {"Local_butterfly_subdivision", Geom_LocalButterflySubdivisionCmd, METH_VARARGS, NULL},
 {"Local_loop_subdivision", Geom_LocalLoopSubdivisionCmd, METH_VARARGS, NULL},
 {"Local_blend", Geom_LocalBlendCmd, METH_VARARGS, NULL},
 {"Set_array_for_local_op_sphere", Geom_SetArrayForLocalOp_SphereCmd, METH_VARARGS, NULL},
 {"Set_array_for_local_op_face", Geom_SetArrayForLocalOp_FaceCmd, METH_VARARGS, NULL},
 {"Set_array_for_local_op_cells", Geom_SetArrayForLocalOp_CellsCmd, METH_VARARGS, NULL},
 {"Set_array_for_local_op_face_blend", Geom_SetArrayForLocalOp_BlendCmd, METH_VARARGS, NULL},
  {"All_union", Geom_All_UnionCmd, METH_VARARGS, NULL},
  {"model_name_model_from_polydata_names", Geom_Convert_NURBS_To_PolyCmd, METH_VARARGS, NULL},
  {NULL,NULL}
};

#if PYTHON_MAJOR_VERSION == 3
static struct PyModuleDef pyGeomModule= {
   PyModuleDef_HEAD_INIT,
   "pyGeom",   /* name of module */
   "", /* module documentation, may be NULL */
   -1,       /* size of per-interpreter state of the module,
                or -1 if the module keeps state in global variables. */
   pyGeom_methods
};
#endif

//==============
// initpyGeom
//==============
#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC initpyGeom(void)

{

  PyObject *pyC;
  if ( gRepository == NULL ) {
    gRepository = new cvRepository();
    fprintf( stdout, "gRepository created from sv_geom_init\n" );
  }
  pyC = Py_InitModule("pyGeom",pyGeom_methods);

  PyRunTimeErr = PyErr_NewException("pyGeom.error",NULL,NULL);
  Py_INCREF(PyRunTimeErr);
  PyModule_AddObject(pyC,"error",PyRunTimeErr);

}

#endif
#if PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC PyInit_pyGeom(void)
{
  PyObject *pyC;
  if ( gRepository == NULL ) {
    gRepository = new cvRepository();
    fprintf( stdout, "gRepository created from sv_geom_init\n" );
  }
  pyC = PyModule_Create(& pyGeomModule);

  PyRunTimeErr = PyErr_NewException("pyGeom.error",NULL,NULL);
  Py_INCREF(PyRunTimeErr);
  PyModule_AddObject(pyC,"error",PyRunTimeErr);

  return pyC;
}

#endif
int Geom_pyInit()

{

#if PYTHON_MAJOR_VERSION == 2
  initpyGeom();
#elif PYTHON_MAJOR_VERSION == 3
  PyInit_pyGeom();
#endif
  return SV_OK;

}

// --------------
// Geom_ReduceCmd
// --------------

PyObject* Geom_ReduceCmd(PyObject* self, PyObject* args)
{
  char *srcName;
  char *dstName;
  double tol;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;
  
  if (!PyArg_ParseTuple(args,"ssd", &srcName, &dstName,&tol))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 2 chars: \
srcName, dstName and one double, tol");
    
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object ");
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr,  "object already exists");
    
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData");
    
  }

  if ( sys_geom_Reduce( (cvPolyData*)src, tol, (cvPolyData**)(&dst) ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "point merging error" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository" );
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}

// -------------
// Geom_UnionCmd
// -------------

PyObject* Geom_UnionCmd(PyObject* self, PyObject* args)
{
  char *aName;
  char *bName;
  char *dstName;
  cvRepositoryData *srcA;
  cvRepositoryData *srcB;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;
  double tolerance = 1e-6;
  
  if (!PyArg_ParseTuple(args,"sss|d", &aName, &bName,&dstName,&tolerance))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 3 chars: \
aName, bName, dstName and one optional double, tol");
    
  }

  // Do work of command:

  // Retrieve source object:
  srcA = gRepository->GetObject( aName );
  if ( srcA == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object ");
    
  }

  srcB = gRepository->GetObject( bName );
  if ( srcB == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object ");
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  type = srcA->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "object not of type cvPolyData");
    
  }

  type = srcB->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "object not of type cvPolyData");
    
  }

  if ( sys_geom_union( (cvPolyData*)srcA, (cvPolyData*)srcB, tolerance, (cvPolyData**)(&dst) )
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "poly manipulation error" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }


  return Py_BuildValue("s",dst->GetName());
}


// -----------------
// Geom_IntersectCmd
// -----------------

PyObject* Geom_IntersectCmd(PyObject* self, PyObject* args)
{
  char *aName;
  char *bName;
  char *dstName;
  cvRepositoryData *srcA;
  cvRepositoryData *srcB;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;
  double tolerance = 1e-6;

  if (!PyArg_ParseTuple(args,"sss|d", &aName, &bName,&dstName,&tolerance))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 3 chars: \
aName, bName, dstName and one optional double, tol");
    
  }


  // Do work of command:

  // Retrieve source object:
  srcA = gRepository->GetObject( aName );
  if ( srcA == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }

  srcB = gRepository->GetObject( bName );
  if ( srcB == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  type = srcA->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "object not of type cvPolyData");
    
  }

  type = srcB->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "object not of type cvPolyData");
    
  }

  if ( sys_geom_intersect( (cvPolyData*)srcA, (cvPolyData*)srcB, tolerance,(cvPolyData**)(&dst) )
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "poly manipulation error" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}


// ----------------
// Geom_SubtractCmd
// ----------------

PyObject* Geom_SubtractCmd(PyObject* self, PyObject* args)
{
  char *aName;
  char *bName;
  char *dstName;
  cvRepositoryData *srcA;
  cvRepositoryData *srcB;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;
  double tolerance = 1e-6;

  if (!PyArg_ParseTuple(args,"sss|d", &aName, &bName,&dstName,&tolerance))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 3 chars: \
aName, bName, dstName and one optional double, tol");
    
  }


  // Do work of command:

  // Retrieve source object:
  srcA = gRepository->GetObject( aName );
  if ( srcA == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }

  srcB = gRepository->GetObject( bName );
  if ( srcB == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  type = srcA->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "object not of type cvPolyData");
    
  }

  type = srcB->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "object not of type cvPolyData");
    
  }

  if ( sys_geom_subtract( (cvPolyData*)srcA, (cvPolyData*)srcB, tolerance, (cvPolyData**)(&dst) )
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "poly manipulation error" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }


  return Py_BuildValue("s",dst->GetName());
}

// ----------------
// Geom_CheckSurfaceCmd
// ----------------

PyObject* Geom_CheckSurfaceCmd(PyObject* self, PyObject* args)
{
  char *Name;
  cvRepositoryData *src;
  RepositoryDataT type;
  double tol = 1e-6;

  if (!PyArg_ParseTuple(args,"s|d", &tol))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 1 char, Name and one optional double, tol");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( Name );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object ");
    
  }
  
  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  int stats[2];
  if ( sys_geom_checksurface( (cvPolyData*)src, stats,tol)
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error checking surface" );
    
  }

  return Py_BuildValue("ii",stats[0],stats[1]);
}

// ------------
// Geom_CleanCmd
// ------------

PyObject* Geom_CleanCmd(PyObject* self, PyObject* args)
{
  char *srcName;
  char *dstName;
  cvRepositoryData *src;
  cvPolyData *dst;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"ss", &srcName,&dstName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 2 chars: srcName, dstName");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  dst = sys_geom_Clean( (cvPolyData*)src );
  if ( dst == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "error cleaning object" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}

// ----------------
// Geom_CapIdSetCmd
// ----------------

PyObject* Geom_CapIdSetCmd(PyObject* self, PyObject* args)
{
  char *Name;
  char *dstName;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;
  if (!PyArg_ParseTuple(args,"ss", &Name,&dstName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 2 chars: Name, dstName");
    
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( Name );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object " );
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }
  int *doublecaps;
  int numfaces=0;

  if ( sys_geom_set_ids_for_caps( (cvPolyData*)src, (cvPolyData**)(&dst),
			  &doublecaps,&numfaces)
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error setting cap ids" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete [] doublecaps;
    delete dst;
    
  }


  PyObject* pylist = PyList_New(numfaces);
  for (int i=0; i<numfaces; i++){
      PyList_SetItem(pylist, i, PyLong_FromLong(doublecaps[i]));
  }
  delete [] doublecaps;
  return pylist;
}

// ----------------
// Geom_SetArrayForLocalOp_FaceCmd
// ----------------

PyObject* Geom_SetArrayForLocalOp_FaceCmd(PyObject* self, PyObject* args){
  char *Name;
  char *dstName;
  char *arrayName = 0;
  char *outArray = "LocalOpsArray";
  int dataType = 1;
  PyObject* values;

  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"sssO|si", &Name,&dstName,&arrayName,&values,&outArray,&dataType))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 3 chars, \
Name, dstName arrayName, one list, values, \
one optional char, outArray and one int dataType");
    
  }


  // Retrieve source object:
  src = gRepository->GetObject( Name );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object " );
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  int nvals = PyList_Size(values);
  if (nvals == 0) {
      return SV_PYTHON_OK;
  }
  int *vals = new int[nvals];

  for (int i =0; i<nvals;i++)
    vals[i] = PyLong_AsLong(PyList_GetItem(values,i));
  
  if(PyErr_Occurred()!=NULL)
  {
    PyErr_SetString(PyRunTimeErr,"error parsing list");
    
  }
  // Do work of command:

  if ( sys_geom_set_array_for_local_op_face( (cvPolyData*)src,(cvPolyData**)(&dst),arrayName,vals,nvals,outArray,dataType)
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error creating array on surface" );
    delete [] vals;
    
  }

  delete [] vals;

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}

// ----------------
// Geom_SetArrayForLocalOp_SphereCmd
// ----------------

PyObject* Geom_SetArrayForLocalOp_SphereCmd(PyObject* self, PyObject* args)
{
  char *Name;
  char *dstName;
  double radius;
  char *outArray = "LocalOpsArray";
  int dataType = 1;
  double ctr[3];
  int nctr;
  PyObject* ctrList;

  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;
  if (!PyArg_ParseTuple(args,"ssdO|si", &Name,&dstName,&radius,&ctrList,&outArray,&dataType))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 2 chars, \
Name, dstName, one double radius, one list, ctrList, \
one optional char, outArray and one int dataType");
    
  }

  // Retrieve source object:
  src = gRepository->GetObject( Name );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object ");
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  nctr = PyList_Size(ctrList);
  if ( nctr != 3 ) {
    PyErr_SetString(PyRunTimeErr, "sphere requires a 3D center coordinate");
    
  }
  for(int i=0;i<3;i++)
    ctr[i] = PyFloat_AsDouble(PyList_GetItem(ctrList,i));

  if(PyErr_Occurred()!=NULL)
  {
    PyErr_SetString(PyRunTimeErr,"List elements must be doubles");
    
  }
  // Do work of command:

  if ( sys_geom_set_array_for_local_op_sphere( (cvPolyData*)src,(cvPolyData**)(&dst),radius,ctr,outArray,dataType)
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error creating array on surface" );
    
  }

  vtkPolyData *geom = ((cvPolyData*)(dst))->GetVtkPolyData();
  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}

// ----------------
// Geom_SetArrayForLocalOp_CellsCmd
// ----------------

PyObject* Geom_SetArrayForLocalOp_CellsCmd(PyObject* self, PyObject* args)
{
  char *Name;
  char *dstName;
  char *outArray = "LocalOpsArray";
  int dataType = 1;
  PyObject* values;

  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"ssO|si", &Name,&dstName,&values,&outArray,&dataType))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 2 chars, \
Name, dstName , one list, values, \
one optional char, outArray and one int dataType");
    
  }


  // Retrieve source object:
  src = gRepository->GetObject( Name );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object ");
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }


  int nvals = PyList_Size(values);
  if (nvals == 0) {
      return SV_PYTHON_OK;
  }
  int *vals = new int[nvals];

  for (int i =0; i<nvals;i++)
    vals[i] = PyLong_AsLong(PyList_GetItem(values,i));
  
  if(PyErr_Occurred()!=NULL)
  {
    PyErr_SetString(PyRunTimeErr,"error parsing list");
    
  }
  // Do work of command:

  if ( sys_geom_set_array_for_local_op_cells( (cvPolyData*)src,(cvPolyData**)(&dst),vals,nvals,outArray,dataType)
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error creating array on surface" );
    delete [] vals;
    
  }

  delete [] vals;

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}

// ----------------
// Geom_SetArrayForLocalOp_BlendCmd
// ----------------

PyObject* Geom_SetArrayForLocalOp_BlendCmd(PyObject* self, PyObject* args)
{
  char *Name;
  char *dstName;
  char *arrayName = 0;
  char *outArray = "LocalOpsArray";
  int dataType = 1;
  double radius;
  PyObject* values;

  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;
   
  if (!PyArg_ParseTuple(args,"sssOd|si", &Name,&dstName,&arrayName,&values,&radius,&outArray,&dataType))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 3 chars, \
Name, dstName, arrayName , one list, values, one double, radius,\
one optional char, outArray and one int dataType");
    
  }


  // Retrieve source object:
  src = gRepository->GetObject( Name );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object ");
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  if (PyList_Size(values) == 0) {
      return SV_PYTHON_OK;
  }

  int nvals = PyList_Size(values);
  int *vals = new int[nvals];

  for (int i =0; i<nvals;i++)
    vals[i] = PyLong_AsLong(PyList_GetItem(values,i));
  
  if(PyErr_Occurred()!=NULL)
  {
    PyErr_SetString(PyRunTimeErr,"error parsing list");
    
  }
  // Do work of command:

  if ( sys_geom_set_array_for_local_op_face_blend( (cvPolyData*)src,(cvPolyData**)(&dst),arrayName,vals,nvals,radius,outArray,dataType)
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error creating array on surface" );
    delete [] vals;
    
  }

  delete [] vals;

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}

// ----------------
// Geom_LocalDecimationCmd
// ----------------

PyObject* Geom_LocalDecimationCmd(PyObject* self, PyObject* args)
{
  char *Name;
  char *dstName;
  char *pointArrayName = 0;
  char *cellArrayName = 0;
  double target = 0.25;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"ss|dss", &Name,&dstName,&target,&pointArrayName,&cellArrayName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 2 chars, \
Name, dstName , one optional double, target, \
two optional chars, pointArrayName, cellArrayName");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( Name );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object " );
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }
  if ( sys_geom_local_quadric_decimation( (cvPolyData*)src, (cvPolyData**)(&dst),target,
			  pointArrayName,cellArrayName)
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "running local decimation operation" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}

// ----------------
// Geom_LocalLaplacianSmoothCmd
// ----------------

PyObject* Geom_LocalLaplacianSmoothCmd(PyObject* self, PyObject* args)
{
  char *Name;
  char *dstName;
  char *pointArrayName = 0;
  char *cellArrayName = 0;
  int numiters = 100;
  double relax = 0.01;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"ss|idss", &Name,&dstName,&numiters,&relax,&pointArrayName,&cellArrayName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 2 chars, \
Name, dstName , one optional int, numiters, one optional double, relax, \
two optional chars, pointArrayName, cellArrayName");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( Name );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object " );
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }
  if ( sys_geom_local_laplacian_smooth( (cvPolyData*)src, (cvPolyData**)(&dst),
			  numiters,relax,
			  pointArrayName,cellArrayName)
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "running local decimation operation" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}

// ----------------
// Geom_LocalConstrainSmoothCmd
// ----------------

PyObject* Geom_LocalConstrainSmoothCmd(PyObject* self, PyObject* args)
{
  char *Name;
  char *dstName;
  char *pointArrayName = 0;
  char *cellArrayName = 0;
  int numiters = 5;
  double constrainfactor = 0.7;
  int numcgsolves = 30;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"ss|idiss", &Name,&dstName,&numiters,&constrainfactor, &numcgsolves,&pointArrayName,&cellArrayName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 2 chars, \
Name, dstName , one optional int, numiters, \
one optional double constrainfactor, \
one optional int numcgsolves,\
two optional chars, pointArrayName, cellArrayName");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( Name );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object " );
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }
  if ( sys_geom_local_constrain_smooth( (cvPolyData*)src, (cvPolyData**)(&dst),
			  numiters,constrainfactor,numcgsolves,
			  pointArrayName,cellArrayName)
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "running local decimation operation" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}

// ----------------
// Geom_LocalLinearSubdivisionCmd
// ----------------
//
PyObject* Geom_LocalLinearSubdivisionCmd(PyObject* self, PyObject* args)
{
  char *Name;
  char *dstName;
  char *pointArrayName = 0;
  char *cellArrayName = 0;
  int numiters = 100;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"ss|iss", &Name,&dstName,&numiters,&pointArrayName,&cellArrayName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 2 chars, \
Name, dstName , one optional int, numiters, \
two optional chars, pointArrayName, cellArrayName");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( Name );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object ");
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }
  if ( sys_geom_local_linear_subdivision( (cvPolyData*)src, (cvPolyData**)(&dst),
			  numiters,pointArrayName,cellArrayName)
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "running local subdivision operation" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}

// ----------------
// Geom_LocalButterflySubdivisionCmd
// ----------------
//
PyObject* Geom_LocalButterflySubdivisionCmd(PyObject* self, PyObject* args)
{
  char *Name;
  char *dstName;
  char *pointArrayName = 0;
  char *cellArrayName = 0;
  int numiters = 100;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"ss|iss", &Name,&dstName,&numiters,&pointArrayName,&cellArrayName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 2 chars, \
Name, dstName , one optional int, numiters, \
two optional chars, pointArrayName, cellArrayName");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( Name );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object ");
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }
  if ( sys_geom_local_butterfly_subdivision( (cvPolyData*)src, (cvPolyData**)(&dst),
			  numiters,pointArrayName,cellArrayName)
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "running local subdivision operation" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}

// ----------------
// Geom_LocalLoopSubdivisionCmd
// ----------------
//
PyObject* Geom_LocalLoopSubdivisionCmd(PyObject* self, PyObject* args)
{
  char *Name;
  char *dstName;
  char *pointArrayName = 0;
  char *cellArrayName = 0;
  int numiters = 100;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"ss|iss", &Name,&dstName,&numiters,&pointArrayName,&cellArrayName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 2 chars, \
Name, dstName , one optional int, numiters, \
two optional chars, pointArrayName, cellArrayName");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( Name );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object ");
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }
  if ( sys_geom_local_loop_subdivision( (cvPolyData*)src, (cvPolyData**)(&dst),
			  numiters,pointArrayName,cellArrayName)
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "running local subdivision operation" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}

// ----------------
// Geom_LocalBlendCmd
// ----------------
//
PyObject* Geom_LocalBlendCmd(PyObject* self, PyObject* args)
{
  char *Name;
  char *dstName;
  char *pointArrayName = 0;
  char *cellArrayName = 0;
  int numblenditers = 2;
  int numsubblenditers = 2;
  int numsubdivisioniters = 1;
  int numcgsmoothiters = 3;
  int numlapsmoothiters = 50;
  double targetdecimation = 0.01;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"ss|iiiiidss", &Name,&dstName,&numblenditers,
&numsubdivisioniters,&numcgsmoothiters,&numlapsmoothiters,&targetdecimation,
&pointArrayName,&cellArrayName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import 2 chars, \
Name, dstName ,\
five optional ints, numblenditers,numsubdivisioniters,numcgsmoothiters,numlapsmoothiters,targetdecimation, \
two optional chars, pointArrayName, cellArrayName");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( Name );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object ");
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }
  if ( sys_geom_local_blend( (cvPolyData*)src, (cvPolyData**)(&dst),
			  numblenditers,numsubblenditers,
			  numsubdivisioniters, numcgsmoothiters,
			  numlapsmoothiters, targetdecimation,
			  pointArrayName,cellArrayName)
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "running local blend operation" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}

// -------------
// Geom_All_UnionCmd
// -------------

PyObject* Geom_All_UnionCmd(PyObject* self, PyObject* args)
{
  int numSrcs;
  int interT;
  PyObject* srcList;
  char *dstName;
  cvRepositoryData *src;
  cvPolyData *dst;
  RepositoryDataT type;
  cvPolyData **srcs;
  cvSolidModel *geom;
  double tolerance = 1e-5;

  if (!PyArg_ParseTuple(args,"Ois|d", &srcList,&interT,&dstName,&tolerance))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one list srcList,\
 one int, interT, one char, dstName and one optional double, tolerance");
    
  }


  // Do work of command:
  numSrcs = PyList_Size(srcList);

  // Foreach src obj, check that it is in the repository and of the
  // correct type (i.e. cvSolidModel).  Also build up the array of
  // cvSolidModel*'s to pass to cvSolidModel::MakeLoftedSurf.

  srcs = new cvPolyData * [numSrcs];

  for (int i = 0; i < numSrcs; i++ ) {
    char* str;
#if PYTHON_MAJOR_VERSION == 2
    str = PyString_AsString(PyList_GetItem(srcList,i));
#endif
#if PYTHON_MAJOR_VERSION ==3
    str = PyBytes_AsString(PyUnicode_AsUTF8String(PyList_GetItem(srcList,i)));
#endif
    src = gRepository->GetObject(str);
    if ( src == NULL ) {
      PyErr_SetString(PyRunTimeErr,  "couldn't find object " );
      delete [] srcs;
      
    }
    type = src->GetType();
    if ( type != POLY_DATA_T ) {
      PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
      delete [] srcs;
      
    }
    srcs[i] = (cvPolyData *) src;
  }

  // We're done with the src object names:

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    delete [] srcs;
    
  }

  // Instantiate the new solid:
  geom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if ( geom == NULL ) {
    delete [] srcs;
    
  }

  if ( sys_geom_all_union( srcs, numSrcs,interT,tolerance,(cvPolyData**)(&dst) )
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "poly manipulation error" );
    delete dst;
    delete [] srcs;
    delete geom;
    
  }

  vtkPolyData *dstPd;
  dstPd = dst->GetVtkPolyData();
  geom->SetVtkPolyDataObject(dstPd);
  if ( !( gRepository->Register( dstName, geom ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete geom;
    delete [] srcs;
    delete dst;
    
  }

  return Py_BuildValue("s",geom->GetName());
}

// -------------
// Geom_Convert_NURBS_To_PolyCmd
// -------------

PyObject* Geom_Convert_NURBS_To_PolyCmd(PyObject* self, PyObject* args)
{
  int numFaces;
  int numIds;
  int *allids;
  int interT;
  PyObject* faceList;
  PyObject* idList;
  char *srcName;
  char *dstName;
  cvRepositoryData *face;
  cvPolyData *dst;
  cvRepositoryData *model;
  RepositoryDataT type;
  cvPolyData **faces;
  cvSolidModel *geom;

  if (!PyArg_ParseTuple(args,"sOOs", &srcName,&faceList,&idList,&dstName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one char srcName,\
 two lists facelist and idlist, and one char dstName");
    
  }


  // Do work of command:
  numFaces = PyList_Size(faceList);
  numIds = PyList_Size(idList);

  if (numFaces != numIds)
  {
      PyErr_SetString(PyRunTimeErr,  "Number of Ids must equal number of faces!");
      
  }

  // Foreach src obj, check that it is in the repository and of the
  // correct type (i.e. cvSolidModel).  Also build up the array of
  // cvSolidModel*'s to pass to cvSolidModel::MakeLoftedSurf.

  faces = new cvPolyData * [numFaces];

  for (int i = 0; i < numFaces; i++ ) {
    char* str;
#if PYTHON_MAJOR_VERSION == 2
    str = PyString_AsString(PyList_GetItem(faceList,i));
#endif
#if PYTHON_MAJOR_VERSION ==3
    str = PyBytes_AsString(PyUnicode_AsUTF8String(PyList_GetItem(faceList,i)));
#endif
    face=gRepository->GetObject(str);
    if ( face == NULL ) {
      PyErr_SetString(PyRunTimeErr,  "couldn't find object ");
      delete [] faces;
      
    }
    type = face->GetType();
    if ( type != POLY_DATA_T ) {
      PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
      delete [] faces;
      
    }
    faces[i] = (cvPolyData *) face;
  }

  // Create an array, and for each id insert it into the array
  allids = new int[numIds];
  for (int i=0; i<numIds;i++)
    allids[i]=PyLong_AsLong(PyList_GetItem(idList,i));

  if(PyErr_Occurred()!=NULL)
  {
    PyErr_SetString(PyRunTimeErr, "Error parsing list objects");
    
  }
  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    delete [] faces;
    delete [] allids;
    
  }

  // Retrieve cvPolyData source:
  model = gRepository->GetObject( srcName );
  if ( model == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    delete [] faces;
    delete [] allids;
    
  }
  type = model->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "object not of type cvPolyData");
    delete [] faces;
    delete [] allids;
    
  }

  // We're done with the src object names:

  // Instantiate the new solid:
  geom = cvSolidModel::pyDefaultInstantiateSolidModel( );
  if ( geom == NULL ) {
    delete [] faces;
    delete [] allids;
    
  }

  if ( sys_geom_assign_ids_based_on_faces((cvPolyData *)model,faces,numFaces,allids,(cvPolyData**)(&dst) )
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "poly manipulation error" );
    delete dst;
    delete [] faces;
    delete [] allids;
    
  }

  delete [] faces;
  delete [] allids;

  vtkPolyData *dstPd;
  dstPd = dst->GetVtkPolyData();
  geom->SetVtkPolyDataObject(dstPd);
  if ( !( gRepository->Register( dstName, geom ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

 
  return Py_BuildValue("s",geom->GetName());
}

// ---------------------------
// Geom_MakePolysConsistentCmd
// ---------------------------

PyObject* Geom_MakePolysConsistentCmd(PyObject* self, PyObject* args)
{
  char *srcName;
  char *dstName;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"ss", &srcName,&dstName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import two chars, srcName, dstName");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  if ( sys_geom_MakePolysConsistent( (cvPolyData*)src, (cvPolyData**)(&dst) )
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "poly manipulation error" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }


  return Py_BuildValue("s",dst->GetName());
}


// -----------------------
// Geom_ReverseAllCellsCmd
// -----------------------

PyObject* Geom_ReverseAllCellsCmd(PyObject* self, PyObject* args)
{
  char *srcName;
  char *dstName;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"ss", &srcName,&dstName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import two chars, srcName, dstName");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  if ( sys_geom_ReverseAllCells( (cvPolyData*)src, (cvPolyData**)(&dst) )
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "cell reversal error" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }


  return Py_BuildValue("s",dst->GetName());
}


// ----------------------------
// Geom_NumClosedLineRegionsCmd
// ----------------------------

PyObject* Geom_NumClosedLineRegionsCmd(PyObject* self, PyObject* args)
{
  char *srcName;
  cvRepositoryData *src;
  RepositoryDataT type;
  int num;
  if (!PyArg_ParseTuple(args,"s", &srcName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one chars, srcName");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  if ( sys_geom_NumClosedLineRegions( (cvPolyData*)src, &num ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "region count failed" );
    
  }


  return Py_BuildValue("i",PyLong_FromLong(num));
}


// ---------------------------
// Geom_GetClosedLineRegionCmd
// ---------------------------

PyObject* Geom_GetClosedLineRegionCmd(PyObject* self, PyObject* args)
{
  char *srcName;
  char *dstName;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  int id;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"sis", &srcName,&id,&dstName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import two chars and one int, srcName, id, dstName");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }
  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  // Make sure the specified destination does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  if ( sys_geom_GetClosedLineRegion( (cvPolyData*)src, id, (cvPolyData**)(&dst) )
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "region retrieval failed" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}


// ------------
// Geom_PickCmd
// ------------

PyObject* Geom_PickCmd(PyObject* self, PyObject* args)
{
  char *objName;
  PyObject* posList;
  char *resultName;
  cvRepositoryData *obj;
  cvRepositoryData *result = NULL;
  RepositoryDataT type;
  double pos[3];

  if (!PyArg_ParseTuple(args,"sOs", &objName,&posList,&resultName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import two chars and one list, objName, posList and resultName");
    
  }


  // Convert given coordinate to double's:
  if ( PyList_Size(posList) != 3 ) {
    PyErr_SetString(PyRunTimeErr,  "list must have three elements"  );
    
  }
  for (int i=0;i<3;i++)
    pos[i] = PyFloat_AsDouble(PyList_GetItem(posList,i));

  if(PyErr_Occurred()!=NULL)
  {
    PyErr_SetString(PyRunTimeErr,"list elements must be doubles");
    
  }

  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  // Make sure the specified result does not exist:
  if ( gRepository->Exists( resultName ) ) {
    PyErr_SetString(PyRunTimeErr,  "object  already exists" );
    
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  if ( sys_geom_Pick( (cvPolyData*)obj, pos, (cvPolyData**)(&result) )
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "no geometry containing given pos found");
    
  }

  if ( !( gRepository->Register( resultName, result ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository" );
    delete result;
    
  }

  return SV_PYTHON_OK;
}


// ---------------------
// Geom_OrientProfileCmd
// ---------------------

PyObject* Geom_OrientProfileCmd(PyObject* self, PyObject* args)
{
  char *srcName;

  PyObject* pathPosList;
  PyObject* pathTanList;
  PyObject* pathXhatList;
  double ppt[3];
  double ptan[3];
  double xhat[3];
  int n;

  char *dstName;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"sOOOs", &srcName,&pathPosList,&pathTanList,&pathXhatList,&dstName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import two chars and three lists,\
 srcName, pathPosList, pathTanlist, pathXhatList,dstName");
    
  }


  // Parse lists
  // -----------
  n = PyList_Size(pathPosList);
  if(n!=3)
  {
    PyErr_SetString(PyRunTimeErr, "path_pos: 3d coordinate" );
    
  }
  n = PyList_Size(pathTanList);
  if(n!=3)
  {
    PyErr_SetString(PyRunTimeErr, "path_tan: 3d vector" );
    
  }
  n = PyList_Size(pathXhatList);
  if(n!=3)
  {
    PyErr_SetString(PyRunTimeErr, "path_xhat: 3d vector" );
    
  }

  for (int i=0;i<3;i++)
  {
    ppt[i]=PyFloat_AsDouble(PyList_GetItem(pathPosList,i));
    ptan[i]=PyFloat_AsDouble(PyList_GetItem(pathTanList,i));
    xhat[i] =PyFloat_AsDouble(PyList_GetItem(pathXhatList,i));
  }
  if(PyErr_Occurred()!=NULL)
  {
    PyErr_SetString(PyRunTimeErr,"List elements must be doubles");
    
  }


  // Do work of command
  // ------------------

  // Retrieve src object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  // Make sure the specified dst does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  // Check src type:
  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  if ( sys_geom_OrientProfile( (cvPolyData*)src, ppt, ptan, xhat,
			       (cvPolyData**)(&dst) ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "orient error" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}


// ------------------------
// Geom_DisorientProfileCmd
// ------------------------

PyObject* Geom_DisorientProfileCmd(PyObject* self, PyObject* args)
{
  char *srcName;

  PyObject* pathPosList;
  PyObject* pathTanList;
  PyObject* pathXhatList;
  double ppt[3];
  double ptan[3];
  double xhat[3];
  int n;

  char *dstName;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"sOOOs", &srcName,&pathPosList,&pathTanList,&pathXhatList,&dstName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import two chars and three lists,\
 srcName, pathPosList, pathTanlist, pathXhatList,dstName");
    
  }

  // Parse lists
  // -----------
  n = PyList_Size(pathPosList);
  if(n!=3)
  {
    PyErr_SetString(PyRunTimeErr, "path_pos: 3d coordinate" );
    
  }
  n = PyList_Size(pathTanList);
  if(n!=3)
  {
    PyErr_SetString(PyRunTimeErr, "path_tan: 3d vector" );
    
  }
  n = PyList_Size(pathXhatList);
  if(n!=3)
  {
    PyErr_SetString(PyRunTimeErr, "path_xhat: 3d vector" );
    
  }

  for (int i=0;i<3;i++)
  {
    ppt[i]=PyFloat_AsDouble(PyList_GetItem(pathPosList,i));
    ptan[i]=PyFloat_AsDouble(PyList_GetItem(pathTanList,i));
    xhat[i] =PyFloat_AsDouble(PyList_GetItem(pathXhatList,i));
  }
  if(PyErr_Occurred()!=NULL)
  {
    PyErr_SetString(PyRunTimeErr,"List elements must be doubles");
    
  }
  // Do work of command
  // ------------------

  // Retrieve src object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  // Make sure the specified dst does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  // Check src type:
  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  if ( sys_geom_DisorientProfile( (cvPolyData*)src, ppt, ptan, xhat,
				  (cvPolyData**)(&dst) ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "orient error" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}


// --------------------
// Geom_AlignProfileCmd
// --------------------

PyObject* Geom_AlignProfileCmd(PyObject* self, PyObject* args)
{
  char *refName;
  char *srcName;
  char *dstName;
  int vecMtd = 1;
  cvRepositoryData *ref;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"sssi", &refName,&srcName,&dstName,&vecMtd))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import three chars and one int,\
 refName, srcName,dstName,vecMtd");
    
  }


  // Do work of command
  // ------------------

  // Retrieve ref object and check type:
  ref = gRepository->GetObject( refName );
  if ( ref == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object ");
    
  }
  type = ref->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  // Retrieve src object and check type:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }
  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  // Make sure the specified dst does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  if ( vecMtd ) {
    dst = sys_geom_Align( (cvPolyData*)ref, (cvPolyData*)src );
  } else {
    dst = sys_geom_AlignByDist( (cvPolyData*)ref, (cvPolyData*)src );
  }
  if ( dst == NULL ) {
    PyErr_SetString(PyRunTimeErr, "alignment error" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}


// -----------------
// Geom_TranslateCmd
// -----------------

PyObject* Geom_TranslateCmd(PyObject* self, PyObject* args)
{
  char *srcName;

  PyObject* vecList;
  double vec[3];
  int n;

  char *dstName;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"sOs", &srcName,&vecList,&dstName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import two chars and one list,\
srcName,vecList,dstName");
    
  }


  // Parse lists
  // -----------
  n = PyList_Size(vecList);
  if(n!=3){  
    PyErr_SetString(PyRunTimeErr, "vec: 3d coordinate" );
    
  }

  for(int i=0;i<3;i++)
    vec[i] = PyFloat_AsDouble(PyList_GetItem(vecList,i));

  if(PyErr_Occurred()!=NULL)
  {
    PyErr_SetString(PyRunTimeErr,"List elements must be doubles");
    
  }

  // Do work of command
  // ------------------

  // Retrieve src object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  // Make sure the specified dst does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  // Check src type:
  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  if ( sys_geom_Translate( (cvPolyData*)src, vec, (cvPolyData**)(&dst) )
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "translate error" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}


// ----------------
// Geom_ScaleAvgCmd
// ----------------

PyObject* Geom_ScaleAvgCmd(PyObject* self, PyObject* args)
{
  char *srcName;
  char *dstName;
  double factor;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"sds", &srcName,&factor,&dstName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import two chars and one double,\
srcName,factor,dstName");
    
  }



  // Do work of command
  // ------------------

  // Retrieve src object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  // Make sure the specified dst does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  // Check src type:
  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  if ( sys_geom_ScaleAvg( (cvPolyData*)src, factor, (cvPolyData**)(&dst) )
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error scaling about average point" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}


// ---------------------
// Geom_GetOrderedPtsCmd
// ---------------------

PyObject* Geom_GetOrderedPtsCmd(PyObject* self, PyObject* args)
{
  char *objName;
  cvRepositoryData *obj;
  RepositoryDataT type;
  double *pts;
  int i, num;
  char dummy[CV_STRLEN];

  if (!PyArg_ParseTuple(args,"s", &objName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one char objName");
    
  }


  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  // Check type:
  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  if ( sys_geom_GetOrderedPts( (cvPolyData*)obj, &pts, &num ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "point query error" );
    
  }
  
  sprintf(dummy,"");
  PyObject* pylist = PyList_New(num);
  for (int i = 0; i < num; i++ ) {
    PyObject* tmplist = PyList_New(3);
    PyList_SetItem(tmplist, 0, PyFloat_FromDouble(pts[3*i]));
    PyList_SetItem(tmplist, 1, PyFloat_FromDouble(pts[3*i+1]));
    PyList_SetItem(tmplist, 2, PyFloat_FromDouble(pts[3*i+2]));
    PyList_SetItem(pylist, i, tmplist);
  }
  delete [] pts;

  return pylist;
}


// -----------------------
// Geom_WriteOrderedPtsCmd
// -----------------------

PyObject* Geom_WriteOrderedPtsCmd(PyObject* self, PyObject* args)
{
  char *objName;
  char *fileName;
  cvRepositoryData *obj;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"ss", &objName,&fileName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import two chars objName, fileName");
    
  }


  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  // Check type:
  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  if ( sys_geom_WriteOrderedPts( (cvPolyData*)obj, fileName ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "geometry write error" );
    
  }

  return SV_PYTHON_OK;
}


// ------------------
// Geom_WriteLinesCmd
// ------------------

PyObject* Geom_WriteLinesCmd(PyObject* self, PyObject* args)
{
  char *objName;
  char *fileName;
  cvRepositoryData *obj;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"ss", &objName,&fileName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import two chars objName, fileName");
    
  }


  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  // Check type:
  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  if ( sys_geom_WriteLines( (cvPolyData*)obj, fileName ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "file write error" );
    
  }

  return SV_PYTHON_OK;
}


// -------------------
// Geom_PolysClosedCmd
// -------------------

PyObject* Geom_PolysClosedCmd(PyObject* self, PyObject* args)
{
  char *srcName;
  cvRepositoryData *src;
  RepositoryDataT type;
  int closed;

  if (!PyArg_ParseTuple(args,"s", &srcName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one chars srcName");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  if ( sys_geom_PolysClosed( (cvPolyData*)src, &closed ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "polygon traversal error" );
    
  }

  
  return Py_BuildValue("N", PyBool_FromLong(closed));

}


// ----------------
// Geom_SurfAreaCmd
// ----------------

PyObject* Geom_SurfAreaCmd(PyObject* self, PyObject* args)
{
  char *srcName;
  cvRepositoryData *src;
  RepositoryDataT type;
  double area;

  if (!PyArg_ParseTuple(args,"s", &srcName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one chars srcName");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  if ( sys_geom_SurfArea( (cvPolyData*)src, &area ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "surface area computation error" );
    
  }


  return Py_BuildValue("d",area);
}


// -----------------------
// Geom_GetPolyCentroidCmd
// -----------------------

PyObject* Geom_GetPolyCentroidCmd(PyObject* self, PyObject* args)
{
  char *srcName;
  cvRepositoryData *src;
  RepositoryDataT type;
  double centroid[3];

  if (!PyArg_ParseTuple(args,"s", &srcName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one chars srcName");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  if ( sys_geom_getPolyCentroid( (cvPolyData*)src, centroid) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "polygon centroid computation error" );
    
  }

  return Py_BuildValue("ddd",centroid[0], centroid[1], centroid[2]);
}


// ---------------------
// Geom_PrintTriStatsCmd
// ---------------------

PyObject* Geom_PrintTriStatsCmd(PyObject* self, PyObject* args)
{
  char *surfName;
  cvRepositoryData *surf;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"s", &surfName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one char surfName");
    
  }


  // Do work of command:

  // Retrieve source object:
  surf = gRepository->GetObject( surfName );
  if ( surf == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object " );
    
  }

  type = surf->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, " object not of type cvPolyData");
    
  }

  if ( sys_geom_PrintTriStats( (cvPolyData*)surf ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "surface processing error" );
    
  }

  return SV_PYTHON_OK;
}


// -----------------------
// Geom_PrintSmallPolysCmd
// -----------------------

PyObject* Geom_PrintSmallPolysCmd(PyObject* self, PyObject* args)
{
  char *srcName;
  double sideTol;
  cvRepositoryData *src;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"sd", &srcName,&sideTol))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one char and one double srcName, sideTol");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  if ( sys_geom_PrintSmallPolys( (cvPolyData*)src, sideTol ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "polygon processing error" );
    
  }

  return SV_PYTHON_OK;
}


// --------------------
// Geom_RmSmallPolysCmd
// --------------------

PyObject* Geom_RmSmallPolysCmd(PyObject* self, PyObject* args)
{
  char *srcName;
  char *dstName;
  double sideTol;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"ssd", &srcName,&dstName,&sideTol))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import two chars and one double srcName,dstName, sideTol");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  if ( sys_geom_RmSmallPolys( (cvPolyData*)src, sideTol, (cvPolyData**)(&dst) )
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "polygon processing error" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}


// ------------
// Geom_BBoxCmd
// ------------

PyObject* Geom_BBoxCmd(PyObject* self, PyObject* args)
{
  char *objName;
  cvRepositoryData *obj;
  RepositoryDataT type;
  double bbox[6];
  char dummy[CV_STRLEN];
  int i;

  if (!PyArg_ParseTuple(args,"s", &objName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one char objName");
    
  }


  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  if ( sys_geom_BBox( (cvPolyData*)obj, bbox ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "bounding box query error" );
    
  }

  PyObject* pylist = PyList_New(6);
  for (int i = 0; i < 6; i++ ) {
      PyList_SetItem(pylist, i, PyFloat_FromDouble(bbox[i]));
  }

  return pylist;

}


// ----------------
// Geom_ClassifyCmd
// ----------------

PyObject* Geom_ClassifyCmd(PyObject* self, PyObject* args)
{
  char *objName;
  cvRepositoryData *obj;
  RepositoryDataT type;
  PyObject* ptList;
  double pt[3];
  int npt;
  int ans;

  if (!PyArg_ParseTuple(args,"sO", &objName,&ptList))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one char and one list objName, ptList");
    
  }


  // Parse coordinate list:
  npt = PyList_Size(ptList);

  if ( npt != 3 ) {
    PyErr_SetString(PyRunTimeErr, "only valid for 3d objects and queries");
    
  }
  for (int i=0; i<3;i++)
    pt[i] = PyFloat_AsDouble(PyList_GetItem(ptList,i));

  if(PyErr_Occurred()!=NULL){
    PyErr_SetString(PyRunTimeErr,"list elements must be doubles");
    
  }
  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  if ( sys_geom_Classify( (cvPolyData*)obj, pt, &ans ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "classify error" );
    
  }

  return Py_BuildValue("i",ans);
}


// ----------------
// Geom_PtInPolyCmd
// ----------------

PyObject* Geom_PtInPolyCmd(PyObject* self, PyObject* args)
{
  char *objName;
  cvRepositoryData *obj;
  RepositoryDataT type;
  PyObject* ptList;
  double pt[3];
  int npt;
  int ans;
  int usePrevPoly = 0;


  if (!PyArg_ParseTuple(args,"sOi", &objName,&ptList,&usePrevPoly))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one char, one list and one int, objName, ptList, usePrevPoly");
    
  }


  // Parse coordinate list:
  npt=PyList_Size(ptList);
  if ( npt != 2 ) {
    PyErr_SetString(PyRunTimeErr, "pt must be two-dimensional" );
    
  }
  for (int i=0; i<2; i++)
    pt[i] = PyFloat_AsDouble(PyList_GetItem(ptList,i));
  //The tcl script parsed a 3d pt while requesting a 2d pt
  if(PyErr_Occurred()!=NULL){
    PyErr_SetString(PyRunTimeErr,"List elements must be doubles");
    
  }
  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  if ( sys_geom_PtInPoly( (cvPolyData*)obj, pt,usePrevPoly, &ans ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "point-in-poly error" );
    
  }

  return Py_BuildValue("i",ans);
}


// ----------------
// Geom_MergePtsCmd
// ----------------

PyObject* Geom_MergePtsCmd(PyObject* self, PyObject* args)
{
  char *srcName;
  char *dstName;
  cvRepositoryData *src;
  RepositoryDataT type;
  cvPolyData *dst;
  double tol = 1e10 * FindMachineEpsilon();

  if (!PyArg_ParseTuple(args,"ssd", &srcName,&dstName,&tol))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import two chars and one double srcName, dstName,tol");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  dst = sys_geom_MergePts_tol( (cvPolyData*)src, tol );

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}


// -----------------
// Geom_Warp3dPtsCmd
// -----------------

PyObject* Geom_Warp3dPtsCmd(PyObject* self, PyObject* args)
{
  char *srcName;
  char *dstName;
  cvRepositoryData *src;
  RepositoryDataT type;
  cvPolyData *dst;
  double scale = 1.0;

  if (!PyArg_ParseTuple(args,"ssd", &srcName,&dstName,&scale))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import two chars and one double srcName, dstName,scale");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  dst = sys_geom_warp3dPts( (cvPolyData*)src, scale );

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}


// --------------
// Geom_NumPtsCmd
// --------------

PyObject* Geom_NumPtsCmd(PyObject* self, PyObject* args)
{
  char *srcName;
  cvRepositoryData *src;
  RepositoryDataT type;
  int num;

  if (!PyArg_ParseTuple(args,"s", &srcName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one char srcName");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  num = ((cvPolyData*)src)->GetVtkPolyData()->GetNumberOfPoints();


  return Py_BuildValue("i", num);
}


// ---------------------
// Geom_sampleLoopCmd
// ---------------------

PyObject* Geom_sampleLoopCmd(PyObject* self, PyObject* args)
{
  char *srcName;
  int targetNumPts;
  char *dstName;
  cvRepositoryData *src;
  RepositoryDataT type;
  cvPolyData *dst;

  if (!PyArg_ParseTuple(args,"sis", &srcName,&targetNumPts,&dstName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import two chars and one int srcName, targetNumPts,dstName");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  dst = sys_geom_sampleLoop( (cvPolyData*)src, targetNumPts );

  if ( dst == NULL ) {
    PyErr_SetString(PyRunTimeErr, "subsample loop error" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}

// ---------------------
// Geom_loftSolidCmd
// ---------------------

PyObject* Geom_loftSolidCmd(PyObject* self, PyObject* args)
{
  int numSrcs;
  PyObject* srcList;
  char *dstName;
  cvRepositoryData *src;
  cvPolyData *dst;
  RepositoryDataT type;
  cvPolyData **srcs;
  int numOutPtsInSegs;
  int numOutPtsAlongLength;
  int numModes;
  int useFFT;
  int useLinearSampleAlongLength;
  int numLinearPtsAlongLength;
  int splineType = 0;
  double continuity = 0;
  double bias = 0;
  double tension = 0;
  if (!PyArg_ParseTuple(args,"Osiiiiii|iddd", &srcList,&dstName,&numOutPtsInSegs,
&numOutPtsAlongLength,&numLinearPtsAlongLength,&numModes,&useFFT,
&useLinearSampleAlongLength,&splineType,&bias,&tension,&continuity))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one list,one char,\
six ints, and one optional int, three optional doubles,\
srcList, dstName, numOutPtsInSegs,numOutPtsAlongLength, numLinearPtsAlongLength,\
numModes,useFFT, useLinearSampleAlongLength, splineType, bias,tension, continuity");
    
  }

  // Do work of command:
  numSrcs = PyList_Size(srcList);

  // Foreach src obj, check that it is in the repository and of the
  // correct type (i.e. cvSolidModel).  Also build up the array of
  // cvSolidModel*'s to pass to cvSolidModel::MakeLoftedSurf.
  srcs = new cvPolyData * [numSrcs];

  for (int i = 0; i < numSrcs; i++ ) {
    char* str;
#if PYTHON_MAJOR_VERSION == 2
    str = PyString_AsString(PyList_GetItem(srcList,i));
#endif
#if PYTHON_MAJOR_VERSION ==3
    str = PyBytes_AsString(PyUnicode_AsUTF8String(PyList_GetItem(srcList,i)));
#endif
    src = gRepository->GetObject(str);
    if ( src == NULL ) {
      PyErr_SetString(PyRunTimeErr,  "couldn't find object ");
      delete [] srcs;
      
    }
    type = src->GetType();
    if ( type != POLY_DATA_T ) {
      PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
      delete [] srcs;
      
    }
    srcs[i] = (cvPolyData *) src;
  }
  // We're done with the src object names:

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    delete [] srcs;
    
  }
  if ( sys_geom_loft_solid( srcs, numSrcs,useLinearSampleAlongLength,useFFT,
			  numOutPtsAlongLength,numOutPtsInSegs,
			  numLinearPtsAlongLength,numModes,splineType,bias,tension,continuity,
			  (cvPolyData**)(&dst) )
       != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "poly manipulation error" );
    delete dst;
    delete [] srcs;
    
  }
  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}

// ---------------------
// Geom_loftSolidWithNURBSCmd
// ---------------------

PyObject* Geom_loftSolidWithNURBSCmd(PyObject* self, PyObject* args)
{
  int numSrcs;
  PyObject* srcList;
  char *dstName;
  cvRepositoryData *src;
  cvPolyData *dst;
  RepositoryDataT type;
  cvPolyData **srcs;
  int uDegree = 2;
  int vDegree = 2;
  double uSpacing = 0.01;
  double vSpacing = 0.01;
  char *uKnotSpanType;
  char *vKnotSpanType;
  char *uParametricSpanType;
  char *vParametricSpanType;

  if (!PyArg_ParseTuple(args,"Osiiddssss", &srcList,&dstName,&uDegree,
&vDegree,&uSpacing,&vSpacing,&uKnotSpanType,&vKnotSpanType,
&uParametricSpanType,&vParametricSpanType))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one list, one char, two ints, two doubles and four chars\
srcList,dstName,uDegree,\
vDegree,uSpacing,vSpacing,uKnotSpanType,vKnotSpanType,\
uParametricSpanType,vParametricSpanType");
    
  }


  // Do work of command:
  numSrcs = PyList_Size(srcList);

  // Foreach src obj, check that it is in the repository and of the
  // correct type (i.e. cvSolidModel).  Also build up the array of
  // cvSolidModel*'s to pass to cvSolidModel::MakeLoftedSurf.

  srcs = new cvPolyData * [numSrcs];

  for (int i = 0; i < numSrcs; i++ ) {
    char* str;
#if PYTHON_MAJOR_VERSION == 2
    str = PyString_AsString(PyList_GetItem(srcList,i));
#endif
#if PYTHON_MAJOR_VERSION ==3
    str = PyBytes_AsString(PyUnicode_AsUTF8String(PyList_GetItem(srcList,i)));
#endif
    src = gRepository->GetObject(str);
    if ( src == NULL ) {
      PyErr_SetString(PyRunTimeErr,  "couldn't find object ");
      delete [] srcs;
      
    }
    type = src->GetType();
    if ( type != POLY_DATA_T ) {
      PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
      delete [] srcs;
      
    }
    srcs[i] = (cvPolyData *) src;
  }

  // We're done with the src object names:

  // Make sure the specified result object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    delete [] srcs;
    
  }

  vtkSmartPointer<vtkSVNURBSSurface> NURBSSurface =
    vtkSmartPointer<vtkSVNURBSSurface>::New();
  if ( sys_geom_loft_solid_with_nurbs(srcs, numSrcs, uDegree, vDegree, uSpacing,
                                      vSpacing, uKnotSpanType, vKnotSpanType,
                                      uParametricSpanType, vParametricSpanType,
                                      NURBSSurface,
			                                (cvPolyData**)(&dst) ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "poly manipulation error" );
    delete dst;
    delete [] srcs;
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}


// --------------------
// Geom_2dWindingNumCmd
// --------------------

PyObject* Geom_2dWindingNumCmd(PyObject* self, PyObject* args)
{
  char *objName;
  cvRepositoryData *obj;
  RepositoryDataT type;
  int wnum;

  if (!PyArg_ParseTuple(args,"s", &objName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one char objName");
    
  }


  // Do work of command:

  // Retrieve source object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  wnum = sys_geom_2DWindingNum( (cvPolyData*)obj );


  return Py_BuildValue("i",wnum);
}


// -------------------
// Geom_PolygonNormCmd
// -------------------

PyObject* Geom_PolygonNormCmd(PyObject* self, PyObject* args)
{
  char *objName;
  cvRepositoryData *obj;
  RepositoryDataT type;
  double n[3];

  if (!PyArg_ParseTuple(args,"s", &objName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one char objName");
    
  }


  // Do work of command:

  // Retrieve source object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  if ( sys_geom_PolygonNormal( (cvPolyData*)obj, n ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr,  "error computing normal");
    
  }

  return Py_BuildValue("ddd",n[0],n[1],n[2]);
}


// -------------
// Geom_AvgPtCmd
// -------------

PyObject* Geom_AvgPtCmd(PyObject* self, PyObject* args)
{
  char *objName;
  cvRepositoryData *obj;
  RepositoryDataT type;
  double pt[3];

  if (!PyArg_ParseTuple(args,"s", &objName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one char objName");
    
  }


  // Do work of command:

  // Retrieve source object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  if ( sys_geom_AvgPt( (cvPolyData*)obj, pt ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr,  "error averaging points");
    
  }

  return Py_BuildValue("ddd",pt[0], pt[1], pt[2]);
}


// ------------
// Geom_CopyCmd
// ------------

PyObject* Geom_CopyCmd(PyObject* self, PyObject* args)
{
  char *srcName;
  char *dstName;
  cvRepositoryData *src;
  cvPolyData *dst;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"ss", &srcName,&dstName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import two chars srcName,dstName");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  dst = sys_geom_DeepCopy( (cvPolyData*)src );
  if ( dst == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "error copying object" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}

// ------------------
// Geom_ReorderPgnCmd
// ------------------

PyObject* Geom_ReorderPgnCmd(PyObject* self, PyObject* args)
{
  char *srcName;
  char *dstName;
  cvRepositoryData *src;
  cvPolyData *dst;
  RepositoryDataT type;
  int start;

  if (!PyArg_ParseTuple(args,"sis", &srcName,&start,&dstName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import two chars and one int srcName, start,dstName");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }
  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  dst = sys_geom_ReorderPolygon( (cvPolyData*)src, start );
  if ( dst == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "error reordering object");
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}


// ---------------------------
// Geom_SplinePtsToPathPlanCmd
// ---------------------------

PyObject* Geom_SplinePtsToPathPlanCmd(PyObject* self, PyObject* args)
{
  char *srcName;
  int numOutputPts;
  char *filename = NULL;
  int flag;
  cvRepositoryData *src;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"sii|s", &srcName,&numOutputPts,&flag,&filename))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one char, two ints and one optional char, srcName, numOutputPts, flags, fileName");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  // if no filename is specified, pass the interp to the spline code to create
  // a tcl string containing the path.  If a filename is specified, return string
  // will be left blank.
  int result;
  char *output;
  if (filename == NULL) {
    result = pysys_geom_splinePtsToPathPlan( ((cvPolyData*)src)->GetVtkPolyData(),numOutputPts,
                                  filename, flag, &output);
  } else {
    result = pysys_geom_splinePtsToPathPlan( ((cvPolyData*)src)->GetVtkPolyData(),numOutputPts,
                                  filename, flag, NULL);
  }

  if (result == SV_OK) {
    if (filename !=NULL)
    	return SV_PYTHON_OK;
    else
	return Py_BuildValue("s",output);
  } else {
    PyErr_SetString(PyRunTimeErr, "Error getting splinePtsToPathPlan");
    
  }

}


// ------------------------
// Geom_IntegrateSurfaceCmd
// ------------------------

PyObject* Geom_IntegrateSurfaceCmd(PyObject* self, PyObject* args)
{
  char *objName;
  PyObject* nrmList;
  cvRepositoryData *obj;
  RepositoryDataT type;
  double nrm[3];
  int tensorType;

  if (!PyArg_ParseTuple(args,"sOi", &objName,&nrmList,&tensorType))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one char, one list, ont int, objName, nrmList,tensorType");
    
  }


    // Convert given coordinate to double's:
    if ( PyList_Size(nrmList) != 3 ) {
      PyErr_SetString(PyRunTimeErr,  "list must have three elements");
      
    }
      
    for (int i=0; i<3;i++)
	nrm[i] = PyFloat_AsDouble(PyList_GetItem(nrmList,i));
    if(PyErr_Occurred()!=NULL)
    {
      PyErr_SetString(PyRunTimeErr, "list elements must all be double's");
      
    }


  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  double q = 0.0;
  if ( sys_geom_IntegrateSurface((cvPolyData*)obj, tensorType, nrm, &q) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error calculating surface integral");
    
  }

  return Py_BuildValue("d",q);
}


// -------------------------
// Geom_IntegrateSurface2Cmd
// -------------------------

PyObject* Geom_IntegrateSurface2Cmd(PyObject* self, PyObject* args)
{
  char *objName;
  cvRepositoryData *obj;
  RepositoryDataT type;
  int tensorType;

  if (!PyArg_ParseTuple(args,"si", &objName,&tensorType))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one char, ont int, objName,tensorType");
    
  }



  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  double q = 0.0;
  double area = 0.0;
  if ( sys_geom_IntegrateSurface2((cvPolyData*)obj, tensorType, &q, &area) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error calculating surface integral");
    
  }


  return Py_BuildValue("dd",q,area);
}


// -----------------------
// Geom_IntegrateEnergyCmd
// -----------------------

PyObject* Geom_IntegrateEnergyCmd(PyObject* self, PyObject* args)
{
  char *objName;
  PyObject* nrmList;
  cvRepositoryData *obj;
  RepositoryDataT type;
  double nrm[3];
  double rho = 0.0;

  if (!PyArg_ParseTuple(args,"sOd", &objName,&nrmList,&rho))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one char, one list, one double, objName, nrmList,rho");
    
  }


    // Convert given coordinate to double's:

    if ( PyList_Size(nrmList) != 3 ) {
      PyErr_SetString(PyRunTimeErr,  "list must have three elements");
      
    }
    for(int i = 0; i<3; i++)
    {
      nrm[i] = PyFloat_AsDouble(PyList_GetItem(nrmList,i));
    }
    if (PyErr_Occurred()!=NULL)
    {      
      PyErr_SetString(PyRunTimeErr, "list elements must all be double's");
      
    }


  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  double energy = 0.0;
  if ( sys_geom_IntegrateEnergy((cvPolyData*)obj, rho, nrm, &energy) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error calculating surface integral");
    
  }

  return Py_BuildValue("d",energy);
}


// --------------------
// Geom_FindDistanceCmd
// --------------------

PyObject* Geom_FindDistanceCmd(PyObject* self, PyObject* args)
{
  char *objName;
  cvRepositoryData *obj;
  RepositoryDataT type;
  PyObject* ptList;
  double pt[3];
  int npt;
  double distance;

  if (!PyArg_ParseTuple(args,"sO", &objName,&ptList))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one char, one list, ont int, objName, ptList");
    
  }


  // Parse coordinate list:
  npt = PyList_Size(ptList);
  if ( npt != 3 ) {
    PyErr_SetString(PyRunTimeErr, "only valid for 3d objects and queries");
    
  }
  for(int i=0; i<3; i++)
    pt[i] = PyFloat_AsDouble(PyList_GetItem(ptList,i));
  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  distance = ((cvPolyData*)obj)->FindDistance( pt[0], pt[1], pt[2] );

  return Py_BuildValue("d",distance);
}


// -------------------------
// Geom_InterpolateScalarCmd
// -------------------------

PyObject* Geom_InterpolateScalarCmd(PyObject* self, PyObject* args)
{
  char *objName;
  cvRepositoryData *obj;
  RepositoryDataT type;
  PyObject* ptList;
  double pt[3];
  int npt;

  if (!PyArg_ParseTuple(args,"sO", &objName,&ptList))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one char, one list, ont int, objName, ptList");
    
  }


  // Parse coordinate list:
  npt = PyList_Size(ptList);

  if ( npt != 3 ) {
    PyErr_SetString(PyRunTimeErr, "only valid for 3d objects and queries");
    
  }

  for(int i = 0; i<3; i++)
    pt[i] = PyFloat_AsDouble(PyList_GetItem(ptList,i));

  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  double scalar = 0.0;
  if ( sys_geom_InterpolateScalar((cvPolyData*)obj, pt, &scalar) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error interpolating scalar");
    
  }

  return Py_BuildValue("d",scalar);
}


// -------------------------
// Geom_InterpolateVectorCmd
// -------------------------

PyObject* Geom_InterpolateVectorCmd(PyObject* self, PyObject* args)
{
  char *objName;
  cvRepositoryData *obj;
  RepositoryDataT type;
  PyObject* ptList;
  double pt[3];
  int npt;

  if (!PyArg_ParseTuple(args,"sO", &objName,&ptList))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one char, one list, ont int, objName, ptList");
    
  }


  // Parse coordinate list:
  npt = PyList_Size(ptList); 

  if ( npt != 3 ) {
    PyErr_SetString(PyRunTimeErr, "only valid for 3d objects and queries");
    
  }
  for (int i=0; i<3; i++)
  {
    pt[i]=PyFloat_AsDouble(PyList_GetItem(ptList,i)); 
  } 
  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  double vect[3];
  vect[0] = 0.0;
  vect[1] = 0.0;
  vect[2] = 0.0;
  if ( sys_geom_InterpolateVector((cvPolyData*)obj, pt, vect) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error interpolating vector");
    
  }

  PyObject* pList = PyList_New(3);
  for (int i = 0; i<3; i++)
    PyList_SetItem(pList,i,PyFloat_FromDouble(vect[i]));
  return pList;
}



// -------------------------
// Geom_IntersectWithLineCmd
// -------------------------

PyObject* Geom_IntersectWithLineCmd(PyObject* self, PyObject* args)
{
  char *objName;
  cvRepositoryData *obj;
  RepositoryDataT type;
  PyObject *p0List,*p1List;
  double p0[3],p1[3];
  int npt0,npt1;

  if (!PyArg_ParseTuple(args,"sOO", &objName,&p0List,&p1List))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one char, two lists, ont int, objName, p0List,p1List");
    
  }


  // Parse coordinate list:
  npt0 = PyList_Size(p0List);
  npt1 = PyList_Size(p1List);

  if ( npt0 != 3 || npt1 != 3 ) {
    PyErr_SetString(PyRunTimeErr, "only valid for 3d objects and queries");
    
  }
  for (int i=0; i<3; i++)
  {
    p0[i] = PyFloat_AsDouble(PyList_GetItem(p0List,i));
    p1[i] = PyFloat_AsDouble(PyList_GetItem(p1List,i));
  }

  // Do work of command:

  // Retrieve object:
  obj = gRepository->GetObject( objName );
  if ( obj == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object" );
    
  }

  type = obj->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  double intersect[3];
  if ( sys_geom_IntersectWithLine((cvPolyData*)obj, p0, p1, intersect) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error intersecting vtkPolyData with line");
    
  }

  return Py_BuildValue("ddd",intersect[0], intersect[1], intersect[2]);
}


// --------------------
// Geom_AddPointDataCmd
// --------------------

PyObject* Geom_AddPointDataCmd(PyObject* self, PyObject* args)
{
  char *srcNameA;
  char *srcNameB;
  int scflag = FALSE;
  int vflag = FALSE;
  char *dstName;

  cvRepositoryData *srcA = NULL;
  cvRepositoryData *srcB = NULL;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"sssii", &srcNameA,&srcNameB,&dstName,&scflag,&vflag))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import three chars,two ints\
srcNameA, srcNameB,dstName,scflag,vflag");
    
  }


  // Do work of command:

  // Retrieve source object:
  srcA = gRepository->GetObject( srcNameA );
  if ( srcA == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }

  // Retrieve source object:
  srcB = gRepository->GetObject( srcNameB );
  if ( srcB == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }


  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  type = srcA->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  type = srcB->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  sys_geom_math_scalar sc = SYS_GEOM_NO_SCALAR;
  sys_geom_math_vector v = SYS_GEOM_NO_VECTOR;
  if (scflag) {
      sc = SYS_GEOM_ADD_SCALAR;
  }
  if (vflag) {
      v = SYS_GEOM_ADD_VECTOR;
  }

  if ( sys_geom_mathPointData( (cvPolyData*)srcA, (cvPolyData*)srcB, sc, v, (cvPolyData**)(&dst) ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "point data math error" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}


// -------------------------
// Geom_SubtractPointDataCmd
// -------------------------

PyObject* Geom_SubtractPointDataCmd(PyObject* self, PyObject* args)
{
  char *srcNameA;
  char *srcNameB;
  int scflag = FALSE;
  int vflag = FALSE;
  char *dstName;

  cvRepositoryData *srcA = NULL;
  cvRepositoryData *srcB = NULL;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"sssii", &srcNameA,&srcNameB,&dstName,&scflag,&vflag))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import three chars,two ints\
srcNameA, srcNameB,dstName,scflag,vflag");
    
  }


  // Do work of command:

  // Retrieve source object:
  srcA = gRepository->GetObject( srcNameA );
  if ( srcA == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }

  // Retrieve source object:
  srcB = gRepository->GetObject( srcNameB );
  if ( srcB == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }


  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  type = srcA->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  type = srcB->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  sys_geom_math_scalar sc = SYS_GEOM_NO_SCALAR;
  sys_geom_math_vector v = SYS_GEOM_NO_VECTOR;
  if (scflag) {
      sc = SYS_GEOM_SUBTRACT_SCALAR;
  }
  if (vflag) {
      v = SYS_GEOM_SUBTRACT_VECTOR;
  }

  if ( sys_geom_mathPointData( (cvPolyData*)srcA, (cvPolyData*)srcB, sc, v, (cvPolyData**)(&dst) ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "point data math error" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}


// -------------------------
// Geom_MultiplyPointDataCmd
// -------------------------

PyObject* Geom_MultiplyPointDataCmd(PyObject* self, PyObject* args)
{
  char *srcNameA;
  char *srcNameB;
  int scflag = FALSE;
  int vflag = FALSE;
  char *dstName;

  cvRepositoryData *srcA = NULL;
  cvRepositoryData *srcB = NULL;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"sssii", &srcNameA,&srcNameB,&dstName,&scflag,&vflag))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import three chars,two ints\
srcNameA, srcNameB,dstName,scflag,vflag");
    
  }


  // Do work of command:

  // Retrieve source object:
  srcA = gRepository->GetObject( srcNameA );
  if ( srcA == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }

  // Retrieve source object:
  srcB = gRepository->GetObject( srcNameB );
  if ( srcB == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }


  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  type = srcA->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  type = srcB->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  sys_geom_math_scalar sc = SYS_GEOM_NO_SCALAR;
  sys_geom_math_vector v = SYS_GEOM_NO_VECTOR;
  if (scflag) {
      sc = SYS_GEOM_MULTIPLY_SCALAR;
  }
  if (vflag) {
      v = SYS_GEOM_MULTIPLY_VECTOR;
  }

  if ( sys_geom_mathPointData( (cvPolyData*)srcA, (cvPolyData*)srcB, sc, v, (cvPolyData**)(&dst) ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "point data math error" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }


  return Py_BuildValue("s",dst->GetName());
}


// -----------------------
// Geom_DividePointDataCmd
// -----------------------

PyObject* Geom_DividePointDataCmd(PyObject* self, PyObject* args)
{
  char *srcNameA;
  char *srcNameB;
  int scflag = FALSE;
  int vflag = FALSE;
  char *dstName;

  cvRepositoryData *srcA = NULL;
  cvRepositoryData *srcB = NULL;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"sssii", &srcNameA,&srcNameB,&dstName,&scflag,&vflag))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import three chars,two ints\
srcNameA, srcNameB,dstName,scflag,vflag");
    
  }


  // Do work of command:

  // Retrieve source object:
  srcA = gRepository->GetObject( srcNameA );
  if ( srcA == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }

  // Retrieve source object:
  srcB = gRepository->GetObject( srcNameB );
  if ( srcB == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }


  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  type = srcA->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  type = srcB->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  sys_geom_math_scalar sc = SYS_GEOM_NO_SCALAR;
  sys_geom_math_vector v = SYS_GEOM_NO_VECTOR;
  if (scflag) {
      sc = SYS_GEOM_DIVIDE_SCALAR;
  }
  if (vflag) {
      v = SYS_GEOM_DIVIDE_VECTOR;
  }

  if ( sys_geom_mathPointData( (cvPolyData*)srcA, (cvPolyData*)srcB, sc, v, (cvPolyData**)(&dst) ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "point data math error" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}


// ---------------
// Geom_ProjectCmd
// ---------------

PyObject* Geom_ProjectCmd(PyObject* self, PyObject* args)
{
  char *srcNameA;
  char *srcNameB;
  int scflag = FALSE;
  int vflag = FALSE;
  char *dstName;

  cvRepositoryData *srcA = NULL;
  cvRepositoryData *srcB = NULL;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"sssii", &srcNameA,&srcNameB,&dstName,&scflag,&vflag))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import three chars,two ints\
srcNameA, srcNameB,dstName,scflag,vflag");
    
  }


  // Do work of command:

  // Retrieve source object:
  srcA = gRepository->GetObject( srcNameA );
  if ( srcA == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }

  // Retrieve source object:
  srcB = gRepository->GetObject( srcNameB );
  if ( srcB == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }


  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  type = srcA->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  type = srcB->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  sys_geom_math_scalar sc = SYS_GEOM_NO_SCALAR;
  sys_geom_math_vector v = SYS_GEOM_NO_VECTOR;
  if (scflag) {
      sc = SYS_GEOM_ADD_SCALAR;
  }
  if (vflag) {
      v = SYS_GEOM_ADD_VECTOR;
  }

  if ( sys_geom_Project( (cvPolyData*)srcA, (cvPolyData*)srcB, sc, v, (cvPolyData**)(&dst) ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error projecting polydata point data" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}


// ---------------------------
// Geom_IntegrateScalarSurfCmd
// ---------------------------

PyObject* Geom_IntegrateScalarSurfCmd(PyObject* self, PyObject* args)
{
  char *srcName;
  cvRepositoryData *src;
  RepositoryDataT type;
  double flux;

  if (!PyArg_ParseTuple(args,"s", &srcName))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one char,srcName");
    
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  if ( sys_geom_IntegrateScalarSurf( (cvPolyData*)src, &flux ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "surface area computation error" );
    
  }

  return Py_BuildValue("d",flux);
}


// -----------------------------
// Geom_IntegrateScalarThreshCmd
// -----------------------------

PyObject* Geom_IntegrateScalarThreshCmd(PyObject* self, PyObject* args)
{
  char *srcName;
  cvRepositoryData *src;
  RepositoryDataT type;
  double wssthresh;
  double flux;
  double area;

  if (!PyArg_ParseTuple(args,"sd", &srcName,&wssthresh))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import one char and one double\
,srcName,wssthresh");
    
  }


  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  if ( sys_geom_IntegrateScalarThresh( (cvPolyData*)src, wssthresh, &flux, &area) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "surface area computation error" );
    
  }

  return Py_BuildValue("dd",flux, area);
}


// -------------------------
// Geom_ReplacePointDataCmd
// ------------------------

PyObject* Geom_ReplacePointDataCmd(PyObject* self, PyObject* args)
{
  char *srcNameA;
  char *srcNameB;
  int scflag = FALSE;
  int vflag = FALSE;
  char *dstName;

  cvRepositoryData *srcA = NULL;
  cvRepositoryData *srcB = NULL;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args,"sssii", &srcNameA,&srcNameB,&dstName,&scflag,&vflag))
  {
    PyErr_SetString(PyRunTimeErr, "Could not import three chars,two ints\
srcNameA, srcNameB,dstName,scflag,vflag");
    
  }


  // Do work of command:

  // Retrieve source object:
  srcA = gRepository->GetObject( srcNameA );
  if ( srcA == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }

  // Retrieve source object:
  srcB = gRepository->GetObject( srcNameB );
  if ( srcB == NULL ) {
    PyErr_SetString(PyRunTimeErr,  "couldn't find object");
    
  }


  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  type = srcA->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  type = srcB->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr,  "object not of type cvPolyData" );
    
  }

  sys_geom_math_scalar sc = SYS_GEOM_NO_SCALAR;
  sys_geom_math_vector v = SYS_GEOM_NO_VECTOR;
  if (scflag) {
      sc = SYS_GEOM_ADD_SCALAR;
  }
  if (vflag) {
      v = SYS_GEOM_ADD_VECTOR;
  }

  if ( sys_geom_ReplacePointData( (cvPolyData*)srcA, (cvPolyData*)srcB, sc, v, (cvPolyData**)(&dst) ) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "error replacing point data" );
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr,  "error registering obj in repository");
    delete dst;
    
  }

  return Py_BuildValue("s",dst->GetName());
}
