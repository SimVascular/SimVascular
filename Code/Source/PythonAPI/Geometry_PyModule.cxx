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

// The functions defined here implement the SV Python API 'geometry' module.
//
// There are no classes defined for this module. All module methods take
// vtkPolyData Python objects as arguments.
//
// A Python exception sv.geometry.GeometryError is defined for this module.
// The exception can be used in a Python 'try' statement with an 'except' clause
// like this
//
//    except sv.geometry.GeometryError:
//
#include "SimVascular.h"
#include "SimVascular_python.h"

#include <stdio.h>
#include <string.h>

#include "sv_Repository.h"
#include "sv_RepositoryData.h"
#include "sv_PolyData.h"
#include "Geometry_PyModule.h"
#include "sv_sys_geom.h"
#include "sv_SolidModel.h"
#include "Modeling_PyModule.h"
#include "sv_integrate_surface.h"
#include "sv_arg.h"
#include "sv_misc_utils.h"
#include "sv_vtk_utils.h"
#include "vtkSmartPointer.h"
#include "PyUtils.h"
#include "sv2_globals.h"
#include "vtkPythonUtil.h"
#include "sv4gui_ModelUtils.h"

// Needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Exception type used by PyErr_SetString() to set the for the error indicator.
static PyObject * PyRunTimeErr;

// Include the definition for the geometry options classes.
#include "GeometryLoftOptions_PyClass.cxx"
#include "GeometryLoftNurbsOptions_PyClass.cxx"
#include "GeometryBlendOptions_PyClass.cxx"

//////////////////////////////////////////////////////
//        U t i l i t y     F u n c t i o n s       //
//////////////////////////////////////////////////////

//----------------
// GetVtkPolyData
//----------------
// Get the vtkPolyData object from the Python vtkPolyData object.
//
static vtkPolyData *
GetVtkPolyData(PyUtilApiFunction& api, PyObject* obj)
{
  vtkPolyData* polydata = nullptr;

 if (!PyVTKObject_Check(obj)) {
      api.error("The polydata argument is not a vtkPolyData object.");
  }

  polydata = (vtkPolyData*)vtkPythonUtil::GetPointerFromObject(obj, "vtkPolyData");
  if (polydata == nullptr) {
      api.error("The polydata argument is not a vtkPolyData object.");
  }
  return polydata;
}

//--------------------
// GetGeometryObjects
//--------------------
// Create a list of cvPolyData objects from a list of
// vtkPolyData objects.
//
// [TODO:DaveP] It would be good to use shared_ptr here
// to manage deleting cvPolyData objects but the sys_geom
// functions don't use them.
//
static std::vector<cvPolyData*>
GetGeometryObjects(PyUtilApiFunction& api, PyObject* objList)
{
  std::vector<cvPolyData*> cvPolyDataList;
  auto numObjs = PyList_Size(objList);

  if (numObjs == 0) {
      api.error("The polydata list argument is empty.");
      return cvPolyDataList;
  }

  for (int i = 0; i < numObjs; i++ ) {
      auto obj = PyList_GetItem(objList, i);
      auto polydata = GetVtkPolyData(api, obj);
      if (polydata == nullptr) {
          api.error("Index " + std::to_string(i) + " of the polydata list argument is not a vtkPolyData object.");
          for (auto cvPolydata : cvPolyDataList) {
              delete cvPolydata;
          }
          cvPolyDataList.clear();
          return cvPolyDataList;
      }
      auto cvPolydata = new cvPolyData(polydata);
      cvPolyDataList.push_back(cvPolydata);
  }

  return cvPolyDataList;
}

//////////////////////////////////////////////////////
//          M o d u l e  F u n c t i o n s          //
//////////////////////////////////////////////////////
//
// Python 'geometry' module methods.

//--------------------
// Geom_align_profile
//--------------------
//
PyDoc_STRVAR(Geom_align_profile_doc,
  "align_profile(reference, align, use_distance=True) \n\
   \n\
   Align a profile represented as a closed curve with a given reference    \n\
   profile also represented as a closed curve.                             \n\
   \n\
   The profile is aligned by reordering its points.                        \n\
   \n\
   Args: \n\
     reference (vtkPolyData): The profile to align to.                     \n\
     align (vtkPolyData): The profile to align to the reference.           \n\
     use_distance (Optional[bool]): If True then align profile initial     \n\
       points using the minium distance between points. If False then align\n\
       profile initial points using the angle beteenn points.              \n\
   \n\
   Returns (vtkPolyData): The aligned profile.                             \n\
");

static PyObject *
Geom_align_profile(PyObject* self, PyObject* args, PyObject* kwargs)
{
  //std::cout << "========== Geom_align_profile ==========" << std::endl;
  auto api = PyUtilApiFunction("OO|O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"reference", "align", "use_distance", NULL};
  PyObject* refObj;
  PyObject* alignObj;
  PyObject* distArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &refObj, &alignObj, &PyBool_Type, &distArg)) {
      return api.argsError();
  }
  auto use_distance = PyObject_IsTrue(distArg);

  // Get the vtkPolyData objects from the Python objects.
  //
  auto refPolydata = GetVtkPolyData(api, refObj);
  if (refPolydata== nullptr) {
      return nullptr;
  }
  cvPolyData refCvPolyData(refPolydata);

  auto alignPolydata = GetVtkPolyData(api, alignObj);
  if (alignPolydata== nullptr) {
      return nullptr;
  }
  cvPolyData alignCvPolyData(alignPolydata);

  // Align the profiles.
  //
  cvPolyData *result;
  if (use_distance) {
    result = sys_geom_AlignByDist(&refCvPolyData, &alignCvPolyData);
  } else {
    result = sys_geom_Align(&refCvPolyData, &alignCvPolyData);
  }

  if (result == nullptr) {
      api.error("The aligning profile operation failed.");
      return nullptr;
  }

  return vtkPythonUtil::GetObjectFromPointer(result->GetVtkPolyData());
}

//--------------------
// Geom_average_point
//--------------------
//
PyDoc_STRVAR(Geom_average_point_doc,
  "average_point(polydata) \n\
   \n\
   Calculate the average point for the points of a VTK PolyData object.    \n\
   \n\
   Args: \n\
     polydata (vtkPolyData): The vtkPolyData object to compute the average \n\
       point for.                                                          \n\
   \n\
   Returns list([float, float, float]): The average point. \n\
");

static PyObject *
Geom_average_point(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("O", PyRunTimeErr, __func__);
  PyObject* pdObj;

  if (!PyArg_ParseTuple(args, api.format, &pdObj)) {
      return api.argsError();
  }

  // Get the vtkPolyData object from the Python object.
  auto polydata = GetVtkPolyData(api, pdObj);
  if (polydata == nullptr) {
      return nullptr;
  }

  double pt[3];
  cvPolyData cvPolydata(polydata);
  if (sys_geom_AvgPt(&cvPolydata, pt) != SV_OK) {
      api.error("Error calculating the average point for the polydata.");
      return nullptr;
  }

  return Py_BuildValue("[d,d,d]", pt[0], pt[1], pt[2]);
}

//---------------------
// Geom_classify_point
//---------------------
//
PyDoc_STRVAR(Geom_point_inside_doc,
  "point_inside(polydata, point) \n\
   \n\
   Determine if a 3D point is inside or outside of a solid.                \n\
   \n\
   Args: \n\
     polydata (vtkPolyData): The vtkPolyData object representing a solid   \n\
        as a closed surface \n\
     point ([float, float, float]): The 3D point to classify.              \n\
   \n\
   Returns True if the point is inside the solid, False if it is outside.  \n\
");

static PyObject *
Geom_point_inside(PyObject* self, PyObject* args, PyObject* kwargs)
{
  //std::cout << "========== Geom_point_inside ==========" << std::endl;
  auto api = PyUtilApiFunction("OO!", PyRunTimeErr, __func__);
  static char *keywords[] = {"polydata", "point", NULL};
  PyObject* pdObj;
  PyObject* pointArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &pdObj, &PyList_Type, &pointArg)) {
      return api.argsError();
  }

  // Get the vtkPolyData object from the Python object.
  auto polydata = GetVtkPolyData(api, pdObj);
  if (polydata == nullptr) {
      return nullptr;
  }

  // Get the point data.
  double point[3];
  std::string emsg;
  if (!PyUtilGetPointData<double>(pointArg, emsg, point)) {
      api.error("The point argument " + emsg);
      return nullptr;
  }

  // Result is -1 if the point is outside, 1 if it is inside.
  cvPolyData cvPolydata(polydata);
  int result;
  if (sys_geom_Classify(&cvPolydata, point, &result) != SV_OK) {
      api.error("Error classifying a point for the geometry.");
      return nullptr;
  }

  return Py_BuildValue("N", PyBool_FromLong(result+1));
}

//-------------------------------
// Geom_interpolate_closed_curve
//-------------------------------
//
PyDoc_STRVAR(Geom_interpolate_closed_curve_doc,
  "interpolate_closed_curve(polydata, number_of_points) \n\
   \n\
   Generate a list of 3D points linearly interpolated between the points   \n\
   of a closed 3D curve.                                                   \n\
   \n\
   Args: \n\
     polydata (vtkPolyData): The vtkPolyData object representing a closed  \n\
        curve.                                                             \n\
     number_of_points (int): The number of points to generate.             \n\
   \n\
   Returns list([float, float, float]): The list of interpolated points.   \n\
");

static PyObject *
Geom_interpolate_closed_curve(PyObject* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("Oi", PyRunTimeErr, __func__);
  static char *keywords[] = {"polydata", "number_of_points", NULL};
  PyObject* pdObj;
  int numSamples;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &pdObj, &numSamples)) {
      return api.argsError();
  }

  if (numSamples < 3) {
      api.error("The number of samples argument is < 3.");
      return nullptr;
  }

  // Get the vtkPolyData object from the Python object.
  auto polydata = GetVtkPolyData(api, pdObj);
  if (polydata == nullptr) {
      return nullptr;
  }

  cvPolyData cvPolydata(polydata);
  auto result = sys_geom_sampleLoop(&cvPolydata, numSamples);

  if (result == NULL) {
      api.error("Error performing the sample loop operation.");
      return nullptr;
  }

  return vtkPythonUtil::GetObjectFromPointer(result->GetVtkPolyData());
}

//------------------
// Geom_local_blend
//------------------
//
PyDoc_STRVAR(Geom_local_blend_doc,
  "local_blend(surface, faces, options)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_local_blend(PyObject* self, PyObject* args, PyObject* kwargs)
{
  #ifdef dbg_Geom_local_blend
  std::cout << " " << std::endl;
  std::cout << "========== Geom_local_blend ==========" << std::endl;
  #endif
  auto api = PyUtilApiFunction("OO!O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"surface", "faces", "options", NULL};
  PyObject* surfaceArg;
  PyObject* facesArg;
  PyObject* blendOptsArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &surfaceArg, &PyList_Type, &facesArg,
        &PyBlendOptionsType, &blendOptsArg)) {
      return api.argsError();
  }

  // Get the vtkPolyData objectsfrom the Python object.
  //
  auto surfPolydata = GetVtkPolyData(api, surfaceArg);
  if (surfPolydata == nullptr) {
      return nullptr;
  }

  // Extract blend option values from the BlendOptions object.
  //
  int numBlendIters = BlendOptionsGetInt(blendOptsArg, BlendOptions::NUM_BLEND_ITERATIONS);
  int numSubblendIters = BlendOptionsGetInt(blendOptsArg, BlendOptions::NUM_SUBBLEND_ITERATIONS);
  int numSubdivisionIters = BlendOptionsGetInt(blendOptsArg, BlendOptions::NUM_SUBDIVISION_ITERATIONS);
  int numCgSmoothIters = BlendOptionsGetInt(blendOptsArg, BlendOptions::NUM_CGSMOOTH_ITERATIONS);
  int numLapSmoothIters = BlendOptionsGetInt(blendOptsArg, BlendOptions::NUM_LAPSMOOTH_ITERATIONS);
  // The API uses a percent, SV uses the value percent/100.
  double targetDecimation = BlendOptionsGetDouble(blendOptsArg, BlendOptions::TARGET_DECIMATION) / 100.0;

  // Check values.
  //
  if (numBlendIters < 1) {
      api.error("The 'num_blend_operations' parameter must be > 0.");
      return nullptr;
  }

  if (numSubblendIters < 1) {
      api.error("The 'num_subblend_operations' parameter must be > 0.");
      return nullptr;
  }

  if (numSubdivisionIters < 0) {
      api.error("The 'num_subdivision_operations' parameter must be >= 0.");
      return nullptr;
  }

  if (numCgSmoothIters < 0) {
      api.error("The 'num_cgsmooth_iterations' parameter must be >= 0.");
      return nullptr;
  }

  if (numLapSmoothIters < 1) {
      api.error("The 'num_cgsmooth_iterations' parameter must be > 0.");
      return nullptr;
  }

  if (targetDecimation < 0.0) {
      api.error("The 'target_decimation' parameter must be > 0.0.");
      return nullptr;
  }

  #ifdef dbg_Geom_local_blend
  std::cout << "[Geom_local_blend] numBlendIters: " << numBlendIters << std::endl;
  std::cout << "[Geom_local_blend] numSubblendIters: " << numSubblendIters << std::endl;
  std::cout << "[Geom_local_blend] numSubdivisionIters: " << numSubdivisionIters << std::endl;
  std::cout << "[Geom_local_blend] numCgSmoothIters: " << numCgSmoothIters << std::endl;
  std::cout << "[Geom_local_blend] numLapSmoothIters: " << numLapSmoothIters << std::endl;
  std::cout << "[Geom_local_blend] targetDecimation: " << targetDecimation << std::endl;
  #endif

  // Set svBlendParam parameters.
  sv4guiModelElement::svBlendParam params;
  params.numblenditers = numBlendIters;
  params.numsubblenditers = numSubblendIters;
  params.numsubdivisioniters = numSubdivisionIters;
  params.numcgsmoothiters = numCgSmoothIters;
  params.numlapsmoothiters = numLapSmoothIters;
  params.targetdecimation = targetDecimation;

  // Compute data needed for blending.
  //
  std::vector<sv4guiModelElement::svBlendParamRadius> blendRadii;
  vtkSmartPointer<vtkPolyData> lastsurfPolydata = surfPolydata;
  int numFaces = PyList_Size(facesArg);
  for (int i = 0; i < numFaces; i++) {
      int faceID1, faceID2;
      double radius;
      PyObject* radiusFace = PyList_GetItem(facesArg, i);
      if (!GetRadiusFaceValues(radiusFace, faceID1, faceID2, radius)) {
          std::string valErrorMsg;
          std::string itemStr;
          PyUtilGetPyErrorInfo(radiusFace, valErrorMsg, itemStr);
          std::string msg = itemStr + ": " + valErrorMsg;
          api.error(msg);
          return nullptr;
      }

      blendRadii.push_back(sv4guiModelElement::svBlendParamRadius(faceID1, faceID2, radius));

      lastsurfPolydata = sv4guiModelUtils::CreatePolyDataByBlend(lastsurfPolydata, faceID1, faceID2, radius, &params);

      if (lastsurfPolydata == nullptr) {
          api.error("Failed creating blend data.");
          return nullptr;
      }
  }

  return vtkPythonUtil::GetObjectFromPointer(lastsurfPolydata);
}

//-----------
// Geom_loft
//-----------
//
// [TODO:DaveP] We may need to add input curve resampling and alignment here
// in order to get lofting to work.
//
PyDoc_STRVAR(Geom_loft_doc,
  "loft(polydata_list, loft_options) \n\
   \n\
   Create a lofted surface from a list of polydata curves.                  \n\
   \n\
   The loft method fits a surface through two or more profile curves that   \n\
   define the surface shape. This is typically used to create a surface of  \n\
   a vessel from a group of contours segmenting the vessel's lumen.         \n\
   \n\
   The surface is created using splines interpolating profile points along  \n\
   the its length and linearly interpolation around its profile.            \n\
   \n\
   Args: \n\
     polydata_list (list[vtkPolyData]): The list of vtkPolyData objects     \n\
        representing the profile curves defining a surface.                 \n\
     loft_options (sv.geometry.LoftOptions): The LoftOptions object         \n\
        containing lofting parameter values.                                \n\
   \n\
   Returns (vtkPolyData): The vtkPolyData object of the lofted surface.     \n\
");

static PyObject *
Geom_loft(PyObject* self, PyObject* args, PyObject* kwargs)
{
  #ifdef dbg_Geom_loft
  std::cout << " " << std::endl;
  std::cout << "========== Geom_loft ==========" << std::endl;
  #endif

  auto api = PyUtilApiFunction("O!O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"polydata_list", "loft_options", NULL};
  PyObject* objListArg;
  PyObject* loftOptsArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyList_Type, &objListArg, &PyLoftOptionsType,
        &loftOptsArg)) {
      return api.argsError();
  }

  // Check the list of polydata curve profiles.
  auto cvPolyDataList = GetGeometryObjects(api, objListArg);
  if (cvPolyDataList.size() == 0) {
      return nullptr;
  }

  if (cvPolyDataList.size() < 2) {
      api.error("At least two profiles are needed for lofing.");
      return nullptr;
  }

  // Check that the profiles have the same number of points.
  int numProfilePoints = 0;
  for (auto const& profile : cvPolyDataList) {
      int numPoints = profile->GetVtkPolyData()->GetNumberOfPoints();
      if (numProfilePoints == 0) {
          numProfilePoints = numPoints;
      } else if (numPoints != numProfilePoints) {
          api.error("Profiles do not have the same number of points.");
          return nullptr;
      }
  }

  // Extract loft option values from the loftOptions object.
  //
  // Most of these are not used for now.
  //
  int numOutPtsInSegs = numProfilePoints;
  int numOutPtsAlongLength = LoftOptionsGetInt(loftOptsArg, LoftOptions::NUM_OUT_PTS_ALONG_LENGTH);
  int numLinearPtsAlongLength = LoftOptionsGetInt(loftOptsArg, LoftOptions::NUM_LINEAR_PTS_ALONG_LENGTH);
  int numModes = 0; /*LoftOptionsGetInt(loftOptsArg, LoftOptions::NUM_MODES); */
  int splineType = 0; /*LoftOptionsGetInt(loftOptsArg, LoftOptions::SPLINE_TYPE); */
  int useFFT = 0; /*LoftOptionsGetInt(loftOptsArg, LoftOptions::USE_FFT); */
  int useLinearSampleAlongLength = LoftOptionsGetInt(loftOptsArg, LoftOptions::USE_LINEAR_SAMPLE_ALONG_LENGTH);
  double bias = 0.0; /*LoftOptionsGetDouble(loftOptsArg, LoftOptions::BIAS); */
  double tension = 0.0; /*LoftOptionsGetDouble(loftOptsArg, LoftOptions::TENSION); */
  double continuity = 0.0; /*LoftOptionsGetDouble(loftOptsArg, LoftOptions::CONTINUITY); */

  #ifdef dbg_Geom_loft
  std::cout << "[Geom_loft] numProfilePoints:" << numProfilePoints << std::endl;
  std::cout << "[Geom_loft] loftOptsArg: " << loftOptsArg << std::endl;
  std::cout << "[Geom_loft] numOutPtsInSegs: " << numOutPtsInSegs << std::endl;
  std::cout << "[Geom_loft] numOutPtsAlongLength : " << numOutPtsAlongLength << std::endl;
  std::cout << "[Geom_loft] numModes: " << numModes << std::endl;
  std::cout << "[Geom_loft] splineType: " << splineType << std::endl;

  std::cout << "[Geom_loft] useLinearSampleAlongLength: " << useLinearSampleAlongLength << std::endl;
  std::cout << "[Geom_loft] useFFT: " << useFFT << std::endl;
  std::cout << "[Geom_loft] bias: " << bias << std::endl;
  std::cout << "[Geom_loft] tension: " << tension << std::endl;
  std::cout << "[Geom_loft] continuity: " << continuity << std::endl;
  #endif

  // Create the lofted surface.
  //
  auto numSrcs = cvPolyDataList.size();
  cvPolyData *result;

  auto status = sys_geom_loft_solid(cvPolyDataList.data(), numSrcs, useLinearSampleAlongLength, useFFT, numOutPtsAlongLength,
                                    numOutPtsInSegs, numLinearPtsAlongLength, numModes, splineType, bias, tension, continuity,
                                    &result);

  if (status != SV_OK) {
      delete result;
      api.error("Error performing the loft operation.");
      return nullptr;
  }

  return vtkPythonUtil::GetObjectFromPointer(result->GetVtkPolyData());
}

//-----------------
// Geom_loft_nurbs
//-----------------
//
PyDoc_STRVAR(Geom_loft_nurbs_doc,
  "loft_nurbs(polydata_list, loft_options) \n\
   \n\
   Create a lofted NURBS surface from a list of polydata curves.            \n\
   \n\
   The loft method fits a surface through two or more profile curves that   \n\
   define the surface shape. This is typically used to create a surface of  \n\
   a vessel from a group of contours segmenting the vessel's lumen.         \n\
   \n\
   Args: \n\
     polydata_list (list[vtkPolyData]): The list of vtkPolyData objects     \n\
        representing the profile curves defining a surface.                 \n\
     loft_options (sv.geometry.LoftNurbsOptions): The LoftNurbsOptions      \n\
        object containing lofting parameter values.                         \n\
     num_sections (Optional[int]): The number of sections created between  \n\
        profile curves.                                                     \n\
   \n\
   Returns (vtkPolyData): The vtkPolyData object of the lofted surface.     \n\
");

static PyObject *
Geom_loft_nurbs(PyObject* self, PyObject* args, PyObject* kwargs)
{
  #define ndbg_Geom_loft_nurbs
  #ifdef dbg_Geom_loft_nurbs
  std::cout << " " << std::endl;
  std::cout << "========== Geom_loft_nurbs ==========" << std::endl;
  #endif
  auto api = PyUtilApiFunction("O!O!|i", PyRunTimeErr, __func__);
  static char *keywords[] = {"polydata_list", "loft_options", "num_sections", NULL};
  PyObject* objListArg;
  PyObject* loftOptsArg;
  int num_sections = 12;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyList_Type, &objListArg,
        &PyLoftNurbsOptionsType, &loftOptsArg, &num_sections)) {
      return api.argsError();
  }

  // Check the list of polydata curve profiles.
  auto cvPolyDataList = GetGeometryObjects(api, objListArg);
  int numProfiles = cvPolyDataList.size();
  if (numProfiles == 0) {
      return nullptr;
  }

  if (numProfiles < 2) {
      api.error("At least two profiles are needed for lofing.");
      return nullptr;
  }

  // Check that the profiles have the same number of points.
  int numProfilePoints = 0;
  for (auto const& profile : cvPolyDataList) {
      int numPoints = profile->GetVtkPolyData()->GetNumberOfPoints();
      if (numProfilePoints == 0) {
          numProfilePoints = numPoints;
      } else if (numPoints != numProfilePoints) {
          api.error("Profiles do not have the same number of points.");
          return nullptr;
      }
  }

  // Extract loft option values from the loftOptions object.
  //
  int uDegree = LoftNurbsOptionsGetInt(loftOptsArg, LoftNurbsOptions::U_DEGREE);
  int vDegree = LoftNurbsOptionsGetInt(loftOptsArg, LoftNurbsOptions::V_DEGREE);
  double uSpacing = 1.0 / (num_sections * numProfiles);
  double vSpacing = 1.0 / numProfilePoints;
  char* uKnotSpanType = LoftNurbsOptionsGetString(loftOptsArg, LoftNurbsOptions::U_KNOT_SPAN_TYPE);
  char* vKnotSpanType = LoftNurbsOptionsGetString(loftOptsArg, LoftNurbsOptions::V_KNOT_SPAN_TYPE);
  char* uParametricSpanType = LoftNurbsOptionsGetString(loftOptsArg, LoftNurbsOptions::U_PARAMETRIC_SPAN_TYPE);
  char* vParametricSpanType = LoftNurbsOptionsGetString(loftOptsArg, LoftNurbsOptions::V_PARAMETRIC_SPAN_TYPE);

  #ifdef dbg_Geom_loft_nurbs
  std::cout << "[Geom_loft_nurbs] numProfilePoints: " << numProfilePoints << std::endl;
  std::cout << "[Geom_loft_nurbs] uDegree: " << uDegree << std::endl;
  std::cout << "[Geom_loft_nurbs] vDegree: " << vDegree << std::endl;
  std::cout << "[Geom_loft_nurbs] uSpacing: " << uSpacing << std::endl;
  std::cout << "[Geom_loft_nurbs] vSpacing: " << vSpacing << std::endl;
  std::cout << "[Geom_loft_nurbs] uKnotSpanType: " << uKnotSpanType << std::endl;
  std::cout << "[Geom_loft_nurbs] vKnotSpanType: " << vKnotSpanType << std::endl;
  std::cout << "[Geom_loft_nurbs] uParametricSpanType: " << uParametricSpanType << std::endl;
  std::cout << "[Geom_loft_nurbs] vParametricSpanType: " << vParametricSpanType << std::endl;
  #endif

  // Reset uDegree and vDegree if they are larger
  // than the number of profile curves.
  //
  if (uDegree >= numProfiles) {
      uDegree = numProfiles - 1;
  }

  if (vDegree >= numProfilePoints) {
      vDegree = numProfilePoints - 1;
  }

  // Create the lofted surface.
  //
  cvPolyData *result;
  vtkSmartPointer<vtkSVNURBSSurface> NURBSSurface = vtkSmartPointer<vtkSVNURBSSurface>::New();
  auto status = sys_geom_loft_solid_with_nurbs(cvPolyDataList.data(), numProfiles, uDegree, vDegree, uSpacing, vSpacing,
          uKnotSpanType, vKnotSpanType, uParametricSpanType, vParametricSpanType, NURBSSurface, &result);

  // [TODO:DaveP] Is this correct?
  /*
  for (auto cvPolyData : cvPolyDataList) {
      delete cvPolyData;
  }
  */

  if (status != SV_OK) {
      delete result;
      api.error("Error creating a lofted solid using nurbs.");
      return nullptr;
  }

  return vtkPythonUtil::GetObjectFromPointer(result->GetVtkPolyData());
}

//------------------------------------
// Geom_set_array_for_local_op_sphere
//------------------------------------
//
PyDoc_STRVAR(Geom_local_sphere_smooth_doc,
  "local_sphere_smooth(surface, radius, center, smoothing_parameters)  \n\
   \n\
   Smooth a surface locally in a region defined by a sphere.     \n\
   \n\
   Polygonal surface smoothing is used to smooth the bumps and ridges (e.g. vessel junctions) of a surface by \n\
   reducing the angles between adjacent polygons. Two smoothing methods are supported: Laplacian and constrained. \n\
   The Laplacian method sweeps over a region of the polygonal mesh several iterations, repeatedly moving each adjustable \n\
   vertex to the arithmetic average of the vertices adjacent to it. Laplacian smoothing is computationally inexpensive \n\
   but may a degrade the accuracy of the representation and possibly change its topology by removing connecting parts. \n\
   Laplacian smoothing is controlled by two parameters: a relaxation (weighting) factor and the number of iterations (sweeps) \n\
   used to apply the smoothing operation. The relaxation factor scales each adjacent vertex by the total area of the polygons \n\
   adjacent to it. The relaxation factor should be choosen between 0.01 and 0.05.                                             \n\
   \n\
   The constrained method method solves for a surface that minimizes the error between the original mesh and the Laplacian    \n\
   smoothed mesh. Constrained smoothing is controlled by three parameters: a constrain factor, the number of iterations (sweeps) \n\
   used to apply the smoothing operation, and the number of conjugate gradient (CG) iteratons used to solve the system of equations   \n\
   used to minimumize the error. The constrain factor determines how much the smoothed surface deviates from the original \n\
   surface. The constrain factor should be choosen between 0.0 (no deviation) and 1.0. \n\
   \n\
   Args: \n\
     surface (vtkPolyData): The surface to be smoothed.                     \n\
     radius (float): The radius of the sphere.                              \n\
     center ([x,y,z]): The list of three floats defining the center of the sphere. \n\
     smoothing_parameters (dict): The parameters used in the smoothing operation. \n\
       For 'laplacian' smoothing: { 'method':'laplacian', 'num_iterations':int, 'relaxation_factor':float }      \n\
           Defaults: 'laplacian' smoothing: { 'method':'laplacian', 'num_iterations':5, 'relaxation_factor':0.01}  \n\
       For 'constrained' smoothing: { 'method':'constrained', 'num_iterations':int, 'constrain_factor':float, 'num_cg_solves':int }  \n\
           Defaults: { 'method':'constrained', 'num_iterations':5, 'constrain_factor':0.2, 'num_cg_solves':30 }  \n\
   \n\
   Returns (vtkPolyData): The vtkPolyData object of the smoothed surface.     \n\
");

static PyObject *
Geom_local_sphere_smooth(PyObject* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("OdO!O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"surface", "radius", "center", "smoothing_parameters", NULL};
  PyObject* surfaceArg;
  double radius = 0.0;
  PyObject* centerArg;
  PyObject* smoothingParmaArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &surfaceArg, &radius, &PyList_Type, &centerArg, 
        &PyDict_Type, &smoothingParmaArg)) {
      return api.argsError();
  }

  // Check that a non-empty dict has been passed.
  int numItems = PyDict_Size(smoothingParmaArg);
  if (numItems == 0) { 
      api.error("The 'smooth_method' argument is empty.");
      return nullptr;
  }

  // Get smoothing method.
  auto methodItem = PyDict_GetItemString(smoothingParmaArg, "method");
  if (methodItem == nullptr) {
      api.error("The 'smooth_parameters' argument has no 'method' key.");
      return nullptr;
  }
  std::string smoothingMethod(PyString_AsString(methodItem));

  int numIters = 0;
  double constrainFactor = 0.0; 
  int numCGSolves = 0;
  double relaxFactor = 0.0;

  // Get number of iterations. 
  auto item = PyDict_GetItemString(smoothingParmaArg, "num_iterations");
  if (item != nullptr) { 
      numIters = PyLong_AsLong(item);
      if (PyErr_Occurred()) {
          PyErr_SetString(PyExc_ValueError, "The 'num_iterations' parameter is not an int.");
          return nullptr;
      }
  }

  // Get constrained smoothing parameters.
  //
  if (smoothingMethod == "constrained") { 
      if (numIters == 0) { 
          numIters = 5;
      }
      constrainFactor = 0.2; 
      numCGSolves = 30;

      PyObject *key, *value;
      Py_ssize_t pos = 0;

      while (PyDict_Next(smoothingParmaArg, &pos, &key, &value)) {
          auto keyName = std::string(PyString_AsString(key));
          if ((keyName == "method") || (keyName == "num_iterations")) {
              continue;
          }
          if (keyName == "num_cg_solves") {
              numCGSolves = PyLong_AsLong(value);
              if (PyErr_Occurred()) {
                  PyErr_SetString(PyExc_ValueError, "The 'num_cg_solves' parameter is not an int.");
                  return nullptr;
              }
          } else if (keyName == "constrain_factor") {
              constrainFactor = PyFloat_AsDouble(value);
              if (PyErr_Occurred()) {
                  PyErr_SetString(PyExc_ValueError, "The 'contrain_factor' parameter is not a float.");
                  return nullptr;
              }
          } else {
              api.error("The 'smoothing_parameter' key '" + keyName + "' is not valid for the 'constrained' method.");
              return nullptr;
          }
      }

  // Get Laplacian smoothing parameters.
  //
  } else if (smoothingMethod != "laplacian") {
      if (numIters == 0) { 
          numIters = 100;
      }
      relaxFactor = 0.01;

      PyObject *key, *value;
      Py_ssize_t pos = 0;

      while (PyDict_Next(smoothingParmaArg, &pos, &key, &value)) {
          auto keyName = std::string(PyString_AsString(key));
          if ((keyName == "method") || (keyName == "num_iterations")) {
              continue;
          }
          if (keyName == "relaxation_factor") {
              relaxFactor = PyFloat_AsDouble(value);
              if (PyErr_Occurred()) {
                  PyErr_SetString(PyExc_ValueError, "The 'relaxation_factor' parameter is not a float.");
                  return nullptr;
              }
          } else {
              api.error("The 'smoothing_parameter' key '" + keyName + "' is not valid for the 'laplacian' method.");
              return nullptr;
          }
      }

  } else {
      api.error("The 'smoothing_method' argument type value '" + smoothingMethod + "' is not valid. Valid types are 'constrained' and 'laplacian'.");
      return nullptr;
  }

  // Get the vtkPolyData object from the Python object.
  //
  auto surfPolydata = GetVtkPolyData(api, surfaceArg);
  if (surfPolydata == nullptr) {
      return nullptr;
  }

  cvPolyData surfCvPolyData(surfPolydata);

  // Get the sphere center.
  std::string emsg;
  std::array<double,3> center;
  if (!PyUtilGetPointData(centerArg, emsg, center.data())) {
      api.error("The 'center' argument " + emsg);
      return nullptr;
  }

  // Set the cell array used in smoothing.
  cvPolyData* arrayCvPolyData = nullptr;
  char* cellArrayName = "ActiveCells";
  int dataType = 1;

  if (sys_geom_set_array_for_local_op_sphere(&surfCvPolyData, &arrayCvPolyData, radius, center.data(), cellArrayName, dataType) != SV_OK) {
      api.error("Error setting local sphere operation array for input surface geometry.");
      return nullptr;
  }

  // Perform the smoothing operation.
  //
  cvPolyData* smoothedCvPolyData = nullptr;
  char* pointArrayName = nullptr;

  if (smoothingMethod == "constrained") { 
      if (sys_geom_local_constrain_smooth(arrayCvPolyData, &smoothedCvPolyData, numIters, constrainFactor, numCGSolves, 
          pointArrayName, cellArrayName) != SV_OK ) {
          api.error("The local sphere constrained smoothing operation has failed.");
          return nullptr;
      }

  } else if (smoothingMethod == "laplacian") { 
      if (sys_geom_local_laplacian_smooth(arrayCvPolyData, &smoothedCvPolyData, numIters, relaxFactor, pointArrayName, cellArrayName) != SV_OK ) {
          api.error("The local sphere laplacian smoothing operation has failed.");
          return nullptr;
      }
    }

  return vtkPythonUtil::GetObjectFromPointer(smoothedCvPolyData->GetVtkPolyData());
}

//===================================================================================
//                                  O l d    M e t h o d s
//===================================================================================
//
// [TODO:DaveP] There were a lot of methods originally defined for this module.
// However, it is not clear how useful these methods might be. It is also not
// clear what they even do.
//
#ifdef python_geom_module_use_old_methods

//-------------
// Geom_reduce
//-------------
//
PyDoc_STRVAR(Geom_reduce_doc,
  "reduce(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_reduce(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ssd", PyRunTimeErr, __func__);
  char *srcName;
  char *dstName;
  double tol;

  if (!PyArg_ParseTuple(args, api.format, &srcName, &dstName, &tol)) {
      return api.argsError();
  }

  // Retrieve source object.
  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  // Check that the repository dstName object does not already exist.
  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  cvPolyData *dst;
  if (sys_geom_Reduce(src, tol, &dst) != SV_OK) {
      api.error("Error merging points for geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//------------
// Geom_union
//------------
//
PyDoc_STRVAR(Geom_union_doc,
  "union(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_union(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sss|d", PyRunTimeErr, __func__);
  char *aName;
  char *bName;
  char *dstName;
  double tolerance = 1e-6;

  if (!PyArg_ParseTuple(args, api.format, &aName, &bName, &dstName, &tolerance)) {
      return api.argsError();
  }

  // Retrieve operands geometry.
  auto srcA = GetRepositoryGeometry(api, aName);
  if (srcA == nullptr ) {
      return nullptr;
  }
  auto srcB = GetRepositoryGeometry(api, bName);
  if (srcB == nullptr ) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  cvPolyData *dst;
  if (sys_geom_union(srcA, srcB, tolerance, &dst) != SV_OK) {
      api.error("Error performing a union operation of geometry '" + std::string(aName) + " with '" +  std::string(bName)+ ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s", dst->GetName());
}

//----------------
// Geom_intersect
//----------------
//
PyDoc_STRVAR(Geom_intersect_doc,
  "intersect(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_intersect(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sss|d", PyRunTimeErr, __func__);
  char *aName;
  char *bName;
  char *dstName;
  double tolerance = 1e-6;

  if (!PyArg_ParseTuple(args, api.format, &aName, &bName, &dstName, &tolerance)) {
      return api.argsError();
  }

  // Retrieve operands geometry.
  auto srcA = GetRepositoryGeometry(api, aName);
  if (srcA == nullptr ) {
      return nullptr;
  }
  auto srcB = GetRepositoryGeometry(api, bName);
  if (srcB == nullptr ) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  cvPolyData *dst;
  if (sys_geom_intersect(srcA, srcB, tolerance, &dst) != SV_OK) {
      api.error("Error performing a Boolean intersection of geometry '" + std::string(aName) + " with '" +  std::string(bName)+ ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//---------------
// Geom_subtract
//---------------
//
PyDoc_STRVAR(Geom_subtract_doc,
  "subtract(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_subtract(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sss|d", PyRunTimeErr, __func__);
  char *aName;
  char *bName;
  char *dstName;
  double tolerance = 1e-6;

  if (!PyArg_ParseTuple(args, api.format, &aName, &bName, &dstName, &tolerance)) {
      return api.argsError();
  }

  // Retrieve operands geometry.
  auto srcA = GetRepositoryGeometry(api, aName);
  if (srcA == nullptr ) {
      return nullptr;
  }
  auto srcB = GetRepositoryGeometry(api, bName);
  if (srcB == nullptr ) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  cvPolyData *dst;
  if (sys_geom_subtract(srcA, srcB, tolerance, &dst) != SV_OK) {
      api.error("Error performing a Boolean subtract of geometry '" + std::string(aName) + " with '" +  std::string(bName)+ ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//--------------------
// Geom_check_surface
//--------------------
//
PyDoc_STRVAR(Geom_check_surface_doc,
  "check_surface(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject*
Geom_check_surface(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s|d", PyRunTimeErr, __func__);
  char *srcName;
  double tol = 1e-6;

  if (!PyArg_ParseTuple(args, api.format, &srcName, &tol)) {
      return api.argsError();
  }

  // Retrieve source object.
  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  int stats[2];
  if (sys_geom_checksurface(src, stats, tol) != SV_OK) {
    api.error("Error checking surface for geometry '" + std::string(srcName) + ".");
    return nullptr;
  }

  return Py_BuildValue("ii",stats[0],stats[1]);
}

//------------
// Geom_clean
//------------
//
PyDoc_STRVAR(Geom_clean_doc,
  "check_surface(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_clean(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ss", PyRunTimeErr, __func__);
  char *srcName;
  char *dstName;

  if (!PyArg_ParseTuple(args, api.format, &srcName, &dstName)) {
      return api.argsError();
  }

  // Retrieve source object.
  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  auto dst = sys_geom_Clean(src);
  if (dst == NULL) {
    api.error("Error cleaning geometry '" + std::string(srcName) + ".");
    return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//-----------------------
// Geom_set_ids_for_caps
//-----------------------
//
PyDoc_STRVAR(Geom_set_ids_for_caps_doc,
  "set_ids_for_caps(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_set_ids_for_caps(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ss", PyRunTimeErr, __func__);
  char *srcName;
  char *dstName;

  if (!PyArg_ParseTuple(args, api.format, &srcName, &dstName)) {
      return api.argsError();
  }

  // Retrieve source object.
  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  int *doublecaps;
  int numfaces=0;
  cvPolyData *dst;

  if (sys_geom_set_ids_for_caps(src, &dst, &doublecaps, &numfaces) != SV_OK) {
      api.error("Error setting cap IDs for geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      delete [] doublecaps;
      return nullptr;
  }

  PyObject* pylist = PyList_New(numfaces);
  for (int i=0; i<numfaces; i++){
      PyList_SetItem(pylist, i, PyLong_FromLong(doublecaps[i]));
  }
  delete [] doublecaps;
  return pylist;
}

//----------------------------------
// Geom_set_array_for_local_op_face
//----------------------------------
//
PyDoc_STRVAR(Geom_set_array_for_local_op_face_doc,
  "set_array_for_local_op_face(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_set_array_for_local_op_face(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sssO|si", PyRunTimeErr, __func__);
  char *srcName;
  char *dstName;
  char *arrayName = 0;
  PyObject* values;
  char *outArray = "LocalOpsArray";
  int dataType = 1;

  if (!PyArg_ParseTuple(args, api.format, &srcName,&dstName,&arrayName,&values,&outArray,&dataType)) {
      return api.argsError();
  }

  // Retrieve source object.
  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  int nvals = PyList_Size(values);
  if (nvals == 0) {
      return SV_PYTHON_OK;
  }

  std::vector<int> vals;
  for (int i =0; i<nvals;i++) {
    vals.push_back(PyLong_AsLong(PyList_GetItem(values,i)));
  }

  if (PyErr_Occurred() != NULL) {
      api.error("Error parsing values list argument.");
      return nullptr;
  }

  cvPolyData *dst;
  if (sys_geom_set_array_for_local_op_face(src, &dst, arrayName,vals.data(),nvals,outArray,dataType) != SV_OK) {
      api.error("Error setting local op array for geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}


//-----------------------------------
// Geom_set_array_for_local_op_cells
//-----------------------------------
//
PyDoc_STRVAR(Geom_set_array_for_local_op_cells_doc,
  "set_array_for_local_op_cells(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_set_array_for_local_op_cells(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ssO|si", PyRunTimeErr, __func__);
  char *srcName;
  char *dstName;
  PyObject* values;
  char *outArray = "LocalOpsArray";
  int dataType = 1;

  if (!PyArg_ParseTuple(args, api.format, &srcName,&dstName,&values,&outArray,&dataType)) {
      return api.argsError();
  }

  // Retrieve source object:
  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  int nvals = PyList_Size(values);
  if (nvals == 0) {
      return SV_PYTHON_OK;
  }

  std::vector<int> vals;
  for (int i =0; i<nvals;i++) {
    vals.push_back(PyLong_AsLong(PyList_GetItem(values,i)));
  }

  if (PyErr_Occurred() != NULL) {
      api.error("Error parsing values list argument.");
      return nullptr;
  }

  cvPolyData *dst;
  if (sys_geom_set_array_for_local_op_cells(src, &dst, vals.data(), nvals, outArray, dataType) != SV_OK) {
    PyErr_SetString(PyRunTimeErr, "error creating array on surface" );
      api.error("Error setting local op array for geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  // [TODO:DaveP] dst is not deleted?

  return Py_BuildValue("s",dst->GetName());
}

//-----------------------------------
// Geom_set_array_for_local_op_blend
//-----------------------------------
//
PyDoc_STRVAR(Geom_set_array_for_local_op_blend_doc,
  "set_array_for_local_op_blend(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_set_array_for_local_op_blend(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sssOd|si", PyRunTimeErr, __func__);
  char *srcName;
  char *dstName;
  char *arrayName = 0;
  PyObject* values;
  double radius;
  char *outArray = "LocalOpsArray";
  int dataType = 1;

  if (!PyArg_ParseTuple(args, api.format, &srcName,&dstName,&arrayName,&values,&radius,&outArray,&dataType)) {
      return api.argsError();
  }

  // Retrieve source object:
  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  // [TODO:DaveP] need to check 'values' is list.

  if (PyList_Size(values) == 0) {
      return SV_PYTHON_OK;
  }

  int nvals = PyList_Size(values);
  std::vector<int> vals;

  for (int i =0; i<nvals;i++) {
    vals.push_back(PyLong_AsLong(PyList_GetItem(values,i)));
  }

  if (PyErr_Occurred() != NULL) {
      api.error("Error parsing values list argument.");
      return nullptr;
  }

  cvPolyData* dst;
  if (sys_geom_set_array_for_local_op_face_blend(src, &dst,arrayName,vals.data(),nvals,radius,outArray,dataType) != SV_OK) {
      api.error("Error setting local op array for geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//------------------------
// Geom_local_decimation
//------------------------
//
PyDoc_STRVAR(Geom_local_decimation_doc,
  "local_decimation(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_local_decimation(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ss|dss", PyRunTimeErr, __func__);
  char *srcName;
  char *dstName;
  double target = 0.25;
  char *pointArrayName = 0;
  char *cellArrayName = 0;

  if (!PyArg_ParseTuple(args, api.format, &srcName,&dstName,&target,&pointArrayName,&cellArrayName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  cvPolyData *dst;
  if (sys_geom_local_quadric_decimation(src, &dst, target, pointArrayName,cellArrayName) != SV_OK) {
      api.error("Error decimating geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//-----------------------------
// Geom_local_laplacian_smooth
//-----------------------------
//
PyDoc_STRVAR(Geom_local_laplacian_smooth_doc,
  "local_laplacian_smooth(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_local_laplacian_smooth(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ss|idss", PyRunTimeErr, __func__);
  char *srcName;
  char *dstName;
  int numiters = 100;
  double relax = 0.01;
  char *pointArrayName = 0;
  char *cellArrayName = 0;

  if (!PyArg_ParseTuple(args, api.format, &srcName,&dstName,&numiters,&relax,&pointArrayName,&cellArrayName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  cvPolyData *dst;
  if (sys_geom_local_laplacian_smooth(src, &dst, numiters,relax, pointArrayName,cellArrayName) != SV_OK) {
      api.error("Error in the laplacian smooth operation on geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//-----------------------------
// Geom_local_constrain_smooth
//-----------------------------
//
PyDoc_STRVAR(Geom_local_constrain_smooth_doc,
  "local_constrain_smooth(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_local_constrain_smooth(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ss|idiss", PyRunTimeErr, __func__);
  char *srcName;
  char *dstName;
  char *pointArrayName = 0;
  char *cellArrayName = 0;
  int numiters = 5;
  double constrainfactor = 0.7;
  int numcgsolves = 30;

  if (!PyArg_ParseTuple(args, api.format, &srcName,&dstName,&numiters,&constrainfactor, &numcgsolves,&pointArrayName,&cellArrayName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  cvPolyData *dst;
  if (sys_geom_local_constrain_smooth(src, &dst, numiters,constrainfactor,numcgsolves, pointArrayName,cellArrayName) != SV_OK) {
      api.error("Error in the local contrain smooth operation on geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

// ------------------------------
// Geom_local_linear_subdivision
// ------------------------------
//
PyDoc_STRVAR(Geom_local_linear_subdivision_doc,
  "local_linear_subdivision(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_local_linear_subdivision(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ss|iss", PyRunTimeErr, __func__);
  char *srcName;
  char *dstName;
  int numiters = 100;
  char *pointArrayName = 0;
  char *cellArrayName = 0;

  if (!PyArg_ParseTuple(args, api.format, &srcName,&dstName,&numiters,&pointArrayName,&cellArrayName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  cvPolyData *dst;
  if (sys_geom_local_linear_subdivision(src, &dst, numiters,pointArrayName,cellArrayName) != SV_OK) {
      api.error("Error in the local linear subdivision operation on geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//-----------------------------------
// Geom_local_butterfly_subdivision
//-----------------------------------
//
PyDoc_STRVAR(Geom_local_butterfly_subdivision_doc,
  "local_butterfly_subdivision(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_local_butterfly_subdivision(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ss|iss", PyRunTimeErr, __func__);
  char *srcName;
  char *dstName;
  int numiters = 100;
  char *pointArrayName = 0;
  char *cellArrayName = 0;

  if (!PyArg_ParseTuple(args, api.format, &srcName,&dstName,&numiters,&pointArrayName,&cellArrayName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  cvPolyData *dst;
  if (sys_geom_local_butterfly_subdivision(src, &dst, numiters,pointArrayName,cellArrayName) != SV_OK) {
      api.error("Error in the local butterfly subdivision operation on geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//-----------------------------
// Geom_local_loop_subdivision
//-----------------------------
//
PyDoc_STRVAR(Geom_local_loop_subdivision_doc,
  "local_butterfly_subdivision(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_local_loop_subdivision(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ss|iss", PyRunTimeErr, __func__);
  char *srcName;
  char *dstName;
  int numiters = 100;
  char *pointArrayName = 0;
  char *cellArrayName = 0;

  if (!PyArg_ParseTuple(args, api.format, &srcName,&dstName,&numiters,&pointArrayName,&cellArrayName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  cvPolyData *dst;
  if (sys_geom_local_loop_subdivision(src, &dst, numiters,pointArrayName,cellArrayName) != SV_OK) {
      api.error("Error in the local loop subdivision operation on geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}


//-----------------
// Geom_all_union
//-----------------
//
PyDoc_STRVAR(Geom_all_union_doc,
  "all_union(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject*
Geom_all_union(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("Ois|d", PyRunTimeErr, __func__);
  PyObject* srcList;
  int interT;
  char *dstName;
  double tolerance = 1e-5;

  if (!PyArg_ParseTuple(args, api.format, &srcList, &interT, &dstName, &tolerance)) {
      return api.argsError();
  }

  // Check that sources are in the repository.
  //
  std::vector<cvPolyData*> srcs;
  if (!GetGeometryObjects(api, srcList, srcs)) {
      return nullptr;
  }
  auto numSrcs = srcs.size();

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  cvPolyData *dst;
  if (sys_geom_all_union(srcs.data(), numSrcs,interT, tolerance, &dst) != SV_OK) {
      api.error("Error in the all union operation.");
      return nullptr;
  }

  // Create new solid:
  auto geom = cvSolidModel::pyDefaultInstantiateSolidModel();
  if (geom == NULL) {
      api.error("Error creating solid model.");
      return nullptr;
  }

  auto dstPd = dst->GetVtkPolyData();
  geom->SetVtkPolyDataObject(dstPd);

  if (!AddGeometryToRepository(api, dstName, dst)) {
      delete geom;
      return nullptr;
  }

  return Py_BuildValue("s",geom->GetName());
}

//-----------------------------
// Geom_convert_nurbs_to_poly
//-----------------------------
//
// [TODO:DaveP] not sure about this function name.
//
PyDoc_STRVAR(Geom_convert_nurbs_to_poly_doc,
  "convert_nurbs_to_poly(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_convert_nurbs_to_poly(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sOOs", PyRunTimeErr, __func__);
  char *srcName;
  PyObject* faceList;
  PyObject* idList;
  char *dstName;

  if (!PyArg_ParseTuple(args, api.format, &srcName, &faceList, &idList, &dstName)) {
      return api.argsError();
  }

  auto model = GetRepositoryGeometry(api, srcName);
  if (model == NULL) {
      return nullptr;
  }

  if (!PyList_Check(faceList)) {
      api.error("Face list argument is not a Python list.");
      return nullptr;
  }

  if (!PyList_Check(idList)) {
      api.error("ID list argument is not a Python list.");
      return nullptr;
  }

  auto numFaces = PyList_Size(faceList);
  auto numIds = PyList_Size(idList);
  if (numFaces != numIds) {
      api.error("The number of IDs (" + std::to_string(numIds)+") != the number of faces ("+std::to_string(numFaces)+").");
  }

  // Check that sources are in the repository.
  //
  std::vector<cvPolyData*> faces;
  if (!GetGeometryObjects(api, faceList, faces)) {
      return nullptr;
  }

  std::vector<int> allids;
  for (int i=0; i<numIds;i++) {
      allids.push_back(PyLong_AsLong(PyList_GetItem(idList,i)));
  }

  if (PyErr_Occurred() != NULL) {
      api.error("Error parsing values ID list argument.");
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  // Instantiate the new solid:
  auto geom = cvSolidModel::pyDefaultInstantiateSolidModel( );
  if (geom == NULL ) {
      api.error("Error creating solid model.");
      return nullptr;
  }

  cvPolyData *dst;
  if (sys_geom_assign_ids_based_on_faces(model, faces.data(), numFaces, allids.data(), &dst) != SV_OK) {
      delete dst;
      api.error("Error in the convert nurbs to poly operation on geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  auto dstPd = dst->GetVtkPolyData();
  geom->SetVtkPolyDataObject(dstPd);

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",geom->GetName());
}

//----------------------------
// Geom_make_polys_consistent
//----------------------------
//
PyDoc_STRVAR(Geom_make_polys_consistent_doc,
  "make_polys_consistent(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_make_polys_consistent(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ss", PyRunTimeErr, __func__);
  char *srcName;
  char *dstName;

  if (!PyArg_ParseTuple(args, api.format, &srcName, &dstName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  cvPolyData *dst;
  if (sys_geom_MakePolysConsistent(src, &dst) != SV_OK) {
      api.error("Error in the make polygons consistent operation on geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//------------------------
// Geom_reverse_all_cells
//------------------------
//
PyDoc_STRVAR(Geom_reverse_all_cells_doc,
  "reverse_all_cells(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_reverse_all_cells(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ss", PyRunTimeErr, __func__);
  char *srcName;
  char *dstName;

  if (!PyArg_ParseTuple(args, api.format, &srcName, &dstName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  cvPolyData *dst;
  if (sys_geom_ReverseAllCells(src, &dst) != SV_OK) {
      api.error("Error in the reverse all cells operation on geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//------------------------------
// Geom_num_closed_line_regions
//-----------------------------
//
PyDoc_STRVAR(Geom_num_closed_line_regions_doc,
  "num_closed_line_regions(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_num_closed_line_regions(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char *srcName;

  if (!PyArg_ParseTuple(args, api.format, &srcName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  int num;
  if (sys_geom_NumClosedLineRegions(src, &num) != SV_OK) {
      api.error("Error in the num closed line regions operation on geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  return Py_BuildValue("i",PyLong_FromLong(num));
}

//-----------------------------
// Geom_get_closed_line_region
//-----------------------------
//
PyDoc_STRVAR(Geom_get_closed_line_region_doc,
  "get_closed_line_region(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_get_closed_line_region(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sis", PyRunTimeErr, __func__);
  char *srcName;
  char *dstName;
  int id;

  if (!PyArg_ParseTuple(args, api.format, &srcName, &id, &dstName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  cvPolyData *dst;
  if (sys_geom_GetClosedLineRegion(src, id, &dst) != SV_OK) {
      api.error("Error in the get closed line region operation on geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//-----------
// Geom_pick
//-----------
//
PyDoc_STRVAR(Geom_pick_doc,
  "pick(kernel)                                    \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_pick(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sOs", PyRunTimeErr, __func__);
  char *objName;
  PyObject* posList;
  char *resultName;

  if (!PyArg_ParseTuple(args, api.format, &objName,&posList,&resultName)) {
      return api.argsError();
  }

  auto obj = GetRepositoryGeometry(api, objName);
  if (obj == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, resultName)) {
      return nullptr;
  }

  std::string emsg;
  if (!svPyUtilCheckPointData(posList, emsg)) {
      api.error("The point argument " + emsg);
      return nullptr;
  }

  double pos[3];
  for (int i=0;i<3;i++) {
      pos[i] = PyFloat_AsDouble(PyList_GetItem(posList,i));
  }

  cvPolyData* result;
  if (sys_geom_Pick(obj, pos, &result) != SV_OK ) {
      api.error("Error performing a pick operation on geometry '" + std::string(objName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, resultName, result)) {
      return nullptr;
  }

  return SV_PYTHON_OK;
}

//---------------------
// Geom_orient_profile
//---------------------
//
PyDoc_STRVAR(Geom_orient_profile_doc,
  "orient_profile(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_orient_profile(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sOOOs", PyRunTimeErr, __func__);
  char *srcName;
  PyObject* pathPosList;
  PyObject* pathTanList;
  PyObject* pathXhatList;
  char *dstName;

  if (!PyArg_ParseTuple(args, api.format, &srcName,&pathPosList,&pathTanList,&pathXhatList,&dstName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  // Get position, tangent and xhat data.
  //
  std::string emsg;
  double ppt[3];
  if (!svPyUtilGetPointData(pathPosList, emsg, ppt)) {
      api.error("The point argument " + emsg);
      return nullptr;
  }

  double ptan[3];
  if (!svPyUtilGetPointData(pathTanList, emsg, ptan)) {
      api.error("The tangent argument " + emsg);
      return nullptr;
  }

  double xhat[3];
  if (!svPyUtilGetPointData(pathXhatList, emsg, xhat)) {
      api.error("The xhat argument " + emsg);
      return nullptr;
  }

  cvPolyData *dst;
  if (sys_geom_OrientProfile(src, ppt, ptan, xhat, &dst) != SV_OK) {
      api.error("Error in the orient profile operation on geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//------------------------
// Geom_disorient_profile
//------------------------
//
// [TODO:DaveP] I can only wonder what 'disorient profile' does!
//
PyDoc_STRVAR(Geom_disorient_profile_doc,
  "disorient_profile(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_disorient_profile(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sOOOs", PyRunTimeErr, __func__);
  char *srcName;
  PyObject* pathPosList;
  PyObject* pathTanList;
  PyObject* pathXhatList;
  char *dstName;

  if (!PyArg_ParseTuple(args,"sOOOs", &srcName,&pathPosList,&pathTanList,&pathXhatList,&dstName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  // Get position, tangent and xhat data.
  //
  std::string emsg;
  double ppt[3];
  if (!svPyUtilGetPointData(pathPosList, emsg, ppt)) {
      api.error("The point argument " + emsg);
      return nullptr;
  }

  double ptan[3];
  if (!svPyUtilGetPointData(pathTanList, emsg, ptan)) {
      api.error("The tangent argument " + emsg);
      return nullptr;
  }

  double xhat[3];
  if (!svPyUtilGetPointData(pathXhatList, emsg, xhat)) {
      api.error("The xhat argument " + emsg);
      return nullptr;
  }

  cvPolyData *dst;
  if ( sys_geom_DisorientProfile(src, ppt, ptan, xhat, &dst) != SV_OK) {
      api.error("Error in the disorient profile operation on geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}


//----------------
// Geom_translate
//----------------
//
PyDoc_STRVAR(Geom_translate_doc,
  "translate(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_translate(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sOs", PyRunTimeErr, __func__);
  char *srcName;
  PyObject* vecList;
  char *dstName;

  int n;

  if (!PyArg_ParseTuple(args,api.format, &srcName, &vecList, &dstName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  // Get vec data.
  //
  std::string emsg;
  double vec[3];
  if (!svPyUtilGetPointData(vecList, emsg, vec)) {
      api.error("The vec argument " + emsg);
      return nullptr;
  }

  cvPolyData *dst;
  if (sys_geom_Translate(src, vec, &dst) != SV_OK) {
      api.error("Error in the translate operation on geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//----------------
// Geom_scale_avg
//----------------
//
PyDoc_STRVAR(Geom_scale_avg_doc,
  "scale_avg(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_scale_avg(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sds", PyRunTimeErr, __func__);
  char *srcName;
  double factor;
  char *dstName;

  if (!PyArg_ParseTuple(args, api.format, &srcName, &factor, &dstName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  cvPolyData *dst;
  if (sys_geom_ScaleAvg(src, factor, &dst) != SV_OK) {
      api.error("Error performing the scaling operation on geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//-------------------------
// Geom_get_ordered_points
//-------------------------
//
PyDoc_STRVAR(Geom_get_ordered_points_doc,
  "get_ordered_points(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_get_ordered_points(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char *srcName;

  if (!PyArg_ParseTuple(args, api.format, &srcName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  cvPolyData *dst;
  double *pts;
  int num;

  if (sys_geom_GetOrderedPts(src, &pts, &num) != SV_OK) {
      api.error("Error geting ordered points from the geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  // Convert returned points array to Python list.
  //
  // [TODO:DaveP] must remove the C-style array addressing.
  //
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

//---------------------------
// Geom_write_ordered_points
//---------------------------
//
PyDoc_STRVAR(Geom_write_ordered_points_doc,
  "write_ordered_points(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_write_ordered_points(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ss", PyRunTimeErr, __func__);
  char *objName;
  char *fileName;

  if (!PyArg_ParseTuple(args, api.format, &objName, &fileName)) {
      return api.argsError();
  }

  auto obj = GetRepositoryGeometry(api, objName);
  if (obj == NULL) {
      return nullptr;
  }

  if (sys_geom_WriteOrderedPts(obj, fileName) != SV_OK) {
      api.error("Error writing geometry '" + std::string(objName) + " to the file '" + std::string(fileName) + "'.");
      return nullptr;
  }

  return SV_PYTHON_OK;
}

//------------------
// Geom_write_lines
//------------------
//
PyDoc_STRVAR(Geom_write_lines_doc,
  "write_lines(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_write_lines(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ss", PyRunTimeErr, __func__);
  char *objName;
  char *fileName;

  if (!PyArg_ParseTuple(args,api.format, &objName,&fileName)) {
      return api.argsError();
  }

  auto obj = GetRepositoryGeometry(api, objName);
  if (obj == NULL) {
      return nullptr;
  }

  if (sys_geom_WriteLines(obj, fileName) != SV_OK) {
      api.error("Error writing lines geometry '" + std::string(objName) + " to the file '" + std::string(fileName) + "'.");
      return nullptr;
  }

  return SV_PYTHON_OK;
}

//-------------------
// Geom_polys_closed
//-------------------
//
PyDoc_STRVAR(Geom_polys_closed_doc,
  "polys_closed(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_polys_closed(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char *srcName;

  if (!PyArg_ParseTuple(args,api.format, &srcName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  int closed;
  if (sys_geom_PolysClosed(src, &closed) != SV_OK) {
      api.error("Error performing a polys closed operation for the geometry '" + std::string(srcName)+ "'.");
      return nullptr;
  }

  return Py_BuildValue("N", PyBool_FromLong(closed));
}

//-------------------
// Geom_surface_area
//-------------------
//
PyDoc_STRVAR(Geom_surface_area_doc,
  "surface_area(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_surface_area(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char *srcName;

  if (!PyArg_ParseTuple(args, api.format, &srcName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  double area;
  if (sys_geom_SurfArea(src, &area) != SV_OK) {
      api.error("Error computing the area for the geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  return Py_BuildValue("d",area);
}

//------------------------
// Geom_get_poly_centroid
//------------------------
//
PyDoc_STRVAR(Geom_get_poly_centroid_doc,
  "get_poly_centroid(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_get_poly_centroid(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char *srcName;

  if (!PyArg_ParseTuple(args, api.format, &srcName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  double centroid[3];
  if (sys_geom_getPolyCentroid(src, centroid) != SV_OK) {
      api.error("Error computing the centroid for the geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  return Py_BuildValue("ddd",centroid[0], centroid[1], centroid[2]);
}

//----------------------
// Geom_print_tri_stats
//----------------------
//
PyDoc_STRVAR(Geom_print_tri_stats_doc,
  "Geom_print_tri_stats(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_print_tri_stats(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char *srcName;

  if (!PyArg_ParseTuple(args, api.format, &srcName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (sys_geom_PrintTriStats(src) != SV_OK) {
      api.error("Error printing tri stats for the geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  return SV_PYTHON_OK;
}

//------------------------
// Geom_print_small_polys
//------------------------
//
PyDoc_STRVAR(Geom_print_small_polys_doc,
  "print_small_polys(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_print_small_polys(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char *srcName;
  double sideTol;

  if (!PyArg_ParseTuple(args, api.format, &srcName, &sideTol)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (sys_geom_PrintSmallPolys( (cvPolyData*)src, sideTol ) != SV_OK ) {
      api.error("Error printing small polys for the geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  return SV_PYTHON_OK;
}

//-------------------------
// Geom_remove_small_polys
//-------------------------
//
PyDoc_STRVAR(Geom_remove_small_polys_doc,
  "remove_small_polys(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_remove_small_polys(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ssd", PyRunTimeErr, __func__);
  char *srcName;
  char *dstName;
  double sideTol;

  if (!PyArg_ParseTuple(args, api.format, &srcName,&dstName,&sideTol)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  cvPolyData *dst;
  if (sys_geom_RmSmallPolys(src, sideTol, &dst) != SV_OK) {
      api.error("Error removing small polygons from the geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//-----------
// Geom_bbox
//-----------
//
PyDoc_STRVAR(Geom_bbox_doc,
  "Geom_bbox(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_bbox(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char *objName;

  if (!PyArg_ParseTuple(args, api.format, &objName)) {
      return api.argsError();
  }

  auto obj = GetRepositoryGeometry(api, objName);
  if (obj == NULL) {
      return nullptr;
  }

  double bbox[6];
  if (sys_geom_BBox(obj, bbox) != SV_OK) {
      api.error("Error getting the bounding box for the geometry '" + std::string(objName) + ".");
      return nullptr;
  }

  PyObject* pylist = PyList_New(6);
  for (int i = 0; i < 6; i++ ) {
      PyList_SetItem(pylist, i, PyFloat_FromDouble(bbox[i]));
  }

  return pylist;
}


//--------------------
// Geom_point_in_poly
//--------------------
//
PyDoc_STRVAR(Geom_point_in_poly_doc,
  "point_in_poly(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_point_in_poly(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sOi", PyRunTimeErr, __func__);
  char *objName;
  PyObject* ptList;
  int usePrevPoly = 0;

  if (!PyArg_ParseTuple(args, api.format, &objName,&ptList,&usePrevPoly)) {
      return api.argsError();
  }

  auto obj = GetRepositoryGeometry(api, objName);
  if (obj == NULL) {
      return nullptr;
  }

  double pt[3];
  std::string emsg;
  if (!svPyUtilGetPointData(ptList, emsg, pt)) {
      api.error("The point argument " + emsg);
      return nullptr;
  }

  int ans;
  if (sys_geom_PtInPoly(obj, pt ,usePrevPoly, &ans) != SV_OK) {
      api.error("Error classifying a point in a poly for the geometry '" + std::string(objName) + ".");
      return nullptr;
  }

  return Py_BuildValue("i",ans);
}

//-------------------
// Geom_merge_points
//-------------------
//
PyDoc_STRVAR(Geom_merge_points_doc,
  "merge_points(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_merge_points(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ssd", PyRunTimeErr, __func__);
  char *srcName;
  char *dstName;
  double tol = 1e10 * FindMachineEpsilon();

  if (!PyArg_ParseTuple(args, api.format, &srcName,&dstName,&tol)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  // [TODO:DaveP] we now see two diffetent return patters.
  auto dst = sys_geom_MergePts_tol(src, tol);
  if (dst == nullptr) {
      api.error("Error merging points poly for the geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//---------------------
// Geom_warp_3d_points
//---------------------
//
PyDoc_STRVAR(Geom_warp_3d_points_doc,
  "warp_3d_points(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_warp_3d_points(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ssd", PyRunTimeErr, __func__);
  char *srcName;
  char *dstName;
  double scale = 1.0;

  if (!PyArg_ParseTuple(args,api.format, &srcName,&dstName,&scale)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  auto dst = sys_geom_warp3dPts(src, scale);
  if (dst == nullptr) {
      api.error("Error warping 3D points from the geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//-----------------
// Geom_num_points
//-----------------
//
PyDoc_STRVAR(Geom_num_points_doc,
  "num_points(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_num_points(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char *srcName;

  if (!PyArg_ParseTuple(args, api.format, &srcName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  auto num = src->GetVtkPolyData()->GetNumberOfPoints();

  return Py_BuildValue("i", num);
}




//---------------------
// Geom_winding_number
//---------------------
//
PyDoc_STRVAR(Geom_winding_number_doc,
  "winding_number(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_winding_number(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char *objName;

  if (!PyArg_ParseTuple(args, api.format, &objName)) {
      return api.argsError();
  }

  auto obj = GetRepositoryGeometry(api, objName);
  if (obj == NULL) {
      return nullptr;
  }

  auto wnum = sys_geom_2DWindingNum(obj);

  return Py_BuildValue("i",wnum);
}

//---------------------
// Geom_polygon_normal
//---------------------
//
PyDoc_STRVAR(Geom_polygon_normal_doc,
  "polygon_norma(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_polygon_normal(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char *objName;

  if (!PyArg_ParseTuple(args, api.format, &objName)) {
      return api.argsError();
  }

  auto obj = GetRepositoryGeometry(api, objName);
  if (obj == NULL) {
      return nullptr;
  }

  double normal[3];

  if (sys_geom_PolygonNormal(obj, normal) != SV_OK) {
      api.error("Error calculating the normal for the geometry '" + std::string(objName) + ".");
      return nullptr;
  }

  return Py_BuildValue("ddd",normal[0],normal[1],normal[2]);
}


//-----------
// Geom_copy
//-----------
//
PyDoc_STRVAR(Geom_copy_doc,
  "Geom_copy(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_copy(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ss", PyRunTimeErr, __func__);
  char *srcName;
  char *dstName;

  if (!PyArg_ParseTuple(args, api.format, &srcName, &dstName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  auto dst = sys_geom_DeepCopy(src);
  if (dst == NULL) {
      api.error("Error copying the geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//----------------------
// Geom_reorder_polygon
//----------------------
//
PyDoc_STRVAR(Geom_reorder_polygon_doc,
  "reorder_polygon(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_reorder_polygon(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sis", PyRunTimeErr, __func__);
  char *srcName;
  int start;
  char *dstName;

  if (!PyArg_ParseTuple(args, api.format, &srcName, &start, &dstName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  auto dst = sys_geom_ReorderPolygon( (cvPolyData*)src, start );
  if (dst == NULL) {
      api.error("Error repordering a polygon for the geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//---------------------------------
// Geom_spline_points_to_path_plan
//---------------------------------
//
PyDoc_STRVAR(Geom_spline_points_to_path_plan_doc,
  "spline_points_to_path_plan(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_spline_points_to_path_plan(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sii|s", PyRunTimeErr, __func__);
  char *srcName;
  int numOutputPts;
  int flag;
  char *filename = NULL;

  if (!PyArg_ParseTuple(args, api.format, &srcName, &numOutputPts, &flag, &filename)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  int result;
  char *output;
  if (filename == NULL) {
    result = pysys_geom_splinePtsToPathPlan(src->GetVtkPolyData(),numOutputPts, filename, flag, &output);
  } else {
    result = pysys_geom_splinePtsToPathPlan(src->GetVtkPolyData(),numOutputPts, filename, flag, NULL);
  }

  if (result != SV_OK) {
      api.error("Error writing spline points for the geometry '" + std::string(srcName) + ".");
      return nullptr;
  }

  if (filename == NULL) {
      return Py_BuildValue("s",output);
  } else {
      return SV_PYTHON_OK;
  }
}

//------------------------
// Geom_integrate_surface
//------------------------
//
PyDoc_STRVAR(Geom_integrate_surface_doc,
  "integrate_surface(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_integrate_surface(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sOi", PyRunTimeErr, __func__);
  char *objName;
  PyObject* nrmList;
  int tensorType;

  if (!PyArg_ParseTuple(args, api.format, &objName, &nrmList, &tensorType)) {
      return api.argsError();
  }

  std::string emsg;
  double normal[3];
  if (!svPyUtilGetPointData(nrmList, emsg, normal)) {
      api.error("The normal argument " + emsg);
      return nullptr;
  }

  auto obj = GetRepositoryGeometry(api, objName);
  if (obj == NULL) {
      return nullptr;
  }

  double q = 0.0;
  if (sys_geom_IntegrateSurface(obj, tensorType, normal, &q) != SV_OK) {
      api.error("Error calculating surface integral for the geometry '" + std::string(objName) + ".");
      return nullptr;
  }

  return Py_BuildValue("d",q);
}

//-------------------------
// Geom_integrate_surface2
//-------------------------
//
PyDoc_STRVAR(Geom_integrate_surface2_doc,
  "integrate_surface2(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_integrate_surface2(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("si", PyRunTimeErr, __func__);
  char *objName;
  int tensorType;

  if (!PyArg_ParseTuple(args, api.format, &objName, &tensorType)) {
      return api.argsError();
  }

  auto obj = GetRepositoryGeometry(api, objName);
  if (obj == NULL) {
      return nullptr;
  }

  double q = 0.0;
  double area = 0.0;
  if (sys_geom_IntegrateSurface2(obj, tensorType, &q, &area) != SV_OK) {
      api.error("Error calculating surface integral for the geometry '" + std::string(objName) + ".");
      return nullptr;
  }

  return Py_BuildValue("dd",q,area);
}

//-----------------------
// Geom_integrate_energy
//-----------------------
//
PyDoc_STRVAR(Geom_integrate_energy_doc,
  "integrate_energy(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_integrate_energy(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sOd", PyRunTimeErr, __func__);
  char *objName;
  PyObject* nrmList;
  double rho = 0.0;

  if (!PyArg_ParseTuple(args, api.format, &objName,&nrmList,&rho)) {
      return api.argsError();
  }

  std::string emsg;
  double normal[3];
  if (!svPyUtilGetPointData(nrmList, emsg, normal)) {
      api.error("The normal argument " + emsg);
      return nullptr;
  }

  auto obj = GetRepositoryGeometry(api, objName);
  if (obj == NULL) {
      return nullptr;
  }

  double energy = 0.0;
  if (sys_geom_IntegrateEnergy(obj, rho, normal, &energy) != SV_OK ) {
      api.error("Error calculating the energy integral for the geometry '" + std::string(objName) + ".");
      return nullptr;
  }

  return Py_BuildValue("d",energy);
}

//--------------------
// Geom_find_distance
//--------------------
//
PyDoc_STRVAR(Geom_find_distance_doc,
  "find_distance(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_find_distance(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sO", PyRunTimeErr, __func__);
  char *objName;
  PyObject* ptList;

  if (!PyArg_ParseTuple(args, api.format, &objName,&ptList)) {
      return api.argsError();
  }

  auto obj = GetRepositoryGeometry(api, objName);
  if (obj == NULL) {
      return nullptr;
  }

  std::string emsg;
  double pt[3];
  if (!svPyUtilGetPointData(ptList, emsg, pt)) {
      api.error("The point argument " + emsg);
      return nullptr;
  }

  auto distance = obj->FindDistance( pt[0], pt[1], pt[2] );

  return Py_BuildValue("d",distance);
}

//-------------------------
// Geom_interpolate_scalar
//-------------------------
//
PyDoc_STRVAR(Geom_interpolate_scalar_doc,
  "interpolate_scalar(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_interpolate_scalar(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sO", PyRunTimeErr, __func__);
  char *objName;
  PyObject* ptList;

  if (!PyArg_ParseTuple(args, api.format, &objName,&ptList)) {
      return api.argsError();
  }

  auto obj = GetRepositoryGeometry(api, objName);
  if (obj == NULL) {
      return nullptr;
  }

  std::string emsg;
  double pt[3];
  if (!svPyUtilGetPointData(ptList, emsg, pt)) {
      api.error("The point argument " + emsg);
      return nullptr;
  }

  double scalar = 0.0;
  if (sys_geom_InterpolateScalar(obj, pt, &scalar) != SV_OK) {
      api.error("Error calculating the scalar integral for the geometry '" + std::string(objName) + ".");
      return nullptr;
  }

  return Py_BuildValue("d",scalar);
}

//-------------------------
// Geom_interpolate_vector
//-------------------------
//
PyDoc_STRVAR(Geom_interpolate_vector_doc,
  "interpolate_vector(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_interpolate_vector(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sO", PyRunTimeErr, __func__);
  char *objName;
  PyObject* ptList;

  if (!PyArg_ParseTuple(args, api.format, &objName,&ptList)) {
      return api.argsError();
  }

  auto obj = GetRepositoryGeometry(api, objName);
  if (obj == NULL) {
      return nullptr;
  }

  std::string emsg;
  double pt[3];
  if (!svPyUtilGetPointData(ptList, emsg, pt)) {
      api.error("The point argument " + emsg);
      return nullptr;
  }

  double vect[3] = {0.0, 0.0, 0.0};
  if ( sys_geom_InterpolateVector(obj, pt, vect) != SV_OK ) {
      api.error("Error interpolating a vector for the geometry '" + std::string(objName) + ".");
      return nullptr;
  }

  PyObject* pList = PyList_New(3);
  for (int i = 0; i<3; i++) {
    PyList_SetItem(pList,i,PyFloat_FromDouble(vect[i]));
  }
  return pList;
}

//--------------------------
// Geom_intersect_with_line
//--------------------------
//
PyDoc_STRVAR(Geom_intersect_with_line_doc,
  "intersect_with_line(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_intersect_with_line(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sOO", PyRunTimeErr, __func__);
  char *objName;
  PyObject *p1List;
  PyObject *p2List;

  if (!PyArg_ParseTuple(args, api.format, &objName, &p1List, &p2List)) {
      return api.argsError();
  }

  auto obj = GetRepositoryGeometry(api, objName);
  if (obj == NULL) {
      return nullptr;
  }

  std::string emsg;
  double pt1[3];
  if (!svPyUtilGetPointData(p1List, emsg, pt1)) {
      api.error("The point1 argument " + emsg);
      return nullptr;
  }

  double pt2[3];
  if (!svPyUtilGetPointData(p2List, emsg, pt2)) {
      api.error("The point2 argument " + emsg);
      return nullptr;
  }

  double intersect[3];
  if (sys_geom_IntersectWithLine(obj, pt1, pt2, intersect) != SV_OK) {
      api.error("Error intersecting the geometry '" + std::string(objName) + " with a line.");
      return nullptr;
  }

  return Py_BuildValue("ddd",intersect[0], intersect[1], intersect[2]);
}

//---------------------
// Geom_add_point_data
//---------------------
//
PyDoc_STRVAR(Geom_add_point_data_doc,
  "add_point_data(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_add_point_data(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sssii", PyRunTimeErr, __func__);
  char *srcNameA;
  char *srcNameB;
  char *dstName;
  int scflag = FALSE;
  int vflag = FALSE;

  if (!PyArg_ParseTuple(args, api.format, &srcNameA, &srcNameB, &dstName, &scflag, &vflag)) {
      return api.argsError();
  }

  auto srcA = GetRepositoryGeometry(api, srcNameA);
  if (srcA == NULL) {
      return nullptr;
  }

  auto srcB = GetRepositoryGeometry(api, srcNameB);
  if (srcB == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  sys_geom_math_scalar sc = SYS_GEOM_NO_SCALAR;
  sys_geom_math_vector v = SYS_GEOM_NO_VECTOR;

  if (scflag) {
      sc = SYS_GEOM_ADD_SCALAR;
  }
  if (vflag) {
      v = SYS_GEOM_ADD_VECTOR;
  }

  cvPolyData *dst;
  if ( sys_geom_mathPointData(srcA, srcB, sc, v, &dst) != SV_OK ) {
      api.error("Error adding point data for the geometry '" + std::string(srcNameA) +
          " and " + std::string(srcNameB) +".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//--------------------------
// Geom_subtract_point_data
//--------------------------
//
PyDoc_STRVAR(Geom_subtract_point_data_doc,
  "subtract_point_data(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_subtract_point_data(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sssii", PyRunTimeErr, __func__);
  char *srcNameA;
  char *srcNameB;
  char *dstName;
  int scflag = FALSE;
  int vflag = FALSE;

  if (!PyArg_ParseTuple(args, api.format, &srcNameA,&srcNameB,&dstName,&scflag,&vflag)) {
      return api.argsError();
  }

  auto srcA = GetRepositoryGeometry(api, srcNameA);
  if (srcA == NULL) {
      return nullptr;
  }

  auto srcB = GetRepositoryGeometry(api, srcNameB);
  if (srcB == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  sys_geom_math_scalar sc = SYS_GEOM_NO_SCALAR;
  sys_geom_math_vector v = SYS_GEOM_NO_VECTOR;
  if (scflag) {
      sc = SYS_GEOM_SUBTRACT_SCALAR;
  }
  if (vflag) {
      v = SYS_GEOM_SUBTRACT_VECTOR;
  }

  cvPolyData *dst;
  if (sys_geom_mathPointData(srcA, srcB, sc, v, &dst) != SV_OK) {
      api.error("Error subtracting point data for the geometry '" + std::string(srcNameA) +
          " and " + std::string(srcNameB) +".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//--------------------------
// Geom_multiply_point_data
//--------------------------
//
PyDoc_STRVAR(Geom_multiply_point_data_doc,
  "multiply_point_data(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_multiply_point_data(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sssii", PyRunTimeErr, __func__);
  char *srcNameA;
  char *srcNameB;
  char *dstName;
  int scflag = FALSE;
  int vflag = FALSE;

  if (!PyArg_ParseTuple(args, api.format, &srcNameA, &srcNameB, &dstName, &scflag, &vflag)) {
      return api.argsError();
  }

  auto srcA = GetRepositoryGeometry(api, srcNameA);
  if (srcA == NULL) {
      return nullptr;
  }

  auto srcB = GetRepositoryGeometry(api, srcNameB);
  if (srcB == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  sys_geom_math_scalar sc = SYS_GEOM_NO_SCALAR;
  sys_geom_math_vector v = SYS_GEOM_NO_VECTOR;
  if (scflag) {
      sc = SYS_GEOM_MULTIPLY_SCALAR;
  }
  if (vflag) {
      v = SYS_GEOM_MULTIPLY_VECTOR;
  }

  cvPolyData *dst;
  if ( sys_geom_mathPointData( (cvPolyData*)srcA, (cvPolyData*)srcB, sc, v, (cvPolyData**)(&dst) ) != SV_OK ) {
      api.error("Error multiplying point data for the geometry '" + std::string(srcNameA) +
          " and " + std::string(srcNameB) +".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//------------------------
// Geom_divide_point_data
//------------------------
//
PyDoc_STRVAR(Geom_divide_point_data_doc,
  "Geom_divide_point_data(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_divide_point_data(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sssii", PyRunTimeErr, __func__);
  char *srcNameA;
  char *srcNameB;
  char *dstName;
  int scflag = FALSE;
  int vflag = FALSE;

  if (!PyArg_ParseTuple(args, api.format, &srcNameA,&srcNameB,&dstName,&scflag,&vflag)) {
      return api.argsError();
  }

  auto srcA = GetRepositoryGeometry(api, srcNameA);
  if (srcA == NULL) {
      return nullptr;
  }

  auto srcB = GetRepositoryGeometry(api, srcNameB);
  if (srcB == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  sys_geom_math_scalar sc = SYS_GEOM_NO_SCALAR;
  sys_geom_math_vector v = SYS_GEOM_NO_VECTOR;

  if (scflag) {
      sc = SYS_GEOM_DIVIDE_SCALAR;
  }
  if (vflag) {
      v = SYS_GEOM_DIVIDE_VECTOR;
  }

  cvPolyData *dst;
  if (sys_geom_mathPointData(srcA, srcB, sc, v, &dst) != SV_OK) {
    PyErr_SetString(PyRunTimeErr, "point data math error" );
      api.error("Error dividing point data for the geometry '" + std::string(srcNameA) +
          " and " + std::string(srcNameB) +".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//--------------
// Geom_project
//---------------
//
PyDoc_STRVAR(Geom_project_doc,
  "project(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_project(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sssii", PyRunTimeErr, __func__);
  char *srcNameA;
  char *srcNameB;
  char *dstName;
  int scflag = FALSE;
  int vflag = FALSE;

  if (!PyArg_ParseTuple(args,api.format, &srcNameA,&srcNameB,&dstName,&scflag,&vflag)) {
      return api.argsError();
  }

  auto srcA = GetRepositoryGeometry(api, srcNameA);
  if (srcA == NULL) {
      return nullptr;
  }

  auto srcB = GetRepositoryGeometry(api, srcNameB);
  if (srcB == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  sys_geom_math_scalar sc = SYS_GEOM_NO_SCALAR;
  sys_geom_math_vector v = SYS_GEOM_NO_VECTOR;

  if (scflag) {
      sc = SYS_GEOM_ADD_SCALAR;
  }
  if (vflag) {
      v = SYS_GEOM_ADD_VECTOR;
  }

  cvPolyData *dst;
  if (sys_geom_Project(srcA, srcB, sc, v, &dst) != SV_OK) {
      api.error("Error projecting point data for the geometry '" + std::string(srcNameA) +
          " and " + std::string(srcNameB) +".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

//-------------------------------
// Geom_integrate_scalar_surface
//-------------------------------
//
PyDoc_STRVAR(Geom_integrate_scalar_surface_doc,
  "integrate_scalar_surface(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_integrate_scalar_surface(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char *srcName;

  if (!PyArg_ParseTuple(args, api.format, &srcName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  double flux;

  if (sys_geom_IntegrateScalarSurf(src, &flux) != SV_OK) {
      api.error("Error integrating scalar over the surface for the geometry '" + std::string(srcName) + "'.");
      return nullptr;
  }

  return Py_BuildValue("d",flux);
}

//---------------------------------
// Geom_integrate_scalar_threshold
//---------------------------------
//
PyDoc_STRVAR(Geom_integrate_scalar_threshold_doc,
  "integrate_scalar_threshold(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_integrate_scalar_threshold(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sd", PyRunTimeErr, __func__);
  char *srcName;
  double wssthresh;

  if (!PyArg_ParseTuple(args, api.format, &srcName,&wssthresh)) {
      return api.argsError();
  }

  if (!PyArg_ParseTuple(args, api.format, &srcName)) {
      return api.argsError();
  }

  auto src = GetRepositoryGeometry(api, srcName);
  if (src == NULL) {
      return nullptr;
  }

  double flux, area;

  if (sys_geom_IntegrateScalarThresh(src, wssthresh, &flux, &area) != SV_OK) {
      api.error("Error in calculating the surface area for the geometry '" + std::string(srcName) + "'.");
      return nullptr;
  }

  return Py_BuildValue("dd",flux, area);
}

//-------------------------
// Geom_replace_point_data
//-------------------------
//
PyDoc_STRVAR(Geom_replace_point_data_doc,
  "replace_point_data(kernel) \n\
   \n\
   ??? Set the computational kernel used to segment image data.       \n\
   \n\
   Args:                                                          \n\
     kernel (str): Name of the contouring kernel. Valid names are: Circle, Ellipse, LevelSet, Polygon, SplinePolygon or Threshold. \n\
");

static PyObject *
Geom_replace_point_data(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sssii", PyRunTimeErr, __func__);
  char *srcNameA;
  char *srcNameB;
  char *dstName;
  int scflag = FALSE;
  int vflag = FALSE;

  if (!PyArg_ParseTuple(args, api.format, &srcNameA,&srcNameB,&dstName,&scflag,&vflag)) {
      return api.argsError();
  }

  auto srcA = GetRepositoryGeometry(api, srcNameA);
  if (srcA == NULL) {
      return nullptr;
  }

  auto srcB = GetRepositoryGeometry(api, srcNameB);
  if (srcB == NULL) {
      return nullptr;
  }

  if (RepositoryGeometryExists(api, dstName)) {
      return nullptr;
  }

  sys_geom_math_scalar sc = SYS_GEOM_NO_SCALAR;
  sys_geom_math_vector v = SYS_GEOM_NO_VECTOR;

  if (scflag) {
      sc = SYS_GEOM_ADD_SCALAR;
  }
  if (vflag) {
      v = SYS_GEOM_ADD_VECTOR;
  }

  cvPolyData *dst;

  if (sys_geom_ReplacePointData(srcA, srcB, sc, v, &dst) != SV_OK) {
      api.error("Error replacing point data for the geometry '" + std::string(srcNameA) +
          " and " + std::string(srcNameB) +".");
      return nullptr;
  }

  if (!AddGeometryToRepository(api, dstName, dst)) {
      return nullptr;
  }

  return Py_BuildValue("s",dst->GetName());
}

#endif // #ifdef python_geom_module_use_old_methods

// ===================================================================================

////////////////////////////////////////////////////////
//          M o d u l e  D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* GEOMETRY_MODULE = "geometry";
static char* GEOMETRY_EXCEPTION = "geometry.Error";
static char* GEOMETRY_EXCEPTION_OBJECT = "Error";

//--------------------
// GeometryModule_doc
//--------------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(GeometryModule_doc,
  "SimVascular geometry module. \n\
   \n\
   The geometry module provides functions for performing geometric operations \n\
   on vtkPolyData objects used to represents vertices, lines and polygons. \n\
");

//---------------
// PyGeomMethods
//---------------
// Geometry module methods.
//
PyMethodDef PyGeomMethods[] =
{
  {"align_profile", (PyCFunction)Geom_align_profile, METH_VARARGS|METH_KEYWORDS, Geom_align_profile_doc},

  {"average_point", (PyCFunction)Geom_average_point, METH_VARARGS, Geom_average_point_doc},

  {"point_inside", (PyCFunction)Geom_point_inside, METH_VARARGS|METH_KEYWORDS, Geom_point_inside_doc},

  {"interpolate_closed_curve", (PyCFunction)Geom_interpolate_closed_curve, METH_VARARGS|METH_KEYWORDS, Geom_interpolate_closed_curve_doc},

  {"local_blend", (PyCFunction)Geom_local_blend, METH_VARARGS|METH_KEYWORDS, Geom_local_blend_doc},

  {"loft", (PyCFunction)Geom_loft, METH_VARARGS|METH_KEYWORDS, Geom_loft_doc},

  {"loft_nurbs", (PyCFunction)Geom_loft_nurbs, METH_VARARGS|METH_KEYWORDS, Geom_loft_nurbs_doc},

  {"local_sphere_smooth", (PyCFunction)Geom_local_sphere_smooth, METH_VARARGS|METH_KEYWORDS, Geom_local_sphere_smooth_doc},

#ifdef python_geom_module_use_old_methods

  {"add_point_data", Geom_add_point_data, METH_VARARGS, Geom_add_point_data_doc},

  {"all_union", Geom_all_union, METH_VARARGS, Geom_all_union_doc},

  // RenameL: AvgPt
  {"bbox", Geom_bbox, METH_VARARGS, Geom_bbox_doc},

  {"check_surface", Geom_check_surface, METH_VARARGS, Geom_check_surface_doc},

  {"clean", Geom_clean, METH_VARARGS, Geom_clean_doc},

  {"copy", Geom_copy, METH_VARARGS, Geom_copy_doc},

  {"disorient_profile", Geom_disorient_profile, METH_VARARGS, Geom_disorient_profile_doc},

  {"divide_point_data", Geom_divide_point_data, METH_VARARGS, Geom_divide_point_data_doc},

  {"find_distance", Geom_find_distance, METH_VARARGS, Geom_find_distance_doc},

  {"get_closed_line_region", Geom_get_closed_line_region, METH_VARARGS, Geom_get_closed_line_region_doc},

  // Rename: GetOrderedPts
  {"get_ordered_points", Geom_get_ordered_points, METH_VARARGS, Geom_get_ordered_points_doc},

  {"get_poly_centroid", Geom_get_poly_centroid, METH_VARARGS, Geom_get_poly_centroid_doc},

  {"integrate_surface", Geom_integrate_surface, METH_VARARGS, Geom_integrate_surface_doc},

  {"integrate_surface2", Geom_integrate_surface2, METH_VARARGS, Geom_integrate_surface2_doc},

  {"integrate_energy", Geom_integrate_energy, METH_VARARGS, Geom_integrate_energy_doc},

  {"integrate_scalar_surface", Geom_integrate_scalar_surface, METH_VARARGS, Geom_integrate_scalar_surface_doc},

  // Renmae: IntegrateScalarThresh
  {"integrate_scalar_threshold", Geom_integrate_scalar_threshold, METH_VARARGS, Geom_integrate_scalar_threshold_doc},

  {"interpolate_scalar", Geom_interpolate_scalar, METH_VARARGS, Geom_interpolate_scalar_doc},

  {"interpolate_vector", Geom_interpolate_vector, METH_VARARGS, Geom_interpolate_vector_doc},

  {"intersect", Geom_intersect, METH_VARARGS, Geom_intersect_doc},

  {"intersect_with_line", Geom_intersect_with_line, METH_VARARGS, Geom_intersect_with_line_doc},

  {"local_butterfly_subdivision", Geom_local_butterfly_subdivision, METH_VARARGS, Geom_local_butterfly_subdivision_doc},

  {"local_constrain_smooth", Geom_local_constrain_smooth, METH_VARARGS, Geom_local_constrain_smooth_doc},

  {"local_decimation", Geom_local_decimation, METH_VARARGS, Geom_local_decimation_doc},

  {"local_laplacian_smooth", Geom_local_laplacian_smooth, METH_VARARGS, Geom_local_laplacian_smooth_doc},

  {"local_linear_subdivision", Geom_local_linear_subdivision, METH_VARARGS, Geom_local_linear_subdivision_doc},

  {"local_loop_subdivision", Geom_local_loop_subdivision, METH_VARARGS, Geom_local_loop_subdivision_doc},


  {"make_polys_consistent", Geom_make_polys_consistent, METH_VARARGS, Geom_make_polys_consistent_doc},

  // Rename: MergePts
  {"merge_points", Geom_merge_points, METH_VARARGS, Geom_merge_points_doc},

  // Renamed: "model_name_model_from_polydata_names"
  {"convert_nurbs_to_poly", Geom_convert_nurbs_to_poly, METH_VARARGS, Geom_convert_nurbs_to_poly_doc},

  {"multiply_point_data", Geom_multiply_point_data, METH_VARARGS, Geom_multiply_point_data_doc},

  {"num_closed_line_regions", Geom_num_closed_line_regions, METH_VARARGS, Geom_num_closed_line_regions_doc},

  // Rename: NumPts
  {"num_points", Geom_num_points, METH_VARARGS, Geom_num_points_doc},

  {"orient_profile", Geom_orient_profile, METH_VARARGS, Geom_orient_profile_doc},

  {"pick", Geom_pick, METH_VARARGS, Geom_pick_doc},

  // Rename: PolygonNorm
  {"polygon_normal", Geom_polygon_normal, METH_VARARGS, Geom_polygon_normal_doc},

  {"polys_closed", Geom_polys_closed, METH_VARARGS, Geom_polys_closed_doc},

  {"print_small_polys", Geom_print_small_polys, METH_VARARGS, Geom_print_small_polys_doc},

  {"print_tri_stats", Geom_print_tri_stats, METH_VARARGS, Geom_print_tri_stats_doc},

  {"project", Geom_project, METH_VARARGS, Geom_project_doc},

  // Rename: PtInPoly
  {"point_in_poly", Geom_point_in_poly, METH_VARARGS, Geom_point_in_poly_doc},

  {"reduce", Geom_reduce, METH_VARARGS, Geom_reduce_doc},

  // Rename: ReorderPgn
  {"reorder_polygon", Geom_reorder_polygon, METH_VARARGS, Geom_reorder_polygon_doc},

  {"replace_point_data", Geom_replace_point_data, METH_VARARGS, Geom_replace_point_data_doc},

  {"reverse_all_cells", Geom_reverse_all_cells, METH_VARARGS, Geom_reverse_all_cells_doc},

  // Rename: RmSmallPolys
  {"remove_small_polys", Geom_remove_small_polys, METH_VARARGS, Geom_remove_small_polys_doc},

  {"scale_avg", Geom_scale_avg, METH_VARARGS, Geom_scale_avg_doc},

  {"set_array_for_local_op_cells", Geom_set_array_for_local_op_cells, METH_VARARGS, Geom_set_array_for_local_op_cells_doc},

  {"set_array_for_local_op_face", Geom_set_array_for_local_op_face, METH_VARARGS, Geom_set_array_for_local_op_face_doc},

  {"set_array_for_local_op_blend", Geom_set_array_for_local_op_blend, METH_VARARGS, Geom_set_array_for_local_op_blend_doc},

  {"set_ids_for_caps", Geom_set_ids_for_caps, METH_VARARGS, Geom_set_ids_for_caps_doc},

  // Rename: SplinePtsToPathPlan
  {"spline_points_to_path_plan", Geom_spline_points_to_path_plan, METH_VARARGS, Geom_spline_points_to_path_plan_doc},

  {"subtract", Geom_subtract, METH_VARARGS, Geom_subtract_doc},

  {"subtract_point_data", Geom_subtract_point_data, METH_VARARGS, Geom_subtract_point_data_doc},

  // Rename: SurfArea
  {"surface_area", Geom_surface_area, METH_VARARGS, Geom_surface_area_doc},

  {"translate", Geom_translate, METH_VARARGS, Geom_translate_doc},

  {"union", Geom_union, METH_VARARGS, Geom_union_doc},

  // Rename:Warp3dPts
  {"warp_3d_points", Geom_warp_3d_points, METH_VARARGS, Geom_warp_3d_points_doc},

  {"winding_number", Geom_winding_number, METH_VARARGS, Geom_winding_number_doc},

  {"write_lines", Geom_write_lines, METH_VARARGS, Geom_write_lines_doc},

  // Rename: WriteOrderedPts
  {"write_ordered_points", Geom_write_ordered_points, METH_VARARGS, Geom_write_ordered_points_doc},

#endif // #ifdef python_geom_module_use_old_methods

  {NULL,NULL}
};


//-----------------------
// Initialize the module
//-----------------------
// Define the initialization function called by the Python
// interpreter when the module is loaded.

//---------------------------------------------------------------------------
//                           PYTHON_MAJOR_VERSION 3
//---------------------------------------------------------------------------

#if PYTHON_MAJOR_VERSION == 3

// Size of per-interpreter state of the module.
// Set to -1 if the module keeps state in global variables.
static int perInterpreterStateSize = -1;

// Always initialize this to PyModuleDef_HEAD_INIT.
static PyModuleDef_Base m_base = PyModuleDef_HEAD_INIT;

// Define the module definition struct which holds all information
// needed to create a module object.

static struct PyModuleDef PyGeomModule = {
   m_base,
   GEOMETRY_MODULE,
   GeometryModule_doc,
   perInterpreterStateSize,
   PyGeomMethods
};

//-------------------
// PyInit_PyGeometry
//-------------------
// The initialization function called by the Python interpreter when the module is loaded.
//
PyMODINIT_FUNC
PyInit_PyGeometry(void)
{
  //std::cout << "========== load geometry module ==========" << std::endl;

  // Initialize the BlenOptions class type.
  SetBlendOptionsTypeFields(PyBlendOptionsType);
  if (PyType_Ready(&PyBlendOptionsType) < 0) {
    fprintf(stdout,"Error in PyBlendOptionsClassType\n");
    return SV_PYTHON_ERROR;
  }

  // Initialize the LoftOptions class type.
  SetLoftOptionsTypeFields(PyLoftOptionsType);
  if (PyType_Ready(&PyLoftOptionsType) < 0) {
    fprintf(stdout,"Error in PyLoftOptionsClassType\n");
    return SV_PYTHON_ERROR;
  }

  // Initialize the LoftNurnsOptions class type.
  SetLoftNurbsOptionsTypeFields(PyLoftNurbsOptionsType);
  if (PyType_Ready(&PyLoftNurbsOptionsType) < 0) {
    fprintf(stdout,"Error in PyLoftNurbsOptionsClassType\n");
    return SV_PYTHON_ERROR;
  }

  // Create the geometry module.
  auto module = PyModule_Create(&PyGeomModule);

  PyRunTimeErr = PyErr_NewException(GEOMETRY_EXCEPTION, NULL, NULL);
  Py_INCREF(PyRunTimeErr);
  PyModule_AddObject(module, GEOMETRY_EXCEPTION_OBJECT, PyRunTimeErr);

  // Add the 'BlendOptions' class.
  Py_INCREF(&PyBlendOptionsType);
  PyModule_AddObject(module, GEOMETRY_BLEND_OPTIONS_CLASS, (PyObject*)&PyBlendOptionsType);
  SetBlendOptionsClassTypes(PyBlendOptionsType);

  // Add the 'LoftOptions' class.
  Py_INCREF(&PyLoftOptionsType);
  PyModule_AddObject(module, GEOMETRY_LOFT_OPTIONS_CLASS, (PyObject*)&PyLoftOptionsType);
  SetLoftOptionsClassTypes(PyLoftOptionsType);

  // Add the 'LoftNurbsOptions' class.
  Py_INCREF(&PyLoftNurbsOptionsType);
  PyModule_AddObject(module, GEOMETRY_LOFT_NURBS_OPTIONS_CLASS, (PyObject*)&PyLoftNurbsOptionsType);

  return module;
}

#endif


//---------------------------------------------------------------------------
//                           PYTHON_MAJOR_VERSION 2
//---------------------------------------------------------------------------

#if PYTHON_MAJOR_VERSION == 2

//------------
// initpyGeom
//------------
//
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

