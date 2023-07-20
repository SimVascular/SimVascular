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

#include "sv_PolyData.h"
#include "Geometry_PyModule.h"
#include "sv_sys_geom.h"
#include "sv_SolidModel.h"
#include "Modeling_PyModule.h"
#include "sv_integrate_surface.h"
#include "sv_misc_utils.h"
#include "sv_vtk_utils.h"
#include "vtkSmartPointer.h"
#include "PyUtils.h"
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

