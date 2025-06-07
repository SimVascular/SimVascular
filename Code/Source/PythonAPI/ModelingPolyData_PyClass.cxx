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

// The functions defined here implement the SV Python API polydata solid class.
//
// The class name is 'PolyData'.
//

#include "sv_sys_geom.h"

#include "sv4gui_ModelUtils.h"
#include <vtkMath.h>
#include "vtkSVGlobals.h"
#include "vtkSVNURBSSurface.h"
#include "sv_vmtk_utils.h"
#include "sv_polydatasolid_utils.h"

//-----------------
// PyPolyDataSolid
//-----------------
// Define the PolyDataSolid class (type).
//
typedef struct {
  PyModelingModel super;
} PyPolyDataSolid;

//////////////////////////////////////////////////////
//          U t i l i t i e s                       //
//////////////////////////////////////////////////////

//------------------------------
// CreateVtkPolyDataFromContour
//------------------------------
// This replicates Contour::CreateVtkPolyDataFromContour(bool includingAllLines)
// defined in sv3/Segmentation/sv3_Contour.cxx.
//
static vtkSmartPointer<vtkPolyData> 
CreateVtkPolyDataFromContour(vtkPolyData* contour)
{
  vtkSmartPointer<vtkPoints> points = vtkSmartPointer<vtkPoints>::New();
  vtkSmartPointer<vtkCellArray> lines = vtkSmartPointer<vtkCellArray>::New();
  int pointNumber = contour->GetNumberOfPoints();
  auto contour_pts = contour->GetPoints();

  for (int i=0; i<=pointNumber; i++) {
    auto pt = contour_pts->GetPoint(i);
    if (i < pointNumber) {
      points->InsertPoint(i, pt[0], pt[1], pt[2]);
    }

    if (i> 0 && i < pointNumber) {
      vtkIdType cell[2] = {i-1,i};
      lines->InsertNextCell(2,cell);
    }else if (i == pointNumber) {
      vtkIdType cell[2] = {i-1,0};
      lines->InsertNextCell(2,cell);
    }
  }

  vtkSmartPointer<vtkPolyData> polyData = vtkSmartPointer<vtkPolyData>::New();
  polyData->SetPoints(points);
  polyData->SetLines(lines);

  return polyData;
}

//-------------------
// CreateLoftSurface
//-------------------
// This replicates sv4guiModelUtils::CreateLoftSurface() defined
// in sv4gui/Modules/Model/Common/sv4gui_ModelUtils.cxx.
//
static vtkPolyData*
CreateLoftSurface(std::vector<vtkPolyData*> contourSet, int numSamplingPts, svLoftingParam& param)
{
  int contourNumber = contourSet.size();
  param.numOutPtsAlongLength = param.samplePerSegment * contourNumber;
  param.numPtsInLinearSampleAlongLength = param.linearMuliplier * param.numOutPtsAlongLength;
  param.numSuperPts = 0;

  for(int i=0;i<contourNumber;i++) {   
    int pointNunumber = contourSet[i]->GetNumberOfPoints();

    if (pointNunumber > param.numSuperPts) {
      param.numSuperPts = pointNunumber;
    }

  }

  if (param.numOutPtsInSegs > param.numSuperPts) {
    param.numSuperPts = param.numOutPtsInSegs;
  }

  int newNumSamplingPts = param.numOutPtsInSegs;

  if (numSamplingPts > 3) {
    newNumSamplingPts = numSamplingPts; 
    if (numSamplingPts > param.numSuperPts) {
      param.numSuperPts = numSamplingPts;
    }
  }

  // Resample contours.
  //
  std::vector<cvPolyData*> superSampledContours;

  for (int i = 0; i < contourNumber; i++) {
    vtkPolyData* vtkpd = vtkPolyData::New();
    vtkpd->DeepCopy(CreateVtkPolyDataFromContour(contourSet[i]));

    cvPolyData* cvpd = new cvPolyData(vtkpd);
    vtkpd->Delete();

    cvPolyData* cvpd2 = sys_geom_sampleLoop(cvpd, param.numSuperPts);

    if (cvpd2 == nullptr) {
      throw std::runtime_error("Supersampling error");
    }
 
    superSampledContours.push_back(cvpd2);
  }

  // Align contours so their first points are the closest points.
  //
  std::vector<cvPolyData*> alignedContours;

  for (int i = 0; i < contourNumber; i++) {
    cvPolyData* cvpd3;
    auto cont = superSampledContours[i]->GetVtkPolyData();

    if (i == 0) {
      alignedContours.push_back(superSampledContours[0]);

    } else {

      if (param.vecFlag == 1) {
        cvpd3 = sys_geom_Align(alignedContours[i-1], superSampledContours[i]);
      } else {
        cvpd3 = sys_geom_AlignByDist(alignedContours[i-1], superSampledContours[i]);
      }

      if (cvpd3 == nullptr) {
        throw std::runtime_error("Alignment error");
      }

      alignedContours.push_back(cvpd3);
    }
  }

  cvPolyData **sampledContours = new cvPolyData*[contourNumber];

  for(int i=0;i<contourNumber;i++) {
    cvPolyData * cvpd4 = sys_geom_sampleLoop(alignedContours[i],newNumSamplingPts);

    if (cvpd4 == NULL) {
      throw std::runtime_error("Sampling error");
    }

    sampledContours[i] = cvpd4;
  }

  // Create a lofted surface spanning the contours.
  //
  cvPolyData *dst;
  vtkPolyData* outpd = NULL;
  bool addCaps = true;

  if (param.method=="spline") {
    if ( sys_geom_loft_solid(sampledContours, contourNumber, param.useLinearSampleAlongLength, param.useFFT,
        param.numOutPtsAlongLength, newNumSamplingPts, param.numPtsInLinearSampleAlongLength, param.numModes,
        param.splineType, param.bias, param.tension, param.continuity, &dst ) != SV_OK ) {
      outpd=NULL;
    } else {

      if (addCaps == 1) {
        outpd = sv4guiModelUtils::CreateOrientClosedPolySolidVessel(dst->GetVtkPolyData());
      } else {
        outpd = sv4guiModelUtils::CreateOrientOpenPolySolidVessel(dst->GetVtkPolyData());
      }
    }

  } else if (param.method == "nurbs") {

    // Degrees of surface
    int uDegree = param.uDegree;
    int vDegree = param.vDegree;

    // Override to maximum possible degree if too large a degree for given number of inputs!
    if (uDegree >= contourNumber) {
        uDegree = contourNumber-1;
    }

    if (vDegree >= newNumSamplingPts) {
        vDegree = newNumSamplingPts-1;
    }

    // Output spacing function of given input points
    double uSpacing = 1.0 / param.numOutPtsAlongLength;
    double vSpacing = 1.0 / newNumSamplingPts;

    // span types
    const char *uKnotSpanType       = param.uKnotSpanType.c_str();
    const char *vKnotSpanType       = param.vKnotSpanType.c_str();
    const char *uParametricSpanType = param.uParametricSpanType.c_str();
    const char *vParametricSpanType = param.vParametricSpanType.c_str();
    vtkNew(vtkSVNURBSSurface, NURBSSurface);

    if ( sys_geom_loft_solid_with_nurbs(sampledContours, contourNumber,
        uDegree, vDegree, uSpacing, vSpacing, uKnotSpanType, vKnotSpanType, uParametricSpanType,
        vParametricSpanType, NURBSSurface, &dst ) != SV_OK ) {
      throw std::runtime_error("Lofting has failed.");
      outpd = NULL;

    } else {
      if (PlyDtaUtils_CheckLoftSurface(dst->GetVtkPolyData()) != SV_OK) {
        //throw std::runtime_error("Lofting has failed.");
        outpd=NULL;

      } else {
        if(addCaps==1) {
          outpd = sv4guiModelUtils::CreateOrientClosedPolySolidVessel(dst->GetVtkPolyData());
        } else {
          outpd = sv4guiModelUtils::CreateOrientOpenPolySolidVessel(dst->GetVtkPolyData());
        }
      }
    }
  }

  // Clean up
  for (int i=0; i<contourNumber; i++) {
    delete superSampledContours[i];
    delete sampledContours[i];
  }

  delete [] sampledContours;

  if (dst != NULL) delete dst;

  return outpd;
}


//----------------
// CreatePolyData
//----------------
//
static vtkPolyData*
CreatePolyData(std::vector< std::vector<vtkPolyData*> >& contour_groups, int num_sampling_pts, svLoftingParam& param)
{
  int num_groups = contour_groups.size();
  cvPolyData **srcs = new cvPolyData* [num_groups];

  for (int i = 0; i < num_groups; i++) {
    auto& group = contour_groups[i];
    vtkPolyData *vtkpd = CreateLoftSurface(group, num_sampling_pts, param);
    srcs[i] = new cvPolyData(vtkpd);
    vtkpd->Delete();
  }

  int noInterOut = 1;
  double tol = 0.0;
  cvPolyData *dst = NULL;

  int status = sys_geom_all_union(srcs, num_groups, noInterOut, tol, &dst);

  if (status != SV_OK) {
    throw std::runtime_error("Union of vessels has failed. ");
  }
 
  return dst->GetVtkPolyData();
}


//------------------------
// pyCreatePolyDataSolid
//------------------------
//
cvPolyDataSolid* pyCreatePolyDataSolid()
{
  return new cvPolyDataSolid();
}

//---------------
// classify_face
//---------------
// Determine if a model face is a cap.
//
// A face is considered a cap if it is flat.
//
static bool
classify_face(PyModelingModel* self, int faceID, PyUtilApiFunction& api, double tolerance)
{
  // Get the cvPolyData for the face.
  auto model = self->solidModel;
  double max_dist = -1.0;
  int useMaxDist = 0;
  auto cvPolydata = model->GetFacePolyData(faceID, useMaxDist, max_dist);
  if (cvPolydata == nullptr) {
      throw std::runtime_error("Error getting polydata for the solid model face ID '" + std::to_string(faceID) + "'.");
  }
  vtkSmartPointer<vtkPolyData> polydata = vtkSmartPointer<vtkPolyData>::New();
  polydata = cvPolydata->GetVtkPolyData();
  if (polydata == nullptr) {
      throw std::runtime_error("Error getting polydata for the solid model face ID '" + std::to_string(faceID) + "'.");
  }

  // Compute the face center.
  auto points = polydata->GetPoints();
  int numPoints = points->GetNumberOfPoints();
  double cx = 0.0;
  double cy = 0.0;
  double cz = 0.0;
  for (vtkIdType i = 0; i < numPoints; i++) {
      double point[3];
      points->GetPoint(i,point);
      cx += point[0];
      cy += point[1];
      cz += point[2];
  }

  // Compute the variance matrix for the polydata points.
  //
  double com[3] = { cx /= numPoints, cy /= numPoints, cz /= numPoints};
  double csum[3] = { 0.0, 0.0, 0.0};
  for (int i = 0; i < numPoints; i++) {
      double point[3];
      points->GetPoint(i,point);
      csum[0] += point[0] - com[0];
      csum[1] += point[1] - com[1];
      csum[2] += point[2] - com[2];
  }

  double* var[3], v0[3]={ 0.0, 0.0, 0.0}, v1[3]={ 0.0, 0.0, 0.0}, v2[3]={ 0.0, 0.0, 0.0};
  var[0] = v0;
  var[1] = v1;
  var[2] = v2;

  for (int k = 0; k < numPoints; k++) {
      double point[3];
      points->GetPoint(k,point);
      for (int i = 0; i < 3; i++) {
          for (int j = 0; j < 3; j++) {
              var[i][j] += (point[i] - com[i]) * (point[j] - com[j]);
          }
      }
  }

  for (int i = 0; i < 3; i++) {
      for (int j = 0; j < 3; j++) {
          var[i][j] = (var[i][j] - csum[i]*csum[j] / numPoints) / (numPoints-1);
      }
  }

  // Compute the eigenvalues and eigenvectors of the variance matrix.
  double eigvals[3]; 
  double w0[3], w1[3], w2[3];
  double* w[3] = {w0, w1, w2};
  vtkMath::Jacobi(var, eigvals, w);

  // If the smallest eigenvalue is close to zero then the face 
  // is flat and is considered a cap.
  bool isCap = false;
  if (eigvals[2] < tolerance) {
      isCap = true;
  }

  return isCap;
}

//////////////////////////////////////////////////////
//          C l a s s    M e t h o d s              //
//////////////////////////////////////////////////////
// PolyData class methods.

PyDoc_STRVAR(ModelingPolyData_combine_faces_doc,
  "combine_faces(face_id, combine_with)  \n\
   \n\
   Combine a list of faces with a given face in the solid model. \n\
   \n\
   The model face IDs specified in 'combine_with' are replaced with a single 'face_id' face ID. \n\
   \n\
   Args: \n\
     face_id (int): The ID of the face to combine other faces with.      \n\
     combine_with (list[int]): The list of face IDs to combine.          \n\
   \n\
");

static PyObject *
ModelingPolyData_combine_faces(PyModelingModel* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("iO!", PyRunTimeErr, __func__);
  static char *keywords[] = {"face_id", "combine_with", nullptr};
  int combFaceID;
  PyObject* faceListArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &combFaceID, &PyList_Type, &faceListArg)) {
      return api.argsError();
  }

  if (PyList_Size(faceListArg) == 0) {
      api.error("The 'combine_with' list is empty.");
      return nullptr;
  }

  // Get the face IDs.
  auto faceIDs = ModelingModelGetFaceIDs(api, self);
  if (faceIDs.size() == 0) {
      api.error("The model does not have an face IDs defined for it.");
      return nullptr;
  }

  if (faceIDs.count(combFaceID) == 0) {
    api.error("The face ID " + std::to_string(combFaceID) + " is not a valid face ID for the model.");
    return nullptr;
  }

  // Create list of faces to combine.
  std::set<int> combFaceIDs{combFaceID};
  for (int i = 0; i < PyList_Size(faceListArg); i++) {
      auto faceID = PyLong_AsLong(PyList_GetItem(faceListArg,i));
      bool faceFound = false;
      if (faceIDs.count(faceID) == 0) {
          api.error("The face ID " + std::to_string(faceID) + " in 'combine_with' is not a valid face ID for the model.");
          return nullptr;
      }
      combFaceIDs.insert(faceID);
  }

  // Get the model PolyData.
  //
  auto model = self->solidModel;
  double max_dist = -1.0;
  int useMaxDist = 0;
  auto cvPolydata = model->GetPolyData(useMaxDist, max_dist);

  auto polydata = vtkSmartPointer<vtkPolyData>::New();
  polydata->DeepCopy(cvPolydata->GetVtkPolyData());
  if (polydata == nullptr) {
      api.error("Could not get polydata for the solid model.");
      return nullptr;
  }

  // Create a new ModelFaceID data array.
  //
  std::string markerListName = "ModelFaceID";
  auto modelFaceIDs = vtkSmartPointer<vtkIntArray>::New();
  modelFaceIDs = vtkIntArray::SafeDownCast(polydata->GetCellData()->GetScalars(markerListName.c_str()));
  int numCells = polydata->GetNumberOfCells();
  for (vtkIdType cellID = 0; cellID < numCells; cellID++) {
      int faceID = modelFaceIDs->GetValue(cellID); 
      if (combFaceIDs.count(faceID) != 0) { 
          modelFaceIDs->SetValue(cellID,combFaceID);
      }
  }

  // Add the a new ModelFaceID data array to the model PolyData.
  //
  polydata->GetCellData()->RemoveArray(markerListName.c_str());
  modelFaceIDs->SetName(markerListName.c_str());
  polydata->GetCellData()->AddArray(modelFaceIDs);
  polydata->GetCellData()->SetActiveScalars(markerListName.c_str());

  // Set the model PolyData to the one with the new ModelFaceID data array.
  self->solidModel->SetVtkPolyDataObject(polydata);

  Py_RETURN_NONE;
}

PyDoc_STRVAR(ModelingPolyData_compute_boundary_faces_doc,
  "compute_boundary_faces(angle)  \n\
   \n\
   Compute the boundary faces for the solid model. \n\
   \n\
   This method needs to be called when creating new PolyData models or      \n\
   reading in models from files with formats that don't contain face        \n\
   information (e.g. models derived from STL data).                         \n\
   \n\
   Models faces are distinguished using the angle of the normals between    \n\
   adjacent model surface triangles. If the angle is less than or equal to  \n\
   the 'angle' argument then the triangles are considered to part of the    \n\
   same face.                                                               \n\
   \n\
   Args: \n\
     angle (float): The angle used to distinguish faces in a model.         \n\
   \n\
   Returns list([int]): The list of integer face IDs. \n\
");

static PyObject *
ModelingPolyData_compute_boundary_faces(PyModelingModel* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("d", PyRunTimeErr, __func__);
  static char *keywords[] = {"angle", nullptr};
  double angle = 0.0;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &angle)) {
      return api.argsError();
  }

  if (angle < 0.0) {
      api.error("The angle argument < 0.0.");
      return nullptr;
  }

  auto model = self->solidModel;

  // Compute the faces.
  if (model->GetBoundaryFaces(angle) != SV_OK ) {
      api.error("Error computing the boundary faces for the solid model using angle '" + std::to_string(angle) + ".");
      return nullptr;
  }

  // Get the face IDs.
  auto faceIDs = ModelingModelGetFaceIDs(api, self);
  if (faceIDs.size() == 0) {
      return nullptr;
  }

  // Create a list of IDs.
  //
  auto faceList = PyList_New(faceIDs.size());
  int n = 0;
  for (auto id : faceIDs) {
      PyList_SetItem(faceList, n, PyLong_FromLong(id));
      n += 1;
  }

  return faceList;
}

//---------------------
// compute_centerlines
//---------------------
// The following code replicates sv4guiModelUtils::CreateCenterlines().
//
PyDoc_STRVAR(ModelingPolyData_compute_centerlines_doc,
  "compute_centerlines(inlet_ids, outlet_ids, use_face_ids=False)  \n\
   \n\
   Compute the centerlines for the solid model.                     \n\
   \n\
   \n\
   Args: \n\
     inlet_ids (list[int]): The list of integer IDs identifying the vessel \n\
        inlet faces.                                                       \n\
     outlet_ids (list[int]): The list of integer IDs identifying the vessel\n\
        outlet faces. \n\
     use_face_ids (bool): If True then the input IDs are face IDs, else    \n\
        they are VTK point IDs.                                            \n\
   \n\
   Returns (vtkPolyData): The centerlines geometry (lines) and data.       \n\
");

static PyObject *
ModelingPolyData_compute_centerlines(PyModelingModel* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("O!O!|O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"inlet_ids", "outlet_ids", "use_face_ids", NULL};
  PyObject* inletIdsArg;
  PyObject* outletIdsArg;
  PyObject* useFaceIdsArg = nullptr;
  bool useFaceIds = false;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyList_Type, &inletIdsArg,
      &PyList_Type, &outletIdsArg, &PyBool_Type, &useFaceIdsArg)) {
    return api.argsError();
  }

  // Get source IDs.
  //
  int numInletIds = PyList_Size(inletIdsArg);

  if (numInletIds == 0) {
    api.error("The 'inlet_ids' argument is empty.");
    return nullptr;
  }

  std::set<int> sources;
  auto sourceCapIds = vtkSmartPointer<vtkIdList>::New();

  for (int i = 0; i < numInletIds; i++) {
    auto item = PyList_GetItem(inletIdsArg, i);
    if (!PyLong_Check(item)) {
      api.error("The 'inlet_ids' argument is not a list of integers.");
      return nullptr;
    }
    int id = PyLong_AsLong(item);
    //std::cout << "[Vmtk_centerlines]   ID: " << id << std::endl;
    sources.insert(id);
    sourceCapIds->InsertNextId(id);
  }

  // Get the target IDs.
  //
  int numOutletIds = PyList_Size(outletIdsArg);
  std::set<int> targets;
  auto targetCapIds = vtkSmartPointer<vtkIdList>::New();

  if (numOutletIds == 0) {
    api.error("The 'outlet_ids' argument is empty.");
    return nullptr;
  }
  for (int i = 0; i < numOutletIds; i++) {
    auto item = PyList_GetItem(outletIdsArg, i);
    if (!PyLong_Check(item)) {
      api.error("The 'outlet_ids' argument is not a list of integers.");
      return nullptr;
    }
    int id = PyLong_AsLong(item);
    targets.insert(id);
    targetCapIds->InsertNextId(id);
  }

  // Check for IDs given as both sources and targets.
  //
  std::vector<int> commonIds;
  std::set_intersection(sources.begin(), sources.end(), targets.begin(), targets.end(), 
      std::back_inserter(commonIds));
  if (commonIds.size() != 0) {
    std::ostringstream ids;
    std::copy(commonIds.begin(), commonIds.end()-1, std::ostream_iterator<int>(ids, ", "));
    ids << commonIds.back();
    api.error("The 'inlet_ids' and 'outlet_ids' arguments contain identical IDs '" + ids.str() + "'.");
    return nullptr;
  }

  // Get the model PolyData:
  //
  auto model = self->solidModel;
  double max_dist = -1.0;
  int useMaxDist = 0;

  auto cvPolydata = model->GetPolyData(useMaxDist, max_dist);
  auto fullpd = vtkSmartPointer<vtkPolyData>::New();
  fullpd->DeepCopy(cvPolydata->GetVtkPolyData());

  if (fullpd == NULL) {
    api.error("Could not get polydata for the solid model.");
    return nullptr;
  }

  auto inpd = vtkSmartPointer<vtkPolyData>::New();
  inpd->DeepCopy(fullpd);

  // Get the face IDs.
  //
  auto faceIDs = ModelingModelGetFaceIDs(api, self);
  if (faceIDs.size() == 0) {
    api.error("The model has no face IDs.");
    return nullptr;
  }

  // Classify each face as a cap or wall and add the
  // result to a bool list.
  //
  double tolerance = 1e-5;
  std::vector<int> cap_ids;

  for (auto faceID : faceIDs) {
    try {
      if (classify_face(self, faceID, api, tolerance)) {
        cap_ids.push_back(faceID);
        //std::cout << "[centerlines] Cap ID: " << faceID << std::endl;
      }
    } catch (std::exception &e) {
      api.error(e.what());
      return nullptr;
    }
  }

  if (useFaceIdsArg != nullptr) {
    useFaceIds = PyObject_IsTrue(useFaceIdsArg);
  }

  auto sourcePtIds = vtkSmartPointer<vtkIdList>::New();
  auto targetPtIds = vtkSmartPointer<vtkIdList>::New();
  vtkPolyData* centerlines;

  // If using face IDs then we have do all of this,
  // not sure why though.
  //
  if (useFaceIds) { 

    // Remove the cells defining the model caps.
    sv4guiModelUtils::DeleteRegions(inpd, cap_ids);

    cvPolyData *src = new cvPolyData(inpd);
    auto cleaned = sys_geom_Clean(src);
    delete src;

    // Recap the model surface.
    //
    cvPolyData *capped  = NULL;
    int numCapCenterIds;
    int *capCenterIds = NULL;
    int capUsingCenter = 1;     // Cap using a point in the cap center.

    if (sys_geom_cap_for_centerlines(cleaned, &capped, &numCapCenterIds, &capCenterIds, 
        capUsingCenter) != SV_OK) {
      delete cleaned;
      if (capped != NULL) {
        delete capped;
      }
      api.error("Capping for centerlines has failed.");
      return nullptr;
    }

    auto locator = vtkSmartPointer<vtkCellLocator>::New();
    locator->SetDataSet(fullpd);
    locator->BuildLocator();
    auto genericCell = vtkSmartPointer<vtkGenericCell>::New();
    std::map<int,int> facePtIdMap;

    for (int i = 0;  i < numCapCenterIds; i++) {
      int ptId = capCenterIds[i];
      double capPt[3];
      capped->GetVtkPolyData()->GetPoint(ptId, capPt);

      int subId;
      double closestPt[3];
      vtkIdType closestCellId;
      double distance;
      locator->FindClosestPoint(capPt, closestPt, genericCell, closestCellId, subId, distance);

      int capFaceId = fullpd->GetCellData()->GetArray("ModelFaceID")->GetTuple1(closestCellId);
      facePtIdMap[capFaceId] = ptId;
    }

    // Add point IDs to the source and target lists.
    //
    for (auto face : facePtIdMap) {
      int capFaceId = face.first;
      int ptId = face.second;

      if (sourceCapIds->IsId(capFaceId) != -1) {
        sourcePtIds->InsertNextId(ptId);
        //std::cout << "[centerlines] Add source: " << capFaceId << std::endl;
      } else if (targetCapIds->IsId(capFaceId) != -1) {
        targetPtIds->InsertNextId(ptId);
        //std::cout << "[centerlines] Add target: " << capFaceId << std::endl;
      }
    }

    centerlines = sv4guiModelUtils::CreateCenterlines(capped->GetVtkPolyData(), sourcePtIds, targetPtIds);

    delete [] capCenterIds;
    delete capped;

  // Cap IDs are given as node IDs.
  //
  } else {

    auto points = fullpd->GetPoints();

    for (int id : sources) {
      sourcePtIds->InsertNextId(id);
      auto pt = points->GetPoint(id);
      std::cout << "[centerlines] Add source: " << id << std::endl;
      std::cout << "[centerlines]   pt: " << pt[0] << " " << pt[1] << " " << pt[2] << std::endl;
    }

    for (int id : targets) {
      targetPtIds->InsertNextId(id);
      auto pt = points->GetPoint(id);
      std::cout << "[centerlines] Add target: " << id << std::endl;
      std::cout << "[centerlines]   pt: " << pt[0] << " " << pt[1] << " " << pt[2] << std::endl;
    }

    centerlines = sv4guiModelUtils::CreateCenterlines(fullpd, sourcePtIds, targetPtIds);
  }


  return vtkPythonUtil::GetObjectFromPointer(centerlines);
}

//-------------------------------
// ModelingPolyData_delete_faces
//-------------------------------
//
// Delete faces only works for PolyData.
//
PyDoc_STRVAR(ModelingPolyData_delete_faces_doc,
" delete_faces(face_ids)  \n\
  \n\
  Delete faces using a list of face IDs. \n\
  \n\
  Args: \n\
    name (str): Name in the repository to store the unstructured grid. \n\
");

static PyObject *
ModelingPolyData_delete_faces(PyModelingModel* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"face_ids", nullptr};
  PyObject* faceListArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyList_Type, &faceListArg)) {
      return api.argsError();
  }

  if (PyList_Size(faceListArg) == 0) {
      api.error("The 'face_ids' list is empty.");
      return nullptr;
  }

  // Get the face IDs.
  auto faceIDs = ModelingModelGetFaceIDs(api, self);
  if (faceIDs.size() == 0) {
      return nullptr;
  }

  // Create list of faces to delete.
  std::vector<int> faceList;
  for (int i = 0; i < PyList_Size(faceListArg); i++) {
      auto faceID = PyLong_AsLong(PyList_GetItem(faceListArg,i));
      bool faceFound = false;
      if (faceIDs.count(faceID) == 0) {
          api.error("The face ID " + std::to_string(faceID) + " is not a valid face ID for the model.");
          return nullptr;
      }
      faceList.push_back(faceID);
  }

  // [TODO:DaveP] The DeleteFaces() function deletes cells, not faces.
  //
  // Implement this copying sv4guiModelUtils::DeleteRegions().
  /*
  auto model = self->solidModel;
  if (model->DeleteFaces(faceList.size(), faceList.data()) != SV_OK) {
      api.error("Error deleting faces for the solid model.");
  }
  */

  Py_RETURN_NONE;
}

//--------------------------------------
// ModelingPolyData_create_vessel_model
//--------------------------------------
// This method attempts to reproduce the calling sequence from the
// sv4guiModelUtils::CreateModelElementPolyData() method in
// sv4gui/Modules/Model/Common/sv4gui_ModelUtils.cxx used to create
// a solid model from segmentation contours for multiple vessels.
//
PyDoc_STRVAR(ModelingPolyData_create_vessel_model_doc,
" create_vessel_mode(contour_list, num_sampling_pts, loft_options)  \n\
  \n\
  Create a solid model from the segmentation contours for multiple vessels.       \n\
  \n\
  Args: \n\
    contour_list(list(list)): A list of lists of vtkPolyData objects representing \n\
        the segmentation contours for multiple vessels.                           \n\
    num_sampling_pts (int): The number of points used to sample each segmentation \n\
        to the closed curve for lofting.                                          \n\
    loft_options (sv.LoftOptions): The parameters used to loft segmentations.     \n\
  \n\
  Returns vtkPolyData object. \n\
");

static PyObject *
ModelingPolyData_create_vessel_model(PyModelingModel* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("O!iO", PyRunTimeErr, __func__);
  static char *keywords[] = {"contour_list", "num_sampling_pts", "loft_options", NULL};

  PyObject* contourListArg;
  int num_sampling_pts;
  PyObject* loftOptsArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyList_Type, &contourListArg, 
      &num_sampling_pts, &loftOptsArg)) {
    return api.argsError();
  }

  // Get list of vtkPolyData storing contour points.
  //
  std::vector<vtkPolyData*> polyDataList;
  auto contourListArg_size = PyList_Size(contourListArg);

  if (contourListArg_size == 0) {
    api.error("The 'contour_list' argument is empty.");
    return nullptr;
  }

  // Get the vtkPolyData objects representing contour data.
  //
  std::vector< std::vector<vtkPolyData*> > contour_groups;

  for (int i = 0; i < contourListArg_size; i++ ) {
    auto polydata_list = PyList_GetItem(contourListArg, i);
    auto polydata_list_size = PyList_Size(polydata_list);
    std::vector<vtkPolyData*> cont_grp;

    for (int j = 0; j < polydata_list_size; j++ ) {
      auto obj = PyList_GetItem(polydata_list, j);
      auto polydata = (vtkPolyData*)vtkPythonUtil::GetPointerFromObject(obj, "vtkPolyData");
      cont_grp.push_back(polydata);
    }

    contour_groups.push_back(cont_grp);
  }

  // Set lofting parameters.
  //
  svLoftingParam params;
  params.vecFlag = 0;
  PyUtilSetLoftParams(api, loftOptsArg, params);

  // Create a solid model by lofting the contours for each vessel and
  // unioning them.
  vtkPolyData* solidvpd = CreatePolyData(contour_groups, num_sampling_pts, params); 

  cvPolyData *src = new cvPolyData(solidvpd);
  cvPolyData *dst = NULL;

  /* [TODO] Not sure what 'stats' is so forget this for now. 
  if (sys_geom_checksurface(src,stats,tol) !=SV_OK) {
    api.error("Vessel union has failed.");
    return nullptr;
  }
  */

  // Compute cap faces.
  //
  int *doublecaps;
  int numfaces = 0;

  if (sys_geom_set_ids_for_caps(src, &dst,  &doublecaps, &numfaces) != SV_OK) {
    solidvpd->Delete();
    api.error("Vessel union has failed.");
    return nullptr;
  }

  // Clean up the model (again).
  //
  auto forClean = vtkSmartPointer<vtkPolyData>::New();
  forClean->DeepCopy(dst->GetVtkPolyData());
  auto nowClean = vtkSmartPointer<vtkPolyData>::New();
  nowClean = sv4guiModelUtils::OrientVtkPolyData(forClean);
  solidvpd->DeepCopy(nowClean);;

  self->solidModel->SetVtkPolyDataObject(solidvpd);

  Py_RETURN_NONE;
}

//--------------------------------
// ModelingPolyData_identify_caps
//--------------------------------
//
PyDoc_STRVAR(ModelingPolyData_identify_caps_doc,
  "identify_caps()  \n\
   \n\
   Identify which model faces are caps.        \n\
   \n\
   Returns list([bool]): A list of bool values for each face ID, True if it is a cap. \n\
");

static PyObject *
ModelingPolyData_identify_caps(PyModelingModel* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("|d", PyRunTimeErr, __func__);
  static char *keywords[] = {"tolerance", nullptr};
  double tolerance = 1e-5;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &tolerance)) {
      return api.argsError();
  }

  if (tolerance < 0.0) {
      api.error("The tolerance argument < 0.0.");
      return nullptr;
  }

  auto model = self->solidModel;

  // Get the face IDs.
  auto faceIDs = ModelingModelGetFaceIDs(api, self);
  if (faceIDs.size() == 0) {
      api.error("The model has no face information.");
      return nullptr;
  }

  // Classify each face as a cap or wall and add the
  // result to a bool list.
  //
  auto faceList = PyList_New(faceIDs.size());
  int n = 0;
  for (auto faceID: faceIDs) {
      try { 
          bool isCap = classify_face(self, faceID, api, tolerance);
          PyList_SetItem(faceList, n, PyBool_FromLong(isCap));
          n += 1;
      } catch (std::exception &e) {
          api.error(e.what());
          return nullptr;
      }
  }

  return faceList;
}

//------------------------------
// ModelingPolyData_set_surface
//------------------------------
//
// Delete faces only works for PolyData.
//
PyDoc_STRVAR(ModelingPolyData_set_surface_doc,
  "set_surface(surface)  \n\
  \n\
  Set the PolyData surface defining the model. \n\
  \n\
  Args: \n\
    surface (vtkPolyData): The vtkPolyData object representing the surface. \n\
");

static PyObject *
ModelingPolyData_set_surface(PyModelingModel* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("O", PyRunTimeErr, __func__);
  static char *keywords[] = {"surface", nullptr};
  PyObject* surfaceArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &surfaceArg)) {
      return api.argsError();
  }

  auto polydata = PyUtilGetVtkPolyData(api, surfaceArg);
  if (polydata == nullptr) {
      return nullptr;
  }

  self->solidModel->SetVtkPolyDataObject(polydata);

  Py_RETURN_NONE;
}

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* MODELING_POLYDATA_CLASS = "PolyData";
static char* MODELING_POLYDATA_MODULE_CLASS = "modeling.PolyData";

//--------------------------
// PyPolyDataSolidClass_doc
//--------------------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(PyPolyDataSolidClass_doc,
  "The modeling PolyData class is used to represent a PolyData solid       \n\
   model.                                                                  \n\
");

//------------------------
// PyPolyDataSolidMethods
//------------------------
//
PyMethodDef PyPolyDataSolidMethods[] = {

  // [TODO:DaveP] The DeleteFaces() function deletes cells, not faces.
  // { "delete_faces", (PyCFunction)ModelingPolyData_delete_faces, METH_NOARGS|METH_KEYWORDS, ModelingPolyData_delete_faces_doc},

  // [TODO:DaveP] This should be implemented.
  // { "remesh_faces", (PyCFunction)ModelingPolyData_remesh_faces, METH_NOARGS|METH_KEYWORDS, ModelingPolyData_remesh_faces_doc},

  { "combine_faces", (PyCFunction)ModelingPolyData_combine_faces, METH_VARARGS|METH_KEYWORDS, ModelingPolyData_combine_faces_doc},

  { "compute_boundary_faces", (PyCFunction)ModelingPolyData_compute_boundary_faces, METH_VARARGS|METH_KEYWORDS, ModelingPolyData_compute_boundary_faces_doc},

  { "compute_centerlines", (PyCFunction)ModelingPolyData_compute_centerlines, METH_VARARGS|METH_KEYWORDS, ModelingPolyData_compute_centerlines_doc},

  { "create_vessel_model", (PyCFunction)ModelingPolyData_create_vessel_model, METH_VARARGS|METH_KEYWORDS, ModelingPolyData_create_vessel_model_doc},

  { "identify_caps", (PyCFunction)ModelingPolyData_identify_caps, METH_VARARGS|METH_KEYWORDS, ModelingPolyData_identify_caps_doc},

  { "set_surface", (PyCFunction)ModelingPolyData_set_surface, METH_VARARGS|METH_KEYWORDS, ModelingPolyData_set_surface_doc},

  {nullptr, nullptr}
};

//---------------------
// PyPolyDataSolidInit
//---------------------
// This is the __init__() method for the PolyDataSolid class.
//
// This function is used to initialize an object after it is created.
//
// [TODO:DaveP] I'm not sure if it makes sense to have a constructor argument or not.
//
static int
PyPolyDataSolidInit(PyPolyDataSolid* self, PyObject* args, PyObject *kwargs)
{
  auto api = PyUtilApiFunction("|O", PyRunTimeErr, "PolyDataSolid");
  static char *keywords[] = {"surface", nullptr};
  PyObject* surfaceArg = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &surfaceArg)) {
      return -1;
  }

  self->super.solidModel = new cvPolyDataSolid();
  self->super.kernel = SM_KT_POLYDATA;

  if (surfaceArg != nullptr) {
      auto polydata = PyUtilGetVtkPolyData(api, surfaceArg);
      if (polydata == nullptr) {
          return -1;
      }
      //auto cvPolydata = new cvPolyData(polydata);
      self->super.solidModel->SetVtkPolyDataObject(polydata);
  }

  return 0;
}

//--------------------
// PyPolyDataSolidNew
//--------------------
//
static PyObject *
PyPolyDataSolidNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  //std::cout << "[PyPolyDataSolidNew] PyPolyDataSolidNew " << std::endl;
  auto self = (PyPolyDataSolid*)type->tp_alloc(type, 0);
  if (self != nullptr) {
      //self->super.id = 2;
  }
  return (PyObject *) self;
}

//------------------------
// PyPolyDataSolidDealloc
//------------------------
//
static void
PyPolyDataSolidDealloc(PyPolyDataSolid* self)
{
  //std::cout << "[PyPolyDataSolidDealloc] Free PyPolyDataSolid" << std::endl;
  delete self->super.solidModel;
  Py_TYPE(self)->tp_free(self);
}

//--------------------------
// PyPolyDataSolidType
//--------------------------
// Define the Python type object that stores PolyDataSolid data.
//
// Can't set all the fields here because g++ does not suppor non-trivial
// designated initializers.
//
PyTypeObject PyPolyDataSolidType = {
  PyVarObject_HEAD_INIT(nullptr, 0)
  // Dotted name that includes both the module name and
  // the name of the type within the module.
  MODELING_POLYDATA_MODULE_CLASS,
  sizeof(PyPolyDataSolid)
};

//----------------------------
// SetPolyDataSolidTypeFields
//----------------------------
// Set the Python type object fields that stores PolyDataSolid data.
//
// Need to set the fields here because g++ does not suppor non-trivial
// designated initializers.
//
void
SetPolyDataSolidTypeFields(PyTypeObject& solidType)
 {
  // Doc string for this type.
  solidType.tp_doc = PyPolyDataSolidClass_doc;

  // Object creation function, equivalent to the Python __new__() method.
  // The generic handler creates a new instance using the tp_alloc field.
  solidType.tp_new = PyPolyDataSolidNew;
  //.tp_new = PyType_GenericNew,

  // Subclass to PyPolyDataSolid.
  solidType.tp_base = &PyModelingModelType;

  solidType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  solidType.tp_init = (initproc)PyPolyDataSolidInit;
  solidType.tp_dealloc = (destructor)PyPolyDataSolidDealloc;
  solidType.tp_methods = PyPolyDataSolidMethods;
};


