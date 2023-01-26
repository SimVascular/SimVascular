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

static vtkSmartPointer<vtkPolyData> 
OrientVtkPolyData(vtkSmartPointer<vtkPolyData> inpd)
{
  vtkSmartPointer<vtkCleanPolyData> cleaner=vtkSmartPointer<vtkCleanPolyData>::New();
  cleaner->PointMergingOn();
  cleaner->ConvertLinesToPointsOff();
  cleaner->ConvertPolysToLinesOff();
  cleaner->SetInputDataObject(inpd);
  cleaner->Update();

  vtkSmartPointer<vtkPolyDataNormals> orienter=vtkSmartPointer<vtkPolyDataNormals>::New();
  orienter->SetInputDataObject(cleaner->GetOutput());
  orienter->AutoOrientNormalsOn();
  orienter->ComputePointNormalsOff();
  orienter->SplittingOff();
  orienter->ComputeCellNormalsOn();
  orienter->ConsistencyOn();
  orienter->NonManifoldTraversalOff();
  orienter->Update();

  return orienter->GetOutput();
}

static vtkPolyData* 
Orient(vtkPolyData* inpd)
{
  vtkSmartPointer<vtkCleanPolyData> cleaner = vtkSmartPointer<vtkCleanPolyData>::New();
  cleaner->PointMergingOn();
  cleaner->ConvertLinesToPointsOff();
  cleaner->ConvertPolysToLinesOff();
  cleaner->SetInputData(inpd);
  cleaner->Update();

  vtkSmartPointer<vtkPolyDataNormals> orienter = vtkSmartPointer<vtkPolyDataNormals>::New();
  orienter->SetInputData(cleaner->GetOutput());
  orienter->AutoOrientNormalsOn();
  orienter->ComputePointNormalsOff();
  orienter->FlipNormalsOn();
  orienter->SplittingOff();
  orienter->ComputeCellNormalsOn();
  orienter->ConsistencyOn();
  orienter->NonManifoldTraversalOff();
  orienter->Update();

  vtkPolyData* outpd=vtkPolyData::New();
  outpd->DeepCopy(orienter->GetOutput());

  return outpd;
}

static vtkPolyData* 
FillHolesWithIDs(vtkPolyData* inpd, int fillID, int fillType)
{
  cvPolyData* cvpd=new cvPolyData(inpd);
  int numFilled=0;
  cvPolyData* tmpcvpd;

  if (sys_geom_cap_with_ids(cvpd,&tmpcvpd,fillID,numFilled,fillType)!=SV_OK) {
    return NULL;
  }

  if(tmpcvpd==NULL) {
    return NULL;
  }

  vtkPolyData* outpd = Orient(tmpcvpd->GetVtkPolyData());

  delete tmpcvpd;

  return outpd;
}


static vtkPolyData* 
CreateOrientClosedPolySolidVessel(vtkPolyData* inpd)
{
  int fillID=0;
  int fillType=0;

  vtkPolyData* tmppd = FillHolesWithIDs(inpd,fillID,fillType);

  vtkSmartPointer<vtkPolyDataNormals> nrmls = vtkSmartPointer<vtkPolyDataNormals>::New();
  nrmls->SplittingOff();
  nrmls->ConsistencyOn();
  nrmls->AutoOrientNormalsOn();
  nrmls->ComputeCellNormalsOn();
  nrmls->ComputePointNormalsOff();
  nrmls->SetInputData(tmppd);
  nrmls->Update();

  vtkPolyData* outpd=vtkPolyData::New();
  outpd->DeepCopy(nrmls->GetOutput());

  tmppd->Delete();

  return outpd;
}

//------------------------------
// CreateVtkPolyDataFromContour
//------------------------------
//
static vtkSmartPointer<vtkPolyData> 
CreateVtkPolyDataFromContour(vtkPolyData* contour)
{
  vtkSmartPointer<vtkPoints> points = vtkSmartPointer<vtkPoints>::New();
  vtkSmartPointer<vtkCellArray> lines = vtkSmartPointer<vtkCellArray>::New();
  int pointNumber = contour->GetNumberOfPoints();
  //std::cout << "[CreateVtkPolyDataFromContour] pointNumber: " << pointNumber << std::endl;
  auto contour_pts = contour->GetPoints();

  for (int i=0; i<=pointNumber; i++) {
    auto pt = contour_pts->GetPoint(i);
    if(i < pointNumber) {
      //std::array<double, 3> point = GetContourPoint(i);
      points->InsertPoint(i, pt[0], pt[1], pt[2]);
    }

    if(i>0&&i<pointNumber){
      vtkIdType cell[2] = {i-1,i};
      lines->InsertNextCell(2,cell);
    }else if(i==pointNumber){
      vtkIdType cell[2] = {i-1,0};
      lines->InsertNextCell(2,cell);
    }
  }

  vtkSmartPointer<vtkPolyData> polyData = vtkSmartPointer<vtkPolyData>::New();
  polyData->SetPoints(points);
  polyData->SetLines(lines);
  //std::cout << "[CreateVtkPolyDataFromContour] polyData num points: " << polyData->GetNumberOfPoints() << std::endl;
  //std::cout << "[CreateVtkPolyDataFromContour] polyData num cells: " << polyData->GetNumberOfCells() << std::endl;

  return polyData;
}

static vtkPolyData*
CreateLoftSurface(std::vector<vtkPolyData*> contourSet, int numSamplingPts, svLoftingParam& param)
{
  std::cout << "========== [CreateLoftSurface] ========== " << std::endl;
  int contourNumber = contourSet.size();

  param.numOutPtsAlongLength = param.samplePerSegment * contourNumber;
  param.numPtsInLinearSampleAlongLength = param.linearMuliplier * param.numOutPtsAlongLength;
  param.numSuperPts = 0;

  std::cout << "[CreateLoftSurface] contourNumber: " << contourNumber << std::endl;
  std::cout << "[CreateLoftSurface] param.samplePerSegment: " << param.samplePerSegment << std::endl;
  std::cout << "[CreateLoftSurface] param.linearMuliplier: " << param.linearMuliplier << std::endl;
  std::cout << "[CreateLoftSurface] param.numOutPtsAlongLength: " << param.numOutPtsAlongLength << std::endl;

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

  std::vector<cvPolyData*> superSampledContours;

  for (int i = 0; i < contourNumber; i++) {
    vtkPolyData* vtkpd = vtkPolyData::New();
    //vtkpd->DeepCopy(contourSet[i]);
    vtkpd->DeepCopy(CreateVtkPolyDataFromContour(contourSet[i]));
    cvPolyData* cvpd = new cvPolyData(vtkpd);
    //vtkpd->Delete();
    cvPolyData* cvpd2 = sys_geom_sampleLoop(cvpd, param.numSuperPts);

    if (cvpd2 == nullptr) {
      throw std::runtime_error("Supersampling error");
    }
 
    superSampledContours.push_back(cvpd2);
  }

  std::cout << "[CreateLoftSurface] align contours ... " << std::endl;
  std::cout << "[CreateLoftSurface] param.vecFlag: " << param.vecFlag << std::endl;

  std::vector<cvPolyData*> alignedContours;

  for (int i = 0; i < contourNumber; i++) {
    //std::cout << "[CreateLoftSurface] ----- i " << i << " -----" << std::endl;
    cvPolyData* cvpd3;
    auto cont = superSampledContours[i]->GetVtkPolyData();
    //std::cout << "[CreateLoftSurface] cont: " << cont << std::endl;
    //std::cout << "[CreateLoftSurface] cont npts: " << cont->GetNumberOfPoints() << std::endl;

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
    cvPolyData * cvpd4=sys_geom_sampleLoop(alignedContours[i],newNumSamplingPts);

    if(cvpd4==NULL) {
      throw std::runtime_error("Sampling error");
    }

    sampledContours[i]=cvpd4;
  }

  cvPolyData *dst;
  vtkPolyData* outpd = NULL;
  bool addCaps = true;
  std::cout << "[CreateLoftSurface] param.method: " << param.method << std::endl;
  std::cout << "[CreateLoftSurface] addCaps: " << addCaps << std::endl;

  if (param.method=="spline") {
    if ( sys_geom_loft_solid(sampledContours, contourNumber, param.useLinearSampleAlongLength, param.useFFT,
        param.numOutPtsAlongLength, newNumSamplingPts, param.numPtsInLinearSampleAlongLength, param.numModes,
        param.splineType, param.bias, param.tension, param.continuity, &dst ) != SV_OK ) {
      outpd=NULL;
    } else {

      if (addCaps == 1) {
        //outpd = CreateOrientClosedPolySolidVessel(dst->GetVtkPolyData());
      } else {
        //outpd = CreateOrientOpenPolySolidVessel(dst->GetVtkPolyData());
      }
    }

  } else if (param.method == "nurbs") {

    // Degrees of surface
    int uDegree = param.uDegree;
    int vDegree = param.vDegree;

    // Override to maximum possible degree if too large a degree for given number of inputs!
    if (uDegree >= contourNumber)
        uDegree = contourNumber-1;

    if (vDegree >= newNumSamplingPts)
        vDegree = newNumSamplingPts-1;

    std::cout << "[CreateLoftSurface] contourNumber: " << contourNumber << std::endl;
    std::cout << "[CreateLoftSurface] newNumSamplingPts: " << newNumSamplingPts << std::endl;
    std::cout << "[CreateLoftSurface] uDegree: " << uDegree << std::endl;
    std::cout << "[CreateLoftSurface] vDegree: " << vDegree << std::endl;

    // Output spacing function of given input points
    double uSpacing = 1.0 / param.numOutPtsAlongLength;
    double vSpacing = 1.0 / newNumSamplingPts;

    // span types
    const char *uKnotSpanType       = param.uKnotSpanType.c_str();
    const char *vKnotSpanType       = param.vKnotSpanType.c_str();
    const char *uParametricSpanType = param.uParametricSpanType.c_str();
    const char *vParametricSpanType = param.vParametricSpanType.c_str();
    vtkNew(vtkSVNURBSSurface, NURBSSurface);

    std::cout << "[CreateLoftSurface] uKnotSpanType: " << uKnotSpanType << std::endl;
    std::cout << "[CreateLoftSurface] vKnotSpanType: " << vKnotSpanType << std::endl;
    std::cout << "[CreateLoftSurface] uParametricSpanType: " << uParametricSpanType << std::endl;
    std::cout << "[CreateLoftSurface] vParametricSpanType: " << vParametricSpanType << std::endl;
    std::cout << "[CreateLoftSurface] uSpacing: " << uSpacing << std::endl;
    std::cout << "[CreateLoftSurface] vSpacing: " << vSpacing << std::endl;

    if ( sys_geom_loft_solid_with_nurbs(sampledContours, contourNumber,
        uDegree, vDegree, uSpacing, vSpacing, uKnotSpanType, vKnotSpanType, uParametricSpanType,
        vParametricSpanType, NURBSSurface, &dst ) != SV_OK ) {
      //MITK_ERROR << "poly manipulation error ";
      outpd=NULL;

    } else {
      if (PlyDtaUtils_CheckLoftSurface(dst->GetVtkPolyData()) != SV_OK) {
        throw std::runtime_error("Error lofting surface");
        outpd=NULL;

      } else {
        if(addCaps==1) {
          outpd = CreateOrientClosedPolySolidVessel(dst->GetVtkPolyData());
        } else {
          //outpd=CreateOrientOpenPolySolidVessel(dst->GetVtkPolyData());
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
  std::cout << "========== CreatePolyData ==========" << std::endl;

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
  if (cvPolydata == NULL) {
      throw std::runtime_error("Error getting polydata for the solid model face ID '" + std::to_string(faceID) + "'.");
  }
  vtkSmartPointer<vtkPolyData> polydata = vtkSmartPointer<vtkPolyData>::New();
  polydata = cvPolydata->GetVtkPolyData();
  if (polydata == NULL) {
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
  static char *keywords[] = {"face_id", "combine_with", NULL};
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
  if (polydata == NULL) {
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
  static char *keywords[] = {"angle", NULL};
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
  static char *keywords[] = {"face_ids", NULL};
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
//
PyDoc_STRVAR(ModelingPolyData_create_vessel_model_doc,
  "create_vessel_mode()  \n\
   \n\
   Identify which model faces are caps.        \n\
   \n\
   Returns list([bool]): A list of bool values for each face ID, True if it is a cap. \n\
");

static PyObject *
ModelingPolyData_create_vessel_model(PyModelingModel* self, PyObject* args, PyObject* kwargs)
{
  std::cout << "========== create_vessel_model ==========" << std::endl;

  auto api = PyUtilApiFunction("O!O!iO!", PyRunTimeErr, __func__);
  static char *keywords[] = {"contour_list", "contour_names", "num_sampling_pts", "loft_options", NULL};

  PyObject* contourListArg;
  PyObject* contourNamesListArg;
  int num_sampling_pts;
  PyObject* loftOptsArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyList_Type, &contourListArg, 
      &PyList_Type, &contourNamesListArg, &num_sampling_pts, &PyDict_Type, &loftOptsArg)) {
    return api.argsError();
  }

  // Get list of vtkPolyData storing contour points.
  //
  std::vector<vtkPolyData*> polyDataList;
  auto contourListArg_size = PyList_Size(contourListArg);

  if (contourListArg_size == 0) {
    api.error("The points list argument is empty.");
    return nullptr;
  }

  std::cout << "[create_vessel_model] contourListArg_size: " << contourListArg_size << std::endl;

  std::vector< std::vector<vtkPolyData*> > contour_groups;

  for (int i = 0; i < contourListArg_size; i++ ) {
    auto polydata_list = PyList_GetItem(contourListArg, i);
    auto polydata_list_size = PyList_Size(polydata_list);
    std::cout << "[create_vessel_model] polydata_list_size: " << polydata_list_size << std::endl;
    std::vector<vtkPolyData*> cont_grp;

    for (int j = 0; j < polydata_list_size; j++ ) {
      auto obj = PyList_GetItem(polydata_list, j);
      auto polydata = (vtkPolyData*)vtkPythonUtil::GetPointerFromObject(obj, "vtkPolyData");
      //std::cout << "[create_vessel_model] polydata: " << polydata << std::endl;
      //std::cout << "[create_vessel_model] polydata num pts: " << polydata->GetNumberOfPoints() << std::endl;
      cont_grp.push_back(polydata);
    }

    contour_groups.push_back(cont_grp);
  }

  // Get contour names.
  //
  auto contourNamesListArg_size = PyList_Size(contourNamesListArg);

  if (contourNamesListArg_size == 0) {
    api.error("The contour name list argument is empty.");
    return nullptr;
  }

  std::vector<std::string> contourNames;

  for (int i = 0; i < contourNamesListArg_size; i++ ) {
    auto obj = PyList_GetItem(contourNamesListArg, i);
    contourNames.push_back(std::string(PyString_AsString(obj)));
  }

  // Get lofting parameters.
  //
  svLoftingParam param;
  param.vecFlag = 0;

  PyObject *key, *value;
  Py_ssize_t pos = 0;

  while (PyDict_Next(loftOptsArg, &pos, &key, &value)) {
    auto name = std::string(PyString_AsString(key));
    std::cout << "[create_vessel_model] name: " << name << std::endl;

    if (name == "linearMuliplier") {
      param.linearMuliplier = PyLong_AsLong(value);
      std::cout << "[create_vessel_model] value: " << param.linearMuliplier << std::endl;

    } else if (name == "method") {
      param.method = PyString_AsString(value);
      std::cout << "[create_vessel_model] value: " << param.method << std::endl;

    } else if (name == "numOutPtsInSegs") {
      param.numOutPtsInSegs = PyLong_AsLong(value);
      std::cout << "[create_vessel_model] value: " << param.numOutPtsInSegs << std::endl;

    } else if (name == "samplePerSegment") {
      param.samplePerSegment = PyLong_AsLong(value);
      std::cout << "[create_vessel_model] value: " << param.samplePerSegment << std::endl;

    } else if (name == "uKnotSpanType") {
      param.uKnotSpanType = PyString_AsString(value);
      std::cout << "[create_vessel_model] value: " << param.uKnotSpanType << std::endl;

    } else if (name == "vKnotSpanType") {
      param.vKnotSpanType = PyString_AsString(value);
      std::cout << "[create_vessel_model] value: " << param.vKnotSpanType << std::endl;

    } else if (name == "uParametricSpanType") {
      param.uParametricSpanType = PyString_AsString(value);
      std::cout << "[create_vessel_model] value: " << param.uParametricSpanType << std::endl;

    } else if (name == "vParametricSpanType") {
      param.vParametricSpanType = PyString_AsString(value);
      std::cout << "[create_vessel_model] value: " << param.vParametricSpanType << std::endl;

    }

  }

  // Create a solid model by lofting the contours for each vessel and
  // unioning them.
  vtkPolyData* solidvpd = CreatePolyData(contour_groups, num_sampling_pts, param); 

  cvPolyData *src = new cvPolyData(solidvpd);
  cvPolyData *dst = NULL;

  /*
  if (sys_geom_checksurface(src,stats,tol) !=SV_OK) {
    api.error("Vessel union has failed.");
    return nullptr;
  }
  */


  int *doublecaps;
  int numfaces = 0;

  if (sys_geom_set_ids_for_caps(src, &dst,  &doublecaps,&numfaces) != SV_OK) {
    solidvpd->Delete();
    api.error("Vessel union has failed.");
    return nullptr;
  }

  auto forClean = vtkSmartPointer<vtkPolyData>::New();
  forClean->DeepCopy(dst->GetVtkPolyData());
  auto nowClean = vtkSmartPointer<vtkPolyData>::New();
  nowClean = OrientVtkPolyData(forClean);

  solidvpd->DeepCopy(nowClean);;

  int numSeg = contourNames.size();
  int numCap2 = 0;

  for (int i = numSeg-1; i > -1; i--) {   
    if (doublecaps[i] != 0) {   
      numCap2=doublecaps[i];
      break;
      }
   }

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
  static char *keywords[] = {"tolerance", NULL};
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
  static char *keywords[] = {"surface", NULL};
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

  { "create_vessel_model", (PyCFunction)ModelingPolyData_create_vessel_model, METH_VARARGS|METH_KEYWORDS, ModelingPolyData_create_vessel_model_doc},

  { "identify_caps", (PyCFunction)ModelingPolyData_identify_caps, METH_VARARGS|METH_KEYWORDS, ModelingPolyData_identify_caps_doc},

  { "set_surface", (PyCFunction)ModelingPolyData_set_surface, METH_VARARGS|METH_KEYWORDS, ModelingPolyData_set_surface_doc},

  {NULL, NULL}
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
  static char *keywords[] = {"surface", NULL};
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
  if (self != NULL) {
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
  PyVarObject_HEAD_INIT(NULL, 0)
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


