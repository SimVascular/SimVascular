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

#include <vtkMath.h>

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


