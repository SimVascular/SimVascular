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

// Define the Python 'modeling.Model' class used as a base class for
// the PolyData, OpenCascade and Parasolid classes.
//
// This class is not exposed.

extern bool ModelingCheckFileFormat(PyUtilApiFunction& api, SolidModel_KernelT kernel, std::string fileName);

//////////////////////////////////////////////////////
//        U t i l i t y     F u n c t i o n s       //
//////////////////////////////////////////////////////

//-------------------------
// ModelingModelGetFaceIDs
//-------------------------
// Get the model face IDs.
//
std::set<int>
ModelingModelGetFaceIDs(PyUtilApiFunction& api, PyModelingModel* self)
{
  std::set<int> faceIDs;
  auto model = self->solidModel;

  int numFaces;
  int *faces;

  if (model->GetFaceIds(&numFaces, &faces) != SV_OK) {
      api.error("The model has no face IDs.");
      return faceIDs;
  }

  if (numFaces == 0) {
      api.error("The model has no face IDs.");
      return faceIDs;
  }

  for (int i = 0; i < numFaces; i++) {
      faceIDs.insert(faces[i]);
  }
  delete faces;

  return faceIDs;
}

/////////////////////////////////////////////////////////////////
//              C l a s s   M e t h o d s                      //
/////////////////////////////////////////////////////////////////
//
// Python API functions for the Python solid.Model class.

//----------------------------
// ModelingModel_get_face_ids
//----------------------------
//
PyDoc_STRVAR(ModelingModel_get_face_ids_doc,
  "get_face_ids()  \n\
   \n\
   Get the model face IDs. \n\
   \n\
   Face IDs identify the boundary faces representing the solid model. They  \n\
   are used to identify a face for certain operations (e.g. get_face_polydata). \n\
   \n\
   Returns list([int]): The list of integer face IDs. \n\
");

static PyObject *
ModelingModel_get_face_ids(PyModelingModel* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);

  auto model = self->solidModel;

  // Get the face IDs.
  auto faceIDs = ModelingModelGetFaceIDs(api, self);
  if (faceIDs.size() == 0) {
      return nullptr;
  }

  // Create Python list of ints.
  auto faceList = PyList_New(faceIDs.size());
  int n = 0;
  for (auto id : faceIDs) {
      PyList_SetItem(faceList, n, PyLong_FromLong(id));
      n += 1;
  }

  return faceList;
}

//---------------------------------
// ModelingModel_get_face_polydata
//---------------------------------
//
PyDoc_STRVAR(ModelingModel_get_face_polydata_doc,
   "get_face_polydata(face_id)  \n\
   \n\
   Get the polydata geometry for a face.  \n\
   \n\
   Args: \n\
     face_id (int): The face ID. \n\
   \n\
   Returns (vtkPolyData object): The vtkPolyData object containing the face geometry.  \n\
");

static PyObject *
ModelingModel_get_face_polydata(PyModelingModel* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("i|d", PyRunTimeErr, __func__);
  static char *keywords[] = {"face_id", "max_dist", NULL};
  int faceID;
  double max_dist = -1.0;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &faceID, &max_dist)) {
      return api.argsError();
  }

  // Check the face ID argument.
  //
  if (faceID <= 0) {
      api.error("The face ID argument <= 0.");
      return nullptr;
  }

  // Get the face IDs.
  auto faceIDs = ModelingModelGetFaceIDs(api, self);
  if (faceIDs.size() == 0) {
      return nullptr;
  }

  if (faceIDs.count(faceID) == 0) {
      api.error("The face ID argument is not a valid face ID for the model.");
      return nullptr;
  }

  int useMaxDist = 0;
  if (max_dist > 0) {
      useMaxDist = 1;
  }

  // Get the cvPolyData:
  //
  auto model = self->solidModel;
  auto cvPolydata = model->GetFacePolyData(faceID, useMaxDist, max_dist);
  if (cvPolydata == NULL) {
      api.error("Error getting polydata for the solid model face ID '" + std::to_string(faceID) + "'.");
      return nullptr;
  }
  vtkSmartPointer<vtkPolyData> polydata = vtkSmartPointer<vtkPolyData>::New();
  polydata = cvPolydata->GetVtkPolyData();
  if (polydata == NULL) {
      api.error("Error getting polydata for the solid model face ID '" + std::to_string(faceID) + "'.");
      return nullptr;
  }

  return PyUtilGetVtkObject(api, polydata);
}

//----------------------------
// ModelingModel_get_polydata
//----------------------------
//
PyDoc_STRVAR(ModelingModel_get_polydata_doc,
   "get_polydata()  \n\
   \n\
   Get the polydata geometry for the model.  \n\
   \n\
   Returns (vtkPolyData object): The vtkPolyData object containing the model geometry.  \n\
");

static PyObject *
ModelingModel_get_polydata(PyModelingModel *self, PyObject* args)
{
  auto api = PyUtilApiFunction("|d", PyRunTimeErr, __func__);
  double max_dist = -1.0;

  if (!PyArg_ParseTuple(args, api.format, &max_dist)) {
      return api.argsError();
  }

  auto model = self->solidModel;

  int useMaxDist = 0;
  if (max_dist > 0) {
      useMaxDist = 1;
  }

  // Get the cvPolyData:
  //
  auto cvPolydata = model->GetPolyData(useMaxDist, max_dist);
  vtkSmartPointer<vtkPolyData> polydata = vtkSmartPointer<vtkPolyData>::New();
  polydata->DeepCopy(cvPolydata->GetVtkPolyData());
  if (polydata == NULL) {
      api.error("Could not get polydata for the solid model.");
      return nullptr;
  }

  return PyUtilGetVtkObject(api, polydata);
}

//---------------------
// ModelingModel_write
//---------------------
//
PyDoc_STRVAR(ModelingModel_write_doc,
   "write(file_name, format)  \n\
   \n\
   Write the solid model to a file in its native format. \n\
   \n\
   The native formats supported for each modeling kernel are: \n\
   \n\
         OpenCascade: brep \n\
   \n\
         Parasolid: xmt_txt \n\
   \n\
         PolyData: ply, stl, vtk and vtp  \n\
   \n\
   Args: \n\
     file_name (str): The name in the file to write the model to. \n\
     format (str): The native format to write the model to. \n\
");

static PyObject *
ModelingModel_write(PyModelingModel* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("ss|i", PyRunTimeErr, __func__);
  static char *keywords[] = {"file_name", "format", "version", NULL};
  char* fileName;
  char* fileFormat;
  int fileVersion = 0;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &fileName, &fileFormat, &fileVersion)) {
      return api.argsError();
  }

  // Check the format.
  std::string fullFileName = std::string(fileName);
  auto extension = fullFileName.substr(fullFileName.find_last_of(".") + 1);
  if (extension != fullFileName) {
      api.error("The file name argument has a file extension '" + extension + "'.");
      return nullptr;
  }

  // Add format as file extension.
  fullFileName += "." + std::string(fileFormat);
  std::vector<char> cstr(fullFileName.c_str(), fullFileName.c_str() + fullFileName.size() + 1);

  // Check that the file extension is valid for the modeler kernel.
  if (!ModelingCheckFileFormat(api, self->kernel, std::string(fullFileName))) {
      return nullptr;
  }

  auto model = self->solidModel;
  if (model->WriteNative(fileVersion, cstr.data()) != SV_OK) {
      api.error("Error writing the solid model to the file '" + std::string(fileName) +
        "' using version '" + std::to_string(fileVersion)+"'.");
      return nullptr;
  }

  Py_RETURN_NONE;
}

////////////////////////////////////////////////////////
//           C l a s s    D e f i n i t i o n         //
////////////////////////////////////////////////////////

static char* MODELING_MODEL_CLASS = "Model";

// Dotted name that includes both the module name and
// the name of the type within the module.
static char* MODELING_MODEL_MODULE_CLASS = "modeling.Model";

//------------------------
// ModelingModelClass_doc
//------------------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(ModelingModelClass_doc,
   "Model() \n\
   \n\
   ----------------------------------------------------------------------  \n\
   The Model class is use to represent 3D solid models of each solid       \n\
   modeling kernel.                                                        \n\
   \n\
   Args: \n\
     kernel (str): The solid modeling kernel name from the Kernel class.   \n\
   \n\
");

//------------------------
// PyModelingModelMethods
//------------------------
// Define method names for ModelingModel class
//
static PyMethodDef PyModelingModelMethods[] = {

  { "get_face_ids", (PyCFunction)ModelingModel_get_face_ids, METH_NOARGS, ModelingModel_get_face_ids_doc },

  { "get_face_polydata", (PyCFunction)ModelingModel_get_face_polydata, METH_VARARGS|METH_KEYWORDS, ModelingModel_get_face_polydata_doc},

  { "get_polydata", (PyCFunction)ModelingModel_get_polydata, METH_VARARGS, ModelingModel_get_polydata_doc },

  { "write", (PyCFunction)ModelingModel_write, METH_VARARGS|METH_KEYWORDS, ModelingModel_write_doc },

  {NULL,NULL}

};

//---------------------
// PyModelingModelInit
//---------------------
// This is the __init__() method for the ModelingModel class.
//
// This function is used to initialize an object after it is created.
//
static int
PyModelingModelInit(PyModelingModel* self, PyObject* args, PyObject *kwds)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, "ModelingModel");
  static int numObjs = 1;
  char* kernelName = nullptr;
  if (!PyArg_ParseTuple(args, "s", &kernelName)) {
      return -1;
  }
  auto kernel = kernelNameEnumMap.at(std::string(kernelName));
  cvSolidModel* solidModel;

  try {
      solidModel = CvSolidModelCtorMap[kernel]();
  } catch (const std::bad_function_call& except) {
      api.error("The '" + std::string(kernelName) + "' kernel is not supported.");
      return -1;
  }

  self->id = numObjs;
  self->kernel = kernel;
  self->solidModel = solidModel;
  numObjs += 1;
  return 0;
}

//---------------------
// PyModelingModelType
//---------------------
// This is the definition of the ModelingModel class.
//
// The type object stores a large number of values, mostly C function pointers,
// each of which implements a small part of the type’s functionality.
//
PyTypeObject PyModelingModelType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  MODELING_MODEL_MODULE_CLASS,
  sizeof(PyModelingModel)
};

//--------------------
// PyModelingModelNew
//--------------------
//
static PyObject *
PyModelingModelNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  //std::cout << "[PyModelingModelNew] New ModelingModel" << std::endl;
  auto api = PyUtilApiFunction("s", PyRunTimeErr, "ModelingModel");
  char* kernelName = nullptr;
  if (!PyArg_ParseTuple(args, api.format, &kernelName)) {
      return api.argsError();
  }

  SolidModel_KernelT kernel;

  try {
      kernel = kernelNameEnumMap.at(std::string(kernelName));
  } catch (const std::out_of_range& except) {
      auto msg = "Unknown kernel name '" + std::string(kernelName) + "'." +
          " Valid names are: " + kernelValidNames + ".";
      api.error(msg);
      return nullptr;
  }

  auto self = (PyModelingModel*)type->tp_alloc(type, 0);
  if (self != NULL) {
      //self->id = 1;
  }

  return (PyObject *) self;
}

//------------------------
// PyModelingModelDealloc
//------------------------
//
static void
PyModelingModelDealloc(PyModelingModel* self)
{
  //std::cout << "[PyModelingModelDealloc] Free PyModelingModel: " << self->id << std::endl;
  // [TODO:DaveP] what to do here?
  //delete self->solidModel;
  Py_TYPE(self)->tp_free(self);
}

//----------------------------
// SetModelingModelTypeFields
//----------------------------
// Set the Python type object fields that stores ModelingModel data.
//
// Need to set the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static void
SetModelingModelTypeFields(PyTypeObject& solidModelType)
{
  // Doc string for this type.
  solidModelType.tp_doc = ModelingModelClass_doc;
  // Object creation function, equivalent to the Python __new__() method.
  // The generic handler creates a new instance using the tp_alloc field.
  solidModelType.tp_new = PyModelingModelNew;
  //solidModelType.tp_new = PyType_GenericNew,
  solidModelType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  solidModelType.tp_init = (initproc)PyModelingModelInit;
  solidModelType.tp_dealloc = (destructor)PyModelingModelDealloc;
  solidModelType.tp_methods = PyModelingModelMethods;
};

//-------------------------
// CreateModelingModelType
//-------------------------
static PyModelingModel *
CreateModelingModelType()
{
  return PyObject_New(PyModelingModel, &PyModelingModelType);
}

