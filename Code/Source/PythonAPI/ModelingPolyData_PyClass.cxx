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

//-----------------
// PyPolyDataSolid 
//-----------------
// Define the PolyDataSolid class (type).
//
typedef struct {
  PyModelingModel super;
} PyPolyDataSolid;

cvPolyDataSolid* pyCreatePolyDataSolid()
{
  return new cvPolyDataSolid();
}

//////////////////////////////////////////////////////
//          C l a s s    M e t h o d s              //
//////////////////////////////////////////////////////
// PolyData class methods. 

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

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* MODELING_POLYDATA_CLASS = "PolyData";
static char* MODELING_POLYDATA_MODULE_CLASS = "modeling.PolyData";

PyDoc_STRVAR(PyPolyDataSolidClass_doc, 
  "SimVascular PolyData class. \n\
   \n\
   The PolyData class provides an interface to SV PolyData modeler          \n\
   functionality. \n\
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

  {NULL, NULL}
};

//---------------------
// PyPolyDataSolidInit 
//---------------------
// This is the __init__() method for the PolyDataSolid class. 
//
// This function is used to initialize an object after it is created.
//
static int
PyPolyDataSolidInit(PyPolyDataSolid* self, PyObject* args, PyObject *kwds)
{ 
  static int numObjs = 1;
  std::cout << "[PyPolyDataSolidInit] New PolyDataSolid object: " << numObjs << std::endl;
  self->super.solidModel = new cvPolyDataSolid();
  numObjs += 1;
  return 0;
}

//--------------------
// PyPolyDataSolidNew 
//--------------------
//
static PyObject *
PyPolyDataSolidNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  std::cout << "[PyPolyDataSolidNew] PyPolyDataSolidNew " << std::endl;
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
  std::cout << "[PyPolyDataSolidDealloc] Free PyPolyDataSolid" << std::endl;
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
  .tp_name = MODELING_POLYDATA_MODULE_CLASS,
  .tp_basicsize = sizeof(PyPolyDataSolid)
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


