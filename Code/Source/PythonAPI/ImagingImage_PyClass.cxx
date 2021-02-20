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

// The functions defined here implement the SV Python API 'imaging' module 'Image' class.
//
//     image = imaging.Image()
//
// The Python Image class is implemented using the PyImage struct defined in Imaging_PyModule.h.
//

//////////////////////////////////////////////////////
//          U t i l i t y  F u n c t i o n s        //
//////////////////////////////////////////////////////

//----------
// ReadFile
//----------
//
mitk::DataNode::Pointer
ReadFile(const std::string& fileName)
{
  return sv4guiProjectManager::LoadDataNode(fileName);
}

//----------------
// GetPathElement
//----------------
// Get the path PathElement object.
//
/*
static PathElement*
GetPathElement(PyUtilApiFunction& api, PyPath* self)
{
  auto path = self->path;
  if (path == NULL) {
      api.error("The path element data has not be created.");
      return nullptr;
  }
  return path;
}
*/

//////////////////////////////////////////////////////
//          C l a s s   M e t h o d s               //
//////////////////////////////////////////////////////
//
// Python 'Image' class methods.

//----------------
// get_dimensions 
//----------------
//
PyDoc_STRVAR(Image_get_dimensions_doc,
  "get_dimensions() \n\
   \n\
   Get the image dimensions.  \n\
   \n\
");

static PyObject *
Image_get_dimensions(PyImage* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  auto dimentions = self->image_data->GetDimensions();
  return Py_BuildValue("[i, i, i]", dimentions[0], dimentions[1], dimentions[2]);
}

//------------
// get_origin 
//------------
//
PyDoc_STRVAR(Image_get_origin_doc,
  "get_origin() \n\
   \n\
   Get the image origin.  \n\
   \n\
");

static PyObject *
Image_get_origin(PyImage* self, PyObject* args)
{ 
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  auto origin = self->image_data->GetGeometry()->GetOrigin();
  return Py_BuildValue("[d, d, d]", origin[0], origin[1], origin[2]);
}

//-------------------
// Image_get_spacing 
//-------------------
//
PyDoc_STRVAR(Image_get_spacing_doc,
  "get_spacing() \n\
   \n\
   Get the image spacing.  \n\
   \n\
");

static PyObject *
Image_get_spacing(PyImage* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  auto spacing = self->image_data->GetGeometry()->GetSpacing();
  return Py_BuildValue("[d, d, d]", spacing[0], spacing[1], spacing[2]);
}

//-----------------
// Image_transform 
//-----------------
//
PyDoc_STRVAR(Image_transform_doc,
  "transform(matrix) \n\
   \n\
   Transform the image using a 4x4 transformation matrix. \n\
   \n\
   \n\
   Args:                                    \n\
     matrix (vtkMatrix4x4): The 4x4 transformation matrix. \n\
");

static PyObject *
Image_transform(PyImage* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("O", PyRunTimeErr, __func__);
  static char *keywords[] = {"matrix", NULL};
  PyObject* matrixArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &matrixArg)) {
    return api.argsError();
  }

  // Get the vtkMatrix4x4 object.
  auto matrix = (vtkMatrix4x4*)vtkPythonUtil::GetPointerFromObject(matrixArg, "vtkMatrix4x4");
  if (matrix == nullptr) { 
      api.error("The 'matrix' argument is not a vtkMatrix4x4 object.");
      return nullptr;
  }
  std::cout << "[Image_transform] ---------- matrix ----------" << std::endl;
  vtkIndent indent;
  matrix->PrintSelf(std::cout, indent);
  std::cout << "[Image_transform] ----------------------------" << std::endl;

  auto transform = self->image_data->GetGeometry()->GetVtkMatrix();
  std::cout << "[Image_transform] ---------- transform ----------" << std::endl;
  transform->PrintSelf(std::cout, indent);
  std::cout << "[Image_transform] ----------------------------" << std::endl;

  // Transform the image.
  self->image_data->GetGeometry()->SetIndexToWorldTransformByVtkMatrix(matrix);
  self->image_data->UpdateOutputInformation();

  Py_RETURN_NONE;
}

////////////////////////////////////////////////////////
//           C l a s s   D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* IMAGE_CLASS = "Image";
// Dotted name that includes both the module name and
// the name of the type within the module.
static char* IMAGING_MODULE_CLASS = "imaging.Image";

//----------------
// ImageClass_doc
//----------------
// Define the Image class documentation.
//
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(ImageClass_doc,
   "The Image class provides methods for                                       \n\
   \n\
");

//----------------
// PyImageMethods
//----------------
// Image class methods.
//
static PyMethodDef PyImageMethods[] = {

  {"get_dimensions", (PyCFunction)Image_get_dimensions, METH_NOARGS, Image_get_dimensions_doc },
  {"get_origin", (PyCFunction)Image_get_origin, METH_NOARGS, Image_get_origin_doc},
  {"get_spacing", (PyCFunction)Image_get_spacing, METH_NOARGS, Image_get_spacing_doc },
  {"transform", (PyCFunction)Image_transform, METH_VARARGS|METH_KEYWORDS, Image_transform_doc },

  {NULL,NULL}
};

//-------------
// PyImageType
//-------------
// Define the Python type object that stores Image data.
//
// Can't set all the fields here because g++ does not suppor non-trivial
// designated initializers.
//
PyTypeObject PyImageType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  // Dotted name that includes both the module name and
  // the name of the type within the module.
  IMAGING_MODULE_CLASS,
  sizeof(PyImage)
};

//-------------
// PyImageInit
//-------------
// This is the __init__() method for the Image class.
//
// This function is used to initialize an object after it is created.
//
static int
PyImageInit(PyImage* self, PyObject* args, PyObject *kwds)
{
  static int numObjs = 1;
  std::cout << "[PyImageInit] New Path object: " << numObjs << std::endl;
  //self->path = new PathElement();
  self->id = numObjs;
  numObjs += 1;
  return 0;
}

//------------
// PyImageNew
//------------
// Object creation function, equivalent to the Python __new__() method.
// The generic handler creates a new instance using the tp_alloc field.
//
static PyObject *
PyImageNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, "imaging.Image");
  char* fileNameArg = nullptr; 
  if (!PyArg_ParseTuple(args, api.format, &fileNameArg)) {
      return api.argsError();
  }

  std::cout << "[PyImageNew] PyImageNew " << std::endl;
  auto self = (PyImage*)type->tp_alloc(type, 0);
  if (self != NULL) {
      self->id = 1;
  }

  if (fileNameArg != nullptr) {
      std::cout << "[PyImageNew] fileNameArg: " << fileNameArg << std::endl;
      self->image_node = ReadFile(std::string(fileNameArg));
      self->image_data  = dynamic_cast<mitk::Image*>(self->image_node->GetData());
  }

  return (PyObject*)self;
}

//----------------
// PyImageDealloc
//----------------
//
static void
PyImageDealloc(PyImage* self)
{
  //std::cout << "[PyImageDealloc] Free PyImage" << std::endl;
  //delete self->path;
  Py_TYPE(self)->tp_free(self);
}

//--------------------
// SetImageTypeFields
//--------------------
// Set the Python type object fields that stores Image data.
//
// Need to set the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static void
SetPyImageTypeFields(PyTypeObject& imageType)
{
  // Doc string for this type.
  imageType.tp_doc = ImageClass_doc;
  // Object creation function, equivalent to the Python __new__() method.
  // The generic handler creates a new instance using the tp_alloc field.
  imageType.tp_new = PyImageNew;
  imageType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  imageType.tp_init = (initproc)PyImageInit;
  imageType.tp_dealloc = (destructor)PyImageDealloc;
  imageType.tp_methods = PyImageMethods;
}

//---------------
// CreatePyImage
//---------------
// Create a PyImage object.
//
PyObject *
CreatePyImage()
//CreatePyImage(PathElement* path)
{
  //std::cout << "[CreatePyImage] Create Path object ... " << std::endl;
  auto imageObj = PyObject_CallObject((PyObject*)&PyImageType, NULL);
  auto pyImage = (PyImage*)imageObj;

  /*
  if (path != nullptr) {
      delete pyPath->path;
      pyPath->path = path;
  }
  */
  //std::cout << "[CreatePyImage] pyPath id: " << pyPath->id << std::endl;
  //std::cout << "[CreatePyImage] pathObj ref count: " << Py_REFCNT(pathObj) << std::endl;
  return imageObj;
}

