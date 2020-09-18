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

// The functions defined here implement the SV Python API Open Cascade solid class.
//
// The class name is 'modeling.OpenCascade'.

#include <XCAFApp_Application.hxx>
#include <XCAFDoc_DocumentTool.hxx>
#include "Standard_Version.hxx"
#include "sv4gui_RegisterOCCTFunction.h"

//-------------
// PyOcctSolid
//-------------
// Define the OcctSolid class (type).
//
typedef struct {
  PyModelingModel super;
} PyOcctSolid;

cvOCCTSolidModel * pyCreateOcctSolid()
{
  return new cvOCCTSolidModel();
}

//////////////////////////////////////////////////////
//          C l a s s    M e t h o d s              //
//////////////////////////////////////////////////////
// OcctSolid class methods.

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* MODELING_OCCT_CLASS = "OpenCascade";
static char* MODELING_OCCT_MODULE_CLASS = "modeling.OpenCascade";

//----------------------
// PyOcctSolidClass_doc
//----------------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(PyOcctSolidClass_doc,
  "The modeling OpenCascade class is used to represent an Open Cascade    \n\
   solid model.                                                           \n\
");

PyMethodDef PyOcctSolidMethods[] = {
  {NULL, NULL}
};

//-----------------
// PyOcctSolidInit
//-----------------
// This is the __init__() method for the OcctSolid class.
//
// This function is used to initialize an object after it is created.
//
static int
PyOcctSolidInit(PyOcctSolid* self, PyObject* args, PyObject *kwds)
{
  static int numObjs = 1;
  //std::cout << "[PyOcctSolidInit] New OcctSolid object: " << numObjs << std::endl;
  self->super.solidModel = new cvOCCTSolidModel();
  numObjs += 1;
  return 0;
}

//----------------
// PyOcctSolidNew
//----------------
//
static PyObject *
PyOcctSolidNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  //std::cout << "[PyOcctSolidNew] PyOcctSolidNew " << std::endl;
  auto self = (PyOcctSolid*)type->tp_alloc(type, 0);
  if (self != NULL) {
      //self->super.id = 2;
  }
  return (PyObject *) self;
}

//--------------------
// PyOcctSolidDealloc
//--------------------
//
static void
PyOcctSolidDealloc(PyOcctSolid* self)
{
  //std::cout << "[PyOcctSolidDealloc] Free PyOcctSolid" << std::endl;
  delete self->super.solidModel;
  Py_TYPE(self)->tp_free(self);
}

//----------------------
// PyOcctSolidType
//----------------------
// Define the Python type object that stores OcctSolid data.
//
// Can't set all the fields here because g++ does not suppor non-trivial
// designated initializers.
//
PyTypeObject PyOcctSolidType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  // Dotted name that includes both the module name and
  // the name of the type within the module.
  MODELING_OCCT_MODULE_CLASS,
  sizeof(PyOcctSolid)
};

//------------------------
// SetOcctSolidTypeFields
//------------------------
// Set the Python type object fields that stores OcctSolid data.
//
// Need to set the fields here because g++ does not suppor non-trivial
// designated initializers.
//
void
SetOcctSolidTypeFields(PyTypeObject& solidType)
 {
  // Doc string for this type.
  solidType.tp_doc = PyOcctSolidClass_doc;

  // Object creation function, equivalent to the Python __new__() method.
  // The generic handler creates a new instance using the tp_alloc field.
  solidType.tp_new = PyOcctSolidNew;
  //.tp_new = PyType_GenericNew,

  // Subclass to PyOcctSolid.
  solidType.tp_base = &PyModelingModelType;

  solidType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  solidType.tp_init = (initproc)PyOcctSolidInit;
  solidType.tp_dealloc = (destructor)PyOcctSolidDealloc;
  solidType.tp_methods = PyOcctSolidMethods;
};

//----------
// InitOcct
//----------
// Initialize Open Cascade.
//
void
InitOcct()
{
  Handle(XCAFApp_Application) OCCTManager = static_cast<XCAFApp_Application*>(gOCCTManager);
  OCCTManager = XCAFApp_Application::GetApplication();
  Handle(TDocStd_Document) doc;
  OCCTManager->NewDocument("MDTV-XCAF", doc);

  if (!XCAFDoc_DocumentTool::IsXCAFDocument(doc)) {
    fprintf(stdout,"OCCT XDE is not setup correctly, file i/o and register of solid will not work correctly.\n");
  }
  //printf("%-12s %s\n","[InitOcct] Python API OpenCASCADE version:", OCC_VERSION_COMPLETE);

  // Register the file extensions: brep, step, iges and stl.
  auto registerFunction = new sv4guiRegisterOCCTFunction();
}


