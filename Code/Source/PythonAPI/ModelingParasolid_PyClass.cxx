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

// The functions defined here implement the SV Python API Parasolid solid class.
//
// The class name is 'modeling.Parasolid'.

//------------------
// PyParasolidSolid
//------------------
// Define the ParasolidSolid class (type).
//
typedef struct {
  PyModelingModel super;
} PyParasolidSolid;

CreateSolidModelObjectFunction PyCreateParasolidSolidObject = nullptr;

//////////////////////////////////////////////////////
//          C l a s s    M e t h o d s              //
//////////////////////////////////////////////////////
// ParasolidSolid class methods.

//----------------------
// ParasolidSolid_write
//----------------------
//
PyDoc_STRVAR(ParasolidSolid_write_doc,
" write(file_name)  \n\
  \n\
  Write the solid model to a file in the Parasolid .xmt_txt format. \n\
  \n\
  The xmt_txt extension does not need to be give in the file name.  \n\
  \n\
  Example: Write a model to the file named 'model.xmt_txt'.         \n\
  \n\
           model.write('model')                                     \n\
  \n\
  Args: \n\
    file_name (str): Name of the file to write the model to. \n\
");

static PyObject *
ParasolidSolid_write(PyModelingModel* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("s|i", PyRunTimeErr, __func__);
  static char *keywords[] = {"file_name", "version", NULL};
  char* fileName;
  int fileVersion = 0;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &fileName, &fileVersion)) {
      return api.argsError();
  }

  auto model = self->solidModel;
  if (model->WriteNative(fileVersion, fileName) != SV_OK) {
      api.error("Error writing the solid model to the file '" + std::string(fileName) +
        "' using version '" + std::to_string(fileVersion)+"'.");
      return nullptr;
  }

  Py_RETURN_NONE;
}

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* MODELING_PARAMODELING_CLASS = "Parasolid";
static char* MODELING_PARAMODELING_MODULE_CLASS = "modeling.Parasolid";

//---------------------------
// PyParasolidSolidClass_doc
//---------------------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(PyParasolidSolidClass_doc,
  "The modeling Parasolid class is used to represent a Parasolid solid     \n\
   model.                                                                  \n\
");

//-------------------------
// PyParasolidSolidMethods
//-------------------------
//
PyMethodDef PyParasolidSolidMethods[] = {
  {"write", (PyCFunction)ParasolidSolid_write, METH_VARARGS|METH_KEYWORDS, NULL},
  {NULL, NULL}
};

//----------------------
// PyParasolidSolidInit
//----------------------
// This function is used to initialize an object after it is created.
//
// This implements the Python __init__ method for the Contour class.
// It is called after calling the __new__ method.
//
static int
PyParasolidSolidInit(PyParasolidSolid* self, PyObject* args, PyObject *kwds)
{
  static int numObjs = 1;
  //std::cout << "[PyParasolidSolidInit] New ParasolidSolid object: " << numObjs << std::endl;
  //self->super.solidModel = new cvPARAMODELINGSolidModel();
  numObjs += 1;
  return 0;
}

//---------------------
// PyParasolidSolidNew
//---------------------
// Create a new instance of a ParasolidSolid object.
//
// This implements the Python __new__ method. It is called before the
// __init__ method.
//
static PyObject *
PyParasolidSolidNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  //std::cout << "[PyParasolidSolidNew] PyParasolidSolidNew " << std::endl;
  auto self = (PyParasolidSolid*)type->tp_alloc(type, 0);
  if (self != NULL) {
      //self->super.id = 2;
  }
  return (PyObject *) self;
}

//-------------------------
// PyParasolidSolidDealloc
//-------------------------
//
static void
PyParasolidSolidDealloc(PyParasolidSolid* self)
{
  //std::cout << "[PyParasolidSolidDealloc] Free PyParasolidSolid" << std::endl;
  delete self->super.solidModel;
  Py_TYPE(self)->tp_free(self);
}

//---------------------------
// PyParasolidSolidType
//---------------------------
// Define the Python type object that stores ParasolidSolid data.
//
// Can't set all the fields here because g++ does not suppor non-trivial
// designated initializers.
//
PyTypeObject PyParasolidSolidType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  // Dotted name that includes both the module name and
  // the name of the type within the module.
  MODELING_PARAMODELING_MODULE_CLASS,
  sizeof(PyParasolidSolid)
};

//----------------------------
// SetParasolidSolidTypeFields
//----------------------------
// Set the Python type object fields that stores ParasolidSolid data.
//
// Need to set the fields here because g++ does not suppor non-trivial
// designated initializers.
//
void
SetParasolidSolidTypeFields(PyTypeObject& solidType)
 {
  // Doc string for this type.
  solidType.tp_doc = PyParasolidSolidClass_doc;

  // Object creation function, equivalent to the Python __new__() method.
  // The generic handler creates a new instance using the tp_alloc field.
  solidType.tp_new = PyParasolidSolidNew;
  //.tp_new = PyType_GenericNew,

  // Subclass to PyParasolidSolid.
  solidType.tp_base = &PyModelingModelType;

  solidType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  solidType.tp_init = (initproc)PyParasolidSolidInit;
  solidType.tp_dealloc = (destructor)PyParasolidSolidDealloc;
  solidType.tp_methods = PyParasolidSolidMethods;
};

//---------------------
// PyAPI_InitParasolid
//---------------------
// Setup creating Parasolid modeling objects.
//
// This is called from the Parasolid plugin Python API code.
//
void
PyAPI_InitParasolid(CreateSolidModelObjectFunction create_object)
{
  // Set the function to create Parasolid modeling objects.
  PyCreateParasolidSolidObject = create_object;

  // Add a method to create a Parasolid modeling object.
  CvSolidModelCtorMap[SolidModel_KernelT::SM_KT_PARASOLID] = []()-> cvSolidModel*{ return PyCreateParasolidSolidObject(); };
}


