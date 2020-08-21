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

// This is currently not used.

// The functions defined here implement the SV Python API 'Project' class.
//

//////////////////////////////////////////////////////
//          C l a s s   M e t h o d s               //
//////////////////////////////////////////////////////
//
// Python 'Project' class methods.

//-----------------
// sv4Project_open
//-----------------
//
PyDoc_STRVAR(sv4Project_open_doc,
  "open(project_dir) \n\
   \n\
   Open a SimVascular project. \n\
   \n\
   Args: \n\
     project_dir (str): The project directory path. \n\
");

static PyObject *
sv4Project_open(PyProject* self, PyObject* args)
{
  auto api = SvPyUtilApiFunction("s", PyRunTimeErr, __func__);
  char* fileNameArg;

  if (!PyArg_ParseTuple(args, api.format, &fileNameArg)) {
      return api.argsError();
  }

  return SV_PYTHON_OK;
}

////////////////////////////////////////////////////////
//           C l a s s   D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* PROJECT_CLASS = "Project";
static char* PROJECT_MODULE_CLASS = "project.Project";

PyDoc_STRVAR(ProjectClass_doc, "Project class functions.");

//------------------
// PyProjectMethods
//------------------
// Project class methods.
//
static PyMethodDef PyProjectClassMethods[] = {

  {"open", (PyCFunction)sv4Project_open, METH_VARARGS, sv4Project_open_doc },

  {NULL,NULL}
};

//--------------------
// PyProjectClassType
//--------------------
// Define the Python type object that stores Project data.
//
// Can't set all the fields here because g++ does not suppor non-trivial
// designated initializers.
//
PyTypeObject PyProjectClassType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  // Dotted name that includes both the module name and
  // the name of the type within the module.
  PROJECT_MODULE_CLASS,
  sizeof(PyProject)
};

//---------------
// PyProjectInit
//---------------
// This is the __init__() method for the Project class.
//
// This function is used to initialize an object after it is created.
//
static int
PyProjectInit(PyProject* self, PyObject* args, PyObject *kwds)
{
  static int numObjs = 1;
  std::cout << "[PyProjectInit] New Project object: " << numObjs << std::endl;
/*
  self->project = new ProjectElement();
  self->id = numObjs;
  numObjs += 1;
*/
  return 0;
}

//--------------
// PyProjectNew
//--------------
// Object creation function, equivalent to the Python __new__() method.
// The generic handler creates a new instance using the tp_alloc field.
//
static PyObject *
PyProjectNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  std::cout << "[PyProjectNew] PyProjectNew " << std::endl;
  auto self = (PyProject*)type->tp_alloc(type, 0);
  if (self != NULL) {
      self->id = 1;
  }

  return (PyObject *) self;
}

//--------
// PyProject
//--------
//
static void
PyProjectDealloc(PyProject* self)
{
  std::cout << "[PyProjectDealloc] Free PyProject" << std::endl;
/*
  delete self->project;
  Py_TYPE(self)->tp_free(self);
*/
}

//-------------------
// SetProjectTypeFields
//-------------------
// Set the Python type object fields that stores Project data.
//
// Need to set the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static void
SetPyProjectTypeFields(PyTypeObject& projectType)
{
  // Doc string for this type.
  projectType.tp_doc = "Project  objects";
  // Object creation function, equivalent to the Python __new__() method.
  // The generic handler creates a new instance using the tp_alloc field.
  projectType.tp_new = PyProjectNew;
  projectType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  projectType.tp_init = (initproc)PyProjectInit;
  projectType.tp_dealloc = (destructor)PyProjectDealloc;
  projectType.tp_methods = PyProjectClassMethods;
}

//-----------------
// CreatePyProject
//-----------------
//
PyObject *
CreatePyProject()
{
/*
  std::cout << "[CreatePyProject] Create Project object ... " << std::endl;
  auto projectObj = PyObject_CallObject((PyObject*)&PyProjectType, NULL);
  auto pyProject = (PyProject*)projectObj;

  if (project != nullptr) {
      delete pyProject->project;
      pyProject->project = project;
  }
  std::cout << "[CreatePyProject] pyProject id: " << pyProject->id << std::endl;
  return projectObj;
*/
}

