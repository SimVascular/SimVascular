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

// The functions defined here implement the SV Python API 'simulation' module 'Fluid' class. 
//
//     fluid_sim = simulation.Fluid()

//////////////////////////////////////////////////////
//          U t i l i t y  F u n c t i o n s        //
//////////////////////////////////////////////////////

//////////////////////////////////////////////////////
//          C l a s s   M e t h o d s               //
//////////////////////////////////////////////////////
//
// Python 'Fluid' class methods.

////////////////////////////////////////////////////////
//           C l a s s   D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* SIMULATION_FLUID_CLASS = "Fluid";
// Dotted name that includes both the module name and 
// the name of the type within the module.
static char* SIMULATION_FLUID_MODULE_CLASS = "simulation.Fluid";

//---------------
// FluidClass_doc
//---------------
// Define the Fluid class documentation.
//
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(FluidSimClass_doc,
   "The Fluid class provides methods for                                       \n\
   \n\
");

//-------------------
// PyFluidSimMethods 
//-------------------
// Fluid class methods.
//
static PyMethodDef PyFluidSimMethods[] = {

  //{"set_subdivision_method", (PyCFunction)Fluid_set_subdivision_method, METH_VARARGS|METH_KEYWORDS, Fluid_set_subdivision_method_doc},

  {NULL,NULL}
};

//----------------
// PyFluidSimType 
//----------------
// Define the Python type object that stores Fluid data. 
//
// Can't set all the fields here because g++ does not suppor non-trivial 
// designated initializers. 
//
PyTypeObject PySimulationFluidType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  // Dotted name that includes both the module name and 
  // the name of the type within the module.
  SIMULATION_FLUID_MODULE_CLASS, 
  sizeof(PySimulationFluid)
};

//----------------
// PyFluidSimInit
//----------------
// This is the __init__() method for the Fluid class. 
//
// This function is used to initialize an object after it is created.
//
static int
PyFluidSimInit(PySimulationFluid* self, PyObject* args, PyObject *kwds)
{
  std::cout << "[PyFluidSimInit] New Fluid object: " << std::endl;
  return 0;
}

//---------------
// PyFluidSimNew 
//---------------
// Object creation function, equivalent to the Python __new__() method. 
// The generic handler creates a new instance using the tp_alloc field.
//
static PyObject *
PyFluidSimNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  std::cout << "[PyFluidSimNew] PyFluidSimNew " << std::endl;
  auto self = (PySimulationFluid*)type->tp_alloc(type, 0);
  if (self != NULL) {
  }

  return (PyObject *) self;
}

//-------------------
// PyFluidSimDealloc 
//-------------------
//
static void
PyFluidSimDealloc(PySimulationFluid* self)
{
  std::cout << "[PyFluidSimDealloc] Free PyFluidSim" << std::endl;
  Py_TYPE(self)->tp_free(self);
}

//-------------------------
// SetPyFluidSimTypeFields 
//-------------------------
// Set the Python type object fields that stores Fluid data. 
//
// Need to set the fields here because g++ does not suppor non-trivial 
// designated initializers. 
//
static void
SetPyFluidSimTypeFields(PyTypeObject& fluidSimType)
{
  // Doc string for this type.
  fluidSimType.tp_doc = FluidSimClass_doc; 
  // Object creation function, equivalent to the Python __new__() method. 
  // The generic handler creates a new instance using the tp_alloc field.
  fluidSimType.tp_new = PyFluidSimNew;
  fluidSimType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  fluidSimType.tp_init = (initproc)PyFluidSimInit;
  fluidSimType.tp_dealloc = (destructor)PyFluidSimDealloc;
  fluidSimType.tp_methods = PyFluidSimMethods;
}

/*
//--------------
// CreatePyFluidSim
//--------------
// Create a PyFluidSim object. 
//
// If the path argument is not null then use it 
// for the PyFluidSim object.
//
PyObject *
CreatePyFluidSim(FluidElement* path)
{
  std::cout << "[CreatePyFluidSim] Create Fluid object ... " << std::endl;
  auto pathObj = PyObject_CallObject((PyObject*)&PyFluidSimType, NULL);
  auto pyFluid = (PyFluidSim*)pathObj;

  if (path != nullptr) {
      delete pyFluid->path; 
      pyFluid->path = path; 
  }
  std::cout << "[CreatePyFluidSim] pyFluid id: " << pyFluid->id << std::endl;
  std::cout << "[CreatePyFluidSim] pathObj ref count: " << Py_REFCNT(pathObj) << std::endl;
  return pathObj;
}
*/

