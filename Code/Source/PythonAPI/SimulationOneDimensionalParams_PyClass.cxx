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

// The functions defined here implement the SV Python API 'simulation' module 'OneDimensional.Parameters' class. 
//

#include <structmember.h>

//-------------------
// PySimulationFluid 
//-------------------
// Define the SV Python simulation.Fluid class.
//
typedef struct
{
  PyObject_HEAD
  double density;
  double viscosity;
  double element_size;
  PyObject* material_model;
} PySimulationOneDimensionalParameters;


//////////////////////////////////////////////////////
//          U t i l i t y  F u n c t i o n s        //
//////////////////////////////////////////////////////

//////////////////////////////////////////////////////
//          C l a s s   M e t h o d s               //
//////////////////////////////////////////////////////
//
// Python 'OneDimensional.Parameters' class methods.

////////////////////////////////////////////////////////
//           C l a s s   D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* SIMULATION_ONE_DIMENSIONAL_PARAMETERS_CLASS = "OneDimensionalParameters";
// Dotted name that includes both the module name and 
// the name of the type within the module.
static char* SIMULATION_ONE_DIMENSIONAL_PARAMETERS_MODULE_CLASS = "simulation.OneDimensionalParameters";

//--------------------
// OneDimSimClass_doc 
//--------------------
// Define the Fluid class documentation.
//
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(OneDimSimParamsClass_doc,
   "The OneDimensionalParameters class provides methods for                                       \n\
   \n\
");

//--------------------
// PyOneDimSimMethods 
//--------------------
// Fluid class methods.
//
static PyMethodDef PyOneDimSimParamsMethods[] = {
  {NULL,NULL}
};

static PyMemberDef PyOneDimSimParamsMembers[] = {
    {"element_size", T_DOUBLE, offsetof(PySimulationOneDimensionalParameters, element_size), 0, NULL},
    {"fluid_density", T_DOUBLE, offsetof(PySimulationOneDimensionalParameters, density), 0, NULL},
    {"fluid_viscosity", T_DOUBLE, offsetof(PySimulationOneDimensionalParameters, viscosity), 0, NULL},
    {"material_model", T_OBJECT_EX, offsetof(PySimulationOneDimensionalParameters, material_model), 0, NULL},
    {NULL}
};

//----------------
// PyOneDimSimType 
//----------------
// Define the Python type object that stores Fluid data. 
//
// Can't set all the fields here because g++ does not suppor non-trivial 
// designated initializers. 
//
PyTypeObject PySimulationOneDimensionalParametersType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  // Dotted name that includes both the module name and 
  // the name of the type within the module.
  SIMULATION_ONE_DIMENSIONAL_PARAMETERS_MODULE_CLASS, 
  sizeof(PySimulationOneDimensionalParameters)
};

//-----------------------
// PyOneDimSimParamsInit 
//-----------------------
// This is the __init__() method for the Fluid class. 
//
// This function is used to initialize an object after it is created.
//
static int
PyOneDimSimParamsInit(PySimulationOneDimensionalParameters* self, PyObject* args, PyObject *kwds)
{
  self->density = 1.06;
  self->element_size = 0.1;
  self->viscosity = 0.04;
  self->material_model = Py_BuildValue("");
  return 0;
}

//---------------
// PyOneDimSimParamsNew 
//---------------
// Object creation function, equivalent to the Python __new__() method. 
// The generic handler creates a new instance using the tp_alloc field.
//
static PyObject *
PyOneDimSimParamsNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  auto self = (PySimulationOneDimensional*)type->tp_alloc(type, 0);
  if (self != NULL) {
  }

  return (PyObject *) self;
}

//--------------------
// PyOneDimSimParamsDealloc 
//--------------------
//
static void
PyOneDimSimParamsDealloc(PySimulationOneDimensional* self)
{
  Py_TYPE(self)->tp_free(self);
}

//--------------------------
// SetPyOneDimSimParamsTypeFields 
//--------------------------
// Set the Python type object fields that stores OneDimensional data. 
//
// Need to set the fields here because g++ does not suppor non-trivial 
// designated initializers. 
//
static void
SetPyOneDimSimParamsTypeFields(PyTypeObject& oneDimSimType)
{
  // Doc string for this type.
  oneDimSimType.tp_doc = OneDimSimParamsClass_doc; 
  // Object creation function, equivalent to the Python __new__() method. 
  // The generic handler creates a new instance using the tp_alloc field.
  oneDimSimType.tp_new = PyOneDimSimParamsNew;
  oneDimSimType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  oneDimSimType.tp_init = (initproc)PyOneDimSimParamsInit;
  oneDimSimType.tp_dealloc = (destructor)PyOneDimSimParamsDealloc;
  oneDimSimType.tp_methods = PyOneDimSimParamsMethods;
  oneDimSimType.tp_members = PyOneDimSimParamsMembers;
}

