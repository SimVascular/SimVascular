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

// The functions defined here implement the SV Python API 'simulation' module. 
//
// A Python exception sv.simulation.Error is defined for this module. 
//
#include "SimVascular.h"
#include "SimVascular_python.h"
#include "Python.h"

#include "Simulation_PyModule.h"
#include "PyUtils.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Exception type used by PyErr_SetString() to set the for the error indicator.
static PyObject* PyRunTimeErr;

// Include the definitions for the 'simulation' classes.
#include "SimulationFluid_PyClass.cxx"
#include "SimulationROM_PyClass.cxx"

////////////////////////////////////////////////////////
//          M o d u l e  D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* SIMULATION_MODULE = "simulation";
static char* SIMULATION_MODULE_EXCEPTION = "simulation.Error";
static char* SIMULATION_MODULE_EXCEPTION_OBJECT = "Error";

//----------------------
// SimulationModule_doc
//----------------------
// Define the simulation module documentation.
//
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(SimulationModule_doc,
   "SimVascular simulation module. \n\
   \n\
   The simulation module provides an interface for SV solvers.              \n\
   \n\
");

//---------------------------
// PySimulationModuleMethods
//---------------------------
//
static PyMethodDef PySimulationModuleMethods[] =
{
    {NULL,NULL}
};

//-----------------------
// Initialize the module
//-----------------------
// Define the initialization function called by the Python 
// interpreter when the module is loaded.

//---------------------------------------------------------------------------
//                           PYTHON_MAJOR_VERSION 3                         
//---------------------------------------------------------------------------

#if PYTHON_MAJOR_VERSION == 3

// Size of per-interpreter state of the module.
// Set to -1 if the module keeps state in global variables. 
static int perInterpreterStateSize = -1;

// Always initialize this to PyModuleDef_HEAD_INIT.
static PyModuleDef_Base m_base = PyModuleDef_HEAD_INIT;

// Define the module definition struct which holds all information 
// needed to create a module object. 
//
static struct PyModuleDef PySimulationModule = {
   m_base,
   SIMULATION_MODULE,
   SimulationModule_doc, 
   perInterpreterStateSize,
   PySimulationModuleMethods
};

//---------------------
// PyInit_PySimulation   
//---------------------
// The initialization function called by the Python interpreter when the module is loaded.
//
PyMODINIT_FUNC PyInit_PySimulation()
{
  // Setup the simulation.Fluid class type.
  //
  SetPyFluidSimTypeFields(PySimulationFluidType);
  if (PyType_Ready(&PySimulationFluidType) < 0) {
      fprintf(stdout, "Error initilizing PySimulationFluidType \n");
      return nullptr;
  }

  // Setup the simulation.Fluid class type.
  //
  SetPyROMSimTypeFields(PySimulationROMType);
  if (PyType_Ready(&PySimulationROMType) < 0) {
      fprintf(stdout, "Error initilizing PySimulationROMType \n");
      return nullptr;
  }

  // Create the simulation module.
  auto module = PyModule_Create(&PySimulationModule);
  if (module == NULL) {
      fprintf(stdout,"Error in initializing 'simulation' module \n");
      return nullptr;
  }

  // Add exception.
  //
  PyRunTimeErr = PyErr_NewException(SIMULATION_MODULE_EXCEPTION, NULL, NULL);
  PyModule_AddObject(module, SIMULATION_MODULE_EXCEPTION_OBJECT, PyRunTimeErr);

  // Add Fluid class.
  Py_INCREF(&PySimulationFluidType);
  PyModule_AddObject(module, SIMULATION_FLUID_CLASS, (PyObject*)&PySimulationFluidType);

  // Add ROM class.
  Py_INCREF(&PySimulationROMType);
  PyModule_AddObject(module, SIMULATION_ROM_CLASS, (PyObject*)&PySimulationROMType);

  return module;
}

#endif

