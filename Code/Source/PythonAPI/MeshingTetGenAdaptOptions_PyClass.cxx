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

// Define the Python 'meshing.TetGenAdaptiveOptions' class that encapsulates the parameters
// used for generating a mesh using TetGen adaptive meshing. Options are stored as Python class
// attributes and are set directly in the object created from that class.
//
//     options = sv.meshing.TetGenOptions(
//     options. = 0.1
//
// The SV adaptive options are a total mess. Option values are either an int or a double.
// The int value has sometimes represents a type (e.g. strategy=1 represents isotropic meshing)
// and sometimes a count.
//
#ifndef PYAPI_MESHING_TETGEN_ADAPTIVE_OPTIONS_H
#define PYAPI_MESHING_TETGEN_ADAPTIVE_OPTIONS_H

#include <string>
#include <structmember.h>

//------------------------------
// PyMeshingTetGenAdaptOpt
//------------------------------
// Define the MeshingOptions.
//
// hmax: Global Max Edge Size: Specify a maximum target edge size. No edge size will be larger than
//     this size, even if the adaptor identifies that the solution does not require an edge length this small.
//
// hmin: Global Min Edge Size: Specify a minimum target edge size. No edge size will be smaller than
//     this size, even if the adaptor identifies that solution needs a edge length smaller than this.
//
// instep: simulation file start step number if using multiple simulation steps.
//
// metric_option: value is based on the option: (UseOneStep=0 || UseMultipleStpes=1) + 1 -> 1 or 2
//      (from sv_TetGenAdapt.cxx, cvTetGenAdapt::SetMetric)
//      1: Read average speed from file and then calculate hessian from  average speed simulation
//      2: Read average speed from vtu mesh and then calculate hessian from average speed simulation
//      3: Read solution from vtu mesh, calculate avg. magnitude of velocity over specified timestep range. Must provide
//         cylinder_results as one vtu with all timesteps. Hessian is then calculated from avg. magnitude of velocity.
//      4: Read array from mesh, and specify mesh metric with this array."<<endl;
//
// outstep: simulation file end step number if using multiple simulation steps.
//
// polynomial_order: Polynomial order of the data (currently only 1). Don't expose this to the user.
//
// ratio: Error Reduction Factor. Value multiplied by the average interpolation error in order to get a target uniform
//     local error distribution. This should be a value between zero and one. A smaller factor will attempt to achieve a
//     mesh with smaller error.
//
// sphere: this parameter does not appear to be set anywhere.
//
// step_incr: simulation file increment number if using multiple simulation steps.
//
// strategy: meshing strategy. values=isotropic. should be named 'method' ?
//
// SV uses the 'end_step' option for both the simulation end step and step. The Python API
// has both a 'step' and a 'end_step' option.
//
typedef struct {
  PyObject_HEAD
  // These options map directly to sv options.
  //
  double max_edge_size;            // SV hmax;
  double min_edge_size;            // SV hmin;
  int start_step;                  // SV instep;
  //int metric_option;               // Set to 1 or 2 depending on use_multiple_steps, don't expose.
  int end_step;                    // SV outstep;
  //int polynomial_order;            // Always = 1, don't expose.
  double error_reduction_factor;   // SV ratio
  //PyObject* sphere;                // In SV it is used but not set.
  int step_increment;              // SV step_incr;
  int use_isotropic_meshing;       // SV strategy;

  // These options have no counterpart in sv options but are used
  // to clarify setting options (e.g. sv uses outstep for two differnt
  // things.
  //
  bool use_multiple_steps;
  int step;
} PyMeshingTetGenAdaptOpt;

//-------------------
// TetGenAdaptOption
//-------------------
// PyMeshingTetGenAdaptOpt attribute names.
//
namespace TetGenAdaptOption {
  char* max_edge_size = "max_edge_size";
  char* min_edge_size = "min_edge_size";
  char* start_step = "start_step";
  char* metric_option = "metric_option";
  char* end_step = "end_step";
  //char* polynomial_order = "polynomial_order";
  char* error_reduction_factor = "error_reduction_factor";
  //char* sphere = "sphere";
  char* step_increment = "step_increment";
  char* use_isotropic_meshing = "use_isotropic_meshing";
  // Not SV options.
  char* step = "step";
  char* use_multiple_steps = "use_multiple_steps";

  // Parameter names for the 'sphere' option.
  //
  std::string SphereOption_Type = "dictionary ";
  std::string SphereOption_Format = "{ 'coordinate':[x,y,z], 'region_size':int }";
  std::string SphereOption_Desc = SphereOption_Type + SphereOption_Format;
  // Use char* for these because they are used in the Python C API functions.
  char* SphereOption_CoordinateParam = "coordinate";
  char* SphereOption_RegionSizeParam = "region_size";

  // Create a map beteen Python and SV names. The SV names are needed when
  // setting mesh options.
  //
  std::map<std::string,char*> pyToSvNameMap = {
      {std::string(end_step), "outstep"},
      {std::string(error_reduction_factor), "ratio"},
      {std::string(max_edge_size), "hmax"},
      //{std::string(metric_option), "metric_option"},
      {std::string(min_edge_size), "hmin"},
      //{std::string(polynomial_order), "poly"},
      //{std::string(sphere), "sphere"},
      {std::string(start_step), "instep"},
      {std::string(step_increment), "step_incr"},
      //{std::string(use_isotropic_meshing), "use_isotropic_meshing"}
   };

};

//////////////////////////////////////////////////////
//          U t i l i t y  F u n c t i o n s        //
//////////////////////////////////////////////////////

//--------------------------
// PyTetGenAdaptOptGetValue
//--------------------------
// Get attribute values from the MeshingOptions object.
//
static bool
PyTetGenAdaptOptGetValue(PyObject* meshingOptions, std::string name, double& value)
{
  //std::cout << "[PyTetGenAdaptOptGetValues] " << std::endl;
  //std::cout << "[PyTetGenAdaptOptGetValues] ---- PyTetGenAdaptOptGetValues ---- " << std::endl;
  auto obj = PyObject_GetAttrString(meshingOptions, name.c_str());
  if (obj == Py_None) {
      return false;
  }
  if (obj == nullptr) {
      std::cout << "[PyTetGenAdaptOptGetValues] Error: option name '" << name << "' not defined." << std::endl;
      return false;
  }

  if (PyFloat_Check(obj)) {
      value = PyFloat_AsDouble(obj);
  } else if (PyInt_Check(obj)) {
      value = PyLong_AsDouble(obj);
  }

  Py_DECREF(obj);
  return true;
}

////////////////////////////////////////////////////////
//          C l a s s    M e t h o d s                //
////////////////////////////////////////////////////////
//
// Methods for the TetGenAdaptOpt class.

//-----------------------------
// PyTetGenAdaptOpt_get_values
//-----------------------------
//
PyDoc_STRVAR(PyTetGenAdaptOpt_get_values_doc,
" get_values()  \n\
  \n\
  Get the names and values of TetGen adaptive mesh generation options. \n\
  \n\
  Args:  \n\
    None  \n\
");

static PyObject *
PyTetGenAdaptOpt_get_values(PyMeshingTetGenAdaptOpt* self, PyObject* args)
{
  PyObject* values = PyDict_New();

  PyDict_SetItemString(values, TetGenAdaptOption::error_reduction_factor, Py_BuildValue("d", self->error_reduction_factor));
  PyDict_SetItemString(values, TetGenAdaptOption::max_edge_size, Py_BuildValue("d", self->max_edge_size));
  PyDict_SetItemString(values, TetGenAdaptOption::min_edge_size, Py_BuildValue("d", self->min_edge_size));
  PyDict_SetItemString(values, TetGenAdaptOption::start_step, Py_BuildValue("i", self->start_step));
  PyDict_SetItemString(values, TetGenAdaptOption::end_step, Py_BuildValue("i", self->end_step));
  PyDict_SetItemString(values, TetGenAdaptOption::step, Py_BuildValue("i", self->step));
  PyDict_SetItemString(values, TetGenAdaptOption::step_increment, Py_BuildValue("i", self->step_increment));
  PyDict_SetItemString(values, TetGenAdaptOption::use_isotropic_meshing, PyBool_FromLong(self->use_isotropic_meshing));
  PyDict_SetItemString(values, TetGenAdaptOption::use_multiple_steps, PyBool_FromLong(self->use_multiple_steps));

  return values;
}

//-------------------------------
// PyTetGenAdaptOpt_set_defaults
//-------------------------------
// Set the default options parameter values.
//
static PyObject *
PyTetGenAdaptOpt_set_defaults(PyMeshingTetGenAdaptOpt* self)
{
  self->error_reduction_factor = 0.2;
  self->max_edge_size = 1.0;
  self->min_edge_size = 1.0;
  self->start_step = 0;
  self->step_increment = 1;
  self->end_step = 0;
  self->use_isotropic_meshing = 1;

  self->step = 0;
  self->use_multiple_steps = 0;

  Py_RETURN_NONE;
}

//-------------------------
// PyTetGenAdaptOptMethods
//-------------------------
//
static PyMethodDef PyTetGenAdaptOptMethods[] = {
  //{"create_sphere_parameter", (PyCFunction)PyTetGenAdaptOpt_create_sphere_parameter, METH_VARARGS|METH_KEYWORDS, PyTetGenAdaptOpt_create_sphere_parameter_doc},
  {"get_values", (PyCFunction)PyTetGenAdaptOpt_get_values, METH_NOARGS, PyTetGenAdaptOpt_get_values_doc},
  {NULL, NULL}
};

////////////////////////////////////////////////////////
//          C l a s s    M e m b e r s                //
////////////////////////////////////////////////////////
//
// Define the PyMeshingTetGenAdaptOpt attribute names.
//
// The attributes can be set/get directly in from the MeshingOptions object.
//
PyDoc_STRVAR(end_step_doc,
" Simulation end step (int). The last simulation step. \n\
");

PyDoc_STRVAR(error_reduction_factor_doc,
" Error reduction factor (float). The value multiplied by the average interpolation error in order to get a target uniform \n\
  local error distribution. This should be a value between zero and one. A smaller factor will attempt to achieve a \n\
  mesh with smaller error.\n\
");

PyDoc_STRVAR(max_edge_size_doc,
" Global maximum edge size (float). Specify a maximum target edge size. No edge size will be larger than  \n\
 this size, even if the adaptor identifies that the solution does not require an edge length this small. \n\
");

PyDoc_STRVAR(min_edge_size_doc,
" Global minimum edge size (float). Specify a minimum target edge size. No edge size will be smaller than  \n\
  this size, even if the adaptor identifies that solution needs a edge length smaller than this. \n\
");

PyDoc_STRVAR(use_isotropic_meshing_doc,
" Use an isotropic meshing methods (bool). This is currently the only option. \n\
");

PyDoc_STRVAR(use_multiple_steps_doc, "If True then use multiple simlation steps.");

static PyMemberDef PyTetGenAdaptOptMembers[] = {

    {TetGenAdaptOption::end_step, T_INT, offsetof(PyMeshingTetGenAdaptOpt, end_step), 0, end_step_doc},

    {TetGenAdaptOption::max_edge_size, T_DOUBLE, offsetof(PyMeshingTetGenAdaptOpt, max_edge_size), 0, max_edge_size_doc},

    {TetGenAdaptOption::min_edge_size, T_DOUBLE, offsetof(PyMeshingTetGenAdaptOpt, min_edge_size), 0, min_edge_size_doc},

    {TetGenAdaptOption::start_step, T_INT, offsetof(PyMeshingTetGenAdaptOpt, start_step), 0, "Simulation start step"},

    {TetGenAdaptOption::step_increment, T_INT, offsetof(PyMeshingTetGenAdaptOpt, step_increment), 0, "Simulation step increment."},

    {TetGenAdaptOption::use_multiple_steps, T_BOOL, offsetof(PyMeshingTetGenAdaptOpt, use_multiple_steps), 0, use_multiple_steps_doc},

    {TetGenAdaptOption::use_isotropic_meshing, T_BOOL, offsetof(PyMeshingTetGenAdaptOpt, use_isotropic_meshing), READONLY, use_isotropic_meshing_doc},
    {NULL, NULL}
};

////////////////////////////////////////////////////////
//          C l a s s    G e t / S e t                //
////////////////////////////////////////////////////////
//
// Define setters/getters for certain options.

//------------------------
// error_reduction_factor
//------------------------
//
static PyObject*
PyTetGenOptions_get_error_reduction_factor(PyMeshingTetGenAdaptOpt* self, void* closure)
{
  return Py_BuildValue("d", self->error_reduction_factor);
}

static int
PyTetGenOptions_set_error_reduction_factor(PyMeshingTetGenAdaptOpt* self, PyObject* arg, void* closure)
{
  double value = PyFloat_AsDouble(arg);
  if (PyErr_Occurred()) {
      return -1;
  }

  if (value < 0) {
      PyErr_SetString(PyExc_ValueError, "error_reduction_factor must be positive.");
      return -1;
  }

  if (value > 1.0) {
      PyErr_SetString(PyExc_ValueError, "error_reduction_factor must be <= 1.0.");
      return -1;
  }

  self->error_reduction_factor = value;
  return 0;
}

//------
// step
//------
//
static PyObject*
PyTetGenOptions_get_step(PyMeshingTetGenAdaptOpt* self, void* closure)
{
  return Py_BuildValue("i", self->step);
}

static int
PyTetGenOptions_set_step(PyMeshingTetGenAdaptOpt* self, PyObject* arg, void* closure)
{
  int value = PyLong_AsLong(arg);
  if (PyErr_Occurred()) {
      return -1;
  }

  if (value < 0) {
      PyErr_SetString(PyExc_ValueError, "The TetGen step option must be positive.");
      return -1;
  }

  self->step = value;
  return 0;
}

//-------------------------
// PyTetGenAdaptOptGetSets
//-------------------------
//
PyGetSetDef PyTetGenAdaptOptGetSets[] = {

    { TetGenAdaptOption::error_reduction_factor,
          (getter)PyTetGenOptions_get_error_reduction_factor, (setter)PyTetGenOptions_set_error_reduction_factor, NULL,  error_reduction_factor_doc },

    { TetGenAdaptOption::step,
          (getter)PyTetGenOptions_get_step, (setter)PyTetGenOptions_set_step, NULL,  NULL },

    {NULL, NULL}
};


////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* MESHING_TETGEN_ADAPTIVE_OPTIONS_CLASS = "TetGenAdaptiveOptions";
static char* MESHING_TETGEN_ADAPTIVE_OPTIONS_MODULE_CLASS = "meshing.TetGenAdaptiveOptions";

PyDoc_STRVAR(TetGenAdaptOptClass_doc, "TetGen adaptive meshing options class functions");

//----------------------
// PyTetGenAdaptOptType
//----------------------
// Define the Python type object that implements the meshing.MeshingOptions class.
//
static PyTypeObject PyTetGenAdaptOptType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  MESHING_TETGEN_ADAPTIVE_OPTIONS_MODULE_CLASS,
  sizeof(PyMeshingTetGenAdaptOpt)
};

//-----------------------
// PyTetGenAdaptOpt_init
//-----------------------
// This is the __init__() method for the meshing.MeshingOptions class.
//
// This function is used to initialize an object after it is created.
//
// Arguments:
//
static int
PyTetGenAdaptOptInit(PyMeshingTetGenAdaptOpt* self, PyObject* args, PyObject* kwargs)
{
  static int numObjs = 1;
  //std::cout << "[PyTetGenAdaptOptInit] New MeshingOptions object: " << numObjs << std::endl;
/*
  auto api = PyUtilApiFunction("d|O!O!O!", PyRunTimeErr, __func__);
  static char *keywords[] = { TetGenAdaptOption::GlobalEdgeSize, TetGenAdaptOption::SurfaceMeshFlag, TetGenAdaptOption::VolumeMeshFlag,
                              TetGenAdaptOption::MeshWallFirst, NULL};
  double global_edge_size = 0.0;
  PyObject* surface_mesh_flag = NULL;
  PyObject* volume_mesh_flag = NULL;
  PyObject* mesh_wall_first = NULL;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &global_edge_size, &PyBool_Type, &surface_mesh_flag,
        &PyBool_Type, &volume_mesh_flag, &PyBool_Type, &mesh_wall_first)) {
      api.argsError();
      return -1;
  }
*/

  // Set the default option values.
  PyTetGenAdaptOpt_set_defaults(self);

/*
  // Set the values that may have been passed in.
  //
  self->global_edge_size = global_edge_size;
  if (surface_mesh_flag) {
       self->surface_mesh_flag = PyObject_IsTrue(surface_mesh_flag);
  }
  if (volume_mesh_flag) {
      self->volume_mesh_flag = PyObject_IsTrue(volume_mesh_flag);
  }
*/

  return 0;
}

//---------------------
// PyTetGenAdaptOptNew
//---------------------
// Object creation function, equivalent to the Python __new__() method.
// The generic handler creates a new instance using the tp_alloc field.
//
static PyObject *
PyTetGenAdaptOptNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  //std::cout << "[PyTetGenAdaptOptNew] PyTetGenAdaptOptNew " << std::endl;
  auto self = (PyMeshingTetGenAdaptOpt*)type->tp_alloc(type, 0);
  if (self == NULL) {
      std::cout << "[PyTetGenAdaptOptNew] ERROR: Can't allocate type." << std::endl;
      return nullptr;
  }
  return (PyObject *) self;
}

//-------------------------
// PyTetGenAdaptOptDealloc
//-------------------------
//
static void
PyTetGenAdaptOptDealloc(PyMeshingTetGenAdaptOpt* self)
{
  //std::cout << "[PyTetGenAdaptOptDealloc] Free PyTetGenAdaptOpt" << std::endl;
  Py_TYPE(self)->tp_free(self);
}

//-----------------------------
// SetTetGenAdaptOptTypeFields
//-----------------------------
// Set the Python type object fields that stores loft option data.
//
static void
SetTetGenAdaptOptTypeFields(PyTypeObject& meshingOpts)
 {
  //std::cout << "[SetTetGenAdaptOptTypeFields] ################ SetTetGenAdaptOptTypeFields ######## " << std::endl;
  meshingOpts.tp_doc = TetGenAdaptOptClass_doc;
  meshingOpts.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  meshingOpts.tp_dict = PyDict_New();
  meshingOpts.tp_new = PyTetGenAdaptOptNew;
  meshingOpts.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  meshingOpts.tp_init = (initproc)PyTetGenAdaptOptInit;
  meshingOpts.tp_dealloc = (destructor)PyTetGenAdaptOptDealloc;
  meshingOpts.tp_methods = PyTetGenAdaptOptMethods;
  meshingOpts.tp_members = PyTetGenAdaptOptMembers;
  meshingOpts.tp_getset = PyTetGenAdaptOptGetSets;
};

//------------------------
// SetMeshingOptionsTypes
//------------------------
// Set the  loft optinnames in the MeshingOptionsType dictionary.
//
// These are for read only attibutes.
//
static void
SetTetGenAdaptOptTypes(PyTypeObject& meshingOptsType)
{
/*
  std::cout << "=============== SetMeshingOptionsTypes ==========" << std::endl;

  //PyDict_SetItemString(meshingOptsType.tp_dict, "num_pts", PyLong_AsLong(10));

  PyObject *o = PyLong_FromLong(1);
  PyDict_SetItemString(meshingOptsType.tp_dict, "num_pts", o);

  //PyDict_SetItem(meshingOptsType.tp_dict, "num_pts", o);

  std::cout << "[SetMeshingOptionsClassTypes] Done! " << std::endl;
*/

};

//--------------------------
// CreateTetGenAdaptOptType
//--------------------------
//
static PyObject *
CreateTetGenAdaptOptType(PyObject* args, PyObject* kwargs)
{
  return PyObject_Call((PyObject*)&PyTetGenAdaptOptType, args, kwargs);
}

#endif

