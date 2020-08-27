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

// Define the Python 'meshing.MeshSimOptions' class that encapsulates the parameters
// used for generating a mesh using MeshSim. Options are stored as Python class attributes
// and are set directly in the object created from that class.
//
// Several of the options (e.g. global_edge_size) use two values: a double size and a boolean
// flag indicating an absolute or relative size. These options are set using a Python
// dictionary. For axample, global_edge_size = {'edge_size':0.1, 'absolute':True}.
//
//     options = sv.meshing.MeshSimOptions(global_edge_size={'edge_size':0.1, 'absolute':True})
//
// Once options parameters have been set they are used to set the MeshSim mesher options using
//
//    mesher.set_options(options)
//
// The options used to set atributes on faces (e.g. local_curvature) are stored as a list of dicts.
// These options can be implemented three ways
//
// 1) Use PyMemberDef to defined atributes in PyMeshingMeshSimOptions that can be directtly store into
//
//   {MeshSimOption::LocalCurvature, T_OBJECT_EX, offsetof(PyMeshingMeshSimOptions, local_curvature) , 0, "local curvature"},
//
//   This allows the local_curvature lists to be created using Python list methods
//
//       options.local_curvature = [ {'face_id':1, 'curvature':1.0, 'absolute':True } ]
//       options.local_curvature.append( {'face_id':3, 'curvature':3.0, 'absolute':True } )
//
//   There is no way to check what options.local_curvature is set to however, can be a list of anything
//   or not even a list.
//
// 2) Use PyGetSetDef to register functions to be called to get or set the value of an attribute.
//
//    This can be used to ensure that only lists of the correct dict types are used. However, this
//    is incompatble with the standard Python list methods.
//
// 3) Define a class in Python to store options.
//
//    This can be implemented to behave like a list and check what is added to the list.
//
//
#ifndef PYAPI_MESHING_MESHSIM_OPTIONS_H
#define PYAPI_MESHING_MESHSIM_OPTIONS_H

#include <string>
#include <structmember.h>
#include <tuple>

//------------------------------
// PyMeshingMeshSimOptions
//------------------------------
// Define the PyMeshingMeshSimOptions data.
//
//  surface_optimization (bool):
//
//  surface_smoothing (int): Sets the level of surface mesh smoothing. level = 0 turns surface mesh smoothing off.
//    Higher values will result in more smoothing. Practical range for level is < 5.
//
//  volume_optimization (bool):
//
typedef struct {
  PyObject_HEAD
  PyObject* global_curvature;
  PyObject* global_curvature_min;
  PyObject* global_edge_size;
  PyObject* local_curvature;
  PyObject* local_curvature_min;
  PyObject* local_edge_size;    // [TODO:DaveP] should be face_edge_size?
  int surface_mesh_flag;
  int surface_optimization;
  int surface_smoothing;
  int volume_mesh_flag;
  int volume_optimization;
  int volume_smoothing;
} PyMeshingMeshSimOptions;

//--------------
// MeshSimOption
//--------------
// PyMeshingMeshSimOptions attribute names.
//
// [TODO:DaveP] Maybe change some of these names to be more descriptive.
//
namespace MeshSimOption {
  char* GlobalCurvature = "global_curvature";
  char* GlobalCurvatureMin = "global_curvature_min";
  char* GlobalEdgeSize = "global_edge_size";
  char* LocalCurvature = "local_curvature";
  char* LocalCurvatureMin = "local_curvature_min";
  char* LocalEdgeSize = "local_edge_size";
  char* SurfaceMeshFlag = "surface_mesh_flag";
  char* VolumeMeshFlag = "volume_mesh_flag";

  //--------------------
  // GetBooleanFromDict
  //--------------------
  bool GetBooleanFromDict(PyObject* dict, const std::string& name, bool& value)
  {
    PyObject* item = PyDict_GetItemString(dict, name.c_str());
    if (item == nullptr) {
        return false;
    }
    if (item == Py_True) {
        value = true;
    } else {
        value = false;
    }
    return true;
  }

  //-------------------
  // GetDoubleFromDict
  //-------------------
  bool GetDoubleFromDict(PyObject* dict, const std::string& name, double& value)
  {
    PyObject* sizeItem = PyDict_GetItemString(dict, name.c_str());
    if (sizeItem == nullptr) {
              return false;
    }
    value = PyFloat_AsDouble(sizeItem);
    if (PyErr_Occurred()) {
        return false;
    }
    return true;
  };

  //----------------
  // GetIntFromDict
  //----------------
  bool GetIntFromDict(PyObject* dict, const std::string& name, int& value)
  {
    PyObject* sizeItem = PyDict_GetItemString(dict, name.c_str());
    if (sizeItem == nullptr) {
        return false;
    }
    value = PyLong_AsLong(sizeItem);
    if (PyErr_Occurred()) {
        return false;
    }
    return true;
  };

  //----------------
  // DoubleBoolDict
  //----------------
  // Class representing an option parameter as the Python dict { 'doubleName':double, 'boolName':bool }.
  //
  class DoubleBoolParam {
    public:
      DoubleBoolParam(const std::string& name, const std::string& doubleName, const std::string& boolName) : paramName(name),
              doubleName(doubleName), boolName(boolName)
      {
          format = "{ '" + doubleName + "':double, '" + boolName + "':bool }";
          description = "dictionary " + format;
      };

      std::string boolName;
      std::string description;
      std::string doubleName;
      std::string format;
      std::string paramName;

      //-------------
      // GetErrorMsg
      //-------------
      std::string GetErrorMsg(PyObject* obj)
      {
          std::string paramErrMsg = "The " + paramName + " parameter must be a " + description;
          auto objRep = PyObject_Repr(obj);
          const char* objStr = PyString_AsString(objRep);
          auto objErrorMsg = "Error in option '" + std::string(objStr) + "'. ";
          auto errorMsg = objErrorMsg + paramErrMsg;
          return errorMsg;
      };

      //-----------
      // GetValues
      //-----------
      virtual bool GetValues(PyObject* obj, double& doubleValue, bool& boolValue, bool subDict=false)
      {
          auto errorMsg = GetErrorMsg(obj);
          int num = PyDict_Size(obj);
          if (!subDict && (num != 2)) {
              PyErr_SetString(PyExc_ValueError, errorMsg.c_str());
              return false;
          }

          if (!GetDoubleFromDict(obj, doubleName, doubleValue)) {
              if (!subDict) {
                  PyErr_SetString(PyExc_ValueError, errorMsg.c_str());
              }
              return false;
          }
          if (doubleValue <= 0.0) {
              auto msg = "The " + doubleName + " parameter must be > 0.";
              PyErr_SetString(PyExc_ValueError, msg.c_str());
              return false;
          }

          if (!GetBooleanFromDict(obj, boolName, boolValue)) {
              PyErr_SetString(PyExc_ValueError, errorMsg.c_str());
              return false;
          }

          return true;
      };

  };

  //-------------------
  // IntDoubleBoolParam
  //-------------------
  // Class representing an option parameter as the Python dict { 'intName':int, 'doubleName':double, 'boolName':bool }.
  //
  class IntDoubleBoolParam : public DoubleBoolParam {
    public:
      IntDoubleBoolParam(const std::string& name, const std::string& intName, const std::string& doubleName, const std::string& boolName) :
              DoubleBoolParam(name, doubleName, boolName), intName(intName)
      {
          format = "{ '" + intName + "':int, '" + doubleName + "':double, '" + boolName + "':bool }";
          description = "dictionary " + format;
      };
      std::string intName;

      PyObject* Create(int intValue, double doubleValue, int boolValue)
      {
          return Py_BuildValue("{s:i, s:d, s:O}", intName.c_str(), intValue, doubleName.c_str(), doubleValue,
              boolName.c_str(), PyBool_FromLong(boolValue));
      }

      //-----------
      // GetValues
      //-----------
      bool GetValues(PyObject* obj, int& intValue, double& doubleValue, bool& boolValue)
      {
          auto errorMsg = GetErrorMsg(obj);
          int num = PyDict_Size(obj);
          if (num != 3) {
              PyErr_SetString(PyExc_ValueError, errorMsg.c_str());
              return false;
          }

          if (!GetIntFromDict(obj, intName, intValue)) {
              PyErr_SetString(PyExc_ValueError, errorMsg.c_str());
              return false;
          }

          // Get double and bool values, don't check size of 'obj'.
          bool subDict = true;
          if (!DoubleBoolParam::GetValues(obj, doubleValue, boolValue, subDict)) {
              PyErr_SetString(PyExc_ValueError, errorMsg.c_str());
              return false;
          }

          return true;
      };
  };

  // Create objects storing information for options that are Python dicts.
  static DoubleBoolParam globalCurvatureParam("global_curvature", "curvature", "absolute");
  static DoubleBoolParam globalCurvatureMinParam("global_curvature_min", "min_curvature", "absolute");
  static DoubleBoolParam globalEdgeSizeParam("global_edge_size", "edge_size", "absolute");
  static IntDoubleBoolParam localCurvatureParam("local_curvature", "face_id", "curvature", "absolute");
  static IntDoubleBoolParam localCurvatureMinParam("local_curvature_min", "face_id","min_curvature", "absolute");
  static IntDoubleBoolParam localEdgeSizeParam("local_edge_size", "face_id", "edge_size", "absolute");

  // Create a map beteen Python and SV names. The SV names are needed when
  // setting mesh options.
  std::map<std::string,char*> pyToSvNameMap = {
      {std::string(GlobalCurvature), "GlobalCurvature"},
      {std::string(GlobalCurvatureMin), "GlobalCurvatureMin"},
      {std::string(GlobalEdgeSize), "GlobalEdgeSize"},
      {std::string(LocalCurvature), "LocalCurvature"},
      {std::string(LocalCurvatureMin), "LocalCurvatureMin"},
      {std::string(LocalEdgeSize), "LocalEdgeSize"},
      {std::string(SurfaceMeshFlag), "SurfaceMeshFlag"},
      {std::string(VolumeMeshFlag), "VolumeMeshFlag"}
   };

};

// Define a tuple for storing local edge size data: face ID, edge size and absolute flag.
typedef std::tuple<int, double, bool> PyMeshSimOptionsLocalEdgeSizeData;

//////////////////////////////////////////////////////
//          U t i l i t y  F u n c t i o n s        //
//////////////////////////////////////////////////////

//-----------------------------------------
// PyMeshSimOptionsCreateLocalEdgeSizeDict
//-----------------------------------------
// Create a Python dict for a local edge size parameter.
//
static PyObject *
PyMeshSimOptionsCreateLocalEdgeSizeDict(PyUtilApiFunction& api, int faceID, double edgeSize, int absolute)
{
  if (edgeSize <= 0.0) {
      api.error("The '" + std::string(MeshSimOption::localEdgeSizeParam.doubleName) + "' must be > 0.");
      return nullptr;
  }

  if (faceID <= 0) {
      api.error("The '" + std::string(MeshSimOption::localEdgeSizeParam.intName) + "' must be > 0.");
      return nullptr;
  }

  return MeshSimOption::localEdgeSizeParam.Create(faceID, edgeSize, absolute);
}

//---------------------------
// PyMeshSimOptionsGetValues
//---------------------------
// Get attribute values from the MeshingOptions object.
//
// Return a vector of doubles to mimic how SV processes options.
//
// Parameter values must be stored in the same order as they
// are processed in sv/MeshSimMeshObject/cvMeshSimMeshObject.cxx.
//
static std::vector<double>
PyMeshSimOptionsGetValues(PyObject* meshingOptions, std::string name)
{
  std::vector<double> values;
  auto obj = PyObject_GetAttrString(meshingOptions, name.c_str());
  if (obj == Py_None) {
      return values;
  }

  if (PyFloat_Check(obj)) {
      auto value = PyFloat_AsDouble(obj);
      values.push_back(value);
  } else if (PyInt_Check(obj)) {
      auto value = PyLong_AsDouble(obj);
      values.push_back(value);
  } else if (PyTuple_Check(obj)) {
      int num = PyTuple_Size(obj);
      for (int i = 0; i < num; i++) {
          auto item = PyTuple_GetItem(obj, i);
          auto value = PyFloat_AsDouble(item);
          values.push_back(value);
      }
  }
/*
  } else if (name == MeshSimOption::GlobalEdgeSize) {
      double edgeSize;
      bool absolute;
      MeshSimOption::globalEdgeSizeParam.GetValues(obj, edgeSize, absolute);
      values.push_back((double)absolute);
      values.push_back(edgeSize);
  }
*/

  Py_DECREF(obj);
  return values;
}

//-----------------------------------
// PyMeshSimOptionsGetLocalEdgeSizes
//-----------------------------------
// Get attribute local edge size values from the MeshingOptions object.
//
// Return a vector of (faceID, edgeSize, absoluteFlag) tuples for the local edge size data.
//
static std::vector<PyMeshSimOptionsLocalEdgeSizeData>
PyMeshSimOptionsGetLocalEdgeSizes(PyObject* meshingOptions, std::string name)
{
  std::cout << "[PyMeshSimOptionsGetLocalEdgeSizes]  " << std::endl;
  std::cout << "[PyMeshSimOptionsGetLocalEdgeSizes]  --------- PyMeshSimOptionsGetLocalEdgeSizes ---------" << std::endl;
  std::vector<PyMeshSimOptionsLocalEdgeSizeData> localEdgeSizes;
  auto obj = PyObject_GetAttrString(meshingOptions, name.c_str());
  if (obj == Py_None) {
      return localEdgeSizes;
  }
  int listSize = PyList_Size(obj);
  for (int i = 0; i < listSize; i++) {
      auto localEdgeSize = PyList_GetItem(obj,i);
      int faceID;
      double edgeSize;
      bool absolute;

      if (!MeshSimOption::localEdgeSizeParam.GetValues(localEdgeSize, faceID, edgeSize, absolute)) {
          return localEdgeSizes;
      }
      //PyMeshSimOptionsGetLocalEdgeSizeValues(localEdgeSize, faceID, edgeSize, absolute);
      localEdgeSizes.push_back( std::make_tuple(faceID, edgeSize, absolute) );
  }

  Py_DECREF(obj);
  return localEdgeSizes;
}

//-----------------------------------------------
// PyMeshSimOptions_parse_python_meshsim_options
//-----------------------------------------------
// This is used to test an experimental  MeshSim options object
// create in Python using a class.
//
void
PyMeshSimOptions_parse_python_meshsim_options(PyMeshingMeshSimOptions* self, PyObject* optionArg)
{
  std::cout << "---------- PyMeshSimOptions_check ----------" << std::endl;

  //PyTypeObject* type = optionArg->ob_type;
  //const char* p = type->tp_name;
  //std::cout << "[PyMeshSimOptions_check] Type: " << p << std::endl;

  std::cout << "[PyMeshSimOptions_check] Iterate over options ... " << std::endl;
  for (auto const& entry : MeshSimOption::pyToSvNameMap) {
      auto name = entry.first;
      std::cout << "[PyMeshSimOptions_check] Option: " << name << std::endl;
      auto item = PyObject_GetAttrString(optionArg, name.c_str());
      PyTypeObject* type = item->ob_type;
      auto itemType = std::string(type->tp_name);
      std::cout << "[PyMeshSimOptions_check]   Type: " << itemType << std::endl;

      if (PyList_Check(item)) {
          std::cout << "[PyMeshSimOptions_check]   List " << std::endl;
      } else if (PyDict_Check(item)) {
          std::cout << "[PyMeshSimOptions_check]   Dict " << std::endl;
      } else if (itemType == "MeshSimListOption") {
          auto innerList = PyObject_GetAttrString(item, "inner_list");
          PyTypeObject* type = innerList->ob_type;
          auto itemType = std::string(type->tp_name);
          std::cout << "[PyMeshSimOptions_check]   innerList: " << innerList << std::endl;
          std::cout << "[PyMeshSimOptions_check]   innerList type: " << itemType << std::endl;
          int num = PyList_Size(innerList);
          std::cout << "[PyMeshSimOptions_check]   Number of values: " << num << std::endl;
          for (int i = 0; i < num; i++) {
              std::cout << "[PyMeshSimOptions_check]   ---- " << i << "----" << std::endl;
              auto listItem = PyList_GetItem(innerList, i);
              PyTypeObject* type = listItem->ob_type;
              auto listItemType = std::string(type->tp_name);
              std::cout << "[PyMeshSimOptions_check]   listItemType: " << listItemType << std::endl;
              PyObject* intItem = PyDict_GetItemString(listItem, "face_id");
              std::cout << "[PyMeshSimOptions_check]   intItem: " << intItem << std::endl;
              //PyTypeObject* itype = intItem->ob_type;
              //auto intItemType = std::string(itype->tp_name);
              //std::cout << "[PyMeshSimOptions_check]   intItemType: " << intItemType << std::endl;
              auto value = PyLong_AsDouble(intItem);
              std::cout << "[PyMeshSimOptions_check]   int value: " << value << std::endl;
          }

      } else {
          auto values = PyMeshSimOptionsGetValues(optionArg, name);
          std::cout << "[PyMeshSimOptions_check]   Number of values: " << values.size() << std::endl;
          std::cout << "[PyMeshSimOptions_check]   Values: ";
          for (auto const& value : values) {
              std::cout << " " << value;
          }
          std::cout << std::endl;
      }
  }
}

////////////////////////////////////////////////////////
//          C l a s s    M e t h o d s                //
////////////////////////////////////////////////////////
//
// Methods for the MeshSimOptions class.

//--------------------------------------
// PyMeshSimOptions_add_local_curvature
//--------------------------------------
//
PyDoc_STRVAR(PyMeshSimOptions_add_local_curvature_doc,
  "add_local_curvature(face_id, curvature, absolute=True)  \n\
  \n\
  Add the given value to the local edge size list. \n\
  \n\
  Args:  \n\
    face_id (int): The ID of a solid model face.  \n\
    curvature (double): The curvature for the face.  \n\
    absolute (bool): The flag indicating an absolute or relative edge size. If true then edge size is an absolute size.  \n\
");

static PyObject *
PyMeshSimOptions_add_local_curvature(PyMeshingMeshSimOptions* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("id|O!", PyRunTimeErr, __func__);
  static char* keywords[] = { const_cast<char*>(MeshSimOption::localCurvatureParam.intName.c_str()),
                              const_cast<char*>(MeshSimOption::localCurvatureParam.doubleName.c_str()),
                              const_cast<char*>(MeshSimOption::localCurvatureParam.boolName.c_str()), NULL };
  int faceID = 0;
  double curvature = 0.0;
  PyObject* absoluteArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &faceID, &curvature, &PyBool_Type, &absoluteArg)) {
      return api.argsError();
  }

  int absolute = 1;
  if (absoluteArg == Py_False) {
      absolute = 0;
  }

  // Create a local curvature dict containing faceID, curvature, and absolute values.
  auto value = MeshSimOption::localCurvatureParam.Create(faceID, curvature, absolute);
  if (value == nullptr) {
      return nullptr;
  }
  Py_INCREF(value);

  PyList_Append(self->local_curvature, value);
  Py_RETURN_NONE;
}

//--------------------------------------
// PyMeshSimOptions_add_local_edge_size
//--------------------------------------
//
PyDoc_STRVAR(PyMeshSimOptions_add_local_edge_size_doc,
  "add_local_edge_size(face_id, edge_size, absolute=True)  \n\
  \n\
  Add the given value to the local edge size list. \n\
  \n\
  Args:  \n\
    face_id (int): The ID of a solid model face.  \n\
    edge_size (double): The edge size for the face.  \n\
    absolute (bool): The flag indicating an absolute or relative edge size. If true then edge size is an absolute size.  \n\
");

static PyObject *
PyMeshSimOptions_add_local_edge_size(PyMeshingMeshSimOptions* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("id|O!", PyRunTimeErr, __func__);
  static char* keywords[] = { const_cast<char*>(MeshSimOption::localEdgeSizeParam.intName.c_str()),
                              const_cast<char*>(MeshSimOption::localEdgeSizeParam.doubleName.c_str()),
                              const_cast<char*>(MeshSimOption::localEdgeSizeParam.boolName.c_str()), NULL };
  int faceID = 0;
  double edgeSize = 0.0;
  PyObject* absoluteArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &faceID, &edgeSize, &PyBool_Type, &absoluteArg)) {
      return api.argsError();
  }

  int absolute = 1;
  if (absoluteArg == Py_False) {
      absolute = 0;
  }

  auto value = PyMeshSimOptionsCreateLocalEdgeSizeDict(api, faceID, edgeSize, absolute);
  if (value == nullptr) {
      return nullptr;
  }

  PyList_Append(self->local_edge_size, value);
  Py_INCREF(value);
  Py_RETURN_NONE;
}

//------------------------
// PyMeshSimOptions_check
//------------------------
//
PyDoc_STRVAR(PyMeshSimOptions_check_doc,
  "check()  \n\
  \n\
  Check options. \n\
  \n\
");

static PyObject *
PyMeshSimOptions_check(PyMeshingMeshSimOptions* self, PyObject* args)
{
  std::cout << "---------- PyMeshSimOptions_check ----------" << std::endl;

  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);

  Py_RETURN_NONE;
}

//---------------------------------------------------
// PyMeshSimOptions_create_local_edge_size_parameter
//---------------------------------------------------
//
PyDoc_STRVAR(PyMeshSimOptions_create_local_edge_size_parameter_doc,
  "create_local_edge_size(face_id, edge_size, absolute=True)  \n\
  \n\
  Create a parameter for the local_edge_size option. \n\
  \n\
  Args:  \n\
    face_id (int): The ID of a solid model face.  \n\
    edge_size (double): The edge size for the face.  \n\
    absolute (bool): The flag indicating an absolute or relative edge size. If true then edge size is an absolute size.  \n\
  \n\
  Returns the dict { 'edge_size':edge_size, 'absolute':absolute } \n\
");

static PyObject *
PyMeshSimOptions_create_local_edge_size_parameter(PyMeshingMeshSimOptions* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("id|O!", PyRunTimeErr, __func__);
  static char *keywords[] = { const_cast<char*>(MeshSimOption::localEdgeSizeParam.intName.c_str()),
                              const_cast<char*>(MeshSimOption::localEdgeSizeParam.doubleName.c_str()),
                              const_cast<char*>(MeshSimOption::localEdgeSizeParam.boolName.c_str()), NULL };
  int faceID = 0;
  double edgeSize = 0.0;
  PyObject* absoluteArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &faceID, &edgeSize, &PyBool_Type, &absoluteArg)) {
      return api.argsError();
  }

  int absolute = 1;
  if (absoluteArg == Py_False) {
      absolute = 0;
  }

  auto value = PyMeshSimOptionsCreateLocalEdgeSizeDict(api, faceID, edgeSize, absolute);
  if (value == nullptr) {
      return nullptr;
  }

  return value;
}

//-----------------------------
// PyMeshSimOptions_get_values
//-----------------------------
//
PyDoc_STRVAR(PyMeshSimOptions_get_values_doc,
" get_values()  \n\
  \n\
  Get the names and values of MeshSim mesh generation options. \n\
  \n\
  Returns a dict of options names/values.  \n\
");

static PyObject *
PyMeshSimOptions_get_values(PyMeshingMeshSimOptions* self, PyObject* args)
{
  PyObject* values = PyDict_New();

  PyDict_SetItemString(values, MeshSimOption::GlobalCurvature, self->global_curvature);
  PyDict_SetItemString(values, MeshSimOption::GlobalEdgeSize, self->global_edge_size);
  PyDict_SetItemString(values, MeshSimOption::LocalCurvature, self->local_curvature);
  PyDict_SetItemString(values, MeshSimOption::LocalEdgeSize, self->local_edge_size);
  PyDict_SetItemString(values, MeshSimOption::SurfaceMeshFlag, PyBool_FromLong(self->surface_mesh_flag));
  PyDict_SetItemString(values, MeshSimOption::VolumeMeshFlag, PyBool_FromLong(self->volume_mesh_flag));

  Py_INCREF(values);
  return values;
}

//-------------------------------
// PyMeshSimOptions_set_defaults
//-------------------------------
// Set the default options parameter values.
//
PyDoc_STRVAR(PyMeshSimOptions_set_defaults_doc,
  "set_defaults()  \n\
  \n\
  Set the MeshSim options to their default values. .\n\
  \n\
");

static PyObject *
PyMeshSimOptions_set_defaults(PyMeshingMeshSimOptions* self)
{
  self->global_curvature = Py_BuildValue("{}");
  self->global_curvature_min = Py_BuildValue("{}");
  self->global_edge_size = Py_BuildValue("{}");

  self->local_curvature = Py_BuildValue("[]");
  Py_INCREF(self->local_curvature);
  self->local_curvature_min = Py_BuildValue("[]");
  Py_INCREF(self->local_curvature_min);
  self->local_edge_size = Py_BuildValue("[]");
  Py_INCREF(self->local_edge_size);

  self->surface_mesh_flag = 0;
  self->surface_optimization = 1;  // bool
  self->surface_smoothing = 3;

  self->volume_mesh_flag = 0;
  self->volume_optimization = 1;  // bool
  self->volume_smoothing = 1;

  Py_RETURN_NONE;
}

//--------------------------------------
// PyMeshSimOptions_set_local_curvature
//--------------------------------------
//
PyDoc_STRVAR(PyMeshSimOptions_set_local_curvature_doc,
  "set_local_curvature(face_id, curvature, absolute=True)  \n\
  \n\
  Set the local edge size list to the given value. The original values in the list are removed.\n\
  \n\
  Args:  \n\
    face_id (int): The ID of a solid model face.  \n\
    edge_size (double): The edge size for the face.  \n\
    absolute (bool): The flag indicating an absolute or relative edge size. If true then edge size is an absolute size.  \n\
");

static PyObject *
PyMeshSimOptions_set_local_curvature(PyMeshingMeshSimOptions* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("id|O!", PyRunTimeErr, __func__);
  static char* keywords[] = { const_cast<char*>(MeshSimOption::localCurvatureParam.intName.c_str()),
                              const_cast<char*>(MeshSimOption::localCurvatureParam.doubleName.c_str()),
                              const_cast<char*>(MeshSimOption::localCurvatureParam.boolName.c_str()), NULL };
  int faceID = 0;
  double curvature = 0.0;
  PyObject* absoluteArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &faceID, &curvature, &PyBool_Type, &absoluteArg)) {
      return api.argsError();
  }

  int absolute = 1;
  if (absoluteArg == Py_False) {
      absolute = 0;
  }

  auto value = MeshSimOption::localCurvatureParam.Create(faceID, curvature, absolute);
  if (value == nullptr) {
      return nullptr;
  }
  Py_INCREF(value);

  // Create a new list.
  //
  // [TODO:DaveP] need to release memory here?
  //
  auto curvatureList = PyList_New(1);
  PyList_SetItem(curvatureList, 0, value);
  self->local_curvature = curvatureList;
  Py_INCREF(curvatureList);

  Py_RETURN_NONE;
}

//--------------------------------------
// PyMeshSimOptions_set_local_edge_size
//--------------------------------------
//
PyDoc_STRVAR(PyMeshSimOptions_set_local_edge_size_doc,
  "set_local_edge_size(face_id, edge_size, absolute=True)  \n\
  \n\
  Set the local edge size list to the given value. The original values in the list are removed.\n\
  \n\
  Args:  \n\
    face_id (int): The ID of a solid model face.  \n\
    edge_size (double): The edge size for the face.  \n\
    absolute (bool): The flag indicating an absolute or relative edge size. If true then edge size is an absolute size.  \n\
");

static PyObject *
PyMeshSimOptions_set_local_edge_size(PyMeshingMeshSimOptions* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("id|O!", PyRunTimeErr, __func__);
  static char* keywords[] = { const_cast<char*>(MeshSimOption::localEdgeSizeParam.intName.c_str()),
                              const_cast<char*>(MeshSimOption::localEdgeSizeParam.doubleName.c_str()),
                              const_cast<char*>(MeshSimOption::localEdgeSizeParam.boolName.c_str()), NULL };
  int faceID = 0;
  double edgeSize = 0.0;
  PyObject* absoluteArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &faceID, &edgeSize, &PyBool_Type, &absoluteArg)) {
      return api.argsError();
  }

  int absolute = 1;
  if (absoluteArg == Py_False) {
      absolute = 0;
  }

  auto value = PyMeshSimOptionsCreateLocalEdgeSizeDict(api, faceID, edgeSize, absolute);
  if (value == nullptr) {
      return nullptr;
  }

  // [TODO:DaveP] need to release memory here.
  auto edgeSizeList = PyList_New(1);
  PyList_SetItem(edgeSizeList, 0, value);
  Py_INCREF(value);
  self->local_edge_size = edgeSizeList;

  Py_RETURN_NONE;
}

//-------------------------
// PyMeshSimOptionsMethods
//-------------------------
//
static PyMethodDef PyMeshSimOptionsMethods[] = {
  {"add_local_curvature", (PyCFunction)PyMeshSimOptions_add_local_curvature, METH_VARARGS|METH_KEYWORDS, PyMeshSimOptions_add_local_curvature_doc},
  {"add_local_edge_size", (PyCFunction)PyMeshSimOptions_add_local_edge_size, METH_VARARGS|METH_KEYWORDS, PyMeshSimOptions_add_local_edge_size_doc},
  {"check", (PyCFunction)PyMeshSimOptions_check, METH_NOARGS, PyMeshSimOptions_check_doc},
  {"create_local_edge_size_parameter", (PyCFunction)PyMeshSimOptions_create_local_edge_size_parameter, METH_VARARGS|METH_KEYWORDS, PyMeshSimOptions_create_local_edge_size_parameter_doc},
  {"get_values", (PyCFunction)PyMeshSimOptions_get_values, METH_NOARGS, PyMeshSimOptions_get_values_doc},
  {"set_local_curvature", (PyCFunction)PyMeshSimOptions_set_local_curvature, METH_VARARGS|METH_KEYWORDS, PyMeshSimOptions_set_local_curvature_doc},
  {"set_local_edge_size", (PyCFunction)PyMeshSimOptions_set_local_edge_size, METH_VARARGS|METH_KEYWORDS, PyMeshSimOptions_set_local_edge_size_doc},
  {NULL, NULL}
};

////////////////////////////////////////////////////////
//          C l a s s    M e m b e r s                //
////////////////////////////////////////////////////////
//
// Define the PyMeshingMeshSimOptions attribute names.
//
// The attributes can be set/get directly in from the MeshingOptions object.
//
// [TODO:DaveP] need to add doc here.
//
static PyMemberDef PyMeshSimOptionsMembers[] = {

    //{MeshSimOption::LocalCurvature, T_OBJECT_EX, offsetof(PyMeshingMeshSimOptions, local_curvature) , 0, "local curvature"},

    {MeshSimOption::LocalEdgeSize, T_OBJECT_EX, offsetof(PyMeshingMeshSimOptions, local_edge_size) , 0, "local edge size"},

    {MeshSimOption::SurfaceMeshFlag, T_BOOL, offsetof(PyMeshingMeshSimOptions, surface_mesh_flag), 0, "surface_mesh_flag"},

    {MeshSimOption::SurfaceMeshFlag, T_BOOL, offsetof(PyMeshingMeshSimOptions, surface_mesh_flag), 0, "surface_mesh_flag"},

    {MeshSimOption::VolumeMeshFlag, T_BOOL, offsetof(PyMeshingMeshSimOptions, volume_mesh_flag), 0, "volume_mesh_flag"},
    {NULL}
};

////////////////////////////////////////////////////////
//          C l a s s    G e t / S e t                //
////////////////////////////////////////////////////////
//
// Define setters/getters for certain options.

//------------------------------------------
// PyMeshSimOptions_global_curvature_getter
//------------------------------------------
//
static PyObject*
PyMeshSimOptions_global_curvature_getter(PyMeshingMeshSimOptions* self, void* closure)
{
  return self->global_curvature;
}

//--------
// setter
//--------
static int
PyMeshSimOptions_global_curvature_setter(PyMeshingMeshSimOptions* self, PyObject* value, void* closure)
{
  std::cout << "---------- PyMeshSimOptions_global_curvature_setter ----------" << std::endl;

  static std::string errorMsg = "The global_curvature parameter must be a " + MeshSimOption::globalCurvatureParam.description;
  if (!PyDict_Check(value)) {
      PyErr_SetString(PyExc_ValueError, errorMsg.c_str());
      return -1;
  }

  // Check that the option is vaild.
  //
  double curvature;
  bool absoluteFlag;
  if (!MeshSimOption::globalCurvatureParam.GetValues(value, curvature, absoluteFlag)) {
      return -1;
  }

  self->global_curvature = value;
  Py_INCREF(value);
  return 0;
}

//---------------------------------------
// PyMeshSimOptions_get_global_edge_size
//---------------------------------------
//
static PyObject*
PyMeshSimOptions_global_edge_size_getter(PyMeshingMeshSimOptions* self, void* closure)
{
  return self->global_edge_size;
}

static int
PyMeshSimOptions_global_edge_size_setter(PyMeshingMeshSimOptions* self, PyObject* value, void* closure)
{
  static std::string errorMsg = "The global_edge_size parameter must be a " + MeshSimOption::globalEdgeSizeParam.description;
  std::cout << "[PyMeshSimOptions_set_global_edge_size]  " << std::endl;
  if (!PyDict_Check(value)) {
      PyErr_SetString(PyExc_ValueError, errorMsg.c_str());
      return -1;
  }

  double edgeSize;
  bool absoluteFlag;
  if (!MeshSimOption::globalEdgeSizeParam.GetValues(value, edgeSize, absoluteFlag)) {
      return -1;
  }

  self->global_edge_size = value;
  Py_INCREF(value);
  return 0;
}

//------------------------------------------
// PyMeshSimOptions_global_curvature_getter
//------------------------------------------
//
static PyObject*
PyMeshSimOptions_local_curvature_getter(PyMeshingMeshSimOptions* self, void* closure)
{
  return self->local_curvature;
}

//--------
// setter
//--------
static int
PyMeshSimOptions_local_curvature_setter(PyMeshingMeshSimOptions* self, PyObject* value, void* closure)
{
  std::cout << "---------- PyMeshSimOptions_local_curvature_setter ----------" << std::endl;
  static std::string errorMsg = "The local_curvature parameter must be a " + MeshSimOption::localCurvatureParam.description;
  std::cout << "[PyMeshSimOptions_local_curvature_setter] paramName: " << MeshSimOption::localCurvatureParam.paramName << std::endl;
  std::cout << "[PyMeshSimOptions_local_curvature_setter] desc: " << MeshSimOption::localCurvatureParam.description << std::endl;

  if (!PyList_Check(value)) {
      auto msg = "The local_curvature parameter must be a list of " + MeshSimOption::localCurvatureParam.description;
      PyErr_SetString(PyExc_ValueError, msg.c_str());
      return -1;
  }

  // Check list of dicts.
  //
  int listSize = PyList_Size(value);
  for (int i = 0; i < listSize; i++) {
      auto localCurvature = PyList_GetItem(value,i);
      int faceID;
      double curvature;
      bool absoluteFlag;
      if (!MeshSimOption::localCurvatureParam.GetValues(localCurvature, faceID, curvature, absoluteFlag)) {
          return -1;
      }
  }

  self->local_curvature = value;
  Py_INCREF(value);
  return 0;
}

//-------------------------
// PyMeshSimOptionsGetSets
//-------------------------
//
PyGetSetDef PyMeshSimOptionsGetSets[] = {
    { MeshSimOption::GlobalCurvature, (getter)PyMeshSimOptions_global_curvature_getter, (setter)PyMeshSimOptions_global_curvature_setter, NULL,  NULL },
    { MeshSimOption::GlobalEdgeSize, (getter)PyMeshSimOptions_global_edge_size_getter, (setter)PyMeshSimOptions_global_edge_size_setter, NULL,  NULL },
    { MeshSimOption::LocalCurvature, (getter)PyMeshSimOptions_local_curvature_getter, (setter)PyMeshSimOptions_local_curvature_setter, NULL,  NULL },
    {NULL}
};

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* MESHING_MESHSIM_OPTIONS_CLASS = "MeshSimOptions";
static char* MESHING_MESHSIM_OPTIONS_MODULE_CLASS = "meshing.MeshSimOptions";

PyDoc_STRVAR(MeshSimOptionsClass_doc, "MeshSim meshing options class functions");

//----------------------
// PyMeshSimOptionsType
//----------------------
// Define the Python type object that implements the meshing.MeshingOptions class.
//
static PyTypeObject PyMeshSimOptionsType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  MESHING_MESHSIM_OPTIONS_MODULE_CLASS,
  sizeof(PyMeshingMeshSimOptions)
};

//-----------------------
// PyMeshSimOptions_init
//-----------------------
// This is the __init__() method for the meshing.MeshingOptions class.
//
// This function is used to initialize an object after it is created.
//
static int
PyMeshSimOptionsInit(PyMeshingMeshSimOptions* self, PyObject* args, PyObject* kwargs)
{
  static int numObjs = 1;
  std::cout << "[PyMeshSimOptionsInit] New MeshingOptions object: " << numObjs << std::endl;
  auto api = PyUtilApiFunction("O!|O!O!", PyRunTimeErr, __func__);
  static char *keywords[] = { MeshSimOption::GlobalEdgeSize, MeshSimOption::SurfaceMeshFlag, MeshSimOption::VolumeMeshFlag, NULL};
  PyObject* global_edge_size = nullptr;
  PyObject* surface_mesh_flag = nullptr;
  PyObject* volume_mesh_flag = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyDict_Type, &global_edge_size,
        &PyBool_Type, &surface_mesh_flag, &PyBool_Type, &volume_mesh_flag)) {
      api.argsError();
      return -1;
  }

  // Set the default option values.
  PyMeshSimOptions_set_defaults(self);

  // Check global edge size parameter.
  double edgeSize;
  bool absolute;
  if (!MeshSimOption::globalEdgeSizeParam.GetValues(global_edge_size, edgeSize, absolute)) {
      return -1;
  }

  // Set the values that may have been passed in.
  //
  self->global_edge_size = PyDict_Copy(global_edge_size);
  if (surface_mesh_flag) {
       self->surface_mesh_flag = PyObject_IsTrue(surface_mesh_flag);
  }
  if (volume_mesh_flag) {
      self->volume_mesh_flag = PyObject_IsTrue(volume_mesh_flag);
  }

  return 0;
}

//---------------------
// PyMeshSimOptionsNew
//---------------------
// Object creation function, equivalent to the Python __new__() method.
// The generic handler creates a new instance using the tp_alloc field.
//
static PyObject *
PyMeshSimOptionsNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  std::cout << "[PyMeshSimOptionsNew] PyMeshSimOptionsNew " << std::endl;
  auto self = (PyMeshingMeshSimOptions*)type->tp_alloc(type, 0);
  if (self == NULL) {
      std::cout << "[PyMeshSimOptionsNew] ERROR: Can't allocate type." << std::endl;
      return nullptr;
  }
  return (PyObject *) self;
}

//-------------------------
// PyMeshSimOptionsDealloc
//-------------------------
//
static void
PyMeshSimOptionsDealloc(PyMeshingMeshSimOptions* self)
{
  std::cout << "[PyMeshSimOptionsDealloc] Free PyMeshSimOptions" << std::endl;
  Py_TYPE(self)->tp_free(self);
}

//-----------------------------
// SetMeshSimOptionsTypeFields
//-----------------------------
// Set the Python type object fields that stores loft option data.
//
static void
SetMeshSimOptionsTypeFields(PyTypeObject& meshingOpts)
 {
  meshingOpts.tp_doc = MeshSimOptionsClass_doc;
  meshingOpts.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  meshingOpts.tp_dict = PyDict_New();
  meshingOpts.tp_new = PyMeshSimOptionsNew;
  meshingOpts.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  meshingOpts.tp_init = (initproc)PyMeshSimOptionsInit;
  meshingOpts.tp_dealloc = (destructor)PyMeshSimOptionsDealloc;
  meshingOpts.tp_methods = PyMeshSimOptionsMethods;
  meshingOpts.tp_members = PyMeshSimOptionsMembers;
  meshingOpts.tp_getset = PyMeshSimOptionsGetSets;
};

//------------------------
// SetMeshingOptionsTypes
//------------------------
// Set the  loft optinnames in the MeshingOptionsType dictionary.
//
// These are for read only attibutes.
//
static void
SetMeshSimOptionsTypes(PyTypeObject& meshingOptsType)
{
/*
  std::cout << "=============== SetMeshingOptionsClassTypes ==========" << std::endl;

  //PyDict_SetItemString(meshingOptsType.tp_dict, "num_pts", PyLong_AsLong(10));

  PyObject *o = PyLong_FromLong(1);
  PyDict_SetItemString(meshingOptsType.tp_dict, "num_pts", o);

  //PyDict_SetItem(meshingOptsType.tp_dict, "num_pts", o);

  std::cout << "[SetMeshingOptionsClassTypes] Done! " << std::endl;
*/

};

//--------------------------
// CreateMeshSimOptionsType
//--------------------------
//
static PyObject *
CreateMeshSimOptionsType(PyObject* args, PyObject* kwargs)
{
  return PyObject_Call((PyObject*)&PyMeshSimOptionsType, args, kwargs);
}


#endif

