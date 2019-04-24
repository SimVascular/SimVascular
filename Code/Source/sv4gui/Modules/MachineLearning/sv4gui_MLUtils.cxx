#include "sv4gui_MLUtils.h"
#include <json.hpp>
#include <iostream>

using json = nlohmann::json;

//see https://stackoverflow.com/questions/16777126/pyobject-callmethod-with-keyword-arguments
//for some examples of calling python code from C++

sv4gui_MLUtils* sv4gui_MLUtils::instance = 0;

sv4gui_MLUtils* sv4gui_MLUtils::getInstance(std::string network_type){
  if (instance == 0){
    instance = new sv4gui_MLUtils(network_type);
  }
  return instance;
}

sv4gui_MLUtils::sv4gui_MLUtils(std::string network_type){

  Py_Initialize();
  PyRun_SimpleString("import sys");
  PyRun_SimpleString("sys.path.append(\"/home/marsdenlab/projects/SV/SimVascular-Plugin-MachineLearning/build/lib/python_packages\")");
  PyRun_SimpleString("sys.path.append(\"/home/marsdenlab/projects/SV/SimVascular-Plugin-MachineLearning/build/lib/seg_regression\")");

  PyRun_SimpleString("print(sys.path)");
  PyRun_SimpleString("print(sys.version)");

  py_wrapper_mod = PyImport_ImportModule("sv_wrapper");
  if (py_wrapper_mod == NULL){
    std::cout << "error failed to import sv_wrapper module\n";
  }

  py_wrapper_class = PyObject_GetAttrString(py_wrapper_mod, "SVWrapper");
  if (py_wrapper_class == NULL){
    std::cout << "error SVWrapper class not loaded\n";
  }

  PyObject* pargs  = Py_BuildValue("(s)", network_type.c_str());
  if (pargs == NULL){
    std::cout << "error SVWrapper args not loaded\n";
  }

  py_wrapper_inst  = PyEval_CallObject(py_wrapper_class, pargs);
  if (py_wrapper_inst == NULL){
    std::cout << "error SVWrapper instance not loaded\n";
  }

  Py_DECREF(pargs);

}

sv4gui_MLUtils::~sv4gui_MLUtils(){
  std::cout << "sv4gui_MLUtils destructor\n";
  Py_DECREF(py_wrapper_mod);
  Py_DECREF(py_wrapper_class);
  Py_DECREF(py_wrapper_inst);
  Py_Finalize();
}

std::string sv4gui_MLUtils::setImage(std::string image_path){
  PyObject* py_res = PyObject_CallMethod(py_wrapper_inst, "set_image",
                        "s", image_path.c_str());

  if (py_res == NULL){
    std::cout << "Error setting image, sv_wrapper returned null\n";
    return NULL;
  }

  char* cstr;
  PyArg_Parse(py_res, "s", &cstr);
  return std::string(cstr);
}

std::vector<std::vector<double>> sv4gui_MLUtils::segmentPathPoint(sv4guiPathElement::sv4guiPathPoint path_point){

  std::vector<std::vector<double>> empty_points;

  auto pos = path_point.pos;
  auto tx  = path_point.tangent;
  auto n   = path_point.rotation;

  json msg;
  msg["p"]     = {pos[0], pos[1], pos[2]};
  msg["tx"]    = {tx[0], tx[1], tx[2]};
  msg["n"]     = {n[0], n[1], n[2]};

  std::string msg_string = msg.dump();

  PyObject* py_res = PyObject_CallMethod(py_wrapper_inst, "segment",
                        "s", msg_string.c_str());

  if (py_res == NULL){
    std::cout << "Error calling sv_wrapper.segment\n";
    return empty_points;
  }

  char* cstr;
  bool parse_success = PyArg_Parse(py_res, "s", &cstr);
  if(!parse_success){
    std::cout << "Error parsing return result of sv_wrapper.segment\n";
    return empty_points;
  }

  std::string result = std::string(cstr);

  std::cout << "result " << result << "\n";

  if (result == ""){
    std::cout << "Error segmenting, sv_wrapper returned null\n";
    return empty_points;
  }

  json result_json = json::parse(result);
  std::vector<std::vector<double>> points = result_json["points"];

  return points;
}

void sv4gui_MLUtils::sampleNetwork(){
  PyObject* py_res = PyObject_CallMethod(py_wrapper_inst, "sample", "");

  if (py_res == NULL){
    std::cout << "Error calling sv_wrapper.sample\n";
  }
}
