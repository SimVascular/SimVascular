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
#include "sv4gui_MachineLearningUtils.h"
#include <sv4gui_json.hxx>
#include <iostream>

using json = nlohmann::json;

//see https://stackoverflow.com/questions/16777126/pyobject-callmethod-with-keyword-arguments
//for some examples of calling python code from C++

sv4gui_MachineLearningUtils* sv4gui_MachineLearningUtils::instance = 0;

/**
 * Function to get an instance of the machine learning module.
 *
 * This function helps to prevent reloading the ML module if it has already been loaded
 *
 * @param network_type Name of the desired neural network to use (see sv_ml python ML code)
 * @return the machine learning module instance
 */
sv4gui_MachineLearningUtils* sv4gui_MachineLearningUtils::getInstance(std::string network_type){
  if (instance == 0){
    instance = new sv4gui_MachineLearningUtils(network_type);
  }
  return instance;
}

/**
 * Function to interface between C++ and sv_ml python code
 *
 * This function mostly just loads the sv_wrapper modulem and creates an SVWrapper object
 *
 * @param network_type Name of the desired neural network to use (see sv_ml python ML code)
 */
sv4gui_MachineLearningUtils::sv4gui_MachineLearningUtils(std::string network_type){

  //Py_Initialize();
  PyRun_SimpleString("import sys");
  // PyRun_SimpleString("sys.path.append(\"/home/marsdenlab/projects/SV/SimVascular-Plugin-MachineLearning/build/lib/python_packages\")");
  // PyRun_SimpleString("sys.path.append(\"/home/marsdenlab/projects/SV/SimVascular-Plugin-MachineLearning/build/lib/seg_regression\")");

  PyRun_SimpleString("print(sys.path)");
  PyRun_SimpleString("print(sys.version)");

  py_wrapper_mod = PyImport_ImportModule("sv_ml.sv_wrapper");
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

/**
 * Destructor to properly clean up references to the python objects
 */
sv4gui_MachineLearningUtils::~sv4gui_MachineLearningUtils(){
  std::cout << "sv4gui_MachineLearningUtils destructor\n";
  Py_DECREF(py_wrapper_mod);
  Py_DECREF(py_wrapper_class);
  Py_DECREF(py_wrapper_inst);
  //Py_Finalize();
}

/**
 * Function to tell the sv_ml python code which image volume to use.
 *
 * This function points the SVWrapper class to the correct medical image volume file
 *
 * @param image_path filename of the image volume to be segmented
 * @return cstr string indicating successful call of the function
 */
std::string sv4gui_MachineLearningUtils::setImage(std::string image_path){
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

/**
 * Function to use the python SVWrapper object to segment a path point.
 *
 * This function constructs a JSON object containing the path point information
 * and then passes that to the SVWrapper python object where it is segmented
 * using the earlier loaded neural network.
 *
 * @param path_point an instance of a SimVascular PathPoint object
 * @return points, a vector of vectors of doubles indicating the segmented contour
 */
std::vector<std::vector<double>> sv4gui_MachineLearningUtils::segmentPathPoint(sv4guiPathElement::sv4guiPathPoint path_point){

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

  //std::cout << "result " << result << "\n";

  if (result == ""){
    std::cout << "Error segmenting, sv_wrapper returned null\n";
    return empty_points;
  }

  json result_json = json::parse(result);
  std::vector<std::vector<double>> points = result_json["points"];

  return points;
}

/**
 * Function to sample a new dropout mask for the network (currently unused).
 */
void sv4gui_MachineLearningUtils::sampleNetwork(){
  PyObject* py_res = PyObject_CallMethod(py_wrapper_inst, "sample", "");

  if (py_res == NULL){
    std::cout << "Error calling sv_wrapper.sample\n";
  }
}
