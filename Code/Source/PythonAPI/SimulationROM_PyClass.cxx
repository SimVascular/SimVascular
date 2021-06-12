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

// The functions defined here implement the SV Python API 'simulation' module 'ROMensional' class. 
//
//     oneD_sim = simulation.ROM()

#include "sv4gui_ROMSimulationPython.h"

#include <map>
#include <fstream>

//-----------------
// PySimulationROM 
//-----------------
// Define the SV Python simulation.ROM class.
//
typedef struct
{
  PyObject_HEAD
  PyObject* parameters;
} PySimulationROM;

#include "SimulationROMParams_PyClass.cxx"

//////////////////////////////////////////////////////
//          U t i l i t y  F u n c t i o n s        //
//////////////////////////////////////////////////////

namespace ROMSim_Parameters {

  // The names here primarily identify the attributes for the
  // various classes in 
  //
  //    SimVascular/Python/site-packages/sv/simulation_parameters.py
  
  // BoundaryConditions class
  //
  static const char* BOUNDARY_CONDITION_LIST = "bc_list";
  // Fields in the BC dict.
  static const char* BOUNDARY_CONDITION_TYPE = "type";
  static const char* BOUNDARY_CONDITION_FACE_ID = "faceID";
  static const char* BOUNDARY_CONDITION_RESISTANCE_VALUE = "resistance";
  static const char* BOUNDARY_CONDITION_RCR_RP = "Rp";
  static const char* BOUNDARY_CONDITION_RCR_C = "C";
  static const char* BOUNDARY_CONDITION_RCR_RD = "Rd";
  static const char* BOUNDARY_CONDITION_FILE_NAME = "file_name";
  static const char* BOUNDARY_CONDITION_RESISTANCE_FILE_NAME = "resistance.dat";
  static const char* BOUNDARY_CONDITION_RCR_FILE_NAME = "rcrt.dat"; 
  // BC type.
  static const char* BOUNDARY_CONDITION_PRESCRIBED_VELOCITIES = "Prescribed Velocities";
  static const char* BOUNDARY_CONDITION_RESISTANCE = "Resistance";
  static const char* BOUNDARY_CONDITION_RCR = "RCR";

  // FluidProperties class.
  //
  static const char* FLUID_DENSITY = "density";
  static const char* FLUID_VISCOSITY = "viscosity";

  // OlufsenMaterial class.
  static const char* MATERIAL_EH_R = "eh_r";
  static const char* MATERIAL_EXPONENT = "exponent";
  static const char* MATERIAL_NAME = "name";
  static const char* MATERIAL_K1 = "k1";
  static const char* MATERIAL_K2 = "k2";
  static const char* MATERIAL_K3 = "k3";
  static const char* MATERIAL_PRESSURE = "pressure";

  // WallProperties class.
  static const char* MATERIAL_LINEAR = "LINEAR";
  static const char* MATERIAL_OLUFSEN = "OLUFSEN";

  // MeshParameters class.
  static const char* MESH_ELEMENT_SIZE = "element_size";
  static const char* MESH_NUM_BRANCH_SEGS = "num_branch_segments";
  static const char* MESH_USE_ADAPTIVE = "use_adaptive_meshing";

  // ModelParameters class.
  static const char* MODEL_NAME = "name";
  static const char* MODEL_CENTERLINES_FILE = "centerlines_file_name";
  static const char* MODEL_OUTLET_FACE_FILE_NAME = "outlet_face_names.dat";
  static const char* MODEL_OUTLET_FACE_NAMES = "outlet_face_names";


  // Solution class.
  static const char* SOLUTION_TIME_STEP = "time_step"; 
  static const char* SOLUTION_NUM_TIME_STEPS = "num_time_steps"; 
  static const char* SOLUTION_SAVE_DATA_FREQUENCY = "save_frequency"; 

  static const char* MODEL_ORDER = "model_order";
  static const char* PYTHON_ROM_SIMULATION_MODULE_NAME = "sv_rom_simulation";
  static const char *SOLVER_0D_FILE_NAME = "solver_0d.in";
  static const char *SOLVER_1D_FILE_NAME = "solver_1d.in";
}

//----------------------
// ROMSim_WriteFlowFile
//----------------------
// Copy the input flow file to the output directory.
void
ROMSim_WriteFlowFile(sv4guiROMSimulationPython& pythonInterface, std::vector<std::map<std::string,std::string>>& bcValues, 
    std::string& outputDir)
{
  using namespace ROMSim_Parameters;
  std::map<std::string,std::string> velBcItem;
 
  for (auto bcItem : bcValues) { 
      auto bctype = bcItem[BOUNDARY_CONDITION_TYPE];
      if (bctype == BOUNDARY_CONDITION_PRESCRIBED_VELOCITIES) {
          velBcItem = bcItem;
          break;
      }
  }

  if (velBcItem.size() == 0) {
     throw std::runtime_error("No prescribe velocities boundary condition was found.");
  }

  // Get the inlet velocity file name.
  auto fileName = velBcItem[BOUNDARY_CONDITION_FILE_NAME];
  auto baseFileName = fileName.substr(fileName.find_last_of("/\\")+1);
  auto copyFileName = outputDir + baseFileName; 

  // Copy it to the output directory.
  std::ifstream src(fileName, std::ios::binary);
  std::ofstream dest(copyFileName, std::ios::binary);
  dest << src.rdbuf();

  auto params = pythonInterface.m_ParameterNames;
  pythonInterface.AddParameter(params.INFLOW_INPUT_FILE, copyFileName); 
}

//---------------------
// ROMSim_WriteRCRFile  
//---------------------
//
bool ROMSim_WriteRCRFile(sv4guiROMSimulationPython& pythonInterface, std::vector<std::map<std::string,std::string>>& bcValues,
    std::string& outputDir)
{
  using namespace ROMSim_Parameters;
  std::vector<std::map<std::string,std::string>> rcrBcList;
  std::string fileWritten;

  for (auto bcItem : bcValues) {
      auto bctype = bcItem[BOUNDARY_CONDITION_TYPE];
      if (bctype == BOUNDARY_CONDITION_RCR) {
          rcrBcList.push_back(bcItem);
      }
  }

  if (rcrBcList.size() == 0) {
     return false;
  }

  // Write the rcr values to a file.
  std::string fileName = outputDir + "/" + std::string(BOUNDARY_CONDITION_RCR_FILE_NAME);
  ofstream outs;
  outs.open(fileName, std::ofstream::out);
  if (outs.fail()) {
      throw std::runtime_error("Unable to open the file '" + fileName + "' for writing.");
  }

  // [TODO:DaveP] not sure what these two twos are about but they are 
  // in every rcrt.dat file i've seen.
  //
  outs << 2 << std::endl; 
  for (auto& bcItem : rcrBcList) {
      outs << 2 << std::endl; 
      auto faceID = bcItem[BOUNDARY_CONDITION_FACE_ID];
      auto Rp = bcItem[BOUNDARY_CONDITION_RCR_RP];
      auto C = bcItem[BOUNDARY_CONDITION_RCR_C];
      auto Rd = bcItem[BOUNDARY_CONDITION_RCR_RD];
      outs << faceID <<  std::endl;
      outs << Rp << std::endl; 
      outs << C << std::endl; 
      outs << Rd << std::endl; 
      outs << "0.0 0" << std::endl;
      outs << "1.0 0" << std::endl;
  }
  outs.close();
  return true;
}

//----------------------------
// ROMSim_WriteResistanceFile 
//----------------------------
//
bool ROMSim_WriteResistanceFile(sv4guiROMSimulationPython& pythonInterface, std::vector<std::map<std::string,std::string>>& bcValues,
    std::string& outputDir)
{
  using namespace ROMSim_Parameters;
  std::vector<std::map<std::string,std::string>> resBcList;

  for (auto bcItem : bcValues) {
      auto bctype = bcItem[BOUNDARY_CONDITION_TYPE];
      if (bctype == BOUNDARY_CONDITION_RESISTANCE) {
          resBcList.push_back(bcItem);
      }
  }

  if (resBcList.size() == 0) {
     return false; 
  }

  // Write the resistance values to a file.
  std::string fileName = outputDir + "/" + std::string(BOUNDARY_CONDITION_RESISTANCE_FILE_NAME);
  ofstream outs;
  outs.open(fileName, std::ofstream::out);
  if (outs.fail()) {
      throw std::runtime_error("Unable to open the file '" + fileName + "' for writing.");
  }

  for (auto& bcItem : resBcList) {
      auto faceID = bcItem[BOUNDARY_CONDITION_FACE_ID];
      auto resistance = bcItem[BOUNDARY_CONDITION_RESISTANCE_VALUE];
      outs << faceID << " " << resistance << "\n";
  }
  outs.close();
  return true; 
}

//---------------------------------------
// ROMSim_AddBoundaryConditionParameters 
//---------------------------------------
// Add parameter values from the 'ROMParameters.BoundaryConditions' object.
//
void
ROMSim_AddBoundaryConditionParameters(sv4guiROMSimulationPython& pythonInterface, PyObject* modelObj, std::string& outputDir)
{
  using namespace ROMSim_Parameters;
  auto params = pythonInterface.m_ParameterNames;

  // Get bcs from object.
  auto bcValues = PyUtilGetDictListAttr(modelObj, BOUNDARY_CONDITION_LIST);

  // Check BC types.
  std::set<std::string> validTypes = { BOUNDARY_CONDITION_PRESCRIBED_VELOCITIES, BOUNDARY_CONDITION_RESISTANCE, BOUNDARY_CONDITION_RCR } ;
  for (auto bcItem : bcValues) { 
      auto bcType = bcItem[BOUNDARY_CONDITION_TYPE];
      if (validTypes.find(bcType) == validTypes.end()) {
          throw std::runtime_error("Unknown boundary condition type '" + bcType + "'.");
      }
  }

  std::vector<std::string> filesWritten;

  // Write the inflow BC data.
  ROMSim_WriteFlowFile(pythonInterface, bcValues, outputDir);

  // Write the resistance BC data.
  if (ROMSim_WriteResistanceFile(pythonInterface, bcValues, outputDir)) { 
      filesWritten.push_back(std::string(BOUNDARY_CONDITION_RESISTANCE_FILE_NAME));
  }

  // Write the RCR BC data.
  if (ROMSim_WriteRCRFile(pythonInterface, bcValues, outputDir)) {
      filesWritten.push_back(std::string(BOUNDARY_CONDITION_RCR_FILE_NAME));
  }

  // Set BC file names.
  if (filesWritten.size() != 0) { 
      pythonInterface.AddParameterList(params.OUTFLOW_BC_TYPE, filesWritten);
      pythonInterface.AddParameter(params.UNIFORM_BC, "false");
  }

  pythonInterface.AddParameter(params.OUTFLOW_BC_INPUT_FILE, outputDir);
}

//---------------------------
// ROMSim_AddFluidParameters 
//---------------------------
//
void
ROMSim_AddFluidParameters(sv4guiROMSimulationPython& pythonInterface, PyObject* fluidObj)
{
  using namespace ROMSim_Parameters;
  auto params = pythonInterface.m_ParameterNames;

  auto typeName = PyUtilGetObjectType(fluidObj);

  auto density = PyUtilGetDoubleAttr(fluidObj, FLUID_DENSITY);
  pythonInterface.AddParameter(params.DENSITY, std::to_string(density));

  auto viscosity = PyUtilGetDoubleAttr(fluidObj, FLUID_VISCOSITY);
  pythonInterface.AddParameter(params.VISCOSITY, std::to_string(viscosity));
}

//------------------------------
// ROMSim_AddMaterialParameters 
//------------------------------
// Add material properties to sv4guiROMSimulationPython..
//
void
ROMSim_AddMaterialParameters(sv4guiROMSimulationPython& pythonInterface, PyObject* materialObj)
{
  using namespace ROMSim_Parameters;
  auto params = pythonInterface.m_ParameterNames;

  // Get material name.
  auto nameObj = PyObject_GetAttrString(materialObj, MATERIAL_NAME);
  if (nameObj == nullptr) { 
      throw std::runtime_error("The material model has no 'name' attribute");
  }
  std::string materialName(PyString_AsString(nameObj));

  if (materialName == MATERIAL_OLUFSEN) {
      pythonInterface.AddParameter(params.MATERIAL_MODEL, materialName);

      auto k1 = PyUtilGetDoubleAttr(materialObj, MATERIAL_K1);
      pythonInterface.AddParameter(params.OLUFSEN_MATERIAL_K1, std::to_string(k1));

      auto k2 = PyUtilGetDoubleAttr(materialObj, MATERIAL_K2);
      pythonInterface.AddParameter(params.OLUFSEN_MATERIAL_K2, std::to_string(k2));

      auto k3 = PyUtilGetDoubleAttr(materialObj, MATERIAL_K3);
      pythonInterface.AddParameter(params.OLUFSEN_MATERIAL_K3, std::to_string(k3));

      auto exponent = PyUtilGetDoubleAttr(materialObj, MATERIAL_EXPONENT);
      pythonInterface.AddParameter(params.OLUFSEN_MATERIAL_EXP, std::to_string(exponent));

      auto pressure = PyUtilGetDoubleAttr(materialObj, MATERIAL_PRESSURE);
      pythonInterface.AddParameter(params.OLUFSEN_MATERIAL_PRESSURE, std::to_string(pressure));

  } else if (materialName == MATERIAL_LINEAR) {
      auto eh_r = PyUtilGetDoubleAttr(materialObj, MATERIAL_EH_R);
      auto pressure = PyUtilGetDoubleAttr(materialObj, MATERIAL_PRESSURE);
      pythonInterface.AddParameter(params.LINEAR_MATERIAL_EHR, std::to_string(eh_r));
      pythonInterface.AddParameter(params.LINEAR_MATERIAL_PRESSURE, std::to_string(pressure));

  } else {
      throw std::runtime_error("Unknown material model '" + std::string(materialName) + "'.");
  }

}

//--------------------------
// ROMSim_AddMeshParameters
//--------------------------
//
void
ROMSim_AddMeshParameters(sv4guiROMSimulationPython& pythonInterface, PyObject* meshObj)
{
  using namespace ROMSim_Parameters;
  auto params = pythonInterface.m_ParameterNames;

  auto element_size = PyUtilGetDoubleAttr(meshObj, MESH_ELEMENT_SIZE);
  pythonInterface.AddParameter(params.ELEMENT_SIZE, std::to_string(element_size));

  auto numBranchSegements = PyUtilGetIntAttr(meshObj, MESH_NUM_BRANCH_SEGS);
  pythonInterface.AddParameter(params.SEG_MIN_NUM, std::to_string(numBranchSegements));

  auto adaptMeshing = PyUtilGetBoolAttr(meshObj, MESH_USE_ADAPTIVE);
  pythonInterface.AddParameter(params.SEG_SIZE_ADAPTIVE, std::to_string(adaptMeshing));
}

//---------------------------
// ROMSim_AddModelParameters 
//---------------------------
// Add parameter values from the 'ROMParameters.ModelParameters' object.
//
void
ROMSim_AddModelParameters(sv4guiROMSimulationPython& pythonInterface, PyObject* modelObj, std::string& outputDir)
{
  using namespace ROMSim_Parameters;
  auto params = pythonInterface.m_ParameterNames;

  auto modelName = PyUtilGetStringAttr(modelObj, MODEL_NAME);
  pythonInterface.AddParameter(params.MODEL_NAME, modelName);

  auto centerLinesFilelName = PyUtilGetStringAttr(modelObj, MODEL_CENTERLINES_FILE);
  pythonInterface.AddParameter(params.CENTERLINES_INPUT_FILE, centerLinesFilelName);

  // Write outlet face names.
  //
  auto outletFaceNames = PyUtilGetStringListAttr(modelObj, MODEL_OUTLET_FACE_NAMES);
  std::string fileName = outputDir + "/" + std::string(MODEL_OUTLET_FACE_FILE_NAME);
  ofstream outs;
  outs.open(fileName, std::ofstream::out);
  if (outs.fail()) {
      throw std::runtime_error("Unable to open the file '" + fileName + "' for writing.");
  }
  for (auto const& faceName : outletFaceNames) {
      outs << faceName << "\n";
  }
  outs.close();
  pythonInterface.AddParameter(params.OUTLET_FACE_NAMES_INPUT_FILE, fileName);
}

//-----------------------------
//ROMSim_AddSolutionParameters 
//-----------------------------
// Add solution paramaters. 
//
void
ROMSim_AddSolutionParameters(sv4guiROMSimulationPython& pythonInterface, PyObject* solutionObj, const int modelOrder)
{
  using namespace ROMSim_Parameters;
  auto params = pythonInterface.m_ParameterNames;

  // Add the name of the solver input file.
  if (modelOrder == 0)
	  pythonInterface.AddParameter(params.SOLVER_OUTPUT_FILE, ROMSim_Parameters::SOLVER_0D_FILE_NAME);
  else if (modelOrder == 1)
	  pythonInterface.AddParameter(params.SOLVER_OUTPUT_FILE, ROMSim_Parameters::SOLVER_1D_FILE_NAME);

  auto numTimeSteps = PyUtilGetIntAttr(solutionObj, SOLUTION_NUM_TIME_STEPS);
  pythonInterface.AddParameter(params.NUM_TIME_STEPS, std::to_string(numTimeSteps));

  auto timeStep = PyUtilGetDoubleAttr(solutionObj, SOLUTION_TIME_STEP);
  pythonInterface.AddParameter(params.TIME_STEP, std::to_string(timeStep));

  auto saveFreq = PyUtilGetIntAttr(solutionObj, SOLUTION_SAVE_DATA_FREQUENCY);
  pythonInterface.AddParameter(params.SAVE_DATA_FREQUENCY, std::to_string(saveFreq));
}

//----------------------------
// ROMSim_GenerateSolverInput 
//----------------------------
// Generate a ROM solver input file. 
//
// This is similar to the sv4guiROMSimulationPython::GenerateSolverInput() method.
//
void
ROMSim_GenerateSolverInput(sv4guiROMSimulationPython& pythonInterface, std::string& outputDir)
{
  using namespace ROMSim_Parameters;
  auto params = pythonInterface.m_ParameterNames;

  // Import the ROM mesh generation module.
  //
  auto pyName = PyUnicode_DecodeFSDefault(PYTHON_ROM_SIMULATION_MODULE_NAME);
  auto pyModule = PyImport_Import(pyName);

  if (pyModule == nullptr) {
      throw std::runtime_error("Unable to load the Python '" + std::string(PYTHON_ROM_SIMULATION_MODULE_NAME) + "' module.");
  }

  // Get the module interface function that executes 
  // module functions based on input arguments. 
  //
  auto pyFuncName = (char*)"run_from_c";
  auto pyDict = PyModule_GetDict(pyModule);
  auto pyFunc = PyDict_GetItemString(pyDict, (char*)pyFuncName);

  if (!PyCallable_Check(pyFunc)) {
      throw std::runtime_error("Can't find the function '" + std::string(pyFuncName) + "' in the '" + 
        std::string(PYTHON_ROM_SIMULATION_MODULE_NAME) + "' module.");
  }

  // Create an argument containing the output directory.
  // This is used to write a script log file to the
  // solver job directory.
  //
  auto dummyArgs = PyTuple_New(1);
  auto dummyValue = PyUnicode_DecodeFSDefault(outputDir.c_str());
  PyTuple_SetItem(dummyArgs, 0, dummyValue);

  // Create the **kwarg arguments that are the input arguments to the module.
  //
  auto kwargs = PyDict_New();
  for (auto const& param :  pythonInterface.m_ParameterValues) {
      PyDict_SetItemString(kwargs, param.first.c_str(), PyUnicode_DecodeFSDefault(param.second.c_str()));
  }

  // Execute the Python script.
  //
  auto result = PyObject_Call(pyFunc, dummyArgs, kwargs);

  // Check for errors.
  PyErr_Print();

  // If the solver input file has been successfully generated then
  // show the number of nodes, segments and elements generated.
  // Otherwise display error messages and the script log file.
  //
  // Search for the Python logger ERROR or WARNING messages in the 
  // returned result to determine if the script failed.
  //
  if (result) {
      auto uResult = PyUnicode_FromObject(result);
      auto sResult = std::string(PyUnicode_AsUTF8(uResult));

      if (sResult.find("ERROR") != std::string::npos) {
          std::string msg = "The generation of the solver input file failed. ";
          msg += "Returned message: " + sResult;
          throw std::runtime_error(msg);

      } else if (sResult.find("WARNING") != std::string::npos) {
          std::string msg = "A solver input file has been generated with warnings.\n";
          std::cout << msg << std::endl; 

      // Display mesh information.
      //
      } else {
          std::string token;
          std::istringstream tokenStream(sResult);
          char delimiter = '\n';
          int num_nodes, num_elems, num_segs;
          bool meshInfoFound = false;
          while (std::getline(tokenStream, token, delimiter)) {
             if (sscanf(token.c_str(), "Mesh: num_nodes=%d num_elements=%d num_segs=%d", &num_nodes, &num_elems, &num_segs)) {
                 meshInfoFound = true;
                 break;
             }
          }

          if (meshInfoFound) {
              std::cout << "A solver input file has been successfully generated." << std::endl; 
              std::cout << "Number of segments: " << std::to_string(num_segs) << std::endl;
              std::cout << "Number of nodes: " << std::to_string(num_nodes) << std::endl;
              std::cout << "Number of elements: " << std::to_string(num_elems) << std::endl;
          }
      }
  }

}

//////////////////////////////////////////////////////
//          C l a s s   M e t h o d s               //
//////////////////////////////////////////////////////
//
// Python 'ROM' class methods.

//----------------------------
// ROMSim_write_input_file 
//----------------------------
// This method uses a sv4guiROMSimulationPython() object to collect
// parameter values and is similar to sv4guiROMSimulationView::CreateDataFiles().
//
PyDoc_STRVAR(ROMSim_write_input_file_doc,
  "write_input_file(model, mesh, fluid, material, boundary_conditions, directory) \n\
   \n\
   Write the ROM simulation solver input file.                                     \n\
   \n\
   Args: \n\
     model (ModelParameters): The model parameters.                               \n\
");

static PyObject *
ROMSim_write_input_file(PySimulationROM* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("O!OOOOOOs", PyRunTimeErr, __func__);
  static char *keywords[] = {"model_order", "model", "mesh", "fluid", "material", "boundary_conditions", "solution", "directory", NULL};
  PyObject* modelOrderArg = 0;
  PyObject* modelParamsArg = nullptr;
  PyObject* meshParamsArg = nullptr;
  PyObject* fluidPropsArg = nullptr;
  PyObject* materialModelArg = nullptr;
  PyObject* bcsParamsArg = nullptr;
  PyObject* solutionParamsArg = nullptr;
  char* outputDirArg = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyInt_Type, &modelOrderArg, &modelParamsArg, &meshParamsArg, 
        &fluidPropsArg, &materialModelArg, &bcsParamsArg, &solutionParamsArg, &outputDirArg)) {
      return api.argsError();
  }

  // Create the 'sv4guiROMSimulationPython' object used to Set the 
  // parameters used by the Python script.
  auto pythonInterface = sv4guiROMSimulationPython();
  auto params = pythonInterface.m_ParameterNames;

  // Set model order.
  //
  int modelOrder = PyInt_AsLong(modelOrderArg);
  if (PyErr_Occurred()) {
      return nullptr;
  }
  if ((modelOrder != 0) && (modelOrder != 1)) {
      api.error("The 'model_order' argument '" + std::to_string(modelOrder) + "' is not valid. Valid model orders are 0 or 1.");
      return nullptr;
  }
  pythonInterface.AddParameter(params.MODEL_ORDER, std::to_string(modelOrder));

  // Add parameter values from the argument objects.
  //
  std::string outputDir(outputDirArg);
  pythonInterface.AddParameter(params.OUTPUT_DIRECTORY, outputDir);
  pythonInterface.AddParameter(params.UNITS, "cm");

  try {

      ROMSim_AddModelParameters(pythonInterface, modelParamsArg, outputDir);
      ROMSim_AddMeshParameters(pythonInterface, meshParamsArg);
      ROMSim_AddFluidParameters(pythonInterface, fluidPropsArg);
      ROMSim_AddMaterialParameters(pythonInterface, materialModelArg);
      ROMSim_AddBoundaryConditionParameters(pythonInterface, bcsParamsArg, outputDir);
      ROMSim_AddSolutionParameters(pythonInterface, solutionParamsArg, modelOrder);
      ROMSim_GenerateSolverInput(pythonInterface, outputDir);

  } catch (const std::exception& exception) {
      api.error(exception.what());
      return nullptr;
  }

  Py_RETURN_NONE;
}


////////////////////////////////////////////////////////
//           C l a s s   D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* SIMULATION_ROM_CLASS = "ROM";
// Dotted name that includes both the module name and 
// the name of the type within the module.
static char* SIMULATION_ROM_MODULE_CLASS = "simulation.ROM";

//--------------------
// ROMSimClass_doc 
//--------------------
// Define the Fluid class documentation.
//
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(ROMSimClass_doc,
   "The ROM class provides methods for                                       \n\
   \n\
");

//--------------------
// PyROMSimMethods 
//--------------------
// Fluid class methods.
//
static PyMethodDef PyROMSimMethods[] = {

  {"write_input_file", (PyCFunction)ROMSim_write_input_file, METH_VARARGS|METH_KEYWORDS, ROMSim_write_input_file_doc},

  {NULL,NULL}
};

static PyMemberDef PyROMSimMembers[] = {
    {"parameters", T_OBJECT_EX, offsetof(PySimulationROM, parameters), 0, NULL},
    {NULL}
};

//----------------
// PyROMSimType 
//----------------
// Define the Python type object that stores Fluid data. 
//
// Can't set all the fields here because g++ does not suppor non-trivial 
// designated initializers. 
//
PyTypeObject PySimulationROMType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  // Dotted name that includes both the module name and 
  // the name of the type within the module.
  SIMULATION_ROM_MODULE_CLASS, 
  sizeof(PySimulationROM)
};

//----------------
// PyROMSimInit
//----------------
// This is the __init__() method for the Fluid class. 
//
// This function is used to initialize an object after it is created.
//
static int
PyROMSimInit(PySimulationROM* self, PyObject* args, PyObject *kwds)
{
  static bool initParams = true;

  if (initParams) { 
      SetPyROMSimParamsTypeFields(PySimulationROMParametersType);
      if (PyType_Ready(&PySimulationROMParametersType) < 0) {
          fprintf(stdout, "Error initilizing PySimulationROMParametersType \n");
          return -1;
      }
      initParams = false;
  }

  self->parameters = PyObject_CallObject((PyObject*)&PySimulationROMParametersType, NULL);

  return 0;
}

//---------------
// PyROMSimNew 
//---------------
// Object creation function, equivalent to the Python __new__() method. 
// The generic handler creates a new instance using the tp_alloc field.
//
static PyObject *
PyROMSimNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  auto self = (PySimulationROM*)type->tp_alloc(type, 0);
  if (self != NULL) {
  }

  return (PyObject *) self;
}

//--------------------
// PyROMSimDealloc 
//--------------------
//
static void
PyROMSimDealloc(PySimulationROM* self)
{
  Py_TYPE(self)->tp_free(self);
}

//--------------------------
// SetPyROMSimTypeFields 
//--------------------------
// Set the Python type object fields that stores ROM data. 
//
// Need to set the fields here because g++ does not suppor non-trivial 
// designated initializers. 
//
static void
SetPyROMSimTypeFields(PyTypeObject& romSimType)
{
  // Doc string for this type.
  romSimType.tp_doc = ROMSimClass_doc; 
  // Object creation function, equivalent to the Python __new__() method. 
  // The generic handler creates a new instance using the tp_alloc field.
  romSimType.tp_new = PyROMSimNew;
  romSimType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  romSimType.tp_init = (initproc)PyROMSimInit;
  romSimType.tp_dealloc = (destructor)PyROMSimDealloc;
  romSimType.tp_methods = PyROMSimMethods;
  romSimType.tp_members = PyROMSimMembers;
}

