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

// The sv4guiROMSimulationPython class methods defined here are primarily used to 
// execute 0D and 1D simulation Python scripts.
//
// The 'GenerateSolverInput()' method generates a 1D solver input file. This file
// is also used by the 0D solver.
//

#include <Python.h>

#include <map>
#include "sv4gui_ROMSimulationPython.h"
#include "sv4gui_ROMSimulationView.h"
#include <mitkLogMacros.h>
#include <QMessageBox>

#include <vtkXMLPolyDataWriter.h>

//-------------
// Constructor
//-------------
sv4guiROMSimulationPython::sv4guiROMSimulationPython()
{

}

//------------
// Destructor
//------------
sv4guiROMSimulationPython::~sv4guiROMSimulationPython()
{
}

//--------------
// GenerateMesh   
//--------------
// Generate a mesh. 
//
// The mesh is the centerlines geometry computed for an SV model (polygonal surface).
//
// Script arguments: 
//
//     output-directory: The output directory to write the mesh file.
//     centerlines-input-file: The input centerlines geometry (.vtp format). 
//     compute-mesh: Switch to enable computing just the mesh.
//     write-mesh-file: Switch to enable writing a mesh file.
//     mesh-output-file: The name of the mesh file to write (.vtp format).
//
bool sv4guiROMSimulationPython::GenerateMesh(const std::string& outputDir, const std::string& centerlinesFile,
                                             const std::string& meshFile) 
{
  std::string msg = "[sv4guiROMSimulationPython::GenerateMesh] ";
  MITK_INFO << msg << "---------- GenerateMesh ----------";
  sv4guiROMSimulationPythonParamNames paramNames;

  // Create the script command.
  auto last = true;
  auto cmd = StartCommand();
  cmd += AddArgument(paramNames.OUTPUT_DIRECTORY, outputDir);
  cmd += AddArgument(paramNames.CENTERLINES_INPUT_FILE, centerlinesFile);
  cmd += AddArgument(paramNames.MESH_OUTPUT_FILE, meshFile, last);
  MITK_INFO << msg << "Execute cmd " << cmd;
  //PyRun_SimpleString(cmd.c_str());
  MITK_INFO << msg << "Done!";

  PyObject *pName, *pModule;

  //std::string cmd = "import " + pythonModuleName + "\n";
  //cmd += pythonModuleName + ".run(";

  pName = PyUnicode_DecodeFSDefault(m_PythonROMSimulationModuleName.c_str());

  pModule = PyImport_Import(pName);
  Py_DECREF(pName);

  if (pModule != NULL) {
      MITK_INFO << msg << "Module is not null";
  }
  return SV_OK;
}

//---------------------
// GenerateSolverInput
//---------------------
// Generate a 1D solver input file.
//
// The solver input file is used by both the 0D and 1D solvers.
//
// The 'run_from_c' function, defined in 'generate_1d_mesh.py', is called
// with args and kwargs function arguments. 
//
// Example script arguments: 
//
//    python generate_1d_mesh.py \
//        --output-directory $PWD/output \
//        --units mm \
//        --element_size 0.01 \
//        --centerlines-input-file ${cl_file} \
//        --outlet-face-names-input-file ${outlet_face_names_file} \
//        --uniform-bc false \
//        --inflow-input-file ${inflow_file} \
//        --outflow-bc-type rcr \
//        --outflow-bc-input-file ${outflow_bc_input_file} \
//        --write-solver-file   \
//        --solver-output-file solver.in
//
//     output-directory: The output directory to write the mesh file.
//     centerlines-input-file: The input centerlines geometry (.vtp format). 
//     write-solver-file: Switch to enable writing a solver file.
//     solver-output-file: The name of the solver input file to write.
//
bool sv4guiROMSimulationPython::GenerateSolverInput(const std::string outputDirectory, const sv4guiROMSimJob* job)
{
  std::string msg = "[sv4guiROMSimulationPython::GenerateSolverInput] ";
  MITK_INFO << msg << "---------- GenerateSolverInput ----------";
  sv4guiROMSimulationPythonParamNames paramNames;

  // Import the 1D mesh generation module.
  //
  auto pyName = PyUnicode_DecodeFSDefault((char*)m_PythonROMSimulationModuleName.c_str());
  auto pyModule = PyImport_Import(pyName);

  if (pyModule == nullptr) {
      auto msg = "Unable to load the Python '" + QString(m_PythonROMSimulationModuleName.c_str()) + "' module.";
      MITK_ERROR << msg;
      QMessageBox::warning(NULL, sv4guiROMSimulationView::MsgTitle, msg);
      return false;
  } 

  // Get the module interface function that executes 
  // module functions based on input arguments. 
  //
  auto pyFuncName = (char*)"run_from_c";
  auto pyDict = PyModule_GetDict(pyModule);
  auto pyFunc = PyDict_GetItemString(pyDict, (char*)pyFuncName);

  if (!PyCallable_Check(pyFunc)) {
      auto msg = "Can't find the function '" + QString(pyFuncName) + "' in the '" + QString(m_PythonROMSimulationModuleName.c_str()) + "' module.";
      MITK_ERROR << msg;
      QMessageBox::warning(NULL, sv4guiROMSimulationView::MsgTitle, msg);
      return false;
  }

  // Create an argument containing the output directory.
  // This is used to write a script log file to the
  // solver job directory.
  //
  auto dummyArgs = PyTuple_New(1);
  auto dummyValue = PyUnicode_DecodeFSDefault(outputDirectory.c_str());
  PyTuple_SetItem(dummyArgs, 0, dummyValue);

  // Create the **kwarg arguments that are the input arguments to the module.
  //
  MITK_INFO << msg << "Add arguments ... ";
  auto kwargs = PyDict_New();
  for (auto const& param : m_ParameterValues) {
      MITK_INFO << msg << param.first << "   " << "'" << param.second << "'"; 
      PyDict_SetItemString(kwargs, param.first.c_str(), PyUnicode_DecodeFSDefault(param.second.c_str()));
  }
  MITK_INFO << msg << "Done.";

  // Execute the Python script.
  //
  MITK_INFO << msg << "Execute script ...";
  auto result = PyObject_Call(pyFunc, dummyArgs, kwargs);
  MITK_INFO << msg << "Done.";

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
          MITK_WARN << "The generation of the solver input file failed.";
          MITK_WARN << "Returned message: " << QString(sResult.c_str()); 
          QMessageBox mb(nullptr);
          mb.setWindowTitle(sv4guiROMSimulationView::MsgTitle);
          mb.setText("The generation of the solver input file failed.");
          mb.setIcon(QMessageBox::Critical);
          mb.setDetailedText(QString(sResult.c_str()));
          mb.setDefaultButton(QMessageBox::Ok);
          mb.exec();

      } else if (sResult.find("WARNING") != std::string::npos) {
          QString wmsg = "A solver input file has been generated with warnings.\n"; 
          QMessageBox mb(nullptr);
          mb.setWindowTitle(sv4guiROMSimulationView::MsgTitle);
          mb.setText(wmsg);
          mb.setIcon(QMessageBox::Warning);
          mb.setDetailedText(QString(sResult.c_str()));
          mb.setDefaultButton(QMessageBox::Ok);
          mb.exec();

      // Display mesh information.
      //
      } else {
          MITK_INFO << QString(sResult.c_str()); 
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
              QString rmsg = "A solver input file has been successfully generated.\n"; 
              rmsg += "Number of segments: " + QString::number(num_segs) + "\n"; 
              rmsg += "Number of nodes: " + QString::number(num_nodes) + "\n"; 
              rmsg += "Number of elements: " + QString::number(num_elems) + "\n"; 
              MITK_INFO << msg << rmsg; 
              QMessageBox::information(NULL, sv4guiROMSimulationView::MsgTitle, rmsg);
          }
      }
  }

/* [DaveP] This is causing a crash, figure out later.
  Py_DECREF(pyFunc);
  Py_DECREF(pyDict);
  Py_DECREF(pyName);
  Py_DECREF(pyModule);
  Py_DECREF(dummyArgs);
  Py_DECREF(dummyValue);
  Py_DECREF(kwargs);
*/

/*
  // Create the script command.
  auto last = true;
  auto cmd = StartCommand();
  for (auto const& param : m_ParameterValues) {
      cmd += AddArgument(param.first, param.second);
  }
  cmd.pop_back();
  cmd += ")\n";

  MITK_INFO << msg << "Execute cmd " << cmd;
  PyRun_SimpleString(cmd.c_str());
  MITK_INFO << msg << "Done!";
*/
  return SV_OK;
} 

//------------------------
// ExecuteZeroDSimulation
//------------------------
// Execute a 0D simulation.
//
// The svZeroDSolver.py Python script is executed by calling the 'run_from_c' function. 
//
bool sv4guiROMSimulationPython::ExecuteZeroDSimulation(const std::string outputDirectory, const sv4guiROMSimJob* job)
{
  std::string msg = "[sv4guiROMSimulationPython::ExecuteZeroDSimulation] ";
  MITK_INFO << msg << "---------- ExecuteZeroDSimulation ----------";
  sv4guiROMSimulationPythonParamNames paramNames;

  // Import the svZeroDSolver module.
  //
  auto moduleName = m_PythonZeroDSolverModuleName;
  auto pyName = PyUnicode_DecodeFSDefault((char*)moduleName.c_str());
  auto pyModule = PyImport_Import(pyName);

  if (pyModule == nullptr) {
      auto msg = "Unable to load the Python '" + QString(moduleName.c_str()) + "' module.";
      MITK_ERROR << msg;
      QMessageBox::warning(NULL, sv4guiROMSimulationView::MsgTitle, msg);
      return false;
  }

  // Get the module interface function that executes 
  // module functions based on input arguments. 
  //
  auto pyFuncName = (char*)"run_from_c";
  auto pyDict = PyModule_GetDict(pyModule);
  auto pyFunc = PyDict_GetItemString(pyDict, (char*)pyFuncName);

  if (!PyCallable_Check(pyFunc)) {
      auto msg = "Can't find the function '" + QString(pyFuncName) + "' in the '" + QString(moduleName.c_str()) + "' module.";
      MITK_ERROR << msg;
      QMessageBox::warning(NULL, sv4guiROMSimulationView::MsgTitle, msg);
      return false;
  }

  // Create an argument containing the name of the solver.in file. 
  //
  auto fileName = outputDirectory + "/" + m_PythonZeroDSolverFileName;
  auto args = PyTuple_New(1);
  auto argValue = PyUnicode_DecodeFSDefault(fileName.c_str());
  PyTuple_SetItem(args, 0, argValue);

  // Create the **kwarg arguments that are the input arguments to the module.
  //
  // No parameters are currently passed.
  //
  auto kwargs = PyDict_New();

  // Execute the Python script.
  //
  MITK_INFO << msg << "Execute script ...";
  auto result = PyObject_Call(pyFunc, args, kwargs);
  MITK_INFO << msg << "Done.";

  // Check for errors.
  PyErr_Print();

  // Search for the Python ERROR or WARNING messages in the 
  // returned result string to determine if the script failed.
  //
  if (result) {
      auto uResult = PyUnicode_FromObject(result);
      auto sResult = std::string(PyUnicode_AsUTF8(uResult));

      if (sResult.find("ERROR") != std::string::npos) {
          MITK_WARN << "The 0D solver has failed.";
          MITK_WARN << "Returned message: " << QString(sResult.c_str());
          QMessageBox mb(nullptr);
          mb.setWindowTitle(sv4guiROMSimulationView::MsgTitle);
          mb.setText("The 0D solver has failed.");
          mb.setIcon(QMessageBox::Critical);
          mb.setDetailedText(QString(sResult.c_str()));
          mb.setDefaultButton(QMessageBox::Ok);
          mb.exec();
      } else {
          QMessageBox::information(NULL, sv4guiROMSimulationView::MsgTitle, "The 0D solver has completed.");
      }
  }

  return true;
}

//--------------
// StartCommand 
//--------------
// Start a command used to run a script from 'generate_1d_mesh.py'
// using the 'run' function.
//
// Function arguments are added later.
//
std::string sv4guiROMSimulationPython::StartCommand()
{
  std::string cmd = "import " + m_PythonROMSimulationModuleName + "\n";
  cmd += m_PythonROMSimulationModuleName + ".run(";
  return cmd;
}

//-------------
// AddArgument
//-------------
// Add an named argument to a command string executed as a function.
//
std::string sv4guiROMSimulationPython::AddArgument(const std::string& name, const std::string& value, bool last)
{
    std::string arg = name + "='" + value + "'";
    if (last)  {
        arg += ")\n";
    } else {
        arg += ",";
    }
    return arg;
}

//--------------
// AddParameter
//--------------
// Add a parameter with a single value to a list of parameters.
//
bool sv4guiROMSimulationPython::AddParameter(const std::string& name, const std::string& value)
{
    m_ParameterValues.insert(std::pair<std::string,std::string>(name, value));
    return SV_OK;
}

//------------------
// AddParameterList
//------------------
// Add a parameter with a list of values to a list of parameters.
//
bool sv4guiROMSimulationPython::AddParameterList(const std::string& name, const std::vector<std::string>& values)
{
    std::string list = "";
    std::string sep;
    for (auto value : values) {
        list += sep + value;
        sep = ",";
    }
    m_ParameterValues.insert(std::pair<std::string,std::string>(name, list));
    return SV_OK;
}
