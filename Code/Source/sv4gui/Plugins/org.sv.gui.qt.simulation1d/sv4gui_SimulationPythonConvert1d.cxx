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

#include <Python.h>

#include <map>
#include "sv4gui_SimulationPythonConvert1d.h"
#include "sv4gui_SimulationView1d.h"
#include <mitkLogMacros.h>
#include <QMessageBox>

#include <vtkXMLPolyDataWriter.h>

//-------------
// Constructor
//-------------
sv4guiSimulationPythonConvert1d::sv4guiSimulationPythonConvert1d()
{

}

//------------
// Destructor
//------------
sv4guiSimulationPythonConvert1d::~sv4guiSimulationPythonConvert1d()
{
}

//----------------
// ConvertResults
//----------------
// Convert 1D solver results into a general format for plotting. 
//
// Example script arguments: 
//
//    python extract_results.py          \
//      --results-directory ${res_dir}   \
//      --solver-file-name ${file}       \
//      --outlet-segments                \
//      --data-names ${data_names}       \
//      --time-range ${time_range}       \
//      --output-directory ${out_dir}    \
//      --output-file-name ${out_file}   \
//      --output-format ${out_format}
//
bool sv4guiSimulationPythonConvert1d::ConvertResults(const std::string outputDirectory)
{
  std::string msg = "[sv4guiSimulationPythonConvert1d::ConvertResults] ";
  MITK_INFO << msg << "---------- ConvertResults ----------";
  sv4guiSimulationPythonConvert1dParamNames paramNames;

  // Import the convert 1D solver results module.
  //
  auto pyName = PyUnicode_DecodeFSDefault((char*)m_PythonModuleName.c_str());
  auto pyModule = PyImport_Import(pyName);

  if (pyModule == nullptr) {
      auto msg = "Unable to load the Python '" + QString(m_PythonModuleName.c_str()) + "' module.";
      MITK_ERROR << msg;
      QMessageBox::warning(NULL, sv4guiSimulationView1d::MsgTitle, msg);
      return false;
  } 

  // Get the module interface function that executes 
  // module functions based on input arguments. 
  //
  auto pyFuncName = (char*)"run_from_c";
  auto pyDict = PyModule_GetDict(pyModule);
  auto pyFunc = PyDict_GetItemString(pyDict, (char*)pyFuncName);

  if (!PyCallable_Check(pyFunc)) {
      auto msg = "Can't find the function '" + QString(pyFuncName) + "' in the '" + QString(m_PythonModuleName.c_str()) + "' module.";
      MITK_ERROR << msg;
      QMessageBox::warning(NULL, sv4guiSimulationView1d::MsgTitle, msg);
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

  // If the convert results files was not successful then
  // then display error messages and the script log file.
  //
  // Search for the Python logger ERROR or WARNING messages in the 
  // returned result to determine if the script failed.
  //
  if (result) {
      auto uResult = PyUnicode_FromObject(result);
      auto sResult = std::string(PyUnicode_AsUTF8(uResult));

      if (sResult.find("ERROR") != std::string::npos) {
          MITK_WARN << "Converting 1D solver results files has failed.";
          MITK_WARN << "Returned message: " << QString(sResult.c_str()); 
          QMessageBox mb(nullptr);
          mb.setWindowTitle(sv4guiSimulationView1d::MsgTitle);
          mb.setText("Converting 1D solver results files has failed.");
          mb.setIcon(QMessageBox::Critical);
          mb.setDetailedText(QString(sResult.c_str()));
          mb.setDefaultButton(QMessageBox::Ok);
          mb.exec();
      } else {
          QString rmsg = "1D solver files have been successfully converted.\n";
          MITK_INFO << msg << rmsg;
          QMessageBox::information(NULL, sv4guiSimulationView1d::MsgTitle, rmsg);
      }
  }

  return SV_OK;
}

//--------------
// StartCommand 
//--------------
// Start a command.
//
std::string sv4guiSimulationPythonConvert1d::StartCommand()
{
  std::string cmd = "import " + m_PythonModuleName + "\n";
  cmd += m_PythonModuleName + ".run(";
  return cmd;
}

//-------------
// AddArgument
//-------------
// Add an argument to a command string.
//
std::string sv4guiSimulationPythonConvert1d::AddArgument(const std::string& name, const std::string& value, bool last)
{
    std::string arg = name + "='" + value + "'";
    if (last)  {
        arg += ")\n";
    } else {
        arg += ",";
    }
    return arg;
}

bool sv4guiSimulationPythonConvert1d::AddParameter(const std::string& name, const std::string& value)
{
    m_ParameterValues.insert(std::pair<std::string,std::string>(name, value));
    return SV_OK;
}
