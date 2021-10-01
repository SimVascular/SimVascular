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
#include "sv4gui_ROMSimulationPythonConvert.h"
#include "sv4gui_ROMSimulationView.h"
#include "sv4gui_ConvertProcessHandlerROM.h"
#include "sv4gui_ConvertWorkerROM.h"

#include <mitkLogMacros.h>

#include <QMessageBox>
#include <QProcess>
#include <QObject>

#include <vtkXMLPolyDataWriter.h>

// Redefine MITK_INFO to deactivate all of the debugging statements.
#define MITK_INFO MITK_DEBUG

//-------------
// Constructor
//-------------
sv4guiROMSimulationPythonConvert::sv4guiROMSimulationPythonConvert()
{

}

//------------
// Destructor
//------------
sv4guiROMSimulationPythonConvert::~sv4guiROMSimulationPythonConvert()
{
}

//----------------
// ConvertResults
//----------------
// Convert ROM solver results into a general format for plotting. 
//
// This uses CPython PyImport_Import() and PyObject_Call() API functioins to 
// load the 'm_PythonModuleName' module and execute the 'run_from_c()' function.
//
bool sv4guiROMSimulationPythonConvert::ConvertResults(const std::string outputDirectory)
{
  std::string msg = "[sv4guiROMSimulationPythonConvert::ConvertResults] ";
  sv4guiROMSimulationPythonConvertParamNames paramNames;

  // Import the convert 1D solver results module.
  //
  auto pyName = PyUnicode_DecodeFSDefault((char*)m_PythonModuleName.c_str());
  auto pyModule = PyImport_Import(pyName);

  if (pyModule == nullptr) {
      auto msg = "Unable to load the Python '" + QString(m_PythonModuleName.c_str()) + "' module.";
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
      auto msg = "Can't find the function '" + QString(pyFuncName) + "' in the '" + QString(m_PythonModuleName.c_str()) + "' module.";
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
          MITK_WARN << "Converting reduced-order results files has failed.";
          MITK_WARN << "Returned message: " << QString(sResult.c_str()); 
          QMessageBox mb(nullptr);
          mb.setWindowTitle(sv4guiROMSimulationView::MsgTitle);
          mb.setText("Converting reduced-order results files has failed.");
          mb.setIcon(QMessageBox::Critical);
          mb.setDetailedText(QString(sResult.c_str()));
          mb.setDefaultButton(QMessageBox::Ok);
          mb.exec();
      } else {
          QString rmsg = "Reduced-order solver files have been successfully converted.\n";
          MITK_INFO << msg << rmsg;
          QMessageBox::information(NULL, sv4guiROMSimulationView::MsgTitle, rmsg);
      }
  }

  return SV_OK;
}

//----------------------
// ConvertResultsWorker
//----------------------
// Set the data and event connections for a sv4guiConvertWorkerROM object
// used to convert ROM simulation results in a QThread.
//
// Note: This was hanging SV on Ubuntu 20 when importing scipy.
// The code is not used but keep it around just in case it might be useful.
//
bool sv4guiROMSimulationPythonConvert::ConvertResultsWorker(sv4guiConvertWorkerROM* convertWorker, const std::string outputDirectory)
{
  // Set work values.
  convertWorker->SetModuleName(m_PythonModuleName);
  convertWorker->SetParameterValues(m_ParameterValues);
  convertWorker->SetOutputDirectory(outputDirectory);

  // Create a thread.
  auto thread = new QThread();
  convertWorker->SetThread(thread);

  // Connect thread and sv4guiConvertWorkerROM events with callbacks.
  QObject::connect(thread, &QThread::started, convertWorker, &sv4guiConvertWorkerROM::convertResults);
  QObject::connect(convertWorker, &sv4guiConvertWorkerROM::finished, thread, &QThread::quit);
  QObject::connect(thread, &QThread::finished, thread, &QThread::deleteLater);

  // Move the sv4guiConvertWorkerROM to the thread and start it.
  convertWorker->moveToThread(thread);
  thread->start();
}

//-----------------------
// ConvertResultsProcess
//-----------------------
// Convert ROM solver results into a general format for plotting. 
//
// This executes the Python script as a separate process using the Python interpreter. 
//
// Note: This did not work because VTK could not be successfully imported in Python.
// The code is not used but keep it around just in case it might be useful. This 
// might could work by calling '$SV_HOME/simvascular --python -- convert.py' where
// 'convert.py' is a Python script that like this
//
//   from sv_rom_extract_results import *
//   extract_results.run(args)
//
bool sv4guiROMSimulationPythonConvert::ConvertResultsProcess(const std::string outputDirectory)
{
  sv4guiROMSimulationPythonConvertParamNames params;

  auto resultsDir = m_ParameterValues[params.RESULTS_DIRECTORY];
  auto outDir = m_ParameterValues[params.OUTPUT_DIRECTORY];

  auto pyName = PyUnicode_DecodeFSDefault((char*)m_PythonModuleName.c_str());
  auto pyModule = PyImport_Import(pyName);

  // Create a process and set program to execute and the directory it will execute from.
  QProcess* convertProcess = new QProcess();
  convertProcess->setWorkingDirectory(QString(resultsDir.c_str()));

  // Add environtment variables.
  //
  auto convertProcessEnv = QProcessEnvironment::systemEnvironment();

  auto envPythonHome = std::getenv("PYTHONHOME");
  convertProcessEnv.insert("PYTHONHOME", envPythonHome); 

  std::string programName = std::string(envPythonHome) + "/bin/python";
  convertProcess->setProgram(QString(programName.c_str()));

  auto envPythonPath = std::getenv("PYTHONPATH");
  convertProcessEnv.insert("PYTHONPATH", envPythonPath); 

  auto envLibPath = std::getenv("DYLD_LIBRARY_PATH");
  convertProcessEnv.insert("DYLD_LIBRARY_PATH", envLibPath); 

  convertProcess->setProcessEnvironment(convertProcessEnv);

  // Create the arguments for Python script.
  //
  // Arguments have the format 
  //   --NAME VALUE
  //
  std::string scriptName = "sv_rom_extract_results.extract_results";
  QStringList arguments;
  arguments << "-m";
  arguments << QString(scriptName.c_str());

  for (auto const& param : m_ParameterValues) {
      // Set the argument name.
      auto conv_arg = "--" + QString(param.first.c_str()).replace("_", "-");
      arguments << conv_arg; 

      // Set the argument value.
      //
      auto arg_value = param.second;

      // Time range format is start,stop so we need to quote it.
      if (conv_arg == "--time-range") {
          arg_value = "\"" + arg_value + "\"";
      }

      // Check for Boolean values. Boolean true/false are not passed; 
      // just the presence of the argumnet sets it to true.
      if (arg_value == "false") { 
          continue;
      }
      if (arg_value != "true") { 
          arguments << QString(arg_value.c_str());
      }
  }
  convertProcess->setArguments(arguments);

  int startStep = 0;
  int totalSteps = 2000;

  auto handler = new sv4guiConvertProcessHandlerROM(convertProcess, startStep, totalSteps, QString(resultsDir.c_str()), nullptr);
  handler->Start();

  return true;
}

//--------------
// StartCommand 
//--------------
// Start a command.
//
std::string sv4guiROMSimulationPythonConvert::StartCommand()
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
std::string sv4guiROMSimulationPythonConvert::AddArgument(const std::string& name, const std::string& value, bool last)
{
    std::string arg = name + "='" + value + "'";
    if (last)  {
        arg += ")\n";
    } else {
        arg += ",";
    }
    return arg;
}

bool sv4guiROMSimulationPythonConvert::AddParameter(const std::string& name, const std::string& value)
{
    m_ParameterValues.insert(std::pair<std::string,std::string>(name, value));
    return SV_OK;
}
