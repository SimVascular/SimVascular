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

//----------------------
// ConvertResultsWorker
//----------------------
// Set the data and event connections for a sv4guiConvertWorkerROM object
// used to convert ROM simulation results in a QThread.
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
// The code is not used but keep it around just in case it might be useful.
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
