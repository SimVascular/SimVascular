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

// The sv4guiConvertWorkerROM class methods defined here are used to convert ROM simulation 
// results in a QThread so not to freeze SimVascular while executing.
//
// The 'sv_rom_extract_results' Python module 'extract_results.py' script is directly exectuted
// using the CPython API PyObject_Call() function.
//
// Because Qt widgets can't be executed in a QThread, QMessage widgets used to display conversion
// success/failure messages must be called outside of a sv4guiConvertWorkerROM object. The
// QMessage widgets are therefore called in sv4guiROMSimulationView::ShowConvertWorkerMessage().
//
// The sv4guiConvertWorkerROM object's 'finished', 'error', and 'showMessage' signals are connected
// to the sv4guiROMSimulationView class 'ShowConvertWorkerMessage', 'ConvertWorkerError' and
// 'ConvertWorkerFinished' methods in sv4guiROMSimulationPythonConvert::ConvertResultsWorker().
// The sv4guiConvertWorkerROM emit statements pass conversion status messages to them.
//
#include <Python.h>

#include "sv4gui_ConvertWorkerROM.h"
#include "sv4gui_ROMSimulationPythonConvert.h"
#include "sv4gui_ROMSimulationView.h"

#include <mitkLogMacros.h>

#include <QMessageBox>

#include <iostream>
#include <map>

//------------------------
// sv4guiConvertWorkerROM
//------------------------
//
sv4guiConvertWorkerROM::sv4guiConvertWorkerROM()
{
  m_Thread = nullptr;
}

sv4guiConvertWorkerROM::~sv4guiConvertWorkerROM()
{
  m_Thread->quit();
}

//---------
// convert
//---------
// Convert ROM solver results into a general format for plotting. 
//
// This executes the Python script the CPython PyObject_Call() function.
//
void sv4guiConvertWorkerROM::convertResults()
{
  sv4guiROMSimulationPythonConvertParamNames paramNames;

  // Import the convert ROM solver results module.
  //
  auto pyName = PyUnicode_DecodeFSDefault((char*)m_ModuleName.c_str());
  auto pyModule = PyImport_Import(pyName);

  if (pyModule == nullptr) {
      auto msg = "Unable to load the Python '" + QString(m_ModuleName.c_str()) + "' module.";
      emit error(msg);
  }

  // Get the module interface function that executes 
  // module functions based on input arguments. 
  //
  auto pyFuncName = (char*)"run_from_c";
  auto pyDict = PyModule_GetDict(pyModule);
  auto pyFunc = PyDict_GetItemString(pyDict, (char*)pyFuncName);

  if (!PyCallable_Check(pyFunc)) {
      auto msg = "Can't find the function '" + QString(pyFuncName) + "' in the '" + QString(m_ModuleName.c_str()) + "' module.";
      emit error(msg);
      return;
  }

  // Create an argument containing the output directory.
  // This is used to write a script log file to the
  // solver job directory.
  //
  auto dummyArgs = PyTuple_New(1);
  auto dummyValue = PyUnicode_DecodeFSDefault(m_OutputDirectory.c_str());
  PyTuple_SetItem(dummyArgs, 0, dummyValue);

  // Create the **kwarg arguments that are the input arguments to the module.
  auto kwargs = PyDict_New();
  for (auto const& param : m_ParameterValues) {
      PyDict_SetItemString(kwargs, param.first.c_str(), PyUnicode_DecodeFSDefault(param.second.c_str()));
  }

  // Execute the Python script.
  auto result = PyObject_Call(pyFunc, dummyArgs, kwargs);

  // Check for errors.
  PyErr_Print();

  // If the convert results files was not successful then
  // then display error messages and the script log file.
  //
  // Search for the Python logger ERROR or WARNING messages in the 
  // returned result to determine if the script failed.
  //
  // The 'emit showMessage(errorMsg, msg)' statement sends a message to 
  // sv4guiROMSimulationView::ShowConvertWorkerMessage().
  //
  if (result) {
      auto uResult = PyUnicode_FromObject(result);
      auto sResult = std::string(PyUnicode_AsUTF8(uResult));
      bool errorMsg;
      QString msg = QString(sResult.c_str());

      if (sResult.find("Error") != std::string::npos) {
          errorMsg = true;
      } else {
          errorMsg = false;
      }

      emit showMessage(errorMsg, msg);
  }

  // Signal the thead is finished;
  emit finished();
}

