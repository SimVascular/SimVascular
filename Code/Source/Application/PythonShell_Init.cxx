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

#include "SimVascular.h"

#include "simvascular_options.h"

#ifdef WIN32
#include <windows.h>
#endif

#ifdef SV_USE_PYTHON
  #include "Python.h"
#endif
#include "vtksys/SystemTools.hxx"

#include "sv_IOstream.h"
#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <iostream>

#include "tcl.h"
#include "tk.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

#include "sv2_globals.h"

#ifdef WIN32
#include <windows.h>
#include <tchar.h>
#include "Shlwapi.h"
#include <Shlobj.h>

#define BUFSIZE 1024
#define BUF_SIZE 1024

#ifndef GetShortPathName
  #ifdef UNICODE
    #define GetShortPathName GetShortPathNameW
  #else
    #define GetShortPathName GetShortPathNameA
  #endif // !UNICODE
#endif

#endif

/* The maximum length of a file name.  */
#if defined(PATH_MAX)
#define SV_PYTHON_MAXPATH PATH_MAX
#elif defined(MAXPATHLEN)
#define SV_PYTHON_MAXPATH MAXPATHLEN
#else
#define SV_PYTHON_MAXPATH 16384
#endif

#include "SimVascular_Init.h"

#ifdef WIN32
#ifdef SV_USE_WIN32_REGISTRY
  #include "sv_use_win32_registry.h"
#endif
#endif

#ifdef SV_USE_PYTHON_EMBEDDED_IMPORTS
  #include "SimVascular_Init_py.h"
  #include "sv4gui_Vis_init_py.h"
#endif

#include "PythonShell_Init.h"

// -------------------
// svPythonPrependPath
// -------------------

#ifdef SV_USE_PYTHON
static void svPythonPrependPath(const char* dir)
{
  // Convert slashes for this platform.
  std::string out_dir = dir;
#if defined(_WIN32) && !defined(__CYGWIN__)
  for(std::string::size_type i = 0; i < out_dir.length(); ++i)
  {
    if(out_dir[i] == '/')
    {
      out_dir[i] = '\\';
    }
  }
#endif

  // Append the path to the python sys.path object.
  char tmpPath[] = "path";
  PyObject* path = PySys_GetObject(tmpPath);
  PyObject* newpath;
  newpath = PyUnicode_FromString(out_dir.c_str());
  PyList_Insert(path, 0, newpath);
  Py_DECREF(newpath);
}

// -----------
// PythonShell_Init
// -----------

int PythonShell_Init(int argc, char *argv[])
{
  cout << "\n" <<endl;
  cout << "SimVascular Python Shell" << endl;
  cout << "Copyright (c) Stanford University, The Regents of the University" << endl;
  cout << "              of California, and others.  All Rights Reserved.";
  cout << "\n" << endl;

  // SETTING UP A PYTHON SHELL THE WAY VTK DOES FOR TESTING.
  // SHOULD BE FINE SEEING
  // AS HOW WE BUILD VTK WITH THE SAME PYTHON WE USE, BUT MIGHT WANT
  // TO SET UP ON OUR OWN

  // The following code will hack in the path for running VTK/Python
  // from the build tree. Do not try this at home. We are
  // professionals.

  // Set the program name, so that we can ask python to provide us
  // full path.  We need to collapse the path name to aid relative
  // path computation for the VTK python module installation.
  std::string av0 = vtksys::SystemTools::CollapseFullPath(argv[0]);
#if PYTHON_MAJOR_VERSION >= 3
  wchar_t *argv0;
#if PY_VERSION_HEX >= 0x03050000
  argv0 = Py_DecodeLocale(av0.c_str(), NULL);
#elif defined(__APPLE__)
  argv0 = _Py_DecodeUTF8_surrogateescape(av0.data(), av0.length());
#else
  argv0 = _Py_char2wchar(av0.c_str(), NULL);
#endif
  if (argv0 == 0)
  {
    fprintf(stderr, "Fatal simvascular python shell error: "
                    "unable to decode the program name\n");
    return 1;
  }
#else
  static char argv0[SV_PYTHON_MAXPATH];
  strcpy(argv0, av0.c_str());
#endif
  Py_SetProgramName(argv0);

// CALL SIMVASCULAR PYTHON MODULES HERE

//  // This function is generated, and will register any static Python modules for VTK
//  // This needs to be done *before* Py_Initialize().
//  CMakeLoadAllPythonModules();

// CALL SIMVASCULAR PYTHON MODULES HERE
// Initialize interpreter.
  
#ifdef SV_USE_PYTHON_EMBEDDED_IMPORTS
  #if PYTHON_MAJOR_VERSION == 3
    SimVascular_pyInit();  
    Py_Initialize();
  #endif
  #if PYTHON_MAJOR_VERSION == 2
    Py_Initialize();
    SimVascular_pyInit();
  #endif
    SimVascular_pyImport();
#else
    Py_Initialize();
#endif

  // Initialize python thread support. This function should first be
  // called from the main thread, after Py_Initialize.
//#ifndef VTK_NO_PYTHON_THREADS
  PyEval_InitThreads();
//#endif

  // Compute the directory containing this executable.  The python
  // sys.executable variable contains the full path to the interpreter
  // executable.
  char tmpExe[] = "executable";
  PyObject *executable = PySys_GetObject(tmpExe);
#if PYTHON_MAJOR_VERSION >= 3
  executable = PyUnicode_EncodeFSDefault(executable);
#endif
  const char *exe_str = PyBytes_AsString(executable);
  if (exe_str)
  {
    // Use the executable location to try to set sys.path to include
    // the VTK python modules.
    std::string self_dir = vtksys::SystemTools::GetFilenamePath(exe_str);
    svPythonPrependPath(self_dir.c_str());
  }
#if PYTHON_MAJOR_VERSION >= 3
  Py_DECREF(executable);
#endif

  // Ok, all done, now enter python main.
#if PYTHON_MAJOR_VERSION >= 3
  // Need two copies of args, because programs might modify the first
  wchar_t **argvWide = new wchar_t *[argc];
  wchar_t **argvWide2 = new wchar_t *[argc];
  for (int i = 0; i < argc; i++)
  {
#if PY_VERSION_HEX >= 0x03050000
    argvWide[i] = Py_DecodeLocale(argv[i], NULL);
#elif defined(__APPLE__)
    argvWide[i] = _Py_DecodeUTF8_surrogateescape(argv[i], strlen(argv[i]));
#else
    argvWide[i] = _Py_char2wchar(argv[i], NULL);
#endif
    argvWide2[i] = argvWide[i];
    if (argvWide[i] == 0)
    {
      fprintf(stderr, "Fatal vtkpython error: "
                      "unable to decode the command line argument #%i\n",
                      i + 1);
      for (int k = 0; k < i; k++)
      {
        PyMem_Free(argvWide2[i]);
      }
      PyMem_Free(argv0);
      delete [] argvWide;
      delete [] argvWide2;
      return 1;
    }
  }
  int res = Py_Main(argc, argvWide);
  PyMem_Free(argv0);
  for (int i = 0; i < argc; i++)
  {
    PyMem_Free(argvWide2[i]);
  }
  delete [] argvWide;
  delete [] argvWide2;
  return res;
#else
  return Py_Main(argc, argv);
#endif
}
#endif
