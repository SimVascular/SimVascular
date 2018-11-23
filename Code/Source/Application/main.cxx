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

#ifdef SV_USE_PYTHON
  #include "Python.h"
#endif
#include "vtksys/SystemTools.hxx"

#ifdef SV_USE_QT_GUI
  #include <QApplication>
  #include <QDir>
  #include <QVariant>
  #include <QDebug>
  #include "mitkBaseApplication.h"
  #include "ctkPluginFrameworkLauncher.h"
  #include "sv4gui_MitkApp.h"
  #include "sv4gui_Main.h"
#endif

#include "sv_IOstream.h"
#include <time.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <iostream>

#include "tcl.h"
#include "tk.h"
#include "svTcl_AppInit.h"

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

#ifdef SV_USE_PYTHON
  #include "PythonShell_Init.h"
#endif

void
svCatchDebugger() {
    static volatile int debuggerPresent =0;
    while (!debuggerPresent ); // assign debuggerPresent=1
}

// ----
// main
// ----

//  Note: Static Modules don't seem to work for MITK plugins.
//        This code should return an error for now.
//
// #ifdef SV_USE_QT_GUI
//   #ifdef QT_STATICPLUGIN
//      //Q_IMPORT_PLUGIN(...)
//   #endif
//   //#include <usModuleImport.h>
//   // seems to be missing from mitk's cppservices
//   //US_IMPORT_MODULE_RESOURCES(...)
//   #ifdef US_STATIC_MODULE
//     //US_INITIALIZE_STATIC_MODULE(...)
//     //US_INITIALIZE_IMPORT_STATIC_MODULE_RESOURCES(...)
//   #endif
// #endif

 FILE *simvascularstdout;
 FILE *simvascularstderr;

 int main( int argc, char *argv[] )
 {

  // default to tcl gui
  bool use_tcl = false;
  bool use_tk_gui = false;
  bool use_python  = true;
  bool use_qt_gui  = true;
  bool use_workbench  = false;
  bool catch_debugger = false;
  bool use_provisioning_file = false;
  gSimVascularBatchMode = 0;
 
  ios::sync_with_stdio();

#ifdef BUILD_WITH_STDOUT_STDERR_REDIRECT
  simvascularstdout = freopen( "stdout", "w", stdout );
  // Note: freopen is deprecated; consider using freopen_s instead

  if( simvascularstdout == NULL ) {
    fprintf( stdout, "error on reassigning stdout\n" );
    fflush ( stdout );
  }
  simvascularstderr = freopen( "stderr", "w", stderr );
  // Note: freopen is deprecated; consider using freopen_s instead

  if( simvascularstderr == NULL ) {
    fprintf( stderr, "error on reassigning stderr\n" );
    fflush ( stderr );
  }
#endif

  // parse registry for plugins!
  //sv_parse_registry_for_plugins();
  //exit(0);

  if (argc != 0) {

    // default to tcl gui
    for (int iarg = 1; iarg < argc;iarg++) {
      bool foundValid = false;
      bool warnInvalid = false;
      fprintf(stdout,"processing command line option: %s\n",argv[iarg]);
      if((!strcmp("-h",argv[iarg]))    ||
	 (!strcmp("-help",argv[iarg])) ||
	 (!strcmp("--help",argv[iarg]))) {
	fprintf(stdout,"simvascular command line options:\n");
	fprintf(stdout,"  -h, --help      : print this info and exit\n");
	fprintf(stdout,"  --python        : use python command line\n");
	fprintf(stdout,"  -tcl, --tcl     : use tcl command line\n");
	fprintf(stdout,"  -qt, --qt-gui   : use Qt GUI (SV_BATCH_MODE overrides)\n");
	fprintf(stdout,"  -tk, --tk-gui   : use TclTk GUI (SV_BATCH_MODE overrides)\n");	
	fprintf(stdout,"  -d, --debug     : infinite loop for debugging\n");
	fprintf(stdout,"  --warn          : warn if invalid cmd line params (off by default)\n");
	fprintf(stdout,"  --workbench     : use mitk workbench application\n");
	fprintf(stdout,"  --use-pro       : use the .provisioning file \n");
	exit(0);
      }
      if((!strcmp("--warn",argv[iarg]))) {
	warnInvalid = true;
	foundValid = true;
      }
      if((!strcmp("-python",argv[iarg]))    ||
	 (!strcmp("--python",argv[iarg]))) {
	use_tcl = false;
	use_python = true;
	use_tk_gui = false;
	use_qt_gui = false;
	gSimVascularBatchMode = 1;
	foundValid = true;
      }
      if((!strcmp("-tcl",argv[iarg]))    ||
	 (!strcmp("--tcl",argv[iarg]))) {
	use_tcl = true;
	use_python = false;
	use_tk_gui = false;
	use_qt_gui = false;
	gSimVascularBatchMode = 1;
	foundValid = true;
      }
      if((!strcmp("-qt",argv[iarg]))    ||
	 (!strcmp("--qt-gui",argv[iarg]))) {
	use_tcl = false;
	use_python = true;
	use_tk_gui = false;
	use_qt_gui = true;
	gSimVascularBatchMode = 0;
	foundValid = true;
      }
      if((!strcmp("-tk",argv[iarg]))    ||
	 (!strcmp("--tk-gui",argv[iarg]))) {
	use_tcl = true;
	use_python = false;
	use_tk_gui = true;
	use_qt_gui = false;
	gSimVascularBatchMode = 0;
	foundValid = true;
      }
      if((!strcmp("-d",argv[iarg]))    ||
	 (!strcmp("--debug",argv[iarg]))) {
	catch_debugger = true;
	foundValid = true;
      }
      if((!strcmp("--workbench",argv[iarg]))) {
	use_tcl = false;
	use_python = true;
	use_tk_gui = false;
	use_qt_gui = true;
	use_workbench = true;
	gSimVascularBatchMode = 0;
	foundValid = true;
      }
      if((!strcmp("--use-pro",argv[iarg]))) {
	use_provisioning_file = true;
      }
      if (!foundValid && warnInvalid) {
	fprintf(stderr,"Warning:  unknown option (%s) ignored!\n",argv[iarg]);
      }
    }
  }

  // enter infinite loop for debugger
  if (catch_debugger) {
    svCatchDebugger();
  }

  char *envstr=getenv("SV_BATCH_MODE");
  if (envstr != NULL) {
    fprintf(stdout,"\n  Using SimVascular in batch mode.\n");
    gSimVascularBatchMode = 1;
  }

#ifdef WIN32
#ifdef SV_USE_WIN32_REGISTRY
  sv_parse_reg();
#endif
#endif

  if (gSimVascularBatchMode == 1) {
    if (use_tcl) {    
      Tcl_Main (argc, argv, Tcl_AppInit);
      return 0;
    }
#ifdef SV_USE_PYTHON
    if(use_python) {
      return PythonShell_Init(argc, argv);
    }
#endif
  } else {
    if (use_tk_gui) {
      Tk_Main( argc, argv, Tcl_AppInit );
    }
#ifdef SV_USE_QT_GUI
    if(use_qt_gui) {
      sv4guiMain(argc, argv, use_provisioning_file, use_workbench);
    }
    return 0;
#endif
  }
  return 0;
}
