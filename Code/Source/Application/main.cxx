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

#ifdef SV_USE_SV4_GUI
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
#include <vtkObject.h>

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
// #ifdef SV_USE_SV4_GUI
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

  // default to qt gui if built
  // default to python if built
  bool use_tcl = false;
  bool use_tk_gui = false;
  bool use_python  = true;
  bool use_qt_gui  = true;
  gSimVascularBatchMode = 0;
 
#ifndef SV_USE_PYTHON
  use_python  = false;
  use_tcl = true;
#endif

#ifndef SV_USE_SV4_GUI
  use_qt_gui  = false;
  // default to python console instead of tcl gui
  //use_tk_gui = true;
  gSimVascularBatchMode = 1;
#endif
  
  bool use_workbench  = false;
  bool catch_debugger = false;
  bool use_provisioning_file = false;
  bool pass_along_options = false;

#ifdef WIN32  
  gSimVascularUseWin32Registry = 1;
#endif
  
  ios::sync_with_stdio();

  // want to hide launch only flags from tcl/python/mitk
  // shells
  int pass_along_start_index = 0;
  int pass_along_argc = 1;
     
  if (argc != 0) {

    bool warnInvalid = true;
    for (int iarg = 1; iarg < argc;iarg++) {
      bool foundValid = false;
      //fprintf(stdout,"processing command line option (%i of %i): %s\n",iarg, argc-1, argv[iarg]);
      //fflush(stdout);
      if((!strcmp("-h",argv[iarg]))    ||
	 (!strcmp("-help",argv[iarg])) ||
	 (!strcmp("--help",argv[iarg]))) {
	fprintf(stdout,"simvascular command line options:\n");
	fprintf(stdout,"  -h, --help               : print this info and exit\n");
#ifdef SV_USE_PYTHON
	fprintf(stdout,"  --python                 : use python command line\n");
#endif
#ifdef SV_USE_TCL
	fprintf(stdout,"  -tcl, --tcl              : use tcl command line\n");
	fprintf(stdout,"  -tk, --tk-gui            : use TclTk GUI (SV_BATCH_MODE overrides)\n");
#endif
#ifdef SV_USE_SV4_GUI
	fprintf(stdout,"  -qt, --qt-gui            : use Qt GUI (SV_BATCH_MODE overrides)\n");
	fprintf(stdout,"  --workbench              : use mitk workbench application\n");
	fprintf(stdout,"  --use-pro                : use the .provisioning file \n");
#endif
#ifdef WIN32
	fprintf(stdout,"  --use-registry           : use Windows registry entries (default) \n");
	fprintf(stdout,"  --ignore-registry        : ignore Windows registry entries \n");
#endif	
	fprintf(stdout,"  -d, --debug              : infinite loop for debugging\n");
	fprintf(stdout,"  --warn                   : warn if invalid cmd line params (on by default)\n");
	fprintf(stdout,"  --redirect-stdio prefix  : redirect stdout & stderr to prefix/std.out etc. \n");
	fprintf(stdout,"  --                       : pass remaining params to tcl/python shells\n");
	exit(0);
      }
      if((!strcmp("--warn",argv[iarg]))) {
	warnInvalid = true;
	foundValid = true;
      }
#ifdef SV_USE_PYTHON
      if((!strcmp("-python",argv[iarg]))    ||
	 (!strcmp("--python",argv[iarg]))) {
	use_tcl = false;
	use_python = true;
	use_tk_gui = false;
	use_qt_gui = false;
	gSimVascularBatchMode = 1;
	foundValid = true;
      }
#endif
#ifdef SV_USE_TCL
      if((!strcmp("-tcl",argv[iarg]))    ||
	 (!strcmp("--tcl",argv[iarg]))) {
	use_tcl = true;
	use_python = false;
	use_tk_gui = false;
	use_qt_gui = false;
	gSimVascularBatchMode = 1;
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
#endif
#ifdef SV_USE_SV4_GUI
      if((!strcmp("-qt",argv[iarg]))    ||
	 (!strcmp("--qt-gui",argv[iarg]))) {
	use_tcl = false;
	use_python = true;
	use_tk_gui = false;
	use_qt_gui = true;
	gSimVascularBatchMode = 0;
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
	foundValid = true;
      }
#endif
#ifdef WIN32
      if((!strcmp("--ignore-registry",argv[iarg]))) {
	gSimVascularUseWin32Registry = 0;
	foundValid = true;
      }
      if((!strcmp("--use-registry",argv[iarg]))) {
	gSimVascularUseWin32Registry = 1;
	foundValid = true;
      }
#endif
      if((!strcmp("-d",argv[iarg]))    ||
	 (!strcmp("--debug",argv[iarg]))) {
	catch_debugger = true;
	foundValid = true;
      }
      if((!strcmp("--redirect-stdio",argv[iarg]))) {
	iarg++;
	if(iarg >= argc) {
	  fprintf(stdout,"error!  no prefix for redirect-stdio!");
	  foundValid = false;
	  break;
	}
	char filestdout[1024];
	filestdout[0]='\0';
	sprintf(filestdout,"%s-stdout.txt",argv[iarg]);
	char filestderr[1024];
	filestderr[0]='\0';
	sprintf(filestderr,"%s-stderr.txt",argv[iarg]);
	//#ifdef BUILD_WITH_STDOUT_STDERR_REDIRECT
        simvascularstdout = freopen( filestdout, "w", stdout );
        // Note: freopen is deprecated; consider using freopen_s instead
        if( simvascularstdout == NULL ) {
          fprintf( stdout, "error on reassigning stdout\n" );
          fflush ( stdout );
        }
        simvascularstderr = freopen( filestderr, "w", stderr );
        // Note: freopen is deprecated; consider using freopen_s instead
        if( simvascularstderr == NULL ) {
          fprintf( stderr, "error on reassigning stderr\n" );
          fflush ( stderr );
	}
        //#endif
	foundValid = true;
      }
      if((!strcmp("--",argv[iarg]))) {
	pass_along_start_index = iarg + 1;
	if(pass_along_start_index < argc) {
	  pass_along_options = true;
	  pass_along_argc = argc - pass_along_start_index + 1;
	}
        //fprintf(stdout,"  Note: remaining args passed along...\n");
	//fprintf(stdout,"    pass_along_start_index: %i\n",pass_along_start_index);
	//fprintf(stdout,"    pass_along_argc: %i\n\n",pass_along_argc);
	//fflush(stdout);
	foundValid = true;
	break;
      }
      if (!foundValid && warnInvalid) {
	fprintf(stderr,"  Warning:  unknown option (%s) ignored!\n",argv[iarg]);
	fflush(stderr);
      }
    }
  }

  char** useme_argv;
  int useme_argc;
  
  useme_argv = (char**)malloc(pass_along_argc*sizeof(char*));
  useme_argc = pass_along_argc;
  
  // argv[0] is the executable name
  char* str = (char*)malloc(strlen(argv[0])+1);
  str[0]='\0';
  strcpy(str,argv[0]);
  useme_argv[0] = str;
  //fprintf(stdout,"useme_argv[0] = %s\n",useme_argv[0]);

  if (pass_along_options) {
    for (int i = 0; i < (pass_along_argc - 1);i++) {
      char* str = (char*)malloc(strlen(argv[pass_along_start_index+i])+1);
      str[0]='\0';
      strcpy(str,argv[pass_along_start_index+i]);
      useme_argv[i+1] = str;
      //fprintf(stdout,"useme_argv[%i] = %s\n",i+1,useme_argv[i+1]);
    }
  }

  fflush(stdout);
  fflush(stderr);

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
  if (gSimVascularUseWin32Registry) {
    fprintf(stdout,"\nReading SimVascular registry entries.\n\n");
    sv_parse_registry_for_core_app();
  }
#endif
#endif

  vtkObject::GlobalWarningDisplayOff();

  if (gSimVascularBatchMode == 1) {
    if (use_tcl) {    
      Tcl_Main (useme_argc, useme_argv, Tcl_AppInit);
      return 0;
    }
#ifdef SV_USE_PYTHON
    if(use_python) {
      return PythonShell_Init(useme_argc, useme_argv);
    }
#endif
  } else {
    if (use_tk_gui) {
      Tk_Main(useme_argc, useme_argv, Tcl_AppInit );
    }
#ifdef SV_USE_SV4_GUI
    if(use_qt_gui) {
      sv4guiMain(useme_argc, useme_argv, use_provisioning_file, use_workbench);
    }
    return 0;
#endif
  }
  return 0;
}
