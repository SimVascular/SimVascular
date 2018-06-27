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
  #include "sv_repos_init_py.h"
#endif
#include "vtksys/SystemTools.hxx"

#ifdef SV_USE_QT_GUI
  #include <QApplication>
  #include <QDir>
  #include <QVariant>
  #include <QDebug>
  #include "mitkBaseApplication.h"
  #include "ctkPluginFrameworkLauncher.h"
#endif

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

#ifdef WIN32
#ifdef SV_USE_WIN32_REGISTRY
#ifdef __MINGW32__
// imperfect simple work around for missing getenv_s
// on mingw32 stdlib (no bounds checking!)
#define getenv_s cv_getenv_s
errno_t cv_getenv_s(
   size_t *pReturnValue,
   char* buffer,
   size_t numberOfElements,
   const char *varname
) {
  *pReturnValue = 0;
  char *rtnstr = NULL;
  rtnstr = getenv(varname);
  if (rtnstr == NULL) {
    return 0;
  }
  *pReturnValue = strlen(rtnstr);
  if (buffer != NULL) {
    buffer[0]='\0';
    sprintf(buffer,rtnstr);
  }
  return 0;
}
#endif
#endif
#endif

#include "SimVascular_Init.h"
#ifdef SV_USE_PYTHON
#include "SimVascular_Init_py.h"
#endif

#ifdef SV_USE_QT_GUI
typedef void Tcl_MainLoopProc(void);
void SimVascularTcl_MainLoop(void) {
    QApplication::exec();
}
#endif

void
svCatchDebugger() {
    static volatile int debuggerPresent =0;
    while (!debuggerPresent ); // assign debuggerPresent=1
}

#ifdef SV_USE_QT_GUI
class simvascularApp : public mitk::BaseApplication {

  public:

  simvascularApp(int argc, char** argv);
  ~simvascularApp();

  protected:
  void initializeLibraryPaths();

};

simvascularApp::simvascularApp(int argc, char* argv[]) : BaseApplication(argc, argv)
{
}

simvascularApp::~simvascularApp()
{
}

void simvascularApp::initializeLibraryPaths() {

  //std::cout << "\n\n *** simvascularApp: initializeLibraryPaths! *** \n\n" << std::endl << std::flush;

  bool found_sv_plugin_path = false;

  //
  //  This is SV code to start using env variables and registry
  //  to specify library paths.
  //

  // read environment variables for plugin paths
  fprintf(stdout,"Reading plugin paths SV_PLUGIN_PATH environment variable...\n");
  fprintf(stdout,"\n");
  fflush(stdout);
#ifdef WIN32
  char plugin_env[_MAX_ENV];
  size_t requiredSize;
  plugin_env[0]='\0';
  requiredSize = 0;
  getenv_s( &requiredSize, NULL, 0, "SV_PLUGIN_PATH");

  if (requiredSize == 0) {
    std::cerr << "Warning:  SV_PLUGIN_PATH doesn't exist!\n" << std::endl << std::flush;
  } else if (requiredSize >= _MAX_ENV) {
    std::cerr << "FATAL ERROR:  SV_PLUGIN_PATH to long!\n" << std::endl << std::flush;
    exit(-1);
  } else {
    found_sv_plugin_path = true;
    getenv_s( &requiredSize, plugin_env, requiredSize, "SV_PLUGIN_PATH" );
    char seps[] = ";";
    char *token;
    token = strtok( plugin_env, seps );
    while( token != NULL ) {
      // While there are tokens in "string"
      //printf( " %s\n", token );
      QString pluginPath = token;
      ctkPluginFrameworkLauncher::addSearchPath(pluginPath);
      std::cout << "   Adding to plugin search path (" << pluginPath.toStdString() << ")" << std::endl << std::flush;
      // Get next token
      token = strtok( NULL, seps );
    }
  }
#else
  char *plugin_env = getenv("SV_PLUGIN_PATH");
  if (plugin_env == NULL) {
    std::cerr << "Warning:  SV_PLUGIN_PATH doesn't exist!\n" << std::endl << std::flush;
  } else {
    found_sv_plugin_path = true;
    char seps[] = ":";
    char *token;
    token = strtok( plugin_env, seps );
    while( token != NULL ) {
      // While there are tokens in "string"
      //printf( " %s\n", token );
      QString pluginPath = token;
      ctkPluginFrameworkLauncher::addSearchPath(pluginPath);
      std::cout << "   Adding to plugin search path (" << pluginPath.toStdString() << ")" << std::endl << std::flush;
      // Get next token
      token = strtok( NULL, seps );
    }
  }
#endif
  fprintf(stdout,"\n");

  //
  // This is the default behavior in AppUtil for MITK.
  //

  if (!found_sv_plugin_path) {
    QStringList suffixes;
    QDir appDir;

    suffixes << "plugins";
#ifdef WIN32
    suffixes << "bin/plugins";
#ifdef CMAKE_INTDIR
    suffixes << "bin/" CMAKE_INTDIR "/plugins";
#endif
#else
    suffixes << "lib/plugins";
#ifdef CMAKE_INTDIR
    suffixes << "lib/" CMAKE_INTDIR "/plugins";
#endif
#endif

#ifdef __APPLE__
    suffixes << "../../plugins";
#endif

    // we add a couple of standard library search paths for plug-ins
    appDir = QCoreApplication::applicationDirPath();

    // walk one directory up and add bin and lib sub-dirs; this
    // might be redundant
    appDir.cdUp();

    foreach(QString suffix, suffixes)
    {
      ctkPluginFrameworkLauncher::addSearchPath(appDir.absoluteFilePath(suffix));
    }

    suffixes << "plugins";
    suffixes << "bin/plugins";
    suffixes << "lib/plugins";

    // we add a couple of standard library search paths for plug-ins
    appDir = QCoreApplication::applicationDirPath();

    foreach(QString suffix, suffixes)
    {
      ctkPluginFrameworkLauncher::addSearchPath(appDir.absoluteFilePath(suffix));
      std::cout << "Adding to plugin search path (" << appDir.absoluteFilePath(suffix).toStdString() <<  ")" << std::endl << std::flush;
    }

  }

  //
  //  This code is a debugging check to make sure that all of the dll's
  //  can be found in the search path.
  //
  fprintf(stdout,"Checking all plugin paths...\n");
  fprintf(stdout,"\n");
  fflush(stdout);

  QVariant pluginsToStartVariant = this->getProperty(ctkPluginFrameworkLauncher::PROP_PLUGINS);
  QStringList pluginsToStart = pluginsToStartVariant.toStringList();

  for (QStringList::iterator it =  pluginsToStart.begin();
       it !=  pluginsToStart.end(); ++it) {
     QString current = *it;
     QString MypluginPath = ctkPluginFrameworkLauncher::getPluginPath(current);
     std::cout << "  plugin (" << current.toStdString() << ")" << std::endl << std::flush;
     std::cout << "    resolves to [" << MypluginPath.toStdString() << "]" << std::endl << std::flush;
  }
  fprintf(stdout,"\n");
  fflush(stdout);

  return;
}

#endif

// -----------
// svPythonPrependPath
// -----------

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
  cout << "Initializing Python Shell..." << endl;

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
#if PYTHON_MAJOR_VERSION == 3

  SimVascular_pyInit();
#endif

  // Initialize interpreter.
  Py_Initialize();
#if PYTHON_MAJOR_VERSION ==2
  SimVascular_pyInit();
#endif

  //SimVascular_pyImport();

   //PyImport_ImportModule("pyRepository");
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

// ----
// main
// ----

//  Note: Static Modules don't seem to work for MITK plugins.
//        This code should return an error for now.

#ifdef SV_USE_QT_GUI
  #ifdef QT_STATICPLUGIN
    //Q_IMPORT_PLUGIN(...)
  #endif
  //#include <usModuleImport.h>
  // seems to be missing from mitk's cppservices
  //US_IMPORT_MODULE_RESOURCES(...)
  #ifdef US_STATIC_MODULE
    //US_INITIALIZE_STATIC_MODULE(...)
    //US_INITIALIZE_IMPORT_STATIC_MODULE_RESOURCES(...)
  #endif
#endif

 FILE *simvascularstdout;
 FILE *simvascularstderr;

inline bool file_exists (char* name) {
    if (FILE *file = fopen(name, "r")) {
        fclose(file);
        return true;
    } else {
        return false;
    }
}

 int main( int argc, char *argv[] )
 {

  // default to tcl gui
  bool use_tcl_gui = false;
  bool use_qt_gui  = true;
  bool use_workbench  = false;
  bool catch_debugger = false;
  bool use_provisioning_file = false;

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

  fprintf(stdout,"argc %i\n",argc);
  fflush(stdout);

  // check for file to select default qt gui on win32
#ifdef WIN32
  CHAR user_home_path[MAX_PATH];
  if (SUCCEEDED(SHGetFolderPathA(NULL, CSIDL_PROFILE, NULL, 0, user_home_path))) {
    fprintf(stdout,"User home path: %s\n",user_home_path);
    char default_gui_qt_filename[255];
    default_gui_qt_filename[0]='\0';
    sprintf(default_gui_qt_filename,"%s\\%s",user_home_path,"simvascular_default_gui_qt.txt");
    fprintf(stdout,"filename: %s\n",default_gui_qt_filename);
    if (file_exists(default_gui_qt_filename)) {
      use_tcl_gui = false;
      use_qt_gui  = true;
      fprintf(stdout,"Note: Defaulting to qt gui because (%s) exists.",default_gui_qt_filename);
    }
    default_gui_qt_filename[0]='\0';
    sprintf(default_gui_qt_filename,"%s\\%s",user_home_path,".simvascular_default_gui_qt.txt");
    if (file_exists(default_gui_qt_filename)) {
      use_tcl_gui = false;
      use_qt_gui  = true;
      fprintf(stdout,"Note: Defaulting to qt gui because (%s) exists.",default_gui_qt_filename);
    }
    default_gui_qt_filename[0]='\0';
    sprintf(default_gui_qt_filename,"%s\\%s",user_home_path,"simvascular_default_gui_qt");
    if (file_exists(default_gui_qt_filename)) {
      use_tcl_gui = false;
      use_qt_gui  = true;
      fprintf(stdout,"Note: Defaulting to qt gui because (%s) exists.",default_gui_qt_filename);
    }
    default_gui_qt_filename[0]='\0';
    sprintf(default_gui_qt_filename,"%s\\%s",user_home_path,".simvascular_default_gui_qt");
    if (file_exists(default_gui_qt_filename)) {
      use_tcl_gui = false;
      use_qt_gui  = true;
      fprintf(stdout,"Note: Defaulting to qt gui because (%s) exists.",default_gui_qt_filename);
    }
  }
#endif

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
	fprintf(stdout,"  -tcl, --tcl-gui : use TclTk GUI\n");
	fprintf(stdout,"  -qt, --qt-gui   : use Qt GUI\n");
	fprintf(stdout,"  -d,--debug      : infinite loop for debugging\n");
        fprintf(stdout,"  -ng ,--no-gui   : use command line mode (SV_BATCH_MODE overrides)\n");
	fprintf(stdout,"  --qt-tcl-interp : use command line tcl interp with qt gui\n");
	fprintf(stdout,"  --warn          : warn if invalid cmd line params (off by default)\n");
	fprintf(stdout,"  --workbench     : use mitk workbench application\n");
	fprintf(stdout,"  --use-pro       : use the .provisioning file \n");
	exit(0);
      }
      if((!strcmp("--warn",argv[iarg]))) {
	warnInvalid = true;
	foundValid = true;
      }
      if((!strcmp("-tcl",argv[iarg]))    ||
	 (!strcmp("--tcl-gui",argv[iarg]))) {
	use_tcl_gui = true;
	use_qt_gui = false;
	foundValid = true;
      }
      if((!strcmp("-qt",argv[iarg]))    ||
	 (!strcmp("--qt-gui",argv[iarg]))) {
	use_qt_gui = true;
	use_tcl_gui = false;
	foundValid = true;
      }
      if((!strcmp("-d",argv[iarg]))    ||
	 (!strcmp("--debug",argv[iarg]))) {
	catch_debugger = true;
	foundValid = true;
      }
      if((!strcmp("-ng",argv[iarg]))    ||
	 (!strcmp("--no-gui",argv[iarg]))) {
	gSimVascularBatchMode = 1;
	foundValid = true;
      }
      if((!strcmp("--workbench",argv[iarg]))) {
	use_qt_gui = true;
	use_tcl_gui = false;
	use_workbench = true;
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

  char rundir[255];
  rundir[0]='\0';

  HKEY hKey2;
  LONG returnStatus2;

  DWORD dwType2=REG_SZ;
  DWORD dwSize2=255;
  char lszValue2[255];
  SecureZeroMemory(lszValue2,sizeof(lszValue2));

  DWORD dwType3=REG_SZ;
  DWORD dwSize3=255;
  char lszValue3[255];
  SecureZeroMemory(lszValue3,sizeof(lszValue3));

  DWORD dwType4=REG_SZ;
  DWORD dwSize4=1024;
  char lszValue4[1024];
  SecureZeroMemory(lszValue4,sizeof(lszValue4));

  DWORD dwType5=REG_SZ;
  DWORD dwSize5=1024;
  char lszValue5[1024];
  SecureZeroMemory(lszValue5,sizeof(lszValue5));

  DWORD dwType6=REG_SZ;
  DWORD dwSize6=1024;
  char lszValue6[1024];
  SecureZeroMemory(lszValue6,sizeof(lszValue6));

  DWORD dwType7=REG_SZ;
  DWORD dwSize7=255;
  char lszValue7[255];
  SecureZeroMemory(lszValue7,sizeof(lszValue7));

  char mykey[1024];
  mykey[0]='\0';
  sprintf(mykey,"%s\\%s\\%s\\%s-%s-%s","SOFTWARE",SV_REGISTRY_TOPLEVEL,SV_VERSION,SV_MAJOR_VERSION,SV_MINOR_VERSION,SV_PATCH_VERSION);

  // we first assume that we are running on a 32-bit OS
  returnStatus2 = RegOpenKeyEx(HKEY_LOCAL_MACHINE, mykey, 0L,  KEY_READ, &hKey2);
  // if that fails, we check for the key hidden on a 64-bit OS
  // if this fails, then just give up and go home
  if (returnStatus2 != ERROR_SUCCESS) {
    fprintf(stdout,"Could not find SV registry!\n(%s)\n Looking elsewhere.....",mykey);
    mykey[0]='\0';
    sprintf(mykey,"%s\\%s\\%s\\%s-%s-%s","SOFTWARE\\Wow6432Node",SV_REGISTRY_TOPLEVEL,SV_VERSION,SV_MAJOR_VERSION,SV_MINOR_VERSION,SV_PATCH_VERSION);
    //fprintf(stdout,"%s\n\n",mykey);
    returnStatus2 = RegOpenKeyEx(HKEY_LOCAL_MACHINE, mykey, 0L,  KEY_READ, &hKey2);
  }
  if (returnStatus2 != ERROR_SUCCESS) {
    fprintf(stderr,"FATAL ERROR: SV registry error!\n(%s)\n",mykey);
    exit(-1);
  }

  returnStatus2 = RegQueryValueEx(hKey2, "RunDir", NULL, &dwType2,(LPBYTE)&rundir, &dwSize2);
  //RegCloseKey(hKey2);

  if (returnStatus2 != ERROR_SUCCESS) {
    fprintf(stdout,"  FATAL ERROR: Invalid application registry.  SV RunDir not found!\n\n");
    exit(-1);
  }

  //printf("Value Read is %s\n", rundir);

  // set the environment variables using the registry
  char oldpath[_MAX_ENV];
  char newpath[_MAX_ENV];
  size_t requiredSize;

   //
   //  Add to PATH
   //

  oldpath[0]='\0';
  getenv_s( &requiredSize, NULL, 0, "PATH");
  if (requiredSize >= _MAX_ENV) {
    fprintf(stderr,"FATAL ERROR:  path to long!\n");
    exit(-1);
  }
  getenv_s( &requiredSize, oldpath, requiredSize, "PATH" );

  // prepend path with location of our shared libs

  int newpathlength = 0;
  newpath[0]='\0';

  for (newpathlength = 0; newpathlength < strlen(rundir);newpathlength++) {
    newpath[newpathlength]=rundir[newpathlength];
    if (newpathlength == _MAX_ENV) {
      fprintf(stderr,"FATAL ERROR:  path to long!\n");
      exit(-1);
    }
  }
  newpath[newpathlength++]=';';

  // need mitk path, sigh.
  char mitkrunpath[_MAX_ENV];
  mitkrunpath[0]='\0';
  sprintf(mitkrunpath,"%s\\%s",rundir,"mitk/bin");
  for (int i = 0; i < strlen(mitkrunpath);i++) {
    newpath[newpathlength++]=mitkrunpath[i];
    if (newpathlength == _MAX_ENV) {
      fprintf(stderr,"FATAL ERROR:  path to long!\n");
      exit(-1);
    }
  }
  newpath[newpathlength++]=';';

  // now add original path

  for (int i = 0; i < strlen(oldpath);i++) {
    newpath[newpathlength++]=oldpath[i];
    if (newpathlength == _MAX_ENV) {
      fprintf(stderr,"FATAL ERROR:  path to long!\n");
      exit(-1);
    }
  }
  newpath[newpathlength]='\0';

  _putenv_s( "PATH", newpath );

  fprintf(stdout,"ORIGINAL PATH: %s\n",oldpath);
  //fprintf(stdout,"RUNDIR: %s\n",rundir);
  fprintf(stdout,"NEW PATH: %s\n",newpath);
  fprintf(stdout,"length of path: %i\n",newpathlength);

   // set the environment variables using the registry
  char envvar[_MAX_ENV];
  char newvar[_MAX_ENV];

#ifdef SV_USE_PYTHON

  lszValue4[0]='\0';
  returnStatus2 = RegQueryValueEx(hKey2, "PythonPackagesDir", NULL, &dwType4,(LPBYTE)&lszValue4, &dwSize4);
  //returnStatus2 = RegQueryValueEx(hKey2, "Python", NULL, &dwType4,(LPBYTE)&lszValue4, &dwSize4);
  fprintf(stdout,"PythonPackagesDir: %s\n",lszValue4);

  if (returnStatus2 != ERROR_SUCCESS) {
    fprintf(stderr,"  FATAL ERROR: Invalid application registry.  SV PythonPackagesDir not found!\n\n");
    exit(-1);
  }

  char pythonpath[_MAX_ENV];
  pythonpath[0]='\0';
  sprintf(pythonpath,"%s",lszValue4);
  fprintf(stdout,"%s\n",pythonpath);

  _putenv_s( "PYTHONPATH", pythonpath );

  lszValue7[0]='\0';
  returnStatus2 = RegQueryValueEx(hKey2, "PythonHome", NULL, &dwType7,(LPBYTE)&lszValue7, &dwSize7);
  fprintf(stdout,"PythonHome: %s\n",lszValue7);

  if (returnStatus2 != ERROR_SUCCESS) {
    fprintf(stderr,"  FATAL ERROR: Invalid application registry.  SV PythohHome not found!\n\n");
    exit(-1);
  }

  char pythonhomepath[_MAX_ENV];
  pythonhomepath[0]='\0';
  sprintf(pythonhomepath,"%s",lszValue7);
  fprintf(stdout,"%s\n",pythonhomepath);

  _putenv_s( "PYTHONHOME", pythonhomepath );


#endif

#ifdef SV_USE_QT_GUI

  lszValue5[0]='\0';
  returnStatus2 = RegQueryValueEx(hKey2, "SV_PLUGIN_PATH", NULL, &dwType5,(LPBYTE)&lszValue5, &dwSize5);
  fprintf(stdout,"SV_PLUGIN_PATH: %s\n",lszValue5);

  if (returnStatus2 != ERROR_SUCCESS) {
    fprintf(stderr,"  FATAL ERROR: Invalid application registry.  SV_PLUGIN_PATH not found!\n\n");
    exit(-1);
  }

  char sv_plugin_path[_MAX_ENV];
  sv_plugin_path[0]='\0';
  sprintf(sv_plugin_path,"%s",lszValue5);
  fprintf(stdout,"%s\n",sv_plugin_path);

  _putenv_s( "SV_PLUGIN_PATH", sv_plugin_path );

#endif

#ifdef SV_USE_QT

  lszValue6[0]='\0';
  returnStatus2 = RegQueryValueEx(hKey2, "QT_PLUGIN_PATH", NULL, &dwType6,(LPBYTE)&lszValue6, &dwSize6);
  fprintf(stdout,"QT_PLUGIN_PATH: %s\n",lszValue6);

  if (returnStatus2 != ERROR_SUCCESS) {
    fprintf(stderr,"  FATAL ERROR: Invalid application registry.  QT_PLUGIN_PATH not found!\n\n");
    exit(-1);
  }

  char qt_plugin_path[_MAX_ENV];
  qt_plugin_path[0]='\0';
  sprintf(qt_plugin_path,"%s",lszValue6);
  fprintf(stdout,"%s\n",qt_plugin_path);

  _putenv_s( "QT_PLUGIN_PATH", qt_plugin_path );

#endif

RegCloseKey(hKey2);
#endif

#endif

  if (use_tcl_gui) {
    if (gSimVascularBatchMode == 0) {
      Tk_Main( argc, argv, Tcl_AppInit );
    } else {
      Tcl_Main (argc, argv, Tcl_AppInit);
    }
  }

#ifdef SV_USE_QT_GUI

  if(use_qt_gui) {

#ifdef SV_USE_PYTHON
    if (gSimVascularBatchMode == 1)
    {
      return PythonShell_Init(argc, argv);
    }
    else
    {
#endif

     // intentionally remove any additional params when calling qt gui
     int single_argc = 1;
     //mitk::BaseApplication app(single_argc, argv);
     simvascularApp app(single_argc, argv);

     // note: this command doesn't seem to work.
     // US_LOAD_IMPORTED_MODULES_INTO_MAIN(svmodel svpath ...)
     #ifdef QT_STATICPLUGIN
       //Q_INIT_RESOURCE(...);
       // need to call activators for each plugin, e.g.
       //MitkImagePluginActivator* mitkimageplugin = new MitkImagePluginActivator();
       //mitkimageplugin->start();
     #endif

     app.setSingleMode(true);
     app.setApplicationName("SimVascularApplication");
     app.setOrganizationName("SimVascular");

     if (use_workbench) {
       fprintf(stdout,"Note: Using WorkBench App.\n");
       fflush(stdout);
       app.setProperty(mitk::BaseApplication::PROP_PRODUCT, "org.mitk.gui.qt.extapplication.workbench");
     } else {
       app.setProperty(mitk::BaseApplication::PROP_PRODUCT, "org.sv.gui.qt.application.svworkbench");
     }

     QStringList preloadLibs;
     preloadLibs << "liborg_mitk_gui_qt_ext";
     app.setPreloadLibraries(preloadLibs);

     if (use_provisioning_file == false) {

       fprintf(stdout,"Note: Not using the provisioning file. To use a provisioning file, provide --use-pro flag\n");
       fprintf(stdout,"\n");
       fflush(stdout);

       // can set a provisioning file here, but we hard code the plugins below
       QString provisioningFilePath = "";
       app.setProvisioningFilePath(provisioningFilePath);

       QString plugin_dirs = "";
       app.setProperty(mitk::BaseApplication::ARG_PLUGIN_DIRS, "");

       QStringList pluginsToStart;
       QString pluginPath;

       // Note: You can specify full URL filenames as well, e.g.
       // pluginsToStart.push_back("file:///C:/.../liborg_commontk_eventadmin.dll");

      // remove lib prefix and dll postfix
       pluginsToStart.push_back("org_commontk_configadmin");
       pluginsToStart.push_back("org_commontk_eventadmin");
       pluginsToStart.push_back("org_blueberry_core_runtime");
       pluginsToStart.push_back("org_blueberry_core_expressions");
       pluginsToStart.push_back("org_blueberry_core_commands");
       pluginsToStart.push_back("org_blueberry_ui_qt");
       pluginsToStart.push_back("org_blueberry_ui_qt_help");
       pluginsToStart.push_back("org_blueberry_ui_qt_log");
       pluginsToStart.push_back("org_mitk_core_services");
       pluginsToStart.push_back("org_mitk_gui_common");
       pluginsToStart.push_back("org_mitk_planarfigure");
       pluginsToStart.push_back("org_mitk_core_ext");
       pluginsToStart.push_back("org_mitk_gui_qt_application");
       pluginsToStart.push_back("org_mitk_gui_qt_ext");
       pluginsToStart.push_back("org_mitk_gui_qt_extapplication");
       pluginsToStart.push_back("org_mitk_gui_qt_common");
       pluginsToStart.push_back("org_mitk_gui_qt_stdmultiwidgeteditor");
       pluginsToStart.push_back("org_mitk_gui_qt_common_legacy");
       pluginsToStart.push_back("org_mitk_gui_qt_datamanager");
       pluginsToStart.push_back("org_mitk_gui_qt_properties");
       pluginsToStart.push_back("org_mitk_gui_qt_basicimageprocessing");
       pluginsToStart.push_back("org_mitk_gui_qt_dicom");
       pluginsToStart.push_back("org_mitk_gui_qt_geometrytools");
#if MITK_MAJOR_VERSION == 2016
       pluginsToStart.push_back("org_mitk_gui_qt_imagecropper");
#endif
       pluginsToStart.push_back("org_mitk_gui_qt_imagenavigator");
       pluginsToStart.push_back("org_mitk_gui_qt_measurementtoolbox");
#ifdef SV_USE_PYTHON
       pluginsToStart.push_back("org_mitk_gui_qt_python");
#endif
       pluginsToStart.push_back("org_mitk_gui_qt_segmentation");
       pluginsToStart.push_back("org_mitk_gui_qt_volumevisualization");

       // SimVascular plugins
       if (!use_workbench) {
         pluginsToStart.push_back("org_sv_gui_qt_application");
         pluginsToStart.push_back("org_sv_projectdatanodes");
         pluginsToStart.push_back("org_sv_gui_qt_datamanager");
         pluginsToStart.push_back("org_sv_gui_qt_projectmanager");
         pluginsToStart.push_back("org_sv_gui_qt_pathplanning");
         pluginsToStart.push_back("org_sv_gui_qt_modeling");
         pluginsToStart.push_back("org_sv_gui_qt_segmentation");
         pluginsToStart.push_back("org_sv_gui_qt_meshing");
         pluginsToStart.push_back("org_sv_gui_qt_simulation");
#ifdef SV_USE_PYTHON
         pluginsToStart.push_back("org_sv_pythondatanodes");
#endif
       }

        // read environment variables for custom plugins
  fprintf(stdout,"Reading custom plugins SV_CUSTOM_PLUGINS environment variable...\n");
  fprintf(stdout,"\n");
#ifdef WIN32
       char custom_plugins[_MAX_ENV];
       size_t requiredSize;
       custom_plugins[0]='\0';
       requiredSize = 0;
       getenv_s( &requiredSize, NULL, 0, "SV_CUSTOM_PLUGINS");

       if (requiredSize >= _MAX_ENV) {
         std::cerr << "FATAL ERROR:  SV_CUSTOM_PLUGINS to long!\n" << std::endl << std::flush;
         exit(-1);
       } else if (requiredSize > 0) {
         getenv_s( &requiredSize, custom_plugins, requiredSize, "SV_CUSTOM_PLUGINS" );
         char seps[] = ";";
         char *token;
         token = strtok( custom_plugins, seps );
         while( token != NULL ) {
           // While there are tokens in "string"
           //printf( " %s\n", token );
           QString newPlugin = token;
           pluginsToStart.push_back(newPlugin);
           std::cout << "   Adding custom plugin (" << newPlugin.toStdString() << ")" << std::endl << std::flush;
           // Get next token
           token = strtok( NULL, seps );
         }
       }
#else
       char *custom_plugins = getenv("SV_CUSTOM_PLUGINS");
       if (custom_plugins != NULL) {
         char seps[] = ":";
         char *token;
         token = strtok( custom_plugins, seps );
         while( token != NULL ) {
           // While there are tokens in "string"
           //printf( " %s\n", token );
           QString newPlugin = token;
           pluginsToStart.push_back(newPlugin);
           std::cout << "   Adding custom plugin (" << newPlugin.toStdString() << ")" << std::endl << std::flush;
           // Get next token
           token = strtok( NULL, seps );
         }
       }
#endif
       fprintf(stdout,"\n");

       app.setProperty(ctkPluginFrameworkLauncher::PROP_PLUGINS, pluginsToStart);

       //Use transient start with declared activation policy
       ctkPlugin::StartOptions startOptions(ctkPlugin::START_TRANSIENT | ctkPlugin::START_ACTIVATION_POLICY);
       app.setProperty(ctkPluginFrameworkLauncher::PROP_PLUGINS_START_OPTIONS, static_cast<int>(startOptions));
     }

     return app.run();
#ifdef SV_USE_PYTHON
    }
#endif
  }

#endif

  return 0;
}


#ifdef WIN32
#ifdef SV_USE_WIN32_REGISTRY

int Tcl_AppInt_Win32ReadRegistryVar(char* regVarName, char* interpVarName, Tcl_Interp *interp ) {

  HKEY hKey2;
  LONG returnStatus2;
  DWORD dwType2=REG_SZ;
  DWORD dwSize2=255;
  char lszValue2[255];
  char mykey[1024];
  char scmd[2048];

  mykey[0]='\0';
  lszValue2[0]='\0';
  scmd[0]='\0';

  sprintf(mykey,"%s\\%s\\%s\\%s-%s-%s","SOFTWARE",SV_REGISTRY_TOPLEVEL,SV_VERSION,SV_MAJOR_VERSION,SV_MINOR_VERSION,SV_PATCH_VERSION);

  // we first assume that we are running on a 32-bit OS
  returnStatus2 = RegOpenKeyEx(HKEY_LOCAL_MACHINE, mykey, 0L,  KEY_READ, &hKey2);

  // if 32-bit check fails, we check for the key hidden on a 64-bit OS
  // if this fails, then just give up and go home
  if (returnStatus2 != ERROR_SUCCESS) {
    mykey[0]='\0';
    sprintf(mykey,"%s\\%s\\%s\\%s-%s-%s","SOFTWARE\\Wow6432Node",SV_REGISTRY_TOPLEVEL,SV_VERSION,SV_MAJOR_VERSION,SV_MINOR_VERSION,SV_PATCH_VERSION);
    returnStatus2 = RegOpenKeyEx(HKEY_LOCAL_MACHINE, mykey, 0L,  KEY_READ, &hKey2);
  }
  if (returnStatus2 != ERROR_SUCCESS) {
    fprintf(stderr,"FATAL ERROR: SV registry error!\n(%s)\n",mykey);
    exit(-1);
  }

  returnStatus2 = RegQueryValueEx(hKey2, regVarName, NULL, &dwType2,(LPBYTE)&lszValue2, &dwSize2);
  RegCloseKey(hKey2);

  if (returnStatus2 != ERROR_SUCCESS) {
    fprintf(stdout,"  FATAL ERROR: Invalid application registry (%s).\n\n",regVarName);
    exit(-1);
  }

  PathRemoveBackslash(lszValue2);
  // set the variable in tcl interpreter
  sprintf(scmd,"set %s {%s}\n",interpVarName,lszValue2);
  if (Tcl_Eval(interp,scmd) == TCL_ERROR) {
    fprintf ( stderr,"error on (%s)\n",scmd);
    return TCL_ERROR;
  }

  return TCL_OK;
}

#endif
#endif

// -----------
// Tcl_AppInit
// -----------

int Tcl_AppInit( Tcl_Interp *interp )
{

  // store interp into a global variable
  gVtkTclInterp = interp;

#ifdef WIN32
#ifdef SV_USE_WIN32_REGISTRY

  if (Tcl_AppInt_Win32ReadRegistryVar("HomeDir", "env(SV_HOME)", interp ) == TCL_ERROR) {
    exit(-1);
  }

  if (Tcl_AppInt_Win32ReadRegistryVar("TclLibDir", "tcl_library", interp ) == TCL_ERROR) {
    exit(-1);
  }

  if (Tcl_AppInt_Win32ReadRegistryVar("TkLibDir", "tk_library", interp ) == TCL_ERROR) {
    exit(-1);
  }

#endif
#endif

  if ( Tcl_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on Tcl_Init\n" );
    return TCL_ERROR;
  }

  if (gSimVascularBatchMode == 0) {
    if ( Tk_Init(interp) == TCL_ERROR ) {
      fprintf( stderr, "error on Tk_Init\n" );
      return TCL_ERROR;
    }
  }

  if ( SimVascular_Init(interp) == TCL_ERROR ) {
    fprintf( stderr, "error on SimVascular_Init\n" );
    return TCL_ERROR;
  }

  return TCL_OK;

}


