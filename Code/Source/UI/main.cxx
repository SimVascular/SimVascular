/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson, Ken Wang.
 *
 * See SimVascular Acknowledgements file for additional
 * contributors to the source code. 
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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include "SimVascular.h" 

#include "cvIOstream.h"
#include <time.h>
#include <stdlib.h>

#include "tcl.h"
#include "tk.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

 extern "C" CV_DLL_EXPORT int SimVascular_Init(Tcl_Interp *interp);

 void SimVascularWelcome( Tcl_Interp *interp );

#include "cv_globals.h"

#ifdef WIN32
#include <windows.h>
#include <tchar.h>
#include <stdio.h>
#include "Shlwapi.h"

#define BUFSIZE 1024
#define BUF_SIZE 1024
#endif

#ifdef WIN32
#ifdef SIMVASCULAR_USE_WIN32_REGISTRY
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

// ----
// main
// ----

 FILE *simvascularstdout;
 FILE *simvascularstderr;

 int main( int argc, char *argv[] )
 {
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

  char *envstr=getenv("SIMVASCULAR_BATCH_MODE");
  if (envstr != NULL) {
    fprintf(stdout,"\n  Using SimVascular in batch mode.\n");
    gSimVascularBatchMode = 1;
  }

#ifdef WIN32
#ifdef SIMVASCULAR_USE_WIN32_REGISTRY

  HKEY hKey2;
  LONG returnStatus2;
  DWORD dwType2=REG_SZ;
  DWORD dwSize2=255;
  char lszValue2[255];
  lszValue2[0]='\0';

  DWORD dwSize3=255;
  char lszValue3[255];
  lszValue3[0]='\0';

  char mykey[1024];
  mykey[0]='\0';
  sprintf(mykey,"%s\\%s\\%s %s","SOFTWARE",SIMVASCULAR_REGISTRY_TOPLEVEL,SIMVASCULAR_VERSION,SIMVASCULAR_MAJOR_VER_NO);

  // we first assume that we are running on a 32-bit OS
  returnStatus2 = RegOpenKeyEx(HKEY_LOCAL_MACHINE, mykey, 0L,  KEY_READ, &hKey2);
  // if that fails, we check for the key hidden on a 64-bit OS
  // if this fails, then just give up and go home
  if (returnStatus2 != ERROR_SUCCESS) {
    fprintf(stdout,"Could not find SIMVASCULAR registry!\n(%s)\n Looking elsewhere.....",mykey);
    mykey[0]='\0';
    sprintf(mykey,"%s\\%s\\%s %s","SOFTWARE\\Wow6432Node",SIMVASCULAR_REGISTRY_TOPLEVEL,SIMVASCULAR_VERSION,SIMVASCULAR_MAJOR_VER_NO);
    //fprintf(stdout,"%s\n\n",mykey);
    returnStatus2 = RegOpenKeyEx(HKEY_LOCAL_MACHINE, mykey, 0L,  KEY_READ, &hKey2);
  }
  if (returnStatus2 != ERROR_SUCCESS) {
    fprintf(stderr,"FATAL ERROR: SIMVASCULAR registry error!\n(%s)\n",mykey);
    exit(-1);
  }
  
  returnStatus2 = RegQueryValueEx(hKey2, "RunDir", NULL, &dwType2,(LPBYTE)&lszValue2, &dwSize2);
  //RegCloseKey(hKey2);

  if (returnStatus2 != ERROR_SUCCESS) {
    fprintf(stdout,"  FATAL ERROR: Invalid application registry.  SIMVASCULAR RunDir not found!\n\n");
    exit(-1);
  }

  //printf("Value Read is %s\n", lszValue2);
  int envi = 0;
  for (envi = 0; envi < strlen(lszValue2);envi++) {
    //if (lszValue2[envi]=='\\') lszValue2[envi]='/';
  }

   // set the environment variables using the registry
  char envvar[_MAX_ENV];
  char newvar[_MAX_ENV];
  size_t requiredSize;

#ifdef USE_PARASOLID
   //
   //  P_SCHEMA
   //

  envvar[0]='\0';
  getenv_s( &requiredSize, NULL, 0, "P_SCHEMA");
  if (requiredSize >= _MAX_ENV) {
   fprintf(stderr,"FATAL ERROR:  p_schema to long!\n");
   exit(-1);
 }
 if (requiredSize > 0) {
     // Get the value of the p_schema environment variable.
   getenv_s( &requiredSize, envvar, requiredSize, "P_SCHEMA" );

     //if( envvar != NULL )
      //printf( "Original P_SCHEMA variable is: %s\n", envvar );
 }

   // Attempt to change p_schema. Note that this only affects
   // the environment variable of the current process. The command
   // processor's environment is not changed.
 
 returnStatus2 = RegQueryValueEx(hKey2, "PSchemaDir", NULL, &dwType2,(LPBYTE)&lszValue3, &dwSize3);
  //fprintf(stdout,"pschema: %s\n",lszValue3);

 if (returnStatus2 != ERROR_SUCCESS) {
  fprintf(stderr,"  FATAL ERROR: Invalid application registry.  SIMVASCULAR PSchemaDir not found!\n\n");
  exit(-1);
}


newvar[0]='\0';
sprintf(newvar,"%s",lszValue3);
  //fprintf(stdout,"%s\n",newvar);
TCHAR  shortpschema[1024]=TEXT(""); 
   // convert to short filename without spaces
if(!GetShortPathName(newvar,shortpschema,1024)) {
     // Handle an error condition.
 printf ("GetFullPathName failed (%s) (%d)\n", newvar, GetLastError());
 _putenv_s( "P_SCHEMA", newvar );
} else {
     //fprintf(stdout,"ShortPathName (%s)\n",shortpschema);
  _putenv_s( "P_SCHEMA", shortpschema );
}

getenv_s( &requiredSize, NULL, 0, "P_SCHEMA");

envvar[0]='\0';

   // Get the new value of the p_schema environment variable. 
getenv_s( &requiredSize, envvar, requiredSize, "P_SCHEMA" );

   //if( envvar != NULL )
   //   printf( "New P_SCHEMA variable is: %s\n", envvar );
#endif
RegCloseKey(hKey2);
#endif
#endif

if (gSimVascularBatchMode == 0) {
  Tk_Main( argc, argv, Tcl_AppInit );
} else {
  Tcl_Main (argc, argv, Tcl_AppInit);
}

return 0;
}


#ifdef WIN32
#ifdef SIMVASCULAR_USE_WIN32_REGISTRY

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

  sprintf(mykey,"%s\\%s\\%s %s","SOFTWARE",SIMVASCULAR_REGISTRY_TOPLEVEL,SIMVASCULAR_VERSION,SIMVASCULAR_MAJOR_VER_NO);

  // we first assume that we are running on a 32-bit OS
  returnStatus2 = RegOpenKeyEx(HKEY_LOCAL_MACHINE, mykey, 0L,  KEY_READ, &hKey2);

  // if 32-bit check fails, we check for the key hidden on a 64-bit OS
  // if this fails, then just give up and go home
  if (returnStatus2 != ERROR_SUCCESS) {
    mykey[0]='\0';
    sprintf(mykey,"%s\\%s\\%s %s","SOFTWARE\\Wow6432Node",SIMVASCULAR_REGISTRY_TOPLEVEL,SIMVASCULAR_VERSION,SIMVASCULAR_MAJOR_VER_NO);
    returnStatus2 = RegOpenKeyEx(HKEY_LOCAL_MACHINE, mykey, 0L,  KEY_READ, &hKey2);
  }
  if (returnStatus2 != ERROR_SUCCESS) {
    fprintf(stderr,"FATAL ERROR: SIMVASCULAR registry error!\n(%s)\n",mykey);
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

#ifdef WIN32  
#ifdef SIMVASCULAR_USE_WIN32_REGISTRY

  if (Tcl_AppInt_Win32ReadRegistryVar("HomeDir", "env(SIMVASCULAR_HOME)", interp ) == TCL_ERROR) {
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


