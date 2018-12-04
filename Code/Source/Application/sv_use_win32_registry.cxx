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

#ifdef WIN32
#ifdef SV_USE_WIN32_REGISTRY

#include "sv_use_win32_registry.h"

int sv_parse_reg() {
  
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
}


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
