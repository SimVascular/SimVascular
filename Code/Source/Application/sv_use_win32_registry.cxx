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

int Tcl_AppInt_Win32ReadRegistryVar(char* regVarName, char* interpVarName, Tcl_Interp *interp ) {

  HKEY hKey;
  LONG returnStatus;
  DWORD dwType=REG_SZ;
  DWORD dwSize=2048;
  char lszValue[2048];
  char mykey[1024];
  char scmd[4096];
  SecureZeroMemory(lszValue,sizeof(lszValue));
  mykey[0]='\0';
  scmd[0]='\0';
  
  sprintf(mykey,"%s\\%s\\%s\\%s-%s-%s","SOFTWARE\\Wow6432Node",SV_REGISTRY_TOPLEVEL,SV_VERSION,SV_MAJOR_VERSION,SV_MINOR_VERSION,SV_PATCH_VERSION);
  returnStatus = RegOpenKeyEx(HKEY_LOCAL_MACHINE, mykey, 0L,  KEY_READ, &hKey);
  if (returnStatus != ERROR_SUCCESS) {
    fprintf(stderr,"FATAL ERROR: SV registry error!\n(%s)\n",mykey);
    exit(-1);
  }

  returnStatus = RegQueryValueEx(hKey, regVarName, NULL, &dwType,(LPBYTE)&lszValue, &dwSize);
  RegCloseKey(hKey);

  if (returnStatus != ERROR_SUCCESS) {
    fprintf(stdout,"  FATAL ERROR: Invalid application registry (%s).\n\n",regVarName);
    exit(-1);
  }

  PathRemoveBackslash(lszValue);
  // set the variable in tcl interpreter
  sprintf(scmd,"set %s {%s}\n",interpVarName,lszValue);
  if (Tcl_Eval(interp,scmd) == TCL_ERROR) {
    fprintf ( stderr,"error on (%s)\n",scmd);
    return TCL_ERROR;
  }

  return TCL_OK;
}

// QueryKey - Enumerates the subkeys of key and its associated values.
//     hKey - Key whose subkeys and values are to be enumerated.

void QueryKey(HKEY hKey) 
{ 
    TCHAR    achKey[MAX_KEY_LENGTH];   // buffer for subkey name
    DWORD    cbName;                   // size of name string 
    TCHAR    achClass[MAX_PATH] = TEXT("");  // buffer for class name 
    DWORD    cchClassName = MAX_PATH;  // size of class string 
    DWORD    cSubKeys=0;               // number of subkeys 
    DWORD    cbMaxSubKey;              // longest subkey size 
    DWORD    cchMaxClass;              // longest class string 
    DWORD    cValues;              // number of values for key 
    DWORD    cchMaxValue;          // longest value name 
    DWORD    cbMaxValueData;       // longest value data 
    DWORD    cbSecurityDescriptor; // size of security descriptor 
    FILETIME ftLastWriteTime;      // last write time 
 
    DWORD i, retCode; 
 
    TCHAR  achValue[MAX_VALUE_NAME]; 
    DWORD cchValue = MAX_VALUE_NAME; 
 
    // Get the class name and the value count. 
    retCode = RegQueryInfoKey(
        hKey,                    // key handle 
        achClass,                // buffer for class name 
        &cchClassName,           // size of class string 
        NULL,                    // reserved 
        &cSubKeys,               // number of subkeys 
        &cbMaxSubKey,            // longest subkey size 
        &cchMaxClass,            // longest class string 
        &cValues,                // number of values for this key 
        &cchMaxValue,            // longest value name 
        &cbMaxValueData,         // longest value data 
        &cbSecurityDescriptor,   // security descriptor 
        &ftLastWriteTime);       // last write time 
 
    // Enumerate the subkeys, until RegEnumKeyEx fails.
    
    if (cSubKeys)
    {
        printf( "\nNumber of subkeys: %d\n", cSubKeys);

        for (i=0; i<cSubKeys; i++) 
        { 
            cbName = MAX_KEY_LENGTH;
            retCode = RegEnumKeyEx(hKey, i,
                     achKey, 
                     &cbName, 
                     NULL, 
                     NULL, 
                     NULL, 
                     &ftLastWriteTime); 
            if (retCode == ERROR_SUCCESS) 
            {
                _tprintf(TEXT("(%d) %s\n"), i+1, achKey);
            }
        }
    } 
 
    // Enumerate the key values. 

    if (cValues) 
    {
        printf( "\nNumber of values: %d\n", cValues);

        for (i=0, retCode=ERROR_SUCCESS; i<cValues; i++) 
        { 
            cchValue = MAX_VALUE_NAME; 
            achValue[0] = '\0'; 
            retCode = RegEnumValue(hKey, i, 
                achValue, 
                &cchValue, 
                NULL, 
                NULL,
                NULL,
                NULL);
 
            if (retCode == ERROR_SUCCESS ) 
            { 
                _tprintf(TEXT("(%d) %s\n"), i+1, achValue); 
            } 
        }
    }
}

int sv_parse_registry_for_core_app() {

  HKEY hKey;
  LONG returnStatus;

  DWORD dwType=REG_SZ;
  DWORD dwSize=2048;
  char lszValue[2048];
  SecureZeroMemory(lszValue,sizeof(lszValue));

  // require 64-bit registry entries
  
  char mykey[1024];
  mykey[0]='\0';
  sprintf(mykey,"%s\\%s\\%s\\%s-%s-%s","SOFTWARE\\Wow6432Node",SV_REGISTRY_TOPLEVEL,SV_VERSION,SV_MAJOR_VERSION,SV_MINOR_VERSION,SV_PATCH_VERSION);
  //sprintf(mykey,"%s\\%s\\%s","SOFTWARE\\Wow6432Node",SV_REGISTRY_TOPLEVEL,"SimVascular\\2018-11-30");
  fprintf(stdout,"%s\n\n",mykey);
  
  returnStatus = RegOpenKeyEx(HKEY_LOCAL_MACHINE, mykey, 0L,  KEY_READ, &hKey);
  if (returnStatus != ERROR_SUCCESS) {
    fprintf(stderr,"FATAL ERROR: SV registry error!\n(%s)\n",mykey);
    exit(-1);
  }

  QueryKey(hKey);
  
  // process variables unique to core then process paths which can also be found in plugins

#ifdef SV_USE_PYTHON

  dwSize=1024;
  SecureZeroMemory(lszValue,sizeof(lszValue));
  
  returnStatus = RegQueryValueEx(hKey, "PYTHONHOME", NULL, &dwType,(LPBYTE)&lszValue, &dwSize);
  fprintf(stdout,"PYTHONHOME: %s\n",lszValue);

  if (returnStatus != ERROR_SUCCESS) {
    fprintf(stderr,"  FATAL ERROR: Invalid application registry.  SV PythohHome not found!\n\n");
    exit(-1);
  }

  sv_main_append_to_envvar("PYTHONHOME",lszValue);

#endif

#ifdef SV_USE_QT

  dwSize=1024;
  SecureZeroMemory(lszValue,sizeof(lszValue));

  returnStatus = RegQueryValueEx(hKey, "QT_PLUGIN_PATH", NULL, &dwType,(LPBYTE)&lszValue, &dwSize);
  fprintf(stdout,"QT_PLUGIN_PATH: %s\n",lszValue);

  if (returnStatus != ERROR_SUCCESS) {
    fprintf(stderr,"  FATAL ERROR: Invalid application registry.  QT_PLUGIN_PATH not found!\n\n");
    exit(-1);
  }

  sv_main_append_to_envvar("QT_PLUGIN_PATH",lszValue);

#endif

  RegCloseKey(hKey);

  // now use code common to plugins to create/append path env vars
  sv_parse_registry_for_plugins(mykey);

  return SV_OK;
  
}

int sv_parse_registry_for_plugins(char* toplevel_key) {

  HKEY hKey;
  LONG returnStatus;

  returnStatus = RegOpenKeyEx(HKEY_LOCAL_MACHINE, toplevel_key, 0L,  KEY_READ, &hKey);
  if (returnStatus != ERROR_SUCCESS) {
    fprintf(stderr,"RegistryForPlugins: SV registry error!\n(%s)\n",toplevel_key);
    return SV_ERROR;
  }

  DWORD dwType=REG_SZ;
  DWORD dwSize=2048;
  char lszValue[2048];
  SecureZeroMemory(lszValue,sizeof(lszValue));
  
  returnStatus = RegQueryValueEx(hKey, "ADD_TO_PATH", NULL, &dwType,(LPBYTE)&lszValue, &dwSize);
  if (returnStatus == ERROR_SUCCESS) {
     sv_main_append_to_envvar("PATH",lszValue);
  } else {
    fprintf(stdout,"  NOTE: ADD_TO_PATH not found in (%s)\n",toplevel_key);
  }

  // this needs to be added to installer script
  
  // need mitk path, sigh.
  //char mitkrunpath[_MAX_ENV];
  //mitkrunpath[0]='\0';
  //sprintf(mitkrunpath,"%s\\%s",lszValue,"mitk/bin");
  //sv_main_append_to_envvar("PATH",mitkrunpath);

#ifdef SV_USE_PYTHON

  dwSize=1024;
  SecureZeroMemory(lszValue,sizeof(lszValue));
  returnStatus = RegQueryValueEx(hKey, "ADD_TO_PYTHONPATH", NULL, &dwType,(LPBYTE)&lszValue, &dwSize);
  fprintf(stdout,"PythonPackagesDir: %s\n",lszValue);
  if (returnStatus == ERROR_SUCCESS) {
     sv_main_append_to_envvar("PYTHONPATH",lszValue);
  } else {
    fprintf(stdout,"  NOTE: ADD_TO_PYTHONPATH not found in (%s)\n",toplevel_key);
  }

#endif

#ifdef SV_USE_QT_GUI

  dwSize=1024;
  SecureZeroMemory(lszValue,sizeof(lszValue));
  returnStatus = RegQueryValueEx(hKey, "ADD_TO_SV_PLUGIN_PATH", NULL, &dwType,(LPBYTE)&lszValue, &dwSize);
  fprintf(stdout,"SV_PLUGIN_PATH: %s\n",lszValue);
  if (returnStatus == ERROR_SUCCESS) {
     sv_main_append_to_envvar("SV_PLUGIN_PATH",lszValue);
  } else {
    fprintf(stdout,"  NOTE: ADD_TO_SV_PLUGIN_PATH not found in (%s)\n",toplevel_key);
  }

#endif

  RegCloseKey(hKey);
  return SV_OK;
  
}

int sv_main_append_to_envvar(char* envvar_to_update, char* appendme) {

  // set the environment variables using the registry
  char oldvar[_MAX_ENV];
  char newvar[_MAX_ENV];
  oldvar[0]='\0';
  newvar[0]='\0';
  size_t requiredSize;

  // check that the environment variable actually exists
  // default to empty string if it doesn't
  
  char* envstr=getenv(envvar_to_update);
  if (envstr != NULL) {
    getenv_s( &requiredSize, NULL, 0, envvar_to_update);
    if (requiredSize >= _MAX_ENV) {
      fprintf(stderr,"FATAL ERROR:  var (%s) to long!\n",envvar_to_update);
      exit(-1);
    }
    getenv_s( &requiredSize, oldvar, requiredSize, envvar_to_update);
  }
  
  int newvarlength = 0;

  for (newvarlength = 0; newvarlength < strlen(appendme);newvarlength++) {
    newvar[newvarlength]=appendme[newvarlength];
    if (newvarlength == _MAX_ENV) {
      fprintf(stderr,"FATAL ERROR:  var (%s) to long!\n",envvar_to_update);
      exit(-1);
    }
  }
  newvar[newvarlength++]=';';

  // now add original value
  for (int i = 0; i < strlen(oldvar);i++) {
    newvar[newvarlength++]=oldvar[i];
    if (newvarlength == _MAX_ENV) {
      fprintf(stderr,"FATAL ERROR:  var (%s) to long!\n",envvar_to_update);
      exit(-1);
    }
  }
  newvar[newvarlength]='\0';

  _putenv_s( envvar_to_update, newvar );

  fprintf(stdout,"APPENDME: %s\n",appendme);
  fprintf(stdout,"ORIGINAL ENV (%s): %s\n", envvar_to_update, oldvar);
  fprintf(stdout,"NEW ENV (%s): %s\n", envvar_to_update, newvar);
  fprintf(stdout,"LENGTH ENV (%s): %i\n",envvar_to_update, newvarlength);

  return SV_OK;

}

#endif
#endif
