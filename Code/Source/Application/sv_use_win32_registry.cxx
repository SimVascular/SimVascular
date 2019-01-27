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
    fprintf(stderr,"  FATAL ERROR: Invalid application registry (%s).\n\n",regVarName);
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
  //fprintf(stdout,"%s\n\n",mykey);
 
  returnStatus = RegOpenKeyEx(HKEY_LOCAL_MACHINE, mykey, 0L,  KEY_READ, &hKey);
  if (returnStatus != ERROR_SUCCESS) {
    fprintf(stderr,"FATAL ERROR: SV registry error!\n(%s)\n",mykey);
    exit(-1);
  }
 
  RegCloseKey(hKey);

  // now use code common to plugins to create/append path env vars
  sv_parse_registry_for_environment_variables(mykey);

  sv_parse_registry_for_plugins();
  
  return SV_OK;
  
}

int sv_parse_registry_for_plugins() {
  
  HKEY hKey[3];
  LONG returnStatus;

  DWORD dwType=REG_SZ;
  DWORD dwSize=2048;
  char lszValue[2048];
  SecureZeroMemory(lszValue,sizeof(lszValue));

  // require 64-bit registry entries
  
  char mykey[3][1024];
  mykey[0][0]='\0';
  sprintf(mykey[0],"%s\\%s\\%s\\%s-%s-%s","SOFTWARE\\Wow6432Node",SV_REGISTRY_TOPLEVEL,"Plugins",SV_MAJOR_VERSION,SV_MINOR_VERSION,SV_PATCH_VERSION);
  //fprintf(stdout,"%s\n\n",mykey[0]);
 
  returnStatus = RegOpenKeyEx(HKEY_LOCAL_MACHINE, mykey[0], 0L,  KEY_READ, &hKey[0]);
  if (returnStatus != ERROR_SUCCESS) {
    fprintf(stdout,"Note: No SimVascular plugins found in registry.\n(%s)\n",mykey[0]);
    return SV_OK;
  }
  
  TCHAR    achKey[2][MAX_KEY_LENGTH];   // buffer for subkey name
  DWORD    cbName[2];                   // size of name string 
  TCHAR    achClass0[MAX_PATH] = TEXT(""); // buffer for class name
  TCHAR    achClass1[MAX_PATH] = TEXT(""); // buffer for class name           
  DWORD    cchClassName[2];  // size of class string
           cchClassName[0] = MAX_PATH;
           cchClassName[1] = MAX_PATH;
  DWORD    cSubKeys[2];  // number of subkeys
	   cSubKeys[0]=0;
	   cSubKeys[1]=0; 
  DWORD    cbMaxSubKey[2];              // longest subkey size 
  DWORD    cchMaxClass[2];              // longest class string 
  DWORD    cValues[2];              // number of values for key 
  DWORD    cchMaxValue[2];          // longest value name 
  DWORD    cbMaxValueData[2];       // longest value data 
  DWORD    cbSecurityDescriptor[2]; // size of security descriptor 
  FILETIME ftLastWriteTime[2];      // last write time 
 
  DWORD i, j, retCode; 
 
  TCHAR  achValue[2][MAX_VALUE_NAME]; 
  DWORD cchValue[2];
  cchValue[0] = MAX_VALUE_NAME;
  cchValue[1] = MAX_VALUE_NAME;
  
  // Get the class name and the value count. 
  retCode = RegQueryInfoKey(
        hKey[0],                    // key handle 
        achClass0,                // buffer for class name 
        &cchClassName[0],           // size of class string 
        NULL,                    // reserved 
        &cSubKeys[0],               // number of subkeys 
        &cbMaxSubKey[0],            // longest subkey size 
        &cchMaxClass[0],            // longest class string 
        &cValues[0],                // number of values for this key 
        &cchMaxValue[0],            // longest value name 
        &cbMaxValueData[0],         // longest value data 
        &cbSecurityDescriptor[0],   // security descriptor 
        &ftLastWriteTime[0]);       // last write time 
 
  // Enumerate the subkeys, until RegEnumKeyEx fails.
  if (cSubKeys[0]) {
    
    //fprintf(stdout, "\nNumber of subkeys: %d\n", cSubKeys[0]);

    for (i=0; i<cSubKeys[0]; i++) {
      
      cbName[0] = MAX_KEY_LENGTH;
      retCode = RegEnumKeyEx(hKey[0], i,
                     achKey[0], 
                     &cbName[0], 
                     NULL, 
                     NULL, 
                     NULL, 
                     &ftLastWriteTime[0]);
      
       if (retCode == ERROR_SUCCESS) {
	      
	        _tprintf(TEXT("\nPlugin %d: %s\n"), i+1, achKey[0]);
                mykey[1][0]='\0';
                sprintf(mykey[1],"%s\\%s",mykey[0],achKey[0]);
                //fprintf(stdout,"%s\n\n",mykey[1]);
		returnStatus = RegOpenKeyEx(HKEY_LOCAL_MACHINE, mykey[1], 0L,  KEY_READ, &hKey[1]);

                if (returnStatus != ERROR_SUCCESS) {
                  fprintf(stderr,"ERROR: SV subkeys not found in registry!\n(%s)\n",mykey[1]);
                  return SV_ERROR;
		  
		} else {

                  // Get the class name and the value count. 
                  retCode = RegQueryInfoKey(
                     hKey[1],                    // key handle 
                     achClass1,                // buffer for class name 
                     &cchClassName[1],           // size of class string 
                     NULL,                    // reserved 
                     &cSubKeys[1],               // number of subkeys 
                     &cbMaxSubKey[1],            // longest subkey size 
                     &cchMaxClass[1],            // longest class string 
                     &cValues[1],                // number of values for this key 
                     &cchMaxValue[1],            // longest value name 
                     &cbMaxValueData[1],         // longest value data 
                     &cbSecurityDescriptor[1],   // security descriptor 
                     &ftLastWriteTime[1]);       // last write time 
 
                  // Enumerate the subkeys, until RegEnumKeyEx fails.
                  if (cSubKeys[1]) {
                    //fprintf(stdout, "\nNumber of sub-subkeys: %d\n", cSubKeys[1]);
		    for (j=0; j<cSubKeys[1]; j++) {
                      cbName[1] = MAX_KEY_LENGTH;
                      retCode = RegEnumKeyEx(hKey[1], j,
                         achKey[1], 
                         &cbName[1], 
                         NULL, 
                         NULL, 
                         NULL, 
                         &ftLastWriteTime[1]);
     
                      if (retCode == ERROR_SUCCESS) {
	                //_tprintf(TEXT("cSubKeys1 (%d) (j %i) %s\n"), i+1, j, achKey[1]);
                        mykey[2][0]='\0';
                        sprintf(mykey[2],"%s\\%s",mykey[1],achKey[1]);
                        fprintf(stdout,"  Found plugin in registry (%s)\n",mykey[2]);
			// now use code common to plugins to create/append path env vars
                        sv_parse_registry_for_environment_variables(mykey[2]);
                        break;
		      } // retCode
		      
		    } // j 
		  } // cSubKeys[1]
		  RegCloseKey(hKey[1]);
		} // returnStatus
       } // retCode
    } // for i
  }  //subKeys[0]

  fprintf(stdout,"\n\n");	
  RegCloseKey(hKey[0]);

  return SV_OK;
}


int sv_parse_registry_for_environment_variables(char* toplevel_key) {

  HKEY hKey;
  LONG returnStatus;

  char append_env_var_key[1024];
  append_env_var_key[0]='\0';
  sprintf(append_env_var_key,"%s\\%s",toplevel_key,"ENVIRONMENT_VARIABLES");
  
  //fprintf(stdout,"parse for env vars (%s)\n\n",append_env_var_key);
  
  returnStatus = RegOpenKeyEx(HKEY_LOCAL_MACHINE, append_env_var_key, 0L,  KEY_READ, &hKey);
  if (returnStatus != ERROR_SUCCESS) {
    fprintf(stderr,"RegistryForPlugins: SV registry error!\n(%s)\n",append_env_var_key);
    return SV_ERROR;
  }

  //QueryKey(hKey);
  
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
  if (retCode != ERROR_SUCCESS) {
    fprintf(stderr,"error reading hkey!\n\n");
  } else {
    //fprintf(stdout,"cSubKeys: %i\n",cSubKeys);
  }
  if (cValues) {
    //fprintf(stdout, "\n  Number of values: %d\n", cValues);
    
    for (i=0, retCode=ERROR_SUCCESS; i<cValues; i++) { 
      cchValue = MAX_VALUE_NAME;
      achValue[0] = '\0';
      DWORD dwType=REG_SZ;
      DWORD dwSize=2048;
      char lszValue[2048];
      SecureZeroMemory(lszValue,sizeof(lszValue));
      retCode = RegEnumValue(hKey, i, 
                achValue, 
                &cchValue, 
                NULL,
		&dwType,
		(LPBYTE)&lszValue,
		&dwSize);
      if (retCode == ERROR_SUCCESS) {
	  fprintf(stdout,"  Set/Append (%s) with value (%s)\n",achValue,lszValue);
          if (returnStatus == ERROR_SUCCESS) {
            sv_main_append_to_envvar(achValue,lszValue);
          } else {
            fprintf(stderr,"  WARNING: error reading value (%s) in (%s)\n",achValue,toplevel_key);
	  }
      }
    }
  }
  
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
  if (strlen(oldvar) > 0) newvar[newvarlength++]=';';

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

  //fprintf(stdout,"APPENDME (%s): %s\n",envvar_to_update, appendme);
  //fprintf(stdout,"ORIGINAL ENV (%s): %s\n", envvar_to_update, oldvar);
  //fprintf(stdout,"NEW ENV (%s): %s\n", envvar_to_update, newvar);
  //fprintf(stdout,"LENGTH ENV (%s): %i\n",envvar_to_update, newvarlength);

  return SV_OK;

}

#endif
#endif
