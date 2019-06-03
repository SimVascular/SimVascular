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

#include "sv4gui_win32_use_registry.h"

// QueryKey - Enumerates the subkeys of key and its associated values.
//     hKey - Key whose subkeys and values are to be enumerated.

void sv4guiQueryKey(HKEY hKey) 
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

int sv4gui_parse_registry_for_svsolver(char* keytofind, char* rtnval) {

  HKEY hKey[3];
  LONG returnStatus;

  DWORD dwType=REG_SZ;
  DWORD dwSize=2048;
  char lszValue[2048];
  SecureZeroMemory(lszValue,sizeof(lszValue));

  // require 64-bit registry entries
  
  char mykey[3][1024];
  mykey[0][0]='\0';
  sprintf(mykey[0],"%s\\%s\\%s\\%s","SOFTWARE\\Wow6432Node",SV_REGISTRY_TOPLEVEL,"Solvers","svSolver");
  fprintf(stdout,"%s\n\n",mykey[0]);
 
  returnStatus = RegOpenKeyEx(HKEY_LOCAL_MACHINE, mykey[0], 0L,  KEY_READ, &hKey[0]);
  if (returnStatus != ERROR_SUCCESS) {
    fprintf(stdout,"Note: No SimVascular svSolver found in registry.\n(%s)\n",mykey[0]);
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
    
    fprintf(stdout, "\nNumber of subkeys: %d\n", cSubKeys[0]);

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
	      
	        _tprintf(TEXT("\nsvSolver %d: %s\n"), i+1, achKey[0]);
                mykey[1][0]='\0';
                sprintf(mykey[1],"%s\\%s",mykey[0],achKey[0]);
                fprintf(stdout,"%s\n\n",mykey[1]);
		returnStatus = RegOpenKeyEx(HKEY_LOCAL_MACHINE, mykey[1], 0L,  KEY_READ, &hKey[1]);

                if (returnStatus != ERROR_SUCCESS) {
                  fprintf(stderr,"ERROR: SV subkeys not found in registry!\n(%s)\n",mykey[1]);
                  return SV_ERROR;

		} else {

		  //sv4guiQueryKey(hKey[1]);
		  sv4gui_parse_registry_for_svsolver_internal(mykey[1],keytofind,rtnval);

		} // returnStatus
       } // retCode
    } // for i
  }  //subKeys[0]

  fprintf(stdout,"\n\n");
  RegCloseKey(hKey[0]);

  return SV_OK;
}


int sv4gui_parse_registry_for_svsolver_internal(char* toplevel_key, char* keytofind, char* rtnval) {

  HKEY hKey;
  LONG returnStatus;

  char append_env_var_key[1024];
  append_env_var_key[0]='\0';
  sprintf(append_env_var_key,"%s",toplevel_key);
  
  fprintf(stdout,"parse for env vars (%s)\n\n",append_env_var_key);
  
  returnStatus = RegOpenKeyEx(HKEY_LOCAL_MACHINE, append_env_var_key, 0L,  KEY_READ, &hKey);
  if (returnStatus != ERROR_SUCCESS) {
    fprintf(stderr,"RegistryForSvsolver: SV registry error!\n(%s)\n",append_env_var_key);
    return SV_ERROR;
  }

  //sv4guiQueryKey(hKey);
  
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
    fprintf(stdout,"cSubKeys: %i\n",cSubKeys);
  }
  if (cValues) {
    fprintf(stdout, "\n  Number of values: %d\n", cValues);
    
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
	if (!strcmp(keytofind,achValue)) {
	  fprintf(stdout,"keytofind(%s)  achValue (%s) with value (%s)\n",achValue,lszValue, keytofind);
	  sprintf(rtnval,"%s",lszValue);
	  return SV_OK;
	}
      }
    }
  }
  
  RegCloseKey(hKey);
  return SV_ERROR;
  
}

#endif
#endif
