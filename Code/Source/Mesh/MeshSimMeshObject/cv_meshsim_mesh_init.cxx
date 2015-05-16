/*=========================================================================
 *
 * Copyright (c) 2014 The Regents of the University of California.
 * All Rights Reserved. 
 *
 * Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson, William Katz.
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
 *
 *=========================================================================*/

#include "SimVascular.h" 

#include "cv_misc_utils.h"
#include "cv_arg.h"

#include "cvMeshSimMeshSystem.h"
#include "cv_meshsim_mesh_init.h"

#include "cv_globals.h"

#ifdef USE_PARASOLID
#include "SimParasolidKrnl.h"
#include "SimParasolidInt.h"
#endif

#include "SimAdvMeshing.h"

#include "MeshSim.h"
#include "SimError.h"
#include "SimErrorCodes.h"
#include "SimMeshingErrorCodes.h"

#ifdef WIN32
#ifdef MESHSIM_LICENSE_IN_WIN32_REGISTRY

int MeshSim_Win32ReadRegistrySimRegister(char* regVarName) {

  HKEY hKey2;
  LONG returnStatus2;
  DWORD dwType2=REG_SZ;
  DWORD dwSize2=255;
  char lszValue2[255];
  char simkey[255];

  simkey[0]='\0';
  lszValue2[0]='\0';

  // we first assume that we are running on a 64-bit OS
  returnStatus2 = RegOpenKeyEx(HKEY_LOCAL_MACHINE, "SOFTWARE\\SIMVASCULAR\\LICENSES", 0L,  KEY_READ, &hKey2);

  // if 32-bit check fails, we check for the key hidden on a 32-bit OS
  // if this fails, then just give up and go home
  if (returnStatus2 != ERROR_SUCCESS) {
    returnStatus2 = RegOpenKeyEx(HKEY_LOCAL_MACHINE, "SOFTWARE\\Wow6432Node\\SIMVASCULAR\\LICENSES", 0L,  KEY_READ, &hKey2);
  }
  if (returnStatus2 != ERROR_SUCCESS) {
    fprintf(stderr,"No MeshSim license key for (%s) in registry. Meshsim functionality isn't available.\n",regVarName);
    fflush(stdout);
    return CV_ERROR;
  }
   
  returnStatus2 = RegQueryValueEx(hKey2, regVarName, NULL, &dwType2,(LPBYTE)&lszValue2, &dwSize2);
  RegCloseKey(hKey2);

  if (returnStatus2 != ERROR_SUCCESS) {
    fprintf(stdout,"  Warning: Invalid application registry (%s).\n\n",regVarName);
    fflush(stdout);
    return CV_ERROR;
  }

  // set the variable in tcl interpreter
  sprintf(simkey,"%s",lszValue2);

  Sim_registerKey(simkey);

  return CV_OK;

}

#endif
#endif

int MeshSimMesh_AvailableCmd( ClientData clientData, Tcl_Interp *interp,
		   int argc, CONST84 char *argv[] );


// Initialization routine when the library is loaded via Tcl

int Meshsimmesh_Init( Tcl_Interp *interp )
{
  try 
  {
  // Initialize the MeshSim libraries
    printf("  %-12s %s\n", "MeshSim:", MS_version());

    MS_init();
  } catch (...) {
   fprintf(stdout,"  ERROR Initializng MeshSim.\n");
   return TCL_OK;
 }
#if defined(MESHSIM_LICENSE_IN_WIN32_REGISTRY)
 try 
 {
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_ATTRIBUTES");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_PARASOLID");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_DISCRETE");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_SURFACE");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_VOLUME");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_EXPORT");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_ADV");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_ADAPT");
} catch (...) {
 fprintf(stdout,"  ERROR MeshSim Registry.\n");
 return TCL_OK;
}
#elif defined(MESHSIM_USE_LICENSE_FILE)
try 
{

  Sim_readLicenseFile(NULL);

} catch (...) {
 fprintf(stdout,"  Note: Missing or expired MeshSim License. This causes a General error.\n");
 fprintf(stdout,"  This means MeshSim functionality will be unavailable.\n");
 return TCL_OK;
}
#elif defined(MESHSIM_EMBED_LICENSE_KEYS)
#include "../../../Licenses/MeshSim/meshsim_license.h"
try 
{
  Sim_registerKey(MESHSIM_KEY_ATTRIBUTES);
  Sim_registerKey(MESHSIM_KEY_PARASOLID);
  Sim_registerKey(MESHSIM_KEY_DISCRETE);
  Sim_registerKey(MESHSIM_KEY_SURFACE);
  Sim_registerKey(MESHSIM_KEY_VOLUME);
  Sim_registerKey(MESHSIM_KEY_EXPORT);
  Sim_registerKey(MESHSIM_KEY_ADV);
  Sim_registerKey(MESHSIM_KEY_ADAPT);
} catch (...) {
 fprintf(stdout,"  ERROR MeshSim Embeded Keys.\n");
 return TCL_OK;
}
#else
fprintf(stdout,"ERROR: need to register license keys somehow!\n");
return CV_ERROR;
#endif
try{
  SimModel_start();
  #ifdef USE_PARASOLID
  if (SimParasolid_start( 0 ) != 0) {
   fprintf(stdout,"ERROR starting MeshSim Parasolid Interface!\n");
   return CV_ERROR;
 }
  #endif

 SimAdvMeshing_start();
} 
catch (...) {
 fprintf(stdout,"  ERROR starting MeshSim.  Will not be available!\n");
 return TCL_OK;
}
	// Get the main mesh
	MeshKernelRegistryMethodPtr pMeshKernelRegistryMethod = 
    (MeshKernelRegistryMethodPtr) Tcl_GetAssocData( interp, "MeshSystemRegistrar", NULL);
	
  if (pMeshKernelRegistryMethod != NULL) {
    cvMeshSystem* meshSimSystem = new cvMeshSimMeshSystem();
    if ((*pMeshKernelRegistryMethod)( cvMeshObject::KERNEL_MESHSIM, meshSimSystem ) == CV_OK) {
      //printf("  MeshSim module registered\n");
      Tcl_CreateCommand( interp, "meshsim_mesh_available", MeshSimMesh_AvailableCmd,
		         (ClientData)NULL, (Tcl_CmdDeleteProc *)NULL );
    }
  }
  else {
    return TCL_ERROR;
  }

  return TCL_OK;
}

int MeshSimMesh_AvailableCmd( ClientData clientData, Tcl_Interp *interp,
		      int argc, CONST84 char *argv[] )
{
  Tcl_SetResult( interp, "MeshSim Mesh Module Available", TCL_VOLATILE );

  return TCL_OK;
}
