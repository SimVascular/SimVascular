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

#include "cv_misc_utils.h"
#include "cv_arg.h"

#include "cvMeshSimMeshSystem.h"
#include "cv_meshsim_mesh_init.h"

#include "cv_globals.h"
#include "Python.h"


#ifdef SV_USE_PARASOLID
#include "SimParasolidKrnl.h"
#include "SimParasolidInt.h"
#endif

#include "SimAdvMeshing.h"

#include "MeshSim.h"
#include "SimError.h"
#include "SimErrorCodes.h"
#include "SimMeshingErrorCodes.h"

void SimVascularMeshSimMessageHandler(int type, const char *msg);

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
    return Py_ERROR;
  }

  returnStatus2 = RegQueryValueEx(hKey2, regVarName, NULL, &dwType2,(LPBYTE)&lszValue2, &dwSize2);
  RegCloseKey(hKey2);

  if (returnStatus2 != ERROR_SUCCESS) {
    fprintf(stdout,"  Warning: Invalid application registry (%s).\n\n",regVarName);
    fflush(stdout);
    return Py_ERROR;
  }

  // set the variable in tcl interpreter
  sprintf(simkey,"%s",lszValue2);

  Sim_registerKey(simkey);

  return Py_OK;

}

#endif
#endif

PyObject* MeshSimMesh_AvailableCmd(PyObject* self,PyObject* args);
PyMethodDef MeshSim_methods[]=
{
  {"meshsim_available",MeshSimMesh_AvailableCmd,METH_NOARGS,NULL},
  {NULL, NULL}
};

// Initialization routine when the library is loaded via Tcl

PyObject*  Meshsimmesh_pyInit()
{
  try
  {
  // Initialize the MeshSim libraries
    printf("  %-12s %s\n", "MeshSim:", MS_version());
    MS_init();
    Sim_setMessageHandler(SimVascularMeshSimMessageHandler);
  } catch (...) {
   fprintf(stdout,"  ERROR Initializng MeshSim.\n");
   return Py_BuildValue("s","success");
 }
#if defined(MESHSIM_LICENSE_IN_WIN32_REGISTRY)
 try
 {
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_ATTRIBUTES");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_PARASOLID");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_DISCRETE");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_GMIMPORT");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_SURFACE");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_VOLUME");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_EXPORT");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_ADV");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_ADAPT");
} catch (...) {
 fprintf(stdout,"  ERROR MeshSim Registry.\n");
 return Py_BuildValue("s","success");
}
#elif defined(MESHSIM_USE_LICENSE_FILE)
try
{

  Sim_readLicenseFile(NULL);

} catch (...) {
 fprintf(stdout,"  Note: Missing or expired MeshSim License. This causes a General error.\n");
 fprintf(stdout,"  This means MeshSim functionality will be unavailable.\n");
 return Py_BuildValue("s","success");
}
#elif defined(MESHSIM_EMBED_LICENSE_KEYS)
#include "../../../Licenses/MeshSim/meshsim_license.h"
try
{
  Sim_registerKey(MESHSIM_KEY_ATTRIBUTES);
  Sim_registerKey(MESHSIM_KEY_PARASOLID);
  Sim_registerKey(MESHSIM_KEY_DISCRETE);
  Sim_registerKey(MESHSIM_KEY_GMIMPORT);
  Sim_registerKey(MESHSIM_KEY_SURFACE);
  Sim_registerKey(MESHSIM_KEY_VOLUME);
  Sim_registerKey(MESHSIM_KEY_EXPORT);
  Sim_registerKey(MESHSIM_KEY_ADV);
  Sim_registerKey(MESHSIM_KEY_ADAPT);
} catch (...) {
 fprintf(stdout,"  ERROR MeshSim Embeded Keys.\n");
 return Py_BuildValue("s","success");
}
#else
fprintf(stdout,"ERROR: need to register license keys somehow!\n");
return Py_ERROR;
#endif
try{
  SimModel_start();
  #ifdef SV_USE_PARASOLID
  if (SimParasolid_start( 0 ) != 0) {
   fprintf(stdout,"ERROR starting MeshSim Parasolid Interface!\n");
   return Py_ERROR;
 }
  #endif

 SimAdvMeshing_start();
}
catch (...) {
 fprintf(stdout,"  ERROR starting MeshSim.  Will not be available!\n");
 return Py_BuildValue("s","success");
}
	// Get the main mesh
	MeshKernelRegistryMethodPtr pMeshKernelRegistryMethod =
    (MeshKernelRegistryMethodPtr)PySys_GetObject("MeshSystemRegistrar");

  if (pMeshKernelRegistryMethod != NULL) {
    cvMeshSystem* meshSimSystem = new cvMeshSimMeshSystem();
    if ((*pMeshKernelRegistryMethod)( cvMeshObject::KERNEL_MESHSIM, meshSimSystem ) != SV_OK) {
      //printf("  MeshSim module registered\n");
    return Py_ERROR;
    }
  }
  else {
    return Py_ERROR;
  }
  PyObject* pythonC;
  pythonC = Py_InitModule("pyMeshSim",MeshSim_methods);
  if (pythonC==NULL)
  {
    fprintf(stdout,"Error in intializing pyMeshSim\n");
    return Py_ERROR;
  }
  return pythonC;
}


PyMODINIT_FUNC  initpyMeshSim()
{
  try
  {
  // Initialize the MeshSim libraries
    printf("  %-12s %s\n", "MeshSim:", MS_version());
    MS_init();
    Sim_setMessageHandler(SimVascularMeshSimMessageHandler);
  } catch (...) {
   fprintf(stdout,"  ERROR Initializng MeshSim.\n");
   return;
 }
#if defined(MESHSIM_LICENSE_IN_WIN32_REGISTRY)
 try
 {
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_ATTRIBUTES");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_PARASOLID");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_DISCRETE");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_GMIMPORT");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_SURFACE");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_VOLUME");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_EXPORT");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_ADV");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_ADAPT");
} catch (...) {
 fprintf(stdout,"  ERROR MeshSim Registry.\n");
 return;
}
#elif defined(MESHSIM_USE_LICENSE_FILE)
try
{

  Sim_readLicenseFile(NULL);

} catch (...) {
 fprintf(stdout,"  Note: Missing or expired MeshSim License. This causes a General error.\n");
 fprintf(stdout,"  This means MeshSim functionality will be unavailable.\n");
 return;
}
#elif defined(MESHSIM_EMBED_LICENSE_KEYS)
#include "../../../Licenses/MeshSim/meshsim_license.h"
try
{
  Sim_registerKey(MESHSIM_KEY_ATTRIBUTES);
  Sim_registerKey(MESHSIM_KEY_PARASOLID);
  Sim_registerKey(MESHSIM_KEY_DISCRETE);
  Sim_registerKey(MESHSIM_KEY_GMIMPORT);
  Sim_registerKey(MESHSIM_KEY_SURFACE);
  Sim_registerKey(MESHSIM_KEY_VOLUME);
  Sim_registerKey(MESHSIM_KEY_EXPORT);
  Sim_registerKey(MESHSIM_KEY_ADV);
  Sim_registerKey(MESHSIM_KEY_ADAPT);
} catch (...) {
 fprintf(stdout,"  ERROR MeshSim Embeded Keys.\n");
 return;
}
#else
fprintf(stdout,"ERROR: need to register license keys somehow!\n");
return;
#endif
try{
  SimModel_start();
  #ifdef SV_USE_PARASOLID
  if (SimParasolid_start( 0 ) != 0) {
   fprintf(stdout,"ERROR starting MeshSim Parasolid Interface!\n");
   return;
 }
  #endif

 SimAdvMeshing_start();
}
catch (...) {
 fprintf(stdout,"  ERROR starting MeshSim.  Will not be available!\n");
 return;
}
	// Get the main mesh
	MeshKernelRegistryMethodPtr pMeshKernelRegistryMethod =
    (MeshKernelRegistryMethodPtr)PySys_GetObject("MeshSystemRegistrar");

  if (pMeshKernelRegistryMethod != NULL) {
    cvMeshSystem* meshSimSystem = new cvMeshSimMeshSystem();
    if ((*pMeshKernelRegistryMethod)( cvMeshObject::KERNEL_MESHSIM, meshSimSystem ) != SV_OK) {
      //printf("  MeshSim module registered\n");
    return;
    }
  }
  else {
    return;
  }
  PyObject* pythonC;
  pythonC = Py_InitModule("pyMeshSim",MeshSim_methods);
  if (pythonC==NULL)
  {
    fprintf(stdout,"Error in intializing pyMeshSim\n");
    return;
  }
}
PyObject*  MeshSimMesh_AvailableCmd(PyObject* self, PyObject* args)
{
  PyErr_SetString(PyRunTimeErr, "MeshSim Mesh Module Available");

  return Py_BuildValue("s","success");
}
