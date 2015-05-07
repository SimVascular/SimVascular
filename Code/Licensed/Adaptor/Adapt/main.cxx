/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved.
 *
 * Portions of the code Copyright (c) 2009-2011 Open Source Medical
 * Software Corporation, University of California, San Diego.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University, 
 * RPI, Charles Taylor, Ken Jansen, Nathan Wilson, Ken Wang.
 *
 * Redistribution and use in source and binary forms, with or without 
 * modification, are permitted provided that the following conditions 
 * are met:

 * Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer. 
 * Redistributions in binary form must reproduce the above copyright 
 * notice, this list of conditions and the following disclaimer in the 
 * documentation and/or other materials provided with the distribution. 
 * Neither the name of the Stanford University or Rensselaer Polytechnic
 * Institute nor the names of its contributors may be used to endorse or
 * promote products derived from this software without specific prior 
 * written permission.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS 
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE 
 * COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, 
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, 
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS
 * OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF
 * THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 *
 *=========================================================================*/

#include "vtkSmartPointer.h"
#include "vtkPointLocator.h"
#include "vtkXMLUnstructuredGridWriter.h"

#include "cvAdaptCore.h" 

#ifdef USE_PARASOLID
  #include "cv_parasolid_utils.h"
  #include "parasolid_kernel.h"
  #include "kernel_interface.h"
#endif

#ifdef USE_PARASOLID
  #include "SimError.h"
  #include "SimErrorCodes.h"
  #include "SimParasolidInt.h"
  #include "SimParasolidKrnl.h"
#endif

#ifdef USE_DISCRETE_MODEL
  #include "SimError.h"
  #include "SimErrorCodes.h"
  #include "MeshSim.h"
  #include "SimModel.h"
  #include "SimDiscrete.h"
#endif

#include "cvMeshSimMeshObject.h"

#ifdef WIN32
#include <windows.h>
#include <tchar.h>
#endif
#include <stdio.h>
#include <cstring>

// main routine to control mesh adaptation, solution transfer

#include <iostream>

class cvAdapt : public cvAdaptCore {

  public:

  cvAdapt() {
    CBcounter=0;
    CBinvalidRegionCount=0; 
    numVars = 5;
    //const char* outiformat="binary";
    iformat="binary";
    model_file_[0]='\0';
    mesh_file_[0]='\0';
    solution_file_[0]='\0';
    error_indicator_file_[0]='\0';
    out_mesh_file_[0]='\0';
    out_mesh_vtu_file_[0]='\0';
    out_solution_file_[0]='\0';
    sphere_[0] = -1;
    adaptSimLog.open("adaptor.log");
  }

  ~cvAdapt() {
    adaptSimLog.close();
  }

  int Adapt(int,int,double,int,int,double,double,int option, int discreteFlag); 

  char model_file_[1024];
  char mesh_file_[1024];
  char solution_file_[1024];
  char error_indicator_file_[1024];
  char out_mesh_file_[1024];
  char out_mesh_vtu_file_[1024];
  char out_solution_file_[1024];

  double sphere_[5];
 
};

char argv0_[1024];

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

  // we first assume that we are running on a 32-bit OS
  returnStatus2 = RegOpenKeyEx(HKEY_LOCAL_MACHINE, "SOFTWARE\\SIMVASCULAR\\LICENSES", 0L,  KEY_READ, &hKey2);

  // if 32-bit check fails, we check for the key hidden on a 64-bit OS
  // if this fails, then just give up and go home
  if (returnStatus2 != ERROR_SUCCESS) {
    returnStatus2 = RegOpenKeyEx(HKEY_LOCAL_MACHINE, "SOFTWARE\\Wow6432Node\\SIMVASCULAR\\LICENSES", 0L,  KEY_READ, &hKey2);
  }
  if (returnStatus2 != ERROR_SUCCESS) {
    fprintf(stderr,"FATAL ERROR: SIMVASCULAR registry error!\n");
    exit(-1);
  }
   
  returnStatus2 = RegQueryValueEx(hKey2, regVarName, NULL, &dwType2,(LPBYTE)&lszValue2, &dwSize2);
  RegCloseKey(hKey2);

  if (returnStatus2 != ERROR_SUCCESS) {
    fprintf(stdout,"  FATAL ERROR: Invalid application registry (%s).\n\n",regVarName);
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

void printUsage(char* argv0) {
    printf("usage : %s\n",argv0);
    printf("\n");  
    printf("        -model_file            : input solid model   (e.g. geom.xmt_txt) \n");
    printf("        -mesh_file             : input mesh file     (e.g. geom.sms) \n");
    printf("        -solution_file         : input solution      (e.g. restart.x.1) \n");
    printf("        -error_indicator_file  : error file          (e.g. ybar.x.0) \n");
    printf("        -out_mesh_file         : adapted mesh        (e.g. adapted.sms) \n");
    printf("        -out_mesh_vtu_file     : adapted mesh        (e.g. adapted.vtu) \n");
    printf("        -out_solution_file     : adapted restart     (e.g. adapted-restart.x.1) \n");
    printf("        -out_sn                : output step number  (e.g. reset to zero)\n");
    printf("        -nvar                  : number of soln vars (default is 5)\n");
    printf("        -ratio                 : error tolerance\n");
    printf("        -hmax                  : max edge length allowed\n");
    printf("        -hmin                  : min edge length allowed\n");
    printf("        -discrete_model_flag   : 0 == parasolid, 1 == discrete model (default is parasolid)\n");
    printf("        -sphere_refinement     : sphereR sphereX sphereY sphereZ sphereH (default is disabled)\n\n");
    fflush(stdout);
    /*
    printf("\n");
    printf("        - the Error Indicators (EI) are read from:\n");
    printf("          ybar.stepNumber.0 or error.stepNumber.1\n");
    printf("          (depending upon selectedStrategy)\n");
    printf("        - solution is read from restart.sn.0\n");
    printf("\n");
    printf("        - numberVariables : number of vars. for error indicators\n");
    printf("          (5:for ybar & 10:for residual based errors)\n");
    printf("        - reductionRatio : constant in error indicator\n");
    printf("          (for isotropic defines threshold for refinement)\n");
    printf("          (for anistropic defines error tolerance)\n");
    printf("        - hmax & hmin : used for anisotropic adaptation\n");
    printf("          max. and min. edge length allowed, respectively\n");
    printf("        - MaxCoarseFactor & MaxRefineFactor : used for size-field driven isotropic adaptation\n");
    printf("          MaxCoarseFactor (>1.) : h^(new)/h^(old) (how much coarsening)\n");
    printf("          MaxRefineFactor (>1.) : h^(old)/h^(new) (how much refinement)\n\n");
    printf("        - discreteFlag : 0 if parasolid, 1 if discrete model\n");
    printf("        - sphere_refinement : Radius and center (XYZ) of a sphere to fix refinement (H) to sphere\n");
    */
    return;
}

int main(int argc,char* argv[])
{

  ios::sync_with_stdio();

  argv0_[0] = '\0';
  strcpy(argv0_,argv[0]);

  // variables used in processing the commandline
  int iarg, arglength;
  string tmpstr;

  // must specify options
  if (argc <= 1) {
    printUsage(argv0_);
    exit(0);
  }

  // some default options, some good, some bad

  cvAdapt* adaptor = new cvAdapt();

  // R, ctrX, ctrY, ctrZ, h
  adaptor->sphere_[0] = -1;
  adaptor->sphere_[1] = 0;
  adaptor->sphere_[2] = 0;
  adaptor->sphere_[3] = 0;
  adaptor->sphere_[4] = 1;

  int sn=0;
  double ratio=0.2;
  int nvar=5;
  double hmax=1;
  double hmin=1;
  int discreteFlag=0;

  /* argc is the number of strings on the command-line */
  /*  starting with the program name */
  for(iarg=1; iarg<argc; iarg++){
    arglength = strlen(argv[iarg]);
    tmpstr.replace(0,arglength,argv[iarg],0,arglength);
    if(tmpstr == "-h" || tmpstr == "--help"){
      printUsage(argv0_);
      exit(0);
    } else if (tmpstr=="-model_file") {
      iarg++;
      adaptor->model_file_[0]='\0';
      strcpy(adaptor->model_file_,argv[iarg]);
    } else if (tmpstr=="-mesh_file") {
      iarg++;
      adaptor->mesh_file_[0]='\0';
      strcpy(adaptor->mesh_file_,argv[iarg]);
    } else if (tmpstr=="-solution_file") {
      iarg++;
      adaptor->solution_file_[0]='\0';
      strcpy(adaptor->solution_file_,argv[iarg]);
    } else if (tmpstr=="-error_indicator_file") {
      iarg++;
      adaptor->error_indicator_file_[0]='\0';
      strcpy(adaptor->error_indicator_file_,argv[iarg]);
    } else if (tmpstr=="-out_mesh_file") {
      iarg++;
      adaptor->out_mesh_file_[0]='\0';
      strcpy(adaptor->out_mesh_file_,argv[iarg]);
    } else if (tmpstr=="-out_mesh_vtu_file") {
      iarg++;
      adaptor->out_mesh_vtu_file_[0]='\0';
      strcpy(adaptor->out_mesh_vtu_file_,argv[iarg]);
    } else if (tmpstr=="-out_solution_file") {
      iarg++;
      adaptor->out_solution_file_[0]='\0';
      strcpy(adaptor->out_solution_file_,argv[iarg]);
    } else if (tmpstr=="-out_sn") {
      iarg++;
      sn = atoi(argv[iarg]);
    } else if (tmpstr=="-ratio") {
      iarg++;
      ratio = atof(argv[iarg]);
    } else if (tmpstr=="-nvar") {
      iarg++;
      nvar = atoi(argv[iarg]);
    } else if (tmpstr=="-hmax") {
      iarg++;
      hmax = atof(argv[iarg]);
    } else if (tmpstr=="-hmin") {
      iarg++;
      hmin = atof(argv[iarg]);
    } else if (tmpstr=="-discrete_model_flag") {
      iarg++;
      discreteFlag = atoi(argv[iarg]); 
    } else if (tmpstr=="-sphere_refinement") {
      iarg++;
      adaptor->sphere_[0] = atof(argv[iarg]);
      iarg++;
      adaptor->sphere_[1] = atof(argv[iarg]);
      iarg++;
      adaptor->sphere_[2] = atof(argv[iarg]);
      iarg++;
      adaptor->sphere_[3] = atof(argv[iarg]);
      iarg++;
      adaptor->sphere_[4] = atof(argv[iarg]);
    } else {
      cout << "invalid command line option (" << tmpstr << ").  Exiting." << endl;
      exit(1);
    }
    /* reset tmpstr for next argument */
    tmpstr.erase(0,arglength);
  }
 
#ifdef WIN32

#ifdef SIMVASCULAR_USE_WIN32_REGISTRY
  HKEY hKey2;
  LONG returnStatus2;
  DWORD dwType2=REG_SZ;
  DWORD dwSize2=255;
  char lszValue2[255];
  lszValue2[0]='\0';

  char mykey[1024];
  mykey[0]='\0';
  sprintf(mykey,"%s\\%s %s","SOFTWARE\\SIMVASCULAR",SIMVASCULAR_VERSION,SIMVASCULAR_MAJOR_VER_NO);

  // we first assume that we are running on a 32-bit OS
  returnStatus2 = RegOpenKeyEx(HKEY_LOCAL_MACHINE, mykey, 0L,  KEY_READ, &hKey2);
  // if that fails, we check for the key hidden on a 64-bit OS
  // if this fails, then just give up and go home
  if (returnStatus2 != ERROR_SUCCESS) {
    mykey[0]='\0';
    sprintf(mykey,"%s\\%s %s","SOFTWARE\\Wow6432Node\\SIMVASCULAR",SIMVASCULAR_VERSION,SIMVASCULAR_MAJOR_VER_NO);
    //fprintf(stdout,"%s\n\n",mykey);
    returnStatus2 = RegOpenKeyEx(HKEY_LOCAL_MACHINE, mykey, 0L,  KEY_READ, &hKey2);
  }
  if (returnStatus2 != ERROR_SUCCESS) {
    fprintf(stderr,"FATAL ERROR: SimVascular registry error!\n");
    exit(-1);
  }
   
  returnStatus2 = RegQueryValueEx(hKey2, "RunDir", NULL, &dwType2,(LPBYTE)&lszValue2, &dwSize2);
  RegCloseKey(hKey2);

  if (returnStatus2 != ERROR_SUCCESS) {
    fprintf(stdout,"  FATAL ERROR: Invalid application registry.  SimVascular RunDir not found!\n");
    exit(-1);
  }

  //printf("Value Read is %s\n", lszValue2);
  int envi = 0;
  for (envi = 0; envi < strlen(lszValue2);envi++) {
    //if (lszValue2[envi]=='\\') lszValue2[envi]='/';
  }

   // set the environment variables using the registry
   char envvar[4096];
   char newvar[5000];
   size_t requiredSize;

   //
   //  PATH
   //

   envvar[0]='\0';
   getenv_s( &requiredSize, NULL, 0, "PATH");
   if (requiredSize >= 4096) {
     fprintf(stderr,"FATAL ERROR:  path to long!\n");
     exit(-1);
   }
   // Get the value of the LIB environment variable.
   getenv_s( &requiredSize, envvar, requiredSize, "PATH" );

   //if( envvar != NULL )
     //printf( "Original PATH variable is: %s\n", envvar );

   // Attempt to change path. Note that this only affects
   // the environment variable of the current process. The command
   // processor's environment is not changed.
   
   newvar[0]='\0';
   sprintf(newvar,"%s;%s",lszValue2,envvar);
   
   _putenv_s( "PATH", newvar );

   getenv_s( &requiredSize, NULL, 0, "PATH");

   envvar[0]='\0';

   // Get the new value of the LIB environment variable. 
   getenv_s( &requiredSize, envvar, requiredSize, "PATH" );

   //if( envvar != NULL )
     //printf( "New PATH variable is: %s\n", envvar );

   //
   //  SIMVASCULAR_HOME
   //

   envvar[0]='\0';
   getenv_s( &requiredSize, NULL, 0, "SIMVASCULAR_HOME");
   if (requiredSize >= 4096) {
     fprintf(stderr,"FATAL ERROR:  TCL_LIBRARY to long!\n");
     exit(-1);
   }

   if (requiredSize > 0) {
     // Get the value of the simvascular_home environment variable.
     getenv_s( &requiredSize, envvar, requiredSize, "SIMVASCULAR_HOME" );
     //if( envvar != NULL )
       //printf( "Original SIMVASCULAR_HOME variable is: %s\n", envvar );
   }

   // Attempt to change tcl_library. Note that this only affects
   // the environment variable of the current process. The command
   // processor's environment is not changed.
   
   newvar[0]='\0';
   sprintf(newvar,"%s",lszValue2);
   
   _putenv_s( "SIMVASCULAR_HOME", newvar );

   getenv_s( &requiredSize, NULL, 0, "SIMVASCULAR_HOME");

   envvar[0]='\0';

   // Get the new value of the SIMVASCULAR_HOME environment variable. 
   getenv_s( &requiredSize, envvar, requiredSize, "SIMVASCULAR_HOME" );

   //if( envvar != NULL )
     //printf( "New SIMVASCULAR_HOME variable is: %s\n", envvar );

   //
   //  P_SCHEMA
   //

   envvar[0]='\0';
   getenv_s( &requiredSize, NULL, 0, "P_SCHEMA");
   if (requiredSize >= 4096) {
     fprintf(stderr,"FATAL ERROR:  p_schema to long!\n");
     exit(-1);
   }
   if (requiredSize > 0) {
     // Get the value of the p_schema environment variable.
     getenv_s( &requiredSize, envvar, requiredSize, "P_SCHEMA" );

     //if( envvar != NULL )
     // printf( "Original P_SCHEMA variable is: %s\n", envvar );
   }

   // Attempt to change p_schema. Note that this only affects
   // the environment variable of the current process. The command
   // processor's environment is not changed.
   
   newvar[0]='\0';
   sprintf(newvar,"%s/schema",lszValue2);
   
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

#endif
 
  // step number, i.e., time step number in phasta filename
  //int sn=atoi(argv[7]);
  // strategy is to specify how to do adaptation 
  // (i.e., size-field or tag driven)
  // 1-2 : size-field driven (for anisotropic) 
  // 3-4 : tag driven (for isotropic)
  // 5-6 : size-field driven (for isotropic)
  // < 0 sets a manual mesh size-field
  //int strategy=atoi(argv[8]);
  // number of variables for error indicators (EI)
  // (e.g., 5 for ybar & 10 for residual-based)
  //int nvar=atoi(argv[9]);
  // factor is the constant appearing in error indicator/estimator
  // for isotropic it is used to define the threshold for refinement
  // for anisotropic it is used to define error tolerance (in a way)
  //double ratio=atof(argv[10]);
  // max. allowable edge length
  // needed in case hessian is singular and eigenvalues are zero
  //double hmax=atof(argv[11]);
  // min. edge length that user wants, i.e., limit on refinement
  // needed for discontinuous solutions 
  // (esp. for shocks when hessians are not reliable)
  //double hmin=atof(argv[12]);
  // option is to decide how to compute error value
  // 0 : use 1 scalar (like temp.) EI for scalar problems
  // 1 : use 3 momentum EI for flow problems
  //int option=atoi(argv[13]);

  // discrete solid model flag
  //int discreteFlag=atoi(argv[14]);

  // sphere refinement
  //double sphereR=atof(argv[15]);
  //double sphereX=atof(argv[16]);
  //double sphereY=atof(argv[17]);
  //double sphereZ=atof(argv[18]);
  //double sphereH=atof(argv[19]);

  //Sim_logOn("TF_JMSIM.log");

  // this is the right value for spipe example
  // `ratio' : not aspect ratio (ratio to reduce mean error)

  FILE *simvascularstdout;
  FILE *simvascularstderr;

#ifndef DO_NOT_BUILD_WITH_STDOUT_STDERR_REDIRECT
  simvascularstdout = freopen( "run_adaptor.log", "w", stdout );
  // Note: freopen is deprecated; consider using freopen_s instead

  if( simvascularstdout == NULL ) {
    fprintf( stdout, "error on reassigning stdout\n" );
    fflush ( stdout );
  }
  simvascularstderr = freopen( "adaptor.stderr", "w", stderr );
  // Note: freopen is deprecated; consider using freopen_s instead

  if( simvascularstderr == NULL ) {
    fprintf( stderr, "error on reassigning stderr\n" );
    fflush ( stderr );
  }
#endif

  int strategy = 1;
  int option = 1;

  adaptor->Adapt(sn,strategy,ratio,5,nvar,hmax,hmin,option,discreteFlag);

  //Sim_logOff();
  //filestr_stdout.close();
  //filestr_stderr.close();
  //filestr_stdlog.close();

  return 0;
}


#ifdef WIN32
void  bzero(void* ptr, size_t sz) {
    int i;
    char *cptr;
    cptr = (char*) ptr;
    for (i=0; i < sz; i++) {
        cptr[i]=0;
    }
    return;
}
#endif

int cvAdapt::Adapt(//time step
	    int lstep,
	    // strategy is to specify 
	    // how to do adaptation (i.e., size-field or tag driven)
	    // 1-2 : size-field driven (for anisotropic) 
	    // 3-4 : tag driven (for isotropic)
	    // 5-6 : size-field driven (for isotropic)
	    // < 0 sets a manual mesh size-field
	    int strategy,
	    // factor is the constant appearing in the error expression
	    // for tag driven it is used to define threshold for refinement
	    // for size-field driven it is used to define the error tolerance
	    double factor,
	    // number of solution variables (5 for incompressible)
	    int ndof,
	    // number of variables for error indicators (EI)
	    // (e.g., 5 for ybar & 10 for residual-based)
	    int nvar,
	    // the maximal mesh edge length allowed in mesh size 
	    double hmax,
	    // the minimal mesh edge length allowed in mesh size
	    double hmin,
	    // option is used to decide how to compute the error value
	    // provides different choices like analytic hessian, manual size-field etc.
	    // for isotropic (ta or size-field driven :
	    // use 3 EI for flow problem or use 1 EI for scalar problem
	    int option, 
            int discreteFlag)
{  
  // polynomial order
  int poly = 1;   
  // number of nodes (as considering only linear basis)
  int nshg;       

  MS_init();

#if defined(MESHSIM_LICENSE_IN_WIN32_REGISTRY)
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_ATTRIBUTES");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_PARASOLID");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_DISCRETE");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_SURFACE");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_VOLUME");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_EXPORT");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_ADV");
  MeshSim_Win32ReadRegistrySimRegister("MESHSIM_KEY_ADAPT");
#elif defined(MESHSIM_USE_LICENSE_FILE)
  Sim_readLicenseFile(NULL);
#elif defined(MESHSIM_EMBED_LICENSE_KEYS)
#include "../../../Licenses/MeshSim/meshsim_license.h"
  Sim_registerKey(MESHSIM_KEY_ATTRIBUTES);
  Sim_registerKey(MESHSIM_KEY_PARASOLID);
  Sim_registerKey(MESHSIM_KEY_DISCRETE);
  Sim_registerKey(MESHSIM_KEY_SURFACE);
  Sim_registerKey(MESHSIM_KEY_VOLUME);
  Sim_registerKey(MESHSIM_KEY_EXPORT);
  Sim_registerKey(MESHSIM_KEY_ADV);
  Sim_registerKey(MESHSIM_KEY_ADAPT);
#else
  fprintf(stdout,"ERROR: need to register license keys somehow!\n");
  return CV_ERROR;
#endif

  SimModel_start();

  if (!discreteFlag) {

    #ifdef USE_PARASOLID
    PsdUtils_Init();
    if (SimParasolid_start( 0 ) != 0) {
      fprintf(stdout,"ERROR starting MeshSim Parasolid Interface!\n");
      return CV_ERROR;
    }
    #endif

  } else {

    #ifdef USE_DISCRETE_MODEL
    SimDiscrete_start(0);
    #endif

  }

  SimAdvMeshing_start();

  pGModel model;

  if (!discreteFlag) {

    #ifdef USE_PARASOLID
    pProgress progress = Progress_new();
    PK_PART_t firstPart;

    //fprintf(stdout,"model: %s\n",model_file_);

    int isAssembly;
    if (PsdUtils_ReadNative(model_file_,NULL,&firstPart,&isAssembly) != CV_OK) {
      return CV_ERROR;
    }

    pParasolidNativeModel paramodel = NULL;
    paramodel = ParasolidNM_createFromPart(firstPart);
    if (paramodel == NULL) {
      fprintf(stderr,"ERROR: Problem from ParasolidNM_createFromPart.\n");
      fflush(stderr);
      return CV_ERROR;
    } 
    if (!isAssembly) {

      model = GM_createFromNativeModel(paramodel,progress);

    } else {

      pGAModel pGAM = NULL;
      pGAM = GAM_createFromNativeModel(paramodel,progress);
      if (pGAM == NULL) {
        fprintf(stderr,"ERROR: Problem from GM_createFromNativeModel.\n");
        fflush(stderr);
        return CV_ERROR;
      }
      fprintf(stdout,"GAM_numAssemblies(%i)\n",GAM_numAssemblies(pGAM));
      fprintf(stdout,"GAM_numParts(%i)\n",GAM_numParts(pGAM));
      fflush(stdout);
      // what is a connector?
      pMConnector connector = MC_new();
      model = GM_createFromAssemblyModel (pGAM, connector, progress);

    }

    if (model == NULL) {
      fprintf(stderr,"ERROR: Problem from GM_createFromAssemblyModel.\n");
      fflush(stderr);
      return CV_ERROR;
    } 
    NM_release(paramodel);
    Progress_delete(progress);
    #else
    return CV_ERROR;
    #endif
  
  } else {

    #ifdef USE_DISCRETE_MODEL
    // read discrete model
    pProgress progressDM = Progress_new();
    model = DM_load(model_file_, progressDM);
    Progress_delete(progressDM);
    #else
    return CV_ERROR;
    #endif

  }

  pACase mcase = MS_newMeshCase(model);
  pModelItem mdomain = GM_domain(model);

  //pGModel model=GM_createFromParasolidFile(model_file_);

  //pMesh mesh=MS_newMesh(model);

  pMesh mesh;

  pProgress progressML = Progress_new();
  try {
    mesh = M_load(mesh_file_, model, progressML);
  } catch (pSimError err) {
    fprintf(stderr,"Simmetrix error caught:\n");
    fprintf(stderr,"  Error code: %d\n",SimError_code(err));
    fprintf(stderr,"  Error string: %s\n",SimError_toString(err));
    Progress_delete(progressML);
    return CV_ERROR;
  } catch (...) {
    fprintf(stderr,"Unhandled exception caught\n");
    Progress_delete(progressML);
    return CV_ERROR;
  }
  Progress_delete(progressML);

  printf("\n-- Before Adaptation...\n");
  printf(" Total # of elements: %d\n", M_numRegions(mesh));
  printf(" Total # of vertices: %d\n", M_numVertices(mesh));

  cout<<endl;
  cout<<"MeshSim   Library build ID : "<<SimMeshing_buildID()<<endl;
  cout<<"MeshTools Library build ID : "<<SimMeshTools_buildID()<<endl;
  cout<<endl;

  nshg=M_numVertices(mesh);

  //
  //  find the points in the initial mesh
  //

  vtkSmartPointer<vtkPoints> inPoints = 
    vtkSmartPointer<vtkPoints>::New();
  vtkSmartPointer<vtkPolyData> inPD = 
    vtkSmartPointer<vtkPolyData>::New();

  inPoints->SetNumberOfPoints(nshg);

  int i;

  VIter myViter = M_vertexIter(mesh);
  for (i = 0; i < nshg; i++) {
    pPoint point = V_point (VIter_next(myViter));
    //int nodenumber = P_id (point);
    int nodenumber = i + 1;
    P_setID(point,nodenumber);  
    double x = P_x (point);
    double y = P_y (point);
    double z = P_z (point);  
    inPoints->SetPoint(nodenumber-1,x,y,z);
  } // i 
  VIter_delete(myViter);

  inPD->Initialize();
  inPD->SetPoints(inPoints);

  errorIndicatorID = MD_newMeshDataId("error indicator");

  modes = MD_newMeshDataId("number of modes");// required for higher order

  pMSAdapt simAdapter; 

    cout<<"\nStrategy chosen for ANISOTROPIC adaptation : size-field driven"<<endl;
    
    char error_tag[28];
    error_tag[0]='\0';

    cout<<"\nUsing ybar to compute hessians...\n"<<endl;
    sprintf(error_tag,"ybar");
      
    cout<<"\n Reading files:"<<endl;

    // attaching the solution to the original mesh
    cout<<" ..."<<solution_file_<<" (for \"solution\")"<<endl;
    double *sol;
    readArrayFromFile(solution_file_,"solution",sol);
    //   attachArray(sol,mesh,phasta_solution,ndof,poly);

    // read ybar (and compute/use hessians of ybar) 
    cout<<" ..."<<error_indicator_file_<<" (for \""<<error_tag<<"\")"<<endl<<endl;
    double *error_indicator;
    readArrayFromFile(error_indicator_file_,error_tag,error_indicator);
    // undoing change to read only from solution file
    //    readArrayFromFile(solution_file_,error_tag,error_indicator);

    attachArray(error_indicator,mesh,errorIndicatorID,nvar,poly);
    delete [] error_indicator;

    simAdapter = MSA_new(mesh,1);

    // need to use only local refinement if boundary layer exists
    pVertex v;
    VIter vIter=M_vertexIter(mesh);
    while(v = VIter_next(vIter)) {
      if (EN_isBLEntity(v)) {
        MSA_setLocal(simAdapter,1);
        cout<<endl<<" ** boundary layer mesh detected, using local refinement **" << endl <<endl;
        break;
      }
    }
    VIter_delete(vIter);
 
    // calculating hessians for ybar field
    // first reconstruct gradients and then the hessians 
    // also deals with boundary issues &
    // applies smoothing procedure for hessians
    // (simple average : arithmetic mean)

    hessiansFromSolution(mesh,lstep);
    
    // compute mesh size-field using hessian strategy (anisotropic adaptation)
    // and set it at each vertex
    setSizeFieldUsingHessians(mesh,simAdapter,factor,hmax,hmin,option,sphere_);
  

  // data clean up
  cleanAttachedData(mesh,errorIndicatorID,0);
 
  // adaptation

  pProgress progressAdapt = Progress_new();

  MSA_adapt(simAdapter, progressAdapt);

  Progress_delete(progressAdapt);

  pProgress progressFix = Progress_new();
  // 7.0+ version
  // takes case of bad brdy. elements (elements with no interior nodes)
  // is this the replacement for 7+?
  int dimfilter = 12;
  MS_ensureInteriorVertices(mesh,dimfilter,progressFix);
  Progress_delete(progressFix);

  printf("-- Adaptation Done...\n");
  printf(" Total # of elements: %d\n", M_numRegions(mesh));
  printf(" Total # of vertices: %d\n\n", M_numVertices(mesh));
  printf(" Total # of sol. callbacks: %d\n\n",CBcounter);
  printf(" Total # of invalid regions in  callbacks: %d\n\n",CBinvalidRegionCount);

  printf("\n Writing out the mesh...\n\n");
  int smsver = 0;
  pProgress progressWriteMesh = Progress_new();
  M_write(mesh,out_mesh_file_,smsver, progressWriteMesh);
  Progress_delete(progressWriteMesh);

  double xyz[3];
  vtkIdType pointId;
  vtkIdType closestPoint;

  vtkSmartPointer<vtkPoints> outPoints = 
    vtkSmartPointer<vtkPoints>::New();
  vtkSmartPointer<vtkDoubleArray> inSolution = 
    vtkSmartPointer<vtkDoubleArray>::New();
  vtkSmartPointer<vtkDoubleArray> outSolution = 
    vtkSmartPointer<vtkDoubleArray>::New();
  vtkSmartPointer<vtkPointLocator> locator = 
    vtkSmartPointer<vtkPointLocator>::New();

  // build solution vector for old nodes

  cout<<"project solution from old mesh to new mesh using nearest point...\n";

  inSolution->SetNumberOfComponents(nvar);
  inSolution->Allocate(nshg,10000);
  inSolution->SetNumberOfTuples(nshg);

  int count = 0;

  for (pointId = 0;pointId < nshg;pointId++)
  {
    for (i=0;i<nvar;i++)
    { 
      inSolution->InsertComponent(pointId,i,sol[count++]);
    }
  }
  
  int nshg_adapted = M_numVertices(mesh);

  vtkIntArray* gid = vtkIntArray::New();
  gid->SetNumberOfComponents(1);
  gid->Allocate(nshg_adapted,1000);
  gid->SetNumberOfTuples(nshg_adapted);
  gid->SetName("GlobalNodeID");

  outSolution->SetNumberOfComponents(nvar);
  outSolution->Allocate(nshg_adapted,10000);
  outSolution->SetNumberOfTuples(nshg_adapted);
  outSolution->SetName("solution");

  outPoints->SetNumberOfPoints(nshg_adapted);

  myViter = M_vertexIter(mesh);
  for (i = 0; i < nshg_adapted; i++) {
    pPoint point = V_point (VIter_next(myViter));
    //int nodenumber = P_id (point);
    int nodenumber = i + 1;
    P_setID(point,nodenumber);  
    double x = P_x (point);
    double y = P_y (point);
    double z = P_z (point);
    //fprintf(stdout,"%i %lf %lf %lf\n",nodenumber,x,y,z);  
    outPoints->SetPoint(nodenumber-1,x,y,z);
    gid->SetTuple1(nodenumber-1,nodenumber);
  } // i 
  VIter_delete(myViter);

  locator->SetDataSet(inPD);
  locator->BuildLocator();

  count = 0;

  double* adapted_solution = new double[nshg_adapted*nvar];

  for (pointId=0;pointId<nshg_adapted;pointId++)
  {
    outPoints->GetPoint(pointId,xyz);
    closestPoint = locator->FindClosestPoint(xyz);
    for (i=0;i<nvar;i++)
    {
      double solncomp = inSolution->GetComponent(closestPoint,i);
      outSolution->SetComponent(pointId,i,solncomp);
      adapted_solution[i*nshg_adapted+pointId] = solncomp;
    }
  }

  writeArrayToFile(out_solution_file_,"solution","binary","write",
                   nshg_adapted,ndof,lstep,adapted_solution);

  int neltot = M_numRegions(mesh);

  vtkSmartPointer<vtkUnstructuredGrid> ug = 
    vtkSmartPointer<vtkUnstructuredGrid>::New();
  //  vtkSmartPointer<vtkIdList> ptids = 
  //  vtkSmartPointer<vtkIdList>::New();
  vtkIdList* ptids = vtkIdList::New();
 
  vtkSmartPointer<vtkIntArray> eid = 
    vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkIntArray> rid = 
    vtkSmartPointer<vtkIntArray>::New();

  ug->SetPoints(outPoints);
  ug->GetPointData()->AddArray(gid);
  ug->GetPointData()->AddArray(outSolution);

  ptids->Allocate(10,10);
  ptids->Initialize();
  ptids->SetNumberOfIds(4);

  eid->SetNumberOfComponents(1);
  eid->Allocate(neltot,1000);
  eid->Initialize();
  eid->SetName("GlobalElementID");

  rid->SetNumberOfComponents(1);
  rid->Allocate(neltot,1000);
  rid->Initialize();
  rid->SetName("ModelRegionID");

  // only for linear tets

  pRegion myelement = NULL;
  RIter myRIter = M_regionIter(mesh);

  // only allow one model region for now
  int curMdlRegID = 1;

  while ((myelement = RIter_next(myRIter)) != NULL) {
    // the elements are numbered from 1 to N.
    int curElemID = EN_id((pEntity)myelement)+1;
    pPList vert_list = R_vertices (myelement,MY_MESHSIM_VERTEX_ORDERING);
    int num_elem_verts = PList_size (vert_list);
    // must be linear
    if (num_elem_verts != 4) {
      exit(-1);
    }
    for (i = 0; i < num_elem_verts; i++) {
        pVertex vertex = (pVertex)PList_item (vert_list, i);
        // vtk nodes must start at zero
        ptids->SetId(i,P_id(V_point(vertex))-1);
    } // i
    PList_delete(vert_list);      
    ug->InsertNextCell(VTK_TETRA,ptids);
    eid->InsertNextTuple1(curElemID);
    rid->InsertNextTuple1(curMdlRegID);
  }

  ptids->Delete();
   
  ug->GetCellData()->AddArray(rid);
  ug->GetCellData()->SetScalars(eid);
  ug->GetCellData()->SetActiveScalars("GlobalElementID");
  ug->GetCellData()->CopyAllOn();

  if (out_mesh_vtu_file_[0] != '\0') {
    vtkXMLUnstructuredGridWriter *ugwriter = vtkXMLUnstructuredGridWriter::New();
    ugwriter->SetCompressorTypeToZLib();
    //vtkUnstructuredGridWriter *ugwriter = vtkUnstructuredGridWriter::New();
    //ugwriter->SetFileTypeToASCII();
    ugwriter->EncodeAppendedDataOff();
    ugwriter->SetInputDataObject(ug);
    ugwriter->SetFileName(out_mesh_vtu_file_);
    ugwriter->Write();
    ugwriter->Delete();
  }
  //eid->Delete();

  MD_deleteMeshDataId(errorIndicatorID);
  MD_deleteMeshDataId(modes);

  MSA_delete(simAdapter);
  M_release(mesh);

  GM_release(model);

  MS_exit();

  delete [] sol;

  delete [] adapted_solution;

  return 1;
}
 

