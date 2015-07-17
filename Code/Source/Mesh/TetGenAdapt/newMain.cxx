/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved. 
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
 *
 *=========================================================================*/

#include "cvTetAdaptCore.h" 

#include "cvAdaptObject.h"
#include "cvTetGenAdapt.h"
#include "AdaptHelpers.h"

#include "vtkXMLUnstructuredGridReader.h"
#include "vtkXMLUnstructuredGridWriter.h"
#include "vtkXMLPolyDataReader.h"
#include "vtkXMLPolyDataWriter.h"
#include "vtkGradientFilter.h"
#include "vtkDoubleArray.h"
#include "vtkPointData.h"
#include "vtkSmartPointer.h"

#ifdef WIN32
#include <windows.h>
#include <tchar.h>
#include <stdio.h>
#endif

// main routine to control mesh adaptation, solution transfer

#include <iostream>

char argv0_[1024];
void printUsage(char* argv0) {
    printf("usage : %s\n",argv0);
    printf("\n");  
    printf("        -surface_mesh_file     : input surface mesh file (e.g. geom.vtp) \n");
    printf("        -mesh_file             : input mesh file     (e.g. geom.vtu) \n");
    printf("        -solution_file         : input solution      (e.g. restart.x.1) \n");
    printf("        -error_indicator_file  : error file          (e.g. ybar.x.0) \n");
    printf("        -out_surface_mesh_file : adapted surface mesh(e.g. adapted.vtp) \n");
    printf("        -out_mesh_file         : adapted mesh        (e.g. adapted.vtu) \n");
    printf("        -out_solution_file     : adapted restart     (e.g. adapted-restart.x.1) \n");
    printf("        -out_sn                : output step number  (e.g. reset to zero)\n"); 
    printf("        -strategy              : always set to 1     (default is 1)\n");
    printf("        -nvar                  : number of soln vars (default is 5)\n");
    printf("        -ratio                 : error tolerance\n");
    printf("        -hmax                  : max edge length allowed\n");
    printf("        -hmin                  : min edge length allowed\n");
    fflush(stdout);
    return;
}

int main(int argc,char* argv[])
{

  ios::sync_with_stdio();

  argv0_[0] = '\0';
  strcpy(argv0_,argv[0]);

  // variables used in processing the commandline
  int iarg, arglength;
  std::string tmpstr;

  // must specify options
  if (argc <= 1) {
    printUsage(argv0_);
    exit(0);
  }

  //Tcl_interp *interp;
  cvAdaptObject *Adaptor;
  Adaptor = cvAdaptObject::ExecutableAdaptObject(KERNEL_TETGEN);

  int sn=0;
  int strategy=1;
  double ratio=0.2;
  int nvar=5;
  double hmax=1;
  double hmin=1;
  char surface_mesh_file_[1024];
  char mesh_file_[1024];
  char solution_file_[1024];
  char error_indicator_file_[1024];
  char out_surface_mesh_file_[1024];
  char out_mesh_file_[1024];
  char out_solution_file_[1024];

  /* argc is the number of strings on the command-line */
  /*  starting with the program name */
  for(iarg=1; iarg<argc; iarg++){
    arglength = strlen(argv[iarg]);
    tmpstr.replace(0,arglength,argv[iarg],0,arglength);
    if(tmpstr == "-h" || tmpstr == "--help"){
      printUsage(argv0_);
      exit(0);
    } else if (tmpstr=="-surface_mesh_file") {
      iarg++;
      surface_mesh_file_[0]='\0';
      strcpy(surface_mesh_file_,argv[iarg]);
      Adaptor->LoadModel(surface_mesh_file_);
    } else if (tmpstr=="-mesh_file") {
      iarg++;
      mesh_file_[0]='\0';
      strcpy(mesh_file_,argv[iarg]);
      Adaptor->LoadMesh(mesh_file_);
    } else if (tmpstr=="-solution_file") {
      iarg++;
      solution_file_[0]='\0';
      strcpy(solution_file_,argv[iarg]);
    } else if (tmpstr=="-error_indicator_file") {
      iarg++;
      error_indicator_file_[0]='\0';
      strcpy(error_indicator_file_,argv[iarg]);
    } else if (tmpstr=="-out_surface_mesh_file") {
      iarg++;
      out_surface_mesh_file_[0]='\0';
      strcpy(out_surface_mesh_file_,argv[iarg]);
    } else if (tmpstr=="-out_mesh_file") {
      iarg++;
      out_mesh_file_[0]='\0';
      strcpy(out_mesh_file_,argv[iarg]);
    } else if (tmpstr=="-out_solution_file") {
      iarg++;
      out_solution_file_[0]='\0';
      strcpy(out_solution_file_,argv[iarg]);
    } else if (tmpstr=="-out_sn") {
      iarg++;
      sn = atoi(argv[iarg]);
      Adaptor->SetAdaptOptions("sn",sn);
    } else if (tmpstr=="-strategy") {
      iarg++;
      strategy = atoi(argv[iarg]);
      Adaptor->SetAdaptOptions("strategy",strategy);
    } else if (tmpstr=="-ratio") {
      iarg++;
      ratio = atof(argv[iarg]);
      Adaptor->SetAdaptOptions("ratio",ratio);
    } else if (tmpstr=="-nvar") {
      iarg++;
      nvar = atoi(argv[iarg]);
      Adaptor->SetAdaptOptions("nvar",nvar);
    } else if (tmpstr=="-hmax") {
      iarg++;
      hmax = atof(argv[iarg]);
      Adaptor->SetAdaptOptions("hmax",hmax);
    } else if (tmpstr=="-hmin") {
      iarg++;
      hmin = atof(argv[iarg]);
      Adaptor->SetAdaptOptions("hmin",hmin);
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

//  FILE *simvascularstdout;
//  FILE *simvascularstderr;
//
//#ifndef DO_NOT_BUILD_WITH_STDOUT_STDERR_REDIRECT
//  simvascularstdout = freopen( "run_adaptor.log", "w", stdout );
//  // Note: freopen is deprecated; consider using freopen_s instead
//
//  if( simvascularstdout == NULL ) {
//    fprintf( stdout, "error on reassigning stdout\n" );
//    fflush ( stdout );
//  }
//  simvascularstderr = freopen( "adaptor.stderr", "w", stderr );
//  // Note: freopen is deprecated; consider using freopen_s instead
//
//  if( simvascularstderr == NULL ) {
//    fprintf( stderr, "error on reassigning stderr\n" );
//    fflush ( stderr );
//  }
//#endif

  //Set Error based on input strategy
  Adaptor->SetErrorMetric(solution_file_);
  //Set mesh using given options and for mesh kernel
  Adaptor->SetupMesh();
  //Adapt
  Adaptor->RunAdaptor();
  //Post-process the mesh from the mesh kernel and transfer back
  Adaptor->GetAdaptedMesh();
  Adaptor->PrintStats();
  //Transfer Solution
  Adaptor->TransferSolution();
  //WriteMeshFiles
  Adaptor->WriteAdaptedModel(out_surface_mesh_file_);
  Adaptor->WriteAdaptedMesh(out_mesh_file_);
  Adaptor->WriteAdaptedSolution(out_solution_file_);

  //delete [] Adaptor;
  return 1;
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
