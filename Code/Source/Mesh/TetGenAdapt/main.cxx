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

// main routine to control mesh adaptation, solution transfer

#include <iostream>

class cvTetAdapt : public cvTetAdaptCore {

  public:

  cvTetAdapt() {
    CBcounter=0;
    CBinvalidRegionCount=0; 
    numVars = 5;
    iformat="binary";
    surface_mesh_file_[0]='\0';
    mesh_file_[0]='\0';
    solution_file_[0]='\0';
    error_indicator_file_[0]='\0';
    out_surface_mesh_file_[0]='\0';
    out_mesh_file_[0]='\0';
    out_solution_file_[0]='\0';
  }

  ~cvTetAdapt() {
  }

  int Adapt(int,int,double,int,int,double,double); 

  char surface_mesh_file_[1024];
  char mesh_file_[1024];
  char solution_file_[1024];
  char error_indicator_file_[1024];
  char out_surface_mesh_file_[1024];
  char out_mesh_file_[1024];
  char out_solution_file_[1024];
 
};

extern "C" {
  void cstylePhastaTransfer( int mtype, int mco, void *userData) {
    (static_cast<cvTetAdapt*>(userData))->phastaTransfer(mtype,mco,userData);
  }
}

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
    /*
    printf("\n");
    printf("        - the Error Indicators (EI) are read from:\n");
    printf("          ybar.stepNumber.0 or error.stepNumber.1\n");
    printf("          (depending upon selectedStrategy)\n");
    printf("        - solution is read from restart.sn.0\n");
    printf("\n");
    printf("        - selectedStrategy : how to do adaptation\n");
    printf("          (i.e., 1-2 for anisotropic & size-field driven)\n");
    printf("          (i.e., 3-4 for isotropic & tag driven)\n");
    printf("          (i.e., 5-6 for isotropic & size-field driven)\n");
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
  std::string tmpstr;

  // must specify options
  if (argc <= 1) {
    printUsage(argv0_);
    exit(0);
  }

  cvTetAdapt* adaptor = new cvTetAdapt();

  int sn=0;
  int strategy=1;
  double ratio=0.2;
  int nvar=5;
  double hmax=1;
  double hmin=1;

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
      adaptor->surface_mesh_file_[0]='\0';
      strcpy(adaptor->surface_mesh_file_,argv[iarg]);
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
    } else if (tmpstr=="-out_surface_mesh_file") {
      iarg++;
      adaptor->out_surface_mesh_file_[0]='\0';
      strcpy(adaptor->out_surface_mesh_file_,argv[iarg]);
    } else if (tmpstr=="-out_mesh_file") {
      iarg++;
      adaptor->out_mesh_file_[0]='\0';
      strcpy(adaptor->out_mesh_file_,argv[iarg]);
    } else if (tmpstr=="-out_solution_file") {
      iarg++;
      adaptor->out_solution_file_[0]='\0';
      strcpy(adaptor->out_solution_file_,argv[iarg]);
    } else if (tmpstr=="-out_sn") {
      iarg++;
      sn = atoi(argv[iarg]);
    } else if (tmpstr=="-strategy") {
      iarg++;
      strategy = atoi(argv[iarg]);
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

#endif

#endif

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

  fprintf(stderr,"about to adapt\n");
  adaptor->Adapt(sn,strategy,ratio,5,nvar,hmax,hmin);

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

int cvTetAdapt::Adapt(//time step
	    int lstep,
	    // strategy is to specify 
	    // how to do adaptation (i.e., size-field or tag driven)
	    // 1-2 : size-field driven (for isotropic) 
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
	    double hmin)
{  
  // polynomial order
  int poly = 1;   
  // number of nodes (as considering only linear basis)
  int nshg;       

  //Set up the readers, polydata, and unstructured grid for the input 
  //volume mesh and surface mesh
  vtkSmartPointer<vtkUnstructuredGrid> inputug = 
    vtkSmartPointer<vtkUnstructuredGrid>::New();
  vtkSmartPointer<vtkXMLUnstructuredGridReader> ugreader = 
    vtkSmartPointer<vtkXMLUnstructuredGridReader>::New();
  vtkSmartPointer<vtkPolyData> inputpd = 
    vtkSmartPointer<vtkPolyData>::New();
  vtkSmartPointer<vtkXMLPolyDataReader> pdreader = 
    vtkSmartPointer<vtkXMLPolyDataReader>::New();

  ugreader->SetFileName(mesh_file_);
  ugreader->Update();
  inputug = ugreader->GetOutput();

  pdreader->SetFileName(surface_mesh_file_);
  pdreader->Update();
  inputpd = pdreader->GetOutput();

  tetgenio inmesh;
  tetgenio outmesh;
  inmesh.firstnumber = 0;
  outmesh.firstnumber = 0;

  printf("\n-- Before TetGen Adaptation...\n");
  printf(" Total # of elements: %d\n", inputug->GetNumberOfCells());
  printf(" Total # of vertices: %d\n", inputug->GetNumberOfPoints());

  nshg=inputug->GetNumberOfPoints();

  // if strategy<0 (command line argument as of now) 
  switch(strategy) {
  //this code processes for both if strategy == 1 || strategy ==2
  //Right now the only implemented adaptation is for isotropic meshing. 
  //TetGen only has the ability to specify one size metric at each node 
  //within the mesh, so anisotropic meshing is not capable at this moment.
  //Strategies 1 and 2 implement isotropic adaptation 
  case 1 :
  case 2 : { //isotropic adaptation
    cout<<"\nStrategy chosen for ANISOTROPIC adaptation : size-field driven"<<endl;
    
    char error_tag[28];
    if(strategy == 1) {
      cout<<"\nUsing ybar to compute hessians...\n"<<endl;
      sprintf(error_tag,"ybar");
    }
    else if (strategy == 2) {
      cout<<"\nUsing numerical/computed hessians (i.e, from phasta)...\n"<<endl;
      sprintf(error_tag,"hessains");
    }
    
    cout<<"\n Reading file:"<<endl;
    cout<<" ..."<<solution_file_<<" (for \"solution and error\")"<<endl;
    //cout<<" ..."<<error_indicator_file_<<" (for \""<<error_tag<<"\")"<<endl;

    if(strategy==1) {
      // attaching the solution to the original mesh
      double *sol;
      readArrayFromFile(solution_file_,"solution",sol);
      attachArray(sol,inputug,"solution",ndof,poly);
      delete [] sol;

      // read ybar (and compute/use hessians of ybar) 
      double *error_indicator;
      //readArrayFromFile(error_indicator_file_,error_tag,error_indicator);
      readArrayFromFile(solution_file_,error_tag,error_indicator);
      attachArray(error_indicator,inputug,"error",nvar,poly);
      delete [] error_indicator;
      
      // calculating hessians for ybar field
      // first reconstruct gradients and then the hessians 
      // also deals with boundary issues &
      // applies smoothing procedure for hessians
      // (simple average : arithmetic mean)
      hessiansFromSolution(inputug,lstep);
    }
    else if (strategy == 2) { // cannot use analytic hessian in this case
      // use the hessians computed from phasta
      double *hessiansFromRestart;
      //readArrayFromFile(error_indicator_file_,error_tag,hessiansFromRestart);
      readArrayFromFile(solution_file_,error_tag,hessiansFromRestart);
      double *hessians = new double[nshg*6];
      getHessiansFromPhasta(hessiansFromRestart,inputug,nvar,hessians);
      delete [] hessiansFromRestart;
      attachArray(hessians,inputug,"hessians",6,poly);
      delete [] hessians;
      return 0;
    }
    setSizeFieldUsingHessians(inputug,&inmesh,factor,hmax,hmin);
  }
  break;
  case 3:
  case 4: { // anisotropic adaptation (tag driven)
    cout<<"Strategy has not been implemented"<<endl;
    return 0;
  }
  break;
  case 5:
  case 6: { //anisotropic adaptation (size-field driven)
    cout<<"Strategy has not been implemented"<<endl;
    return 0;
  }
  break;
  default : {
    if(strategy<0) {
      cout<<"This is default case but has not been implemented"<<endl;

    }
    else {
      cout<<"\nSpecify a correct (adaptation) strategy (adapt.cc)"<<endl;
      exit(-1);
    }
  }
  break;
  }

  //Convert VTK Structures to tetgenio object for meshing
  convertToTetGen(inputug,inputpd,&inmesh);

  //Run tetgen with specified parameters
  runAdaptor(&inmesh,&outmesh);

  vtkSmartPointer<vtkUnstructuredGrid> outputug = 
    vtkSmartPointer<vtkUnstructuredGrid>::New();
  vtkSmartPointer<vtkPolyData> outputpd = 
    vtkSmartPointer<vtkPolyData>::New();

  //Convert adapted mesh back to VTK Structures
  convertToVTK(outputug,outputpd,&outmesh);

  modelFaceIDTransfer(inputpd,outputpd);

  //Interpolate solution onto new mesh
  fix4SolutionTransfer(inputug,outputug,ndof);

  //Write out new solution file
  vtkSmartPointer<vtkXMLUnstructuredGridWriter> ugwriter =
    vtkSmartPointer<vtkXMLUnstructuredGridWriter>::New();
  ugwriter->SetInputData(outputug);
  ugwriter->SetFileName(out_mesh_file_);
  ugwriter->Write();

  vtkSmartPointer<vtkXMLPolyDataWriter> pdwriter =
    vtkSmartPointer<vtkXMLPolyDataWriter>::New();
  pdwriter->SetInputData(outputpd);
  pdwriter->SetFileName(out_surface_mesh_file_);
  pdwriter->Write();

  printf("-- Adaptation Done...\n");
  printf(" Total # of elements: %d\n", outputug->GetNumberOfCells());
  printf(" Total # of vertices: %d\n\n", outputug->GetNumberOfPoints());

  printf("\n Writing out the mesh...\n\n");

  int nshg_fine = outputug->GetNumberOfPoints();
                 
  //Write out new solution file
  double *solution; 
  getAttachedArray(solution,outputug,"solution",ndof,poly);

  writeArrayToFile(out_solution_file_,"solution","binary","write",
      			nshg_fine,ndof,lstep,solution);

  delete [] solution;

  return 1;
}
 

