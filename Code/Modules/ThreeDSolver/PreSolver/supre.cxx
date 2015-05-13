/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved.
 *
 * Portions of the code Copyright (c) 2009-2011 Open Source Medical
 * Software Corporation, University of California, San Diego.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson.
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

#include "cvFlowsolverOptions.h"

#include <stdlib.h>
#include <stdio.h>
#ifndef WIN32
#include "sys/param.h"
#else
#define MAXPATHLEN 1024
#endif
#include <string.h>

#include "cmd.h"
#include <vector>
using namespace std;
// globals
char* oformat = "binary";
//char* oformat = "ascii";
int verbose_ = 0;
int cvsolver_node_order_ = 0;

int numNodes_= 0;
int numElements_ = 0;
int numMeshEdges_ = 0;
int numMeshFaces_ = 0;
int numSolnVars_ = 5;
int numBoundaryFaces_ = 0;
double* nodes_ = NULL;
int* elements_ = NULL;
int** boundaryElements_ = NULL;
int* boundaryElementsIds_ = NULL;
int* xadj_ = NULL;
int xadjSize_ = 0;
int* adjncy_ = NULL;
int adjncySize_ = 0;
int* iBC_ = NULL;
int* iBCB_ = NULL;
double* BCB_ = NULL;
double  init_p_ = 0.0;
double  init_v_[3];
double* soln_ = NULL;
double* dispsoln_ = NULL;
#if(VER_VARWALL == 1)
//variable wall thickness and Young Mod, all nodes
double* wallpropsoln_ = NULL;
#endif
double* acc_ = NULL;
#if(VER_VARWALL == 1)
//dirichlet BCs used in Laplace Eqn for the wallthickness
double* gBC_ = NULL;
#endif

int     DisplacementNumElements_ = 0;
int*    DisplacementConn_[3];
int     DisplacementNumNodes_    = 0;
int*    DisplacementNodeMap_     = NULL;
double* DisplacementSolution_    = NULL;
#if(VER_VARWALL == 1)
//variable wall thickness and Young Mod, deformable wall nodes, like DisplacementSolution_
double* ThicknessSolution_   = NULL;
double* EvwSolution_   = NULL;
#endif
double  Displacement_Evw_        = 0;
double  Displacement_nuvw_       = 0;
double  Displacement_thickness_  = 0;
double  Displacement_kcons_      = 0;
double  Displacement_pressure_   = 0;

double rho_=0.0;
double mu_=0.0;
int bctShape_=0;
double bctPeriod_=0.0;
int bctPointNum_=0;
int bctModeNum_=0;
int bctPreserve_=1;
int bctFlip_=0;
int bctMerge_=0;
int bctNodeNumTotal_=0;
int bctPointNumMax_=0;

vector<BCTData> vbct;

int main(int argc, char *argv[]) {

  // default initial velocity
  init_v_[0] = 0.0001;
  init_v_[1] = 0.0001;
  init_v_[2] = 0.0001;
  // default initial pressure
  init_p_ = 0.0;

  char logname[MAXPATHLEN];
  char mname[MAXPATHLEN];
  char debug_file[MAXPATHLEN];
  char s[MAXCMDLINELENGTH];
  int stat = 0;

  stddbg = stdout;
  //stddbg = NULL;

  logname[0]='\0';
  mname[0]='\0';
  debug_file[0]='\0';

  if (argc != 2) {
      fprintf(stdout,"usage: svpre <params_file>\n");
      exit(-1);
  }

  //check to make sure file exists! 
  debugprint(stddbg,"attempt to open [%s]\n",argv[1]);
  FILE *fp = fopen (argv[1], "r");
  if (fp == NULL) {
       fprintf(stderr,"ERROR opening file %s.\n", argv[1]);
       exit(-1);
  }
    
  // set the input to the cmd file
  cmd_set_input (0, fp);

  int cmd_number=0;
  s[0]='\0';

  while (fgets (s, MAXCMDLINELENGTH, fp) != (char *)0) {

      fprintf(stdout,"LINE %.4i: %.60s\n",cmd_number,s);
      if (cmd_proc (s, &stat) == CV_ERROR) {
        fprintf(stderr,"ERROR:  command could not be processed.\n");
        fclose(fp);
        exit(-1);
      }
      cmd_number++;

      s[0]='\0';
  }

  fclose (fp);

  return 0;

}


