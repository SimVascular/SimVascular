/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved.
 *
 * Portions of the code Copyright (c) 2000-2007, Stanford University,
 * Rensselaer Polytechnic Institute, Kenneth E. Jansen,
 * Charles A. Taylor (see SimVascular Acknowledgements file
 * for additional contributors to the source code).
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * Redistributions of source code must retain the above copyright notice,
 * this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditions and the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * Neither the name of the Stanford University or Rensselaer Polytechnic
 * Institute nor the names of its contributors may be used to endorse or
 * promote products derived from this software without specific prior
 * written permission.
 *
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

#include "cvFlowsolverOptions.h"

#include <stdlib.h>
#include <math.h>

#include "cmd.h"

struct Kentry {
      int row;
      int col;
      double value;
};

#define SPARSE_OFFSET 1

extern "C" int StanfordSolveSparseMatrix(Kentry* Kentries,
                                         double *b,
                                         int numUniqueEntries,
                                         int size,double *soln);

extern int   DisplacementNumElements_;
extern int*  DisplacementConn_[3];
extern int   DisplacementNumNodes_;
extern int*  DisplacementNodeMap_;
extern double* DisplacementSolution_;
extern double* ThicknessSolution_;
extern double* EvwSolution_;
extern int* iBC_;
extern int numNodes_;
extern double* nodes_;




int stiffnessmatrix(double Evw,double nuvw,
                    double thickness,double pressure,double kcons,
                    double x1[3],double x2[3],double x3[3],
                    double Kglobal9[9][9], double fglobal9[9]) ;

void siftDownKentries(Kentry Kentries[], int root, int bottom, int array_size);

extern "C" void deleteKentries(Kentry *Kentries);


extern "C" int STANNSPCG(int *n,int *ndim,double *coef,int *jcoef1,
                     int *jcoef2,double *rhs,double *u);

int StanfordIterativeSolve(Kentry* Kentries,double *b,
                           int numUniqueEntries,int size,double *soln);

// =============================================================
// Stiffness matrix computation for the CST membrane element
// Alberto Figueroa.  Fall 2003  (matlab)
// nate 12/2004 (C)
// =============================================================

//matlab function [Kglobal9,fglobal9] = stiffnessmatrix(r,L,Evw,nuvw,thickness,pressure,kcons,x1,x2,x3)


//clear r L Evw nuvw thickness pressure x1 x2 x3
//return

/*
 *
 *
 *
 */





int calcInitDisplacements_var_prop(double Evw,double nuvw,
                          double thickness,double pressure,double kcons,
                          int use_direct_solve) {

  int nsdim = 3;
  int nnode = 3;
  int nudof = 3;

  int numNodes = DisplacementNumNodes_;
  int numElems = DisplacementNumElements_;
  int* conn[3];
  conn[0] = DisplacementConn_[0];
  conn[1] = DisplacementConn_[1];
  conn[2] = DisplacementConn_[2];
  int*  map  = DisplacementNodeMap_;

  int i,j,k;
  int ig, jg;
  int il, jl;
  int ielem;
  int kk;

  //matlab % Assembly of the element contributions
  //matlab % =====================================

  //matlab Kglobal = zeros(npoin*nsdim,npoin*nsdim);

  // we first create all of the entries, then
  // collapse into a spare data structure

  debugprint(stddbg,"  Allocating Kentry[%i]\n",9*9*numElems);

  Kentry* Kentries = new Kentry[9*9*numElems];

  //matlab Fglobal = zeros(npoin*nsdim,1);
  double* Fglobal = new double[numNodes*nsdim+SPARSE_OFFSET];
  double* soln    = new double[numNodes*nsdim+SPARSE_OFFSET];
  Fglobal[0] = 0.0;
  soln[0] = 0.0;
  for (i = 0; i < numNodes*nsdim; i++) {
      Fglobal[i+SPARSE_OFFSET] = 0.0;
      soln[i+SPARSE_OFFSET]    = 0.0;
  }
  double Kglobal9[9][9];
  double fglobal9[9];
  double x[3][3];
  double Evw_nodal[3];
  double thickness_nodal[3];

  //matlab % Loop over the elements

  // here, we vary significantly from the matlab code.  first, we compute all of the
  // global stiffness matrix entries individually, sort them, and create a compressed
  // data structure (while summing the entries) to call a sparse matrix solver

  int entryID = 0;

  //for ielem=1:nelem
  for (ielem =0; ielem < numElems ; ielem++) {
    // get nodal coords
    //matlab x1 = xcoor(conec(ielem,1),:)';
    //matlab x2 = xcoor(conec(ielem,2),:)';
    //matlab x3 = xcoor(conec(ielem,3),:)';
    for (i = 0; i < 3; i++) {
      j = map[conn[i][ielem]];
      x[i][0] = nodes_[0*numNodes_+j-1];
      x[i][1] = nodes_[1*numNodes_+j-1];
      x[i][2] = nodes_[2*numNodes_+j-1];
      Evw_nodal[i]=EvwSolution_[j];
      thickness_nodal[i]=ThicknessSolution_[j];
    }


    //matlab % Compute the element's stiffness and force vector
    //matlab [Kglobal9,fglobal9] = stiffnessmatrix(r,L,Evw,nuvw,thickness,pressure,kcons,x1,x2,x3);

     Evw=(Evw_nodal[0]+Evw_nodal[1]+Evw_nodal[2])/3.0;
     thickness=(thickness_nodal[0]+thickness_nodal[1]+thickness_nodal[2])/3.0;


    stiffnessmatrix(Evw,nuvw,thickness,pressure,kcons,x[0],x[1],x[2],Kglobal9,fglobal9);

#ifdef REALLY_OUTPUT_A_WHOLE_BUNCH
  fprintf(stdout,"\n");
  for (int iii = 0; iii < 9; iii++) {
      fprintf(stdout,"Kglobal9: ");
      for (int jjj = 0; jjj < 9; jjj++) {
          fprintf(stdout,"%12.5lf ",Kglobal9[iii][jjj]);
      }
      fprintf(stdout,"\n");
  }
  fprintf(stdout,"\n");
  for (int iii = 0; iii < 9; iii++) {
    fprintf(stdout,"Fglobal: %12.5lf\n",fglobal9[iii]);
  }
  fprintf(stdout,"\n");
#endif

    //matlab % Now we must place the element contribution into the right place of
    //matlab % the global stiffness matrix

    //matlab for i=1:nnode
    for (i = 0; i < nnode; i++) {
      //matlab ig = conec(ielem,i);
      ig = conn[i][ielem];
      //matlab for j=1:nnode
      for (j = 0; j < 3; j++) {
        //matlab jg = conec(ielem,j);
        jg = conn[j][ielem];
        //matlab for il=1:nsdim
        for (il = 0; il < nsdim; il++) {
          //matlab for jl=1:nsdim
          for (jl = 0; jl < nsdim; jl++) {
          //matlab Kglobal(nsdim*(ig-1)+il,nsdim*(jg-1)+jl) = Kglobal9(nsdim*(i-1)+il,nsdim*(j-1)+jl) + Kglobal(nsdim*(ig-1)+il,nsdim*(jg-1)+jl);
            Kentries[entryID].row = nsdim*ig+il;
            Kentries[entryID].col = nsdim*jg+jl;
            Kentries[entryID].value = Kglobal9[nsdim*i+il][nsdim*j+jl];
            entryID++;
          // matlab end
          }
        //matlab end
        }
      //matlab end
      }

      //matlab % Assemble the right-hand-side vector
      //matlab for il=1:nsdim
      for (il = 0; il < 3; il++) {
        //matlab  Fglobal(nsdim*(ig-1)+il,1) = Fglobal(nsdim*(ig-1)+il,1) + fglobal9(nsdim*(i-1)+il,1);
        // should it be ig or ig-1 ??????????????
        Fglobal[nsdim*ig+il+SPARSE_OFFSET] += fglobal9[nsdim*i+il];
      //matlab end
      }
    //matlab end
    }
  //matlab end
  }

  // need to sort entries here, consolidate them into flat structure
  // insertion sort algorithm
  int row, col;
  double value;

  int array_size = entryID;

  // heap sort by row
  for (i = (array_size / 2)-1; i >= 0; i--)
    siftDownKentries(Kentries, i, array_size, array_size);

  for (i = array_size-1; i >= 1; i--)
  {
    row   = Kentries[0].row;
    col   = Kentries[0].col;
    value = Kentries[0].value;
    Kentries[0].row   = Kentries[i].row;
    Kentries[0].col   = Kentries[i].col;
    Kentries[0].value = Kentries[i].value;
    Kentries[i].row   = row;
    Kentries[i].col   = col;
    Kentries[i].value = value;
    siftDownKentries(Kentries, 0, i-1, array_size);
  }


/*
  // sort by rows
  for (i=1; i < entryID; i++) {
    if (!(i % (int)(entryID/20.0))) {
        fprintf(stdout,"first pass: %i of %i\n",i,entryID);
    }
    row   = Kentries[i].row;
    col   = Kentries[i].col;
    value = Kentries[i].value;
    j = i;
    while ((j > 0) && (Kentries[j-1].row > row)) {
      Kentries[j].row   = Kentries[j-1].row;
      Kentries[j].col   = Kentries[j-1].col;
      Kentries[j].value = Kentries[j-1].value;
      j = j - 1;
    }
    Kentries[j].row   = row;
    Kentries[j].col   = col;
    Kentries[j].value = value;
  }
*/

  // secondary sort by cols
  for (i=1; i < entryID; i++) {
    if (!(i % (int)(entryID/20.0))) {
        debugprint(stddbg,"second pass: %i of %i\n",i,entryID);
    }
    row   = Kentries[i].row;
    col   = Kentries[i].col;
    value = Kentries[i].value;
    j = i;
    while ((j > 0) && ((Kentries[j-1].row >= row) && (Kentries[j-1].col > col))) {
      Kentries[j].row   = Kentries[j-1].row;
      Kentries[j].col   = Kentries[j-1].col;
      Kentries[j].value = Kentries[j-1].value;
      j = j - 1;
    }
    Kentries[j].row   = row;
    Kentries[j].col   = col;
    Kentries[j].value = value;
  }

  // consolidate into a single entry for each non-zero row & col
  int numUniqueEntries = 0;

  // count the number of unique nodes
  row = Kentries[0].row;
  col = Kentries[0].col;
  value = 0;
  for (i = 0; i < entryID; i++) {
    if ((Kentries[i].row != row) || (Kentries[i].col != col)) {
      Kentries[numUniqueEntries].row   = row;
      Kentries[numUniqueEntries].col   = col;
      Kentries[numUniqueEntries].value = value;
      numUniqueEntries++;
      row   = Kentries[i].row;
      col   = Kentries[i].col;
      value = Kentries[i].value;
    } else {
      value += Kentries[i].value;
    }
  }
  // last number from loop must be unique
  Kentries[numUniqueEntries].row   = row;
  Kentries[numUniqueEntries].col   = col;
  Kentries[numUniqueEntries].value = value;
  numUniqueEntries++;

  debugprint(stddbg,"  Number of Non-Zero entries: %i\n",numUniqueEntries);

#ifdef REALLY_OUTPUT_A_WHOLE_BUNCH
  FILE *fp = NULL;
  fp = fopen("full-matrix-before-bc","w");
  int curentry = 0;
  for (i = 0; i < numNodes*nsdim; i++) {
      for (j= 0; j < numNodes*nsdim; j++) {
          if (Kentries[curentry].row == i && Kentries[curentry].col == j) {
              fprintf(fp,"%lf\n",Kentries[curentry].value);
              curentry++;
          } else {
              fprintf(fp,"0\n");
          }
      }
  }
  fclose(fp);
  fp = NULL;
  fp = fopen("RHS-before-bc","w");
  for (i = 0; i < numNodes*nsdim; i++) {
      fprintf(fp,"%lf\n",Fglobal[i+SPARSE_OFFSET]);
  }
  fclose(fp);
#endif

#ifdef REALLY_OUTPUT_A_WHOLE_BUNCH
  fp = NULL;
  fp = fopen("nonzero-matrix-before-bc","w");
  fprintf(fp,"title goes here (%i nonzero)\n",numUniqueEntries);
  fprintf(fp,"%i        real\n",numNodes*nsdim);
  for (i = 0; i < numUniqueEntries; i++) {
      // we add SPARSE_OFFSET to row and col numbers for sparse solver
    fprintf(fp,"%i %i %lf\n",Kentries[i].row+SPARSE_OFFSET,Kentries[i].col+SPARSE_OFFSET,
            Kentries[i].value);
  }
  fprintf(fp,"0 0 0.0\n");
  for (i = 0; i < numNodes*nsdim; i++) {
      fprintf(fp,"%lf\n",Fglobal[i+SPARSE_OFFSET]);
  }
  fclose(fp);
#endif

  //matlab  % Read and apply the boundary conditions
  //matlab  % ======================================

  double bcval = 0.0;

  // here we differ from the matlab code again, and loop over the nodal b.c.
  // array to see which nodes should be fixed
  for (i = 0; i < numNodes; i++) {
    // should use bit test instead of an int
    if (iBC_[map[i]-1] == 56) {

        //matlab for kk=1:npoin*nsdim
        //matlab  Fglobal(kk,1)=Fglobal(kk,1) - Kglobal(nsdim*(inode-1)+idegree,kk)*value;
        //matlab  end
        for (int idegree=0; idegree < 3; idegree++) {
          for (kk = 0; kk < numUniqueEntries; kk++) {
            if (Kentries[kk].row == (nsdim*i+idegree)) {
                Fglobal[nsdim*i+idegree+SPARSE_OFFSET] = Fglobal[nsdim*i+idegree+SPARSE_OFFSET] - (Kentries[kk].value)*bcval;
            }
          }
        }
        //matlab Fglobal(nsdim*(inode-1)+idegree,1) = value;
        Fglobal[nsdim*i+0+SPARSE_OFFSET] = bcval;
        Fglobal[nsdim*i+1+SPARSE_OFFSET] = bcval;
        Fglobal[nsdim*i+2+SPARSE_OFFSET] = bcval;
        //matlab Kglobal(nsdim*(inode-1)+idegree,:)=0.0;
        //matlab Kglobal(:,nsdim*(inode-1)+idegree)=0.0;
        //matlab Kglobal(nsdim*(inode-1)+idegree,nsdim*(inode-1)+idegree) = 1.0;
        for (int idegree=0; idegree < 3; idegree++) {
          for (kk = 0; kk < numUniqueEntries; kk++) {
            if ((Kentries[kk].row == (nsdim*i+idegree)) || (Kentries[kk].col == (nsdim*i+idegree))) {
                Kentries[kk].value = 0;
            }
            if ((Kentries[kk].row == (nsdim*i+idegree)) && (Kentries[kk].col == (nsdim*i+idegree))) {
                Kentries[kk].value = 1.0;
            }
          }
        }
    //end
    }
  }

#ifdef REALLY_OUTPUT_A_WHOLE_BUNCH
  fp = NULL;
  fp = fopen("full-matrix-after-bc","w");
  curentry = 0;
  for (i = 0; i < numNodes*nsdim; i++) {
      for (j= 0; j < numNodes*nsdim; j++) {
          if (Kentries[curentry].row == i && Kentries[curentry].col == j) {
              fprintf(fp,"%lf\n",Kentries[curentry].value);
              curentry++;
          } else {
              fprintf(fp,"0\n");
          }
      }
  }
  fclose(fp);
  fp = NULL;
  fp = fopen("RHS-after-bc","w");
  for (i = 0; i < numNodes*nsdim; i++) {
      fprintf(fp,"%lf\n",Fglobal[i+SPARSE_OFFSET]);
  }
  fclose(fp);
#endif

#ifdef REALLY_OUTPUT_A_WHOLE_BUNCH
  fp = NULL;
  fp = fopen("nonzero-matrix-after-bc","w");
  fprintf(fp,"title goes here (%i nonzero)\n",numUniqueEntries);
  fprintf(fp,"%i        real\n",numNodes*nsdim);
  for (i = 0; i < numUniqueEntries; i++) {
      // we add 1 to row and col numbers for sparse
    fprintf(fp,"%i %i %lf\n",Kentries[i].row+SPARSE_OFFSET,Kentries[i].col+SPARSE_OFFSET,
            Kentries[i].value);
  }
  fprintf(fp,"0 0 0.0\n");
  for (i = 0; i < numNodes*nsdim; i++) {
      fprintf(fp,"%lf\n",Fglobal[i+SPARSE_OFFSET]);
  }
  fclose(fp);
#endif

  //% Solve the system
  //% ================
  //sol = sparse(Kglobal)\Fglobal;

  //
  //        call sparse
  //

  // to save space the following function call deallocates Kentries
  if (use_direct_solve == 1) {
    StanfordSolveSparseMatrix(Kentries, Fglobal,
                           numUniqueEntries, numNodes*nsdim,soln);
  } else {
    StanfordIterativeSolve(Kentries, Fglobal,
                           numUniqueEntries, numNodes*nsdim,soln);
  }

  DisplacementSolution_ = soln;

#ifdef REALLY_OUTPUT_A_WHOLE_BUNCH
  fp = NULL;
  fp = fopen("solution","w");
  for (i = 0; i < numNodes*nsdim; i++) {
      fprintf(fp,"%lf\n",soln[i+SPARSE_OFFSET]);
  }
  fclose(fp);
#endif

  // shift the array down by SPARSE_OFFSET to get the first
  // dof at 0
  for (i = 0; i < numNodes*nsdim; i++) {
      soln[i] = soln[i+SPARSE_OFFSET];
  }

  // the caller should free soln
  delete Fglobal;

  return 0;

}

