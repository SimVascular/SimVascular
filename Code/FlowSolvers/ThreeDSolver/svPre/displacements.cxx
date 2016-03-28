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

extern int* iBC_;
extern int numNodes_;
extern double* nodes_;

// =========
//   Cross
// =========

inline
void Cross( double ax, double ay, double az,
            double bx, double by, double bz,
            double *prodx, double *prody, double *prodz )
{
  (*prodx) = ay * bz - az * by;
  (*prody) = -( ax * bz - az * bx );
  (*prodz) = ax * by - ay * bx;
  return;
}


// =======
//   Dot
// =======

inline
double Dot( double ax, double ay, double az,
            double bx, double by, double bz )
{
  double product;

  product = ax * bx + ay * by + az * bz;
  return product;
}


// ========
//   Norm
// ========

inline
double Norm( double ax, double ay, double az )
{
  double product;

  product = sqrt(ax * ax + ay * ay + az * az);
  return product;
}


// ==========
//   SubVec
// ==========

inline
void SubVec( double ax, double ay, double az,
            double bx, double by, double bz,
            double *prodx, double *prody, double *prodz )
{
  (*prodx) = ax - bx;
  (*prody) = ay - by;
  (*prodz) = az - bz;
  return;
}


// ==========
//   Mat3Vec
// ==========

inline
void Mat3Vec( double mat[3][3],double v[3],double *prodx, double *prody, double *prodz )
{

  (*prodx) = mat[0][0]*v[0] + mat[0][1]*v[1] + mat[0][2]*v[2];
  (*prody) = mat[1][0]*v[0] + mat[1][1]*v[1] + mat[1][2]*v[2];
  (*prodz) = mat[2][0]*v[0] + mat[2][1]*v[1] + mat[2][2]*v[2];
  return;
}

// =============================================================
// Stiffness matrix computation for the CST membrane element
// Alberto Figueroa.  Fall 2003  (matlab)
// nate 12/2004 (C)
// =============================================================

//matlab function [Kglobal9,fglobal9] = stiffnessmatrix(r,L,Evw,nuvw,thickness,pressure,kcons,x1,x2,x3)

int stiffnessmatrix(double Evw,double nuvw,
                    double thickness,double pressure,double kcons,
                    double x1[3],double x2[3],double x3[3],
                    double Kglobal9[9][9], double fglobal9[9]) {

  //matlab format short;

  int i, j, k;
  int il,jl;
  int ii,jj, kk;
  double norm;
  double v1[3],v2[3],v3[3];
  double T[3][3];
  double area;
  double detjacrot;
  double x1rot[3],x2rot[3],x3rot[3];
  double Bmatrix[5][9],Ematrix[5][5];
  double EtimesB[5][9];
  double temp[3][3];
  double temp2[3][3];
  double temp3[3][3];
  double Klocal9[9][9];

  // just in case we zero out the returns
  for (i = 0; i < 9; i++) {
    fglobal9[i] = 0;
    for (j = 0; j < 9; j++) {
      Kglobal9[i][j] = 0.0;
    }
  }

  // Rotation matrix
  //matlab v1 = x2 - x1;
  SubVec(x2[0],x2[1],x2[2],x1[0],x1[1],x1[2],&v1[0],&v1[1],&v1[2]);
  //matlab v1 = v1/norm(v1);
  norm = Norm(v1[0],v1[1],v1[2]);
  v1[0] = v1[0] / norm; v1[1] = v1[1] / norm; v1[2] = v1[2] / norm;
  //matlab v2 = x3 - x1;
  SubVec(x3[0],x3[1],x3[2],x1[0],x1[1],x1[2],&v2[0],&v2[1],&v2[2]);
  //matlab v3 = cross(v1, v2);
  Cross(v1[0],v1[1],v1[2],v2[0],v2[1],v2[2],&v3[0],&v3[1],&v3[2]);
  //matlab v3 = v3/norm(v3);
  norm = Norm(v3[0],v3[1],v3[2]);
  v3[0] = v3[0] / norm; v3[1] = v3[1] / norm; v3[2] = v3[2] / norm;
  //matlab v2 = cross(v3, v1);
  Cross(v3[0],v3[1],v3[2],v1[0],v1[1],v1[2],&v2[0],&v2[1],&v2[2]);

  //matlab T = zeros(3,3);
  //matlab T(1,:)=v1';
  //matlab T(2,:)=v2';
  //matlab T(3,:)=v3';

  T[0][0] = v1[0]; T[0][1] = v1[1]; T[0][2] = v1[2];
  T[1][0] = v2[0]; T[1][1] = v2[1]; T[1][2] = v2[2];
  T[2][0] = v3[0]; T[2][1] = v3[1]; T[2][2] = v3[2];

  //matlab % Rotated coordinates
  //matlab x1rot = T*x1;
  //matlab x2rot = T*x2;
  //matlab x3rot = T*x3;

  Mat3Vec(T,x1,&x1rot[0], &x1rot[1], &x1rot[2]);
  Mat3Vec(T,x2,&x2rot[0], &x2rot[1], &x2rot[2]);
  Mat3Vec(T,x3,&x3rot[0], &x3rot[1], &x3rot[2]);

  //matlab detjacrot = (x2rot(1)-x1rot(1)) * (x3rot(2)-x1rot(2)) - (x3rot(1)-x1rot(1))*(x2rot(2)-x1rot(2));
  //matlab area = detjacrot/2;

  detjacrot = (x2rot[0]-x1rot[0]) * (x3rot[1]-x1rot[1]) - (x3rot[0]-x1rot[0])*(x2rot[1]-x1rot[1]);
  area = detjacrot/2;

#ifdef REALLY_OUTPUT_A_WHOLE_BUNCH
  fprintf(stdout,"x1rot: %lf %lf %lf\n",x1rot[0],x1rot[1],x1rot[2]);
  fprintf(stdout,"x2rot: %lf %lf %lf\n",x2rot[0],x2rot[1],x2rot[2]);
  fprintf(stdout,"x3rot: %lf %lf %lf\n",x3rot[0],x3rot[1],x3rot[2]);
  fprintf(stdout,"detjacrot: %lf\n",detjacrot);
  fprintf(stdout,"area: %lf\n",area);
#endif

  //matlab % B matrix
  //matlab Bmatrix = zeros(5,9);

  for (i = 0; i < 5; i++) {
    for (j = 0; j < 9; j++) {
      Bmatrix[i][j] = 0.0;
    }
  }

  //matlab Bmatrix(1,1) = x2rot(2)-x3rot(2);
  //matlab Bmatrix(1,4) = x3rot(2)-x1rot(2);
  //matlab Bmatrix(1,7) = x1rot(2)-x2rot(2);
  //matlab Bmatrix(2,2) = x3rot(1)-x2rot(1);
  //matlab Bmatrix(2,5) = x1rot(1)-x3rot(1);
  //matlab Bmatrix(2,8) = x2rot(1)-x1rot(1);
  //matlab Bmatrix(3,1) = x3rot(1)-x2rot(1);
  //matlab Bmatrix(3,2) = x2rot(2)-x3rot(2);
  //matlab Bmatrix(3,4) = x1rot(1)-x3rot(1);
  //matlab Bmatrix(3,5) = x3rot(2)-x1rot(2);
  //matlab Bmatrix(3,7) = x2rot(1)-x1rot(1);
  //matlab Bmatrix(3,8) = x1rot(2)-x2rot(2);
  //matlab Bmatrix(4,3) = x2rot(2)-x3rot(2);
  //matlab Bmatrix(4,6) = x3rot(2)-x1rot(2);
  //matlab Bmatrix(4,9) = x1rot(2)-x2rot(2);
  //matlab Bmatrix(5,3) = x3rot(1)-x2rot(1);
  //matlab Bmatrix(5,6) = x1rot(1)-x3rot(1);
  //matlab Bmatrix(5,9) = x2rot(1)-x1rot(1);

  Bmatrix[0][0] = x2rot[1]-x3rot[1];
  Bmatrix[0][3] = x3rot[1]-x1rot[1];
  Bmatrix[0][6] = x1rot[1]-x2rot[1];
  Bmatrix[1][1] = x3rot[0]-x2rot[0];
  Bmatrix[1][4] = x1rot[0]-x3rot[0];
  Bmatrix[1][7] = x2rot[0]-x1rot[0];
  Bmatrix[2][0] = x3rot[0]-x2rot[0];
  Bmatrix[2][1] = x2rot[1]-x3rot[1];
  Bmatrix[2][3] = x1rot[0]-x3rot[0];
  Bmatrix[2][4] = x3rot[1]-x1rot[1];
  Bmatrix[2][6] = x2rot[0]-x1rot[0];
  Bmatrix[2][7] = x1rot[1]-x2rot[1];
  Bmatrix[3][2] = x2rot[1]-x3rot[1];
  Bmatrix[3][5] = x3rot[1]-x1rot[1];
  Bmatrix[3][8] = x1rot[1]-x2rot[1];
  Bmatrix[4][2] = x3rot[0]-x2rot[0];
  Bmatrix[4][5] = x1rot[0]-x3rot[0];
  Bmatrix[4][8] = x2rot[0]-x1rot[0];

  //matlab Bmatrix = Bmatrix/(2*area);

  for (i = 0; i < 5; i++) {
    for (j = 0; j < 9; j++) {
      Bmatrix[i][j] = Bmatrix[i][j]/(2*area);
    }
  }

#ifdef REALLY_OUTPUT_A_WHOLE_BUNCH
  for (i = 0; i < 5; i++) {
      fprintf(stdout,"Bmatrix: ");
      for (j = 0; j < 9; j++) {
          fprintf(stdout,"%12.5lf ",Bmatrix[i][j]);
      }
      fprintf(stdout,"\n");
  }
#endif

  //matlab % E matrix
  //matlab Ematrix = zeros(5,5);
  for (i = 0; i < 5; i++) {
    for (j = 0; j < 5; j++) {
      Ematrix[i][j] = 0.0;
    }
  }

  //matlab term1 = Evw / (1-nuvw^2);
  //matlab term2 = nuvw * term1;
  //matlab term3 = 0.5 * (1-nuvw) * term1;

  double term1 = Evw / (1-nuvw*nuvw);
  double term2 = nuvw * term1;
  double term3 = 0.5 * (1-nuvw) * term1;

#ifdef REALLY_OUTPUT_A_WHOLE_BUNCH
  fprintf(stdout,"term1: %lf\n",term1);
  fprintf(stdout,"term2: %lf\n",term2);
  fprintf(stdout,"term3: %lf\n",term3);
  fprintf(stdout,"kcons: %lf\n",kcons);
#endif

  //matlab Ematrix(1,1) = term1;
  //matlab Ematrix(1,2) = term2;
  //matlab Ematrix(2,1) = term2;
  //matlab Ematrix(2,2) = term1;
  //matlab Ematrix(3,3) = term3;
  //matlab Ematrix(4,4) = term3*kcons;
  //matlab Ematrix(5,5) = term3*kcons;

  Ematrix[0][0] = term1;
  Ematrix[0][1] = term2;
  Ematrix[1][0] = term2;
  Ematrix[1][1] = term1;
  Ematrix[2][2] = term3;
  Ematrix[3][3] = term3*kcons;
  Ematrix[4][4] = term3*kcons;

#ifdef REALLY_OUTPUT_A_WHOLE_BUNCH
  for (i = 0; i < 5; i++) {
      fprintf(stdout,"Ematrix: ");
      for (j = 0; j < 5; j++) {
          fprintf(stdout,"%12.5le ",Ematrix[i][j]);
      }
      fprintf(stdout,"\n");
  }
#endif

  //matlab % Matrix products
  //matlab EtimesB = zeros(5,9);

  for (i = 0; i < 5; i++) {
    for (j = 0; j < 9; j++) {
      EtimesB[i][j] = 0.0;
    }
  }

  //matlab EtimesB = Ematrix*Bmatrix;

  for( i = 0; i < 5; i++ ) {
    for( j = 0; j < 9; j++ ) {
      for( k = 0; k < 5; k++ ) {
        EtimesB[ i ][ j ] += Ematrix[ i ][ k ] * Bmatrix[ k ][ j ];
      }
    }
  }

#ifdef REALLY_OUTPUT_A_WHOLE_BUNCH
  for (i = 0; i < 5; i++) {
      fprintf(stdout,"EtimesB: ");
      for (j = 0; j < 9; j++) {
          fprintf(stdout,"%12.5le ",EtimesB[i][j]);
      }
      fprintf(stdout,"\n");
  }
#endif

  //matlab Klocal9 = Bmatrix'*EtimesB*thickness*area;

  for (i = 0; i < 9; i++) {
    for (j = 0; j < 9; j++) {
      Klocal9[i][j] = 0.0;
    }
  }

  for( i = 0; i < 9; i++ ) {
    for( j = 0; j < 9; j++ ) {
      for( k = 0; k < 5; k++ ) {
        // we flip the indices here on Bmatrix to get BmatrixT
        Klocal9[ i ][ j ] += Bmatrix[ k ][ i ] * EtimesB[ k ][ j ] * thickness * area;
      }
    }
  }

#ifdef REALLY_OUTPUT_A_WHOLE_BUNCH
  for (i = 0; i < 9; i++) {
      fprintf(stdout,"Klocal9: ");
      for (j = 0; j < 9; j++) {
          fprintf(stdout,"%12.5lf ",Klocal9[i][j]);
      }
      fprintf(stdout,"\n");
  }
#endif

  //matlab % Rotate the 9x9 local matrix to the global reference frame

  //matlab for i=1:3
  for (i=0;i<3;i++) {
    //matlab for j=1:3
    for (j = 0; j < 3; j++) {
      //matlab temp = zeros(3,3);
      for (ii =0; ii < 3; ii++) {
        for (jj = 0; jj < 3; jj++) {
           temp[ii][jj] = 0;
        }
      }
      //matlab for il=1:3
      for(il=0;il<3;il++) {
        //matlab for jl=1:3
          for(jl=0;jl<3;jl++) {
            //matlab temp(il,jl) = Klocal9(3*(i-1)+il,3*(j-1)+jl);
            temp[il][jl] = Klocal9[3*i+il][3*j+jl];
          //matlab end
          }
      //matlab end
      }

#ifdef REALLY_OUTPUT_A_WHOLE_BUNCH
      fprintf(stdout,"\n");
      for (int iii = 0; iii < 3; iii++) {
        fprintf(stdout,"temp: ");
        for (int jjj = 0; jjj < 3; jjj++) {
          fprintf(stdout,"%12.5lf ",temp[iii][jjj]);
        }
        fprintf(stdout,"\n");
      }
#endif

      //matlab temp2 = zeros(3,3);
      for (ii =0; ii < 3; ii++) {
        for (jj = 0; jj < 3; jj++) {
          temp2[ii][jj] = 0;
          temp3[ii][jj] = 0;
        }
      }
      //matlab temp2 = T'*temp*T;
      for( ii = 0; ii < 3; ii++ ) {
        for( jj = 0; jj < 3; jj++ ) {
          for( kk = 0; kk < 3; kk++ ) {
            // flip indicies for transpose of T
            temp3[ ii ][ jj ] += T[ kk ][ ii ] * temp[ kk ][ jj ];
          }
        }
      }

#ifdef REALLY_OUTPUT_A_WHOLE_BUNCH
      fprintf(stdout,"\n");
      for (int iii = 0; iii < 3; iii++) {
        fprintf(stdout,"temp3: ");
        for (int jjj = 0; jjj < 3; jjj++) {
          fprintf(stdout,"%12.5le ",temp3[iii][jjj]);
        }
        fprintf(stdout,"\n");
      }
#endif

      for( ii = 0; ii < 3; ii++ ) {
        for( jj = 0; jj < 3; jj++ ) {
          for( kk = 0; kk < 3; kk++ ) {
            temp2[ ii ][ jj ] += temp3[ ii ][ kk ] * T[ kk ][ jj ];
          }
        }
      }

#ifdef REALLY_OUTPUT_A_WHOLE_BUNCH
      fprintf(stdout,"\n");
      for (int iii = 0; iii < 3; iii++) {
        fprintf(stdout,"temp2: ");
        for (int jjj = 0; jjj < 3; jjj++) {
          fprintf(stdout,"%12.5le ",temp2[iii][jjj]);
        }
        fprintf(stdout,"\n");
      }
#endif

      //matlab for il=1:3
      for (il = 0; il < 3; il++) {
        //matlab for jl=1:3
        for (jl = 0; jl < 3; jl++) {
          //matlab Kglobal9(3*(i-1)+il,3*(j-1)+jl) = temp2(il,jl);
          Kglobal9[3*i+il][3*j+jl] = temp2[il][jl];
        //matlab end
        }
      //matlab end
      }

    //matlab end
    }
  //matlab end
  }

  //matlab % right-hand-side vector
  //matlab fglobal9 = zeros(9,1);
  for (i = 0; i < 9; i++) {
     fglobal9[0] = 0;
  }
  //matlab for i=1:3
  for (i = 0; i < 3; i++) {
      //matlab for il=1:3
      for (il = 0; il < 3; il++) {
        //matlab fglobal9(3*(i-1)+il,1) = area*pressure*v3(il,1)/3;
        fglobal9[3*i+il] = area*pressure*v3[il]/3;
        //matlab end
      }
  //matlab end
  }
//matlab end

  return 0;
}

//clear r L Evw nuvw thickness pressure x1 x2 x3
//return

/*
 *
 *
 *
 */

void siftDownKentries(Kentry Kentries[], int root, int bottom, int array_size)
{
  int done, maxChild;
  int row, col;
  double value;

  done = 0;
  while ((root*2 <= bottom) && (!done))
  {
    if (root*2 == bottom)
      maxChild = root * 2;
    else if (Kentries[root * 2].row > Kentries[root * 2 + 1].row)
      maxChild = root * 2;
    else
      maxChild = root * 2 + 1;

    // hack since this seems to constantly end
    if (root < 0 || root >= array_size) break;
    if (maxChild < 0 || maxChild >= array_size) {
      fprintf(stdout,"warning: illegal maxChild (%i)\n",maxChild);
      fprintf(stdout,"root= %d bottom=%d array_size=%d\n",root,bottom,array_size);
      break;
    }

    if (Kentries[root].row < Kentries[maxChild].row)
    {
      row   = Kentries[root].row;
      col   = Kentries[root].col;
      value = Kentries[root].value;
      Kentries[root].row   = Kentries[maxChild].row;
      Kentries[root].col   = Kentries[maxChild].col;
      Kentries[root].value = Kentries[maxChild].value;
      Kentries[maxChild].row   = row;
      Kentries[maxChild].col   = col;
      Kentries[maxChild].value = value;
      root = maxChild;
    }
    else
      done = 1;
  }
}

extern "C" void deleteKentries(Kentry *Kentries);

void deleteKentries(Kentry *Kentries) {
    delete [] Kentries;
}

#ifdef SV_WRAP_FORTRAN_IN_CAPS_NO_UNDERSCORE
  #define STANNSPCG STANNSPCG
#elif SV_WRAP_FORTRAN_IN_LOWERCASE_WITH_UNDERSCORE
  #define STANNSPCG stannspcg_
#else
  // error here!
#endif

extern "C" int STANNSPCG(int *n,int *ndim,double *coef,int *jcoef1,
                     int *jcoef2,double *rhs,double *u);

int StanfordIterativeSolve(Kentry* Kentries,double *b,
                           int numUniqueEntries,int size,double *soln) {

  int i,row,col;

  // create nspcg data structures

  // assume symmetric matrix
  int numDiagEntries = 0;
  int numUpperEntries = 0;

  for (i = 0; i < numUniqueEntries; i++) {
    row   = Kentries[i].row;
    col   = Kentries[i].col;
    if (row == col) {
        numDiagEntries++;
    } else if (col > row) {
        numUpperEntries++;
    }
  }

  debugprint(stddbg,"num diagonal entries: %i (size %i)\n",numDiagEntries,size);
  debugprint(stddbg,"num upper entries   : %i (size %i)\n",numUpperEntries,size);

  int ndim = numDiagEntries+numUpperEntries;
  double *coef = (double*)malloc(ndim*sizeof(double));
  int  *jcoef1 = (int*)malloc(ndim*sizeof(int));
  int  *jcoef2 = (int*)malloc(ndim*sizeof(int));

//  double *coef = new double[ndim];
//  int *jcoef1 = new int[ndim];
//  int *jcoef2 = new int[ndim];

  int count = 0;

  // add 1 to all indices for nspcg

  // first insert the diagonal entries

  for (i = 0; i < numUniqueEntries; i++) {
    row   = Kentries[i].row;
    col   = Kentries[i].col;
    if (row == col) {
        jcoef1[count] = row + 1;
        jcoef2[count] = col + 1;
        coef[count]   = Kentries[i].value;
//        fprintf(stdout,"c_coef %10i%10i%30.5lf\n",jcoef1[count],jcoef2[count],coef[count]);
        count++;
    }
  }


  // second insert the upper entries

  for (i = 0; i < numUniqueEntries; i++) {
    row   = Kentries[i].row;
    col   = Kentries[i].col;
    if (col > row) {
        jcoef1[count] = row + 1;
        jcoef2[count] = col + 1;
        coef[count]   = Kentries[i].value;
//        fprintf(stdout,"c_coef %10i%10i%30.5lf\n",jcoef1[count],jcoef2[count],coef[count]);
        count++;
    }
  }

  fflush(stdout);

  // free up memory used by Kentries
  deleteKentries(Kentries);

  if (count != ndim) {
      fprintf(stderr,"ERROR: count does not equal ndim!\n");
      return CV_ERROR;
  }

  int n = size;

  STANNSPCG(&n,&ndim,coef,jcoef1,jcoef2,&(b[SPARSE_OFFSET]),&(soln[SPARSE_OFFSET]));
  return CV_OK;
}

int calcInitDisplacements(double Evw,double nuvw,
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
    }

    //matlab % Compute the element's stiffness and force vector
    //matlab [Kglobal9,fglobal9] = stiffnessmatrix(r,L,Evw,nuvw,thickness,pressure,kcons,x1,x2,x3);

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

