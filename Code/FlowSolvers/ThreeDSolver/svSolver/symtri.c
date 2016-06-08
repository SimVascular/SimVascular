/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University, 
 * Rensselaer Polytechnic Institute, Charles A. Taylor, 
 * Kenneth E. Jansen.
 * 
 * See SimVascular Acknowledgements file for additional
 * contributors to the source code. 
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
 */


typedef double DARR3[3];

int triIntPnt(int, DARR3**,double**);

#ifdef SV_WRAP_FORTRAN_IN_LOWERCASE_WITH_UNDERSCORE
  #define SYMTRI symtri_
#endif

void  SYMTRI(int *n1, double pt[][4], double wt[], int *err)
{
  double *lwt;
  DARR3 *lpt;
  int i,j;
  *err = triIntPnt(*n1, &lpt, &lwt);
  for(i=0; i < *n1; i++){
    wt[i] = lwt[i];
    for(j=0; j < 3; j++)
      pt[i][j] = lpt[i][j];
  }
}

#include <stdio.h>

#define A 0.3333333333333330000000000000000000
#define B 0.666666666666667
#define C 0.166666666666667
#define D 0.562500000000000
#define E 0.520833333333333

#define F 0.109951743655322
#define G 0.223381589678011
#define H 0.816847572980459
#define I 0.091576213509771
#define J 0.108103018168070
#define K 0.445948490915965

#define L 0.225000000000000
#define M 0.125939180544827
#define N 0.132394152788506

#define O 0.797426985353087
#define P 0.101286507323456
#define Q 0.470142064105115
#define R 0.059715871789770

#define S 0.050844906370207
#define T 0.116786275726379
#define U 0.082851075618374

#define V 0.873821971016996
#define W 0.063089014491502
#define X 0.501426509658179
#define Y 0.249286745170910
#define Z 0.636502499121399
#define AA 0.310352451033785
#define BB 0.053145049844816

/* typedef double DARR3[3] ; */

static double rst1[][3] = {{A,A,A}};
static double wt1[] = {1.00000000000000000};

static double rst3[][3] = {{B,C,C},{C,B,C},{C,C,B}};
static double wt3[] = {A,A,A};

static double rst4[][3] = {{A,A,A},
                             {0.6,0.2,0.2},{0.2,0.6,0.2},{0.2,0.2,0.6}};
static double wt4[] = {-D,E,E,E};

static double rst6[][3] = {{H,I,I},{I,H,I},{I,I,H},
                             {J,K,K},{K,J,K},{K,K,J}};
static double wt6[] = {F,F,F,G,G,G};

static double rst7[][3] = {{A,A,A},
                             {O,P,P},{P,O,P},{P,P,O},
                             {Q,Q,R},{Q,R,Q},{R,Q,Q}};
static double wt7[] = {L,M,M,M,N,N,N};

static double rst12[][3] = {{V,W,1.0-V-W},{W,V,1.0-V-W},{W,W,1.0-W-W},
			    {X,Y,1.0-X-Y},{Y,X,1.0-X-Y},{Y,Y,1.0-Y-Y},
			    {Z,AA,1.0-Z-AA},{AA,Z,1.0-Z-AA},{Z,BB,1.0-Z-BB},
			    {AA,BB,1.0-AA-BB},{BB,AA,1.0-BB-AA},{BB,Z,1.0-BB-Z}};

static double wt12[] = {S,S,S,T,T,T,U,U,U,U,U,U};
                             
int triIntPnt(int nint, DARR3 **bcord, double **wt)
{
  int retval = 1 ;
  if( nint == 3 ) {*bcord = rst3 ; *wt = wt3; }
  else if( nint == 1 ){*bcord = rst1 ; *wt = wt1; }
  else if( nint == 4 ){*bcord = rst4 ; *wt = wt4; }
  else if( nint == 6 ){*bcord = rst6 ; *wt = wt6; }
  else if( nint == 7 ){*bcord = rst7 ; *wt = wt7; }
  else if( nint == 12){*bcord = rst12; *wt = wt12;}
  else
  {
    fprintf(stderr,"\n%d integration points unsupported in symtri.c; give {1,3,4,6,7,12}\n",nint);
    retval = 0;
  }
  return retval ;
}



