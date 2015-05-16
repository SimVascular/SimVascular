/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University, 
 * RPI, Charles Taylor, Ken Jansen.
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

#ifdef __cplusplus
extern "C" {
#endif

  /* Calculate shape functions and derivative for tet elements */
int TetShapeAndDrv(int p,double par[3],double N[],double dN[][3]) {
  static int TetEMAP[6][2]={{0,1},{1,2},{2,0},{0,3},{1,3},{2,3}};
  static int TetFMAP[4][3]={{0,1,2},{0,3,1},{1,3,2},{0,2,3}};
  int NS,is,i,j,ip,nshp=0,(*fpdrv)[2];
  double L[4],Le[2],Lf[3],mode,blend,bdrv[3],mdrv[3],epdrv[3][2],mfdrv[2];
  double tmp,rst,rsw,stw,rstw,rtw;
  if(p<1)
    return nshp;
  L[0]=par[0];
  L[1]=par[1];
  L[2]=par[2];
  L[3]=1.0e0-par[0]-par[1]-par[2];
  /* collect all vertex modes */
  for(i=0; i <4; i++) {
    N[i] = L[i];
    if(i==3) 
      dN[i][0]=dN[i][1]=dN[i][2]=-1.0e0;
    else {
      for(j=0; j <3; j++) {
	if(i==j)
	  dN[i][j] = 1.0e0;
	else
	  dN[i][j] = 0.0e0;
      }
    }
  }
  nshp=4;
  return nshp;
}

/* calculate the shape functions and their derivatives for
   triangular faces
   */

int TriShapeAndDrv(int p,double par[2],double N[],double dN[][2]){
  int i,j,nshp=0;
  double L[3];
  
  L[0]=par[0];
  L[1]=par[1];
  L[2]=1.0e0-par[0]-par[1];
    
  /* collect all vertex modes */
  for(i=0; i<3; i++) {
    N[i] = L[i];
    if(i==2)
      dN[i][0]=dN[i][1]=-1.0e0;
    else {
      for(j=0; j<2; j++) {
	if(i==j)
	  dN[i][j] = 1.0e0;
	else
	  dN[i][j] = 0.0e0;
      }
    }
  }
  nshp=3;

  return nshp;
}

#ifdef __cplusplus
}
#endif

