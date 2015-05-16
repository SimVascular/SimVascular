/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University, 
 * RPI, Charles Taylor, Ken Jansen, Nathan Wilson, Ken Wang.
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
 */

/* 
   Functionality:
   Return the aspect ratio (longest edge)/(shortest edge) of a region. 
   Currently, only tetrahedrons are supported.
*/

#include "SimVascular.h"
#include <math.h>

typedef double dArray[3];  /* for hp compiler compatibility */

#ifndef M_PI
#define M_PI		3.14159265358979323846
#endif

#ifndef MTBIG
#define MTBIG 1.0e20
#endif

double scorecXYZ_circumRad2(dArray *xyz);
void scorecXYZ_dhdAngs(double xyz[4][3], double *cosAngs);
double scorecXYZ_inscrRad(dArray *xyz);

double scorecDet(int n, double A[3][3]) {

   return (A[0][0]*A[1][1]*A[2][2] + A[1][0]*A[2][1]*A[0][2] + 
	   A[0][1]*A[1][2]*A[2][0] -  A[0][2]*A[1][1]*A[2][0] - 
	   A[1][2]*A[2][1]*A[0][0] - A[0][1]*A[1][0]*A[2][2]);

}

/*----------------------------------------------------------------------------
----------------------------------------------------------------------------*/
void scorecAddVt(double *a,double *b,double *v)
{
  v[0] = a[0] + b[0] ;
  v[1] = a[1] + b[1] ;
  v[2] = a[2] + b[2] ;
}

/*----------------------------------------------------------------------------
----------------------------------------------------------------------------*/
void scorecDiffVt(double *a,double *b,double *v)
{
  v[0] = a[0] - b[0] ;
  v[1] = a[1] - b[1] ;
  v[2] = a[2] - b[2] ;
}


/*----------------------------------------------------------------------------
----------------------------------------------------------------------------*/
void scorecScaleVt(double *a,double n)
{
  a[0] *= n ;
  a[1] *= n ;
  a[2] *= n ;
}

/*----------------------------------------------------------------------------
----------------------------------------------------------------------------*/
double scorecDotProd(double *v1, double *v2)
{
  return v1[0]*v2[0] + v1[1]*v2[1] + v1[2]*v2[2] ;
}

/*----------------------------------------------------------------------------
----------------------------------------------------------------------------*/
void scorecCrossProd(double *v1, double *v2, double *cp)
{
  cp[0] = v1[1]*v2[2] - v1[2]*v2[1] ;
  cp[1] = v1[2]*v2[0] - v1[0]*v2[2] ;
  cp[2] = v1[0]*v2[1] - v1[1]*v2[0] ;
}

/*----------------------------------------------------------------------------
----------------------------------------------------------------------------*/
int scorecSameSign(double x, double y)
{
  return (x<=0. && y<=0.) || ( x>=0. && y>= 0.) ;
}

/*----------------------------------------------------------------------------
----------------------------------------------------------------------------*/
double scorecVecMag(double *vec)
{
  return sqrt((vec[0]*vec[0] + vec[1]*vec[1] + vec[2]*vec[2])) ;
}

/*----------------------------------------------------------------------------
 normalize a 3D vector
----------------------------------------------------------------------------*/
int scorecNormVt(double *v1,double *nv)
{
  double norm ;

  norm = v1[0]*v1[0] + v1[1]*v1[1] + v1[2]*v1[2] ;

  norm = 1./sqrt(norm) ;
  nv[0] = v1[0]*norm ; 
  nv[1] = v1[1]*norm ; 
  nv[2] = v1[2]*norm ;

  return(1) ;
}

/*----------------------------------------------------------------------------
----------------------------------------------------------------------------*/
double scorecDet3Mat(double *v1,double *v2,double *v3) 
{
  return 
    v1[0]*v2[1]*v3[2] + v2[0]*v3[1]*v1[2] + v1[1]*v2[2]*v3[0] -
    v1[2]*v2[1]*v3[0] - v2[2]*v3[1]*v1[0] - v1[1]*v2[0]*v3[2] ;
}


void scorecNegVt(double *a) {
  a[0] = -a[0];
  a[1] = -a[1];
  a[2] = -a[2];
}


double scorecXYZ_aspectRatio2(dArray *xyz) {
  double eLong, hShort2, vec1[3], vec2[3], vec3[3], ilen;
  double alt2, length, dp12;
  int i,j;
  void *temp = 0;

  eLong = 0.0;
  for (i = 0; i < 3; i++) {
    for (j = i+1; j < 4; j++) {
      scorecDiffVt(xyz[i], xyz[j], vec1);
      length = scorecDotProd(vec1,vec1);
      if (length > eLong) eLong = length;
    }
  }

  hShort2 = MTBIG;
  for (i = 0; i < 4; i++) {
    scorecDiffVt(xyz[(i+1)%4], xyz[(i+2)%4], vec1);
    scorecDiffVt(xyz[(i+1)%4], xyz[(i+3)%4], vec2);
    scorecCrossProd(vec1, vec2, vec3);
    ilen = 1.0/scorecDotProd(vec3, vec3); /* reciprocal of vector length */

    scorecDiffVt(xyz[i], xyz[(i+1)%4], vec1);
    dp12 = scorecDotProd(vec1, vec3);
    alt2 = dp12*dp12*ilen;      /* square of height */
    if (hShort2 > alt2) hShort2 = alt2;
  }

  return (eLong/hShort2);

} /* scorecXYZ_aspectRatio2 */


/*-------------------------------------------------------------------------
  Scientific Computation Research Center, RPI, Troy NY
  (C) Copyright 1995, RPI-SCOREC
 
  Project   : Mesh Tools
  Author(s) : Pascal J. Frey
  Creation  : Feb., 95
  Modifi.   : 
  Function  :
    returns the cosine of smallest and largest dihedral angle in a tetra.
-------------------------------------------------------------------------*/

void scorecXYZ_dihedral(dArray *xyz,double *sml,double *big) {
  double  cossml,cosbig,cosangs[6];
  int     e;

  /* get all 6 dihedral angles */
  scorecXYZ_dhdAngs(xyz,cosangs);
  cossml=cosangs[0];
  cosbig=cosangs[0];
  /* evaluate 5 other dihedral angles */
  for (e=1; e<6; e++) {
    /* dihedral angle evaluation */
    if (cosangs[e]<cosbig)
      cosbig = cosangs[e];
    if (cosangs[e] > cossml)
      cossml = cosangs[e];
  }
  if (cosbig<0.)
    *big = -sqrt(-cosbig);
  else
    *big = sqrt(cosbig);
  if (cossml<0.)
    *sml = -sqrt(-cossml);
  else
    *sml = sqrt(cossml);
}


/*-------------------------------------------------------------------------
  Scientific Computation Research Center, RPI, Troy NY
  (C) Copyright 1995, RPI-SCOREC
 
  Project   : Mesh Tools
  Author(s) : Rao Garimella
  Creation  : Aug., 95
  Modifi.   : 
  Function  :
    Return the square of maximum edge length ratio of a region. Currently, only
    tetrahedrons are supported.
-------------------------------------------------------------------------*/

double scorecXYZ_edgLenRatio2(dArray *xyz) {
  double  longest, shortest, vects[3], length;
  int     i,j;
  void   *temp=0;

  shortest = 999999999.0;
  longest = 0.0;
  for (i=0; i<3; i++) {
    for (j=i+1; j<4; j++) {
      scorecDiffVt(xyz[i], xyz[j], vects);
      length = scorecDotProd(vects, vects);
      if (length < shortest) shortest = length;
      if (length > longest) longest = length;
    }
  }
  return (longest/shortest);
} /* scorecXYZ_edgeLenRatio2 */


/* Scientific Computation Research Center, RPI, Troy NY
   (C) Copyright 1995, RPI-SCOREC

   Project: 

   Authors/Dates:
   Rao Garimella, Feb 1995

   Functionality:
   Returns the r by R ratio

   Argument Variable(s):
   Name  Type In/Out Description
 |------|----|------|-----------------------------------------------------|
*/


double scorecXYZ_rbyR2(dArray *xyz) {
  double irad;

  irad = scorecXYZ_inscrRad(xyz);
  return (9.0*irad*irad/scorecXYZ_circumRad2(xyz));  

} /* scorecXYZ_rbyR */


/*--------------------------------------------------------------------------
  computes and returns the volume of a tetra.
--------------------------------------------------------------------------*/
int scorecXYZ_shape(dArray *xyz,double *shape) {
  double v31[3],v32[3],v30[3],v20[3],v21[3],cp[3],jcb,area ;

  scorecDiffVt(xyz[0],xyz[3],v30) ;
  scorecDiffVt(xyz[2],xyz[3],v32) ;
  scorecDiffVt(xyz[1],xyz[3],v31) ;
  scorecCrossProd(v30,v32,cp) ;
  jcb = scorecDotProd(cp,v31) ;
  /* check if element is valid */
  if ( jcb < 0. ) {
    *shape = jcb ;
    return(0) ;
  }
  /* compute the face areas */
  area = scorecDotProd(cp,cp) ;
  scorecCrossProd(v30,v31,cp) ;
  area += scorecDotProd(cp,cp) ;
  scorecCrossProd(v31,v32,cp) ;
  area += scorecDotProd(cp,cp) ;
  
  scorecDiffVt(xyz[0],xyz[2],v20) ;
  scorecDiffVt(xyz[1],xyz[2],v21) ;
  scorecCrossProd(v20,v21,cp) ;
  area += scorecDotProd(cp,cp) ;

  *shape = jcb / area ;
  *shape = (*shape) * (*shape) * (*shape) *jcb ;
  return(1) ;
}


/* Scientific Computation Research Center, RPI, Troy NY
   (C) Copyright 1995, RPI-SCOREC

   Project: 

   Authors/Dates:
   Rao Garimella, Feb 1995

   Functionality:
   Return the circumradius of tetrahedron

   Argument Variable(s):
   Name  Type In/Out Description
 |------|----|------|-----------------------------------------------------|
*/


double scorecXYZ_circumRad2(dArray *xyz) {
  double mids[3][3], rhs[3];
  double p_mixt, center[3], cradius2;
  int i,j;
  void *temp = 0;

  //vects = dMatAlloc(3,3);
  double vects[3][3];
  for (i = 0; i < 3; i++)
    scorecDiffVt(xyz[i+1], xyz[0], vects[i]);

  for (i = 0; i < 3; i++)
    for (j = 0; j < 3; j++)
      mids[i][j] = (xyz[0][j] + xyz[i+1][j])/2.0;

  for (i = 0; i < 3; i++)
    rhs[i] = scorecDotProd(vects[i],mids[i]);

  p_mixt = scorecDet(3, vects);

  double cols[3][3];
  //cols = dMatAlloc(3,3);
  for (i = 0; i < 3; i++) {
    cols[i][0] = rhs[i];
    cols[i][1] = vects[i][1];
    cols[i][2] = vects[i][2];
  }

  center[0] = (scorecDet(3,cols))/p_mixt;

  for (i = 0; i < 3; i++) {
    cols[i][0] = vects[i][0];
    cols[i][1] = rhs[i];
    cols[i][2] = vects[i][2];
  }

  center[1] = (scorecDet(3,cols))/p_mixt;

  for (i = 0; i < 3; i++) {
    cols[i][0] = vects[i][0];
    cols[i][1] = vects[i][1];
    cols[i][2] = rhs[i];
  }

  center[2] = (scorecDet(3,cols))/p_mixt;
   
  scorecDiffVt(center,xyz[0],vects[0]);

  cradius2 = scorecDotProd(vects[0],vects[0]);

  //dMatFree(cols);
  //dMatFree(vects);

  return cradius2;

} /* R_circumRad2 */


void scorecXYZ_dhdAngs(double xyz[4][3], double *cosAngs) {
  double t1, t2, t3, t4, t6, t7, t10, t11, t12, t13, t14, t16, t17;
  double t20, t22, t23, t25, t28, t29, t30, t32, t35, t40, t44, t46;
  double t48, t49, t50, t52, t53, t54, t55, t57, t60, t62, t65, t67;
  double t47, t71, t76, t78, t79, t80, t81, t82, t84, t90, t95, t100;
  double t102, t103, t104, t105, t106, t108, t114, t115, t121, t122;
  double t128, t129, cos_ang;

  t1 = -xyz[0][1];
  t2 = xyz[2][1]+t1;
  t3 = -xyz[0][2];
  t4 = xyz[1][2]+t3;
  t6 = xyz[2][2]+t3;
  t7 = xyz[1][1]+t1;
  t10 = t2*t4-t6*t7;
  t11 = -xyz[1][1];
  t12 = xyz[2][1]+t11;
  t13 = -xyz[1][2];
  t14 = xyz[3][2]+t13;
  t16 = xyz[2][2]+t13;
  t17 = xyz[3][1]+t11;
  t20 = t12*t14-t16*t17;
  t22 = -xyz[0][0];
  t23 = xyz[1][0]+t22;
  t25 = xyz[2][0]+t22;
  t28 = t6*t23-t25*t4;
  t29 = -xyz[1][0];
  t30 = xyz[3][0]+t29;
  t32 = xyz[2][0]+t29;
  t35 = t16*t30-t32*t14;
  t40 = t25*t7-t2*t23;
  t44 = t32*t17-t12*t30;
  t46 = t10*t20+t28*t35+t40*t44;
  t47 = t46*t46;
  t48 = t10*t10;
  t49 = t28*t28;
  t50 = t40*t40;
  t52 = 1/(t48+t49+t50);
  t53 = t20*t20;
  t54 = t35*t35;
  t55 = t44*t44;
  t57 = 1/(t53+t54+t55);
  t60 = xyz[3][2]+t3;
  t62 = xyz[3][1]+t1;
  t65 = t7*t60-t4*t62;
  t67 = xyz[3][0]+t22;
  t71 = t4*t67-t23*t60;
  t76 = t23*t62-t7*t67;
  t78 = t65*t20+t71*t35+t76*t44;
  t79 = t78*t78;
  t80 = t65*t65;
  t81 = t71*t71;
  t82 = t76*t76;
  t84 = 1/(t80+t81+t82);
  t90 = t62*t6-t60*t2;
  t95 = t60*t25-t67*t6;
  t100 = t67*t2-t62*t25;
  t102 = t90*t20+t95*t35+t100*t44;
  t103 = t102*t102;
  t104 = t90*t90;
  t105 = t95*t95;
  t106 = t100*t100;
  t108 = 1/(t104+t105+t106);
  t114 = t10*t65+t28*t71+t40*t76;
  t115 = t114*t114;
  t121 = t10*t90+t28*t95+t40*t100;
  t122 = t121*t121;
  t128 = t65*t90+t71*t95+t76*t100;
  t129 = t128*t128;

  cos_ang = t115*t52*t84;
  if ( t114 > 0)
    cos_ang = - cos_ang ;
  cosAngs[0] = cos_ang;
  
  cos_ang = t122*t52*t108;
  if ( t121 > 0)
    cos_ang = - cos_ang ;
  
  cosAngs[2] = cos_ang;
  
  cos_ang = t129*t84*t108;
  if ( t128 > 0)
    cos_ang = - cos_ang ;
  
  cosAngs[3] = cos_ang;
  
  cos_ang =  t47*t52*t57;
  if ( t46 > 0)
    cos_ang = - cos_ang ;
  
  cosAngs[1] = cos_ang;
  
  cos_ang =   t79*t84*t57;
  if ( t78 > 0)
    cos_ang = - cos_ang ;
  
  cosAngs[4] = cos_ang;
  
  cos_ang =  t103*t108*t57;
  if ( t102 > 0)
    cos_ang = - cos_ang ;
  
  cosAngs[5] = cos_ang;
}


double scorecXYZ_inscrRad(dArray *xyz) {

  double vects[5][3], res[3];
  double A1, A2, A3, A4, SA, V;
  int i;
  void *temp = 0;
  
  for (i = 0; i < 3; i++)
    scorecDiffVt(xyz[i+1], xyz[0], vects[i]);

  scorecCrossProd(vects[0], vects[1], res);
  V = (scorecDotProd(res, vects[2]))/6.0;
  A1 = sqrt(scorecDotProd(res,res))/2.0;

  scorecCrossProd(vects[1], vects[2], res);
  A2 = sqrt(scorecDotProd(res,res))/2.0;

  scorecCrossProd(vects[2], vects[0], res);
  A3 = sqrt(scorecDotProd(res,res))/2.0;

  scorecDiffVt(xyz[1], xyz[2], vects[3]);
  scorecDiffVt(xyz[2], xyz[3], vects[4]);
  scorecCrossProd(vects[3], vects[4], res);
  A4 = sqrt(scorecDotProd(res, res))/2.0;

  SA = A1 + A2 + A3 + A4;

  return (3*V/SA);

} /* R_inscrRad */
