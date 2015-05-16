/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * All rights reserved.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson, Ken Wang.
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

#ifndef __CVGGEMS_H
#define __CVGGEMS_H

#include "cv_misc_utils.h"


#define PI 3.141592653589793324
#define ggemsGeoZeroVec(v) ((v).x = (v).y = (v).z = 0.0)
#define ggemsGeoMultVec(a,b,c) do {(c).x = a*(b).x; (c).y = a*(b).y;	(c).z = a*(b).z; } while (0)
#define ggemsGeo_Vet(a,b,c) do {(c).x = (b).x-(a).x; (c).y = (b).y-(a).y; (c).z = (b).z-(a).z;} while (0)

typedef double Rdouble;
typedef float  Rfloat;
typedef struct _ggemsGeoPoint { Rfloat x, y, z; } ggemsGeoPoint;

/*=========================  ggemsGeometrical Procedures  ======================= */

Rdouble ggemsGeoDotProd ( ggemsGeoPoint *vec0, ggemsGeoPoint *vec1 );

void ggemsGeoCrossProd ( ggemsGeoPoint *in0, ggemsGeoPoint *in1, ggemsGeoPoint *out );

Rdouble ggemsGeoTripleProd ( ggemsGeoPoint *vec0, ggemsGeoPoint *vec1, ggemsGeoPoint *vec2 );

Rdouble ggemsGeoVecLen ( ggemsGeoPoint *vec );

int ggemsGeoPolyNormal ( int	n_verts, ggemsGeoPoint *verts, ggemsGeoPoint *n );


/*=========================  ggemsgeo_solid_angle  =========================*/
/* 
  Calculates the solid angle given by the spherical projection of 
  a 3D plane polygon
*/

Rdouble ggemsgeo_solid_angle ( 
        int      n_vert,  /* number of vertices */
        ggemsGeoPoint *verts,  /* vertex coordinates list */
        ggemsGeoPoint *p );    /* point to be tested */



/*=============  Eric Haines' point-in-polygon functions  =============*/

/* I'm only going to include a subset of Haines' functions (as given
 * in Graphics Gems IV, Chapter I).  Note that the test program which
 * comes with the GGems code uses polygons with points in a CCW order.
 * The end point is not repeated.  Both the polygon (pgon) and the
 * test point (point) are 2D.  The returned result is 1 for inside, 0
 * for outside.  Haines indicates that the crossings multiply
 * algorithm is slightly faster than the crossings algorithm, and that
 * both of these are much better than the angle test.
 */

int ggems_CrossingsMultiplyTest( double pgon[], int numverts,
				 double point[] );

#endif /* __GGEMS_H */
