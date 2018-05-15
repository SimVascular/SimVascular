/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
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
 */

#ifndef __CVGGEMS_H
#define __CVGGEMS_H

#include "sv_misc_utils.h"
#include "svGeometryExports.h" // For exports

#define PI 3.141592653589793324
#define ggemsGeoZeroVec(v) ((v).x = (v).y = (v).z = 0.0)
#define ggemsGeoMultVec(a,b,c) do {(c).x = a*(b).x; (c).y = a*(b).y;	(c).z = a*(b).z; } while (0)
#define ggemsGeo_Vet(a,b,c) do {(c).x = (b).x-(a).x; (c).y = (b).y-(a).y; (c).z = (b).z-(a).z;} while (0)

typedef double Rdouble;
typedef float  Rfloat;
typedef struct _ggemsGeoPoint { Rfloat x, y, z; } ggemsGeoPoint;

/*=========================  ggemsGeometrical Procedures  ======================= */

SV_EXPORT_SYSGEOM Rdouble ggemsGeoDotProd ( ggemsGeoPoint *vec0, ggemsGeoPoint *vec1 );

SV_EXPORT_SYSGEOM void ggemsGeoCrossProd ( ggemsGeoPoint *in0, ggemsGeoPoint *in1, ggemsGeoPoint *out );

SV_EXPORT_SYSGEOM Rdouble ggemsGeoTripleProd ( ggemsGeoPoint *vec0, ggemsGeoPoint *vec1, ggemsGeoPoint *vec2 );

SV_EXPORT_SYSGEOM Rdouble ggemsGeoVecLen ( ggemsGeoPoint *vec );

SV_EXPORT_SYSGEOM int ggemsGeoPolyNormal ( int	n_verts, ggemsGeoPoint *verts, ggemsGeoPoint *n );


/*=========================  ggemsgeo_solid_angle  =========================*/
/*
  Calculates the solid angle given by the spherical projection of
  a 3D plane polygon
*/

SV_EXPORT_SYSGEOM Rdouble ggemsgeo_solid_angle (
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

SV_EXPORT_SYSGEOM int ggems_CrossingsMultiplyTest( double pgon[], int numverts,
				 double point[] );

#endif /* __GGEMS_H */
