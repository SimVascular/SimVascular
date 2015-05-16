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

#include "SimVascular.h" 

#include <math.h>
#include "cv_ggems.h"


/*=========================  ggemsGeometrical Procedures  ======================= */

Rdouble ggemsGeoDotProd ( ggemsGeoPoint *vec0, ggemsGeoPoint *vec1 )
{
 return ( vec0->x * vec1->x + vec0->y * vec1->y + vec0->z * vec1->z );
}

void ggemsGeoCrossProd ( ggemsGeoPoint *in0, ggemsGeoPoint *in1, ggemsGeoPoint *out )
{
 out->x = (in0->y * in1->z) - (in0->z * in1->y);
 out->y = (in0->z * in1->x) - (in0->x * in1->z);
 out->z = (in0->x * in1->y) - (in0->y * in1->x);
}

Rdouble ggemsGeoTripleProd ( ggemsGeoPoint *vec0, ggemsGeoPoint *vec1, ggemsGeoPoint *vec2 )
{
 ggemsGeoPoint tmp;

 ggemsGeoCrossProd ( vec0, vec1, &tmp );
 return ( ggemsGeoDotProd( &tmp, vec2 ) );
}

Rdouble ggemsGeoVecLen ( ggemsGeoPoint *vec )
{
 return sqrt ( ggemsGeoDotProd ( vec, vec ) );
}

int ggemsGeoPolyNormal ( int	n_verts, ggemsGeoPoint *verts, ggemsGeoPoint *n ) 
{
 int      i;
 ggemsGeoPoint v0, v1, p; 

 ggemsGeoZeroVec ( *n );
 ggemsGeo_Vet ( verts[0], verts[1], v0 );
 for ( i = 2; i < n_verts; i++ )
     {
      ggemsGeo_Vet ( verts[0], verts[i], v1 );
      ggemsGeoCrossProd ( &v0, &v1, &p );
      n->x += p.x; n->y += p.y; n->z += p.z;
      v0 = v1;
     }

 Rdouble n_size = ggemsGeoVecLen ( n );
 if (n_size > 0.0)
    {
     ggemsGeoMultVec ( 1/n_size, *n, *n );
     return 1;
    }
 else
     return 0;
}

/*=========================  ggemsgeo_solid_angle  =========================*/
/* 
  Calculates the solid angle given by the spherical projection of 
  a 3D plane polygon
*/

Rdouble ggemsgeo_solid_angle ( 
        int      n_vert,  /* number of vertices */
        ggemsGeoPoint *verts,  /* vertex coordinates list */
        ggemsGeoPoint *p )     /* point to be tested */
{
 int      i;
 Rdouble  area = 0.0, ang, s, l1, l2;
 ggemsGeoPoint p1, p2, r1, a, b, n1, n2;
 ggemsGeoPoint plane;

 if ( n_vert < 3 ) return 0.0;

 ggemsGeoPolyNormal ( n_vert, verts, &plane );
 
 /* 
    WARNING: at this point, a practical implementation should check
    whether p is too close to the polygon plane. If it is, then
    there are two possibilities: 
      a) if the projection of p onto the plane is outside the 
         polygon, then area zero should be returned;
      b) otherwise, p is on the polyhedron boundary.
 */ 

 p2 = verts[n_vert-1];  /* last vertex */
 p1 = verts[0];         /* first vertex */
 ggemsGeo_Vet ( p1, p2, a ); /* a = p2 - p1 */  

 for ( i = 0; i < n_vert; i++ )
     {
      ggemsGeo_Vet(*p, p1, r1); 
      p2 = verts[(i+1)%n_vert];
      ggemsGeo_Vet ( p1, p2, b );
      ggemsGeoCrossProd ( &a, &r1, &n1 );
      ggemsGeoCrossProd ( &r1, &b, &n2 );
    
      l1 = ggemsGeoVecLen ( &n1 );
      l2 = ggemsGeoVecLen ( &n2 );
      s  = ggemsGeoDotProd ( &n1, &n2 ) / ( l1 * l2 );
      ang = acos ( maximum(-1.0,minimum(1.0,s)) );
      s = ggemsGeoTripleProd( &b, &a, &plane );
      area += s > 0.0 ? PI - ang : PI + ang;
     
      ggemsGeoMultVec ( -1.0, b, a );
      p1 = p2;
     }

 area -= PI*(n_vert-2);

 return ( ggemsGeoDotProd ( &plane, &r1 ) > 0.0 ) ? -area : area; 
}


/*=============  Eric Haines' point-in-polygon functions  =============*/


#define X 0
#define Y 1

/* --------------------------- */
/* ggems_CrossingsMultiplyTest */
/* --------------------------- */

int ggems_CrossingsMultiplyTest( double pgon[], int numverts, double point[] )
{
register int	j, yflag0, yflag1, inside_flag ;
register double	ty, tx, vtx0[2], vtx1[2], *mark ;

    tx = point[X] ;
    ty = point[Y] ;

    /* Start vtx0 at end of point list: */
    vtx0[0] = pgon[(numverts-1)*2] ;
    vtx0[1] = pgon[(numverts-1)*2+1] ;
    /* get test bit for above/below X axis */
    yflag0 = ( vtx0[Y] >= ty ) ;

    /* Wrap vtx1 to beginning: */
    mark = pgon;
    vtx1[0] = mark[0] ;
    vtx1[1] = mark[1] ;

    inside_flag = 0 ;
    for ( j = numverts+1 ; --j ; ) {

	yflag1 = ( vtx1[Y] >= ty ) ;
	/* Check if endpoints straddle (are on opposite sides) of X axis
	 * (i.e. the Y's differ); if so, +X ray could intersect this edge.
	 * The old test also checked whether the endpoints are both to the
	 * right or to the left of the test point.  However, given the faster
	 * intersection point computation used below, this test was found to
	 * be a break-even proposition for most polygons and a loser for
	 * triangles (where 50% or more of the edges which survive this test
	 * will cross quadrants and so have to have the X intersection computed
	 * anyway).  I credit Joseph Samosky with inspiring me to try dropping
	 * the "both left or both right" part of my code.
	 */
	if ( yflag0 != yflag1 ) {
	    /* Check intersection of pgon segment with +X ray.
	     * Note if >= point's X; if so, the ray hits it.
	     * The division operation is avoided for the ">=" test by checking
	     * the sign of the first vertex wrto the test point; idea inspired
	     * by Joseph Samosky's and Mark Haigh-Hutchinson's different
	     * polygon inclusion tests.
	     */
	    if ( ((vtx1[Y]-ty) * (vtx0[X]-vtx1[X]) >=
		    (vtx1[X]-tx) * (vtx0[Y]-vtx1[Y])) == yflag1 ) {
		inside_flag = !inside_flag ;
	    }
	}

	/* Move to the next pair of vertices, retaining info as possible. */
	yflag0 = yflag1 ;
	vtx0[0] = vtx1[0] ;
	vtx0[1] = vtx1[1] ;
	mark += 2;
	vtx1[0] = mark[0];
	vtx1[1] = mark[1];
    }

    return( inside_flag ) ;
}

#undef	QUADRANT
#undef	X_INTERCEPT
