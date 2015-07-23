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

#ifndef __CV_SYS_GEOM_H
#define __CV_SYS_GEOM_H

#include "tcl.h"

#include "cvPolyData.h"


cvPolyData *sys_geom_DeepCopy( cvPolyData *src );

cvPolyData *sys_geom_MergePts( cvPolyData *src );

cvPolyData *sys_geom_MergePts_tol( cvPolyData *src, double tol );

int sys_geom_union( cvPolyData *srcA, cvPolyData *srcB,double tolerance, cvPolyData **dst );

int sys_geom_all_union( cvPolyData **src,int numSrcs,int nointerbool,double tolerance, cvPolyData **dst );

int sys_geom_assign_ids_based_on_faces( cvPolyData *model, cvPolyData **faces,int numFaces,int *ids,cvPolyData **dst );

int sys_geom_intersect( cvPolyData *srcA, cvPolyData *srcB,double tolerance, cvPolyData **dst );

int sys_geom_subtract( cvPolyData *srcA, cvPolyData *srcB,double tolerance, cvPolyData **dst );

int sys_geom_checksurface( cvPolyData *src, int stats[],double tolerance);

#ifdef USE_GTS
int sys_geom_union_gts( cvPolyData *srcA, cvPolyData *srcB, cvPolyData **dst );

int sys_geom_intersect_gts( cvPolyData *srcA, cvPolyData *srcB, cvPolyData **dst );

int sys_geom_subtract_gts( cvPolyData *srcA, cvPolyData *srcB, cvPolyData **dst );
#endif

int sys_geom_NumClosedLineRegions( cvPolyData *src, int *num );

int sys_geom_GetClosedLineRegion( cvPolyData *src, int id, cvPolyData **dst );

int sys_geom_Pick( cvPolyData *src, double pos[], cvPolyData **dst );

int sys_geom_Reduce( cvPolyData *src, double tol, cvPolyData **dst );

int sys_geom_ReverseAllCells( cvPolyData *src, cvPolyData **dst );

int sys_geom_GetOrderedPts( cvPolyData *src, double **ord_pts, int *num );

int sys_geom_Get2DPgon( cvPolyData *src, double **pgon, int *num );

int sys_geom_ReversePtList( int num, double ptsIn[], double *ptsOut[] );

int sys_geom_WriteOrderedPts( cvPolyData *src, char *fn );

int sys_geom_WriteLines( cvPolyData *src, char *fn );

int sys_geom_MakePolysConsistent( cvPolyData *src, cvPolyData **dst );

int sys_geom_PolysClosed( cvPolyData *src, int *closed );

int sys_geom_SurfArea( cvPolyData *src, double *area );

int sys_geom_getPolyCentroid( cvPolyData *src, double centroid[]);

int sys_geom_PrintTriStats( cvPolyData *surf );

int sys_geom_PrintSmallPolys( cvPolyData *src, double sideTol );

int sys_geom_RmSmallPolys( cvPolyData *src, double sideTol, cvPolyData **dst );

int sys_geom_BBox( cvPolyData *obj, double bbox[] );

int sys_geom_OrientProfile( cvPolyData *src, double ppt[], double ptan[],
			    double xhat[], cvPolyData **dst );

int sys_geom_DisorientProfile( cvPolyData *src, double ppt[], double ptan[],
			       double xhat[], cvPolyData **dst );

int sys_geom_Translate( cvPolyData *src, double translate[], cvPolyData **dst );

int sys_geom_ScaleAvg( cvPolyData *src, double factor, cvPolyData **dst );

cvPolyData *sys_geom_Align( cvPolyData *ref, cvPolyData *src );

cvPolyData *sys_geom_AlignByDist( cvPolyData *ref, cvPolyData *src );

cvPolyData *sys_geom_ReorderPolygon( cvPolyData *src, int startIx );

int sys_geom_Classify( cvPolyData *obj, double pt[], int *result );

int sys_geom_PtInPoly( cvPolyData *obj, double pt[], int usePrevPoly, int *result );

cvPolyData *sys_geom_sampleLoop( cvPolyData *src, int targetNumPts );

int sys_geom_loft_solid(cvPolyData **srcs,int numSrcs,int useLinearSampleAlongLength,
		int useFFT,int numOutPtsAlongLength, int numOutPtsInSegs,
		int numLinearPtsAlongLength,int numModes,int splineType,double bias,double tension, double continuity,cvPolyData **dst );

int sys_geom_2DWindingNum( cvPolyData *pgn );

int sys_geom_PolygonNormal( cvPolyData *pgn, double n[] );

int sys_geom_AvgPt( cvPolyData *src, double pt[] );

int sys_geom_splinePtsToPathPlan (vtkPolyData *pd,int numOutputPts,
                                  char *filename, int flag, Tcl_Interp *interp);

int sys_geom_InterpolateScalar( cvPolyData *src, double pt[], double *scalar );

int sys_geom_InterpolateVector( cvPolyData *src, double pt[], double vect[] );

int sys_geom_IntersectWithLine( cvPolyData *src, double p0[], double p1[], double intersect[] );

cvPolyData *sys_geom_warp3dPts( cvPolyData *src, double scale );

enum sys_geom_math_scalar {
    SYS_GEOM_NO_SCALAR, 
    SYS_GEOM_ADD_SCALAR,
    SYS_GEOM_SUBTRACT_SCALAR,
    SYS_GEOM_MULTIPLY_SCALAR,
    SYS_GEOM_DIVIDE_SCALAR
};
enum sys_geom_math_vector {
    SYS_GEOM_NO_VECTOR, 
    SYS_GEOM_ADD_VECTOR,
    SYS_GEOM_SUBTRACT_VECTOR,
    SYS_GEOM_MULTIPLY_VECTOR,
    SYS_GEOM_DIVIDE_VECTOR
};

int sys_geom_mathPointData( cvPolyData *srcA, cvPolyData *srcB, sys_geom_math_scalar scflag,
                            sys_geom_math_vector vflag, cvPolyData **dst );

int sys_geom_Project( cvPolyData *srcA, cvPolyData *srcB, sys_geom_math_scalar scflag,
                            sys_geom_math_vector vflag, cvPolyData **dst );

int sys_geom_ReplacePointData( cvPolyData *srcA, cvPolyData *srcB, sys_geom_math_scalar scflag,
                            sys_geom_math_vector vflag, cvPolyData **dst );

int sys_geom_set_array_for_local_op_sphere(cvPolyData *pd,cvPolyData **outpd, double radius,
    double *center,char *outarrayname,int datatype);

int sys_geom_set_array_for_local_op_face(cvPolyData *pd,cvPolyData **outpd, char *inarrayname,
    int *vals, int nvals,char *outarrayname,int datatype);

int sys_geom_set_array_for_local_op_cells(cvPolyData *pd,cvPolyData **outpd, int *vals, int nvals,char *outarrayname,int datatype);

int sys_geom_set_array_for_local_op_face_blend(cvPolyData *pd,cvPolyData **outpd, char *inarrayname,
    int *vals, int nvals,double radius,char *outarrayname,int datatype);

int sys_geom_local_quadric_decimation(cvPolyData *pd,cvPolyData **outpd, double target,
    char *pointarrayname, char *cellarrayname);

int sys_geom_local_laplacian_smooth(cvPolyData *pd,cvPolyData **outpd, int numiters,
    double relax,char *pointarrayname, char *cellarrayname);

int sys_geom_local_constrain_smooth(cvPolyData *pd,cvPolyData **outpd, int numiters,
    double constrainfactor,int numcgsolves,char *pointarrayname, char *cellarrayname);

int sys_geom_local_subdivision(cvPolyData *pd,cvPolyData **outpd, int numiters,
    char *pointarrayname, char *cellarrayname);

#ifdef USE_VMTK
int sys_geom_centerlines( cvPolyData *polydata, int *source, int nsources,
                            int *targets, int ntargets, 
			    cvPolyData **lines, cvPolyData **voronoi);

int sys_geom_distancetocenterlines( cvPolyData *polydata,cvPolyData *lines,cvPolyData **distance );

int sys_geom_cap( cvPolyData *polydata, cvPolyData **cappedpolydata, int *numcenterids,int **centerids,int type);

int sys_geom_cap_with_ids( cvPolyData *polydata, cvPolyData **cappedpolydata, 
		int fillId,int filledholes,int filltype);

int sys_geom_set_ids_for_caps( cvPolyData *pd, cvPolyData **outpd,int **doublecaps,
    int *numfaces); 

int sys_geom_mapandcorrectids( cvPolyData *originalpd, cvPolyData *newpd,cvPolyData **polydata, char *originalarray,char *newarray);
#endif
#endif /* __SYS_GEOM_H */
