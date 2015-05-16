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

#ifndef __CGEOM_H
#define __CGEOM_H

#include "cvVTK.h"

/*------------------------------------------------------------*
 *                                                            *
 *                    ****  cgeom  ****                       *
 *                                                            *
 *------------------------------------------------------------*/


typedef struct _coord_bin_ {
  int id;
  double x, y, z, d;
  struct _coord_bin_ *link;
  } CoordBin;

typedef struct PolyList {
  int id;
  int loc;
  struct PolyList *next;
  } PolyList;

typedef struct NodeList {
  int id;
  struct NodeList *next;
  } NodeList;

typedef struct EdgeList {
  int poly;
  int node;
  int loc;
  int count;
  int polys[10];
  struct EdgeList *next;
  } EdgeList;

typedef struct EdgeVertexList {
  int node;
  int loc;
  struct EdgeVertexList *next; 
  } EdgeVertexList; 


typedef struct PolyConnList {
  int num;
  int *conn;
  struct PolyConnList *next;
  } PolyConnList;


void
cgeom_VertsCompact (int num_verts, vtkFloatingPointType *verts, int num_polys, int *conn, 
                    int *num_new_verts, vtkFloatingPointType **new_verts);

void
cgeom_CompArea (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn, 
                vtkFloatingPointType *p_area);

void
cgeom_GetPolyCentroid (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn, double centroid[]);

void
cgeom_CompVol (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn,
               double *p_vol);

void
cgeom_PolysClosed (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn, 
                   int *closed);

void
cgeom_PolysSmooth (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn,
                   int level, vtkFloatingPointType **p_sverts);

void
cgeom_FindDegen (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn,
                 vtkFloatingPointType tol, int *p_num, int *id);

void
cgeom_FindVert (int num_cvs, int cv_list[][10], int *vert_stat, int v,
                int *p_id);

void
cgeom_FixDegen (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn,
                vtkFloatingPointType tol, int *p_num_verts, vtkFloatingPointType **p_verts, int *p_num_polys, 
                vtkIdType **p_conn);

void
cgeom_PolysManifold (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn, 
                     int *manifold);

void
cgeom_PolysEdgeTab (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn, 
                    EdgeList ***p_edge_table);

void
cgeom_PolysEdgeConn (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn,
                     EdgeList **edge_table, int id, int *p_ncp, int *cp);

void cgeom_CalcAngle(double *point1, double *point2, double *theta);

void cgeom_CalcCentroid(double *listOfPts, int numPts, int numDim, double *centroid);

#endif /* __CGEOM_H */
