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

#ifndef __CGEOM_H
#define __CGEOM_H

#include "sv_VTK.h"

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

#include "SimVascular.h"
#include "svUtilsExports.h" // For exports

void SV_EXPORT_UTILS
cgeom_VertsCompact (int num_verts, vtkFloatingPointType *verts, int num_polys, int *conn,
                    int *num_new_verts, vtkFloatingPointType **new_verts);

void SV_EXPORT_UTILS
cgeom_CompArea (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn,
                vtkFloatingPointType *p_area);

void SV_EXPORT_UTILS
cgeom_GetPolyCentroid (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn, double centroid[]);

void SV_EXPORT_UTILS
cgeom_CompVol (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn,
               double *p_vol);

void SV_EXPORT_UTILS
cgeom_PolysClosed (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn,
                   int *closed);

void SV_EXPORT_UTILS
cgeom_PolysSmooth (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn,
                   int level, vtkFloatingPointType **p_sverts);

void SV_EXPORT_UTILS
cgeom_FindDegen (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn,
                 vtkFloatingPointType tol, int *p_num, int *id);

void SV_EXPORT_UTILS
cgeom_FindVert (int num_cvs, int cv_list[][10], int *vert_stat, int v,
                int *p_id);

void SV_EXPORT_UTILS
cgeom_FixDegen (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn,
                vtkFloatingPointType tol, int *p_num_verts, vtkFloatingPointType **p_verts, int *p_num_polys,
                vtkIdType **p_conn);

void SV_EXPORT_UTILS
cgeom_PolysManifold (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn,
                     int *manifold);

void SV_EXPORT_UTILS
cgeom_PolysEdgeTab (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn,
                    EdgeList ***p_edge_table);

void SV_EXPORT_UTILS
cgeom_PolysEdgeConn (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn,
                     EdgeList **edge_table, int id, int *p_ncp, int *cp);

void SV_EXPORT_UTILS cgeom_CalcAngle(double *point1, double *point2, double *theta);

void SV_EXPORT_UTILS cgeom_CalcCentroid(double *listOfPts, int numPts, int numDim, double *centroid);

#endif /* __CGEOM_H */
