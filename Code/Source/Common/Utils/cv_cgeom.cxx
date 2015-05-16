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

/*------------------------------------------------------------*
 *                                                            *
 *                    ****  cgeom  ****                       *
 *                                                            *
 *------------------------------------------------------------*/


#include "SimVascular.h" 

#include "cv_cgeom.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define JPK_PI 3.14159265358979


/*------------------------------------------------------------*
 *                                                            *
 *              ****  cgeom_VertsCompact  ****                *
 *                                                            *
 *------------------------------------------------------------*/

void
cgeom_VertsCompact (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn, 
                    int *num_new_verts, vtkFloatingPointType **new_verts)
  {

  int v;

  int i, j;

  vtkFloatingPointType x, y, z;

  vtkIdType *new_conn;

  int n;

  vtkFloatingPointType maxx, maxy, maxz, minx, miny, minz;

  CoordBin **hash_table;

  int size;

  int index;

  int found;

  int offset;

  CoordBin *ptr;

  CoordBin *pptr;

  vtkFloatingPointType tx, ty, tz; 

  vtkFloatingPointType dx, dy, dz;

  int id_count;

  int id;

  static vtkFloatingPointType f = 0.00001;

 /**************
  ***  body  ***
  **************/

  new_conn = conn;

  minx = maxx = verts[0];
  miny = maxy = verts[1];
  minz = maxz = verts[2];

  for (i = 0; i < num_verts; i++) {
    x = verts[3*i];
    y = verts[3*i+1];
    z = verts[3*i+2];

    minx = (x < minx ? x : minx);
    miny = (y < miny ? y : miny);
    minz = (z < minz ? z : minz);
    maxx = (x > maxx ? x : maxx);
    maxy = (y > maxy ? y : maxy);
    maxz = (z > maxz ? z : maxz);
    }

  size = num_verts / 10 + 2;
  hash_table = (CoordBin **)malloc(sizeof(CoordBin *) * (size+1));

  /*
  printf (" ---->  num_verts [%d \n", num_verts);
  printf (" ---->  size  [%d \n", size);
  */

  for (i = 0; i <= size; i++) {
    hash_table[i] = NULL;
    }

  dx = maxx - minx;
  dy = maxy - miny;
  dz = maxz - minz;

  tx = dx * f; 
  ty = dy * f; 
  tz = dz * f; 
  offset = sqrt(1.0*num_verts);
  id_count = 0;

  for (i = 0; i < num_polys; i++) {
    n = *conn;
    conn++;
    new_conn++;

    for (j = 0; j < n; j++) {
      v = *conn;
      conn++;

      x = verts[3*v];
      y = verts[3*v+1];
      z = verts[3*v+2];
      /*
      fprintf (stderr, " %g %g %g  ", x, y, z);
      */

      index = offset*((x-minx)/dx + offset*((y-miny)/dy + offset*((z-minz)/dz)));
      index = index % size;

      ptr = hash_table[index];
      found = 0;

      while (ptr) {
        if ((fabs(ptr->x - x) <= tx) &&
            (fabs(ptr->y - y) <= ty) &&
            (fabs(ptr->z - z) <= tz)) {
          found = 1;
          break;
          }

        ptr = ptr->link;
        }

      if (!found) {
        ptr = (CoordBin *)malloc(sizeof(CoordBin));
        ptr->x = x;
        ptr->y = y;
        ptr->z = z;

        ptr->id = id_count++;
        ptr->link = hash_table[index];
        hash_table[index] = ptr;
        }

      *new_conn = ptr->id;
      new_conn++;
      /*
      fprintf (stderr, "   |  hash --> %d \n", *new_conn);
      fprintf (stderr, "   |  hash --> %d \n", ptr->id);
      */
      }
    }

  for (i = 0; i < size; i++) {
    ptr = hash_table[i]; 

    while (ptr != 0) { 
      id = ptr->id;
      verts[3*id] = ptr->x;
      verts[3*id+1] = ptr->y;
      verts[3*id+2] = ptr->z;
      ptr = ptr->link;
      }
    }

  /*
  printf (" num verts [%d] \n", num_verts);
  printf (" num unique verts [%d] \n", id_count);
  */

  for (i = 0; i < size; i++) {
    ptr = hash_table[i]; 

    while (ptr) {
      pptr = ptr;
      ptr = ptr->link;
      free (pptr);
      }

    }

  free (hash_table);
  *num_new_verts = id_count;
  *new_verts = verts;
  }


/*------------------------------------------------------------*
 *                                                            *
 *                    ****  cgeom_CompArea  ****              *
 *                                                            *
 *------------------------------------------------------------*/

void
cgeom_CompArea (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn, 
                vtkFloatingPointType *p_area)
  {

  vtkFloatingPointType area;

  int i, j, p, n, u, v;

  vtkFloatingPointType xp[20], yp[20], zp[20];

  vtkFloatingPointType nx, ny, nz;

 /**************
  ***  body  ***
  **************/

  area = 0.0;

  for (i = 0, p = 0; i < num_polys; i++) {
    n = conn[p++];

   for (j = 0; j < n; j++) {
      v = conn[p++];
      xp[j] = verts[3*v];
      yp[j] = verts[3*v+1];
      zp[j] = verts[3*v+2];
      }
 
    nx = ny = nz = 0.0;

    for (u = 0; u < n; u++) {
      if (u == (n - 1)) {
        v = 0;
        }
      else {
        v = u + 1;
        }

      nx += yp[v]*zp[u] - zp[v]*yp[u];
      ny += zp[v]*xp[u] - xp[v]*zp[u];
      nz += xp[v]*yp[u] - yp[v]*xp[u];
      }

    area += sqrt(nx*nx + ny*ny + nz*nz) / 2.0;
    }

  *p_area = area;
  }


/*------------------------------------------------------------*
 *                                                            *
 *                    ****  cgeom_CompVol  ****               *
 *                                                            *
 *------------------------------------------------------------*/

void
cgeom_CompVol (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn, 
               vtkFloatingPointType *p_vol)
  {

  vtkFloatingPointType vol;

  int i, j, p, n, u, v;

  vtkFloatingPointType xp[20], yp[20], zp[20];

  vtkFloatingPointType nx, ny, nz;

 /**************
  ***  body  ***
  **************/

  vol = 0.0;

  for (i = 0, p = 0; i < num_polys; i++) {
    n = conn[p++];

   for (j = 0; j < n; j++) {
      v = conn[p++];
      xp[j] = verts[3*v];
      yp[j] = verts[3*v+1];
      zp[j] = verts[3*v+2];
      }
 
    nx = ny = nz = 0.0;

    for (u = 0; u < n; u++) {
      if (u == (n - 1)) {
        v = 0;
        }
      else {
        v = u + 1;
        }

      nx += zp[v]*yp[u] - yp[v]*zp[u]; 
      ny += xp[v]*zp[u] - zp[v]*xp[u]; 
      nz += yp[v]*xp[u] - xp[v]*yp[u]; 
      }

    vol += nx*xp[0] + ny*yp[0] + nz*zp[0];
    }

  *p_vol = vol / 6.0;
  }


/*------------------------------------------------------------*
 *                                                            *
 *                ****  cgeom_PolysClosed  ****               *
 *                                                            *
 *------------------------------------------------------------*/

void
cgeom_PolysClosed (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn, 
                   int *closed)
  {

  int i;

  EdgeList **edge_table;

  EdgeList *elist;

 /**************
  ***  body  ***
  **************/

  cgeom_PolysEdgeTab (num_verts, verts, num_polys, conn, &edge_table);
  *closed = 1;

  for (i = 0; i < num_verts; i++) {
    elist = edge_table[i]; 

    while (elist) {
      if (elist->count == 1) {
        *closed = 0;
        return;
        }

      elist = elist->next;
      }
    }
  }


/*------------------------------------------------------------*
 *                                                            *
 *              ****  cgeom_NormsComp  ****                   *
 *                                                            *
 *------------------------------------------------------------*/

void
cgeom_NormsComp (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn, 
                 vtkFloatingPointType **p_norms)
  {

  int v;

  int node;

  int vlist[10];

  int i, j, k;

  vtkFloatingPointType xp[10], yp[10], zp[10];

  vtkFloatingPointType v1[3], v2[3];
  
  vtkFloatingPointType a, b, c, mag;

  int average;

  vtkFloatingPointType *normals;

  int n;

 /**************
  ***  body  ***
  **************/

  average = 1;

  if (average) {
    normals = (vtkFloatingPointType *)malloc(sizeof(vtkFloatingPointType) * num_verts * 3);

    for (i = 0; i < num_verts; i++) {
      normals[3*i] = 0.0;
      normals[3*i+1] = 0.0;
      normals[3*i+2] = 0.0;
      }

    for (i = 0, j = 0; i < num_polys; i++) {
      n = conn[j++];

      for (k = 0; k < n; k++) {
	v = conn[j++];
	vlist[k] = v;
	xp[k] = verts[3*v];
	yp[k] = verts[3*v+1];
	zp[k] = verts[3*v+2];
	}

      v1[0] = xp[1] - xp[0];
      v1[1] = yp[1] - yp[0];
      v1[2] = zp[1] - zp[0];

      v2[0] = xp[2] - xp[0];
      v2[1] = yp[2] - yp[0];
      v2[2] = zp[2] - zp[0];

      a = -v1[2]*v2[1] + v1[1]*v2[2];
      b = -v1[0]*v2[2] + v1[2]*v2[0];
      c = -v1[1]*v2[0] + v1[0]*v2[1];

      for (k = 0; k < n; k++) {
	node = vlist[k];
	normals[3*node] += a;
	normals[3*node+1] += b;
	normals[3*node+2] += c;
	}
      }

    for (i = 0; i < num_verts; i++) {
      a = normals[3*i]; 
      b = normals[3*i+1]; 
      c = normals[3*i+2]; 
      mag = sqrt(a*a + b*b + c*c);

      if (mag != 0.0) {
        normals[3*i] = a / mag;
        normals[3*i+1] = b / mag;
        normals[3*i+2] = c / mag;

        normals[3*i] = -a / mag;
        normals[3*i+1] = -b / mag;
        normals[3*i+2] = -c / mag;
	}
      }
    }

  *p_norms = normals;
  }


/*------------------------------------------------------------*
 *                                                            *
 *                ****  cgeom_PolysSmooth  ****               *
 *                                                            *
 *------------------------------------------------------------*/

void
cgeom_PolysSmooth (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn, 
                   int level, vtkFloatingPointType **p_sverts)
  {

  int i, j;

  vtkFloatingPointType x, y, z;

  vtkFloatingPointType ax, ay, az;

  int node;

  int num, n, m;

  EdgeVertexList **edge_table;

  EdgeVertexList *elist, *edge, *prev_edge;

  int nlist[100], cnlist[1000];

  vtkFloatingPointType wgts[100], w;

  int n1, n2;

  int loc;    

  vtkFloatingPointType *sverts;

  vtkFloatingPointType px, py, pz;

  vtkFloatingPointType dx, dy, dz;

  vtkFloatingPointType td;

 /**************
  ***  body  ***
  **************/

  edge_table = (EdgeVertexList**)malloc(sizeof(EdgeVertexList*) * num_verts);

  for (i = 0; i < num_verts; i++) {
    edge_table[i] = NULL;
    }


  /*  build vertex-vertex table.  */

  for (i = 0, n = 0; i < num_polys; i++) {
    loc = n;
    num = conn[n++];

    for (j = 0; j < num; j++) {
      node = conn[n++];
      nlist[j] = node;
      }

    for (j = 0; j < num; j++) {
      n1 = nlist[j];
      n2 = nlist[(j + 1) % num];
      elist = prev_edge = edge_table[n1];

      while (elist) {
        if (elist->node == n2) {
          break;
          }
        prev_edge = elist;
        elist = elist->next;
        }

      if (!elist) {
        edge = (EdgeVertexList*)malloc(sizeof(EdgeVertexList));
        edge->node = n2;
        edge->loc = loc;
        edge->next = edge_table[n1];
        edge_table[n1] = edge;
        }
      }
    }

  sverts = (vtkFloatingPointType*)malloc(sizeof(vtkFloatingPointType) * num_verts * 3);

  for (i = 0; i < num_verts; i++) {
    elist = edge_table[i]; 
    px = verts[3*i];
    py = verts[3*i+1];
    pz = verts[3*i+2];
    n = 0;
    m = 0;
    td = 0.0;

    while (elist) {
      x = verts[3*elist->node];
      y = verts[3*elist->node+1];
      z = verts[3*elist->node+2];
      dx = x - px;
      dy = y - py;
      dz = z - pz;
      wgts[n] = sqrt (dx*dx + dy*dy + dz*dz);
      td += wgts[n];
      cnlist[m++] = elist->node;
      nlist[n] = elist->node;
      n += 1;
      elist = elist->next;
      }

    if (level == 2) {
      for (j = 0; j < m; j++) {
        node = cnlist[j];
        elist = edge_table[node]; 

        while (elist) {
          if (elist->node != i) {
            x = verts[3*elist->node];
            y = verts[3*elist->node+1];
            z = verts[3*elist->node+2];
            dx = x - px;
            dy = y - py;
            dz = z - pz;
            wgts[n] = sqrt (dx*dx + dy*dy + dz*dz);
            td += wgts[n];
            nlist[n] = elist->node;
            n += 1;
            }

          elist = elist->next;
          }
        }
      }

    ax = ay = az = 0.0;

    for (j = 0; j < n; j++) {
      node = nlist[j];
      x = verts[3*node];
      y = verts[3*node+1];
      z = verts[3*node+2];
      w = (td - wgts[j]) / td;
      w = 1.0;
      ax += w*x;
      ay += w*y;
      az += w*z;
      }

    sverts[3*i] = ax / (vtkFloatingPointType)n;
    sverts[3*i+1] = ay / (vtkFloatingPointType)n;
    sverts[3*i+2] = az / (vtkFloatingPointType)n;
    }

  *p_sverts = sverts;
  }


/*------------------------------------------------------------*
 *                                                            *
 *                    ****  cgeom_FindDegen  ****             *
 *                                                            *
 *------------------------------------------------------------*/

void
cgeom_FindDegen (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn, 
                 vtkFloatingPointType tol, int *p_num, int *id)
  {

  vtkFloatingPointType tot_area, area, min_area, max_area;

  int i, j, p, n, u, v;

  vtkFloatingPointType xp[20], yp[20], zp[20];

  vtkFloatingPointType nx, ny, nz;

  int num;

  vtkFloatingPointType atol; 

 /**************
  ***  body  ***
  **************/

  fprintf (stderr, "\n  ------  cgeom_FindDegen  ------  \n");
  num = 0;
  tot_area = 0.0;
  atol = tol * tol;

  for (i = 0, p = 0; i < num_polys; i++) {
    n = conn[p++];

    for (j = 0; j < n; j++) {
      v = conn[p++];
      xp[j] = verts[3*v];
      yp[j] = verts[3*v+1];
      zp[j] = verts[3*v+2];
      }
 
    nx = ny = nz = 0.0;

    for (u = 0; u < n; u++) {
      if (u == (n - 1)) {
        v = 0;
        }
      else {
        v = u + 1;
        }

      nx += yp[v]*zp[u] - zp[v]*yp[u];
      ny += zp[v]*xp[u] - xp[v]*zp[u];
      nz += xp[v]*yp[u] - yp[v]*xp[u];
      }

    area = sqrt(nx*nx + ny*ny + nz*nz) / 2.0;
    tot_area += area;

    if (i == 0) {
      min_area = area;
      max_area = area;
      }
    else if (area < min_area) {
      min_area = area;
      *id = i;
      }
    else if (area > max_area) {
      max_area = area;
      }

    if (area < atol) {
      num += 1;
      }
    }

  fprintf (stderr, "  >>>>>> min_area [%f]. \n", min_area);
  fprintf (stderr, "  >>>>>> max_area [%f]. \n", max_area);
  *p_num = num;
  }


/*------------------------------------------------------------*
 *                                                            *
 *                    ****  cgeom_FixDegen  ****              *
 *                                                            *
 * fix a degenerate polymesh. fixed mesh is returned.         *
 *------------------------------------------------------------*/

void
cgeom_FixDegen (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn, 
                vtkFloatingPointType tol, int *p_num_verts, vtkFloatingPointType **p_verts, int *p_num_polys, 
                vtkIdType **p_conn)
  {

  vtkFloatingPointType area;

  int i, j, k, m, id, p, n, u, v;

  vtkFloatingPointType x, y, z;

  vtkFloatingPointType xp[20], yp[20], zp[20];

  vtkFloatingPointType nx, ny, nz;

  int num;

  vtkFloatingPointType dx, dy, dz, d;

  vtkFloatingPointType x1, y1, z1, x2, y2, z2;

  vtkFloatingPointType atol;

  int nlist[100];

  int num_new_verts;

  int new_size;

  int num_new_polys;

  vtkFloatingPointType *new_verts;

  int pok;

  vtkIdType *new_conn;

  EdgeList **edge_table;

  int ncp, cp[1000];

  int *vert_map, vid;

  int num_degen_polys, num_del_polys;

  int *degen_polys, *del_polys;

  int num_degen_edges;

  int num_del_verts;

  int np;

 /**************
  ***  body  ***
  **************/

  fprintf (stderr, "\n  ----------  geom_fix_degen  ----------  \n");
  fprintf (stderr, "  >>>>>>  num polys [%d] \n", num_polys);
  fprintf (stderr, "  >>>>>>  num verts [%d] \n", num_verts);

  cgeom_PolysEdgeTab (num_verts, verts, num_polys, conn, &edge_table);

  atol = tol * tol;
  num = 0;
  vert_map = (int *)malloc(sizeof(int) * num_verts);
  degen_polys = (int *)malloc(sizeof(int) * num_polys);
  del_polys = (int *)malloc(sizeof(int) * num_polys);
  num_degen_polys = 0;
  num_del_polys = 0;

  for (i = 0; i < num_verts; i++) {
    vert_map[i] = (i+1);
    }

  for (i = 0, p = 0; i < num_polys; i++) {
    n = conn[p++];

    for (j = 0; j < n; j++) {
      v = conn[p++];
      xp[j] = verts[3*v];
      yp[j] = verts[3*v+1];
      zp[j] = verts[3*v+2];
      }
 
    nx = ny = nz = 0.0;

    for (u = 0; u < n; u++) {
      if (u == (n - 1)) {
        v = 0;
        }
      else {
        v = u + 1;
        }

      nx += yp[v]*zp[u] - zp[v]*yp[u];
      ny += zp[v]*xp[u] - xp[v]*zp[u];
      nz += xp[v]*yp[u] - yp[v]*xp[u];
      }

    area = sqrt(nx*nx + ny*ny + nz*nz) / 2.0;
    /*
    fprintf (stderr, " area [%f] \n", area);
    */

    if (area < atol) {
      degen_polys[num_degen_polys++] = i; 
      }
    }

  fprintf (stderr, "\n  >>>>>>  num degen poly [%d] \n", num_degen_polys);
  num_new_verts = 0;
  num_del_verts = 0;

  for (i = 0; i < num_degen_polys; i++) {
    id = degen_polys[i]; 
    fprintf (stderr, "\n  ----  process poly [%d] ---- \n", id);

    for (j = 0, p = 0; j < num_polys; j++) {
      n = conn[p++];

      if (j != id) {
        p += n;
        continue;
        }

      for (k = 0; k < n; k++) {
        v = conn[p++];
        nlist[k] = v;
        xp[k] = verts[3*v];
        yp[k] = verts[3*v+1];
        zp[k] = verts[3*v+2];
        }

      num_degen_edges = 0;

      for (k = 0; k < n; k++) {
        x1 = xp[k];
        y1 = yp[k];
        z1 = zp[k];
        m = (k+1) % n;
        x2 = xp[m];
        y2 = yp[m];
        z2 = zp[m];

        dx = x1 - x2;
        dy = y1 - y2;
        dz = z1 - z2;
        d = sqrt (dx*dx + dy*dy + dz*dz);

        if (d < tol) {
          num_degen_edges += 1;
          }
        }

      fprintf (stderr, "  >>>>>>  num degen edges [%d] \n", num_degen_edges);

      if (num_degen_edges == n) {
        fprintf (stderr, "  >>>>>>  collapse poly to point. \n");
        vid = nlist[0];

        for (k = 1; k < n; k++) {
          v = nlist[k];
          vert_map[v] = -(vid+1);
          fprintf (stderr, "  >>>>>>  remap vert [%d] to [%d]. \n", v, vid);
          num_del_verts += 1;
          }

        cgeom_PolysEdgeConn (num_verts, verts, num_polys, conn, edge_table,
                             id, &ncp, cp);

        for (j = 0; j < ncp; j++) {
          del_polys[num_del_polys++] = cp[j];
          fprintf (stderr, "  >>>>>>  del poly [%d]. \n", cp[j]);
          }

        del_polys[num_del_polys++] = id;
        break;
        }
      }
    }

  num_new_verts = num_verts - num_del_verts;
  new_verts = (vtkFloatingPointType*)malloc(sizeof(vtkFloatingPointType) * 3 * num_new_verts);
  num_new_verts = 0; 

  for (i = 0; i < num_verts; i++) {
    x = verts[3*i];
    y = verts[3*i+1];
    z = verts[3*i+2];

    if (vert_map[i] > 0) {
      new_verts[3*num_new_verts] = x;
      new_verts[3*num_new_verts+1] = y;
      new_verts[3*num_new_verts+2] = z;
      vert_map[i] = num_new_verts;
      num_new_verts += 1;
      }
    }

  fprintf (stderr, "  >>>>>>  del [%d] verts \n", num_verts - num_new_verts);
  fprintf (stderr, "\n  ------ build new polys ------ \n");
  new_size = 0;
  num_new_polys = 0;

  for (i = 0, p = 0; i < num_polys; i++) {
    pok = 1;
    n = conn[p++];

    for (j = 0; j < num_del_polys; j++) {
      if (del_polys[j] == i) {
        pok = 0;
        break;
        }
      }

    if (pok) {
      new_size += n + 1;
      num_new_polys += 1;
      }

    p += n;
    }

  fprintf (stderr, "  >>>>>>  delete [%d] polys. \n", num_polys - num_new_polys);
  new_conn = (vtkIdType *)malloc(sizeof(vtkIdType) * new_size);
  fprintf (stderr, "\n  ------  gen new polys  ------  \n"); 

  for (i = 0, p = 0, np = 0; i < num_polys; i++) {
    pok = 1;
    n = conn[p++];

    for (j = 0; j < num_del_polys; j++) {
      if (del_polys[j] == i) {
        pok = 0;
        break;
        }
      }

    if (pok) {
      new_conn[np++] = n;

      for (j = 0; j < n; j++) {
        v = conn[p++];
 
        if (vert_map[v] < 0) {
          vid = -(vert_map[v] + 1);
          new_conn[np++] = vert_map[vid]; 
          }
        else {
          new_conn[np++] = vert_map[v];
          }
        }
      }
    else {
      p += n;
      }
    }

  *p_num_verts = num_new_verts;
  *p_verts = new_verts;
  *p_num_polys = num_new_polys;
  *p_conn = new_conn;
  }


/*------------------------------------------------------------*
 *                                                            *
 *                    ****  cgeom_FindVert  ****              *
 *                                                            *
 *------------------------------------------------------------*/

void
cgeom_FindVert (int num_cvs, int cv_list[][10], int *vert_stat, int v, 
                int *p_id)
  {

  int i, j;

  int id, cid;

 /**************
  ***  body  ***
  **************/

  for (i = 0; i < num_cvs; i++) {
    cid = cv_list[i][1];

    for (j = 1; j <= cv_list[i][0]; j++) {
      id = cv_list[i][j];

      if (v == id) { 
        *p_id = cid;
        return;
        }
      }
    }
  }


/*------------------------------------------------------------*
 *                                                            *
 *                ****  cgeom_PolysManifold  ****             *
 *                                                            *
 *------------------------------------------------------------*/

void
cgeom_PolysManifold (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn, 
                      int *manifold)
{

  EdgeList **edge_table;

 /**************
  ***  body  ***
  **************/

  cgeom_PolysEdgeTab (num_verts, verts, num_polys, conn, &edge_table);
}


/*------------------------------------------------------------*
 *                                                            *
 *                ****  cgeom_PolysEdgeTab  ****              *
 *                                                            *
 *------------------------------------------------------------*/

void
cgeom_PolysEdgeTab (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn, 
                    EdgeList ***p_edge_table)
  {

  int i, j;

  int node;

  int num, n;

  EdgeList **edge_table;

  EdgeList *elist, *edge;

  int nlist[100];

  int min_n, max_n;

  int n1, n2;

  int loc;    

 /**************
  ***  body  ***
  **************/

  edge_table = (EdgeList**)malloc(sizeof(EdgeList*) * num_verts);

  for (i = 0; i < num_verts; i++) {
    edge_table[i] = NULL;
    }


  /*  build edge-poly table.  */

  for (i = 0, n = 0; i < num_polys; i++) {
    loc = n;
    num = conn[n++];

    for (j = 0; j < num; j++) {
      node = conn[n++];
      nlist[j] = node;
      }

    for (j = 0; j < num; j++) {
      n1 = nlist[j];
      n2 = nlist[(j + 1) % num];

      if (n1 > n2) {
        max_n = n1;
        min_n = n2;
        }
      else { 
        max_n = n2;
        min_n = n1;
        }

      elist = edge_table[min_n];

      while (elist) {
        if (elist->node == max_n) {
          elist->polys[elist->count] = i;
          elist->count += 1;
          break;
          }
        elist = elist->next; 
        }

      if (!elist) {
        edge = (EdgeList*)malloc(sizeof(EdgeList));
        edge->poly = i;
        edge->node = max_n;
        edge->loc = loc;
        edge->count = 1;
        edge->polys[0] = i;
        edge->next = edge_table[min_n];
        edge_table[min_n] = edge;
        }
      }
    }

  *p_edge_table = edge_table;
  }


/*------------------------------------------------------------*
 *                                                            *
 *                ****  cgeom_PolysEdgeConn  ****             *
 *                                                            *
 *------------------------------------------------------------*/

void
cgeom_PolysEdgeConn (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn, 
                     EdgeList **edge_table, int id, int *p_ncp, int *cp)
  {

  int i, j, k;

  int node;

  int num, n;

  EdgeList *elist;

  int nlist[100];

  int min_n, max_n;

  int n1, n2;

  int ncp;

 /**************
  ***  body  ***
  **************/

  ncp = 0;

  for (i = 0, n = 0; i < num_polys; i++) {
    num = conn[n++];

    if (i != id) {
      n += num;
      continue;
      }

    for (j = 0; j < num; j++) {
      node = conn[n++];
      nlist[j] = node;
      }

    for (j = 0; j < num; j++) {
      n1 = nlist[j];
      n2 = nlist[(j + 1) % num];

      if (n1 > n2) {
        max_n = n1;
        min_n = n2;
        }
      else {
        max_n = n2;
        min_n = n1;
        }

      elist = edge_table[min_n];

      while (elist) {
        if (elist->node == max_n) {
          for (k = 0; k < elist->count; k++) {
            if (elist->polys[k] != id) {
              cp[ncp++] = elist->polys[k]; 
              }
            }
          }

        elist = elist->next; 
        }
      }
    break;
    }

  *p_ncp = ncp;
  }




/**************************************************************
 *                                                            *
 * cgeom_CalcAngle                                            *
 * ---------------                                            *
 *                                                            *
 * Returns the angle between point2 and the x-axis            *
 * with point1 acting as the fulcrum                          *
 *                                                            *
 * ASSUMES that points lie in the same z-plane                *
 *                                                            *
 *                                 # point2                   *
 *                               /                            *
 *                      point1 #/_________# pt on +x-axis     *
 *                                          relative to point1*
 *                                                            *
 * point1[0] = x-value, point1[1] = y-value,                  *
 *      point1[2] = z-value                                   *
 *                                                            *
 * 0 <= angle < 360                                           *
 *                                                            *
 **************************************************************/

void cgeom_CalcAngle(double *point1, double *point2, double *theta)
{

  double *vector1, *vector2;

  double innerProd, norm;

  int i;


  /**************
   ***  body  ***
   **************/  

  vector1 = new double[3];
  vector2 = new double[3];

  vector1[0] = 1;
  vector1[1] = 0;
  vector1[2] = 0;


  vector2[0] = point2[0] - point1[0];
  vector2[1] = point2[1] - point1[1];
  vector2[2] = point2[2] - point1[2];
  
  norm = sqrt(pow(vector2[0],2) + pow(vector2[1],2));
  
  for (i = 0; i < 3; i++)
    vector2[i] = vector2[i] / norm;

  innerProd = vector1[0] * vector2[0] \
               + vector1[1] * vector2[1] \
               + vector1[2] * vector2[2];
  
  *theta = acos(innerProd);

  /* Convert to degrees */
  *theta = *theta * 180.0 / JPK_PI;

 
  /* 
   * Convert theta to be in correct quadrant...acos produces
   * angle only between 0 and 180 degrees
   */
  if (vector2[1] < 0)
    *theta = 360 - *theta;


  // Clean up
  delete [] vector1;
  delete [] vector2;
}




/**************************************************************
 *                                                            *
 * cgeom_CalcCentroid                                         *
 * ------------------                                         *
 *                                                            *
 * Returns the point that is the centroid of the given array  *
 * of points                                                  *
 *                                                            *
 * format for listOfPts:  pt1_X, pt1_Y, pt1_Z, pt2_X, pt2_Y,  *
 *                        pt2_Z, ....                         *
 * note that the number of coordinates used in specifying each*
 * point is dependent on the variable numDim                  *
 *                                                            *
 **************************************************************/


void cgeom_CalcCentroid(double *listOfPts, int numPts, int numDim, double *centroid)
{
  int i, j;

  double total;

  /**************
   ***  body  ***
   **************/  

  for (j = 0; j < numDim; j++)
    {
  
      total = 0.0;

      for (i = 0; i < numPts; i++)
	  total += listOfPts[i * 3 + j];

      centroid[j] = total / (double)(numPts);
    }
}



/**************************************************************
 *                                                            *
 * cgeom_GetPolyCentroid                                      *
 * ---------------------                                      *
 *                                                            *
 * Returns the point that is the centroid of a polygon.       *
 * Assumes that the polygon has been triangulated and has x,  *
 * y, z coordinates (Polydata is in 3D space).                *
 * Result is returned in rtn_centroid--needs to be an array of*
 * at least THREE elements.                                   *
 *                                                            *
 *                                                            *
 *                                                            *
 *                                                            *
 *                                                            *
 **************************************************************/

void
cgeom_GetPolyCentroid (int num_verts, vtkFloatingPointType *verts, int num_polys, vtkIdType *conn, double rtn_centroid[])
{

  int i, j, poly_idx, num_pts_in_poly, u, v, vert_id;

  vtkFloatingPointType xp[20], yp[20], zp[20];

  vtkFloatingPointType nx, ny, nz;

  vtkFloatingPointType sum_x, sum_y, sum_z;

  vtkFloatingPointType wght_cnt_x, wght_cnt_y, wght_cnt_z;

  vtkFloatingPointType area, sum_area;

 /**************
  ***  body  ***
  **************/

  wght_cnt_x = 0.0;
  wght_cnt_y = 0.0;
  wght_cnt_z = 0.0;

  sum_area = 0.0;

  for (i = 0, poly_idx = 0; i < num_polys; i++) {
   num_pts_in_poly = conn[poly_idx++];

   // Get the vertices associated with this poly
   for (j = 0; j < num_pts_in_poly; j++) {
      vert_id = conn[poly_idx++];
      xp[j] = verts[3*vert_id];
      yp[j] = verts[3*vert_id+1];
      zp[j] = verts[3*vert_id+2];
      }
 
   nx = ny = nz = 0.0;
   sum_x = sum_y = sum_z = 0.0;

   // For each poly, need to determine the area and the
   // centroid (for a triangle, this is just the average of
   // the coordinates)
   for (u = 0; u < num_pts_in_poly; u++) {
      if (u == (num_pts_in_poly - 1)) {
        v = 0;
      }
      else {
        v = u + 1;
      }

      nx += yp[v]*zp[u] - zp[v]*yp[u];
      ny += zp[v]*xp[u] - xp[v]*zp[u];
      nz += xp[v]*yp[u] - yp[v]*xp[u];

      sum_x += xp[u];
      sum_y += yp[u];
      sum_z += zp[u];
   }

   area = sqrt(nx*nx + ny*ny + nz*nz) / 2.0;

   sum_area += area;
   wght_cnt_x += area * sum_x / num_pts_in_poly;
   wght_cnt_y += area * sum_y / num_pts_in_poly;
   wght_cnt_z += area * sum_z / num_pts_in_poly;

  }

  rtn_centroid[0] = wght_cnt_x / sum_area;
  rtn_centroid[1] = wght_cnt_y / sum_area;
  rtn_centroid[2] = wght_cnt_z / sum_area;
}



