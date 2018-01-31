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

#ifndef _CVSPLINE_H
#define _CVSPLINE_H

#include "tcl.h"
#include "SimVascular.h"
#include "svGeometryExports.h" // For exports

/****************
 *  TYPEDEFS    *
 ****************/

typedef struct _SplinePoints
{
  int numPts;
  int dim;
  vtkFloatingPointType *pts;
  vtkFloatingPointType *tangents;
  vtkFloatingPointType *rotVectors;
} SplinePoints;


/****************
 *  CONSTANTS   *
 ****************/

#define MAX_DIM 4  /* Maximum number of dimensions vtk handles for spline interpolation */


/***********************
 *  SPLINE PROCEDURES  *
 ***********************/


/***********************************************************************
 *                                                                     *
 * sys_geom_SplinePointsInit                                           *
 * -------------------------                                           *
 * Initializes the struct SplinePoints                                 *
 *                                                                     *
 * INPUTS                                                              *
 * ------                                                              *
 * int numPts:  number of points in spline                             *
 * int dim:  number of dimensions of each point (1 to 4)               *
 *                                                                     *
 * OUTPUTS                                                             *
 * -------                                                             *
 ***********************************************************************/

SV_EXPORT_SYSGEOM SplinePoints *sys_geom_SplinePointsInit(int numPts, int dimensions);


/***********************************************************************
 *                                                                     *
 * sys_geom_SplinePointsDelete                                         *
 * ---------------------------                                         *
 * Frees up the memory associated with the struct SplinePoints         *
 *                                                                     *
 * INPUTS                                                              *
 * ------                                                              *
 * SplinePoints *sp:  SplinePoints structure to delete                 *
 ***********************************************************************/

SV_EXPORT_SYSGEOM void sys_geom_SplinePointsDelete(SplinePoints *sp);

/***********************************************************************
 *                                                                     *
 * sys_geom_SplinePath                                                 *
 * -------------------                                                 *
 * Performs spline interpolation and computes tangents and rotation    *
 * vectors.                                                            *
 *                                                                     *
 * INPUTS                                                              *
 * ------                                                              *
 * SplinePoints *input:  set of input points for interpolation, limited*
 *                       to MAX_DIM dimensions (vtk limitation).       *
 *                       input->points are in the format x1, y1, x2,   *
 *                       y2, x3, y3, ....                              *
 * int type:  specifies basis used in spline interpolation             *
 *               type = 1 --> cardinal basis                           *
 *               type = 2 --> Kochanek basis                           *
 *               see vtk for more details                              *
 * int numOutputPts:  determines number of points to interpolate       *
 *                                                                     *
 * OUTPUTS                                                             *
 * -------                                                             *
 * SplinePoints *output:  interpolated points, in the format x1, y1,   *
 *                        x2, y2, x3, y3, ....                         *
 *                        tangents are normalized vectors in the format*
 *                        x1, y1, x2, y2, x3, y3, ....                 *
 *                        rotVectors are normalized vectors in the     *
 *                        format x1, y1, x2, y2, x3, y3,...            *
 *                        The number of coordinates for each point and *
 *                        each vector is dependent on the dimensions   *
 *                        of the input.                                *
 *                                                                     *
 * EXAMPLE                                                             *
 * -------                                                             *
 * Given 5 points in variable x, you want to use the cardinal          *
 * basis for spline interpolation to get out 25 points in variable y.  *
 *                                                                     *
 *    sys_geom_SplinePath (x, 1, 25, y)                                *
 *                                                                     *
 ***********************************************************************/

SV_EXPORT_SYSGEOM int sys_geom_SplinePath (SplinePoints *input, int type, int numOutputPts, int matchEndPoints, SplinePoints *output);


/***********************************************************************
 *                                                                     *
 * sys_geom_SplineInterpolate                                          *
 * --------------------------                                          *
 * Performs spline interpolation.                                      *
 *                                                                     *
 * INPUTS                                                              *
 * ------                                                              *
 * SplinePoints *input:  set of input points for interpolation, limited*
 *                       to 4 dimensions (vtk limitation).  input->    *
 *                       points are in the format x1, y1, x2, y2,      *
 *                       x3, y3, ....                                  *
 * int type:  specifies basis used in spline interpolation             *
 *               type = 1 --> cardinal basis                           *
 *               type = 2 --> Kochanek basis                           *
 *               see vtk for more details                              *
 * int numOutputPts:  determines number of points to interpolate       *
 *                                                                     *
 * OUTPUTS                                                             *
 * -------                                                             *
 * SplinePoints *output:  interpolated points, in the format x1, y1,   *
 *                        x2, y2, x3, y3, ....                         *
 *                        The number of coordinates for each point     *
 *                        is dependent on the dimensions of the input. *
 *                                                                     *
 * EXAMPLE                                                             *
 * -------                                                             *
 * Given 5 points in variable x, you want to use the cardinal          *
 * basis for spline interpolation to get out 25 points in variable y.  *
 *                                                                     *
 *    sys_geom_SplineInterpolate (x, 1, 25, y)                         *
 *                                                                     *
 ***********************************************************************/

SV_EXPORT_SYSGEOM void sys_geom_SplineInterpolate (SplinePoints *input, int type, int numOutputPts, SplinePoints *output);


/***********************************************************************
 *                                                                     *
 * sys_geom_SplineGetTangents                                          *
 * --------------------------                                          *
 * Performs spline interpolation and computes tangents.                *
 *                                                                     *
 * INPUTS                                                              *
 * ------                                                              *
 * SplinePoints *input:  set of input points for interpolation, limited*
 *                       to MAX_DIM dimensions (vtk limitation).       *
 *                       input->points are in the format x1, y1, x2,   *
 *                       y2, x3, y3, ....                              *
 * int type:  specifies basis used in spline interpolation             *
 *               type = 1 --> cardinal basis                           *
 *               type = 2 --> Kochanek basis                           *
 *               see vtk for more details                              *
 * int numOutputPts:  determines number of points to interpolate       *
 * int matchEndPoints:  true if end points of spline should match end *
 *                       points of input;  otherwise, false.           *
 *                                                                     *
 * OUTPUTS                                                             *
 * -------                                                             *
 * SplinePoints *output:  interpolated points, in the format x1, y1,   *
 *                        x2, y2, x3, y3, ....                         *
 *                        tangents are normalized vectors in the format*
 *                        x1, y1, x2, y2, x3, y3, ....                 *
 *                        The number of coordinates for each point and *
 *                        each vector is dependent on the dimensions   *
 *                        of the input.                                *
 *                                                                     *
 * EXAMPLE                                                             *
 * -------                                                             *
 * Given 5 points in variable x, you want to use the cardinal          *
 * basis for spline interpolation to get out 25 points in variable y.  *
 *                                                                     *
 *    sys_geom_SplineGetTangents (x, 1, 25, y)                         *
 *                                                                     *
 ***********************************************************************/

SV_EXPORT_SYSGEOM int sys_geom_SplineGetTangents(SplinePoints *input, int type, int numOutputPts, int matchEndPoints, SplinePoints *output);



/***********************************************************************
 *                                                                     *
 * sys_geom_SplineGetRotVectors                                        *
 * ----------------------------                                        *
 * Computes a rotational/orientation vector.  In this case, it is just *
 * a vector that is perpendicular to the tangent.  input is updated    *
 * with the rotation vectors.                                          *
 *                                                                     *
 * INPUTS                                                              *
 * ------                                                              *
 * SplinePoints *input:  set of input points for interpolation, limited*
 *                       to MAX_DIM dimensions (vtk limitation).       *
 *                       input->points are in the format x1, y1, x2,   *
 *                       y2, x3, y3, ....                              *
 ***********************************************************************/

SV_EXPORT_SYSGEOM void sys_geom_SplineGetRotVectors(SplinePoints *input);


/***********************************************************************
 *                                                                     *
 * sys_geom_NormalizeVector                                            *
 * ------------------------                                            *
 * Returns the normalized vector.                                      *
 *                                                                     *
 * INPUTS                                                              *
 * ------                                                              *
 * vtkFloatingPointType *input:  vector to be normalized
 * int sizeVector:  number of elements in input array                  *
 *                                                                     *
 * OUTPUTS                                                             *
 * -------                                                             *
 * vtkFloatingPointType *output: normalized vector                                    *
 * error code returned (0 if OK, 1 if error)                           *
 ***********************************************************************/

SV_EXPORT_SYSGEOM int sys_geom_NormalizeVector(vtkFloatingPointType *input, vtkFloatingPointType *output, int sizeVector);

#endif /* _GEOSPLINE_H */


