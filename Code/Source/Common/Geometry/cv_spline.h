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

#ifndef _CVSPLINE_H
#define _CVSPLINE_H

#include "tcl.h"

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

SplinePoints *sys_geom_SplinePointsInit(int numPts, int dimensions);


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

void sys_geom_SplinePointsDelete(SplinePoints *sp);

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

int sys_geom_SplinePath (SplinePoints *input, int type, int numOutputPts, int matchEndPoints, SplinePoints *output);


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
 
void sys_geom_SplineInterpolate (SplinePoints *input, int type, int numOutputPts, SplinePoints *output);


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

int sys_geom_SplineGetTangents(SplinePoints *input, int type, int numOutputPts, int matchEndPoints, SplinePoints *output);



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

void sys_geom_SplineGetRotVectors(SplinePoints *input);


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

int sys_geom_NormalizeVector(vtkFloatingPointType *input, vtkFloatingPointType *output, int sizeVector);

#endif /* _GEOSPLINE_H */


