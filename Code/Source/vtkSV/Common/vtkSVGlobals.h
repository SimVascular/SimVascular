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

/**
 *  \brief Global defines
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVGlobals_h
#define vtkSVGlobals_h

#include <vector>

#include "vtkSmartPointer.h"

#ifndef vtkNew
#define vtkNew(type,name) \
  vtkSmartPointer<type> name = vtkSmartPointer<type>::New()
#endif

#ifndef SV_ERROR
#define SV_ERROR 0
#endif

#ifndef SV_OK
#define SV_OK 1
#endif

#ifndef VTK_SV_DOUBLE_TOL
#define VTK_SV_DOUBLE_TOL 1.0E-12
#endif

#ifndef VTK_SV_LARGE_DOUBLE
#define VTK_SV_LARGE_DOUBLE 1.0E+32
#endif

#ifndef SV_PI
#define SV_PI 3.141592653589793324
#endif

#ifndef svmaximum
#define svmaximum(A, B) ((A) > (B) ? (A) : (B))
#endif

#ifndef svminimum
#define svminimum(A, B) ((A) < (B) ? (A) : (B))
#endif

/// \brief directions of nodes in graph simplification
typedef enum SV_CUBE_DIRECTIONS
{
  RIGHT = 0,
  BACK,
  LEFT,
  FRONT,
}
SV_DIRECTIONS;

/// \brief possible cube types
typedef enum SV_CUBE_END_TYPES
{
  NONE = 0,
  VERT_WEDGE,
  HORZ_WEDGE,
  C_TET_0, // Corner tets
  C_TET_1,
  C_TET_2,
  C_TET_3,
  S_TET_0, // Side tets
  S_TET_1,
  S_TET_2,
  S_TET_3,
  NOTHANDLED
}
SV_END_TYPES;

/// \brief possible split types
typedef enum SV_CUBE_SPLIT_TYPE
{
  ZERO = 0,
  UNO,
  BI,
  TRI,
  QUAD,
  PENT,
  TOOMANY
}
SV_SPLIT_TYPES;

/** \brief Data structure to contain regions of cell scalar labels on a polydata surface.
 * Typically used with clustering algorithms, this data structure allows quick
 * access to the cells in the cluster and the points on the edges of the cluster.
 */
struct Region
{
  int Index;
  int IndexCluster;

  int NumberOfCorners;
  std::vector<int> CornerPoints;

  std::vector<std::vector<int> > BoundaryEdges;

  int NumberOfElements;
  std::vector<int> Elements;

};

/** \brief A small data structure to contain 3D points in a std::vector or list. */
struct XYZ
{
  double x;
  double y;
  double z;
};

#endif
