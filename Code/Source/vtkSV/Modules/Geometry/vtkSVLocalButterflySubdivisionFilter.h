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
  * \class vtkSVLocalButterflySubdivisionFilter - generate a subdivision surface using the Butterfly Scheme
  * \section Description
  * vtkSVLocalButterflySubdivisionFilter is an interpolating subdivision scheme
  * that creates four new triangles for each triangle in the mesh. The
  * user can specify the NumberOfSubdivisions. This filter implements the
  * 8-point butterfly scheme described in: Zorin, D., Schroder, P., and
  * Sweldens, W., "Interpolating Subdivisions for Meshes with Arbitrary
  * Topology," Computer Graphics Proceedings, Annual Conference Series,
  * 1996, ACM SIGGRAPH, pp.189-192. This scheme improves previous
  * butterfly subdivisions with special treatment of vertices with valence
  * other than 6.
  *
  * Currently, the filter only operates on triangles. Users should use the
  * vtkTriangleFilter to triangulate meshes that contain polygons or
  * triangle strips.
  *
  * The filter interpolates point data using the same scheme. New
  * triangles created at a subdivision step will have the cell data of
  * their parent cell.
  *
  * \section Thanks
  * This work was supported by PHS Research Grant No. 1 P41 RR13218-01
  * from the National Center for Research Resources.
  *
  * \section See Also
  * vtkInterpolatingSubdivisionFilter vtkLinearSubdivisionFilter
  */

#ifndef vtkSVLocalButterflySubdivisionFilter_h
#define vtkSVLocalButterflySubdivisionFilter_h

#include "vtkSVGeometryModule.h" // for export

#include "vtkCellArray.h"
#include "vtkIdList.h"
#include "vtkIntArray.h"

#include "vtkSVLocalInterpolatingSubdivisionFilter.h"

class VTKSVGEOMETRY_EXPORT vtkSVLocalButterflySubdivisionFilter : public vtkSVLocalInterpolatingSubdivisionFilter
{
public:
  // Description:
  // Construct object with NumberOfSubdivisions set to 1.
  static vtkSVLocalButterflySubdivisionFilter *New();
  vtkTypeMacro(vtkSVLocalButterflySubdivisionFilter,vtkSVLocalInterpolatingSubdivisionFilter);

protected:
  vtkSVLocalButterflySubdivisionFilter () {}
  ~vtkSVLocalButterflySubdivisionFilter () {}

private:
  int GenerateSubdivisionPoints(vtkPolyData *inputDS, vtkIntArray *edgeData,
                                vtkPoints *outputPts, vtkPointData *outputPD) override;
  void GenerateButterflyStencil(vtkIdType p1, vtkIdType p2, vtkPolyData *polys,
                                vtkIdList *stencilIds, double *weights);
  void GenerateLoopStencil(vtkIdType p1, vtkIdType p2, vtkPolyData *polys,
                           vtkIdList *stencilIds, double *weights);
  void GenerateBoundaryStencil(vtkIdType p1, vtkIdType p2, vtkPolyData *polys,
                               vtkIdList *stencilIds, double *weights);

  int SetFixedCells(vtkPolyData *pd,int *noSubdivideCell);

private:
  vtkSVLocalButterflySubdivisionFilter(const vtkSVLocalButterflySubdivisionFilter&);  // Not implemented.
  void operator=(const vtkSVLocalButterflySubdivisionFilter&);  // Not implemented.
};

#endif


// VTK-HeaderTest-Exclude: vtkSVLocalButterflySubdivisionFilter.h
