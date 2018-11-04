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
 * \class vtkSVLocalLoopSubdivisionFilter - generate a subdivision surface using the Loop Scheme
 * \section Description
 * vtkSVLocalLoopSubdivisionFilter is an approximating subdivision scheme that
 * creates four new triangles for each triangle in the mesh. The user can
 * specify the NumberOfSubdivisions. Loop's subdivision scheme is
 * described in: Loop, C., "Smooth Subdivision surfaces based on
 * triangles,", Masters Thesis, University of Utah, August 1987.
 * For a nice summary of the technique see, Hoppe, H., et. al,
 * "Piecewise Smooth Surface Reconstruction,:, Proceedings of Siggraph 94
 * (Orlando, Florida, July 24-29, 1994). In COmputer Graphics
 * Proceedings, Annual COnference Series, 1994, ACM SIGGRAPH,
 * pp. 295-302.
 * <P>
 * The filter only operates on triangles. Users should use the
 * vtkTriangleFilter to triangulate meshes that contain polygons or
 * triangle strips.
 * <P>
 * The filter approximates point data using the same scheme. New
 * triangles create at a subdivision step will have the cell data of
 * their parent cell.
 *
 * \section Thanks
 * This work was supported by PHS Research Grant No. 1 P41 RR13218-01
 * from the National Center for Research Resources.
 *
 * \section See Also
 * vtkSVLocalApproximatingSubdivisionFilter
 */

#ifndef vtkSVLocalLoopSubdivisionFilter_h
#define vtkSVLocalLoopSubdivisionFilter_h

#include "vtkSVLocalApproximatingSubdivisionFilter.h"
#include "vtkSVGeometryModule.h" // for export

class vtkPolyData;
class vtkIntArray;
class vtkPoints;
class vtkIdList;

class VTKSVGEOMETRY_EXPORT vtkSVLocalLoopSubdivisionFilter : public vtkSVLocalApproximatingSubdivisionFilter
{
public:
  // Description:
  // Construct object with NumberOfSubdivisions set to 1.
  static vtkSVLocalLoopSubdivisionFilter *New();
  vtkTypeMacro(vtkSVLocalLoopSubdivisionFilter,vtkSVLocalApproximatingSubdivisionFilter);

protected:
  vtkSVLocalLoopSubdivisionFilter () {}
  ~vtkSVLocalLoopSubdivisionFilter () {}

  virtual int RequestData(vtkInformation *, vtkInformationVector **, vtkInformationVector *) override;

  int GenerateSubdivisionPoints (vtkPolyData *inputDS, vtkIntArray *edgeData,
                                 vtkPoints *outputPts,
                                 vtkPointData *outputPD) override;
  void GenerateEvenStencil (vtkIdType p1, vtkPolyData *polys,
                            vtkIdList *stencilIds, double *weights);
  void GenerateOddStencil (vtkIdType p1, vtkIdType p2, vtkPolyData *polys,
                           vtkIdList *stencilIds, double *weights);

  int SetFixedCells(vtkPolyData *pd,int *noSubdivideCell);

  virtual int RequestUpdateExtent(vtkInformation *, vtkInformationVector **, vtkInformationVector *) override;

private:
  vtkSVLocalLoopSubdivisionFilter(const vtkSVLocalLoopSubdivisionFilter&);  // Not implemented.
  void operator=(const vtkSVLocalLoopSubdivisionFilter&);  // Not implemented.
};

#endif
// VTK-HeaderTest-Exclude: vtkSVLocalLoopSubdivisionFilter.h
