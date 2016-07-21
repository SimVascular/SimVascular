/*=========================================================================

  Program:   Visualization Toolkit
  Module:    vtkLocalButterflySubdivisionFilter.h

  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
  All rights reserved.
  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
// .NAME vtkLocalButterflySubdivisionFilter - generate a subdivision surface using the Butterfly Scheme
// .SECTION Description
// vtkLocalButterflySubdivisionFilter is an interpolating subdivision scheme
// that creates four new triangles for each triangle in the mesh. The
// user can specify the NumberOfSubdivisions. This filter implements the
// 8-point butterfly scheme described in: Zorin, D., Schroder, P., and
// Sweldens, W., "Interpolating Subdivisions for Meshes with Arbitrary
// Topology," Computer Graphics Proceedings, Annual Conference Series,
// 1996, ACM SIGGRAPH, pp.189-192. This scheme improves previous
// butterfly subdivisions with special treatment of vertices with valence
// other than 6.
//
// Currently, the filter only operates on triangles. Users should use the
// vtkTriangleFilter to triangulate meshes that contain polygons or
// triangle strips.
//
// The filter interpolates point data using the same scheme. New
// triangles created at a subdivision step will have the cell data of
// their parent cell.

// .SECTION Thanks
// This work was supported by PHS Research Grant No. 1 P41 RR13218-01
// from the National Center for Research Resources.

// .SECTION See Also
// vtkInterpolatingSubdivisionFilter vtkLinearSubdivisionFilter

#ifndef vtkLocalButterflySubdivisionFilter_h
#define vtkLocalButterflySubdivisionFilter_h

#include "SimVascular.h"

#include "vtkFiltersModelingModule.h" // For export macro
#include "vtkLocalInterpolatingSubdivisionFilter.h"

class vtkCellArray;
class vtkIdList;
class vtkIntArray;

class SV_EXPORT_SYSGEOM vtkLocalButterflySubdivisionFilter : public vtkLocalInterpolatingSubdivisionFilter
{
public:
  // Description:
  // Construct object with NumberOfSubdivisions set to 1.
  static vtkLocalButterflySubdivisionFilter *New();
  vtkTypeMacro(vtkLocalButterflySubdivisionFilter,vtkLocalInterpolatingSubdivisionFilter);

protected:
  vtkLocalButterflySubdivisionFilter () {}
  ~vtkLocalButterflySubdivisionFilter () {}

private:
  int GenerateSubdivisionPoints(vtkPolyData *inputDS, vtkIntArray *edgeData,
                                vtkPoints *outputPts, vtkPointData *outputPD);
  void GenerateButterflyStencil(vtkIdType p1, vtkIdType p2, vtkPolyData *polys,
                                vtkIdList *stencilIds, double *weights);
  void GenerateLoopStencil(vtkIdType p1, vtkIdType p2, vtkPolyData *polys,
                           vtkIdList *stencilIds, double *weights);
  void GenerateBoundaryStencil(vtkIdType p1, vtkIdType p2, vtkPolyData *polys,
                               vtkIdList *stencilIds, double *weights);

  int SetFixedCells(vtkPolyData *pd,int *noSubdivideCell);

private:
  vtkLocalButterflySubdivisionFilter(const vtkLocalButterflySubdivisionFilter&);  // Not implemented.
  void operator=(const vtkLocalButterflySubdivisionFilter&);  // Not implemented.
};

#endif


// VTK-HeaderTest-Exclude: vtkLocalButterflySubdivisionFilter.h
