/*=========================================================================

  Program:   Visualization Toolkit
  Module:    vtkSVLocalApproximatingSubdivisionFilter.h

  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
  All rights reserved.
  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
// .NAME vtkSVLocalApproximatingSubdivisionFilter - generate a subdivision surface using an Approximating Scheme
// .SECTION Description
// vtkSVLocalApproximatingSubdivisionFilter is an abstract class that defines
// the protocol for Approximating subdivision surface filters.

// .SECTION Thanks
// This work was supported by PHS Research Grant No. 1 P41 RR13218-01
// from the National Center for Research Resources.

#ifndef vtkSVLocalApproximatingSubdivisionFilter_h
#define vtkSVLocalApproximatingSubdivisionFilter_h

#include "vtkPolyDataAlgorithm.h"
#include "vtkSVGeometryModule.h" // for export

class vtkCellArray;
class vtkCellData;
class vtkIdList;
class vtkIntArray;
class vtkPoints;
class vtkPointData;

class VTKSVGEOMETRY_EXPORT vtkSVLocalApproximatingSubdivisionFilter : public vtkPolyDataAlgorithm
{
public:
  vtkTypeMacro(vtkSVLocalApproximatingSubdivisionFilter,vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent);

  // Description:
  // Set/get the number of subdivisions.
  vtkSetMacro(NumberOfSubdivisions,int);
  vtkGetMacro(NumberOfSubdivisions,int);

  vtkSetStringMacro(SubdivideCellArrayName);
  vtkGetStringMacro(SubdivideCellArrayName);
  vtkSetStringMacro(SubdividePointArrayName);
  vtkGetStringMacro(SubdividePointArrayName);

  vtkSetMacro(UseCellArray, int);
  vtkGetMacro(UseCellArray, int);
  vtkBooleanMacro(UseCellArray, int);
  vtkSetMacro(UsePointArray, int);
  vtkGetMacro(UsePointArray, int);
  vtkBooleanMacro(UsePointArray, int);

protected:
  vtkSVLocalApproximatingSubdivisionFilter();
  ~vtkSVLocalApproximatingSubdivisionFilter() {}

  int RequestData(vtkInformation *, vtkInformationVector **, vtkInformationVector *);
  virtual int GenerateSubdivisionPoints (vtkPolyData *inputDS,
                                          vtkIntArray *edgeData,
                                          vtkPoints *outputPts,
                                          vtkPointData *outputPD) = 0;
  void GenerateSubdivisionCells (vtkPolyData *inputDS, vtkIntArray *edgeData,
                                 vtkCellArray *outputPolys,
                                 vtkCellData *outputCD);
  int FindEdge (vtkPolyData *mesh, vtkIdType cellId, vtkIdType p1,
                vtkIdType p2, vtkIntArray *edgeData, vtkIdList *cellIds);
  vtkIdType InterpolatePosition (vtkPoints *inputPts, vtkPoints *outputPts,
                                 vtkIdList *stencil, double *weights);
  vtkIdType KeepPosition (vtkPoints *inputPts, vtkPoints *outputPts,
                                 vtkIdList *stencil, double *weights);

  int GetSubdivideArrays(vtkPolyData *object,int type);
  vtkIntArray      *SubdivideCellArray;
  vtkIntArray      *SubdividePointArray;

  char* SubdivideCellArrayName;
  char* SubdividePointArrayName;
  int UseCellArray;
  int UsePointArray;

  int NumberOfSubdivisions;
private:
  vtkSVLocalApproximatingSubdivisionFilter(const vtkSVLocalApproximatingSubdivisionFilter&);  // Not implemented.
  void operator=(const vtkSVLocalApproximatingSubdivisionFilter&);  // Not implemented.
};


#endif
