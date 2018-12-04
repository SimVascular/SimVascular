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
  * \class vtkSVLocalApproximatingSubdivisionFilter - generate a subdivision surface using an Approximating Scheme
  * \section Description
  * vtkSVLocalApproximatingSubdivisionFilter is an abstract class that defines
  * the protocol for Approximating subdivision surface filters.
  *
  * \section Thanks
  * This work was supported by PHS Research Grant No. 1 P41 RR13218-01
  * from the National Center for Research Resources.
  */

#ifndef vtkSVLocalApproximatingSubdivisionFilter_h
#define vtkSVLocalApproximatingSubdivisionFilter_h

#include "vtkSVGeometryModule.h" // for export

#include "vtkCellArray.h"
#include "vtkCellData.h"
#include "vtkIdList.h"
#include "vtkIntArray.h"
#include "vtkPolyDataAlgorithm.h"
#include "vtkPointData.h"
#include "vtkPoints.h"

class VTKSVGEOMETRY_EXPORT vtkSVLocalApproximatingSubdivisionFilter : public vtkPolyDataAlgorithm
{
public:
  vtkTypeMacro(vtkSVLocalApproximatingSubdivisionFilter,vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  /// \brief Get/Set the number of subdivision iterations to perform
  vtkSetMacro(NumberOfSubdivisions,int);
  vtkGetMacro(NumberOfSubdivisions,int);
  //@}

  //@{
  /// \brief Set name for cell array or point array that indicate the
  /// points/cells to subdivide
  vtkSetStringMacro(SubdivideCellArrayName);
  vtkGetStringMacro(SubdivideCellArrayName);
  vtkSetStringMacro(SubdividePointArrayName);
  vtkGetStringMacro(SubdividePointArrayName);
  //@}

  //@{
  /// \brief Indicate whether point or cell arrays should be used.
  vtkSetMacro(UseCellArray, int);
  vtkGetMacro(UseCellArray, int);
  vtkBooleanMacro(UseCellArray, int);
  vtkSetMacro(UsePointArray, int);
  vtkGetMacro(UsePointArray, int);
  vtkBooleanMacro(UsePointArray, int);
  //@}

protected:
  vtkSVLocalApproximatingSubdivisionFilter();
  ~vtkSVLocalApproximatingSubdivisionFilter();

  int RequestData(vtkInformation *, vtkInformationVector **, vtkInformationVector *) override;
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
