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
 *  \class vtkSVLocalInterpolatingSubdivisionFilter
 *  \brief This implements localized subdivision
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVLocalInterpolatingSubdivisionFilter_h
#define vtkSVLocalInterpolatingSubdivisionFilter_h

#include "vtkSVGeometryModule.h" // for export

#include "vtkPolyDataAlgorithm.h"

class VTKSVGEOMETRY_EXPORT vtkSVLocalInterpolatingSubdivisionFilter : public vtkPolyDataAlgorithm
{
public:
  vtkTypeMacro(vtkSVLocalInterpolatingSubdivisionFilter,vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

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
  vtkSVLocalInterpolatingSubdivisionFilter();
  ~vtkSVLocalInterpolatingSubdivisionFilter();

  int RequestData(vtkInformation *, vtkInformationVector **, vtkInformationVector *) override;
  virtual int GenerateSubdivisionPoints (vtkPolyData *inputDS, vtkIntArray *edgeData, vtkPoints *outputPts, vtkPointData *outputPD) = 0;
  virtual void GenerateSubdivisionCells (vtkPolyData *inputDS, vtkIntArray *edgeData, vtkCellArray *outputPolys, vtkCellData *outputCD);
  int FindEdge (vtkPolyData *mesh, vtkIdType cellId, vtkIdType p1,
                vtkIdType p2, vtkIntArray *edgeData, vtkIdList *cellIds);
  vtkIdType InterpolatePosition (vtkPoints *inputPts, vtkPoints *outputPts,
                                 vtkIdList *stencil, double *weights);

  int GetSubdivideArrays(vtkPolyData *object,int type);
  vtkIntArray 	   *SubdivideCellArray;
  vtkIntArray      *SubdividePointArray;

  char* SubdivideCellArrayName;
  char* SubdividePointArrayName;
  int UseCellArray;
  int UsePointArray;

  int NumberOfSubdivisions;

private:
  vtkSVLocalInterpolatingSubdivisionFilter(const vtkSVLocalInterpolatingSubdivisionFilter&);  // Not implemented.
  void operator=(const vtkSVLocalInterpolatingSubdivisionFilter&);  // Not implemented.
};

#endif


