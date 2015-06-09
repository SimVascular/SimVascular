/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved. 
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
 *
 *=========================================================================*/

/** @file vtkLocalInterpolatingSubdivisionFilter.h
 *  @brief This implements localized subdivision
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 */

#ifndef vtkLocalInterpolatingSubdivisionFilter_h
#define vtkLocalInterpolatingSubdivisionFilter_h

#include "vtkFiltersGeneralModule.h" // For export macro
#include "vtkPolyDataAlgorithm.h"

class vtkCellArray;
class vtkCellData;
class vtkIdList;
class vtkIntArray;
class vtkPointData;
class vtkPoints;
class vtkPolyData;

class VTKFILTERSGENERAL_EXPORT vtkLocalInterpolatingSubdivisionFilter : public vtkPolyDataAlgorithm
{
public:
  vtkTypeMacro(vtkLocalInterpolatingSubdivisionFilter,vtkPolyDataAlgorithm);
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
  vtkLocalInterpolatingSubdivisionFilter();
  ~vtkLocalInterpolatingSubdivisionFilter() {}

  int RequestData(vtkInformation *, vtkInformationVector **, vtkInformationVector *);
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
  vtkLocalInterpolatingSubdivisionFilter(const vtkLocalInterpolatingSubdivisionFilter&);  // Not implemented.
  void operator=(const vtkLocalInterpolatingSubdivisionFilter&);  // Not implemented.
};

#endif


