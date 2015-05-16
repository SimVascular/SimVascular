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

/** @file vtkBooleanOperationPolyDataFilter2.cxx
 *  @brief This is the filter to perform boolean operations within SimVascular
 *  @brief This is an improved version of the VTK filters.  
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 */

#ifndef __vtkBooleanOperationPolyDataFilter2_h
#define __vtkBooleanOperationPolyDataFilter2_h

#include "vtkFiltersGeneralModule.h" // For export macro
#include "vtkPolyDataAlgorithm.h"

#include "vtkDataSetAttributes.h" // Needed for CopyCells() method

class vtkIdList;

class VTKFILTERSGENERAL_EXPORT vtkBooleanOperationPolyDataFilter2 : public vtkPolyDataAlgorithm
{
public:
  // Description:
  // Construct object that computes the boolean surface.
  static vtkBooleanOperationPolyDataFilter2 *New();

  vtkTypeMacro(vtkBooleanOperationPolyDataFilter2,
               vtkPolyDataAlgorithm);

  void PrintSelf(ostream& os, vtkIndent indent);

  // Description:
  // Integer describing the number of intersection points and lines
  vtkGetMacro(NumberOfIntersectionPoints, int);
  vtkSetMacro(NumberOfIntersectionPoints, int);
  vtkBooleanMacro(NumberOfIntersectionPoints, int);

  vtkGetMacro(NumberOfIntersectionLines, int);
  vtkSetMacro(NumberOfIntersectionLines, int);
  vtkBooleanMacro(NumberOfIntersectionLines, int);

  // Description:
  // Variable to determine what is output if no intersection occurs.
  // 0 = neither (default), 1 = first, 2 = second, 3 = both
  vtkGetMacro(NoIntersectionOutput, int);
  vtkSetMacro(NoIntersectionOutput, int);
  vtkBooleanMacro(NoIntersectionOutput, int);

  enum OperationType
  {
    VTK_UNION=0,
    VTK_INTERSECTION,
    VTK_DIFFERENCE
  };
  enum NoIntersectionOutputType
  {
    VTK_NEITHER=0,
    VTK_FIRST,
    VTK_SECOND,
    VTK_BOTH,
  };

  // Description:
  // Set the boolean operation to perform. Defaults to union.
  vtkSetClampMacro( Operation, int, VTK_UNION, VTK_DIFFERENCE );
  vtkGetMacro( Operation, int );
  void SetOperationToUnion()
  { this->SetOperation( VTK_UNION ); }
  void SetOperationToIntersection()
  { this->SetOperation( VTK_INTERSECTION ); }
  void SetOperationToDifference()
  { this->SetOperation( VTK_DIFFERENCE ); }

  // Description:
  // Check the status of the filter after update. If the status is zero,
  // there was an error in the operation. If status is one, everything 
  // went smoothly
  vtkGetMacro(Status, int);

  // Description:
  vtkGetMacro(Tolerance, double);
  vtkSetMacro(Tolerance, double);

protected:
  vtkBooleanOperationPolyDataFilter2();
  ~vtkBooleanOperationPolyDataFilter2();

  int RequestData(vtkInformation*, vtkInformationVector**, vtkInformationVector*);
  int FillInputPortInformation(int, vtkInformation*);

private:
  vtkBooleanOperationPolyDataFilter2(const vtkBooleanOperationPolyDataFilter2&); // Not implemented
  void operator=(const vtkBooleanOperationPolyDataFilter2&); // Not implemented

  // Description:
  // PolyDatas for each surface out of intersection and also the intersection
  // lines
  vtkPolyData *OutputSurface;
  // Description:
  // Which operation to perform.
  // Can be VTK_UNION, VTK_INTERSECTION, or VTK_DIFFERENCE.
  int Operation;
  int NoIntersectionOutput;
  int NumberOfIntersectionPoints;
  int NumberOfIntersectionLines;

  int Verbose;
  int Status;
  double Tolerance;

  class Impl;

};

#endif
