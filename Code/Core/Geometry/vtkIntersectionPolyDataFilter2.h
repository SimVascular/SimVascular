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

/** @file vtkIntersectionPolyDataFilter2.cxx
 *  @brief This is a filter that performs the intersection between to surfaces
 *  @brief 
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 */

#ifndef __vtkIntersectionPolyDataFilter2_h
#define __vtkIntersectionPolyDataFilter2_h

#include "vtkFiltersGeneralModule.h" // For export macro
#include "vtkPolyDataAlgorithm.h"

class VTKFILTERSGENERAL_EXPORT vtkIntersectionPolyDataFilter2 : public vtkPolyDataAlgorithm
{
public:
  static vtkIntersectionPolyDataFilter2 *New();
  vtkTypeMacro(vtkIntersectionPolyDataFilter2, vtkPolyDataAlgorithm);
  virtual void PrintSelf(ostream &os, vtkIndent indent);

  // Description:
  // Integer describing the number of intersection points and lines
  vtkGetMacro(NumberOfIntersectionPoints, int);
  vtkSetMacro(NumberOfIntersectionPoints, int);
  vtkBooleanMacro(NumberOfIntersectionPoints, int);
  vtkGetMacro(NumberOfIntersectionLines, int);
  vtkSetMacro(NumberOfIntersectionLines, int);
  vtkBooleanMacro(NumberOfIntersectionLines, int);

  // Description:
  // If on, the second output will be the first input mesh split by the
  // intersection with the second input mesh. Defaults to on.
  vtkGetMacro(SplitFirstOutput, int);
  vtkSetMacro(SplitFirstOutput, int);
  vtkBooleanMacro(SplitFirstOutput, int);

  // Description:
  // If on, the third output will be the second input mesh split by the
  // intersection with the first input mesh. Defaults to on.
  vtkGetMacro(SplitSecondOutput, int);
  vtkSetMacro(SplitSecondOutput, int);
  vtkBooleanMacro(SplitSecondOutput, int);

  // Description:
  // If on, the third output will be the second input mesh split by the
  // intersection with the first input mesh. Defaults to on.
  vtkGetMacro(ApplyBoundaryPointArray, int);
  vtkSetMacro(ApplyBoundaryPointArray, int);
  vtkBooleanMacro(ApplyBoundaryPointArray, int);

  // Description:
  // If on, the normals of the input will be checked.
  vtkGetMacro(CheckInput, int);
  vtkSetMacro(CheckInput, int);
  vtkBooleanMacro(CheckInput, int);

  // Description:
  // If on, the third output will be the second input mesh split by the
  // intersection with the first input mesh. Defaults to on.
  vtkGetMacro(CheckMesh, int);
  vtkSetMacro(CheckMesh, int);
  vtkBooleanMacro(CheckMesh, int);

  // Description:
  // Check the status of the filter after update. If the status is zero,
  // there was an error in the operation. If status is one, everything 
  // went smoothly
  vtkGetMacro(Status, int);

  // Description:
  vtkGetMacro(Tolerance, double);
  vtkSetMacro(Tolerance, double);

  // Description:
  // Given two triangles defined by points (p1, q1, r1) and (p2, q2,
  // r2), returns whether the two triangles intersect. If they do,
  // the endpoints of the line forming the intersection are returned
  // in pt1 and pt2. The parameter coplanar is set to 1 if the
  // triangles are coplanar and 0 otherwise.
  static int TriangleTriangleIntersection(double p1[3], double q1[3], double r1[3],
                                          double p2[3], double q2[3], double r2[3],
                                          int &coplanar, double pt1[3], double pt2[3],
					  double surfaceid[2],double tolerance);

  static void CleanAndCheckSurface(vtkPolyData *pd,double stats[2],double tolerance);
  static void CleanAndCheckInput(vtkPolyData *pd,double tolerance);


protected:
  vtkIntersectionPolyDataFilter2();
  ~vtkIntersectionPolyDataFilter2();

  int RequestData(vtkInformation*, vtkInformationVector**, vtkInformationVector*);
  int FillInputPortInformation(int, vtkInformation*);

private:
  vtkIntersectionPolyDataFilter2(const vtkIntersectionPolyDataFilter2&); // Not implemented
  void operator=(const vtkIntersectionPolyDataFilter2&); // Not implemented

  int NumberOfIntersectionPoints;
  int NumberOfIntersectionLines;
  int SplitFirstOutput;
  int SplitSecondOutput;
  int ApplyBoundaryPointArray;
  int CheckMesh;
  int CheckInput;
  int Status;
  double Tolerance;

  class Impl;
};


#endif // __vtkIntersectionPolyDataFilter2_h
