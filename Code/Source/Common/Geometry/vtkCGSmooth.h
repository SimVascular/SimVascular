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

/** @file vtkCGSmooth.h
 *  @brief Smoothes a surface using a constrained method solving the 
 *  minimization between the original surface and a laplacian smoothed 
 *  surface 
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 *  @note Most functions in class call functions in cv_polydatasolid_utils.
 */

#ifndef __vtkCGSmooth_h
#define __vtkCGSmooth_h

#include "vtkPolyDataAlgorithm.h"
#include <set>

class vtkCGSmooth : public vtkPolyDataAlgorithm
{
public:
  static vtkCGSmooth* New();
  vtkTypeRevisionMacro(vtkCGSmooth, vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent);

  // Description:
  // Set name for cell array to be used to determine the in between sections
  vtkGetStringMacro(CellArrayName);
  vtkSetStringMacro(CellArrayName);
  vtkGetStringMacro(PointArrayName);
  vtkSetStringMacro(PointArrayName);

  vtkGetMacro(Weight,double);
  vtkSetMacro(Weight,double);

  vtkGetMacro(UsePointArray,int);
  vtkSetMacro(UsePointArray,int);
  vtkBooleanMacro(UsePointArray,int);
  vtkGetMacro(UseCellArray,int);
  vtkSetMacro(UseCellArray,int);
  vtkBooleanMacro(UseCellArray,int);

  vtkGetMacro(NumSmoothOperations,int);
  vtkSetMacro(NumSmoothOperations,int);
  vtkGetMacro(NumGradientSolves,int);
  vtkSetMacro(NumGradientSolves,int);

protected:
  vtkCGSmooth();
  ~vtkCGSmooth();

  // Usual data generation method
  int RequestData(vtkInformation *vtkNotUsed(request), 
		  vtkInformationVector **inputVector, 
		  vtkInformationVector *outputVector);

  vtkIntArray *CellArray;
  vtkIntArray *PointArray;

  char* CellArrayName;
  char* PointArrayName;
  int UsePointArray;
  int UseCellArray;

  int GetArrays(vtkPolyData *object,int type);
  void Test();
  int CGSmooth(vtkPolyData *original,vtkPolyData *current);
  int GetAttachedPoints(vtkPolyData *pd, vtkIdType nodeId, std::set<vtkIdType> *attachedPts);
  int SetFixedPoints(vtkPolyData *pd);

  double Weight;
  int NumSmoothOperations;
  int NumGradientSolves;

  int *fixedPt;
  int NumFixedPoints;

private:
  vtkCGSmooth(const vtkCGSmooth&);  // Not implemented.
  void operator=(const vtkCGSmooth&);  // Not implemented.
};

#endif


