/*=========================================================================
 *
 * Copyright (c) 2015 The Regents of the University of California.
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

/** @file vtkBlendPolyData.h
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 */

#ifndef __vtkBlendPolyData_h
#define __vtkBlendPolyData_h

#include "vtkFiltersCoreModule.h" // For export macro
#include "vtkPolyDataAlgorithm.h"

class VTKFILTERSCORE_EXPORT vtkBlendPolyData : public vtkPolyDataAlgorithm
{
public:
  static vtkBlendPolyData* New();
  vtkTypeRevisionMacro(vtkBlendPolyData, vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent);

  // Description:
  // Specify the feature angle for extracting feature edges.
  //vtkSetClampMacro(FeatureAngle,double,0.0,180.0);
  vtkGetStringMacro(CellArrayName);
  vtkSetStringMacro(CellArrayName);
  vtkGetStringMacro(PointArrayName);
  vtkSetStringMacro(PointArrayName);

  // Description:
  // Set number of blend iterations, default = 2.
  vtkSetMacro(NumBlendIterations, int);
  vtkGetMacro(NumBlendIterations, int);

  // Description:
  // Set number of iterations within the blending procedure. A blend consists
  // of a subdivide and then iterations of smoothing and decimation. 1 sub
  // blend iterations would consist of one smooth and one decimation.
  // default = 4
  vtkSetMacro(NumSubBlendIterations, int);
  vtkGetMacro(NumSubBlendIterations, int);

  // Description:
  // Set number of b iterations, default = 1.
  vtkSetMacro(NumSubdivisionIterations, int);
  vtkGetMacro(NumSubdivisionIterations, int);

  // Description:
  // Set number of smooth iterations, default = 100
  vtkSetMacro(NumSmoothIterations, int);
  vtkGetMacro(NumSmoothIterations, int);

  // Description:
  // Set number for smoothing relaxation, default = 0.01
  vtkSetMacro(SmoothRelaxationFactor, double);
  vtkGetMacro(SmoothRelaxationFactor, double);

  // Description:
  // Set target decimation reduction rate, default = 0.25
  vtkSetMacro(DecimationTargetReduction, double);
  vtkGetMacro(DecimationTargetReduction, double);

  // Description:
  // Turn on/off the use of point array for constraint local operation.
  // If value in array equals 1, nodes will be smoothed
  vtkSetMacro(UsePointArray,int);
  vtkGetMacro(UsePointArray,int);
  vtkBooleanMacro(UsePointArray,int);

  // Description:
  // Turn on/off the use of cell array for constraint on local operation.
  // If value in array equals 1, nodes of cell will be smoothed
  vtkSetMacro(UseCellArray,int);
  vtkGetMacro(UseCellArray,int);
  vtkBooleanMacro(UseCellArray,int);

protected:
  vtkBlendPolyData();
  ~vtkBlendPolyData();

  // Usual data generation method
  int RequestData(vtkInformation *vtkNotUsed(request),
		  vtkInformationVector **inputVector,
		  vtkInformationVector *outputVector);

  vtkIntArray *CellArray;
  vtkIntArray *PointArray;

  char* CellArrayName;
  char* PointArrayName;
  int UseCellArray;
  int UsePointArray;

  int NumBlendIterations;
  int NumSmoothIterations;
  int NumSubdivisionIterations;
  int NumSubBlendIterations;
  double SmoothRelaxationFactor;
  double DecimationTargetReduction;

private:
  vtkBlendPolyData(const vtkBlendPolyData&);  // Not implemented.
  void operator=(const vtkBlendPolyData&);  // Not implemented.
};

#endif


