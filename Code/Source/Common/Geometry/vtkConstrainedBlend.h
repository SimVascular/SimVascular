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

/** @file vtkConstrainedBlend.h
 *  @brief This blends using constrained smoothing, localized decimation,
 *  and localized subdivision
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 *  @note Most functions in class call functions in cv_polydatasolid_utils.
 */

#ifndef __vtkConstrainedBlend_h
#define __vtkConstrainedBlend_h

#include "SimVascular.h"

#include "vtkPolyDataAlgorithm.h"
#include <set>

class SV_EXPORT_SYSGEOM vtkConstrainedBlend : public vtkPolyDataAlgorithm
{
public:
  static vtkConstrainedBlend* New();
  vtkTypeRevisionMacro(vtkConstrainedBlend, vtkPolyDataAlgorithm);
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

  vtkGetMacro(NumBlendOperations,int);
  vtkSetMacro(NumBlendOperations,int);
  vtkGetMacro(NumSubBlendOperations,int);
  vtkSetMacro(NumSubBlendOperations,int);
  vtkGetMacro(NumCGSmoothOperations,int);
  vtkSetMacro(NumCGSmoothOperations,int);
  vtkGetMacro(NumLapSmoothOperations,int);
  vtkSetMacro(NumLapSmoothOperations,int);
  vtkGetMacro(NumGradientSolves,int);
  vtkSetMacro(NumGradientSolves,int);
  vtkGetMacro(NumSubdivisionIterations,int);
  vtkSetMacro(NumSubdivisionIterations,int);

  vtkGetMacro(DecimationTargetReduction,double);
  vtkSetMacro(DecimationTargetReduction,double);

protected:
  vtkConstrainedBlend();
  ~vtkConstrainedBlend();

  // Usual data generation method
  int RequestData(vtkInformation *vtkNotUsed(request),
		  vtkInformationVector **inputVector,
		  vtkInformationVector *outputVector);

  vtkIntArray *CellArray;
  vtkIntArray *PointArray;

  char* CellArrayName;
  char* PointArrayName;

  int GetArrays(vtkPolyData *object,int type);
  int Decimate(vtkPolyData *pd);
  int Subdivide(vtkPolyData *pd);
  int LaplacianSmooth(vtkPolyData *pd);
  int CGSmooth(vtkPolyData *pd);

  int UsePointArray;
  int UseCellArray;
  int NumBlendOperations;
  int NumSubBlendOperations;
  int NumCGSmoothOperations;
  int NumLapSmoothOperations;
  int NumGradientSolves;
  int NumSubdivisionIterations;

  double Weight;
  double RelaxationFactor;
  double DecimationTargetReduction;

private:
  vtkConstrainedBlend(const vtkConstrainedBlend&);  // Not implemented.
  void operator=(const vtkConstrainedBlend&);  // Not implemented.
};

#endif


