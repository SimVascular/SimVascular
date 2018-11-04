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

/** \file vtkSVConstrainedSmoothing.h
 *  \brief Smoothes a surface using a constrained method solving the
 *  minimization between the original surface and a laplacian smoothed
 *  surface
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 *  \note Most functions in class call functions in cv_polydatasolid_utils.
 */

#ifndef vtkSVConstrainedSmoothing_h
#define vtkSVConstrainedSmoothing_h

#include "vtkSVGeometryModule.h" // for export

#include "vtkPolyDataAlgorithm.h"
#include <set>

class VTKSVGEOMETRY_EXPORT vtkSVConstrainedSmoothing : public vtkPolyDataAlgorithm
{
public:
  static vtkSVConstrainedSmoothing* New();
  vtkTypeMacro(vtkSVConstrainedSmoothing, vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  /// \brief Set name for cell array or point array to use for blending
  vtkGetStringMacro(CellArrayName);
  vtkSetStringMacro(CellArrayName);
  vtkGetStringMacro(PointArrayName);
  vtkSetStringMacro(PointArrayName);
  //@}

  //@{
  /// \brief Get/Set weight for constrained smoothing. Default 0.2.
  vtkGetMacro(Weight,double);
  vtkSetMacro(Weight,double);
  //@}

  //@{
  /// \brief Indicate whether point or cell arrays should be used.
  vtkGetMacro(UsePointArray,int);
  vtkSetMacro(UsePointArray,int);
  vtkBooleanMacro(UsePointArray,int);
  vtkGetMacro(UseCellArray,int);
  vtkSetMacro(UseCellArray,int);
  vtkBooleanMacro(UseCellArray,int);
  //@}

  //@{
  /// \brief Get/Set the number of constrained smoothing operations. Set to
  /// 1, a normal laplacian smooth is performed. More than 1, and the weight
  /// kicks in and will attempt to retain the original surface.
  vtkGetMacro(NumSmoothOperations,int);
  vtkSetMacro(NumSmoothOperations,int);
  //@}

  //@{
  /// \brief Get/Set the number of maximum conjugate gradient iterations used
  /// for the constrained smoothing.
  /// each sub blend
  vtkGetMacro(NumGradientSolves,int);
  vtkSetMacro(NumGradientSolves,int);
  //@}

protected:
  vtkSVConstrainedSmoothing();
  ~vtkSVConstrainedSmoothing();

  // Usual data generation method
  int RequestData(vtkInformation *vtkNotUsed(request),
		  vtkInformationVector **inputVector,
		  vtkInformationVector *outputVector) override;

  vtkIntArray *CellArray;
  vtkIntArray *PointArray;

  char* CellArrayName;
  char* PointArrayName;
  int UsePointArray;
  int UseCellArray;

  int GetArrays(vtkPolyData *object,int type);
  int ConstainedSmooth(vtkPolyData *original,vtkPolyData *current);
  int GetAttachedPoints(vtkPolyData *pd, vtkIdType nodeId, std::set<vtkIdType> *attachedPts);
  int SetFixedPoints(vtkPolyData *pd);

  double Weight;
  int NumSmoothOperations;
  int NumGradientSolves;

  int *fixedPt;
  int NumFixedPoints;

private:
  vtkSVConstrainedSmoothing(const vtkSVConstrainedSmoothing&);  // Not implemented.
  void operator=(const vtkSVConstrainedSmoothing&);  // Not implemented.
};

#endif


