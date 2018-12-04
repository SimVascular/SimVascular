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
/** @file vtkSVConstrainedBlend.h
 *  @brief This blends using constrained smoothing, localized decimation,
 *  and localized subdivision
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 *  @note Most functions in class call functions in cv_polydatasolid_utils.
 */

#ifndef vtkSVConstrainedBlend_h
#define vtkSVConstrainedBlend_h

#include "vtkSVGeometryModule.h" // for export

#include "vtkPolyDataAlgorithm.h"
#include <set>

class VTKSVGEOMETRY_EXPORT vtkSVConstrainedBlend : public vtkPolyDataAlgorithm
{
public:
  static vtkSVConstrainedBlend* New();
  vtkTypeMacro(vtkSVConstrainedBlend, vtkPolyDataAlgorithm);
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
  /// \brief Get/Set Number of blending operations. This is essentially
  /// the same as running the filter multiple times.
  vtkGetMacro(NumBlendOperations,int);
  vtkSetMacro(NumBlendOperations,int);
  //@}

  //@{
  /// \brief Get/Set Number of sub blending operations. A sub blend consists of
  /// constrained smoothing, laplacain smoothing, decimation, and subdivision,
  /// in that order and all with their own respective iterations.
  vtkGetMacro(NumSubBlendOperations,int);
  vtkSetMacro(NumSubBlendOperations,int);
  //@}

  //@{
  /// \brief Get/Set the number of constrained smoothings to perform within
  /// each sub blend
  vtkGetMacro(NumConstrainedSmoothOperations,int);
  vtkSetMacro(NumConstrainedSmoothOperations,int);
  //@}

  //@{
  /// \brief Get/Set the number of laplacian smoothings to perform within
  /// each sub blend
  vtkGetMacro(NumLapSmoothOperations,int);
  vtkSetMacro(NumLapSmoothOperations,int);
  //@}

  //@{
  /// \brief Get/Set the number of maximum conjugate gradient iterations used
  /// for the constrained smoothing.
  /// each sub blend
  vtkGetMacro(NumGradientSolves,int);
  vtkSetMacro(NumGradientSolves,int);
  //@}

  //@{
  /// \brief Get/Set the number of subdivision iterations to perform within
  /// each sub blend
  vtkGetMacro(NumSubdivisionIterations,int);
  vtkSetMacro(NumSubdivisionIterations,int);
  //@}

  //@{
  /// \brief Get/Set the target reduction in the decimation filter
  /// each sub blend
  vtkGetMacro(DecimationTargetReduction,double);
  vtkSetMacro(DecimationTargetReduction,double);
  //@}

protected:
  vtkSVConstrainedBlend();
  ~vtkSVConstrainedBlend();

  // Usual data generation method
  int RequestData(vtkInformation *vtkNotUsed(request),
		  vtkInformationVector **inputVector,
		  vtkInformationVector *outputVector) override;

  vtkIntArray *CellArray;
  vtkIntArray *PointArray;

  char* CellArrayName;
  char* PointArrayName;

  /// \brief Set the PointArray and CellArray
  int GetArrays(vtkPolyData *object,int type);

  /// \brief Run decimation filter
  int Decimate(vtkPolyData *pd);

  /// \brief Run subdivision filter
  int Subdivide(vtkPolyData *pd);

  /// \brief Run laplcain smooth filter
  int LaplacianSmooth(vtkPolyData *pd);

  /// \brief Run constrained smooth filter
  int ConstrainedSmooth(vtkPolyData *pd);

  int UsePointArray;
  int UseCellArray;
  int NumBlendOperations;
  int NumSubBlendOperations;
  int NumConstrainedSmoothOperations;
  int NumLapSmoothOperations;
  int NumGradientSolves;
  int NumSubdivisionIterations;

  double Weight;
  double RelaxationFactor;
  double DecimationTargetReduction;

private:
  vtkSVConstrainedBlend(const vtkSVConstrainedBlend&);  // Not implemented.
  void operator=(const vtkSVConstrainedBlend&);  // Not implemented.
};

#endif


