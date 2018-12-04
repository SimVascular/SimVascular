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
  * \class vtkSVSurfaceVectors - Constrains vectors to surface.
  * \section Description
  * This filter works on point vectors.  It does not work on cell vectors yet.
  * A normal is conputed for a point by averaging normals of surrounding
  * 2D cells.  The vector is then constrained to be perpendicular to the normal.
  */

#ifndef vtkSVSurfaceVectors_h
#define vtkSVSurfaceVectors_h

#include "vtkSVMiscModule.h" // For exports

#include "vtkDataSetAlgorithm.h"
#include "vtkFloatArray.h"
#include "vtkIdList.h"

class VTKSVMISC_EXPORT vtkSVSurfaceVectors : public vtkDataSetAlgorithm
{
public:
  vtkTypeMacro(vtkSVSurfaceVectors,vtkDataSetAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;
  static vtkSVSurfaceVectors *New();

//BTX
  enum ConstraintMode {
    Parallel = 0,
    Perpendicular,
    PerpendicularScale
  };
//ETX

  //@{
  /// \brief This mode determines whether this filter projects vectors to be
  /// perpendicular to surface or parallel to surface.
  /// It defaults to parallel.
  vtkSetMacro(ConstraintMode,int);
  vtkGetMacro(ConstraintMode,int);
  void SetConstraintModeToParallel()
    {this->SetConstraintMode(vtkSVSurfaceVectors::Parallel);}
  void SetConstraintModeToPerpendicular()
    {this->SetConstraintMode(vtkSVSurfaceVectors::Perpendicular);}
  void SetConstraintModeToPerpendicularScale()
    {this->SetConstraintMode(vtkSVSurfaceVectors::PerpendicularScale);}
  //@}

protected:
  vtkSVSurfaceVectors();
  ~vtkSVSurfaceVectors();

  // Usual data generation method
  virtual int RequestData(vtkInformation *,
                          vtkInformationVector **,
                          vtkInformationVector *) override;
  virtual int RequestUpdateExtent(vtkInformation*,
                                  vtkInformationVector**,
                                  vtkInformationVector*) override;

  int   ConstraintMode;

private:
  vtkSVSurfaceVectors(const vtkSVSurfaceVectors&);  // Not implemented.
  void operator=(const vtkSVSurfaceVectors&);  // Not implemented.
};

#endif
