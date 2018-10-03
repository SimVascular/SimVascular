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
 *  \class vtkSVFillHolesWithIdsFilter
 *  \brief This is a filter to be able to apply ids to the surfaces that
 *  fill the holes
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVFillHolesWithIdsFilter_h
#define vtkSVFillHolesWithIdsFilter_h

#include "vtkSVMiscModule.h" // For export

#include "vtkPolyDataAlgorithm.h"

class vtkAbstractTransform;

class VTKSVMISC_EXPORT vtkSVFillHolesWithIdsFilter : public vtkPolyDataAlgorithm
{
public:
  // Standard methods for instantiation, type information and printing.
  static vtkSVFillHolesWithIdsFilter *New();
  vtkTypeMacro(vtkSVFillHolesWithIdsFilter,vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  /// \brief Specify the maximum hole size to fill. This is represented as a radius
  /// to the bounding circumsphere containing the hole.  Note that this is an
  /// approximate area; the actual area cannot be computed without first
  /// triangulating the hole.
  vtkSetClampMacro(HoleSize, double, 0.0, VTK_FLOAT_MAX);
  vtkGetMacro(HoleSize, double);
  //@}

  /// \brief Integer describing the number of holes filled
  vtkGetMacro(NumberOfHolesFilled, int);

  //@{
  /// \brief If CapFillType set to VTK_FILL_ID, this value is used
  /// to set the value at all caps. Default is -1.
  vtkGetMacro(FillId, int);
  vtkSetMacro(FillId, int);
  //@}

  //@{
  /// \brief Set the cap fill type, 0 - number filled, 1 - fillid.
  vtkGetMacro(FillType, int);
  vtkSetMacro(FillType, int);
  //@}

  /// \brief FillType VTK_NUM_FILLED and VTK_FILL_ID implemented.
  enum CapFillType
  {
    VTK_NUM_FILLED=0,
    VTK_FILL_ID,
    VTK_INCREASING_START
  };
protected:
  vtkSVFillHolesWithIdsFilter();
  ~vtkSVFillHolesWithIdsFilter();

  int RequestData(vtkInformation *, vtkInformationVector **, vtkInformationVector *) override;

  int FillType;
  double HoleSize;
  int FillId;
  int NumberOfHolesFilled;

  vtkIntArray *capIdArray;

private:
  vtkSVFillHolesWithIdsFilter(const vtkSVFillHolesWithIdsFilter&);  // Not implemented.
  void operator=(const vtkSVFillHolesWithIdsFilter&);  // Not implemented.
};

#endif
