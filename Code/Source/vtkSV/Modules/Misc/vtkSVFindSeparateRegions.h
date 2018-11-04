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
 *  \class vtkSVFindSeparateRegions
 *  \brief This is a filter to locate points that form the boundaries in
 *  between values of a given cell data array
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVFindSeparateRegions_h
#define vtkSVFindSeparateRegions_h

#include "vtkSVMiscModule.h" // For export

#include "vtkIdList.h"
#include "vtkPolyDataAlgorithm.h"

class VTKSVMISC_EXPORT vtkSVFindSeparateRegions : public vtkPolyDataAlgorithm
{
public:
  static vtkSVFindSeparateRegions* New();
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  /// \brief Get/Set macro for array names used by filter
  vtkGetStringMacro(CellArrayName);
  vtkSetStringMacro(CellArrayName);
  vtkGetStringMacro(OutPointArrayName);
  vtkSetStringMacro(OutPointArrayName);
  //@}

  //@{
  /// \brief Set macro for target cell values
  vtkSetObjectMacro(TargetCellIds, vtkIdList);
  //@}

protected:
  vtkSVFindSeparateRegions();
  ~vtkSVFindSeparateRegions();

  // Usual data generation method
  int RequestData(vtkInformation *vtkNotUsed(request),
		  vtkInformationVector **inputVector,
		  vtkInformationVector *outputVector) override;

  int PrepFilter(); // Prep work.
  int RunFilter(); // Run filter operations.

  char* CellArrayName;
  char* OutPointArrayName;

  vtkPolyData *WorkPd;
  vtkIntArray *IntCellScalars;
  vtkIdList *TargetCellIds;

  int GetCellArray(vtkPolyData *object);
  int SetAllCellIds();

private:
  vtkSVFindSeparateRegions(const vtkSVFindSeparateRegions&);  // Not implemented.
  void operator=(const vtkSVFindSeparateRegions&);  // Not implemented.
};

#endif


