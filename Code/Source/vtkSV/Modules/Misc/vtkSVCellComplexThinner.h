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

/** \file vtkSVCellComplexThinner.cxx
 *  \brief This is a filter to take a cell complex and perform iterative
 *  \brief thinning on it and return polydatas with cell data indicating the
 *  \brief absolute and relative persistance of each cell during the
 *  \brief iterative procedure
 *  \brief For more details on the method and the cell data see:
 *  \brief Liu, L., et al. "A simple and robust thinning algorithm on cell complexes."
 *  \brief Computer Graphics Forum. Vol. 29. No. 7. Blackwell Publishing Ltd, 2010.
 *  \brief Also, the corresponding edge pd can be retrieved in OutputEdgePd
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVCellComplexThinner_h
#define vtkSVCellComplexThinner_h

#include "vtkPolyDataAlgorithm.h"
#include "vtkSVMiscModule.h" // For export

class VTKSVMISC_EXPORT vtkSVCellComplexThinner : public vtkPolyDataAlgorithm
{
public:
  static vtkSVCellComplexThinner *New();
  vtkTypeMacro(vtkSVCellComplexThinner,vtkPolyDataAlgorithm);

  //@{
  /// \brief Get/set name for array on edge pd if preserving
  vtkGetStringMacro(PreserveEdgeCellsArrayName);
  vtkSetStringMacro(PreserveEdgeCellsArrayName);
  //@}

  //@{
  /// \brief Get/set name for edge pds
  vtkSetObjectMacro(InputEdgePd, vtkPolyData);
  vtkGetObjectMacro(InputEdgePd, vtkPolyData);

  vtkSetObjectMacro(OutputEdgePd, vtkPolyData);
  vtkGetObjectMacro(OutputEdgePd, vtkPolyData);
  //@}

protected:

  vtkSVCellComplexThinner();
  ~vtkSVCellComplexThinner();

  virtual int RequestData(vtkInformation *,
                          vtkInformationVector **,
                          vtkInformationVector *) override;

private:

  vtkPolyData *InputEdgePd;
  vtkPolyData *OutputEdgePd;

  vtkPolyData *WorkTriPd;
  vtkPolyData *WorkEdgePd;

  char *PreserveEdgeCellsArrayName;

  int PrepFilter();
  int RunFilter();

  vtkSVCellComplexThinner(const vtkSVCellComplexThinner&);  // Not implemented.
  void operator=(const vtkSVCellComplexThinner&);  // Not implemented.
};

#endif
