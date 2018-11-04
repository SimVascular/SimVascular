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
 *  \class vtkSVPolyDataEdgeSplitter
 *  \brief This filter performs edge splits around given points. The purpose
 *  of this filter is to increase the valence of a point. It can create worse quality
 *  triangles as the valence can then be above the ideal size connected cells. This
 *  can be useful in clustering algorithms where multiple regions need to meet at
 *  a single point.
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVPolyDataEdgeSplitter_h
#define vtkSVPolyDataEdgeSplitter_h

#include "vtkSVMiscModule.h" // For export

#include "vtkIdList.h"
#include "vtkPolyData.h"
#include "vtkPolyDataAlgorithm.h"

#include "vtkSVGlobals.h"

class VTKSVMISC_EXPORT vtkSVPolyDataEdgeSplitter : public vtkPolyDataAlgorithm
{
public:
  vtkTypeMacro(vtkSVPolyDataEdgeSplitter,vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  static vtkSVPolyDataEdgeSplitter *New();

  //@{
  /// \brief Get/Set macro for merged centerlines
  vtkSetObjectMacro(SplitPointIds,vtkIdList);
  vtkGetObjectMacro(SplitPointIds,vtkIdList);
  //@}

  //@{
  /// \brief Get/Set macro for array name used by the filter. If this is given,
  /// this array must be present on the surface as point data with -1 indicating
  /// that it should not be split and everything else indicating it should be split
  vtkSetStringMacro(SplitPointsArrayName);
  vtkGetStringMacro(SplitPointsArrayName);
  //@}

protected:
  vtkSVPolyDataEdgeSplitter();
  ~vtkSVPolyDataEdgeSplitter();

  // Usual data generation method
  virtual int RequestData(vtkInformation *,
                          vtkInformationVector **,
                          vtkInformationVector *) override;

  int PrepFilter(); // Prep work.
  int RunFilter(); // Run filter operations.

  /** \brief Internal function to split cells around one specific point */
  int SplitCellsAroundPoint(vtkPolyData *pd, int ptId);
  /** \brief Internal function to split a single edge */
  int SplitEdge(vtkPolyData *pd, int cellId, int ptId0, int ptId1);

  char *SplitPointsArrayName;

  vtkPolyData *WorkPd;

  vtkIdList *SplitPointIds;

  vtkCellArray *NewCells;

  vtkPoints *NewPoints;

  int SplitPointsArrayAdded;
  std::vector<int> CellBool;
  std::vector<std::vector<int> > SplitCellsInfo;

private:
  vtkSVPolyDataEdgeSplitter(const vtkSVPolyDataEdgeSplitter&);  // Not implemented.
  void operator=(const vtkSVPolyDataEdgeSplitter&);  // Not implemented.
};

#endif
