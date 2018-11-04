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
 *  \class vtkSVCenterlineParallelTransportVectors
 *  \brief Travers a centerline structure and define a parallel transport frame
 *  along them. Uses the bifurcation points to orient the parallel transport frame
 *  and updates the vectors to ensure that the vectors match at bifurcation
 *  points. This provides a complete parallel transport frame that is consistent
 *  throughout
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVCenterlineParallelTransportVectors_h
#define vtkSVCenterlineParallelTransportVectors_h

#include "vtkSVSegmentationModule.h" // For export

#include "vtkPolyData.h"
#include "vtkPolyDataAlgorithm.h"

#include "vtkSVCenterlineGraph.h"
#include "vtkSVGlobals.h"

class VTKSVSEGMENTATION_EXPORT vtkSVCenterlineParallelTransportVectors : public vtkPolyDataAlgorithm
{
public:
  vtkTypeMacro(vtkSVCenterlineParallelTransportVectors,vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  static vtkSVCenterlineParallelTransportVectors *New();

  //@{
  /// \brief Get/Set macro for array name used by the filter. Must
  //  be present on the centerlines.
  vtkSetStringMacro(GroupIdsArrayName);
  vtkGetStringMacro(GroupIdsArrayName);
  vtkSetStringMacro(ParallelTransportVectorArrayName);
  vtkGetStringMacro(ParallelTransportVectorArrayName);
  //@}


protected:
  vtkSVCenterlineParallelTransportVectors();
  ~vtkSVCenterlineParallelTransportVectors();

  // Usual data generation method
  virtual int RequestData(vtkInformation *,
                          vtkInformationVector **,
                          vtkInformationVector *) override;

  int PrepFilter(); // Prep work.
  int RunFilter(); // Run filter operations.

  char *GroupIdsArrayName;
  char *ParallelTransportVectorArrayName;

  vtkPolyData *WorkPd;

  vtkSVCenterlineGraph *CenterlineGraph;

private:
  vtkSVCenterlineParallelTransportVectors(const vtkSVCenterlineParallelTransportVectors&);  // Not implemented.
  void operator=(const vtkSVCenterlineParallelTransportVectors&);  // Not implemented.
};

#endif
