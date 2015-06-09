/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
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

/** @file vtkLocalLinearSubdivisionFilter.h
 *  @brief This is localized subdivision methods specifically for linear
 *  subdivision
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 */

#ifndef vtkLocalLinearSubdivisionFilter_h
#define vtkLocalLinearSubdivisionFilter_h

#include "vtkFiltersModelingModule.h" // For export macro
#include "vtkLocalInterpolatingSubdivisionFilter.h"

class vtkIntArray;
class vtkPointData;
class vtkPoints;
class vtkPolyData;

class VTKFILTERSMODELING_EXPORT vtkLocalLinearSubdivisionFilter : public vtkLocalInterpolatingSubdivisionFilter
{
public:
  // Description:
  // Construct object with NumberOfSubdivisions set to 1.
  static vtkLocalLinearSubdivisionFilter *New();
  vtkTypeMacro(vtkLocalLinearSubdivisionFilter,vtkLocalInterpolatingSubdivisionFilter);

protected:
  vtkLocalLinearSubdivisionFilter () {}
  ~vtkLocalLinearSubdivisionFilter () {}

  int GenerateSubdivisionPoints (vtkPolyData *inputDS,
                                 vtkIntArray *edgeData,
                                 vtkPoints *outputPts,
                                 vtkPointData *outputPD);

  int SetFixedCells(vtkPolyData *pd,int *noSubdivideCell);

private:
  vtkLocalLinearSubdivisionFilter(const vtkLocalLinearSubdivisionFilter&);  // Not implemented.
  void operator=(const vtkLocalLinearSubdivisionFilter&);  // Not implemented.
};

#endif


// VTK-HeaderTest-Exclude: vtkLocalLinearSubdivisionFilter.h
