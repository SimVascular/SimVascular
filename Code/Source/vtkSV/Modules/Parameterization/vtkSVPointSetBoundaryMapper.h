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
 * \class vtkSVPointSetBoundaryMapper
 *
 * \brief This filter is derived from vtkSVBoundaryMapper to implement a
 * special plane boundary where it is actually set up to define a rectangule;
 * however, the boundary points do not necessarily all have to be corners.
 * That is set by defining the number of divisions in each length of the
 * rectangle.
 *
 * \author Adam Updegrove
 * \author updega2@gmail.com
 * \author UC Berkeley
 * \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVPointSetBoundaryMapper_h
#define vtkSVPointSetBoundaryMapper_h

#include "vtkSVParameterizationModule.h" // For export macro

#include "vtkDoubleArray.h"
#include "vtkIntArray.h"
#include "vtkPointSet.h"

#include "vtkSVBoundaryMapper.h"

class VTKSVPARAMETERIZATION_EXPORT vtkSVPointSetBoundaryMapper : public vtkSVBoundaryMapper
{
public:
  static vtkSVPointSetBoundaryMapper* New();
  vtkTypeMacro(vtkSVPointSetBoundaryMapper,vtkSVBoundaryMapper);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  vtkSetObjectMacro(PointSet, vtkPointSet);
  vtkGetObjectMacro(PointSet, vtkPointSet);

  vtkSetObjectMacro(PointSetBoundaryIds, vtkIntArray);
  vtkGetObjectMacro(PointSetBoundaryIds, vtkIntArray);

protected:
  vtkSVPointSetBoundaryMapper();
  ~vtkSVPointSetBoundaryMapper();

  int SetBoundaries() override; // Need to implement from BoundaryMapper
  int CalculateEdgeLengths(vtkIntArray *actualIds); // Calculate square edge lengths
  int SetBoundary(vtkIntArray *actualIds); // Set the boundary

  vtkPointSet *PointSet;
  vtkDoubleArray *BoundaryLengths;
  vtkIntArray    *PointSetBoundaryIds;

private:
  vtkSVPointSetBoundaryMapper(const vtkSVPointSetBoundaryMapper&);
  void operator=(const vtkSVPointSetBoundaryMapper&);
};

#endif
