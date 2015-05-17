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

/** @file vtkFillHolesFilterWithIds.h
 *  @brief This is a filter to be able to apply ids to the surfaces that 
 *  @brief fill the holes  
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 */

#ifndef vtkFillHolesFilterWithIds_h
#define vtkFillHolesFilterWithIds_h

#include "vtkFiltersModelingModule.h" // For export macro
#include "vtkPolyDataAlgorithm.h"

class vtkAbstractTransform;

class VTKFILTERSMODELING_EXPORT vtkFillHolesFilterWithIds : public vtkPolyDataAlgorithm
{
public:
  // Description:
  // Standard methods for instantiation, type information and printing.
  static vtkFillHolesFilterWithIds *New();
  vtkTypeMacro(vtkFillHolesFilterWithIds,vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent);

  // Description:
  // Specify the maximum hole size to fill. This is represented as a radius
  // to the bounding circumsphere containing the hole.  Note that this is an
  // approximate area; the actual area cannot be computed without first
  // triangulating the hole.
  vtkSetClampMacro(HoleSize, double, 0.0, VTK_FLOAT_MAX);
  vtkGetMacro(HoleSize, double);

  // Description:
  // Integer describing the number of holes filled
  vtkGetMacro(NumberOfHolesFilled, int);

  // Description:
  // Integer describing the number of holes filled
  vtkGetMacro(FillId, int);
  vtkSetMacro(FillId, int);

  vtkGetMacro(FillType, int);
  vtkSetMacro(FillType, int);

  enum CapFillType
  {
    VTK_NUM_FILLED=0,
    VTK_FILL_ID,
    VTK_INCREASING_START
  };
protected:
  vtkFillHolesFilterWithIds();
  ~vtkFillHolesFilterWithIds();

  int RequestData(vtkInformation *, vtkInformationVector **, vtkInformationVector *);

  int FillType;
  double HoleSize;
  int FillId;
  int NumberOfHolesFilled;

  vtkIntArray *capIdArray;

private:
  vtkFillHolesFilterWithIds(const vtkFillHolesFilterWithIds&);  // Not implemented.
  void operator=(const vtkFillHolesFilterWithIds&);  // Not implemented.
};

#endif
