/*=========================================================================
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
 * \class vtkSVIdListSeedSelector
 *
 * \brief This is a c++ replication of the python code for an id list seed selector
 * in vmtk. It allows seed points to be chosen by giving an id list.
 *
 * \author Adam Updegrove
 * \author updega2@gmail.com
 * \author UC Berkeley
 * \author shaddenlab.berkeley.edu
 */
#ifndef vtkSVIdListSeedSelector_h
#define vtkSVIdListSeedSelector_h

#include "vtkSVSeedSelector.h"
#include "vtkIdList.h"

#include "vtkSVMiscModule.h" // For exports

#include "vtkUnstructuredGrid.h"

class vtkPolyData;
class vtkPoints;
class vtkIdList;
class vtkDataArray;

class VTKSVMISC_EXPORT vtkSVIdListSeedSelector : public vtkSVSeedSelector
{
  public:
  vtkTypeMacro(vtkSVIdListSeedSelector,vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  vtkSetObjectMacro(SourceIds, vtkIdList);
  vtkGetObjectMacro(SourceIds, vtkIdList);
  vtkSetObjectMacro(TargetIds, vtkIdList);
  vtkGetObjectMacro(TargetIds, vtkIdList);

  static vtkSVIdListSeedSelector *New();

  protected:
  vtkSVIdListSeedSelector();
  ~vtkSVIdListSeedSelector();

  virtual int RequestData(vtkInformation *, vtkInformationVector **, vtkInformationVector *) override;

  vtkIdList* SourceIds;
  vtkIdList* TargetIds;

  private:
  vtkSVIdListSeedSelector(const vtkSVIdListSeedSelector&);  // Not implemented.
  void operator=(const vtkSVIdListSeedSelector&);  // Not implemented.
};

#endif
