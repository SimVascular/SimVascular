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
 * \class vtkSVPickPointsSeedSelector
 *
 * \brief This is a c++ replication of the python code for picking seed points on a surface
 * in vmtk.
 *
 * \author Adam Updegrove
 * \author updega2@gmail.com
 * \author UC Berkeley
 * \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVPickPointSeedSelector_h
#define vtkSVPickPointSeedSelector_h

#include "vtkDataArray.h"
#include "vtkIdList.h"
#include "vtkPolyData.h"
#include "vtkPoints.h"
#include "vtkUnstructuredGrid.h"

#include "vtkSVRenderer.h"
#include "vtkSVSeedSelector.h"

#include "vtkSVMiscModule.h" // For exports

class VTKSVMISC_EXPORT vtkSVPickPointSeedSelector : public vtkSVSeedSelector
{
  public:
  vtkTypeMacro(vtkSVPickPointSeedSelector,vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  static vtkSVPickPointSeedSelector *New();

  void InitializeSeeds();

  static void PickCallback( vtkObject* caller, long unsigned int vtkNotUsed(eventId), void* clientData, void* vtkNotUsed(callData) );

  static void UndoCallback( vtkObject* caller, long unsigned int vtkNotUsed(eventId), void* clientData, void* vtkNotUsed(callData) );

  protected:
  vtkSVPickPointSeedSelector();
  ~vtkSVPickPointSeedSelector();

  virtual int RequestData(vtkInformation *, vtkInformationVector **, vtkInformationVector *) override;

  vtkPolyData *PickedSeeds;

  vtkIdList* PickedSeedIds;

  vtkSVRenderer *SVRenderer;

  vtkCallbackCommand *UndoCallbackCommand;
  vtkCallbackCommand *PickCallbackCommand;

  private:
  vtkSVPickPointSeedSelector(const vtkSVPickPointSeedSelector&);  // Not implemented.
  void operator=(const vtkSVPickPointSeedSelector&);  // Not implemented.
};

#endif
