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
  * \class vtkSVIntegrateFlowThroughSurface - Integrates vector dot normal.
  * \brief First this filter finds point normals for a surface.  It
  * Takes a point vector field from the input and computes the
  * dot product with the normal.  It then integrates this dot value
  * to get net flow through the surface.
  */

#ifndef vtkSVIntegrateFlowThroughSurface_h
#define vtkSVIntegrateFlowThroughSurface_h

#include "vtkSVMiscModule.h" // for exports

#include "vtkIdList.h"
#include "vtkDataSetAttributes.h"
#include "vtkUnstructuredGridAlgorithm.h"

class VTKSVMISC_EXPORT vtkSVIntegrateFlowThroughSurface : public vtkUnstructuredGridAlgorithm
{
public:
  vtkTypeMacro(vtkSVIntegrateFlowThroughSurface,vtkUnstructuredGridAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;
  static vtkSVIntegrateFlowThroughSurface *New();

protected:
  vtkSVIntegrateFlowThroughSurface();
  ~vtkSVIntegrateFlowThroughSurface();

  // Usual data generation method
  // Usual data generation method
  virtual int RequestData(vtkInformation *,
                          vtkInformationVector **,
                          vtkInformationVector *) override;
  virtual int RequestUpdateExtent(vtkInformation*,
                                  vtkInformationVector**,
                                  vtkInformationVector*) override;

  virtual int FillInputPortInformation(int port, vtkInformation* info) override;

  // Create a default executive.
  virtual vtkExecutive* CreateDefaultExecutive() override;

  vtkDataSet* GenerateSurfaceVectors(vtkDataSet* input);

private:
  vtkSVIntegrateFlowThroughSurface(const vtkSVIntegrateFlowThroughSurface&); // Not implemented.
  void operator=(const vtkSVIntegrateFlowThroughSurface&);  // Not implemented.
};

#endif
