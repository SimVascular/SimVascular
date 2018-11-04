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
 *  \class vtkSVSmoothVolume
 *  \brief This class uses vtkDijkstraGraphGeodesicPath to get path betwen points
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVSmoothVolume_h
#define vtkSVSmoothVolume_h

#include "vtkSVGeometryModule.h" // For export

#include "vtkIdList.h"
#include "vtkPolyData.h"
#include "vtkUnstructuredGridAlgorithm.h"

class VTKSVGEOMETRY_EXPORT vtkSVSmoothVolume : public vtkUnstructuredGridAlgorithm
{
public:
  static vtkSVSmoothVolume* New();
  vtkTypeMacro(vtkSVSmoothVolume,vtkUnstructuredGridAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  /// \brief Set the number of smooth iterations. Default 1000
  vtkGetMacro(NumberOfSmoothIterations, int);
  vtkSetMacro(NumberOfSmoothIterations, int);
  //@}

protected:
  vtkSVSmoothVolume();
  ~vtkSVSmoothVolume();

  // Usual data generation method
  int RequestData(vtkInformation *vtkNotUsed(request),
		  vtkInformationVector **inputVector,
		  vtkInformationVector *outputVector) override;

  int PrepFilter(); // Prep work.
  int RunFilter(); // Run filter operations.

  int SmoothHexMesh();
  int SmoothTetMesh();

  vtkUnstructuredGrid *WorkUg;
  int NumberOfSmoothIterations;

private:
  vtkSVSmoothVolume(const vtkSVSmoothVolume&);  // Not implemented.
  void operator=(const vtkSVSmoothVolume&);  // Not implemented.

};

#endif
