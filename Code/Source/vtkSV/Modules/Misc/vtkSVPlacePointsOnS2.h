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
 *  \class vtkSVPlacePointsOnS2
 *  \brief Place points of an arbitrary surface on the unit sphere by
 *  translating, rotating, scaling, and projecting the points.
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVPlacePointsOnS2_h
#define vtkSVPlacePointsOnS2_h

#include "vtkSVMiscModule.h" // For export

#include "vtkPolyData.h"
#include "vtkPolyDataAlgorithm.h"

class VTKSVMISC_EXPORT vtkSVPlacePointsOnS2 : public vtkPolyDataAlgorithm
{
public:
  static vtkSVPlacePointsOnS2* New();
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  // \brief Macro to set/get the axis that the object aligns with in order to orient
  // the object with a unit cube.
  vtkSetVector3Macro(ZAxis, double);
  vtkGetVector3Macro(ZAxis, double);
  vtkSetVector3Macro(XAxis, double);
  vtkGetVector3Macro(XAxis, double);
  //@}

  //@{
  // \brief Specify whether to use custom axis alignment.
  vtkSetMacro(UseCustomAxisAlign, int);
  vtkGetMacro(UseCustomAxisAlign, int);
  vtkBooleanMacro(UseCustomAxisAlign, int);
  //@}

protected:
  vtkSVPlacePointsOnS2();
  ~vtkSVPlacePointsOnS2();

  // Usual data generation method
  int RequestData(vtkInformation *vtkNotUsed(request),
		  vtkInformationVector **inputVector,
		  vtkInformationVector *outputVector) override;

  int RunFilter(); // Run filter operations.
  int MoveToOrigin();
  int RotateToCubeCenterAxis();
  int ScaleToUnitCube();
  int DumbMapToSphere();
  int TextureMap();
  int ConvertTextureFieldToPolyData();

private:
  vtkSVPlacePointsOnS2(const vtkSVPlacePointsOnS2&);  // Not implemented.
  void operator=(const vtkSVPlacePointsOnS2&);  // Not implemented.

  vtkPolyData *InitialPd;
  vtkPolyData *WorkPd;

  int UseCustomAxisAlign;
  double ZAxis[3];
  double XAxis[3];
};

#endif
