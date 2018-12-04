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
 *  \class  vtkSVCenterlinesBasedNormals
 *  \brief
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVCenterlinesBasedNormals_h
#define vtkSVCenterlinesBasedNormals_h

#include "vtkSVSegmentationModule.h" // For exports

#include "vtkPolyDataAlgorithm.h"

#include "vtkPolyData.h"

class VTKSVSEGMENTATION_EXPORT vtkSVCenterlinesBasedNormals : public vtkPolyDataAlgorithm
{
public:
  static vtkSVCenterlinesBasedNormals* New();
  vtkTypeMacro(vtkSVCenterlinesBasedNormals,vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  /// \brief Use of cell data
  vtkGetObjectMacro(CenterlinesPd, vtkPolyData);
  vtkSetObjectMacro(CenterlinesPd, vtkPolyData);
  //@}

  //@{
  /// \brief Get/Set macro for array name used by the filter. Must
  //  be present on the input.
  vtkSetStringMacro(PointArrayName);
  vtkGetStringMacro(PointArrayName);
  vtkSetStringMacro(CellArrayName);
  vtkGetStringMacro(CellArrayName);
  vtkSetStringMacro(NewCellArrayName);
  vtkGetStringMacro(NewCellArrayName);
  vtkSetStringMacro(GroupIdsArrayName);
  vtkGetStringMacro(GroupIdsArrayName);
  vtkSetStringMacro(InternalIdsArrayName);
  vtkGetStringMacro(InternalIdsArrayName);
  //@}

  //@{
  /// \brief Use of cell data
  vtkGetMacro(UseCellArray, int);
  vtkSetMacro(UseCellArray, int);
  //@}

  //@{
  /// \brief Use of point data
  vtkGetMacro(UsePointArray, int);
  vtkSetMacro(UsePointArray, int);
  //@}


  const static double GlobalCoords[3][3];

protected:
  vtkSVCenterlinesBasedNormals();
  ~vtkSVCenterlinesBasedNormals();

  // Usual data generation method
  int RequestData(vtkInformation *vtkNotUsed(request),
		  vtkInformationVector **inputVector,
		  vtkInformationVector *outputVector) override;

  int PrepFilter(); // Prep work.
  int RunFilter(); // Run filter operations.

  int ComputeStartVector(const double verts[3][3], double startVec[3]);

  int ComputeLocalCoordinateSystem(const double vz[3], const double vstart[3],
                                   double vx[3], double vy[3]);

  int ComputeRotationMatrix(const double vx[3], const double vy[3], const double vz[3],
                            double rotMatrix[9]);

  int FlipLinePoints(vtkPolyData *pd, const int cellId);

private:
  vtkSVCenterlinesBasedNormals(const vtkSVCenterlinesBasedNormals&);  // Not implemented.
  void operator=(const vtkSVCenterlinesBasedNormals&);  // Not implemented.

  vtkPolyData *WorkPd; // Polydata used during filter processing
  vtkPolyData *CenterlinesPd;

  char *GroupIdsArrayName;
  char *InternalIdsArrayName;
  char *PointArrayName;
  char *CellArrayName;
  char *NewCellArrayName;

  vtkDoubleArray *PointArray;
  vtkDoubleArray *CellArray;
  vtkDoubleArray *NewCellArray;

  int UsePointArray; // Use point info
  int UseCellArray; // Use cell info

};

#endif
