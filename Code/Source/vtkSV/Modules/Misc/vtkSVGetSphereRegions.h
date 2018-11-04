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
 *  \class vtkSVGetSphereRegions
 *  \brief This filter is intended purely for geometries with a branching
 *  structure with a cell data array defining the different branches of the
 *  geometry. It will find the boundary between these regions, find a centroid
 *  of the branching region, create a sphere around the branching region,
 *  and set the value to 1 in an output data array within this sphere region.
 *  CellArrayName holds the array that indicates whether a cell is in the sphere region..
 *  Points defining a boundary should be indicated with the PointArrayName. This
 *  can be easily found using the vtkSVFindSeparateRegionsFilter.
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVGetSphereRegions_h
#define vtkSVGetSphereRegions_h

#include "vtkSVMiscModule.h" // For export

#include "vtkPolyDataAlgorithm.h"

class VTKSVMISC_EXPORT vtkSVGetSphereRegions : public vtkPolyDataAlgorithm
{
public:
  static vtkSVGetSphereRegions* New();
  vtkTypeMacro(vtkSVGetSphereRegions, vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  /// \brief Array name defining the separate branches.
  vtkGetStringMacro(CellArrayName);
  vtkSetStringMacro(CellArrayName);
  //@}
  //@{
  /// \brief Array name defining points that are the boundary between CellArrayName values.
  /// Use vtkSVFindSeparateRegions to get these points.
  vtkGetStringMacro(PointArrayName);
  vtkSetStringMacro(PointArrayName);
  //@{
  /// \brief Array name given to points within sphere of bifurcating region.
  vtkGetStringMacro(OutCellArrayName);
  vtkSetStringMacro(OutCellArrayName);
  //@}

  //@{
  /// \brief Set/Get the sphere radius. Same radius used for all boundaries.
  vtkGetMacro(SphereRadius, double);
  vtkSetMacro(SphereRadius, double);
  //@}

protected:
  vtkSVGetSphereRegions();
  ~vtkSVGetSphereRegions();

  // Usual data generation method
  int RequestData(vtkInformation *vtkNotUsed(request),
		  vtkInformationVector **inputVector,
		  vtkInformationVector *outputVector) override;

  /** \brief Sets the arrays using the given CellArrayName and PointArrayName. */
  int GetArrays(vtkPolyData *object,int type);

  /** \brief Finds the loops of the boundary points. */
  int GetClosedEdgeLoops(vtkPolyData *pd, vtkPolyData *linepd,int *numLoops);

  /** \brief Sets the sphere regions at each boundary loop. */
  int SetSphereRegions(vtkPolyData *pd, vtkPolyData *linepd,int numLoops);

  vtkIntArray *CellArray;
  vtkIntArray *PointArray;

  char* CellArrayName;
  char* PointArrayName;
  char* OutCellArrayName;

  double SphereRadius;

private:
  vtkSVGetSphereRegions(const vtkSVGetSphereRegions&);  // Not implemented.
  void operator=(const vtkSVGetSphereRegions&);  // Not implemented.
};

#endif
