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

// .NAME vtkGetSphereRegions - Get Boundary Faces from poldata and label them with integers
// .SECTION Description
// vtkGetSphereRegions is a filter to extract the boundary surfaces of a model, separate the surace into multiple regions and number each region. 

// .SECTION Caveats
// To see the coloring of the lines you may have to set the ScalarMode
// instance variable of the mapper to SetScalarModeToUseCellData(). (This
// is only a problem if there are point data scalars.)

// .SECTION See Also
// vtkExtractEdges

/** @file vtkGetSphereRegions.h
 *  @brief This is a vtk filter to extract the boundaries from a vtk. It uses 
 *  the common conventions to be able to load this filter into Paraview and 
 *  use it as a filter.
 *  @details This filter is based off of the vtkFeatureEdges filter which
 *  finds lines and points that are defined as the separation between two 
 *  faces based on the angle difference in the normals between these faces
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 *  @note Most functions in class call functions in cv_polydatasolid_utils.
 */

#ifndef __vtkGetSphereRegions_h
#define __vtkGetSphereRegions_h

#include "vtkFiltersCoreModule.h" // For export macro
#include "vtkPolyDataAlgorithm.h"

class VTKFILTERSCORE_EXPORT vtkGetSphereRegions : public vtkPolyDataAlgorithm
{
public:
  static vtkGetSphereRegions* New();
  vtkTypeRevisionMacro(vtkGetSphereRegions, vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent);

  // Description:
  // Set name for cell array to be used to determine the in between sections
  vtkGetStringMacro(CellArrayName);
  vtkSetStringMacro(CellArrayName);
  vtkGetStringMacro(PointArrayName);
  vtkSetStringMacro(PointArrayName);
  vtkGetStringMacro(OutCellArrayName);
  vtkSetStringMacro(OutCellArrayName);

  vtkGetMacro(SphereRadius, double);
  vtkSetMacro(SphereRadius, double);

protected:
  vtkGetSphereRegions();
  ~vtkGetSphereRegions();

  // Usual data generation method
  int RequestData(vtkInformation *vtkNotUsed(request), 
		  vtkInformationVector **inputVector, 
		  vtkInformationVector *outputVector);

  int GetClosedEdgeLoops(vtkPolyData *pd, vtkPolyData *linepd,int *numLoops);
  int SetSphereRegions(vtkPolyData *pd, vtkPolyData *linepd,int numLoops);

  vtkIntArray *CellArray;
  vtkIntArray *PointArray;

  char* CellArrayName;
  char* PointArrayName;
  char* OutCellArrayName;

  double SphereRadius;

  int GetArrays(vtkPolyData *object,int type);

private:
  vtkGetSphereRegions(const vtkGetSphereRegions&);  // Not implemented.
  void operator=(const vtkGetSphereRegions&);  // Not implemented.
};

#endif


