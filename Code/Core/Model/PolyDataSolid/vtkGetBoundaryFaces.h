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

// .NAME vtkGetBoundaryFaces - Get Boundary Faces from poldata and label them with integers
// .SECTION Description
// vtkGetBoundaryFaces is a filter to extract the boundary surfaces of a model, separate the surace into multiple regions and number each region. 

// .SECTION Caveats
// To see the coloring of the lines you may have to set the ScalarMode
// instance variable of the mapper to SetScalarModeToUseCellData(). (This
// is only a problem if there are point data scalars.)

// .SECTION See Also
// vtkExtractEdges

/** @file vtkGetBoundaryFaces.h
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

#ifndef __vtkGetBoundaryFaces_h
#define __vtkGetBoundaryFaces_h

#include "vtkFiltersCoreModule.h" // For export macro
#include "vtkPolyDataAlgorithm.h"

class vtkFeatureEdges;

class VTKFILTERSCORE_EXPORT vtkGetBoundaryFaces : public vtkPolyDataAlgorithm
{
public:
  static vtkGetBoundaryFaces* New();
  vtkTypeRevisionMacro(vtkGetBoundaryFaces, vtkPolyDataAlgorithm);
  void PrintSelf(ostream& os, vtkIndent indent);

  // Description:
  // Specify the feature angle for extracting feature edges.
  //vtkSetClampMacro(FeatureAngle,double,0.0,180.0);
  vtkGetMacro(FeatureAngle,double);
  vtkSetMacro(FeatureAngle,double);

  vtkGetMacro(NumberOfRegions,int);

protected:
  vtkGetBoundaryFaces();
  ~vtkGetBoundaryFaces();

  double FeatureAngle;
  int NumberOfRegions;
  // Usual data generation method
  int RequestData(vtkInformation *vtkNotUsed(request), 
		  vtkInformationVector **inputVector, 
		  vtkInformationVector *outputVector);

  vtkFeatureEdges* boundaries;
  vtkIntArray *newScalars;
  vtkPolyData *mesh;
  vtkPolyData *boundaryLines;
  vtkIntArray *BoundaryPointArray;
  vtkIntArray *BoundaryCellArray;
  vtkIdList *CheckCells;
  vtkIdList *CheckCells2;
  vtkIdList *CheckCellsCareful;
  vtkIdList *CheckCellsCareful2;

  vtkIdType *checked;
  vtkIdType *checkedcarefully;
  vtkIdType *pointMapper;

  void FindBoundaryRegion(int reg,int start);
  void FindBoundaryRegionTipToe(int reg);
  void SetBoundaryArrays();

private:
  vtkGetBoundaryFaces(const vtkGetBoundaryFaces&);  // Not implemented.
  void operator=(const vtkGetBoundaryFaces&);  // Not implemented.
};

#endif


