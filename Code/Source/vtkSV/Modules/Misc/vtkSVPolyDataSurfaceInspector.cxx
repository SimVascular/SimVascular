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

#include "vtkSVPolyDataSurfaceInspector.h"

#include "vtkConnectivityFilter.h"
#include "vtkDataObject.h"
#include "vtkDoubleArray.h"
#include "vtkEdgeTable.h"
#include "vtkErrorCode.h"
#include "vtkFeatureEdges.h"
#include "vtkIdList.h"
#include "vtkObjectFactory.h"
#include "vtkSmartPointer.h"
#include "vtkStreamingDemandDrivenPipeline.h"

#include "vtkSVGlobals.h"

vtkStandardNewMacro(vtkSVPolyDataSurfaceInspector);

vtkSVPolyDataSurfaceInspector::vtkSVPolyDataSurfaceInspector()
{
  this->NumberOfElements  = 0;
  this->NumberOfPoints    = 0;
  this->NumberOfEdges     = 0;
  this->NumberOfOpenEdges = 0;
  this->NumberOfNonTriangularElements = 0;
  this->NumberOfNonManifoldEdges      = 0;
  this->SurfaceGenus                  = 0;
  this->NumberOfConnectedRegions      = 0;
  this->NumberOfHoles        = 0;

  this->CheckNumberOfConnectedRegions = 0;
  this->CheckNumberOfHoles            = 0;
}

int vtkSVPolyDataSurfaceInspector::RequestData(
                                          vtkInformation *vtkNotUsed(request),
    vtkInformationVector **inputVector,
    vtkInformationVector *outputVector)
{
  // get the input and output
  vtkPolyData *input = vtkPolyData::GetData(inputVector[0]);
  vtkPolyData *output = vtkPolyData::GetData(outputVector);
  output->DeepCopy(input);

  input->BuildLinks();

  // get number of points
  int numPts = input->GetNumberOfPoints();
  this->NumberOfPoints = input->GetNumberOfPoints();

  // get number of cells
  int numPolys = input->GetNumberOfCells();
  this->NumberOfElements = input->GetNumberOfCells();

  // Start edge insertion for edge table
  vtkNew(vtkEdgeTable, surfaceEdgeTable);
  surfaceEdgeTable->InitEdgeInsertion(numPts, 1);

  // Loop through polys to get edge stats
  this->NumberOfOpenEdges = 0;
  this->NumberOfNonTriangularElements = 0;
  this->NumberOfNonManifoldEdges = 0;
  for (int i=0; i<numPolys; i++)
  {
    // get cell points
    vtkIdType npts, *pts;
    input->GetCellPoints(i, npts, pts);
    if (npts != 3)
    {
      this->NumberOfNonTriangularElements++;
    }
    for (int j=0; j<npts; j++)
    {
      vtkIdType p0, p1;
      p0 = pts[j];
      p1 = pts[(j+1)%npts];

      vtkNew(vtkIdList, edgeNeighbor);
      input->GetCellEdgeNeighbors(i, p0, p1, edgeNeighbor);

      // if there isn't a neighbor, we have an open edge
      if (edgeNeighbor->GetNumberOfIds() == 0)
      {
        this->NumberOfOpenEdges++;
      }
      // if there is more than one neighbor, we have a non-manifold edge
      if (edgeNeighbor->GetNumberOfIds() > 1)
      {
        this->NumberOfNonManifoldEdges++;
      }

      // Check to see if edge has already been inserted
      vtkIdType checkEdge = surfaceEdgeTable->IsEdge(p0, p1);
      if (checkEdge == -1)
      {
        // Get new edge id and insert into table
        vtkIdType edgeId = surfaceEdgeTable->InsertEdge(p0, p1);
      }
    }
  }

  // Now that we have edge table, calculate other surface stats
  int ne = surfaceEdgeTable->GetNumberOfEdges();
  int nv = numPts;
  int nf = numPolys;

  this->NumberOfEdges = ne;

  if (this->NumberOfOpenEdges > 0)
  {
    if (this->NumberOfNonManifoldEdges == 0)
    {
      vtkNew(vtkFeatureEdges, featureEdges);
      featureEdges->SetInputData(input);
      featureEdges->BoundaryEdgesOn();
      featureEdges->FeatureEdgesOff();
      featureEdges->ManifoldEdgesOff();
      featureEdges->NonManifoldEdgesOff();
      featureEdges->Update();

      vtkNew(vtkConnectivityFilter, connector);
      connector->SetInputData(featureEdges->GetOutput());
      connector->SetExtractionModeToAllRegions();
      connector->Update();

      int numHoles = connector->GetNumberOfExtractedRegions();

      int numBoundaryLines = featureEdges->GetOutput()->GetNumberOfLines();
      if (numBoundaryLines != this->NumberOfOpenEdges)
      {
        vtkWarningMacro("Feature edges and manual processing detected different number of open edges");
      }

      // Use Euler characteristic modified by the number of open boundaries to get genus of surface
      nv = nv + numHoles;
      nf = nf + numBoundaryLines;
      ne = ne + numBoundaryLines;

      this->SurfaceGenus = ((ne - nv - nf)/2) + 1;
    }
    else
    {
      this->SurfaceGenus = -1;
    }
  }
  else
  {
    // If water-tight surface, use Euler characteristic to get genus
    if (this->NumberOfNonManifoldEdges == 0)
    {
      this->SurfaceGenus = ((ne - nv - nf)/2) + 1;
    }
    else
    {
      this->SurfaceGenus = -1;
    }
  }

  // Check the number of connected regions
  if (this->CheckNumberOfConnectedRegions)
  {
    vtkNew(vtkConnectivityFilter, connector);
    connector->SetInputData(input);
    connector->SetExtractionModeToAllRegions();
    connector->Update();

    this->NumberOfConnectedRegions = connector->GetNumberOfExtractedRegions();
  }

  // Check the number of holes
  if (this->CheckNumberOfHoles)
  {
    vtkNew(vtkFeatureEdges, featureEdges);
    featureEdges->SetInputData(input);
    featureEdges->BoundaryEdgesOn();
    featureEdges->FeatureEdgesOff();
    featureEdges->ManifoldEdgesOff();
    featureEdges->NonManifoldEdgesOff();
    featureEdges->Update();

    vtkNew(vtkConnectivityFilter, connector);
    connector->SetInputData(featureEdges->GetOutput());
    connector->SetExtractionModeToAllRegions();
    connector->Update();

    this->NumberOfHoles = connector->GetNumberOfExtractedRegions();
  }

  return SV_OK;
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVPolyDataSurfaceInspector::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);

  os << indent << "Number of elements                : " << this->NumberOfElements << endl;
  os << indent << "Number of points                  :   " << this->NumberOfPoints << endl;
  os << indent << "Number of edges                   : " << this->NumberOfEdges << endl;

  os << indent << "Number of open edges              : " << this->NumberOfOpenEdges << endl;
  os << indent << "Number of non-triangular elements : " << this->NumberOfNonTriangularElements << endl;
  os << indent << "Number of non-manifold elements   : " << this->NumberOfNonManifoldEdges << endl;

  os << indent << "Surface genus                     : " << this->SurfaceGenus << endl;
  os << indent << "Number of connected regions       : " << this->NumberOfConnectedRegions << endl;
  os << indent << "Number of holes                   : " << this->NumberOfHoles << endl;

  os << indent << "Check number of connected regions : " << this->CheckNumberOfConnectedRegions << endl;
  os << indent << "Check number of holes             : " << this->CheckNumberOfHoles << endl;

}
