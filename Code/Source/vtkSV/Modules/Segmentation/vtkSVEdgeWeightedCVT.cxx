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

#include "vtkSVEdgeWeightedCVT.h"

#include "vtkCellData.h"
#include "vtkIdList.h"
#include "vtkMath.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkSmartPointer.h"

#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"
#include "vtkSVMathUtils.h"

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVEdgeWeightedCVT);

// ----------------------
// Constructor
// ----------------------
vtkSVEdgeWeightedCVT::vtkSVEdgeWeightedCVT()
{
  this->NumberOfRings = 2;
  this->EdgeWeight = 1.0;
  this->UseCurvatureWeight = 1;
  this->MaximumNumberOfNeighborPatches = 20;
}

// ----------------------
// Destructor
// ----------------------
vtkSVEdgeWeightedCVT::~vtkSVEdgeWeightedCVT()
{
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVEdgeWeightedCVT::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);

  os << indent << "Number of neighborhood rings: " << this->NumberOfRings << "\n";
  os << indent << "Edge weight: " << this->EdgeWeight << "\n";
}

// ----------------------
// InitializeConnectivity
// ----------------------
int vtkSVEdgeWeightedCVT::InitializeConnectivity()
{
  // Number of points and cells
  int numPoints = this->WorkPd->GetNumberOfPoints();
  int numCells =  this->WorkPd->GetNumberOfCells();

  this->WorkPd->BuildLinks();
  if (this->UseCellArray)
  {
    // Initialize point valences
    this->PointCellValenceNumber.resize(numPoints);
    this->PointCellValence.resize(numPoints);

    this->GetPointCellValence();

    // Initialize the cell neighbor vectors
    this->NumberOfNeighbors.resize(numCells);
    this->Neighbors.resize(numCells);

    // Set element as start
    for (int i=0; i<numCells; i++)
    {
      this->NumberOfNeighbors[i] = 1;
      this->Neighbors[i].push_back(i);
    }

    // Get element neighbor rings
    this->GetCellRingNeighbors();

    // Initialize the direct neighbor vectors
    this->NumberOfDirectNeighbors.resize(numCells);
    this->DirectNeighbors.resize(numCells);

    // Get cell edge neighbors
    this->GetCellDirectNeighbors();

    // Initialize cell patch neighbors
    this->NumberOfNeighborPatches.resize(numCells);
    this->NeighborPatchesNumberOfElements.resize(numCells, std::vector<int>(this->MaximumNumberOfNeighborPatches));
    this->NeighborPatchesIds.resize(numCells, std::vector<int>(this->MaximumNumberOfNeighborPatches));

    for (int i=0; i<numCells; i++)
    {
      this->NumberOfNeighborPatches[i] = 1;
      for (int j=0; j<this->MaximumNumberOfNeighborPatches; j++)
      {
        this->NeighborPatchesNumberOfElements[i][j] = 0;
        this->NeighborPatchesIds[i][j] = -1;
      }

      this->NeighborPatchesIds[i][0] = this->PatchIdsArray->GetTuple1(i);
      this->NeighborPatchesNumberOfElements[i][0] = 1;

    }

    // Get cell patch neighbors
    this->GetCellPatchNeighbors();
  }
  else if (this->UsePointArray)
  {
    vtkErrorMacro("Not implemented yet");
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// InitializeGenerators
// ----------------------
int vtkSVEdgeWeightedCVT::InitializeGenerators()
{
  if (this->UseCellArray)
  {
    int numCells = this->WorkPd->GetNumberOfCells();
    int numGenerators;
    if (this->UseGeneratorsArray)
      numGenerators = this->GeneratorsArray->GetNumberOfTuples();
    else
      numGenerators = this->WorkGenerators->GetNumberOfPoints();

    int numComps = this->CVTDataArray->GetNumberOfComponents();
    double *data = new double[numComps];
    for (int i=0; i<numCells; i++)
    {
      int cellGenerator = 0;
      double minDist = VTK_SV_LARGE_DOUBLE;

      // Get number of comps in generator and data
      this->CVTDataArray->GetTuple(i, data);

      for (int j=0; j<numGenerators; j++)
      {
        double testDist;
        if (this->UseGeneratorsArray)
        {
          // Could be large number of comps
          double *generator = new double[numComps];
          this->GeneratorsArray->GetTuple(j, generator);

          // Compute original distance
          testDist = vtkSVMathUtils::Distance(generator, data, numComps);
          testDist /= 2.0;
          delete [] generator;
        }
        else
        {
          // Compute original distance
          double generator[3];
          this->WorkGenerators->GetPoint(j, generator);
          testDist = vtkSVMathUtils::Distance(generator, data, numComps);
          testDist /= 2.0;
        }

        if (testDist < minDist)
        {
          cellGenerator = j;
          minDist = testDist;
        }
      }
      this->PatchIdsArray->SetTuple1(i, cellGenerator);
    }
    delete [] data;
  }

  this->UpdateGenerators();

  return SV_OK;
}

// ----------------------
// GetClosestGenerator
// ----------------------
int vtkSVEdgeWeightedCVT::GetClosestGenerator(const int evalId, int &newGenerator)
{
  // Get current generator
  int numGenerators = this->WorkGenerators->GetNumberOfPoints();
  int currGenerator = this->PatchIdsArray->GetTuple1(evalId);
  newGenerator =  currGenerator;

  // Current minimum to beat is current generator
  double minDist = this->GetEdgeWeightedDistance(newGenerator, evalId);

  // Loop through neighboring patches
  for (int i=0; i<this->NumberOfNeighborPatches[evalId]; i++)
  {
    // Check to make sure not zero elements or the same genrator
    if (this->NeighborPatchesNumberOfElements[evalId][i] != 0)
    {
      if (currGenerator != this->NeighborPatchesIds[evalId][i])
      {
        // Test this generator
        int neighborGenerator = this->NeighborPatchesIds[evalId][i];
        double testDist = this->GetEdgeWeightedDistance(neighborGenerator, evalId);

        // Set new min if less than current
        if (testDist < minDist)
        {
          minDist = testDist;
          newGenerator = neighborGenerator;
        }
      }
    }
  }

  return SV_OK;
}

// ----------------------
// GetEdgeWeightedDistance
// ----------------------
double vtkSVEdgeWeightedCVT::GetEdgeWeightedDistance(const int generatorId, const int evalId)
{
  // Current generator
  int currGenerator = this->PatchIdsArray->GetTuple1(evalId);

  // TODO CHECK FOR NORMALS EARLIER!!!!
  // Current cell normal
  vtkDataArray *cellNormals = this->WorkPd->GetCellData()->GetArray("Normals");
  double currNormal[3];
  cellNormals->GetTuple(evalId, currNormal);

  // Get number of comps in generator and data
  int numComps = this->CVTDataArray->GetNumberOfComponents();
  double *data = new double[numComps];
  this->CVTDataArray->GetTuple(evalId, data);

  // Calculate the edge weight distance
  double edgeWeightedDist;
  if (this->UseGeneratorsArray)
  {
    // Could be large number of comps
    double *generator = new double[numComps];
    this->GeneratorsArray->GetTuple(generatorId, generator);

    // Compute original distance
    edgeWeightedDist = vtkSVMathUtils::Distance(generator, data, numComps);
    edgeWeightedDist /= 2.0;
    delete [] generator;
  }
  else
  {
    // Compute original distance
    double generator[3];
    this->WorkGenerators->GetPoint(generatorId, generator);
    edgeWeightedDist = vtkSVMathUtils::Distance(generator, data, numComps);
    edgeWeightedDist /= 2.0;
  }
  delete [] data;

  // Square in order to add edge weighted part and take norm
  edgeWeightedDist *= edgeWeightedDist;

  double totalWeight = 0.0;
  int numSameGeneratorNeighbors = 0;
  for (int i=0; i<this->NumberOfNeighbors[evalId]; i++)
  {
    int neighborId = this->Neighbors[evalId][i];
    int neighborGenerator = this->PatchIdsArray->GetTuple1(neighborId);
    if (neighborGenerator == generatorId)
    {
      double normal[3];
      cellNormals->GetTuple(neighborId, normal);

      //double crossVec[3];
      //vtkMath::Cross(currNormal, normal, crossVec);
      //double ang = atan2(vtkMath::Norm(crossVec), vtkMath::Dot(currNormal, normal));
      //totalWeight += ang/SV_PI;

      totalWeight += vtkMath::Dot(currNormal, normal);
      numSameGeneratorNeighbors++;
    }
  }

  // Get the generator patch id
  int stopI;
  for (stopI=0; stopI<this->NumberOfNeighborPatches[evalId]; stopI++)
  {
    if (this->NeighborPatchesIds[evalId][stopI] == generatorId)
    {
      break;
    }
  }

  double edgeWeight = this->EdgeWeight;
  if (this->UseCurvatureWeight)
  {
    //if (this->NeighborPatchesNumberOfElements[evalId][stopI] != 0)
    //  edgeWeight = totalWeight/this->NeighborPatchesNumberOfElements[evalId][stopI];
    if (numSameGeneratorNeighbors != 0)
      edgeWeight = 1.0 - totalWeight/numSameGeneratorNeighbors;
  }

  // Get the edge weighted portion
  double edgeWeighting = 0.0;
  if (currGenerator == generatorId)
  {
    edgeWeighting = 2 * edgeWeight * (this->NumberOfNeighbors[evalId] - this->NeighborPatchesNumberOfElements[evalId][stopI]);
  }
  else
  {
    edgeWeighting = 2 * edgeWeight * (this->NumberOfNeighbors[evalId] - this->NeighborPatchesNumberOfElements[evalId][stopI] - 1);
  }

  // Now check the neighbors
  int numFixedNeighbors = 0;
  for (int i=0; i<this->NumberOfNeighbors[evalId]; i++)
  {
    if (this->FixedIds[this->Neighbors[evalId][i]] == 1 &&
        this->PatchIdsArray->GetTuple1(this->Neighbors[evalId][i]) == generatorId)
      numFixedNeighbors++;
  }

  // Divide by the number of neighboring cells
  edgeWeighting /= this->NumberOfNeighbors[evalId];

  // Get the final edge distance to be normalized
  edgeWeightedDist += edgeWeighting;

  edgeWeightedDist = sqrt(edgeWeightedDist);

  return edgeWeightedDist;
}

// ----------------------
// ComputeSurfaceMetric
// ----------------------
int vtkSVEdgeWeightedCVT::ComputeSurfaceMetric(double &evalMetric)
{
  vtkDebugMacro("Need to implement surface energy calculation");
  evalMetric = 0;
  return SV_OK;
}

// ----------------------
// UpdateConnectivity
// ----------------------
int vtkSVEdgeWeightedCVT::UpdateConnectivity(const int evalId, const int oldGenerator,
                                             const int newGenerator)
{
  // See if we can add a neighbor patch
  int newGeneratorLoc;
  this->AddCellPatchNeighbor(evalId, newGenerator, newGeneratorLoc);

  int oldIndexPosition;
  int newIndexPosition;
  int oldIndexCounted = 0;
  // Loop through neighbor patches
  for (int i=0; i<this->NumberOfNeighborPatches[evalId]; i++)
  {
    // Find spot of old generator
    if(this->NeighborPatchesIds[evalId][i] == oldGenerator)
    {
      this->NeighborPatchesNumberOfElements[evalId][i]--;

      oldIndexPosition = i;
      oldIndexCounted = 1;

      break;
    }
  }

  if (oldIndexCounted == 0)
  {
    vtkErrorMacro("The old patch could not be found on update");
    return SV_ERROR;
  }

  // Loop through neighbor cells
  for (int i=0; i<this->NumberOfNeighbors[evalId]; i++)
  {
    int neighborCell = this->Neighbors[evalId][i];

    // If not current cell
    if (neighborCell != evalId)
    {
      oldIndexCounted = 0;
      // Test adding patch to neighbor
      this->AddCellPatchNeighbor(neighborCell, newGenerator, newGeneratorLoc);

      // Loop through neighbor cells neighbor patches now
      for (int j=0; j<this->NumberOfNeighborPatches[neighborCell]; j++)
      {
        if (this->NeighborPatchesIds[neighborCell][j] == oldGenerator)
        {
          this->NeighborPatchesNumberOfElements[neighborCell][j]--;
          oldIndexPosition = j;
          oldIndexCounted =  1;

          break;
        }
      }
      if (oldIndexCounted == 0)
      {
        vtkErrorMacro("The old patch could not be found on update");
        return SV_ERROR;
      }
    }
  }

  this->PatchIdsArray->SetTuple1(evalId, newGenerator);

  return SV_OK;
}

// ----------------------
// UpdateGenerators
// ----------------------
int vtkSVEdgeWeightedCVT::UpdateGenerators()
{
  // Number of generators
  int numGenerators = this->WorkGenerators->GetNumberOfPoints();

  // Number of cells
  int numCells = this->WorkPd->GetNumberOfCells();

  // Loop through cells
  std::vector<std::vector<double> > newGenerators(numGenerators, std::vector<double>(3));
  std::vector<int>       newGeneratorElements(numGenerators);
  for (int i=0; i<numGenerators; i++)
  {
    for (int j=0; j<3; j++)
      newGenerators[i][j] = 0.0;
    newGeneratorElements[i] = 0;
  }
  for (int i=0; i<numCells; i++)
  {
    // Get data and patch id
    double data[3];
    this->CVTDataArray->GetTuple(i, data);
    int patchId = this->PatchIdsArray->GetTuple1(i);

    // Loop through num of components
    for (int j=0; j<3; j++)
      newGenerators[patchId][j] += data[j];
    newGeneratorElements[patchId]++;
  }

  for (int i=0; i<numGenerators; i++)
  {
    if (newGeneratorElements[i] != 0)
    {
      for (int j=0; j<3; j++)
        newGenerators[i][j] /= newGeneratorElements[i];

      double newGen[3];
      for (int j=0; j<3; j++)
        newGen[j] = newGenerators[i][j];

      vtkMath::Normalize(newGen);

      this->WorkGenerators->GetPoints()->SetPoint(i, newGen);
    }
  }


  return SV_OK;
}

// ----------------------
// GetPointCellValence
// ----------------------
int vtkSVEdgeWeightedCVT::GetPointCellValence()
{
  this->WorkPd->GetNumberOfPoints();
  // Number of points and cells
  int numPoints = this->WorkPd->GetNumberOfPoints();

  for (int i=0; i<numPoints; i++)
  {
    // get point cells
    vtkNew(vtkIdList, pointCells);

    this->WorkPd->GetPointCells(i, pointCells);

    // Set number of point cells
    this->PointCellValenceNumber[i] = pointCells->GetNumberOfIds();

    // Update point cell info
    for (int j=0; j<pointCells->GetNumberOfIds(); j++)
    {
      this->PointCellValence[i].push_back(pointCells->GetId(j));
    }
  }

  return SV_OK;
}

// ----------------------
// GetCellRingNeighbors
// ----------------------
int vtkSVEdgeWeightedCVT::GetCellRingNeighbors(int ringNumber)
{
  // Number of cells
  int numCells = this->WorkPd->GetNumberOfCells();

  for (int i=0; i<numCells; i++)
  {
    // temporary node vec
    std::vector<int> tmpNodes;
    int iSize = this->Neighbors[i].size();

    for (int j=0; j<iSize; j++)
    {
      // Get neighbor cell points
      int neiCellId = this->Neighbors[i][j];
      vtkIdType *pts, npts;
      this->WorkPd->GetCellPoints(neiCellId, npts, pts);

      // Loop around cell points
      for (int k=0; k<npts; k++)
      {
        int tmpNode = pts[k];
        int kSize   = tmpNodes.size();

        int kk = 0;
        for (kk=0; kk<kSize; kk++)
        {
          if (tmpNode == tmpNodes[kk])
          {
            break;
          }
        }
        if (kk == kSize)
        {
          tmpNodes.push_back(tmpNode);
        }
      }
    }

    // Now find neighbor elems
    iSize = tmpNodes.size();

    for (int j=0; j<iSize; j++)
    {
      int tmpNode = tmpNodes[j];

      for (int k=0; k<this->PointCellValenceNumber[tmpNode]; k++)
      {
        int tmpCell = this->PointCellValence[tmpNode][k];
        int kSize =   this->Neighbors[i].size();

        int kk=0;
        for (kk=0; kk<kSize; kk++)
        {
          if (tmpCell == this->Neighbors[i][kk])
          {
            break;
          }
        }
        if (kk == kSize)
        {
          this->Neighbors[i].push_back(tmpCell);
        }
      }
    }

    this->NumberOfNeighbors[i] = this->Neighbors[i].size();
  }

  if (ringNumber < this->NumberOfRings)
  {
    ringNumber++;
    this->GetCellRingNeighbors(ringNumber);
  }

  return SV_OK;
}

// ----------------------
// GetCellDirectNeighbors
// ----------------------
int vtkSVEdgeWeightedCVT::GetCellDirectNeighbors()
{

  int numCells = this->WorkPd->GetNumberOfCells();
  this->WorkPd->BuildLinks();

  // Loop through cells
  for (int i=0; i<numCells; i++)
  {
    // count number of edge neighbors
    int directNeiCount = 0;
    std::vector<int> neighborCells;

    // Get cell points
    vtkIdType *pts, npts;
    this->WorkPd->GetCellPoints(i, npts, pts);

    // Get cell edge neighbors
    for (int j=0; j<npts; j++)
    {
      int ptId0 = pts[j];
      int ptId1 = pts[(j+1)%npts];

      // Get cell edge neighbors
      vtkNew(vtkIdList, cellEdgeNeighbors);
      this->WorkPd->GetCellEdgeNeighbors(i, ptId0, ptId1, cellEdgeNeighbors);
      directNeiCount += cellEdgeNeighbors->GetNumberOfIds();
      for (int k=0; k<cellEdgeNeighbors->GetNumberOfIds(); k++)
      {
        neighborCells.push_back(cellEdgeNeighbors->GetId(k));
      }
    }
    this->DirectNeighbors[i] = neighborCells;
    this->NumberOfDirectNeighbors[i] = directNeiCount;
  }

  return SV_OK;
}

// ----------------------
// GetCellPatchNeighbors
// ----------------------
int vtkSVEdgeWeightedCVT::GetCellPatchNeighbors()
{

  int numCells = this->WorkPd->GetNumberOfCells();
  this->WorkPd->BuildLinks();

  // Loop through cells
  for (int i=0; i<numCells; i++)
  {
    for (int j=0; j<this->NumberOfNeighbors[i]; j++)
    {
      int cellNeighbor = this->Neighbors[i][j];
      if (cellNeighbor != i)
      {
        int cellNeighborPatch = this->PatchIdsArray->GetTuple1(cellNeighbor);
        int neighborLoc;
        this->AddCellPatchNeighbor(i, cellNeighborPatch, neighborLoc);
      }
    }

  }

  // Check to make sure correct
  int error = 0;
  int numNeighbors = 0;

  for (int i=0; i<numCells; i++)
  {
    numNeighbors = 0;

    for (int j=0; j<this->NumberOfNeighborPatches[i]; j++)
    {
      numNeighbors += this->NeighborPatchesNumberOfElements[i][j];
    }

    if (numNeighbors != this->NumberOfNeighbors[i])
    {
      error++;
    }
  }

  if (error != 0)
  {
    vtkErrorMacro("Cell ring neighbors not correct");
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// AddCellPatchNeighbor
// ----------------------
int vtkSVEdgeWeightedCVT::AddCellPatchNeighbor(const int cellId, const int cellNeighborPatch, int &neighborLoc)
{
  int counted = 0;
  int numNeighborPatches;

  for (int i=0; i<this->NumberOfNeighborPatches[cellId]; i++)
  {
    if (this->NeighborPatchesIds[cellId][i] == cellNeighborPatch)
    {
      counted = 1;
      this->NeighborPatchesNumberOfElements[cellId][i]++;
      neighborLoc = i;

      break;
    }
  }

  if (counted == 0)
  {
    this->NumberOfNeighborPatches[cellId]++;
    numNeighborPatches = this->NumberOfNeighborPatches[cellId];

    if (numNeighborPatches > this->MaximumNumberOfNeighborPatches)
    {
      vtkErrorMacro("Error in adding neighbor patch");
      return SV_ERROR;
    }
    else
    {
      this->NeighborPatchesIds[cellId][numNeighborPatches -1] = cellNeighborPatch;
      this->NeighborPatchesNumberOfElements[cellId][numNeighborPatches - 1]++;
      neighborLoc = numNeighborPatches - 1;
    }

  }

  return counted;
}

// ----------------------
// IsBoundaryCell
// ----------------------
int vtkSVEdgeWeightedCVT::IsBoundaryCell(const int cellId)
{
  int isOnBoundary = 0;

  for (int i=0; i<this->NumberOfDirectNeighbors[cellId]; i++)
  {
    int neighborCell = this->DirectNeighbors[cellId][i];

    if (this->PatchIdsArray->GetTuple1(cellId) != this->PatchIdsArray->GetTuple1(neighborCell))
    {
      isOnBoundary = 1;
      break;
    }
  }

  return true;
}
