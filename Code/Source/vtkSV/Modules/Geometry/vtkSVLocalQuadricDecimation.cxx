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

#include "vtkSVLocalQuadricDecimation.h"
#include "vtkCellArray.h"
#include "vtkCellData.h"
#include "vtkEdgeTable.h"
#include "vtkDoubleArray.h"
#include "vtkGenericCell.h"
#include "vtkIdList.h"
#include "vtkMath.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkObjectFactory.h"
#include "vtkPolyData.h"
#include "vtkPointData.h"
#include "vtkPriorityQueue.h"
#include "vtkTriangle.h"
#include "vtkSmartPointer.h"
#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVLocalQuadricDecimation);


// ----------------------
// Constructor
// ----------------------
vtkSVLocalQuadricDecimation::vtkSVLocalQuadricDecimation()
{
  this->Edges = vtkEdgeTable::New();
  this->EdgeCosts = vtkPriorityQueue::New();
  this->EndPoint1List = vtkIdList::New();
  this->EndPoint2List = vtkIdList::New();
  this->ErrorQuadrics = nullptr;
  this->TargetPoints = vtkDoubleArray::New();
//  this->DecimateCellArray = vtkIntArray::New();
//  this->DecimatePointArray = vtkIntArray::New();

  this->TargetReduction = 0.9;
  this->NumberOfEdgeCollapses = 0;
  this->NumberOfComponents = 0;

  this->AttributeErrorMetric = 0;
  this->ScalarsAttribute = 1;
  this->VectorsAttribute = 1;
  this->NormalsAttribute = 1;
  this->TCoordsAttribute = 1;
  this->TensorsAttribute = 1;

  this->ScalarsWeight = 0.1;
  this->VectorsWeight = 0.1;
  this->NormalsWeight = 0.1;
  this->TCoordsWeight = 0.1;
  this->TensorsWeight = 0.1;

  this->ActualReduction = 0.0;

  this->DecimateCellArrayName  = nullptr;
  this->DecimatePointArrayName = nullptr;

  this->UseCellArray = 0;
  this->UsePointArray = 0;

  this->changedPoint = nullptr;
}

// ----------------------
// Destructor
// ----------------------
vtkSVLocalQuadricDecimation::~vtkSVLocalQuadricDecimation()
{
  this->Edges->Delete();
  this->EdgeCosts->Delete();
  this->EndPoint1List->Delete();
  this->EndPoint2List->Delete();
  this->TargetPoints->Delete();

  if (this->DecimateCellArrayName != nullptr)
  {
    delete [] this->DecimateCellArrayName;
    this->DecimateCellArrayName = nullptr;
  }
  if (this->DecimatePointArrayName != nullptr)
  {
    delete [] this->DecimatePointArrayName;
    this->DecimatePointArrayName = nullptr;
  }
}

// ----------------------
// SetPointAttributeArray
// ----------------------
void vtkSVLocalQuadricDecimation::SetPointAttributeArray(vtkIdType ptId,
                                                  const double *x)
{
  int i;
  this->Mesh->GetPoints()->SetPoint(ptId, x);

  for (i = 0; i < this->NumberOfComponents; i++)
    {
    if (i < this->AttributeComponents[0])
      {
      this->Mesh->GetPointData()->GetScalars()->
        SetComponent(ptId, i, x[3+i]/this->AttributeScale[0]);
      }
    else if (i < this->AttributeComponents[1])
      {
      this->Mesh->GetPointData()->GetVectors()->
        SetComponent(ptId, i-this->AttributeComponents[0], x[3+i]/this->AttributeScale[1]);
      }
    else if (i < this->AttributeComponents[2])
      {
      this->Mesh->GetPointData()->GetNormals()->
        SetComponent(ptId, i-this->AttributeComponents[1], x[3+i]/this->AttributeScale[2]);
      }
    else if (i < this->AttributeComponents[3])
      {
      this->Mesh->GetPointData()->GetTCoords()->
        SetComponent(ptId, i-this->AttributeComponents[2], x[3+i]/this->AttributeScale[3]);
      }
    else if (i < this->AttributeComponents[4])
      {
      this->Mesh->GetPointData()->GetTensors()->
        SetComponent(ptId, i-this->AttributeComponents[3], x[3+i]/this->AttributeScale[4]);
      }
    }
}

// ----------------------
// GetPointAttributeArray
// ----------------------
void vtkSVLocalQuadricDecimation::GetPointAttributeArray(vtkIdType ptId, double *x)
{
  int i;
  this->Mesh->GetPoints()->GetPoint(ptId, x);

  for (i = 0; i < this->NumberOfComponents; i++)
    {
    if (i < this->AttributeComponents[0])
      {
      x[3+i] = this->Mesh->GetPointData()->GetScalars()->
        GetComponent(ptId, i) *  this->AttributeScale[0];
      }
    else if (i < this->AttributeComponents[1])
      {
      x[3+i] = this->Mesh->GetPointData()->GetVectors()->
        GetComponent(ptId, i-this->AttributeComponents[0]) *  this->AttributeScale[1];
      }
    else if (i < this->AttributeComponents[2])
      {
      x[3+i] = this->Mesh->GetPointData()->GetNormals()->
        GetComponent(ptId, i-this->AttributeComponents[1]) *  this->AttributeScale[2];
      }
    else if (i < this->AttributeComponents[3])
      {
      x[3+i] = this->Mesh->GetPointData()->GetTCoords()->
        GetComponent(ptId, i-this->AttributeComponents[2]) *  this->AttributeScale[3];
      }
    else if (i < this->AttributeComponents[4])
      {
      x[3+i] = this->Mesh->GetPointData()->GetTensors()->
        GetComponent(ptId, i-this->AttributeComponents[3]) *  this->AttributeScale[4];
      }
    }
}

// ----------------------
// RequestData
// ----------------------
int vtkSVLocalQuadricDecimation::RequestData(
  vtkInformation *vtkNotUsed(request),
  vtkInformationVector **inputVector,
  vtkInformationVector *outputVector)
{
  // get the info objects
  vtkInformation *inInfo = inputVector[0]->GetInformationObject(0);
  vtkInformation *outInfo = outputVector->GetInformationObject(0);

  // get the input and output
  vtkPolyData *input = vtkPolyData::SafeDownCast(
    inInfo->Get(vtkDataObject::DATA_OBJECT()));
  vtkPolyData *output = vtkPolyData::SafeDownCast(
    outInfo->Get(vtkDataObject::DATA_OBJECT()));

  vtkIdType numPts = input->GetNumberOfPoints();
  vtkIdType numTris = input->GetNumberOfPolys();
  vtkIdType edgeId, i;
  int j;
  double cost;
  double *x;
  vtkCellArray *polys;
  vtkDataArray *attrib;
  vtkPoints *points;
  vtkPointData *pointData;
  vtkIdType endPtIds[2];
  vtkIdList *outputCellList;
  vtkIdType npts;
  const vtkIdType *pts;
  vtkIdType numDeletedTris=0;

  // check some assuptiona about the data
  if (input->GetPolys() == nullptr || input->GetPoints() == nullptr ||
      input->GetPointData() == nullptr  || input->GetFieldData() == nullptr)
    {
    vtkErrorMacro("Nothing to decimate");
    return 1;
    }

  if (input->GetPolys()->GetMaxCellSize() > 3)
    {
    vtkErrorMacro("Can only decimate triangles");
    return 1;
    }

  if (this->UsePointArray)
  {
    if (this->DecimatePointArrayName == nullptr)
    {
      std::cout<<"No DecimatePointArrayName given." << endl;
      return SV_ERROR;
    }
    if (this->GetDecimateArrays(input,0) != 1)
    {
      vtkErrorMacro("Need point array on mesh to be able to local decimation");
      return 1;
    }
  }
  if (this->UseCellArray)
  {
    if (this->DecimateCellArrayName == nullptr)
    {
      std::cout<<"No DecimateCellArrayName given." << endl;
      return SV_ERROR;
    }
    if (this->GetDecimateArrays(input,1) != 1)
    {
      vtkErrorMacro("Need cell array on mesh to be able to local decimation");
      return 1;
    }
  }
  input->BuildLinks();

  this->changedPoint = new int[numPts];
  this->fixedPoint = new int[numPts];
  for (i = 0; i < numPts; i++)
  {
    this->fixedPoint[i] = 0;
    this->changedPoint[i] = -1;
  }
  this->SetFixedPoints(input,numTris);

  polys = vtkCellArray::New();
  points = vtkPoints::New();
  pointData = vtkPointData::New();
  outputCellList = vtkIdList::New();

  // copy the input (only polys) to our working mesh
  this->Mesh = vtkPolyData::New();
  points->DeepCopy(input->GetPoints());
  this->Mesh->SetPoints(points);
  points->Delete();
  polys->DeepCopy(input->GetPolys());
  this->Mesh->SetPolys(polys);
  polys->Delete();
  if (this->AttributeErrorMetric)
    {
    this->Mesh->GetPointData()->DeepCopy(input->GetPointData());
    }
  pointData->Delete();
  this->Mesh->GetPointData()->PassData(input->GetPointData());
  this->Mesh->GetCellData()->PassData(input->GetCellData());
  this->Mesh->GetFieldData()->PassData(input->GetFieldData());

  this->Mesh->BuildCells();
  this->Mesh->BuildLinks();


  this->ErrorQuadrics =
    new vtkSVLocalQuadricDecimation::ErrorQuadric[numPts];

  vtkDebugMacro(<<"Computing Edges");
  this->Edges->InitEdgeInsertion(numPts, 1); // storing edge id as attribute
  this->EdgeCosts->Allocate(this->Mesh->GetPolys()->GetNumberOfCells() * 3);
  for (i = 0; i <  this->Mesh->GetNumberOfCells(); i++)
    {
    this->Mesh->GetCellPoints(i, npts, pts);

    for (j = 0; j < 3; j++)
      {
      int useEdge = 1;
      if (this->fixedPoint[pts[j]] ||
          this->fixedPoint[pts[(j+1)%3]])
	useEdge = 0;

      if (useEdge)
      {
        if (this->Edges->IsEdge(pts[j], pts[(j+1)%3]) == -1)
          {
	  // If this edge has not been processed, get an id for it, add it to
	  // the edge list (Edges), and add its endpoints to the EndPoint1List
	  // and EndPoint2List (the 2 endpoints to different lists).
	  edgeId = this->Edges->GetNumberOfEdges();
	  this->Edges->InsertEdge(pts[j], pts[(j+1)%3], edgeId);
	  this->EndPoint1List->InsertId(edgeId, pts[j]);
	  this->EndPoint2List->InsertId(edgeId, pts[(j+1)%3]);
          }
        }
      }
    }

  this->UpdateProgress(0.1);

  this->NumberOfComponents = 0;
  if (this->AttributeErrorMetric)
    {
    this->ComputeNumberOfComponents();
    }
  x = new double [3+this->NumberOfComponents];
  this->CollapseCellIds = vtkIdList::New();
  this->TempX = new double [3+this->NumberOfComponents];
  this->TempQuad = new double[11 + 4 * this->NumberOfComponents];

  this->TempB = new double [3 +  this->NumberOfComponents];
  this->TempA = new double*[3 +  this->NumberOfComponents];
  this->TempData = new double [(3 +  this->NumberOfComponents)*(3 +  this->NumberOfComponents)];
  for (i = 0; i < 3 +  this->NumberOfComponents; i++)
    {
    this->TempA[i] = this->TempData+i*(3 +  this->NumberOfComponents);
    }
  this->TargetPoints->SetNumberOfComponents(3+this->NumberOfComponents);

  vtkDebugMacro(<<"Computing Quadrics");
  this->InitializeQuadrics(numPts);
  this->AddBoundaryConstraints();
  this->UpdateProgress(0.15);

  vtkDebugMacro(<<"Computing Costs");
  // Compute the cost of and target point for collapsing each edge.
  for (i = 0; i < this->Edges->GetNumberOfEdges(); i++)
    {
    if (this->AttributeErrorMetric)
      {
      cost = this->ComputeCost2(i, x);
      }
    else
      {
      cost = this->ComputeCost(i, x);
      }
    this->EdgeCosts->Insert(cost, i);
    this->TargetPoints->InsertTuple(i, x);
    }
  this->UpdateProgress(0.20);

  // Okay collapse edges until desired reduction is reached
  this->ActualReduction = 0.0;
  this->NumberOfEdgeCollapses = 0;
  edgeId = this->EdgeCosts->Pop(0,cost);

  int abort = 0;
  while ( !abort && edgeId >= 0 && cost < VTK_DOUBLE_MAX &&
         this->ActualReduction < this->TargetReduction )
    {

    if ( ! (this->NumberOfEdgeCollapses % 10000) )
      {
      vtkDebugMacro(<<"Collapsing edge#" << this->NumberOfEdgeCollapses);
      this->UpdateProgress (0.20 + 0.80*this->NumberOfEdgeCollapses/numPts);
      abort = this->GetAbortExecute();
      }

    endPtIds[0] = this->EndPoint1List->GetId(edgeId);
    endPtIds[1] = this->EndPoint2List->GetId(edgeId);
    this->TargetPoints->GetTuple(edgeId, x);

    // check for a poorly placed point
    if ( !this->IsGoodPlacement(endPtIds[0], endPtIds[1], x))
      {
      vtkDebugMacro(<<"Poor placement detected " << edgeId << " " <<  cost);
      // return the point to the queue but with the max cost so that
      // when it is recomputed it will be reconsidered
      this->EdgeCosts->Insert(VTK_DOUBLE_MAX, edgeId);

      edgeId = this->EdgeCosts->Pop(0, cost);
      continue;
      }

    this->NumberOfEdgeCollapses++;

    // Set the new coordinates of point0.
    this->SetPointAttributeArray(endPtIds[0], x);
    vtkDebugMacro(<<"Cost: " << cost << " Edge: "
                  << endPtIds[0] << " " << endPtIds[1]);

    // Merge the quadrics of the two points.
    this->AddQuadric(endPtIds[1], endPtIds[0]);

    this->UpdateEdgeData(endPtIds[0], endPtIds[1]);

    // Update the output triangles.
    vtkIdType tmpNum = this->CollapseEdge(endPtIds[0], endPtIds[1]);
    numDeletedTris += tmpNum;
    this->ActualReduction = (double) numDeletedTris / numTris;
    edgeId = this->EdgeCosts->Pop(0, cost);
    }

  vtkDebugMacro(<<"Number Of Edge Collapses: "
                << this->NumberOfEdgeCollapses << " Cost: " << cost);

  // clean up working data
  for (i = 0; i < numPts; i++)
    {
    delete [] this->ErrorQuadrics[i].Quadric;
    }
  delete [] this->ErrorQuadrics;
  delete [] x;
  this->CollapseCellIds->Delete();
  delete [] this->TempX;
  delete [] this->TempQuad;
  delete [] this->TempB;
  delete [] this->TempA;
  delete [] this->TempData;

  // copy the simplified mesh from the working mesh to the output mesh
  for (i = 0; i < this->Mesh->GetNumberOfCells(); i++)
    {
    if (this->Mesh->GetCell(i)->GetCellType() != VTK_EMPTY_CELL)
      {
      outputCellList->InsertNextId(i);
      }
    }
  this->CorrectPointData(this->Mesh);

  output->Reset();
  output->Allocate(this->Mesh, outputCellList->GetNumberOfIds());
  output->GetPointData()->CopyAllocate(this->Mesh->GetPointData(),0);
  output->GetCellData()->CopyAllocate(this->Mesh->GetCellData(),0);
  output->CopyCells(this->Mesh, outputCellList);

  //std::cout<<"Check Pts: "<<input->GetNumberOfPoints()<<" "<<this->Mesh->GetNumberOfPoints()<<endl;
  //std::cout<<"Check Cells: "<<input->GetNumberOfCells()<<" "<<this->Mesh->GetNumberOfCells()<<endl;
  this->Mesh->DeleteLinks();
  this->Mesh->Delete();
  outputCellList->Delete();

  // renormalize, clamp attributes
  if (this->AttributeErrorMetric)
    {
    if (nullptr != (attrib = output->GetPointData()->GetNormals()))
      {
      for (i = 0; i < attrib->GetNumberOfTuples(); i++)
        {
        vtkMath::Normalize(attrib->GetTuple3(i));
        }
      }
    // might want to add clamping texture coordinates??
    }


  delete [] this->changedPoint;
  delete [] this->fixedPoint;
  return 1;
}

// ----------------------
// InitializeQuadrics
// ----------------------
void vtkSVLocalQuadricDecimation::InitializeQuadrics(vtkIdType numPts)
{
  vtkPolyData *input = this->Mesh;
  double *QEM;
  vtkIdType ptId;
  int i, j;
  vtkCellArray *polys;
  vtkIdType npts;
  const vtkIdType *pts;
  double point0[3], point1[3], point2[3];
  double n[3];
  double tempP1[3], tempP2[3],  d, triArea2;
  double data[16];
  double *A[4], x[4];
  int index[4];
  A[0] = data;
  A[1] = data+4;
  A[2] = data+8;
  A[3] = data+12;

  // allocate local QEM sparce matrix
  QEM = new double[11 + 4 * this->NumberOfComponents];

  // clear and allocate global QEM array
  for (ptId = 0; ptId < numPts; ptId++)
    {
    this->ErrorQuadrics[ptId].Quadric =
      new double[11 + 4 * this->NumberOfComponents];
    for (i = 0; i < 11 + 4 * this->NumberOfComponents; i++)
      {
      this->ErrorQuadrics[ptId].Quadric[i] = 0.0;
      }
    }

  polys = input->GetPolys();
  // compute the QEM for each face
  for (polys->InitTraversal(); polys->GetNextCell(npts, pts); )
    {
    input->GetPoint(pts[0], point0);
    input->GetPoint(pts[1], point1);
    input->GetPoint(pts[2], point2);
    for (i = 0; i < 3; i++)
      {
      tempP1[i] = point1[i] - point0[i];
      tempP2[i] = point2[i] - point0[i];
      }
    vtkMath::Cross(tempP1, tempP2, n);
    triArea2 = vtkMath::Normalize(n);
    //triArea2 = (triArea2 * triArea2 * 0.25);
    triArea2 = triArea2 * 0.5;
    // I am unsure whether this should be squared or not??
    d = -vtkMath::Dot(n, point0);
    // could possible add in angle weights??

    // set the geometric part of the QEM
    QEM[0] = n[0] * n[0];
    QEM[1] = n[0] * n[1];
    QEM[2] = n[0] * n[2];
    QEM[3] = d * n[0];

    QEM[4] = n[1] * n[1];
    QEM[5] = n[1] * n[2];
    QEM[6] = d * n[1];

    QEM[7] = n[2] * n[2];
    QEM[8] = d * n[2];

    QEM[9] = d * d;
    QEM[10] = 1;

    if (this->AttributeErrorMetric)
      {
      for (i = 0; i < 3; i++)
        {
        A[0][i] = point0[i];
        A[1][i] = point1[i];
        A[2][i] = point2[i];
        A[3][i] = n[i];
        }
      A[0][3] =  A[1][3] = A[2][3] = 1;
      A[3][3] = 0;

      // should handle poorly condition matrix better
      if (vtkMath::LUFactorLinearSystem(A, index, 4))
        {
        for (i = 0; i < this->NumberOfComponents; i++)
          {
          x[3] = 0;
          if (i < this->AttributeComponents[0])
            {
            x[0] = input->GetPointData()->GetScalars()->GetComponent(pts[0], i) *  this->AttributeScale[0];
            x[1] = input->GetPointData()->GetScalars()->GetComponent(pts[1], i) *  this->AttributeScale[0];
            x[2] = input->GetPointData()->GetScalars()->GetComponent(pts[2], i) *  this->AttributeScale[0];
            }
          else if (i < this->AttributeComponents[1])
            {
            x[0] = input->GetPointData()->GetVectors()->GetComponent(pts[0], i - this->AttributeComponents[0]) *  this->AttributeScale[1];
            x[1] = input->GetPointData()->GetVectors()->GetComponent(pts[1], i - this->AttributeComponents[0]) *  this->AttributeScale[1];
            x[2] = input->GetPointData()->GetVectors()->GetComponent(pts[2], i - this->AttributeComponents[0]) *  this->AttributeScale[1];
            }
          else if (i < this->AttributeComponents[2])
            {
            x[0] = input->GetPointData()->GetNormals()->GetComponent(pts[0], i - this->AttributeComponents[1]) *  this->AttributeScale[2];
            x[1] = input->GetPointData()->GetNormals()->GetComponent(pts[1], i - this->AttributeComponents[1]) *  this->AttributeScale[2];
            x[2] = input->GetPointData()->GetNormals()->GetComponent(pts[2], i - this->AttributeComponents[1]) *  this->AttributeScale[2];
            }
          else if (i < this->AttributeComponents[3])
            {
            x[0] = input->GetPointData()->GetTCoords()->GetComponent(pts[0], i - this->AttributeComponents[2]) *  this->AttributeScale[3];
            x[1] = input->GetPointData()->GetTCoords()->GetComponent(pts[1], i - this->AttributeComponents[2])*  this->AttributeScale[3];
            x[2] = input->GetPointData()->GetTCoords()->GetComponent(pts[2], i - this->AttributeComponents[2])*  this->AttributeScale[3];
            }
          else if (i < this->AttributeComponents[4])
            {
            x[0] = input->GetPointData()->GetTensors()->GetComponent(pts[0], i - this->AttributeComponents[3])*  this->AttributeScale[4];
            x[1] = input->GetPointData()->GetTensors()->GetComponent(pts[1], i - this->AttributeComponents[3])*  this->AttributeScale[4];
            x[2] = input->GetPointData()->GetTensors()->GetComponent(pts[2], i - this->AttributeComponents[3])*  this->AttributeScale[4];
            }
          vtkMath::LUSolveLinearSystem(A, index, x, 4);

          // add in the contribution of this element into the QEM
          QEM[0] += x[0] * x[0];
          QEM[1] += x[0] * x[1];
          QEM[2] += x[0] * x[2];
          QEM[3] += x[3] * x[0];

          QEM[4] += x[1] * x[1];
          QEM[5] += x[1] * x[2];
          QEM[6] += x[3] * x[1];

          QEM[7] += x[2] * x[2];
          QEM[8] += x[3] * x[2];

          QEM[9] += x[3] * x[3];

          QEM[11+i*4] = -x[0];
          QEM[12+i*4] = -x[1];
          QEM[13+i*4] = -x[2];
          QEM[14+i*4] = -x[3];
          }
        }
      else
        {
        vtkErrorMacro(<<"Unable to factor attribute matrix!");
        }
      }

      // add the QEM to all point of the face
    for (i = 0; i < 3; i++)
      {
      for (j = 0; j < 11 + 4 * this->NumberOfComponents; j++)
        {
        this->ErrorQuadrics[pts[i]].Quadric[j] += QEM[j] * triArea2;
        }
      }
    }//for all triangles

  delete [] QEM;
}


// ----------------------
// AddBoundaryConstraints
// ----------------------
void vtkSVLocalQuadricDecimation::AddBoundaryConstraints(void)
{
  vtkPolyData *input = this->Mesh;
  double *QEM;
  vtkIdType  cellId;
  int i, j;
  vtkIdType npts;
  const vtkIdType *pts;
  double t0[3], t1[3], t2[3];
  double e0[3], e1[3], n[3], c, d, w;
  vtkIdList *cellIds = vtkIdList::New();

  // allocate local QEM space matrix
  QEM = new double[11 + 4 * this->NumberOfComponents];

  for (cellId = 0; cellId < input->GetNumberOfCells(); cellId++)
    {
    input->GetCellPoints(cellId, npts, pts);

    for (i = 0; i < 3; i++)
      {
      input->GetCellEdgeNeighbors(cellId, pts[i], pts[(i+1)%3], cellIds);
      if (cellIds->GetNumberOfIds() == 0)
        {
        // this is a boundary
        input->GetPoint(pts[(i+2)%3], t0);
        input->GetPoint(pts[i], t1);
        input->GetPoint(pts[(i+1)%3], t2);

        // computing a plane which is orthogonal to line t1, t2 and incident
        // with it
        for (j = 0; j < 3; j++)
          {
          e0[j] = t2[j] - t1[j];
          }
        for (j = 0; j < 3; j++)
          {
          e1[j] = t0[j] - t1[j];
          }

        // compute n so that it is orthogonal to e0 and parallel to the
        // triangle
        c = vtkMath::Dot(e0,e1)/(e0[0]*e0[0]+e0[1]*e0[1]+e0[2]*e0[2]);
        for (j = 0; j < 3; j++)
          {
          n[j] = e1[j] - c*e0[j];
          }
        vtkMath::Normalize(n);
        d = -vtkMath::Dot(n, t1);
        w = vtkMath::Norm(e0);

        //w *= w;
        // area issue ??
        // could possible add in angle weights??
        QEM[0] = n[0] * n[0];
        QEM[1] = n[0] * n[1];
        QEM[2] = n[0] * n[2];
        QEM[3] = d * n[0];

        QEM[4] = n[1] * n[1];
        QEM[5] = n[1] * n[2];
        QEM[6] = d * n[1];

        QEM[7] = n[2] * n[2];
        QEM[8] = d * n[2];

        QEM[9] = d * d;

        QEM[10] = 1;

        // need to add orthogonal plane with the other Attributes, but this
        // is not clear??
        // check to interaction with attribute data
        for (j = 0; j < 11; j++)
          {
          this->ErrorQuadrics[pts[i]].Quadric[j] += QEM[j]*w;
          this->ErrorQuadrics[pts[(i+1)%3]].Quadric[j] += QEM[j]*w;
          }
        }
      }
    }
  cellIds->Delete();
  delete [] QEM;
}

// ----------------------
// AddQuadric
// ----------------------
void vtkSVLocalQuadricDecimation::AddQuadric(vtkIdType oldPtId, vtkIdType newPtId)
{
  int i;

  for (i = 0; i < 11 + 4*this->NumberOfComponents; i++)
    {
    this->ErrorQuadrics[newPtId].Quadric[i] +=
      this->ErrorQuadrics[oldPtId].Quadric[i];
    }
}

// ----------------------
// FindAffetedEdges
// ----------------------
void vtkSVLocalQuadricDecimation::FindAffectedEdges(vtkIdType p1Id, vtkIdType p2Id,
                                              vtkIdList *edges)
{
  vtkIdType ncells;
  vtkIdType *cells, npts, edgeId; 
  const vtkIdType *pts;
  unsigned short i, j;

  edges->Reset();
  this->Mesh->GetPointCells(p2Id, ncells, cells);
  for (i = 0; i < ncells; i++)
    {
    this->Mesh->GetCellPoints(cells[i], npts, pts);
    for (j = 0; j < 3; j++)
      {
      if (pts[j] != p1Id && pts[j] != p2Id &&
          (edgeId = this->Edges->IsEdge(pts[j], p2Id)) >= 0 &&
          edges->IsId(edgeId) == -1)
        {
        edges->InsertNextId(edgeId);
        }
      }
    }

  this->Mesh->GetPointCells(p1Id, ncells, cells);
  for (i = 0; i < ncells; i++)
    {
    this->Mesh->GetCellPoints(cells[i], npts, pts);
    for (j = 0; j < 3; j++)
      {
      if (pts[j] != p1Id && pts[j] != p2Id &&
          (edgeId = this->Edges->IsEdge(pts[j], p1Id)) >= 0 &&
          edges->IsId(edgeId) == -1)
        {
        edges->InsertNextId(edgeId);
        }
      }
    }
}

// ----------------------
// UpdateEdgeData
// ----------------------
void vtkSVLocalQuadricDecimation::UpdateEdgeData(vtkIdType pt0Id, vtkIdType pt1Id)
{
  vtkIdList *changedEdges = vtkIdList::New();
  vtkIdType i, edgeId, edge[2];
  double cost;

  // Find all edges with exactly either of these 2 endpoints.
  this->FindAffectedEdges(pt0Id, pt1Id, changedEdges);

  // Reset the endpoints for these edges to reflect the new point from the
  // collapsed edge.
  // Add these new edges to the edge table.
  // Remove the the changed edges from the priority queue.
  for (i = 0; i < changedEdges->GetNumberOfIds(); i++)
    {
    edge[0] = this->EndPoint1List->GetId(changedEdges->GetId(i));
    edge[1] = this->EndPoint2List->GetId(changedEdges->GetId(i));

    // Remove all affected edges from the priority queue.
    // This does not include collapsed edge.
    this->EdgeCosts->DeleteId(changedEdges->GetId(i));

    // Determine the new set of edges
    if (edge[0] == pt1Id)
      {
      if (this->Edges->IsEdge(edge[1], pt0Id) == -1)
        { // The edge will be completely new, add it.
        edgeId = this->Edges->GetNumberOfEdges();
        this->Edges->InsertEdge(edge[1], pt0Id, edgeId);
        this->EndPoint1List->InsertId(edgeId, edge[1]);
        this->EndPoint2List->InsertId(edgeId, pt0Id);
        // Compute cost (target point/data) and add to priority cue.
        if (this->AttributeErrorMetric)
          {
          cost = this->ComputeCost2(edgeId, this->TempX);
          }
        else
          {
          cost = this->ComputeCost(edgeId, this->TempX);
          }
        this->EdgeCosts->Insert(cost, edgeId);
        this->TargetPoints->InsertTuple(edgeId, this->TempX);
        }
      }
    else if (edge[1] == pt1Id)
      { // The edge will be completely new, add it.
      if (this->Edges->IsEdge(edge[0], pt0Id) == -1)
        {
        edgeId = this->Edges->GetNumberOfEdges();
        this->Edges->InsertEdge(edge[0], pt0Id, edgeId);
        this->EndPoint1List->InsertId(edgeId, edge[0]);
        this->EndPoint2List->InsertId(edgeId, pt0Id);
        // Compute cost (target point/data) and add to priority cue.
        if (this->AttributeErrorMetric)
          {
          cost = this->ComputeCost2(edgeId, this->TempX);
          }
        else
          {
          cost = this->ComputeCost(edgeId, this->TempX);
          }
        this->EdgeCosts->Insert(cost, edgeId);
        this->TargetPoints->InsertTuple(edgeId, this->TempX);
        }
      }
    else
      { // This edge already has one point as the merged point.
      if (this->AttributeErrorMetric)
        {
        cost = this->ComputeCost2(changedEdges->GetId(i), this->TempX);
        }
      else
        {
        cost = this->ComputeCost(changedEdges->GetId(i), this->TempX);
        }
      this->EdgeCosts->Insert(cost, changedEdges->GetId(i));
      this->TargetPoints->InsertTuple(changedEdges->GetId(i), this->TempX);
      }
    }

  changedEdges->Delete();
  return;
}

// ----------------------
// ComputeCost
// ----------------------
double vtkSVLocalQuadricDecimation::ComputeCost(vtkIdType edgeId, double *x)
{
  static const double errorNumber = 1e-10;
  double temp[3], A[3][3], b[3];
  vtkIdType pointIds[2];
  double cost = 0.0;
  double *index;
  int i, j;
  double newPoint [4];
  double v[3],  c, norm, normTemp,  temp2[3];
  double pt1[3], pt2[3];

  pointIds[0] = this->EndPoint1List->GetId(edgeId);
  pointIds[1] = this->EndPoint2List->GetId(edgeId);

  for (i = 0; i < 11 + 4 * this->NumberOfComponents; i++)
    {
    this->TempQuad[i] = this->ErrorQuadrics[pointIds[0]].Quadric[i] +
      this->ErrorQuadrics[pointIds[1]].Quadric[i];
    }

  A[0][0] = this->TempQuad[0];
  A[0][1] = A[1][0] = this->TempQuad[1];
  A[0][2] = A[2][0] = this->TempQuad[2];
  A[1][1] = this->TempQuad[4];
  A[1][2] = A[2][1] = this->TempQuad[5];
  A[2][2] = this->TempQuad[7];

  b[0] = -this->TempQuad[3];
  b[1] = -this->TempQuad[6];
  b[2] = -this->TempQuad[8];

  norm = vtkMath::Norm(A[0]);
  normTemp = vtkMath::Norm(A[1]);
  norm = norm > normTemp ? norm : normTemp;
  normTemp = vtkMath::Norm(A[2]);
  norm = norm > normTemp ? norm : normTemp;

  if (fabs(vtkMath::Determinant3x3(A))/(norm*norm*norm) >  errorNumber)
    {
    // it would be better to use the normal of the matrix to test singularity??
    vtkMath::LinearSolve3x3(A, b, x);
    vtkMath::Multiply3x3(A,x,temp);
    // error too high, backup plans
    }
  else
    {
    // cheapest point along the edge
    this->Mesh->GetPoints()->GetPoint(pointIds[0], pt1);
    this->Mesh->GetPoints()->GetPoint(pointIds[1], pt2);
    v[0] = pt2[0] - pt1[0];
    v[1] = pt2[1] - pt1[1];
    v[2] = pt2[2] - pt1[2];

    // equation for the edge pt1 + c * v
    // attempt least squares fit for c for A*(pt1 + c * v) = b
    vtkMath::Multiply3x3(A,v,temp2);
    if (vtkMath::Dot(temp2, temp2) > errorNumber)
      {
      vtkMath::Multiply3x3(A,pt1,temp);
      for (i = 0; i < 3; i++)
        temp[i] = b[i] - temp[i];
      c = vtkMath::Dot(temp2, temp) / vtkMath::Dot(temp2, temp2);
      for (i = 0; i < 3; i++)
        x[i] = pt1[i]+c*v[i];
      }
    else
      {
      // use mid point
      // might want to change to best of mid and end points??
      for (i = 0; i < 3; i++)
        {
        x[i] = 0.5*(pt1[i]+pt2[i]);
        }
      }
    }

  newPoint[0] = x[0];
  newPoint[1] = x[1];
  newPoint[2] = x[2];
  newPoint[3] = 1;

  // Compute the cost
  // x'*quad*x
  index = this->TempQuad;
  for (i = 0; i < 4; i++)
    {
    cost += (*index++)*newPoint[i]*newPoint[i];
    for (j = i +1; j < 4; j++)
      {
      cost += 2.0*(*index++)*newPoint[i]*newPoint[j];
      }
    }

  return cost;
}


// ----------------------
// ComputeCost2
// ----------------------
double vtkSVLocalQuadricDecimation::ComputeCost2(vtkIdType edgeId, double *x)
{
  // this function is so ugly because the functionality of converting an QEM
  // into a dence matrix was not extracted into a separate function and
  // neither was multiplication and some other matrix and vector primitives
  static const double errorNumber = 1e-10;
  vtkIdType pointIds[2];
  double cost = 0.0;
  int i, j;
  int solveOk;

  pointIds[0] = this->EndPoint1List->GetId(edgeId);
  pointIds[1] = this->EndPoint2List->GetId(edgeId);

  for (i = 0; i < 11 + 4 * this->NumberOfComponents; i++)
    {
    this->TempQuad[i] = this->ErrorQuadrics[pointIds[0]].Quadric[i] +
      this->ErrorQuadrics[pointIds[1]].Quadric[i];
    }

  // copy the temp quad into TempA
  // converting from the sparce matrix format into a dence
  this->TempA[0][0] = this->TempQuad[0];
  this->TempA[0][1] = this->TempA[1][0] = this->TempQuad[1];
  this->TempA[0][2] = this->TempA[2][0] = this->TempQuad[2];
  this->TempA[1][1] = this->TempQuad[4];
  this->TempA[1][2] = this->TempA[2][1] = this->TempQuad[5];
  this->TempA[2][2] = this->TempQuad[7];

  this->TempB[0] = -this->TempQuad[3];
  this->TempB[1] = -this->TempQuad[6];
  this->TempB[2] = -this->TempQuad[8];

  for (i = 3; i < 3 +  this->NumberOfComponents; i++)
    {
    this->TempA[0][i] = this->TempA[i][0] = this->TempQuad[11+4*(i-3)];
    this->TempA[1][i] = this->TempA[i][1] = this->TempQuad[11+4*(i-3)+1];
    this->TempA[2][i] = this->TempA[i][2] = this->TempQuad[11+4*(i-3)+2];
    this->TempB[i] = -this->TempQuad[11+4*(i-3)+3];
    }

  for (i = 3; i < 3 +  this->NumberOfComponents; i++)
    {
    for (j = 3; j < 3 +  this->NumberOfComponents; j++)
      {
      if (i == j)
        {
        this->TempA[i][j] = this->TempQuad[10];
        }
      else
        {
        this->TempA[i][j] = 0;
        }
      }
    }

  for (i = 0; i < 3 + this->NumberOfComponents; i++)
    {
    x[i] = this->TempB[i];
    }

  // solve A*x = b
  // this clobers A
  // need to develop a quality of the solution test??
  solveOk = vtkMath::SolveLinearSystem(this->TempA, x, 3 +  this->NumberOfComponents);

  // need to copy back into A
  this->TempA[0][0] = this->TempQuad[0];
  this->TempA[0][1] = this->TempA[1][0] = this->TempQuad[1];
  this->TempA[0][2] = this->TempA[2][0] = this->TempQuad[2];
  this->TempA[1][1] = this->TempQuad[4];
  this->TempA[1][2] = this->TempA[2][1] = this->TempQuad[5];
  this->TempA[2][2] = this->TempQuad[7];

  for (i = 3; i < 3 +  this->NumberOfComponents; i++)
    {
    this->TempA[0][i] = this->TempA[i][0] = this->TempQuad[11+4*(i-3)];
    this->TempA[1][i] = this->TempA[i][1] = this->TempQuad[11+4*(i-3)+1];
    this->TempA[2][i] = this->TempA[i][2] = this->TempQuad[11+4*(i-3)+2];
    }

  for (i = 3; i < 3 +  this->NumberOfComponents; i++)
    {
    for (j = 3; j < 3 +  this->NumberOfComponents; j++)
      {
      if (i == j)
        {
        this->TempA[i][j] = this->TempQuad[10];
        }
      else
        {
        this->TempA[i][j] = 0;
        }
      }
    }

  // check for failure to solve the system
  if (!solveOk)
    {
    // cheapest point along the edge
    // this should not frequently occur, so I am using dynamic allocation
    double *pt1 = new double [3+this->NumberOfComponents];
    double *pt2 = new double [3+this->NumberOfComponents];
    double *v = new double [3+this->NumberOfComponents];
    double *temp = new double [3+this->NumberOfComponents];
    double *temp2 = new double [3+this->NumberOfComponents];
    double d = 0;
    double c = 0;

    this->GetPointAttributeArray(pointIds[0], pt1);
    this->GetPointAttributeArray(pointIds[1], pt2);
    for (i = 0; i < 3+this->NumberOfComponents; ++i)
      {
      v[i] = pt2[i] - pt1[i];
      }

    // equation for the edge pt1 + c * v
    // attempt least squares fit for c for A*(pt1 + c * v) = b
    // temp2 = A*v
    for (i = 0; i < 3 + this->NumberOfComponents; ++i)
      {
      temp2[i] = 0;
      for (j = 0; j < 3 + this->NumberOfComponents; ++j)
        {
        temp2[i] += this->TempA[i][j]*v[j];
        }
      }

    // c = v dot v
    for (i = 0; i <  3 + this->NumberOfComponents; ++i)
      {
      d += temp2[i]*temp2[i];
      }

    if ( d > errorNumber)
      {
      // temp = A*pt1
      for (i = 0; i < 3 + this->NumberOfComponents; ++i)
        {
        temp[i] = 0;
        for (j = 0; j < 3 + this->NumberOfComponents; ++j)
          {
          temp[i] += this->TempA[i][j]*pt1[j];
          }
        }

      for (i = 0; i < 3 + this->NumberOfComponents; i++)
        {
        temp[i] = this->TempB[i] - temp[i];
        }

      for (i = 0; i < 3 + this->NumberOfComponents; i++)
        {
        c += temp2[i]*temp[i];
        }
      c = c/d;

      for (i = 0; i < 3  + this->NumberOfComponents; i++)
        {
        x[i] = pt1[i]+c*v[i];
        }
      }
    else
      {
      // use mid point
      // might want to change to best of mid and end points??
      for (i = 0; i < 3  + this->NumberOfComponents; i++)
        {
        x[i] = 0.5*(pt1[i]+pt2[i]);
        }
      }
    delete[] pt1;
    delete[] pt2;
    delete[] v;
    delete[] temp;
    delete[] temp2;
    }

  // Compute the cost
  // x'*A*x - 2*b*x + d
  for (i = 0; i < 3+this->NumberOfComponents; i++)
    {
    cost += this->TempA[i][i]*x[i]*x[i];
    for (j = i+1; j < 3+this->NumberOfComponents; j++)
      {
      cost += 2.0*this->TempA[i][j]*x[i]*x[j];
      }
    }
  for (i = 0; i < 3+this->NumberOfComponents; i++)
    {
    cost -=  2.0 * this->TempB[i]*x[i];
    }

  cost += this->TempQuad[9];

  return cost;
}


// ----------------------
// CollapseEdge
// ----------------------
int vtkSVLocalQuadricDecimation::CollapseEdge(vtkIdType pt0Id, vtkIdType pt1Id)
{
  int j, numDeleted=0;
  vtkIdType i, npts, cellId;
  const vtkIdType *pts;

  this->Mesh->GetPointCells(pt0Id, this->CollapseCellIds);
  for (i = 0; i < this->CollapseCellIds->GetNumberOfIds(); i++)
    {
    cellId = this->CollapseCellIds->GetId(i);
    this->Mesh->GetCellPoints(cellId, npts, pts);
    for (j = 0; j < 3; j++)
      {
      if (pts[j] == pt1Id)
	{
	this->Mesh->RemoveCellReference(cellId);
	this->Mesh->DeleteCell(cellId);
	numDeleted++;
	}
      }
    }

  this->Mesh->GetPointCells(pt1Id, this->CollapseCellIds);
  this->Mesh->ResizeCellList(pt0Id, this->CollapseCellIds->GetNumberOfIds());
  for (i=0; i < this->CollapseCellIds->GetNumberOfIds(); i++)
    {
    cellId = this->CollapseCellIds->GetId(i);
    this->Mesh->GetCellPoints(cellId, npts, pts);
    // making sure we don't already have the triangle we're about to
    // change this one to
    if ((pts[0] == pt1Id && this->Mesh->IsTriangle(pt0Id, pts[1], pts[2])) ||
	(pts[1] == pt1Id && this->Mesh->IsTriangle(pts[0], pt0Id, pts[2])) ||
	(pts[2] == pt1Id && this->Mesh->IsTriangle(pts[0], pts[1], pt0Id)))
      {
      this->Mesh->RemoveCellReference(cellId);
      this->Mesh->DeleteCell(cellId);
      numDeleted++;
      }
    else
      {
      this->Mesh->AddReferenceToCell(pt0Id, cellId);
      this->Mesh->ReplaceCellPoint(cellId, pt1Id, pt0Id);
      this->changedPoint[pt0Id] = pt1Id;
      }
    }
  this->Mesh->DeletePoint(pt1Id);

  return numDeleted;
}


// ----------------------
// TrianglePlaneCheck
// ----------------------
/// \details triangle t0, t1, t2 and point x
/// determins if t0 and x are on the same side of the plane defined by
/// t1 and t2, and parallel to the normal of the triangle
int vtkSVLocalQuadricDecimation::TrianglePlaneCheck(const double t0[3],
                                             const double t1[3],
                                             const double t2[3],
                                             const double *x) {
  double e0[3], e1[3], n[3], e2[3];
  double c;
  int i;

  for (i = 0; i < 3; i++)
    {
    e0[i] = t2[i] - t1[i];
    }
  for (i = 0; i < 3; i++)
    {
    e1[i] = t0[i] - t1[i];
    }

  // projection of e0 onto e1
  c = vtkMath::Dot(e0,e1)/(e0[0]*e0[0]+e0[1]*e0[1]+e0[2]*e0[2]);
  for (i = 0; i < 3; i++)
    {
    n[i] = e1[i] - c*e0[i];
    }

  for ( i = 0; i < 3; i++)
    {
    e2[i] = x[i] - t1[i];
    }

  vtkMath::Normalize(n);
  vtkMath::Normalize(e2);
  if (vtkMath::Dot(n, e2) > 1e-5)
    {
    return 1;
    }
  else
    {
    return 0;
    }
}

// ----------------------
// IsGoodPlacement
// ----------------------
int vtkSVLocalQuadricDecimation::IsGoodPlacement(vtkIdType pt0Id, vtkIdType pt1Id,
const double *x)
{
  vtkIdType ncells, i;
  vtkIdType npts, ptId, *cells;
  const vtkIdType *pts;
  double pt1[3], pt2[3], pt3[3];

  this->Mesh->GetPointCells(pt0Id, ncells, cells);
  for (i = 0; i < ncells; i++) {
  this->Mesh->GetCellPoints(cells[i], npts, pts);
  // assume triangle
  if (pts[0] != pt1Id && pts[1] != pt1Id && pts[2] != pt1Id)
    {
    for (ptId = 0; ptId < 3; ptId++)
      {
      if (pts[ptId] == pt0Id)
        {
        this->Mesh->GetPoint(pts[ptId], pt1);
        this->Mesh->GetPoint(pts[(ptId+1)%3], pt2);
        this->Mesh->GetPoint(pts[(ptId+2)%3], pt3);
        if(!this->TrianglePlaneCheck(pt1, pt2, pt3, x))
          {
          return 0;
          }
        }
      }
    }
  }

  this->Mesh->GetPointCells(pt1Id, ncells, cells);
  for (i = 0; i < ncells; i++)
    {
    this->Mesh->GetCellPoints(cells[i], npts, pts);
    // assume triangle
    if (pts[0] != pt0Id && pts[1] != pt0Id && pts[2] != pt0Id)
      {
      for (ptId = 0; ptId < 3; ptId++)
        {
        if (pts[ptId] == pt1Id)
          {
          this->Mesh->GetPoint(pts[ptId], pt1);
          this->Mesh->GetPoint(pts[(ptId+1)%3], pt2);
          this->Mesh->GetPoint(pts[(ptId+2)%3], pt3);
          if(!this->TrianglePlaneCheck(pt1, pt2, pt3, x))
            {
            return 0;
            }
          }
        }
      }
    }

  return 1;
}


// ----------------------
// ComputeNumberOfComponents
// ----------------------
void vtkSVLocalQuadricDecimation::ComputeNumberOfComponents(void)
{
  vtkPointData *pd = this->Mesh->GetPointData();
  int i, j;
  double range[2], maxRange=0.0;

  this->NumberOfComponents = 0;
  pd->CopyAllOff();

  for (i = 0; i < 6; i++)
    {
    this->AttributeComponents[i] = 0;
    this->AttributeScale[i] = 1.0;
    }

  // Scalar attributes
  if (pd->GetScalars() != nullptr && this->ScalarsAttribute)
    {
    for (j = 0; j < pd->GetScalars()->GetNumberOfComponents(); j++)
      {
      pd->GetScalars()->GetRange(range, j);
      maxRange = (maxRange < (range[1] - range[0]) ?
                  (range[1] - range[0]) : maxRange);
      }
    if (maxRange != 0.0)
      {
      this->NumberOfComponents +=  pd->GetScalars()->GetNumberOfComponents();
      pd->CopyScalarsOn();
      this->AttributeScale[0] = this->ScalarsWeight/maxRange;
      maxRange = 0.0;
      }
    vtkDebugMacro("scalars "<< this->NumberOfComponents << " "
                  << this->AttributeScale[0]);
    }
  this->AttributeComponents[0] = this->NumberOfComponents;

  // Vector attributes
  if (pd->GetVectors() != nullptr && this->VectorsAttribute)
    {
    for (j = 0; j < pd->GetVectors()->GetNumberOfComponents(); j++)
      {
      pd->GetVectors()->GetRange(range, j);
      maxRange = (maxRange < (range[1] - range[0]) ?
                  (range[1] - range[0]) : maxRange);
      }
    if (maxRange != 0.0)
      {
      this->NumberOfComponents += pd->GetVectors()->GetNumberOfComponents();
      pd->CopyVectorsOn();
      this->AttributeScale[1] = this->VectorsWeight/maxRange;
      maxRange = 0.0;
      }
    vtkDebugMacro("vectors "<< this->NumberOfComponents << " "
                  << this->AttributeScale[1]);
    }
  this->AttributeComponents[1] = this->NumberOfComponents;

  // Normals attributes -- normals are assumed normalized
  if (pd->GetNormals() != nullptr && this->NormalsAttribute)
    {
    this->NumberOfComponents += 3;
    pd->CopyNormalsOn();
    this->AttributeScale[2] = 0.5*this->NormalsWeight;
    vtkDebugMacro("normals "<< this->NumberOfComponents << " "
                  << this->AttributeScale[2]);
    }
  this->AttributeComponents[2] = this->NumberOfComponents;

  // Texture coords attributes
  if (pd->GetTCoords() != nullptr && this->TCoordsAttribute)
    {
    for (j = 0; j < pd->GetTCoords()->GetNumberOfComponents(); j++)
      {
      pd->GetTCoords()->GetRange(range, j);
      maxRange = (maxRange < (range[1] - range[0]) ?
                  (range[1] - range[0]) : maxRange);
      }
    if (maxRange != 0.0)
      {
      this->NumberOfComponents += pd->GetTCoords()->GetNumberOfComponents();
      pd->CopyTCoordsOn();
      this->AttributeScale[3] = this->TCoordsWeight/maxRange;
      maxRange = 0.0;
      }
    vtkDebugMacro("tcoords "<< this->NumberOfComponents << " "
                  << this->AttributeScale[3]);
    }
  this->AttributeComponents[3] = this->NumberOfComponents;

  // Tensors attributes
  if (pd->GetTensors() != nullptr && this->TensorsAttribute)
    {
    for (j = 0; j < 9; j++)
      {
      pd->GetTensors()->GetRange(range, j);
      maxRange = (maxRange < (range[1] - range[0]) ?
                  (range[1] - range[0]) : maxRange);
      }
    if (maxRange != 0.0)
      {
      this->NumberOfComponents += 9;
      pd->CopyTensorsOn();
      this->AttributeScale[4] = this->TensorsWeight/maxRange;
      }
    vtkDebugMacro("tensors "<< this->NumberOfComponents << " "
                  << this->AttributeScale[4]);
    }
  this->AttributeComponents[4] = this->NumberOfComponents;

  vtkDebugMacro("Number of components: " << this->NumberOfComponents);
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVLocalQuadricDecimation::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);

  os << indent << "Target Reduction: " << this->TargetReduction << "\n";
  os << indent << "Actual Reduction: " << this->ActualReduction << "\n";

  os << indent << "Attribute Error Metric: "
     << (this->AttributeErrorMetric ? "On\n" : "Off\n");
  os << indent << "Scalars Attribute: "
     << (this->ScalarsAttribute ? "On\n" : "Off\n");
  os << indent << "Vectors Attribute: "
     << (this->VectorsAttribute ? "On\n" : "Off\n");
  os << indent << "Normals Attribute: "
     << (this->NormalsAttribute ? "On\n" : "Off\n");
  os << indent << "TCoords Attribute: "
     << (this->TCoordsAttribute ? "On\n" : "Off\n");
  os << indent << "Tensors Attribute: "
     << (this->TensorsAttribute ? "On\n" : "Off\n");

  os << indent << "Scalars Weight: " << this->ScalarsWeight << "\n";
  os << indent << "Vectors Weight: " << this->VectorsWeight << "\n";
  os << indent << "Normals Weight: " << this->NormalsWeight << "\n";
  os << indent << "TCoords Weight: " << this->TCoordsWeight << "\n";
  os << indent << "Tensors Weight: " << this->TensorsWeight << "\n";
}

// ----------------------
// GetDecimateArrays
// ----------------------
int vtkSVLocalQuadricDecimation::GetDecimateArrays(vtkPolyData *object, int type)
{
  vtkIdType i;
  int numArrays;

  // Set array name
  std::string arrayName;
  if (type == 0)
    arrayName = this->DecimatePointArrayName;
  else
    arrayName = this->DecimateCellArrayName;

  // Check if array exists
  int exists = vtkSVGeneralUtils::CheckArrayExists(object, type, arrayName);

  if (exists)
  {
    if (type == 0)
      this->DecimatePointArray = vtkIntArray::SafeDownCast(
	  object->GetPointData()->GetArray(this->DecimatePointArrayName));
    else
      this->DecimateCellArray = vtkIntArray::SafeDownCast(
	  object->GetCellData()->GetArray(this->DecimateCellArrayName));

  }

  return exists;
}

// ----------------------
// CorrectPointData
// ----------------------
void vtkSVLocalQuadricDecimation::CorrectPointData(vtkPolyData *object)
{
  vtkIdType i,j;
  int numPts;
  int numPointArrays = 0;
  vtkNew(vtkPolyData, objectCopy);
  objectCopy->DeepCopy(object);

  //std::cout<<"Setting up"<<endl;
  numPts = object->GetNumberOfPoints();
  numPointArrays = object->GetPointData()->GetNumberOfArrays();
  //std::cout<<"Num Pt Arrays: "<<numPointArrays<<endl;

  for (i=0;i<numPts;i++)
  {
    if (this->changedPoint[i] != -1)
    {
      for (j=0;j<numPointArrays;j++)
	objectCopy->GetPointData()->GetArray(j)->SetTuple(i,
	    objectCopy->GetPointData()->GetArray(j)->GetTuple(
	      this->changedPoint[i]));
    }
  }

  object->DeepCopy(objectCopy);
}

// ----------------------
// SetFixedPoints
// ----------------------
int vtkSVLocalQuadricDecimation::SetFixedPoints(vtkPolyData *object, int numTris)
{
  vtkIdType npts;
  const vtkIdType *pts;
  if (this->UseCellArray)
  {
    numTris = 0;
    for (int i = 0; i <  object->GetNumberOfCells(); i++)
      {
        object->GetCellPoints(i,npts,pts);
        if (this->DecimateCellArray->GetValue(i) != 1)
        {
          for (int j=0;j<npts;j++)
            this->fixedPoint[pts[j]] = 1;
        }
        else
          numTris++;
      }
  }
  if (this->UsePointArray)
  {
    if (!this->UseCellArray)
      numTris = 0;
    for (int i = 0; i <  object->GetNumberOfCells(); i++)
      {
        int fixed=0;
        object->GetCellPoints(i,npts,pts);
        for (int j=0;j<npts;j++)
        {
          if (this->DecimatePointArray->GetValue(pts[j]) != 1)
          {
            if (!this->fixedPoint[pts[j]])
            {
              this->fixedPoint[pts[j]] = 1;
              fixed++;
            }
          }
        }
        if (fixed == 0)
          numTris++;
      }
  }

  return 1;
}
