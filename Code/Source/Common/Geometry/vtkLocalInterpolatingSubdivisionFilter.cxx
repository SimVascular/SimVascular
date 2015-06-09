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

/** @file vtkLocalInterpolatinSubdivision.cxx
 *  @brief This implements localized subdivision
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 */

#include "vtkLocalInterpolatingSubdivisionFilter.h"

#include "vtkCellArray.h"
#include "vtkCellData.h"
#include "vtkEdgeTable.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkPointData.h"
#include "vtkPolyData.h"


// Construct object with number of subdivisions set to 1.
vtkLocalInterpolatingSubdivisionFilter::vtkLocalInterpolatingSubdivisionFilter()
{
  this->SubdivideCellArrayName = 0;
  this->SubdividePointArrayName = 0;
  this->NumberOfSubdivisions = 1;
  this->UseCellArray = 0;
  this->UsePointArray = 0;
}

int vtkLocalInterpolatingSubdivisionFilter::RequestData(
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

  vtkIdType numPts, numCells;
  int level;
  vtkPoints *outputPts;
  vtkCellArray *outputPolys;
  vtkPointData *outputPD;
  vtkCellData *outputCD;
  vtkIntArray *edgeData;

  numPts=input->GetNumberOfPoints();
  numCells=input->GetNumberOfCells();

  if (numPts < 1 || numCells < 1)
    {
    vtkDebugMacro(<<"No data to interpolate!");
    return 1;
    }

  //
  // Initialize and check input
  //

  vtkPolyData *inputDS = vtkPolyData::New();
  inputDS->CopyStructure (input);
  inputDS->GetPointData()->PassData(input->GetPointData());
  inputDS->GetCellData()->PassData(input->GetCellData());


  // check for triangles in input; if none, stop execution
  inputDS->BuildLinks();
  vtkCellArray *polys = inputDS->GetPolys();
  int hasTris = 0;
  vtkIdType numCellPts = 0, *pts = 0;
  polys->InitTraversal();

  while(polys->GetNextCell(numCellPts, pts))
    {
    if (numCellPts == 3)
      {
      if (inputDS->IsTriangle(pts[0], pts[1], pts[2]))
        {
        hasTris = 1;
        break;
        }
      }
    }

  if (!hasTris)
    {
    vtkWarningMacro( << this->GetClassName() << " only operates on triangles, but this data set has no triangles to operate on.");
    inputDS->Delete();
    return 1;
    }

  for (level = 0; level < this->NumberOfSubdivisions; level++)
    {
    if (this->UseCellArray)
    {
      if (this->GetSubdivideArrays(inputDS,1) != 1)
      {
	vtkErrorMacro("Need cell array on mesh to be able to local subdivide");
	return 0;
      }
    }
    if (this->UsePointArray)
    {
      if (this->GetSubdivideArrays(inputDS,0) != 1)
      {
	vtkErrorMacro("Need cell array on mesh to be able to local subdivide");
	return 0;
      }
    }
    // Generate topology  for the input dataset
    inputDS->BuildLinks();
    numCells = inputDS->GetNumberOfCells ();

    // Copy points from input. The new points will include the old points
    // and points calculated by the subdivision algorithm
    outputPts = vtkPoints::New();
    outputPts->GetData()->DeepCopy(inputDS->GetPoints()->GetData());

    // Copy pointdata structure from input
    outputPD = vtkPointData::New();
    outputPD->CopyAllocate(inputDS->GetPointData(),
                           2*inputDS->GetNumberOfPoints());

    // Copy celldata structure from input
    outputCD = vtkCellData::New();
    outputCD->CopyAllocate(inputDS->GetCellData(),4*numCells);

    // Create triangles
    outputPolys = vtkCellArray::New();
    outputPolys->Allocate(outputPolys->EstimateSize(4*numCells,3));

    // Create an array to hold new location indices
    edgeData = vtkIntArray::New();
    edgeData->SetNumberOfComponents(3);
    edgeData->SetNumberOfTuples(numCells);

    if (this->GenerateSubdivisionPoints (inputDS, edgeData, outputPts, outputPD) == 0)
      {
      outputPts->Delete();
      outputPD->Delete();
      outputCD->Delete();
      outputPolys->Delete();
      inputDS->Delete();
      edgeData->Delete();
      vtkErrorMacro("Subdivision failed.");
      return 0;
      }
    this->GenerateSubdivisionCells (inputDS, edgeData, outputPolys, outputCD);

    // start the next iteration with the input set to the output we just created
    edgeData->Delete();
    inputDS->Delete();
    inputDS = vtkPolyData::New();
    inputDS->SetPoints(outputPts); outputPts->Delete();
    inputDS->SetPolys(outputPolys); outputPolys->Delete();
    inputDS->GetPointData()->PassData(outputPD); outputPD->Delete();
    inputDS->GetCellData()->PassData(outputCD); outputCD->Delete();
    inputDS->Squeeze();
    } // each level

  output->SetPoints(inputDS->GetPoints());
  output->SetPolys(inputDS->GetPolys());
  output->GetPointData()->PassData(inputDS->GetPointData());
  output->GetCellData()->PassData(inputDS->GetCellData());
  inputDS->Delete();

  return 1;
}

int vtkLocalInterpolatingSubdivisionFilter::FindEdge (vtkPolyData *mesh,
                                                 vtkIdType cellId,
                                                 vtkIdType p1, vtkIdType p2,
                                                 vtkIntArray *edgeData,
                                                 vtkIdList *cellIds)
{
  int edgeId = 0;
  int currentCellId = 0;
  int i;
  int numEdges;
  vtkIdType tp1, tp2;
  vtkCell *cell;

  // get all the cells that use the edge (except for cellId)
  mesh->GetCellEdgeNeighbors (cellId, p1, p2, cellIds);

  // find the edge that has the point we are looking for
  for ( i=0; i < cellIds->GetNumberOfIds(); i++)
    {
    currentCellId = cellIds->GetId(i);
    cell = mesh->GetCell(currentCellId);
    numEdges = cell->GetNumberOfEdges();
    tp1 = cell->GetPointId(2);
    tp2 = cell->GetPointId(0);
    for (edgeId=0; edgeId < numEdges; edgeId++)
      {
      if ( (tp1 == p1 && tp2 == p2) ||
           (tp2 == p1 && tp1 == p2))
        {
        // found the edge, return the stored value
        return (int) edgeData->GetComponent(currentCellId,edgeId);
        }
      tp1 = tp2;
      tp2 = cell->GetPointId(edgeId + 1);
      }
    }
  vtkErrorMacro("Edge should have been found... but couldn't find it!!");
  return 0;
}

vtkIdType vtkLocalInterpolatingSubdivisionFilter::InterpolatePosition (
        vtkPoints *inputPts, vtkPoints *outputPts,
        vtkIdList *stencil, double *weights)
{
  double xx[3], x[3];
  int i, j;

  for (j = 0; j < 3; j++)
    {
    x[j] = 0.0;
    }

  for (i = 0; i < stencil->GetNumberOfIds(); i++)
    {
    inputPts->GetPoint(stencil->GetId(i), xx);
    for (j = 0; j < 3; j++)
      {
      x[j] += xx[j] * weights[i];
      }
    }
  return outputPts->InsertNextPoint (x);
}


void vtkLocalInterpolatingSubdivisionFilter::GenerateSubdivisionCells (vtkPolyData *inputDS, vtkIntArray *edgeData, vtkCellArray *outputPolys, vtkCellData *outputCD)
{
  vtkIdType numCells = inputDS->GetNumberOfCells();
  vtkIdType cellId, newId;
  int id;
  vtkIdType npts;
  vtkIdType *pts;
  double edgePts[3];
  vtkIdType newCellPts[3];
  vtkCellData *inputCD = inputDS->GetCellData();

  // Now create new cells from existing points and generated edge points
  for (cellId=0; cellId < numCells; cellId++)
    {
    if ( inputDS->GetCellType(cellId) != VTK_TRIANGLE )
      {
      continue;
      }
    // get the original point ids and the ids stored as cell data
    inputDS->GetCellPoints(cellId, npts, pts);
    edgeData->GetTuple(cellId, edgePts);
    int newPtCount=0;
    for (int i=0;i<npts;i++)
      {
      if (edgePts[i] != -1)
	newPtCount++;
      }

    if (newPtCount == 0)
      {
      //std::cout<<"Old cell! just add old pts"<<endl;
      id = 0;
      newCellPts[id++] = pts[0];
      newCellPts[id++] = pts[1];
      newCellPts[id++] = pts[2];
      newId = outputPolys->InsertNextCell (3, newCellPts);
      outputCD->CopyData (inputCD, cellId, newId);
      }
    else if (newPtCount == 1)
      {
      //std::cout<<"Two bounding edges!!"<<endl;
      if (edgePts[0] != -1)
        {
	  id = 0;
	  newCellPts[id++] = (int) edgePts[0];
	  newCellPts[id++] = pts[1];
	  newCellPts[id++] = pts[2];
	  newId = outputPolys->InsertNextCell (3, newCellPts);
	  outputCD->CopyData (inputCD, cellId, newId);

	  id = 0;
	  newCellPts[id++] = pts[0];
	  newCellPts[id++] = pts[1];
	  newCellPts[id++] = (int) edgePts[0];
	  newId = outputPolys->InsertNextCell (3, newCellPts);
	  outputCD->CopyData (inputCD, cellId, newId);
	}
      else if (edgePts[1] != -1)
        {
	  id = 0;
	  newCellPts[id++] = (int) edgePts[1];
	  newCellPts[id++] = pts[2];
	  newCellPts[id++] = pts[0];
	  newId = outputPolys->InsertNextCell (3, newCellPts);
	  outputCD->CopyData (inputCD, cellId, newId);

	  id = 0;
	  newCellPts[id++] = pts[1];
	  newCellPts[id++] = pts[2];
	  newCellPts[id++] = (int) edgePts[1];
	  newId = outputPolys->InsertNextCell (3, newCellPts);
	  outputCD->CopyData (inputCD, cellId, newId);
        }
      else 
        {
	  id = 0;
	  newCellPts[id++] = (int) edgePts[2];
	  newCellPts[id++] = pts[0];
	  newCellPts[id++] = pts[1];
	  newId = outputPolys->InsertNextCell (3, newCellPts);
	  outputCD->CopyData (inputCD, cellId, newId);

	  id = 0;
	  newCellPts[id++] = pts[2];
	  newCellPts[id++] = pts[0];
	  newCellPts[id++] = (int) edgePts[2];
	  newId = outputPolys->InsertNextCell (3, newCellPts);
	  outputCD->CopyData (inputCD, cellId, newId);
        }
      }
    else if (newPtCount == 2)
      {
      //std::cout<<"One bounding edge!!"<<endl;
      if (edgePts[0] == -1)
        {
	  id = 0;
	  newCellPts[id++] = (int) edgePts[1];
	  newCellPts[id++] = pts[1];
	  newCellPts[id++] = (int) edgePts[2];
	  newId = outputPolys->InsertNextCell (3, newCellPts);
	  outputCD->CopyData (inputCD, cellId, newId);

	  id = 0;
	  newCellPts[id++] = pts[0];
	  newCellPts[id++] = (int) edgePts[1];
	  newCellPts[id++] = (int) edgePts[2];
	  newId = outputPolys->InsertNextCell (3, newCellPts);
	  outputCD->CopyData (inputCD, cellId, newId);

	  id = 0;
	  newCellPts[id++] = pts[2];
	  newCellPts[id++] = pts[0];
	  newCellPts[id++] = (int) edgePts[2];
	  newId = outputPolys->InsertNextCell (3, newCellPts);
	  outputCD->CopyData (inputCD, cellId, newId);
	}
      else if (edgePts[1] == -1)
        {
	  id = 0;
	  newCellPts[id++] = (int) edgePts[2];
	  newCellPts[id++] = pts[2];
	  newCellPts[id++] = (int) edgePts[0];
	  newId = outputPolys->InsertNextCell (3, newCellPts);
	  outputCD->CopyData (inputCD, cellId, newId);

	  id = 0;
	  newCellPts[id++] = pts[1];
	  newCellPts[id++] = (int) edgePts[2];
	  newCellPts[id++] = (int) edgePts[0];
	  newId = outputPolys->InsertNextCell (3, newCellPts);
	  outputCD->CopyData (inputCD, cellId, newId);

	  id = 0;
	  newCellPts[id++] = pts[0];
	  newCellPts[id++] = pts[1];
	  newCellPts[id++] = (int) edgePts[0];
	  newId = outputPolys->InsertNextCell (3, newCellPts);
	  outputCD->CopyData (inputCD, cellId, newId);
        }
      else 
        {
	  id = 0;
	  newCellPts[id++] = (int) edgePts[0];
	  newCellPts[id++] = pts[0];
	  newCellPts[id++] = (int) edgePts[1];
	  newId = outputPolys->InsertNextCell (3, newCellPts);
	  outputCD->CopyData (inputCD, cellId, newId);

	  id = 0;
	  newCellPts[id++] = pts[2];
	  newCellPts[id++] = (int) edgePts[0];
	  newCellPts[id++] = (int) edgePts[1];
	  newId = outputPolys->InsertNextCell (3, newCellPts);
	  outputCD->CopyData (inputCD, cellId, newId);

	  id = 0;
	  newCellPts[id++] = pts[1];
	  newCellPts[id++] = pts[2];
	  newCellPts[id++] = (int) edgePts[1];
	  newId = outputPolys->InsertNextCell (3, newCellPts);
	  outputCD->CopyData (inputCD, cellId, newId);
        }
      }
    else 
      {
      //std::cout<<"All of cell is subdividing"<<endl;

      id = 0;
      newCellPts[id++] = pts[0];
      newCellPts[id++] = (int) edgePts[1];
      newCellPts[id++] = (int) edgePts[0];
      newId = outputPolys->InsertNextCell (3, newCellPts);
      outputCD->CopyData (inputCD, cellId, newId);

      id = 0;
      newCellPts[id++] = (int) edgePts[1];
      newCellPts[id++] = pts[1];
      newCellPts[id++] = (int) edgePts[2];
      newId = outputPolys->InsertNextCell (3, newCellPts);
      outputCD->CopyData (inputCD, cellId, newId);

      id = 0;
      newCellPts[id++] = (int) edgePts[2];
      newCellPts[id++] = pts[2];
      newCellPts[id++] = (int) edgePts[0];
      newId = outputPolys->InsertNextCell (3, newCellPts);
      outputCD->CopyData (inputCD, cellId, newId);

      id = 0;
      newCellPts[id++] = (int) edgePts[1];
      newCellPts[id++] = (int) edgePts[2];
      newCellPts[id++] = (int) edgePts[0];
      newId = outputPolys->InsertNextCell (3, newCellPts);
      outputCD->CopyData (inputCD, cellId, newId);
      }
    }
}

void vtkLocalInterpolatingSubdivisionFilter::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);

  os << indent << "Number of subdivisions: " << this->NumberOfSubdivisions << endl;
}

int vtkLocalInterpolatingSubdivisionFilter::GetSubdivideArrays(vtkPolyData *object, int type)
{
  vtkIdType i;
  int exists = 0;
  int numArrays;

  if (type == 0)
  {
    numArrays = object->GetPointData()->GetNumberOfArrays();
    for (i=0;i<numArrays;i++)
    {
      if (!strcmp(object->GetPointData()->GetArrayName(i),
	    this->SubdividePointArrayName))
      {
	exists = 1;
      }
    }
  }
  else
  {
    numArrays = object->GetCellData()->GetNumberOfArrays();
    for (i=0;i<numArrays;i++)
    {
      if (!strcmp(object->GetCellData()->GetArrayName(i),
	    this->SubdivideCellArrayName))
      {
	exists = 1;
      }
    }
  }

  if (exists)
  {
    if (type == 0)
      this->SubdividePointArray = vtkIntArray::SafeDownCast(
	  object->GetPointData()->GetArray(this->SubdividePointArrayName));
    else
      this->SubdivideCellArray = vtkIntArray::SafeDownCast(
	  object->GetCellData()->GetArray(this->SubdivideCellArrayName));

  }

  return exists;
}
