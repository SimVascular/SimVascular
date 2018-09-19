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

#include "vtkSVPassDataArray.h"

#include "vtkCellData.h"
#include "vtkCellLocator.h"
#include "vtkErrorCode.h"
#include "vtkGenericCell.h"
#include "vtkIdTypeArray.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPointLocator.h"
#include "vtkPolyData.h"
#include "vtkPolygon.h"
#include "vtkSmartPointer.h"
#include "vtkUnstructuredGrid.h"

#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"

#include <iostream>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVPassDataArray);

// ----------------------
// Constructor
// ----------------------
vtkSVPassDataArray::vtkSVPassDataArray()
{
  this->SetNumberOfInputPorts(2);

  this->SourcePd = vtkPolyData::New();
  this->TargetPd = vtkPolyData::New();

  this->PassArrayName = NULL;

  this->PassDataArray = NULL;
  this->NewDataArray  = NULL;

  this->PassDataIsCellData = 0;
  this->PassDataToCellData = 0;

  this->UseCellCentroid = 1;
}

// ----------------------
// Destructor
// ----------------------
vtkSVPassDataArray::~vtkSVPassDataArray()
{
  if (this->SourcePd)
  {
    this->SourcePd->Delete();
    this->SourcePd = NULL;
  }
  if (this->TargetPd)
  {
    this->TargetPd->Delete();
    this->TargetPd = NULL;
  }
  if (this->NewDataArray)
  {
    this->NewDataArray->Delete();
    this->NewDataArray = NULL;
  }

  if (this->PassArrayName != NULL)
  {
    delete [] this->PassArrayName;
    this->PassArrayName = NULL;
  }
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVPassDataArray::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);

  os << indent << "Pass data is cell data: " << this->PassDataIsCellData << "\n";
  os << indent << "Pass data to cell data: " << this->PassDataToCellData << "\n";
  os << indent << "Use cell centroid: " << this->UseCellCentroid << "\n";
  if (this->PassArrayName != NULL)
    os << indent << "Pass array name: " << this->PassArrayName << "\n";
}

// ----------------------
// RequestData
// ----------------------
int vtkSVPassDataArray::RequestData(vtkInformation *vtkNotUsed(request),
                                    vtkInformationVector **inputVector,
                                    vtkInformationVector *outputVector)
{
  // get the input and output
  vtkPolyData *input0 = vtkPolyData::GetData(inputVector[0]);
  vtkPolyData *input1 = vtkPolyData::GetData(inputVector[1]);
  vtkPolyData *output = vtkPolyData::GetData(outputVector);

  this->SourcePd->DeepCopy(input0);
  this->SourcePd->BuildLinks();
  this->TargetPd->DeepCopy(input1);
  this->TargetPd->BuildLinks();

  // Prep work for filter
  if (this->PrepFilter() != SV_OK)
  {
    vtkErrorMacro("Prep of filter failed");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }

  // Run the filter
  if (this->RunFilter() != SV_OK)
  {
    vtkErrorMacro("Could not pass information\n");
    this->SetErrorCode(vtkErrorCode::UserError + 2);
    return SV_ERROR;
  }

  output->DeepCopy(this->TargetPd);
  return SV_OK;
}

// ----------------------
// PrepFilter
// ----------------------
int vtkSVPassDataArray::PrepFilter()
{
  //Get the number of Cells for scalar  allocation
  int numCells0 = this->SourcePd->GetNumberOfCells();
  int numCells1 = this->TargetPd->GetNumberOfCells();
  int numPts0 = this->SourcePd->GetNumberOfPoints();
  int numPts1 = this->TargetPd->GetNumberOfPoints();

  //Check the input to make sure it is there
  if (numCells0 < 1 || numCells1 < 1)
  {
     vtkDebugMacro("No input!");
     return SV_ERROR;
  }

  // Check if array exists on points
  if (this->PassDataIsCellData == 0)
  {
    if (vtkSVGeneralUtils::CheckArrayExists(this->SourcePd, 0, this->PassArrayName) != SV_OK)
    {
      std::cout<<"No Point Array Named "<<this->PassArrayName<<" on surface"<<endl;
      return SV_ERROR;
    }
    // Get data array
    this->PassDataArray = this->SourcePd->GetPointData()->GetArray(this->PassArrayName);
  }

  // Check if array exists on cells
  if (this->PassDataIsCellData == 1)
  {
    if (vtkSVGeneralUtils::CheckArrayExists(this->SourcePd, 1, this->PassArrayName) != SV_OK)
    {
      std::cout<<"No Cell Array Named "<<this->PassArrayName<<" on surface"<<endl;
      return SV_ERROR;
    }
    // Get data array
    this->PassDataArray = this->SourcePd->GetCellData()->GetArray(this->PassArrayName);
  }
  this->NewDataArray  = this->PassDataArray->NewInstance();
  this->NewDataArray->SetName(this->PassArrayName);

  return SV_OK;
}

// ----------------------
// RunFilter
// ----------------------
int vtkSVPassDataArray::RunFilter()
{

  // Pass data to point data
  if (this->PassDataToCellData == 0)
  {
    if (this->PassInformationToPoints(this->SourcePd, this->TargetPd,
                                      this->PassDataIsCellData, this->PassDataArray,
                                      this->NewDataArray) != SV_OK)
    {
      return SV_ERROR;
    }
  }

  // Pass data to point cells
  if (this->PassDataToCellData == 1)
  {
    if (this->PassInformationToCells(this->SourcePd, this->TargetPd,
                                     this->PassDataIsCellData,
                                     this->UseCellCentroid,
                                     this->PassDataArray,
                                     this->NewDataArray) != SV_OK)
    {
      return SV_ERROR;
    }
  }

  return SV_OK;
}

// ----------------------
// PassInformationToPoints
// ----------------------
int vtkSVPassDataArray::PassInformationToPoints(vtkPolyData *sourcePd, vtkPolyData *targetPd,
                                              const int sourceIsCellData, vtkDataArray *sourceDataArray,
                                              vtkDataArray *targetDataArray)
{
  // Number of points
  int numPts   = targetPd->GetNumberOfPoints();
  int numComps = sourceDataArray->GetNumberOfComponents();

  // Set up target array
  targetDataArray->SetNumberOfComponents(numComps);
  targetDataArray->SetNumberOfTuples(numPts);

  // Set up locators!
  vtkNew(vtkCellLocator, cellLocator);
  vtkNew(vtkPointLocator, pointLocator);
  if (sourceIsCellData)
  {
    cellLocator = vtkCellLocator::New();
    cellLocator->SetDataSet(sourcePd);
    cellLocator->BuildLocator();
  }
  else
  {
    pointLocator = vtkPointLocator::New();
    pointLocator->SetDataSet(sourcePd);
    pointLocator->BuildLocator();
  }

  // Loop through points
  vtkNew(vtkGenericCell, genericCell);
  for (int i=0; i<numPts; i++)
  {
    // Get point
    double pt[3];
    targetPd->GetPoint(i, pt);

    // If getting cell data
    if (sourceIsCellData)
    {
      // Use cell locator to get closest cell
      double closestPt[3], distance;
      vtkIdType closestCellId; int subId;
      cellLocator->FindClosestPoint(pt,closestPt,genericCell,closestCellId,
	subId,distance);
      // Loop through comps of data array
      for (int j=0; j<numComps; j++)
      {
        // Set new comp of data array to that of closest cell
        targetDataArray->SetComponent(
          i, j, sourceDataArray->GetComponent(closestCellId, j));
      }
    }
    // If getting point data data
    else
    {
      // Use point locator to get closest point
      int closestPtId = pointLocator->FindClosestPoint(pt);
      // Loop through comps of data array
      for (int j=0; j<numComps; j++)
      {
        // Set new comp of data array to that of closest point
        targetDataArray->SetComponent(
          i, j, sourceDataArray->GetComponent(closestPtId, j));
      }
    }
  }

  // Get the point data and add the new array!
  targetPd->GetPointData()->AddArray(targetDataArray);

  return SV_OK;
}

// ----------------------
// PassInformationToCells
// ----------------------
int vtkSVPassDataArray::PassInformationToCells(vtkPolyData *sourcePd, vtkPolyData *targetPd,
                                              const int sourceIsCellData, const int useCellCentroid, vtkDataArray *sourceDataArray,
                                              vtkDataArray *targetDataArray)
{
  // Number of points
  int numCells = targetPd->GetNumberOfCells();
  int numComps = sourceDataArray->GetNumberOfComponents();

  // Set up target array
  targetDataArray->SetNumberOfComponents(numComps);
  targetDataArray->SetNumberOfTuples(numCells);

  // Set up locators!
  vtkNew(vtkCellLocator, cellLocator);
  vtkNew(vtkPointLocator, pointLocator);
  if (sourceIsCellData)
  {
    cellLocator = vtkCellLocator::New();
    cellLocator->SetDataSet(sourcePd);
    cellLocator->BuildLocator();
  }
  else
  {
    pointLocator = vtkPointLocator::New();
    pointLocator->SetDataSet(sourcePd);
    pointLocator->BuildLocator();
  }

  // Loop through cells
  vtkNew(vtkGenericCell, genericCell);
  for (int i=0; i<numCells; i++)
  {
    // Get cell points
    vtkIdType npts, *pts;
    targetPd->GetCellPoints(i, npts, pts);

    // If using cell centroid, comput the centroid
    double centroid[3];
    vtkNew(vtkPoints, polyPts);
    vtkNew(vtkIdTypeArray, polyPtIds);
    if (useCellCentroid)
    {
      for (int j=0; j<npts; j++)
      {
        polyPtIds->InsertValue(j,j);
        polyPts->InsertNextPoint(targetPd->GetPoint(pts[j]));
      }
      vtkPolygon::ComputeCentroid(polyPtIds,polyPts,centroid);
    }

    // If getting cell data
    if (sourceIsCellData)
    {
      // If using cell centroid, ust the centroid
      double closestPt[3], distance;
      vtkIdType closestCellId; int subId;
      if (useCellCentroid)
      {
        cellLocator->FindClosestPoint(centroid, closestPt, genericCell, closestCellId,
          subId,distance);
      }
      // If not using cell centroid
      else
      {
        // Set up list to hold ids, find closest for each cell point
        vtkNew(vtkIdList, closestIds);
        closestIds->SetNumberOfIds(npts);
        vtkIdType ptClosestCellId;
        // Loop through cell points
        for (int j=0; j<npts; j++)
        {
          // Get point
          double findPt[3];
          targetPd->GetPoint(pts[j], findPt);

          // Use locator to get each closest cell
          cellLocator->FindClosestPoint(findPt, closestPt, genericCell, ptClosestCellId,
            subId,distance);
          closestIds->SetId(j, ptClosestCellId);
        }

        // Get most occuring cell id
        this->GetMostOccuringId(closestIds, closestCellId);
      }

      // Loop through comps and set
      for (int j=0; j<numComps; j++)
      {
        // Set array value
        targetDataArray->SetComponent(
          i, j, sourceDataArray->GetComponent(closestCellId, j));
      }
    }
    // If getting point data
    else
    {
      vtkIdType closestPtId;
      // Use centroid
      if (useCellCentroid)
        closestPtId = pointLocator->FindClosestPoint(centroid);
      // If not using centroid
      else
      {
        // Do for each point of cell
        vtkNew(vtkIdList, closestIds);
        closestIds->SetNumberOfIds(npts);

        // Loop through cell points
        for (int j=0; j<npts; j++)
        {
          // Get point
          double findPt[3];
          targetPd->GetPoint(pts[j], findPt);

          // Get closest point to cell point
          int ptClosestPtId = pointLocator->FindClosestPoint(findPt);
          closestIds->SetId(j, ptClosestPtId);
        }


        // Get most occuring id
        this->GetMostOccuringId(closestIds, closestPtId);
      }

      // Loop through array comps
      for (int j=0; j<numComps; j++)
      {
        // Set Comp value
        targetDataArray->SetComponent(
          i, j, sourceDataArray->GetComponent(closestPtId, j));
      }
    }
  }

  // Get cell data and add array!
  targetPd->GetCellData()->AddArray(targetDataArray);

  return SV_OK;
}

// ----------------------
// GetMostOccuringId
// ----------------------
void vtkSVPassDataArray::GetMostOccuringId(vtkIdList *idList, vtkIdType &output)
{
  int numIds = idList->GetNumberOfIds();

  int max_count = 0;
  int max_val = -1;
  for (int i=0; i<numIds; i++)
  {
    int count = 1;
    for (int j=0; j<numIds; j++)
    {
      if (idList->GetId(i) == idList->GetId(j))
        count++;
    }
    if (count > max_count)
    {
      max_count = count;
      max_val   = idList->GetId(i);
    }
  }

  output = max_val;
}
