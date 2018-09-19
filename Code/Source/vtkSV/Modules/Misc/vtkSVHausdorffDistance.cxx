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

#include "vtkSVHausdorffDistance.h"

#include "vtkCellLocator.h"
#include "vtkDoubleArray.h"
#include "vtkErrorCode.h"
#include "vtkGenericCell.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPolyData.h"
#include "vtkSmartPointer.h"

#include "vtkSVGeneralUtils.h"
#include "vtkSVMathUtils.h"
#include "vtkSVGlobals.h"

#include <iostream>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVHausdorffDistance);

// ----------------------
// Constructor
// ----------------------
vtkSVHausdorffDistance::vtkSVHausdorffDistance()
{
  this->SetNumberOfInputPorts(2);

  this->SourcePd = vtkPolyData::New();
  this->TargetPd = vtkPolyData::New();

  this->DistanceArrayName = NULL;

  this->AverageDistance   = 0.0;
  this->HausdorffDistance = 0.0;
}

// ----------------------
// Destructor
// ----------------------
vtkSVHausdorffDistance::~vtkSVHausdorffDistance()
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
  if (this->DistanceArrayName != NULL)
  {
    delete [] this->DistanceArrayName;
    this->DistanceArrayName = NULL;
  }
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVHausdorffDistance::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);

  if (this->DistanceArrayName != NULL)
  {
    os << indent << "Distance array name: " <<
      this->DistanceArrayName << "\n";
  }
  os << indent << "Average Distance: " << this->AverageDistance << "\n";
  os << indent << "Hausdorff Distance: " << this->HausdorffDistance << "\n";
}

// ----------------------
// RequestData
// ----------------------
int vtkSVHausdorffDistance::RequestData(vtkInformation *vtkNotUsed(request),
                                        vtkInformationVector **inputVector,
                                        vtkInformationVector *outputVector)
{
  // Get the input and output
  vtkPolyData *input0 = vtkPolyData::GetData(inputVector[0]);
  vtkPolyData *input1 = vtkPolyData::GetData(inputVector[1]);
  vtkPolyData *output = vtkPolyData::GetData(outputVector);

  // Get the number of Polys for scalar  allocation
  int numPolys0 = input0->GetNumberOfPolys();
  int numPolys1 = input1->GetNumberOfPolys();
  int numPts0 = input0->GetNumberOfPoints();
  int numPts1 = input1->GetNumberOfPoints();

  // Check the input to make sure it is there
  if (numPts0 < 1 || numPts1 < 1)
  {
    vtkDebugMacro("No input!");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }
  this->SourcePd->DeepCopy(input0);
  this->TargetPd->DeepCopy(input1);

  if (this->PrepFilter() != SV_OK)
  {
    vtkErrorMacro("Error in prepping filter\n");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }

  if (this->RunFilter() != SV_OK)
  {
    vtkErrorMacro("Error in running filter\n");
    this->SetErrorCode(vtkErrorCode::UserError + 2);
    return SV_ERROR;
  }

  output->DeepCopy(this->TargetPd);
  return SV_OK;
}

// ----------------------
// PrepFilter
// ----------------------
int vtkSVHausdorffDistance::PrepFilter()
{
  if (this->DistanceArrayName == NULL)
  {
    vtkDebugMacro("Distance Array Name not given, setting to Distance");
    this->DistanceArrayName = new char[strlen("Distance") + 1];
    strcpy(this->DistanceArrayName, "Distance");
  }

  return SV_OK;
}

// ----------------------
// RunFilter
// ----------------------
int vtkSVHausdorffDistance::RunFilter()
{
  // Set up destination array to contain point-wise distances
  int numPoints = this->TargetPd->GetNumberOfPoints();
  vtkNew(vtkDoubleArray, distances);
  distances->SetNumberOfComponents(1);
  distances->SetNumberOfTuples(numPoints);
  distances->SetName(this->DistanceArrayName);

  // Cell locator using the source pd
  vtkNew(vtkCellLocator, locator);
  locator->SetDataSet(this->SourcePd);
  locator->BuildLocator();

  // Loop through each point in target and get distance
  double maxDistance   = 0.0;
  double minDistance   = VTK_SV_LARGE_DOUBLE;
  double totalDistance = 0.0;
  for (int i=0; i<numPoints; i++)
  {
    double pt[3];
    this->TargetPd->GetPoint(i, pt);

    double closestPt[3];
    vtkIdType closestCell;
    int subId;
    double dist2;
    vtkNew(vtkGenericCell, genericCell);
    locator->FindClosestPoint(pt, closestPt, genericCell, closestCell, subId,
                              dist2);
    double distance = vtkSVMathUtils::Distance(closestPt, pt);

    distances->SetTuple1(i, distance);
    if (distance > maxDistance)
      maxDistance = distance;

    if (distance < minDistance)
      minDistance = distance;

    totalDistance += distance;
  }

  // Add array and update distance information
  this->TargetPd->GetPointData()->AddArray(distances);
  this->AverageDistance   = totalDistance/numPoints;
  this->HausdorffDistance = maxDistance;
  this->MinimumDistance   = minDistance;

  return SV_OK;
}
