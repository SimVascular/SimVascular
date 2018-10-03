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

#include "vtkSVCenterlineParallelTransportVectors.h"

#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"
#include "vtkSVPolycubeGenerator.h"

#include "vtkExecutive.h"
#include "vtkErrorCode.h"
#include "vtkCellArray.h"
#include "vtkCellLocator.h"
#include "vtkPointData.h"
#include "vtkPoints.h"
#include "vtkCellData.h"
#include "vtkIntArray.h"
#include "vtkDoubleArray.h"
#include "vtkLine.h"
#include "vtkMath.h"
#include "vtkSmartPointer.h"
#include "vtkSortDataArray.h"
#include "vtkSmartPointer.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkObjectFactory.h"
#include "vtkPolyDataNormals.h"
#include "vtkTriangle.h"
#include "vtkUnstructuredGrid.h"
#include "vtkVersion.h"

#include "vtkvmtkMath.h"

#include <algorithm>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVCenterlineParallelTransportVectors);

// ----------------------
// Constructor
// ----------------------
vtkSVCenterlineParallelTransportVectors::vtkSVCenterlineParallelTransportVectors()
{
  this->WorkPd = vtkPolyData::New();

  this->CenterlineGraph = vtkSVCenterlineGraph::New();

  this->ParallelTransportVectorArrayName = NULL;
  this->GroupIdsArrayName = NULL;
}

// ----------------------
// Destructor
// ----------------------
vtkSVCenterlineParallelTransportVectors::~vtkSVCenterlineParallelTransportVectors()
{
  if (this->WorkPd != NULL)
  {
    this->WorkPd->Delete();
    this->WorkPd = NULL;
  }
  if (this->CenterlineGraph != NULL)
  {
    this->CenterlineGraph->Delete();
    this->CenterlineGraph = NULL;
  }

  if (this->ParallelTransportVectorArrayName != NULL)
  {
    delete [] this->ParallelTransportVectorArrayName;
    this->ParallelTransportVectorArrayName = NULL;
  }

  if (this->GroupIdsArrayName != NULL)
  {
    delete [] this->GroupIdsArrayName;
    this->GroupIdsArrayName = NULL;
  }
}

// ----------------------
// RequestData
// ----------------------
int vtkSVCenterlineParallelTransportVectors::RequestData(
  vtkInformation *vtkNotUsed(request),
  vtkInformationVector **inputVector,
  vtkInformationVector *outputVector)
{
  // get the input and output
  vtkPolyData *input = vtkPolyData::GetData(inputVector[0]);
  vtkPolyData *output = vtkPolyData::GetData(outputVector);

  this->WorkPd->DeepCopy(input);
  this->WorkPd->BuildLinks();

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
    vtkErrorMacro("Filter failed");
    this->SetErrorCode(vtkErrorCode::UserError + 2);
    return SV_ERROR;
  }

  output->DeepCopy(this->WorkPd);

  return SV_OK;
}

// ----------------------
// PrepFilter
// ----------------------
int vtkSVCenterlineParallelTransportVectors::PrepFilter()
{
  if (this->WorkPd->GetNumberOfPoints() == 0 ||
      this->WorkPd->GetNumberOfCells() == 0)
  {
    vtkErrorMacro(<< "Input does not contain points or cells.");
    return SV_ERROR;
  }

  if (!this->GroupIdsArrayName)
  {
    vtkDebugMacro("Centerline GroupIds Array Name not given, setting to GroupIds");
    this->GroupIdsArrayName = new char[strlen("GroupIds") + 1];
    strcpy(this->GroupIdsArrayName, "GroupIds");
  }

  if (vtkSVGeneralUtils::CheckArrayExists(this->WorkPd, 1, this->GroupIdsArrayName) != SV_OK)
  {
    vtkErrorMacro(<< "GroupIds Array with name specified does not exist");
    return SV_OK;
  }

  if (!this->ParallelTransportVectorArrayName)
  {
    vtkDebugMacro("Centerline parallel transport vectors Array Name not given, setting to ParallelTransportVector");
    this->GroupIdsArrayName = new char[strlen("ParallelTransportVector") + 1];
    strcpy(this->GroupIdsArrayName, "ParallelTransportVector");
  }

  this->CenterlineGraph->SetLines(this->WorkPd);
  this->CenterlineGraph->SetGroupIdsArrayName(this->GroupIdsArrayName);

  if (this->CenterlineGraph->BuildGraph() != SV_OK)
  {
    vtkErrorMacro("Unable to form graph of centerlines");
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// RunFilter
// ----------------------
int vtkSVCenterlineParallelTransportVectors::RunFilter()
{
  int numSegs   = this->CenterlineGraph->NumberOfCells;
  int numPoints = this->WorkPd->GetNumberOfPoints();

  // Set up local arrays
  vtkNew(vtkDoubleArray, localArrayX);
  vtkNew(vtkDoubleArray, localArrayY);
  vtkNew(vtkDoubleArray, localArrayZ);
  localArrayX->SetNumberOfComponents(3);
  localArrayX->SetNumberOfTuples(numPoints);
  localArrayY->SetNumberOfComponents(3);
  localArrayY->SetNumberOfTuples(numPoints);
  localArrayZ->SetNumberOfComponents(3);
  localArrayZ->SetNumberOfTuples(numPoints);
  for (int i=0; i<3; i++)
  {
    localArrayX->FillComponent(i, -1);
    localArrayY->FillComponent(i, -1);
    localArrayZ->FillComponent(i, -1);
  }

  double refVecs[3][3];
  for (int i=0; i<numSegs; i++)
  {
    // Corresponding GCell
    vtkSVCenterlineGCell *gCell = this->CenterlineGraph->GetCell(i);
    fprintf(stdout,"DOING GROUP %d\n", gCell->GroupId);

    // The front direction of segment
    for (int j=0; j<3; j++)
    {
      for (int k=0; k<3; k++)
        refVecs[j][k] = gCell->RefDirs[j][k];
    }

    // cell id in the vtkPolyData
    int cellId = this->WorkPd->GetCellData()->GetArray(
     this->GroupIdsArrayName)->LookupValue(gCell->GroupId);

    // Get Cell points
    vtkIdType npts, *pts;
    this->WorkPd->GetCellPoints(cellId, npts, pts);

    int isTerminating = 0;
    double endVecs[3][3];
    if (gCell->Parent == NULL)
    {
      // found parent, flip around
      isTerminating = 1;

      // Check to make sure not already flipped
      vtkNew(vtkIdList, pointCells);
      this->WorkPd->GetPointCells(pts[0], pointCells);
      if (pointCells->GetNumberOfIds() == 1)
        this->CenterlineGraph->FlipLinePoints(this->WorkPd, cellId);
      this->WorkPd->GetCellPoints(cellId, npts, pts);
    }
    else if (gCell->Children.size() != 0)
    {
      for (int j=0; j<3; j++)
      {
        for (int k=0; k<3; k++)
          endVecs[j][k] = gCell->Children[gCell->DivergingChild]->RefDirs[j][k];
      }
    }
    else
      isTerminating = 1;

    // First vecs
    double tmpX[3];
    double pt0[3], pt1[3];

    if (gCell->Parent == NULL)
    {
      localArrayX->SetTuple(pts[0], refVecs[1]);
      localArrayY->SetTuple(pts[0], refVecs[2]);
      localArrayZ->SetTuple(pts[0], refVecs[0]);
    }

    for (int j=1; j<npts; j++)
    {
      this->WorkPd->GetPoint(pts[j-1], pt0);
      this->WorkPd->GetPoint(pts[j], pt1);

      double checkRefs[3][3];
      for (int k=0; k<3; k++)
        for (int l=0; l<3; l++)
          checkRefs[k][l] = refVecs[k][l];

      if (gCell->Parent == NULL)
        vtkMath::Subtract(pt1, pt0, refVecs[0]);
      else
        vtkMath::Subtract(pt0, pt1, refVecs[0]);
      vtkMath::Normalize(refVecs[0]);

      this->CenterlineGraph->ComputeLocalCoordinateSystem(checkRefs[0], refVecs[0], refVecs[1], tmpX, refVecs[2]);
      for (int k=0; k<3; k++)
        refVecs[1][k] = tmpX[k];

      localArrayX->SetTuple(pts[j], refVecs[1]);
      localArrayY->SetTuple(pts[j], refVecs[2]);
      localArrayZ->SetTuple(pts[j], refVecs[0]);
    }

    if (isTerminating == 0)
    {
      fprintf(stdout,"FIXING GROUP %d FOR NOT TERMINATING BRANCH\n", gCell->GroupId);

      // Get update that needs to happen
      int maxDir;
      double maxDot = -0.1;

      for (int j=0; j<3; j++)
      {
        double compare = fabs(vtkMath::Dot(endVecs[j], refVecs[1]));
        fprintf(stdout,"Dot with Ref %d: %.4f\n", j, compare);
        if (compare > maxDot)
        {
          maxDot = compare;
          maxDir = j;
        }
      }

      double projVec[3];
      for (int j=0; j<3; j++)
        projVec[j] = endVecs[maxDir][j];

      vtkMath::Normalize(projVec);
      double projDot = vtkMath::Dot(refVecs[1], projVec);

      vtkMath::MultiplyScalar(projVec, projDot);
      vtkMath::Normalize(projVec);

      double angleVec1[3];
      vtkMath::Cross(refVecs[1], projVec, angleVec1);
      double updateAngle = 180.0*atan2(vtkMath::Norm(angleVec1), vtkMath::Dot(refVecs[1], projVec))/SV_PI;

      fprintf(stdout,"FULL UPDATE ANGLE %.6f\n", updateAngle);
      if (updateAngle > 45.0)
      {
        fprintf(stdout,"ERROR: Angle cannot be larger than 45\n");
        return SV_ERROR;
      }

      updateAngle = updateAngle * (1./(npts-1));

      fprintf(stdout,"MAX DIR: %d\n", maxDir);
      int updateDir;
      if (maxDir == 1)
      {
        double dotCheck =  vtkMath::Dot(refVecs[1], endVecs[2]);
        fprintf(stdout,"DOT CHECK: %.4f\n", dotCheck);
        if (projDot > 0)
        {
          updateDir = 0;
          if (dotCheck >= 0)
            updateAngle *= -1.0;
        }
        else
        {
          fprintf(stdout,"SHOULD BE HERE\n");
          updateDir = 2;
          if (dotCheck < 0)
            updateAngle *= -1.0;
        }
      }
      else if (maxDir == 2)
      {
        double dotCheck = vtkMath::Dot(refVecs[1], endVecs[1]);
        fprintf(stdout,"DOT CHECK: %.4f\n", dotCheck);
        if (projDot > 0)
        {
          updateDir = 3;
          if (dotCheck < 0)
            updateAngle *= -1.0;
        }
        else
        {
          updateDir = 1;
          if (dotCheck >= 0)
            updateAngle *= -1.0;
        }
      }
      else
      {
        fprintf(stderr,"ERROR: This is incorrect, something wrong in end vectors\n");
        return SV_ERROR;
      }

      fprintf(stdout,"WAHT IS IT AFTER %.6f\n", updateAngle);

      double updateRefVecs[3][3];
      // The front direction of segment
      for (int j=0; j<3; j++)
      {
        for (int k=0; k<3; k++)
          updateRefVecs[j][k] = gCell->RefDirs[j][k];
      }

      //Update now
      double locals[3][3];
      for (int j=1; j<npts; j++)
      {
        this->WorkPd->GetPoint(pts[j-1], pt0);
        this->WorkPd->GetPoint(pts[j], pt1);

        localArrayX->GetTuple(pts[j], locals[1]);
        localArrayY->GetTuple(pts[j], locals[2]);
        localArrayZ->GetTuple(pts[j], locals[0]);
        localArrayZ->GetTuple(pts[j], updateRefVecs[0]);

        double singleUpdateAngle = updateAngle * j;

        double updateVecX[3], updateVecY[3];
        this->CenterlineGraph->RotateVecAroundLine(locals[1], singleUpdateAngle, locals[0], updateRefVecs[1]);
        this->CenterlineGraph->RotateVecAroundLine(locals[2], singleUpdateAngle, locals[0], updateRefVecs[2]);

        localArrayX->SetTuple(pts[j], updateRefVecs[1]);
        localArrayY->SetTuple(pts[j], updateRefVecs[2]);
        localArrayZ->SetTuple(pts[j], updateRefVecs[0]);

      }

      double finalCheck = fabs(vtkMath::Dot(updateRefVecs[1], endVecs[maxDir]));
      fprintf(stdout,"Final check: %.4f\n", finalCheck);
      if (finalCheck < 0.9)
      {
        fprintf(stderr,"ERROR: The vector for %d was not updated to correct ending vector\n", gCell->GroupId);
        fprintf(stderr,"The checking dot is %.6f\n", finalCheck);
        return SV_ERROR;
      }
    }
  }

  std::string localXArrayName = this->ParallelTransportVectorArrayName;
  localXArrayName += "X";
  std::string localYArrayName = this->ParallelTransportVectorArrayName;
  localYArrayName += "Y";
  std::string localZArrayName = this->ParallelTransportVectorArrayName;
  localZArrayName += "Z";


  localArrayX->SetName(localXArrayName.c_str());
  localArrayY->SetName(localYArrayName.c_str());
  localArrayZ->SetName(localZArrayName.c_str());

  this->WorkPd->GetPointData()->AddArray(localArrayX);
  this->WorkPd->GetPointData()->AddArray(localArrayY);
  this->WorkPd->GetPointData()->AddArray(localArrayZ);

  return SV_OK;
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVCenterlineParallelTransportVectors::PrintSelf(ostream& os, vtkIndent indent)
{
  if (this->GroupIdsArrayName != NULL)
    os << indent << "Group ids array name: " << this->GroupIdsArrayName << "\n";
  if (this->ParallelTransportVectorArrayName != NULL)
    os << indent << "Parallel transport vectors array name: " << this->ParallelTransportVectorArrayName << "\n";
}
