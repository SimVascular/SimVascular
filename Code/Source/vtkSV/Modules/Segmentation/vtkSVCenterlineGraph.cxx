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

#include "vtkSVCenterlineGraph.h"

#include "vtkCellArray.h"
#include "vtkCellData.h"
#include "vtkCleanPolyData.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkDoubleArray.h"
#include "vtkGraphToPolyData.h"
#include "vtkIdList.h"
#include "vtkMath.h"
#include "vtkPointData.h"
#include "vtkPoints.h"
#include "vtkSmartPointer.h"
#include "vtkSortDataArray.h"
#include "vtkThreshold.h"
#include "vtkTransform.h"
#include "vtkTransformPolyDataFilter.h"
#include "vtkUnstructuredGrid.h"

#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"
#include "vtkSVMathUtils.h"

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVCenterlineGraph);

// ----------------------
// Constructor
// ----------------------
vtkSVCenterlineGraph::vtkSVCenterlineGraph()
{
  this->NumberOfCells = 0;
  this->NumberOfNodes = 1; // The root
  this->CubeSize = 0.5;
  this->Root = new vtkSVCenterlineGCell(this->NumberOfCells++, 0, RIGHT);

  this->GroupIdsArrayName = NULL;

  this->Lines = NULL;
}

// ----------------------
// Destructor
// ----------------------
vtkSVCenterlineGraph::~vtkSVCenterlineGraph()
{
  if (this->Root != NULL)
  {
    this->Root->Delete();
    this->Root = NULL;
  }
  if (this->Lines != NULL)
  {
    this->Lines->Delete();
    this->Lines = NULL;
  }

  if (this->GroupIdsArrayName != NULL)
  {
    delete [] this->GroupIdsArrayName;
    this->GroupIdsArrayName = NULL;
  }

}

// ----------------------
// Recurse
// ----------------------
int vtkSVCenterlineGraph::Recurse(vtkSVCenterlineGCell *rootGCell, int(*function)(vtkSVCenterlineGCell *currentGCell,
                   void *arg0, void *arg1, void *arg2),
                   void *rec_arg0, void *rec_arg1, void *rec_arg2)
{
  function(rootGCell, rec_arg0, rec_arg1, rec_arg2);
  if (rootGCell->Children.size() != 0)
  {
    for (int i=0; i<rootGCell->Children.size(); i++)
      vtkSVCenterlineGraph::Recurse(rootGCell->Children[i], function, rec_arg0, rec_arg1, rec_arg2);
  }
  return SV_OK;
}

// ----------------------
// PrintGraph
// ----------------------
int vtkSVCenterlineGraph::PrintGraph()
{
  vtkSVCenterlineGraph::Recurse(this->Root, vtkSVCenterlineGraph::PrintGCell, NULL, NULL, NULL);
  return SV_OK;
}

// ----------------------
// PrintGCell
// ----------------------
int vtkSVCenterlineGraph::PrintGCell(vtkSVCenterlineGCell *gCell, void *arg0, void *arg1, void *arg2)
{
  fprintf(stdout, "GCell ID: %d\n", gCell->Id);
  fprintf(stdout, "GCell Group ID: %d\n", gCell->GroupId);
  fprintf(stdout, "Direction: %d\n", gCell->BranchDir);
  if(gCell->Parent != NULL)
    fprintf(stdout, "Parent: %d\n", gCell->Parent->Id);
  else
    fprintf(stdout, "Parent is NULL\n");
  if(gCell->Children.size() != 0)
  {
    for (int i=0; i<gCell->Children.size(); i++)
      fprintf(stdout, "Child %d, Id: %d, GroupId: %d\n", i, gCell->Children[i]->Id, gCell->Children[i]->GroupId);
  }
  fprintf(stdout, "Start Point: %.4f %.4f %.4f\n", gCell->StartPt[0], gCell->StartPt[1], gCell->StartPt[2]);
  fprintf(stdout, "End Point: %.4f %.4f %.4f\n", gCell->EndPt[0], gCell->EndPt[1], gCell->EndPt[2]);
  return SV_OK;
}

// ----------------------
// GetGraphPolyData
// ----------------------
int vtkSVCenterlineGraph::GetGraphPolyData(vtkPolyData *pd)
{
  vtkNew(vtkPoints, newPoints);
  vtkNew(vtkCellArray, newCells);
  vtkNew(vtkIntArray, groupIds);
  groupIds->SetName(this->GroupIdsArrayName);
  vtkSVCenterlineGraph::Recurse(this->Root, vtkSVCenterlineGraph::InsertGCellPoints, newPoints, newCells, groupIds);

  pd->SetPoints(newPoints);
  pd->SetLines(newCells);
  pd->GetCellData()->AddArray(groupIds);

  vtkNew(vtkCleanPolyData, cleaner);
  cleaner->SetInputData(pd);
  cleaner->Update();

  pd->DeepCopy(cleaner->GetOutput());
  pd->BuildLinks();

  return SV_OK;
}

// ----------------------
// InsertGCellPoints
// ----------------------
int vtkSVCenterlineGraph::InsertGCellPoints(vtkSVCenterlineGCell *gCell, void *arg0, void *arg1, void *arg2)
{
  vtkPoints *points =
    reinterpret_cast<vtkPoints*>(arg0);
  int numPoints = points->GetNumberOfPoints();
  points->InsertNextPoint(gCell->StartPt);
  points->InsertNextPoint(gCell->EndPt);

  vtkCellArray *cells =
    reinterpret_cast<vtkCellArray*>(arg1);
  vtkNew(vtkIdList, newIds);
  newIds->InsertNextId(numPoints); newIds->InsertNextId(numPoints+1);
  cells->InsertNextCell(newIds);

  vtkIntArray *groupIds =
    reinterpret_cast<vtkIntArray*>(arg2);
  groupIds->InsertNextValue(gCell->GroupId);

  return SV_OK;
}

// ----------------------
// BuildGraph
// ----------------------
int vtkSVCenterlineGraph::BuildGraph()
{
  fprintf(stdout,"Building graph!!!\n");
  if (this->Lines == NULL)
  {
    vtkErrorMacro("Need to provide the centerlines");
    return SV_ERROR;
  }
  if (!this->GroupIdsArrayName)
  {
    vtkDebugMacro("Centerline GroupIds Array Name not given, setting to GroupIds");
    this->GroupIdsArrayName = new char[strlen("GroupIds") + 1];
    strcpy(this->GroupIdsArrayName, "GroupIds");
  }
  if (vtkSVGeneralUtils::CheckArrayExists(this->Lines, 1, this->GroupIdsArrayName) != SV_OK)
  {
    vtkErrorMacro(<< "GroupIds Array with name specified does not exist on the lines");
    return SV_OK;
  }

  // Check to see if we need to flip the first line
  vtkIdType npts, *pts;
  this->Lines->BuildLinks();
  int cellId = this->Lines->GetCellData()->GetArray(this->GroupIdsArrayName)->LookupValue(this->Root->GroupId);
  this->Lines->GetCellPoints(cellId, npts, pts);

  vtkNew(vtkIdList, point0Cells);
  this->Lines->GetPointCells(pts[0], point0Cells);
  vtkNew(vtkIdList, pointNCells);
  this->Lines->GetPointCells(pts[npts-1], pointNCells);

  if (point0Cells->GetNumberOfIds() > 1 && pointNCells->GetNumberOfIds() > 1)
  {
    fprintf(stdout,"Group 0 must be a centerline that has at least one end of the centerline not touching any other group\n");
    return SV_ERROR;
  }

  if (point0Cells->GetNumberOfIds() > 1)
    this->FlipLinePoints(this->Lines, cellId);


  // Get connecting groups
  std::vector<int> connectingGroups;
  this->GetConnectingLineGroups(this->Root->GroupId, connectingGroups);

  if (connectingGroups.size() == 0)
  {
    // Just only this guy
    fprintf(stdout,"Only one centerline\n");
    //return SV_OK;
  }

  for (int i=0; i<connectingGroups.size(); i++)
  {
    vtkSVCenterlineGCell *newCell = new vtkSVCenterlineGCell(this->NumberOfCells++,
                                                             connectingGroups[i],
                                                             this->Root);
    this->Root->Children.push_back(newCell);
  }

  if (this->Root->Children.size() > 3)
  {
    vtkErrorMacro("GREATER THAN TRIFURCATION IS NOT HANDLED");
    return SV_ERROR;
  }

  this->NumberOfNodes += this->Root->Children.size()+1;

  this->ComputeGlobalReferenceVectors(this->Root);
  if (this->Root->Children.size() != 0)
    this->ComputeBranchReferenceVectors(this->Root);

  for (int i=0; i<this->Root->Children.size(); i++)
  {
    if (this->GrowGraph(this->Root->Children[i]) != SV_OK)
      return SV_ERROR;
  }

  if (this->UpdateBranchReferenceDirections() != SV_OK)
  {
    return SV_ERROR;
  }

  if (this->GetGraphPoints() != SV_OK)
  {
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// GrowGraph
// ----------------------
/**
 * @brief
 * @param *pd
 * @return
 */
int vtkSVCenterlineGraph::GrowGraph(vtkSVCenterlineGCell *parent)
{
  std::vector<int> connectingGroups;
  this->GetConnectingLineGroups(parent->GroupId, connectingGroups);

  int count = 0;
  for (int i=0; i<connectingGroups.size(); i++)
  {
    int groupId = connectingGroups[i];
    int isChild=1;
    for (int j=0; j<parent->Parent->Children.size(); j++)
    {
      if (groupId == parent->Parent->Children[j]->GroupId)
        isChild = 0;
    }
    if (groupId == parent->Parent->GroupId)
      isChild = 0;
    if (isChild)
    {
      vtkSVCenterlineGCell *newCell = new vtkSVCenterlineGCell(this->NumberOfCells++,
                                                               groupId,
                                                               parent);
      parent->Children.push_back(newCell);
      count++;
    }
  }
  if (count > 3)
  {
    vtkErrorMacro("GREATER THAN TRIFURCATION IS NOT HANDLED");
    return SV_ERROR;
  }
  if (count > 0)
  {
    this->NumberOfNodes += parent->Children.size();
    //fprintf(stdout,"What is the child %d\n", parent->Children[0]->GroupId);
    //fprintf(stdout,"What is the child %d\n", parent->Children[1]->GroupId);
    this->ComputeBranchReferenceVectors(parent);

    for (int i=0; i<parent->Children.size(); i++)
    {
      if (this->GrowGraph(parent->Children[i]) != SV_OK)
      {
        return SV_ERROR;
      }
    }
  }
  else
  {
    fprintf(stdout,"Terminating branch\n");
    return SV_OK;
  }

  return SV_OK;
}

// ----------------------
// GetConnectingLineGroups
// ----------------------
int vtkSVCenterlineGraph::GetConnectingLineGroups(const int groupId, std::vector<int> &connectingGroups)
{
  // Get first cell points
  vtkIdType npts, *pts;
  int cellId = this->Lines->GetCellData()->GetArray(this->GroupIdsArrayName)->LookupValue(groupId);
  this->Lines->GetCellPoints(cellId, npts, pts);

  // Get connecting
  vtkNew(vtkIdList, pointCells0);
  vtkNew(vtkIdList, pointCellsN);
  this->Lines->GetPointCells(pts[0], pointCells0);
  this->Lines->GetPointCells(pts[npts-1], pointCellsN);

  for (int i=0; i<pointCells0->GetNumberOfIds(); i++)
  {
    if (pointCells0->GetId(i) != cellId)
    {
      connectingGroups.push_back(this->Lines->GetCellData()->GetArray(
        this->GroupIdsArrayName)->GetTuple1(pointCells0->GetId(i)));
    }
  }
  for (int i=0; i<pointCellsN->GetNumberOfIds(); i++)
  {
    if (pointCellsN->GetId(i) != cellId)
    {
      connectingGroups.push_back(this->Lines->GetCellData()->GetArray(
        this->GroupIdsArrayName)->GetTuple1(pointCellsN->GetId(i)));
    }
  }

  return SV_OK;
}

// ----------------------
// ComputeGlobalReferenceVectors
// ----------------------
/**
 * @brief
 * @param *pd
 * @return
 */
int vtkSVCenterlineGraph::ComputeGlobalReferenceVectors(vtkSVCenterlineGCell *parent)
{
  vtkNew(vtkThreshold, thresholder);
  thresholder->SetInputData(this->Lines);
  thresholder->SetInputArrayToProcess(0, 0, 0, 1, this->GroupIdsArrayName);
  thresholder->ThresholdBetween(parent->GroupId, parent->GroupId);
  thresholder->Update();
  int numPts = thresholder->GetOutput()->GetNumberOfPoints();

  double endPt0[3], endPt1[3];
  if (parent->Parent == NULL && parent->Children.size() == 0)
  {
    thresholder->GetOutput()->GetPoint(1, endPt0);
    thresholder->GetOutput()->GetPoint(0, endPt1);
  }
  else
  {
    thresholder->GetOutput()->GetPoint(numPts-2, endPt0);
    thresholder->GetOutput()->GetPoint(numPts-1, endPt1);
  }
  fprintf(stdout,"End pt 0 is: %.4f %.4f %.4f\n", endPt0[0], endPt0[1], endPt0[2]);
  fprintf(stdout,"End pt 1 is: %.4f %.4f %.4f\n", endPt1[0], endPt1[1], endPt1[2]);
  vtkMath::Subtract(endPt0, endPt1, this->ReferenceVecs[0]);
  vtkMath::Normalize(this->ReferenceVecs[0]);

  int numChildren = parent->Children.size();
  if (numChildren != 0)
  {
    for (int i=0; i<numChildren; i++)
    {
      thresholder->SetInputData(this->Lines);
      thresholder->SetInputArrayToProcess(0, 0, 0, 1, this->GroupIdsArrayName);
      thresholder->ThresholdBetween(parent->Children[i]->GroupId, parent->Children[i]->GroupId);
      thresholder->Update();

      double startPt[3], secondPt[3];
      thresholder->GetOutput()->GetPoint(0, startPt);
      thresholder->GetOutput()->GetPoint(1, secondPt);

      vtkMath::Subtract(secondPt, startPt, parent->Children[i]->BranchVec);
      vtkMath::Normalize(parent->Children[i]->BranchVec);

      // Get angle between vectors
      double angleVec[3];
      vtkMath::Cross(parent->Children[i]->BranchVec, this->ReferenceVecs[0], angleVec);
      parent->Children[i]->RefAngle = atan2(vtkMath::Norm(angleVec), vtkMath::Dot(parent->Children[i]->BranchVec, this->ReferenceVecs[0]));
    }

    double minAngle = VTK_SV_LARGE_DOUBLE;
    double maxAngle = -1.0*VTK_SV_LARGE_DOUBLE;
    int minChild = 0;
    int maxChild = 0;
    for (int i=0; i<numChildren; i++)
    {
      if (parent->Children[i]->RefAngle < minAngle)
      {
        minAngle = parent->Children[i]->RefAngle;
        minChild = i;
      }
      if (parent->Children[i]->RefAngle > maxAngle)
      {
        maxAngle = parent->Children[i]->RefAngle;
        maxChild = i;
      }
      fprintf(stdout,"Vec %d: %.4f %.4f %.4f\n", i, parent->Children[i]->BranchVec[0], parent->Children[i]->BranchVec[1], parent->Children[i]->BranchVec[2]);
      fprintf(stdout,"Angle %d: %4f\n", i, 180*parent->Children[i]->RefAngle/SV_PI);
    }
    vtkMath::Cross(this->ReferenceVecs[0], parent->Children[minChild]->BranchVec, this->ReferenceVecs[2]);
    vtkMath::Normalize(this->ReferenceVecs[2]);

    parent->DivergingChild = minChild;
    parent->AligningChild  = maxChild;

    parent->Children[parent->AligningChild]->IsAlign = 1;
    for (int i=0; i<numChildren; i++)
    {
      if (i != parent->AligningChild)
        parent->Children[i]->IsAlign = 0;
    }
  }
  else
  {
    double xVec[3]; xVec[0] = 1.0; xVec[1] = 0.0; xVec[2] = 0.0;
    vtkMath::Cross(this->ReferenceVecs[0], xVec, this->ReferenceVecs[2]);
    vtkMath::Normalize(this->ReferenceVecs[2]);
  }

  vtkMath::Cross(this->ReferenceVecs[2], this->ReferenceVecs[0], this->ReferenceVecs[1]);
  vtkMath::Normalize(this->ReferenceVecs[1]);
  fprintf(stdout,"Root vec: %.4f %.4f %.4f\n", this->ReferenceVecs[0][0], this->ReferenceVecs[0][1], this->ReferenceVecs[0][2]);

  for (int i=0 ; i<3; i++)
  {
    for (int j=0; j<3; j++)
      parent->RefDirs[i][j] = this->ReferenceVecs[i][j];
  }

  return SV_OK;
}

// ----------------------
// ComputeBranchReferenceVectors
// ----------------------
/**
 * @brief
 * @param *pd
 * @return
 */
int vtkSVCenterlineGraph::ComputeBranchReferenceVectors(vtkSVCenterlineGCell *parent)
{
  //fprintf(stdout,"Child %d of parent %d, dir: %d\n", parent->Children[0]->GroupId, parent->GroupId, parent->Children[0]->BranchDir);
  //fprintf(stdout,"Child %d of parent %d, dir: %d\n", parent->Children[1]->GroupId, parent->GroupId, parent->Children[1]->BranchDir);
  vtkNew(vtkThreshold, thresholder);
  thresholder->SetInputData(this->Lines);
  thresholder->SetInputArrayToProcess(0, 0, 0, 1, this->GroupIdsArrayName);
  thresholder->ThresholdBetween(parent->GroupId, parent->GroupId);
  thresholder->Update();
  int numPts = thresholder->GetOutput()->GetNumberOfPoints();

  double endPt0[3], endPt1[3];
  thresholder->GetOutput()->GetPoint(numPts-2, endPt0);
  thresholder->GetOutput()->GetPoint(numPts-1, endPt1);

  double vec0[3];
  vtkMath::Subtract(endPt0, endPt1, vec0);
  vtkMath::Normalize(vec0);

  double refDirs[3][3];
  for (int i=0; i<3; i++)
    refDirs[0][i] = vec0[i];

  int numChildren = parent->Children.size();
  for (int i=0; i<numChildren; i++)
  {
    thresholder->SetInputData(this->Lines);
    thresholder->SetInputArrayToProcess(0, 0, 0, 1, this->GroupIdsArrayName);
    thresholder->ThresholdBetween(parent->Children[i]->GroupId, parent->Children[i]->GroupId);
    thresholder->Update();

    double startPt[3], secondPt[3];
    thresholder->GetOutput()->GetPoint(0, startPt);
    thresholder->GetOutput()->GetPoint(1, secondPt);

    vtkMath::Subtract(secondPt, startPt, parent->Children[i]->BranchVec);
    vtkMath::Normalize(parent->Children[i]->BranchVec);

    // Get angle between vectors
    double angleVec[3];
    vtkMath::Cross(parent->Children[i]->BranchVec, vec0, angleVec);
    parent->Children[i]->RefAngle = atan2(vtkMath::Norm(angleVec), vtkMath::Dot(parent->Children[i]->BranchVec, vec0));
  }

  double minAngle = VTK_SV_LARGE_DOUBLE;
  double maxAngle = -1.0*VTK_SV_LARGE_DOUBLE;
  int minChild = 0;
  int maxChild = 0;
  for (int i=0; i<numChildren; i++)
  {
    if (parent->Children[i]->RefAngle < minAngle)
    {
      minAngle = parent->Children[i]->RefAngle;
      minChild = i;
    }
    if (parent->Children[i]->RefAngle >= maxAngle)
    {
      maxAngle = parent->Children[i]->RefAngle;
      maxChild = i;
    }
    fprintf(stdout,"Vec %d: %.4f %.4f %.4f\n", i, parent->Children[i]->BranchVec[0], parent->Children[i]->BranchVec[1], parent->Children[i]->BranchVec[2]);
    fprintf(stdout,"Angle %d: %4f\n", i, 180*parent->Children[i]->RefAngle/SV_PI);
  }

  double vec3[3];
  vtkMath::Cross(vec0, parent->Children[minChild]->BranchVec, vec3);
  vtkMath::Normalize(vec3);

  // Update the reference directions
  vtkMath::Normalize(vec3);
  vtkMath::Cross(vec3, vec0, refDirs[1]);
  vtkMath::Normalize(refDirs[1]);
  for (int i=0; i<3; i++)
    refDirs[2][i] = vec3[i];

  // Now children just in case terminating children
  for (int i=0; i<3; i++)
  {
    for (int j=0; j<3; j++)
    {
      for (int k=0; k<parent->Children.size(); k++)
        parent->Children[k]->RefDirs[i][j] = refDirs[i][j];
    }
  }


  parent->DivergingChild = minChild;
  parent->AligningChild  = maxChild;

  parent->Children[parent->AligningChild]->IsAlign  = 1;
  for (int i=0; i<numChildren; i++)
  {
    if (i != parent->AligningChild)
      parent->Children[i]->IsAlign = 0;
  }

  // Now that we know the diverging child, reorder the branches in an order that
  // traverses around parent branch starting with the diverging branch
  vtkNew(vtkDoubleArray, alignVals);
  vtkNew(vtkIntArray, alignKeys);
  for (int i=0; i<numChildren; i++)
  {
    double checkDot = vtkMath::Dot(refDirs[1], parent->Children[i]->BranchVec);
    fprintf(stdout,"DOTER WITH DIVERGER REFERENCE DIRECTION: %.6f FOR GROUP %d\n", checkDot, parent->Children[i]->GroupId);
    double checkAng = parent->Children[i]->RefAngle;

    if (checkDot < 0)
      checkAng = 2.0*SV_PI - checkAng;
    fprintf(stdout,"ANGLE WITH DIVERGER REFERENCE DIRECTION: %.6f FOR GROUP %d\n", 180.0*checkAng/SV_PI, parent->Children[i]->GroupId);

    alignVals->InsertNextTuple1(checkAng);
    alignKeys->InsertNextTuple1(i);
  }

  // Sort the array
  vtkSortDataArray::Sort(alignVals, alignKeys);

  // Now get temporary vals
  std::vector<vtkSVCenterlineGCell*> tempCells(numChildren);
  for (int i=0; i<numChildren; i++)
    tempCells[i] = parent->Children[i];

  fprintf(stdout,"NEW ORDER\n");
  // And replace
  int newDiverger;
  int newAligner;
  for (int i=0; i<numChildren; i++)
  {
    int newVal = alignKeys->GetTuple1(i);
    parent->Children[i] = tempCells[newVal];
    fprintf(stdout,"CHILD %d IS %d\n", i, parent->Children[i]->GroupId);
    if (parent->DivergingChild == newVal)
      newDiverger = i;
    if (parent->AligningChild == newVal)
      newAligner = i;
  }
  parent->DivergingChild = newDiverger;
  parent->AligningChild  = newAligner;
  //Check to see orientation with respect to reference vectors. Angle that
  //it makes with reference vector determines the input angle to a
  //lookup table that tells us the absolute direction that the new branch
  //node should go
   this->GetInitialBranchDirections(parent);

  return SV_OK;
}

// ----------------------
// GetInitialBranchDirections
// ----------------------
/**
 * @brief
 * @param *pd
 * @return
 */
int vtkSVCenterlineGraph::GetInitialBranchDirections(vtkSVCenterlineGCell *parent)
{
  fprintf(stdout,"Determing initial dirs for parent %d\n", parent->GroupId);
  int numChildren = parent->Children.size();

  // Get initial branch dirs
  for (int i=0; i<numChildren; i++)
  {
    if (i != parent->DivergingChild)
    {
      int maxDir=0;
      double maxDot = -1.0;

      for (int j=1; j<3; j++)
      {
        double compare = fabs(vtkMath::Dot(parent->Children[i]->RefDirs[j], parent->Children[i]->BranchVec));
        fprintf(stdout,"Would like to see compare, dir: %d, dot: %.6f\n", j, compare);
        if (compare > maxDot)
        {
          maxDot = compare;
          maxDir = j;
        }
      }

      double compare1 = fabs(vtkMath::Dot(parent->Children[i]->RefDirs[0], parent->Children[i]->BranchVec));
      fprintf(stdout,"WANT TO SEE HOW IT COMPARES TO 0: %.6f\n", compare1);

      double dotCheck = vtkMath::Dot(parent->Children[i]->RefDirs[maxDir], parent->Children[i]->BranchVec);
      if (numChildren == 2)
      {
        parent->Children[i]->BranchDir = LEFT;
      }
      else
      {
        if (maxDir == 1)
        {
          if (dotCheck > 0)
            parent->Children[i]->BranchDir = RIGHT;
          else
            parent->Children[i]->BranchDir = LEFT;
        }
        else if (maxDir == 2)
        {
          if (dotCheck > 0)
            parent->Children[i]->BranchDir = BACK;
          else
            parent->Children[i]->BranchDir = FRONT;
        }
      }
    }
    else
      parent->Children[i]->BranchDir = RIGHT;
    fprintf(stdout,"Child %d got direction %d\n", parent->Children[i]->GroupId, parent->Children[i]->BranchDir);
  }

  // Trifurcation case, set aligning child to the correct directiono
  if (numChildren >= 3)
  {
    vtkSVCenterlineGCell *brother;
    for (int i=0; i<3; i++)
    {
      if (i != parent->DivergingChild && i != parent->AligningChild)
        brother = parent->Children[i];
    }
    vtkSVCenterlineGCell *diver = parent->Children[parent->DivergingChild];
    vtkSVCenterlineGCell *align = parent->Children[parent->AligningChild];

    if ((brother->BranchDir + diver->BranchDir)%2 == 0)
    {
      if ((align->BranchDir + diver->BranchDir)%2 != 0)
      {
        parent->Children[parent->AligningChild]->BranchDir = RIGHT;
        fprintf(stdout,"Aligning of group id %d set to %d\n", parent->Children[parent->AligningChild]->GroupId, RIGHT);
      }
    }
  }

  return SV_OK;
}


// ----------------------
// GetCell
// ----------------------
vtkSVCenterlineGCell* vtkSVCenterlineGraph::GetCell(const int findId)
{
  if (findId > this->NumberOfCells)
  {
    fprintf(stdout,"Id is larger than number of cells\n");
    return NULL;
  }
  return this->FindId(this->Root, findId);
}

// ----------------------
// FindId
// ----------------------
vtkSVCenterlineGCell* vtkSVCenterlineGraph::FindId(vtkSVCenterlineGCell *lookCell, const int findId)
{
  if (lookCell == NULL)
  {
    return lookCell;
  }
  if (lookCell->Id == findId)
  {
    return lookCell;
  }
  else
  {
    if (lookCell->Children.size() != 0)
    {
      for (int i=0; i<lookCell->Children.size(); i++)
      {
        vtkSVCenterlineGCell* foundCell = this->FindId(lookCell->Children[i], findId);
        if (foundCell != NULL)
          return foundCell;
      }
    }
  }

  return NULL;
}

// ----------------------
// GetCellByGroupId
// ----------------------
vtkSVCenterlineGCell* vtkSVCenterlineGraph::GetCellByGroupId(const int findId)
{
  //if (findId > this->NumberOfCells)
  //{
  //  fprintf(stdout,"Id is larger than number of cells\n");
  //  return NULL;
  //}
  return this->FindGroupId(this->Root, findId);
}


// ----------------------
// FindGroupId
// ----------------------
vtkSVCenterlineGCell* vtkSVCenterlineGraph::FindGroupId(vtkSVCenterlineGCell *lookCell, const int findId)
{
  if (lookCell == NULL)
  {
    return lookCell;
  }
  if (lookCell->GroupId == findId)
  {
    return lookCell;
  }
  else
  {
    if (lookCell->Children.size() != 0)
    {
      for (int i=0; i<lookCell->Children.size(); i++)
      {
        vtkSVCenterlineGCell* foundCell = this->FindGroupId(lookCell->Children[i], findId);
        if (foundCell != NULL)
          return foundCell;
      }
    }
  }

  return NULL;
}

// ----------------------
// GetGraphPoints
// ----------------------
int vtkSVCenterlineGraph::GetGraphPoints()
{
  int numSegs = this->NumberOfCells;

  for (int i=0; i<numSegs; i++)
  {
    // Corresponding GCell
    vtkSVCenterlineGCell *gCell = this->GetCell(i);
    fprintf(stdout,"PROCESSING GRAPH POINTS OF %d\n", gCell->GroupId);

    // cell id in the vtkPolyData
    int cellId = this->Lines->GetCellData()->GetArray(
     this->GroupIdsArrayName)->LookupValue(gCell->GroupId);

    // Get Cell points
    vtkIdType npts, *pts;
    this->Lines->GetCellPoints(cellId, npts, pts);

    double length = 0.0;

    for (int j=1; j<npts; j++)
    {
      double pt0[3], pt1[3];
      this->Lines->GetPoint(pts[j-1], pt0);
      this->Lines->GetPoint(pts[j], pt1);

      length += vtkSVMathUtils::Distance(pt0, pt1);
    }

    // Get end point from new direction
    double minLength;
    this->ComputeMinimumLength(gCell, minLength);
    //fprintf(stdout,"SEE LENGTH: %.6f\n", length);
    //fprintf(stdout,"SEE CALCULATED MIN LENGTH: %.6f\n", minLength);
    if (minLength > length)
      length = minLength;

    vtkSVCenterlineGCell *parent = gCell->Parent;
    if (parent == NULL)
    {
      this->Lines->GetPoint(pts[0], gCell->EndPt);
      double lineDir[3];
      for (int j=0; j<3; j++)
        lineDir[j] = gCell->RefDirs[0][j];
      vtkMath::MultiplyScalar(lineDir, length);
      vtkMath::Add(gCell->EndPt, lineDir, gCell->StartPt);
    }
    else
    {
      for (int j=0; j<3; j++)
        gCell->StartPt[j] = parent->EndPt[j];

      // Get rotation matrix from cross to ref
      double rotateVec[3];
      vtkMath::Subtract(parent->StartPt, parent->EndPt, rotateVec);
      vtkMath::Normalize(rotateVec);

      double crossVec[3];
      double lineDir[3];

      double rotationAngle;
      vtkSVCenterlineGCell *grandParent = parent->Parent;
      if (grandParent == NULL)
      {
        fprintf(stdout,"NO GRANDPARENT\n");
        double parentVec[3];
        vtkMath::Subtract(parent->StartPt, parent->EndPt, parentVec);
        vtkMath::Normalize(parentVec);

        vtkMath::Cross(parentVec, this->ReferenceVecs[1], crossVec);
        vtkMath::Normalize(crossVec);

        if ((parent->BranchDir + gCell->BranchDir)%2 == 0)
        {
          if (parent->BranchDir == gCell->BranchDir)
          {
            fprintf(stdout,"PARENT AND CHILD SAME\n");
          }
          else
          {
            fprintf(stdout,"PARENT AND CHILD OPPOSITE\n");
            for (int j=0; j<3; j++)
              crossVec[j] = -1.0*crossVec[j];
          }
        }
        else
        {
          double tempVec[3];
          vtkMath::Cross(parentVec, crossVec, tempVec);
          vtkMath::Normalize(tempVec);
          if ((parent->BranchDir + 1)%4 == gCell->BranchDir)
          {
            fprintf(stdout,"PARENT PLUS ONE IS CHILD\n");
            for (int j=0; j<3; j++)
              crossVec[j] = 1.0*tempVec[j];
          }
          else
          {
            for (int j=0; j<3; j++)
              crossVec[j] = -1.0*tempVec[j];
            fprintf(stdout,"PARENT PLUS THREE IS CHILD\n");
          }
        }

        int begType, begSplitType;
        if (gCell->GetBeginningType(begType, begSplitType) != SV_OK)
        {
          return SV_ERROR;
        }

        if (begType >= C_TET_0 && begType <= S_TET_3)
        {
          if (begSplitType == BI)
            rotationAngle = 90.0;
          else if (begSplitType == TRI)
          {
            if (gCell->Id == parent->Children[parent->AligningChild]->Id)
              rotationAngle = 180.0;
            else
              rotationAngle = 90.0;
          }
        }
        else
          rotationAngle = 180.0*gCell->RefAngle/SV_PI;
      }
      else
      {
        int begType, begSplitType;
        if (gCell->GetBeginningType(begType, begSplitType) != SV_OK)
        {
          return SV_ERROR;
        }

        int parentBegType, parentBegSplitType;
        if (parent->GetBeginningType(parentBegType, parentBegSplitType) != SV_OK)
        {
          return SV_ERROR;
        }
        int parentEndType, parentEndSplitType;
        if (parent->GetEndType(parentEndType, parentEndSplitType) != SV_OK)
        {
          return SV_ERROR;
        }

        double grandParentVec[3];
        vtkMath::Subtract(grandParent->StartPt, grandParent->EndPt, grandParentVec);
        vtkMath::Normalize(grandParentVec);

        double parentVec[3];
        vtkMath::Subtract(parent->EndPt, parent->StartPt, parentVec);
        vtkMath::Normalize(parentVec);

        //double tempVec[3];
        //vtkMath::Cross(grandParentVec, parentVec, tempVec);
        //vtkMath::Normalize(tempVec);

        //vtkMath::Cross(tempVec, parentVec, crossVec);
        //vtkMath::Normalize(crossVec);

        //double lineDot = vtkMath::Dot(grandParentVec, parentVec);
        //if (lineDot < -1.0 + 1.0e-6 && lineDot > -1.0 - 1.0e-6)
        //{
        //  fprintf(stdout,"IS IN LINE!!!\n");
        //}

        double gDiverVec[3];
        vtkSVCenterlineGCell *gDiver = grandParent->Children[grandParent->DivergingChild];
        vtkMath::Subtract(gDiver->EndPt, gDiver->StartPt, gDiverVec);
        vtkMath::Normalize(gDiverVec);

        vtkMath::Cross(grandParentVec, gDiverVec, crossVec);
        vtkMath::Normalize(crossVec);

        //vtkMath::Cross(tempVec, parentVec, crossVec);
        //vtkMath::Normalize(crossVec);

        //if ((grandParent->BranchDir + gDiver->BranchDir)%2 == 0)
        //{
        //  if (grandParent->BranchDir == gDiver->BranchDir)
        //  {
        //    fprintf(stdout,"GRANDPARENT AND GRANDPARENT DIVER SAME\n");
        //  }
        //  else
        //  {
        //    fprintf(stdout,"GRANDPARENT AND GRANDPARENT DIVER OPPOSITE\n");
        //    for (int j=0; j<3; j++)
        //      crossVec[j] = -1.0*crossVec[j];
        //  }
        //}
        //else
        //{
        //  if ((grandParent->BranchDir + 1)%4 == gDiver->BranchDir)
        //  {
        //    fprintf(stdout,"GRANDPARENT PLUS ONE IS GRANDPARENT DIVER\n");
        //  }
        //  else
        //  {
        //    fprintf(stdout,"GRANDPARENT PLUS THREE IS GRANDPARENT DIVER\n");
        //    for (int j=0; j<3; j++)
        //      crossVec[j] = -1.0*crossVec[j];
        //  }
        //}

        if ((gDiver->BranchDir + parent->BranchDir)%2 == 0)
        {
          if (gDiver->BranchDir == parent->BranchDir)
          {
            fprintf(stdout,"GRANDPARENT DIVER AND PARENT SAME\n");
          }
          else
          {
            fprintf(stdout,"GRANDPARENT DIVER AND PARENT OPPOSITE\n");
            for (int j=0; j<3; j++)
              crossVec[j] = -1.0*crossVec[j];
          }
        }
        else
        {
          double tempVec[3];
          vtkMath::Cross(grandParentVec, crossVec, tempVec);
          vtkMath::Normalize(tempVec);
          for (int j=0; j<3; j++)
            crossVec[j] = tempVec[j];

          if ((gDiver->BranchDir + 1)%4 == parent->BranchDir)
          {
            fprintf(stdout,"GRANDPARENT DIVER PLUS ONE IS PARENT\n");
          }
          else
          {
            fprintf(stdout,"GRANDPARENT DIVER PLUS THREE IS PARENT\n");
            for (int j=0; j<3; j++)
              crossVec[j] = -1.0*crossVec[j];
          }
        }

        if ((parent->BranchDir + gCell->BranchDir)%2 == 0)
        {
          if (parent->BranchDir == gCell->BranchDir)
          {
            fprintf(stdout,"PARENT AND CHILD SAME\n");
          }
          else
          {
            fprintf(stdout,"PARENT AND CHILD OPPOSITE\n");
            for (int j=0; j<3; j++)
              crossVec[j] = -1.0*crossVec[j];
          }
        }
        else
        {
          double tempVec[3];
          vtkMath::Cross(crossVec, parentVec, tempVec);
          vtkMath::Normalize(tempVec);
          for (int j=0; j<3; j++)
            crossVec[j] = tempVec[j];

          if ((parent->BranchDir + 1)%4 == gCell->BranchDir)
          {
            fprintf(stdout,"PARENT PLUS ONE IS CHILD\n");
          }
          else
          {
            fprintf(stdout,"PARENT PLUS THREE IS CHILD\n");
            for (int j=0; j<3; j++)
              crossVec[j] = -1.0*crossVec[j];
          }
        }

        //if ((grandParent->BranchDir + parent->BranchDir)%2 != 0)
        //{
        //  fprintf(stdout,"GRANDPARENT AND PARENT SWITCH\n");

        //  if (parentBegType >= C_TET_0 && parentBegType <= C_TET_3)
        //  {
        //    vtkSVCenterlineGCell *grandParentDiverging = grandParent->Children[grandParent->DivergingChild];
        //    fprintf(stdout,"PARENT IS DIRECTLY IN LINE WITH GRANDPARENT\n");
        //    double divergingVec[3];
        //    vtkMath::Subtract(grandParentDiverging->EndPt, grandParentDiverging->StartPt, divergingVec);

        //    vtkMath::Cross(grandParentVec, divergingVec, crossVec);
        //    vtkMath::Normalize(crossVec);

        //    if ((grandParentDiverging->BranchDir + gCell->BranchDir)%2 == 0)
        //    {
        //      if (grandParentDiverging->BranchDir == gCell->BranchDir)
        //      {
        //        fprintf(stdout,"GRANDPARENT DIVERGING AND CHILD SAME\n");
        //      }
        //      else
        //      {
        //        fprintf(stdout,"GRANDPARENT DIVERGING AND CHILD OPPOSITE\n");
        //        for (int j=0; j<3; j++)
        //          crossVec[j] = -1.0*crossVec[j];
        //      }
        //    }
        //    else
        //    {
        //      vtkMath::Cross(grandParentVec, crossVec, tempVec);
        //      vtkMath::Normalize(tempVec);
        //      for (int j=0; j<3; j++)
        //        crossVec[j] = tempVec[j];
        //      if ((grandParentDiverging->BranchDir + 1)%4 == gCell->BranchDir)
        //      {
        //        fprintf(stdout,"GRANDPARENT DIVERGING PLUS ONE IS CHILD\n");
        //      }
        //      else
        //      {
        //        fprintf(stdout,"GRANDPARENT DIVERGING PLUS THREE IS CHILD\n");
        //        for (int j=0; j<3; j++)
        //          crossVec[j] = -1.0*crossVec[j];
        //      }
        //    }
        //  }
        //  else
        //  {
        //    vtkMath::Cross(parentVec, crossVec, tempVec);
        //    vtkMath::Normalize(tempVec);
        //    for (int j=0; j<3; j++)
        //      crossVec[j] = tempVec[j];

        //    if ((parent->BranchDir + gCell->BranchDir)%2 == 0)
        //    {
        //      if (parent->BranchDir == gCell->BranchDir)
        //      {
        //        fprintf(stdout,"PARENT AND CHILD SAME\n");
        //      }
        //      else
        //      {
        //        fprintf(stdout,"PARENT AND CHILD OPPOSITE\n");
        //        for (int j=0; j<3; j++)
        //          crossVec[j] = -1.0*crossVec[j];
        //      }
        //    }
        //    else
        //    {
        //      vtkMath::Cross(crossVec, parentVec, tempVec);
        //      vtkMath::Normalize(tempVec);
        //      if ((parent->BranchDir + 1)%4 == gCell->BranchDir)
        //      {
        //        fprintf(stdout,"PARENT PLUS ONE IS CHILD\n");
        //        for (int j=0; j<3; j++)
        //          crossVec[j] = 1.0*tempVec[j];
        //      }
        //      else
        //      {
        //        for (int j=0; j<3; j++)
        //          crossVec[j] = -1.0*tempVec[j];
        //        fprintf(stdout,"PARENT PLUS THREE IS CHILD\n");
        //      }
        //    }
        //  }
        //}
        //else
        //{
        //  fprintf(stdout,"GRANDPARENT AND PARENT SAME OR OPPOSITE\n");
        //  if (parentBegType >= C_TET_0 && parentBegType <= C_TET_3)
        //  {
        //    vtkSVCenterlineGCell *grandParentDiverging = grandParent->Children[grandParent->DivergingChild];
        //    fprintf(stdout,"PARENT IS DIRECTLY IN LINE WITH GRANDPARENT\n");
        //    double divergingVec[3];
        //    vtkMath::Subtract(grandParentDiverging->EndPt, grandParentDiverging->StartPt, divergingVec);

        //    vtkMath::Cross(grandParentVec, divergingVec, crossVec);
        //    vtkMath::Normalize(crossVec);

        //    if ((grandParentDiverging->BranchDir + gCell->BranchDir)%2 == 0)
        //    {
        //      if (grandParentDiverging->BranchDir == gCell->BranchDir)
        //      {
        //        fprintf(stdout,"GRANDPARENT DIVERGING AND CHILD SAME\n");
        //      }
        //      else
        //      {
        //        fprintf(stdout,"GRANDPARENT DIVERGING AND CHILD OPPOSITE\n");
        //        for (int j=0; j<3; j++)
        //          crossVec[j] = -1.0*crossVec[j];
        //      }
        //    }
        //    else
        //    {
        //      vtkMath::Cross(grandParentVec, crossVec, tempVec);
        //      vtkMath::Normalize(tempVec);
        //      for (int j=0; j<3; j++)
        //        crossVec[j] = tempVec[j];
        //      if ((grandParentDiverging->BranchDir + 1)%4 == gCell->BranchDir)
        //      {
        //        fprintf(stdout,"GRANDPARENT DIVERGING PLUS ONE IS CHILD\n");
        //      }
        //      else
        //      {
        //        fprintf(stdout,"GRANDPARENT DIVERGING PLUS THREE IS CHILD\n");
        //        for (int j=0; j<3; j++)
        //          crossVec[j] = -1.0*crossVec[j];
        //      }
        //    }
        //  }
        //  else
        //  {
        //    if ((parent->BranchDir + gCell->BranchDir)%2 == 0)
        //    {
        //      vtkMath::Cross(parentVec, crossVec, tempVec);
        //      vtkMath::Normalize(tempVec);
        //      for (int j=0; j<3; j++)
        //        crossVec[j] = tempVec[j];
        //      if (parent->BranchDir == gCell->BranchDir)
        //      {
        //        fprintf(stdout,"PARENT AND CHILD SAME\n");
        //      }
        //      else
        //      {
        //        fprintf(stdout,"PARENT AND CHILD OPPOSITE\n");
        //        for (int j=0; j<3; j++)
        //          crossVec[j] = -1.0*crossVec[j];
        //      }
        //    }
        //    else
        //    {
        //      if ((parent->BranchDir + 1)%4 == gCell->BranchDir)
        //      {
        //        fprintf(stdout,"PARENT PLUS ONE IS CHILD\n");
        //        for (int j=0; j<3; j++)
        //          crossVec[j] = 1.0*crossVec[j];
        //      }
        //      else
        //      {
        //        for (int j=0; j<3; j++)
        //          crossVec[j] = -1.0*crossVec[j];
        //        fprintf(stdout,"PARENT PLUS THREE IS CHILD\n");
        //      }
        //    }
        //  }
        //}

        if (begType >= C_TET_0 && begType <= S_TET_3)
        {
          if (begSplitType == BI)
            rotationAngle = 90.0;
          else if (begSplitType == TRI)
          {
            if (gCell->Id == parent->Children[parent->AligningChild]->Id)
              rotationAngle = 180.0;
            else
              rotationAngle = 90.0;
          }
        }
        else
          rotationAngle = 180.0*gCell->RefAngle/SV_PI;
      }

      //fprintf(stdout,"LETS SEE ANG!!!: %.6f\n", rotationAngle);
      this->RotateVecAroundLine(rotateVec, rotationAngle, crossVec, lineDir);
      vtkMath::Normalize(lineDir);

      vtkMath::MultiplyScalar(lineDir, length);
      vtkMath::Add(gCell->StartPt, lineDir, gCell->EndPt);
    }
    fprintf(stdout,"\n");
  }

  return SV_OK;
}

// ----------------------
// UpdateBranchReferenceDirections
// ----------------------
int vtkSVCenterlineGraph::UpdateBranchReferenceDirections()
{
  int numSegs   = this->NumberOfCells;
  int numPoints = this->Lines->GetNumberOfPoints();

  double refVecs[3][3];
  for (int i=0; i<numSegs; i++)
  {
    // Corresponding GCell
    vtkSVCenterlineGCell *gCell = this->GetCell(i);
    fprintf(stdout,"DOING GROUP %d\n", gCell->GroupId);

    // The front direction of segment
    for (int j=0; j<3; j++)
    {
      for (int k=0; k<3; k++)
        refVecs[j][k] = gCell->RefDirs[j][k];
    }

    // cell id in the vtkPolyData
    int cellId = this->Lines->GetCellData()->GetArray(
     this->GroupIdsArrayName)->LookupValue(gCell->GroupId);

    // Get Cell points
    vtkIdType npts, *pts;
    this->Lines->GetCellPoints(cellId, npts, pts);

    int isTerminating = 0;
    double endVecs[3][3];
    if (gCell->Parent == NULL)
    {
      // found parent, flip around
      isTerminating = 1;

      // Check to make sure not already flipped
      vtkNew(vtkIdList, pointCells);
      this->Lines->GetPointCells(pts[0], pointCells);
      if (pointCells->GetNumberOfIds() == 1)
        this->FlipLinePoints(this->Lines, cellId);
      this->Lines->GetCellPoints(cellId, npts, pts);
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

    for (int j=1; j<npts; j++)
    {
      this->Lines->GetPoint(pts[j-1], pt0);
      this->Lines->GetPoint(pts[j], pt1);

      double checkRefs[3][3];
      for (int k=0; k<3; k++)
        for (int l=0; l<3; l++)
          checkRefs[k][l] = refVecs[k][l];

      if (gCell->Parent == NULL)
        vtkMath::Subtract(pt1, pt0, refVecs[0]);
      else
        vtkMath::Subtract(pt0, pt1, refVecs[0]);
      vtkMath::Normalize(refVecs[0]);

      this->ComputeLocalCoordinateSystem(checkRefs[0], refVecs[0], refVecs[1], tmpX, refVecs[2]);
      for (int k=0; k<3; k++)
        refVecs[1][k] = tmpX[k];
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

      // update branch dirs
      this->UpdateBranchDirs(gCell, updateDir);


      // update reference directions
      if (projDot < 0)
      {
        vtkMath::MultiplyScalar(endVecs[maxDir], -1.0);
      }

      for (int j=0; j<3; j++)
      {
        endVecs[1][j] = endVecs[maxDir][j];
      }

      vtkMath::Cross(endVecs[0], endVecs[1], endVecs[2]);

      for (int j=0; j<3; j++)
      {
        for (int k=0; k<3; k++)
        {
          for (int l=0; l<gCell->Children.size(); l++)
            gCell->Children[l]->RefDirs[j][k] = endVecs[j][k];
        }
      }
    }
  }

  return SV_OK;
}

// ----------------------
// UpdateBranchDirs
// ----------------------
int vtkSVCenterlineGraph::UpdateBranchDirs(vtkSVCenterlineGCell *gCell, const int updateDir)
{
  if (gCell->Children.size() != 0)
  {
    for (int j=0; j<gCell->Children.size(); j++)
    {
      gCell->Children[j]->BranchDir = (gCell->Children[j]->BranchDir + updateDir)%4;
      fprintf(stdout,"BRANCH %d updated to direction %d\n", gCell->Children[j]->GroupId, gCell->Children[j]->BranchDir);
      fprintf(stdout,"UPDATE WAS: %d\n", updateDir);
      //this->UpdateBranchDirs(gCell->Children[j], updateDir);
    }
  }

  return SV_OK;
}

// ----------------------
// ComputeLocalCoordinateSystem
// ----------------------
int vtkSVCenterlineGraph::ComputeLocalCoordinateSystem(const double prev_vz[3],
                                                       const double vz[3],
                                                       const double prev_vx[3],
                                                       double vx[3],
                                                       double vy[3])
{
	//double tempArray[3];
  //double tempLength = vtkMath::Dot(prev_vx, vz);
	//for (int i = 0; i < 3; i++)
	//	tempArray[i] = tempLength * vz[i];

  //vtkMath::Subtract(prev_vx, tempArray, vx);
  //vtkMath::Normalize(vx);

  //vtkMath::Cross(vz, vx, vy);
  //vtkMath::Normalize(vy);

  double dot = vtkMath::Dot(prev_vz, vz);

  double theta;
  if (1 - dot < 1.0e-6)
  {
    theta = 0.0;
  }
  else
  {
    theta = acos(dot) * 180.0 / SV_PI;
  }

  double tempArray[3];
  vtkMath::Cross(prev_vz, vz, tempArray);

  vtkNew(vtkTransform, transform);
  transform->Identity();
  transform->RotateWXYZ(theta, tempArray);
  transform->TransformPoint(prev_vx, vx);

  dot = vtkMath::Dot(vz, vx);
  vx[0] -= dot * vz[0];
  vx[1] -= dot * vz[1];
  vx[2] -= dot * vz[2];

  vtkMath::Normalize(vx);

  vtkMath::Cross(vz, vx, vy);
  vtkMath::Normalize(vy);

  return SV_OK;
}

// ----------------------
// RotateVecAroundLine
// ----------------------
int vtkSVCenterlineGraph::RotateVecAroundLine(const double inVec[3],
                                           const double angle,
                                           const double axis[3],
                                           double outVec[3])
{
  double inPt[3];
  vtkMath::Subtract(inVec, axis, inPt);

  vtkNew(vtkPoints, tmpPoints);
  tmpPoints->InsertNextPoint(inPt);

  vtkNew(vtkPolyData, tmpPoly);
  tmpPoly->SetPoints(tmpPoints);

  vtkNew(vtkTransform, transform);
  //transform->RotateWXYZ(double angle, double x, double y, double z);
  transform->RotateWXYZ(angle, axis);

  vtkNew(vtkTransformPolyDataFilter, transformFilter);

  transformFilter->SetTransform(transform);
  transformFilter->SetInputData(tmpPoly);
  transformFilter->Update();

  double outPt[3];
  transformFilter->GetOutput()->GetPoint(0, outPt);

  vtkMath::Add(axis, outPt, outVec);

  return SV_OK;
}

// ----------------------
// FlipLinePoints
// ----------------------
int vtkSVCenterlineGraph::FlipLinePoints(vtkPolyData *pd, const int cellId)
{
  vtkIdType npts, *pts;
  pd->GetCellPoints(cellId, npts, pts);

  double *tmpPts = new double[npts];
  for (int i=0; i<npts; i++)
    tmpPts[i] = pts[i];

  for (int i=0; i<npts; i++)
    pts[(npts-1)-i] = tmpPts[i];

  pd->ReplaceCell(cellId, npts, pts);
  pd->Modified();
  pd->BuildLinks();

  delete [] tmpPts;

  return SV_OK;
}

// ----------------------
// ComputeMinimumLength
// ----------------------
int vtkSVCenterlineGraph::ComputeMinimumLength(vtkSVCenterlineGCell *gCell, double &minLength)
{
  if (this->CubeSize == 0.0)
  {
    minLength = 0.0;
    return SV_OK;
  }

  // Get all brothers
  double minTopLength = 0.0;
  if (gCell->Parent != NULL)
  {
    vtkSVCenterlineGCell *parent = gCell->Parent;

    double tmpLength;
    for (int i=0; i<parent->Children.size(); i++)
    {
      if (parent->Children[i] == gCell)
        continue;

      vtkSVCenterlineGCell *brother = parent->Children[i];

      double broAng = SV_PI * 45.0 / 180.0;
      if (brother->BranchDir == gCell->BranchDir)
      {
        broAng = fabs(brother->RefAngle - gCell->RefAngle);
      }
      else if ((brother->BranchDir + gCell->BranchDir)%2 == 0)
      {
        broAng = (SV_PI - brother->RefAngle) + (SV_PI - gCell->RefAngle);
      }

      if (sin(broAng/2.) > -1.0e-6 && sin(broAng/2.) < 1.0e-6)
        tmpLength = this->CubeSize/2.;
      else
        tmpLength = (this->CubeSize/2.) / ( sin(broAng/2.));

      if (tmpLength > minTopLength)
      {
        minTopLength = tmpLength;
      }
    }

    if (sin(gCell->RefAngle/2.) > -1.0e-6 && sin(gCell->RefAngle/2.) < 1.0e-6)
      tmpLength = this->CubeSize/2.;
    else
      tmpLength = (this->CubeSize/2.) / ( sin(gCell->RefAngle/2.));

    if (tmpLength > minTopLength)
    {
      minTopLength = tmpLength;
    }

  }

  // Get all children
  double minBottomLength = 0.0;

  for (int i=0; i<gCell->Children.size(); i++)
  {
    vtkSVCenterlineGCell *child = gCell->Children[i];
    double childAng = child->RefAngle;

    double tmpLength;
    if (sin(childAng/2.) > -1.0e-6 && sin(childAng/2.) < 1.0e-6)
      tmpLength = this->CubeSize/2.;
    else
      tmpLength = (this->CubeSize/2.) / ( sin(childAng/2.));

    if (tmpLength > minBottomLength)
    {
      minBottomLength = tmpLength;
    }
  }

  minLength = minTopLength + minBottomLength;
  minLength *= 1.1;

  return SV_OK;
}
