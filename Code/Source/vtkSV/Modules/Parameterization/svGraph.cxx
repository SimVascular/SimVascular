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

#include "svGraph.h"

#include "vtkCellArray.h"
#include "vtkCellData.h"
#include "vtkCleanPolyData.h"
#include "vtkIdList.h"
#include "vtkMath.h"
#include "vtkPoints.h"
#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"
#include "vtkSVPolyDataSliceAndDiceFilter.h"
#include "vtkSmartPointer.h"
#include "vtkThreshold.h"

svGraph::svGraph()
{
  this->NumberOfCells = 0;
  this->NumberOfNodes = 1; // The root
  this->Root = NULL;

  this->Lines = vtkPolyData::New();
}

svGraph::svGraph(int rootId,
                 vtkPolyData *linesPd,
                 std::string groupIdsArrayName,
                 std::multimap<int, int> criticalPointMap)
{
  this->NumberOfCells = 0;
  this->NumberOfNodes = 1; // The root
  double startPt[3]; startPt[0] = 0.0; startPt[1] = 0.0; startPt[2] = 0.0;
  double endPt[3]; endPt[0] = 0.0; endPt[1] = 0.0; endPt[2] = -1.0;
  this->Root = this->NewCell(rootId, vtkSVPolyDataSliceAndDiceFilter::DOWN, startPt, endPt);

  this->Lines = vtkPolyData::New();
  this->Lines->DeepCopy(linesPd);
  this->GroupIdsArrayName = groupIdsArrayName;
  this->CriticalPointMap = criticalPointMap;
}

svGraph::~svGraph()
{
  if (this->Root != NULL)
  {
    delete this->Root;
    this->Root = NULL;
  }
  if (this->Lines != NULL)
  {
    this->Lines->Delete();
    this->Lines = NULL;
  }
}

int svGraph::Recurse(svGCell *rootGCell, int(*function)(svGCell *currentGCell,
                   void *arg0, void *arg1, void *arg2),
                   void *rec_arg0, void *rec_arg1, void *rec_arg2)
{
  function(rootGCell, rec_arg0, rec_arg1, rec_arg2);
  if (rootGCell->Children[0] != NULL)
  {
    svGraph::Recurse(rootGCell->Children[0], function, rec_arg0, rec_arg1, rec_arg2);
  }
  if (rootGCell->Children[1] != NULL)
  {
    svGraph::Recurse(rootGCell->Children[1], function, rec_arg0, rec_arg1, rec_arg2);
  }
  return SV_OK;
}

int svGraph::PrintGraph()
{
  svGraph::Recurse(this->Root, svGraph::PrintGCell, NULL, NULL, NULL);
  return SV_OK;
}

int svGraph::PrintGCell(svGCell *gCell, void *arg0, void *arg1, void *arg2)
{
  fprintf(stdout, "GCell ID: %d\n", gCell->Id);
  fprintf(stdout, "GCell Group ID: %d\n", gCell->GroupId);
  fprintf(stdout, "Direction: %d\n", gCell->Dir);
  if(gCell->Parent != NULL)
    fprintf(stdout, "Parent: %d\n", gCell->Parent->Id);
  else
    fprintf(stdout, "Parent is NULL\n");
  if(gCell->Children[0] != NULL)
    fprintf(stdout, "Child 0: %d\n", gCell->Children[0]->Id);
  else
    fprintf(stdout, "Child 0: NULL\n");
  if(gCell->Children[1] != NULL)
    fprintf(stdout, "Child 1: %d\n", gCell->Children[1]->Id);
  else
    fprintf(stdout, "Child 1: NULL\n");
  fprintf(stdout, "Start Point: %.4f %.4f %.4f\n", gCell->StartPt[0], gCell->StartPt[1], gCell->StartPt[2]);
  fprintf(stdout, "End Point: %.4f %.4f %.4f\n", gCell->EndPt[0], gCell->EndPt[1], gCell->EndPt[2]);
  return SV_OK;
}

int svGraph::GetGraphPolyData(vtkPolyData *pd)
{
  vtkNew(vtkPoints, newPoints);
  vtkNew(vtkCellArray, newCells);
  vtkNew(vtkIntArray, groupIds);
  groupIds->SetName(this->GroupIdsArrayName.c_str());
  svGraph::Recurse(this->Root, svGraph::InsertGCellPoints, newPoints, newCells, groupIds);

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

int svGraph::InsertGCellPoints(svGCell *gCell, void *arg0, void *arg1, void *arg2)
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

int svGraph::BuildGraph()
{
  fprintf(stdout,"Building graph!!!\n");

  std::list<int> keyVals;
  vtkSVGeneralUtils::GetValuesFromMap(this->CriticalPointMap, this->Root->GroupId, keyVals);
  if (keyVals.size() != 0)
  {
    std::list<int> children;
    vtkSVGeneralUtils::GetUniqueNeighbors(this->CriticalPointMap, this->Root->GroupId, keyVals, children);
    if (children.size() != 0)
    {
      std::list<int>::iterator childit = children.begin();
      this->Root->Children[0] = this->NewCell(*childit, this->Root); ++childit;
      this->Root->Children[1] = this->NewCell(*childit, this->Root); ++childit;
      this->NumberOfNodes += 3;

      this->ComputeReferenceVectors(this->Root);
      this->GetNewBranchDirections(this->Root);
      this->GrowGraph(this->Root->Children[0]);
      this->GrowGraph(this->Root->Children[1]);
    }
  }

  svGraph::Recurse(this->Root, svGraph::UpdateCellDirection,
                   NULL, NULL, NULL);

  return SV_OK;
}

//---------------------------------------------------------------------------
/**
 * @brief
 * @param *pd
 * @return
 */
int svGraph::GrowGraph(svGCell *parent)
{
  std::list<int> keyVals;
  vtkSVGeneralUtils::GetValuesFromMap(this->CriticalPointMap, parent->GroupId, keyVals);
  if (keyVals.size() != 0)
  {
    std::list<int> children;
    vtkSVGeneralUtils::GetUniqueNeighbors(this->CriticalPointMap, parent->GroupId, keyVals, children);
    if (children.size() > 2)
    {
      //fprintf(stdout,"Number of children!: %lu\n", children.size());
      std::list<int>::iterator childit = children.begin();
      int count = 0;
      for (int i=0; childit != children.end(); ++childit)
      {
        if (*childit != parent->Parent->Children[0]->GroupId && *childit != parent->Parent->Children[1]->GroupId && *childit != parent->Parent->GroupId)
        {
          parent->Children[count] = this->NewCell(*childit, parent);
          count++;
        }
      }
      this->NumberOfNodes += 3;
      //fprintf(stdout,"What is the child %d\n", parent->Children[0]->GroupId);
      //fprintf(stdout,"What is the child %d\n", parent->Children[1]->GroupId);
      this->GetNewBranchDirections(parent);

      this->GrowGraph(parent->Children[0]);
      this->GrowGraph(parent->Children[1]);
    }
  }

  return SV_OK;
}

//---------------------------------------------------------------------------
/**
 * @brief
 * @param *pd
 * @return
 */
int svGraph::ComputeReferenceVectors(svGCell *parent)
{
  vtkNew(vtkThreshold, thresholder);
  thresholder->SetInputData(this->Lines);
  thresholder->SetInputArrayToProcess(0, 0, 0, 1, this->GroupIdsArrayName.c_str());
  thresholder->ThresholdBetween(parent->GroupId, parent->GroupId);
  thresholder->Update();
  int numPts = thresholder->GetOutput()->GetNumberOfPoints();

  double endPt0[3], endPt1[3];
  thresholder->GetOutput()->GetPoint(numPts-2, endPt0);
  thresholder->GetOutput()->GetPoint(numPts-1, endPt1);
  fprintf(stdout,"End pt 0 is: %.4f %.4f %.4f\n", endPt0[0], endPt0[1], endPt0[2]);
  fprintf(stdout,"End pt 1 is: %.4f %.4f %.4f\n", endPt1[0], endPt1[1], endPt1[2]);

  double startPts[2][3];
  double secondPts[2][3];
  for (int i=0; i<2; i++)
  {
    thresholder->SetInputData(this->Lines);
    thresholder->SetInputArrayToProcess(0, 0, 0, 1, this->GroupIdsArrayName.c_str());
    thresholder->ThresholdBetween(parent->Children[i]->GroupId, parent->Children[i]->GroupId);
    thresholder->Update();

    thresholder->GetOutput()->GetPoint(0, startPts[i]);
    thresholder->GetOutput()->GetPoint(1, secondPts[i]);
  }

  double vec0[3], vec1[3];
  vtkMath::Subtract(endPt0, endPt1, this->ReferenceVecs[0]);
  vtkMath::Normalize(this->ReferenceVecs[0]);
  vtkMath::Subtract(secondPts[0], startPts[0], vec0);
  vtkMath::Normalize(vec0);
  vtkMath::Subtract(secondPts[1], startPts[1], vec1);
  vtkMath::Normalize(vec1);

  // Get angle between vectors
  double angleVec0[3], angleVec1[3];
  vtkMath::Cross(vec0, this->ReferenceVecs[0], angleVec0);
  double ang0 = atan2(vtkMath::Norm(angleVec0), vtkMath::Dot(vec0, this->ReferenceVecs[0]));
  vtkMath::Cross(vec1, this->ReferenceVecs[0], angleVec1);
  double ang1 = atan2(vtkMath::Norm(angleVec1), vtkMath::Dot(vec1, this->ReferenceVecs[0]));

  fprintf(stdout,"Root vec: %.4f %.4f %.4f\n", this->ReferenceVecs[0][0], this->ReferenceVecs[0][1], this->ReferenceVecs[0][2]);
  fprintf(stdout,"Vec 0: %.4f %.4f %.4f\n", vec0[0], vec0[1], vec0[2]);
  fprintf(stdout,"Vec 1: %.4f %.4f %.4f\n", vec1[0], vec1[1], vec1[2]);
  fprintf(stdout,"Angle 0: %4f\n", 180*ang0/SV_PI);
  fprintf(stdout,"Angle 1: %4f\n", 180*ang1/SV_PI);
  if (ang0 > ang1)
  {
    fprintf(stdout,"Diverging child is %d\n", parent->Children[1]->GroupId);
    vtkMath::Cross(this->ReferenceVecs[0], vec1, this->ReferenceVecs[2]);
    parent->DivergingChild = 1;
    parent->AligningChild  = 0;
  }
  else
  {
    fprintf(stdout,"Diverging child is %d\n", parent->Children[0]->GroupId);
    vtkMath::Cross(this->ReferenceVecs[0], vec0, this->ReferenceVecs[2]);
    parent->DivergingChild = 0;
    parent->AligningChild  = 1;
  }
  parent->Children[parent->AligningChild]->IsAlign = 1;
  parent->Children[parent->DivergingChild]->IsAlign = 1;
  vtkMath::Normalize(this->ReferenceVecs[2]);
  vtkMath::Cross(this->ReferenceVecs[2], this->ReferenceVecs[0], this->ReferenceVecs[1]);
  vtkMath::Normalize(this->ReferenceVecs[1]);

  vtkMath::Cross(this->ReferenceVecs[1], this->ReferenceVecs[0], parent->FrontDir);

  return SV_OK;
}

//---------------------------------------------------------------------------
/**
 * @brief
 * @param *pd
 * @return
 */
int svGraph::GetDirectionVector(const int dir, double dirVector[3])
{
  if (dir == vtkSVPolyDataSliceAndDiceFilter::RIGHT)
  {
    dirVector[0] = 1.0; dirVector[1] = 0.0; dirVector[2] = 0.0;
  }
  if (dir == vtkSVPolyDataSliceAndDiceFilter::LEFT)
  {
    dirVector[0] = -1.0; dirVector[1] = 0.0; dirVector[2] = 0.0;
  }
  if (dir == vtkSVPolyDataSliceAndDiceFilter::FRONT)
  {
    dirVector[0] = 0.0; dirVector[1] = -1.0; dirVector[2] = 0.0;
  }
  if (dir == vtkSVPolyDataSliceAndDiceFilter::BACK)
  {
    dirVector[0] = 0.0; dirVector[1] = 1.0; dirVector[2] = 0.0;
  }
  if (dir == vtkSVPolyDataSliceAndDiceFilter::UP)
  {
    dirVector[0] = 0.0; dirVector[1] = 0.0; dirVector[2] = 1.0;
  }
  if (dir == vtkSVPolyDataSliceAndDiceFilter::DOWN)
  {
    dirVector[0] = 0.0; dirVector[1] = 0.0; dirVector[2] = -1.0;
  }
  return SV_OK;
}


//---------------------------------------------------------------------------
/**
 * @brief
 * @param *pd
 * @return
 */
int svGraph::GetNewBranchDirections(svGCell *parent)
{
  //fprintf(stdout,"Child %d of parent %d, dir: %d\n", parent->Children[0]->GroupId, parent->GroupId, parent->Children[0]->Dir);
  //fprintf(stdout,"Child %d of parent %d, dir: %d\n", parent->Children[1]->GroupId, parent->GroupId, parent->Children[1]->Dir);
  vtkNew(vtkThreshold, thresholder);
  thresholder->SetInputData(this->Lines);
  thresholder->SetInputArrayToProcess(0, 0, 0, 1, this->GroupIdsArrayName.c_str());
  thresholder->ThresholdBetween(parent->GroupId, parent->GroupId);
  thresholder->Update();
  int numPts = thresholder->GetOutput()->GetNumberOfPoints();

  double endPt0[3], endPt1[3];
  thresholder->GetOutput()->GetPoint(numPts-2, endPt0);
  thresholder->GetOutput()->GetPoint(numPts-1, endPt1);

  double startPts[2][3];
  double secondPts[2][3];
  for (int i=0; i<2; i++)
  {
    thresholder->SetInputData(this->Lines);
    thresholder->SetInputArrayToProcess(0, 0, 0, 1, this->GroupIdsArrayName.c_str());
    thresholder->ThresholdBetween(parent->Children[i]->GroupId, parent->Children[i]->GroupId);
    thresholder->Update();

    thresholder->GetOutput()->GetPoint(0, startPts[i]);
    thresholder->GetOutput()->GetPoint(1, secondPts[i]);
  }
  fprintf(stdout,"Determining Angle for Parent %d with children %d %d\n", parent->GroupId,
                                                                          parent->Children[0]->GroupId,
                                                                          parent->Children[1]->GroupId);

  double vec0[3], vec1[3], vec2[3];
  vtkMath::Subtract(endPt0, endPt1, vec0);
  vtkMath::Normalize(vec0);
  vtkMath::Subtract(secondPts[0], startPts[0], vec1);
  vtkMath::Normalize(vec1);
  vtkMath::Subtract(secondPts[1], startPts[1], vec2);
  vtkMath::Normalize(vec2);

  int maxDir;
  double maxDot = -0.1;
  for (int i=0; i<3; i++)
  {
    double compare = fabs(vtkMath::Dot(this->ReferenceVecs[i], vec0));
    fprintf(stdout,"Dot with Ref %d: %.4f\n", i, vtkMath::Dot(this->ReferenceVecs[i], vec0));
    if (compare > maxDot)
    {
      maxDot = compare;
      maxDir = i;
    }
  }
  fprintf(stdout,"Direction aligns most with: %d\n", maxDir);

  // Get angle between vectors
  double angleVec0[3], angleVec1[3];
  vtkMath::Cross(vec1, vec0, angleVec0);
  double ang0 = atan2(vtkMath::Norm(angleVec0), vtkMath::Dot(vec1, vec0));
  vtkMath::Cross(vec2, vec0, angleVec1);
  double ang1 = atan2(vtkMath::Norm(angleVec1), vtkMath::Dot(vec2, vec0));

  double vec3[3];
  if (ang0 > ang1)
  {
    vtkMath::Cross(vec0, vec2, vec3);
    vtkMath::Cross(vec2, vec0, parent->FrontDir);
    parent->DivergingChild = 1;
    parent->AligningChild  = 0;
  }
  else
  {
    vtkMath::Cross(vec0, vec1, vec3);
    vtkMath::Cross(vec1, vec0, parent->FrontDir);
    parent->DivergingChild = 0;
    parent->AligningChild  = 1;
  }
  parent->Children[parent->AligningChild]->IsAlign  = 1;
  parent->Children[parent->DivergingChild]->IsAlign = 0;

  //Check to see orientation with respect to reference vectors. Angle that
  //it makes with reference vector determines the input angle to a
  //lookup table that tells us the absolute direction that the new branch
  //node should go

  double checkVec[3], dotVec[3];
  for (int i=0; i<3; i++)
  {
    checkVec[i] = this->ReferenceVecs[(maxDir+2)%3][i];
    dotVec[i]   = this->ReferenceVecs[(maxDir+1)%3][i];
  }

  vtkMath::Normalize(vec3);
  //fprintf(stdout,"This Branch: %.4f %.4f %.4f\n", vec3[0], vec3[1], vec3[2]);
  double angleVec2[3];
  vtkMath::Cross(vec3, checkVec, angleVec2);
  double ang2 = atan2(vtkMath::Norm(angleVec2), vtkMath::Dot(vec3, checkVec));
  fprintf(stdout,"Dot between vec3 and ref is %.4f\n", vtkMath::Dot(vec3, dotVec));
  if (parent->GroupId != 0)
  {
    if (vtkMath::Dot(vec3, dotVec) < 0.0)
      ang2 = ang2 + SV_PI;
  }
  parent->Children[parent->DivergingChild]->RefAngle = ang2;

  fprintf(stdout,"Angle check dir: %.4f %.4f %.4f\n", checkVec[0], checkVec[1], checkVec[2]);
  fprintf(stdout,"Dot check dir: %.4f %.4f %.4f\n", dotVec[0], dotVec[1], dotVec[2]);
  fprintf(stdout,"This vec: %.4f %.4f %.4f\n", vec3[0], vec3[1], vec3[2]);
  fprintf(stdout,"Parent direction: %d\n", parent->Dir);
  fprintf(stdout,"Angle is: %.4f\n", 180*ang2/SV_PI);
  fprintf(stdout,"Dot is: %.4f\n", vtkMath::Dot(vec3, dotVec));

  return SV_OK;
}

int svGraph::UpdateCellDirection(svGCell *gCell, void *arg0,
                                 void *arg1, void *arg2)
{
  if (gCell->Parent != NULL)
  {
    int direction0 = gCell->Parent->Dir;
    int direction1;
    if (gCell->IsAlign == 1)
      direction1 = gCell->Parent->Dir;
    else
    {
      int ang = gCell->RefAngle;
      if (ang >= 7.0*SV_PI/4.0 || ang < SV_PI/4.0)
        direction1 = vtkSVPolyDataSliceAndDiceFilter::DT[direction0][0];

      else if (ang >= SV_PI/4.0 && ang < 3.0*SV_PI/4.0)
        direction1 = vtkSVPolyDataSliceAndDiceFilter::DT[direction0][1];

      else if (ang >= 3.0*SV_PI/4.0 && ang < 5.0*SV_PI/4.0)
        direction1 = vtkSVPolyDataSliceAndDiceFilter::DT[direction0][2];

      else if (ang >= 5.0*SV_PI/4.0 && ang < 7.0*SV_PI/4.0)
        direction1 = vtkSVPolyDataSliceAndDiceFilter::DT[direction0][3];
    }
    gCell->Dir = direction1;
    double dirVector[3];
    svGraph::GetDirectionVector(direction1, dirVector);
    for (int i=0; i<3; i++)
    {
      gCell->StartPt[i] = gCell->Parent->EndPt[i];
      gCell->EndPt[i]   = gCell->StartPt[i] + dirVector[i];
    }
    vtkIntArray *rotIndices = reinterpret_cast<vtkIntArray*>(arg0);
    if (rotIndices != NULL)
    {
      int cellIndices[8];
      int tmpIds[8];
      for (int i=0; i<8; i++)
      {
        cellIndices[i] = vtkSVPolyDataSliceAndDiceFilter::LookupIndex(gCell->Parent->Dir, gCell->Parent->Children[gCell->Parent->DivergingChild]->Dir, i);
        tmpIds[i] = gCell->CornerPtIds[i];
      }
      for (int i=0; i<8; i++)
      {
        //fprintf(stdout,"Working on cube %d\n", gCell->GroupId);
        //fprintf(stdout,"Inside graph %d is now becoming %d\n", gCell->CornerPtIds[cellIndices[i]], tmpIds[cellIndices[rotIndices->GetValue(i)]]);
        gCell->CornerPtIds[cellIndices[i]] = tmpIds[rotIndices->GetValue(cellIndices[i])];
      }
    }
  }
  return SV_OK;
}

svGCell* svGraph::NewCell(int a_GroupId, svGCell *a_Parent)
{
  svGCell *newCell = new svGCell;
  newCell->Parent  = a_Parent;
  newCell->Id      = this->NumberOfCells++;
  newCell->GroupId = a_GroupId;
  if (a_Parent != NULL)
  {
    for (int i=0; i<3; i++)
      newCell->StartPt[i] = a_Parent->EndPt[i];
  }

  return newCell;
}

svGCell* svGraph::NewCell(int a_GroupId, int a_Dir, double a_StartPt[3], double a_EndPt[3])
{
  svGCell *newCell = new svGCell;
  newCell->Id      = this->NumberOfCells++;
  newCell->GroupId = a_GroupId;
  newCell->Dir     = a_Dir;
  for (int i=0; i<3; i++)
  {
    newCell->StartPt[i] = a_StartPt[i];
    newCell->EndPt[i]   = a_EndPt[i];
  }

  return newCell;
}

svGCell* svGraph::GetCell(const int findId)
{
  if (findId > this->NumberOfCells)
  {
    fprintf(stdout,"Id is larger than number of cells\n");
    return NULL;
  }
  return this->LookUp(this->Root, findId);
}

svGCell* svGraph::LookUp(svGCell *lookCell, const int findId)
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
    if (lookCell->Children[0] != NULL)
    {
      svGCell* foundCell = this->LookUp(lookCell->Children[0], findId);
      if (foundCell == NULL)
      {
        foundCell = this->LookUp(lookCell->Children[1], findId);
      }
      return foundCell;
    }
  }

  return NULL;
}
