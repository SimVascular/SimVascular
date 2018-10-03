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

#include "vtkSVCenterlineMerger.h"

#include "vtkSplineFilter.h"
#include "vtkCleanPolyData.h"
#include "vtkDoubleArray.h"
#include "vtkErrorCode.h"
#include "vtkPointData.h"
#include "vtkPolyLine.h"
#include "vtkCellData.h"
#include "vtkCellArray.h"
#include "vtkIdList.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkObjectFactory.h"
#include "vtkMath.h"
#include "vtkSmartPointer.h"
#include "vtkVersion.h"

#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"

#include "vtkvmtkCenterlineUtilities.h"
#include "vtkvmtkCenterlineBifurcationReferenceSystems.h"
#include "vtkvmtkReferenceSystemUtilities.h"

#include <cmath>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVCenterlineMerger);

// ----------------------
// Constructor
// ----------------------
vtkSVCenterlineMerger::vtkSVCenterlineMerger()
{
  this->RadiusArrayName = NULL;
  this->GroupIdsArrayName = NULL;
  this->CenterlineIdsArrayName = NULL;
  this->TractIdsArrayName = NULL;
  this->BlankingArrayName = NULL;
  this->ResamplingStepLength = 0.0;
  this->MergeBlanked = 1;
}

// ----------------------
// Destructor
// ----------------------
vtkSVCenterlineMerger::~vtkSVCenterlineMerger()
{
  if (this->RadiusArrayName)
    {
    delete[] this->RadiusArrayName;
    this->RadiusArrayName = NULL;
    }

  if (this->GroupIdsArrayName)
    {
    delete[] this->GroupIdsArrayName;
    this->GroupIdsArrayName = NULL;
    }

  if (this->CenterlineIdsArrayName)
    {
    delete[] this->CenterlineIdsArrayName;
    this->CenterlineIdsArrayName = NULL;
    }

  if (this->TractIdsArrayName)
    {
    delete[] this->TractIdsArrayName;
    this->TractIdsArrayName = NULL;
    }

  if (this->BlankingArrayName)
    {
    delete[] this->BlankingArrayName;
    this->BlankingArrayName = NULL;
    }
}

// ----------------------
// RequestData
// ----------------------
int vtkSVCenterlineMerger::RequestData(vtkInformation *vtkNotUsed(request), vtkInformationVector **inputVector, vtkInformationVector *outputVector)
{
  vtkInformation *inInfo = inputVector[0]->GetInformationObject(0);
  vtkInformation *outInfo = outputVector->GetInformationObject(0);

  vtkPolyData *input = vtkPolyData::SafeDownCast(inInfo->Get(vtkDataObject::DATA_OBJECT()));
  vtkPolyData *output = vtkPolyData::SafeDownCast(outInfo->Get(vtkDataObject::DATA_OBJECT()));

  if (!this->RadiusArrayName)
    {
    vtkErrorMacro(<<"RadiusArrayName not specified");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;;
    }

  vtkDataArray* radiusArray = input->GetPointData()->GetArray(this->RadiusArrayName);

  if (!radiusArray)
    {
    vtkErrorMacro(<<"RadiusArray with name specified does not exist");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;;
    }

  if (!this->GroupIdsArrayName)
    {
    vtkErrorMacro(<<"GroupIdsArrayName not specified");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;;
    }

  vtkDataArray* groupIdsArray = input->GetCellData()->GetArray(this->GroupIdsArrayName);

  if (!groupIdsArray)
    {
    vtkErrorMacro(<<"GroupIdsArray with name specified does not exist");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;;
    }

  if (!this->CenterlineIdsArrayName)
    {
    vtkErrorMacro(<<"CenterlineIdsArrayName not specified");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;;
    }

  vtkDataArray* centerlineIdsArray = input->GetCellData()->GetArray(this->CenterlineIdsArrayName);

  if (!centerlineIdsArray)
    {
    vtkErrorMacro(<<"CenterlineIdsArray with name specified does not exist");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;;
    }

  if (!this->TractIdsArrayName)
    {
    vtkErrorMacro(<<"TractIdsArrayName not specified");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;;
    }

  vtkDataArray* tractIdsArray = input->GetCellData()->GetArray(this->TractIdsArrayName);

  if (!tractIdsArray)
    {
    vtkErrorMacro(<<"TractIdsArray with name specified does not exist");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;;
    }

  if (!this->BlankingArrayName)
    {
    vtkErrorMacro(<<"BlankingArrayName not specified");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;;
    }

  vtkDataArray* blankingArray = input->GetCellData()->GetArray(this->BlankingArrayName);

  if (!blankingArray)
    {
    vtkErrorMacro(<<"BlankingArray with name specified does not exist");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;;
    }

  vtkSmartPointer<vtkCleanPolyData> cleaner = vtkSmartPointer<vtkCleanPolyData>::New();
#if (VTK_MAJOR_VERSION <= 5)
  cleaner->SetInput(input);
#else
  cleaner->SetInputData(input);
#endif
  cleaner->Update();


  if (this->ResamplingStepLength < 1E-12)
  {
    vtkDataArray *radiusVals = cleaner->GetOutput()->GetPointData()->GetArray(this->RadiusArrayName);

    double avgRadiusVal = 0.0;
    double minRadiusVal = VTK_SV_LARGE_DOUBLE;
    for (int i=0; i<radiusVals->GetNumberOfTuples(); i++)
    {
      double val = radiusVals->GetTuple1(i);
      avgRadiusVal += val;
      if (val < minRadiusVal)
      {
        minRadiusVal = val;
      }
    }

    if (minRadiusVal < 0.01 * cleaner->GetOutput()->GetLength())
    {
      this->ResamplingStepLength = minRadiusVal;
    }
    else
    {
      this->ResamplingStepLength = 0.01 * cleaner->GetOutput()->GetLength();
    }
  }

  vtkSplineFilter* resampler = vtkSplineFilter::New();
#if (VTK_MAJOR_VERSION <= 5)
  resampler->SetInput(cleaner->GetOutput());
#else
  resampler->SetInputConnection(cleaner->GetOutputPort());
#endif
  resampler->SetSubdivideToLength();
  resampler->SetLength(this->ResamplingStepLength);
  resampler->Update();

  vtkPolyData* resampledCenterlines = vtkPolyData::New();
  resampledCenterlines->DeepCopy(resampler->GetOutput());

  resampler->Delete();

  radiusArray = resampledCenterlines->GetPointData()->GetArray(this->RadiusArrayName);

  vtkPoints* outputPoints = vtkPoints::New();
  vtkCellArray* outputLines = vtkCellArray::New();

  output->SetPoints(outputPoints);
  output->SetLines(outputLines);

  output->GetPointData()->CopyAllocate(resampledCenterlines->GetPointData());
  output->GetCellData()->CopyAllocate(resampledCenterlines->GetCellData());

  vtkIdList* nonBlankedGroupIds = vtkIdList::New();
  vtkvmtkCenterlineUtilities::GetNonBlankedGroupsIdList(resampledCenterlines,this->GroupIdsArrayName,this->BlankingArrayName,nonBlankedGroupIds);

  vtkIdList* groupIdsToMergedCells = vtkIdList::New();
  vtkIdType maxGroupId = vtkvmtkCenterlineUtilities::GetMaxGroupId(resampledCenterlines,this->GroupIdsArrayName);
  groupIdsToMergedCells->SetNumberOfIds(maxGroupId);
  int i;
  for (i=0; i<maxGroupId; i++)
  {
    groupIdsToMergedCells->SetId(i,-1);
  }

  vtkIdTypeArray* cellEndPointIds = vtkIdTypeArray::New();
  cellEndPointIds->SetNumberOfComponents(2);

  for (i=0; i<nonBlankedGroupIds->GetNumberOfIds(); i++)
  {
    vtkIdType groupId = nonBlankedGroupIds->GetId(i);
    vtkIdList* groupUniqueCellIds = vtkIdList::New();

    vtkvmtkCenterlineUtilities::GetGroupUniqueCellIds(resampledCenterlines,this->GroupIdsArrayName,groupId,groupUniqueCellIds);

    int numberOfMergedCellPoints = 0;
    int j;
    for (j=0; j<groupUniqueCellIds->GetNumberOfIds(); j++)
    {
      vtkIdType cellId = groupUniqueCellIds->GetId(j);
      int numberOfCellPoints = resampledCenterlines->GetCell(cellId)->GetNumberOfPoints();
      if ((j==0) || (numberOfCellPoints < numberOfMergedCellPoints))
      {
        numberOfMergedCellPoints = numberOfCellPoints;
      }
    }

    vtkIdType mergedCellId = outputLines->InsertNextCell(numberOfMergedCellPoints);
    groupIdsToMergedCells->InsertId(groupId,mergedCellId);
    int k;
    for (k=0; k<numberOfMergedCellPoints; k++)
      {
      double mergedPoint[3];
      mergedPoint[0] = 0.0;
      mergedPoint[1] = 0.0;
      mergedPoint[2] = 0.0;
      double weightSum = 0.0;
      for (j=0; j<groupUniqueCellIds->GetNumberOfIds(); j++)
        {
        vtkIdType cellId = groupUniqueCellIds->GetId(j);
        double point[3];
        vtkIdType pointId = resampledCenterlines->GetCell(cellId)->GetPointId(k);
        resampledCenterlines->GetPoint(pointId,point);
        double radius = radiusArray->GetTuple1(pointId);
        double weight = radius*radius;
        resampledCenterlines->GetCell(cellId)->GetPoints()->GetPoint(k,point);
        mergedPoint[0] += weight * point[0];
        mergedPoint[1] += weight * point[1];
        mergedPoint[2] += weight * point[2];
        weightSum += weight;
        }
      mergedPoint[0] /= weightSum;
      mergedPoint[1] /= weightSum;
      mergedPoint[2] /= weightSum;
      vtkIdType mergedPointId = outputPoints->InsertNextPoint(mergedPoint);
      outputLines->InsertCellPoint(mergedPointId);
      vtkIdType cellId = groupUniqueCellIds->GetId(0);
      vtkIdType pointId = resampledCenterlines->GetCell(cellId)->GetPointId(k);
      output->GetPointData()->CopyData(resampledCenterlines->GetPointData(),pointId,mergedPointId);
      if (k==0 || k==numberOfMergedCellPoints-1)
        {
        cellEndPointIds->InsertNextValue(mergedPointId);
        }
      }

    vtkIdType cellId = groupUniqueCellIds->GetId(0);
    output->GetCellData()->CopyData(resampledCenterlines->GetCellData(),cellId,mergedCellId);

    groupUniqueCellIds->Delete();
    }

  if (!this->MergeBlanked)
    {
    nonBlankedGroupIds->Delete();
    resampledCenterlines->Delete();
    outputPoints->Delete();
    outputLines->Delete();
    groupIdsToMergedCells->Delete();
    cellEndPointIds->Delete();
    return SV_OK;
    }

  vtkvmtkCenterlineBifurcationReferenceSystems* referenceSystemsFilter = vtkvmtkCenterlineBifurcationReferenceSystems::New();
#if (VTK_MAJOR_VERSION <= 5)
  referenceSystemsFilter->SetInput(resampledCenterlines);
#else
  referenceSystemsFilter->SetInputData(resampledCenterlines);
#endif
  referenceSystemsFilter->SetRadiusArrayName(this->RadiusArrayName);
  referenceSystemsFilter->SetGroupIdsArrayName(this->GroupIdsArrayName);
  referenceSystemsFilter->SetBlankingArrayName(this->BlankingArrayName);
  referenceSystemsFilter->SetNormalArrayName("Normal");
  referenceSystemsFilter->SetUpNormalArrayName("UpNormal");
  referenceSystemsFilter->Update();

  vtkPolyData* referenceSystems = referenceSystemsFilter->GetOutput();

  int numberOfMergedCells = outputLines->GetNumberOfCells();

  vtkIdList* blankedGroupIds = vtkIdList::New();
  vtkvmtkCenterlineUtilities::GetBlankedGroupsIdList(resampledCenterlines,this->GroupIdsArrayName,this->BlankingArrayName,blankedGroupIds);
  vtkIdList* groupIdsToBifurcationPointIds = vtkIdList::New();
  vtkIdTypeArray* cellAdditionalEndPointIds = vtkIdTypeArray::New();
  cellAdditionalEndPointIds->SetNumberOfComponents(2);
  cellAdditionalEndPointIds->SetNumberOfTuples(numberOfMergedCells);
  vtkIdType tupleValue[2];
  tupleValue[0] = tupleValue[1] = -1;
  for (i=0; i<numberOfMergedCells; i++)
    {
#if VTK_MAJOR_VERSION >= 7 && VTK_MINOR_VERSION >= 1
    cellAdditionalEndPointIds->SetTypedTuple(i, tupleValue);
#else
    cellAdditionalEndPointIds->SetTupleValue(i, tupleValue);
#endif
    }

  for (i=0; i<blankedGroupIds->GetNumberOfIds(); i++)
    {
    vtkIdType groupId = blankedGroupIds->GetId(i);

    vtkIdType referenceSystemPointId = vtkvmtkReferenceSystemUtilities::GetReferenceSystemPointId(referenceSystems,this->GroupIdsArrayName,groupId);

    vtkIdList* upStreamGroupIds = vtkIdList::New();
    vtkIdList* downStreamGroupIds = vtkIdList::New();

    vtkvmtkCenterlineUtilities::FindAdjacentCenterlineGroupIds(resampledCenterlines,this->GroupIdsArrayName,this->CenterlineIdsArrayName,this->TractIdsArrayName,groupId,upStreamGroupIds,downStreamGroupIds);

    double bifurcationPoint[3];
    referenceSystems->GetPoint(referenceSystemPointId,bifurcationPoint);

    vtkIdType bifurcationPointId = outputPoints->InsertNextPoint(bifurcationPoint);

    vtkIdType sourcePointId = -1;
    int j;
    for (j=0; j<upStreamGroupIds->GetNumberOfIds(); j++)
      {
      vtkIdType mergedCellId = groupIdsToMergedCells->GetId(upStreamGroupIds->GetId(j));
      if (mergedCellId == -1)
        {
        continue;
        }
      if (sourcePointId == -1)
        {
        vtkIdList* groupUniqueCellIds = vtkIdList::New();
        vtkvmtkCenterlineUtilities::GetGroupUniqueCellIds(resampledCenterlines,this->GroupIdsArrayName,upStreamGroupIds->GetId(j),groupUniqueCellIds);
        vtkCell* resampledCell = resampledCenterlines->GetCell(groupUniqueCellIds->GetId(0));
        sourcePointId = resampledCell->GetPointId(resampledCell->GetNumberOfPoints()-1);
        groupUniqueCellIds->Delete();
        }
      vtkIdType tupleValue[2];
#if VTK_MAJOR_VERSION >= 7 && VTK_MINOR_VERSION >= 1
      cellAdditionalEndPointIds->GetTypedTuple(mergedCellId,tupleValue);
#else
      cellAdditionalEndPointIds->GetTupleValue(mergedCellId, tupleValue);
#endif
      tupleValue[1] = bifurcationPointId;
#if VTK_MAJOR_VERSION >= 7 && VTK_MINOR_VERSION >= 1
      cellAdditionalEndPointIds->SetTypedTuple(mergedCellId, tupleValue);
#else
      cellAdditionalEndPointIds->SetTupleValue(mergedCellId, tupleValue);
#endif

      }

    for (j=0; j<downStreamGroupIds->GetNumberOfIds(); j++)
      {
      vtkIdType mergedCellId = groupIdsToMergedCells->GetId(downStreamGroupIds->GetId(j));
      if (mergedCellId == -1)
        {
        continue;
        }
      if (sourcePointId == -1)
        {
        vtkIdList* groupUniqueCellIds = vtkIdList::New();
        vtkvmtkCenterlineUtilities::GetGroupUniqueCellIds(resampledCenterlines,this->GroupIdsArrayName,downStreamGroupIds->GetId(j),groupUniqueCellIds);
        vtkCell* resampledCell = resampledCenterlines->GetCell(groupUniqueCellIds->GetId(0));
        sourcePointId = resampledCell->GetPointId(0);
        groupUniqueCellIds->Delete();
        }
      vtkIdType tupleValue[2];
#if VTK_MAJOR_VERSION >= 7 && VTK_MINOR_VERSION >= 1
      cellAdditionalEndPointIds->GetTypedTuple(mergedCellId, tupleValue);
#else
      cellAdditionalEndPointIds->GetTupleValue(mergedCellId,tupleValue);
#endif
      tupleValue[0] = bifurcationPointId;
#if VTK_MAJOR_VERSION >= 7 && VTK_MINOR_VERSION >= 1
      cellAdditionalEndPointIds->SetTypedTuple(mergedCellId, tupleValue);
#else
      cellAdditionalEndPointIds->SetTupleValue(mergedCellId,tupleValue);
#endif
    }
    if (sourcePointId == -1)
      {
      upStreamGroupIds->Delete();
      downStreamGroupIds->Delete();
      continue;
      }

    // TODO: interpolate point data instead of copying from first upstream point - good enough for now
    output->GetPointData()->CopyData(resampledCenterlines->GetPointData(),sourcePointId,bifurcationPointId);

    upStreamGroupIds->Delete();
    downStreamGroupIds->Delete();
    }

  vtkCellArray* extendedOutputLines = vtkCellArray::New();
  outputLines->InitTraversal();
  for (i=0; i<numberOfMergedCells; i++)
    {
    vtkIdType npts, *pts;
    npts = 0;
    pts = NULL;
    outputLines->GetNextCell(npts,pts);
    vtkIdType tupleValue[2];
#if VTK_MAJOR_VERSION >= 7 && VTK_MINOR_VERSION >= 1
    cellAdditionalEndPointIds->GetTypedTuple(i, tupleValue);
#else
    cellAdditionalEndPointIds->GetTupleValue(i,tupleValue);
#endif
    vtkIdType extendedNpts = npts;
    if (tupleValue[0] != -1)
      {
      extendedNpts += 1;
      }
    if (tupleValue[1] != -1)
      {
      extendedNpts += 1;
      }
    extendedOutputLines->InsertNextCell(extendedNpts);
    if (tupleValue[0] != -1)
      {
      extendedOutputLines->InsertCellPoint(tupleValue[0]);
      }
    int j;
    for (j=0; j<npts; j++)
      {
      extendedOutputLines->InsertCellPoint(pts[j]);
      }
    if (tupleValue[1] != -1)
      {
      extendedOutputLines->InsertCellPoint(tupleValue[1]);
      }
    }

  output->SetLines(extendedOutputLines);

  nonBlankedGroupIds->Delete();
  resampledCenterlines->Delete();
  outputPoints->Delete();
  outputLines->Delete();
  groupIdsToMergedCells->Delete();
  cellEndPointIds->Delete();

  referenceSystemsFilter->Delete();
  blankedGroupIds->Delete();
  groupIdsToBifurcationPointIds->Delete();
  cellAdditionalEndPointIds->Delete();
  extendedOutputLines->Delete();

  this->RemovePointsWithinBifurcationRadius(output);

  return SV_OK;
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVCenterlineMerger::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);
}

// ----------------------
// RemovePointsWithinBifurcationRadius
// ----------------------
int vtkSVCenterlineMerger::RemovePointsWithinBifurcationRadius(vtkPolyData *pd)
{
  pd->BuildLinks();

  vtkNew(vtkPoints, newPoints);
  vtkNew(vtkCellArray, newCells);
  vtkNew(vtkPointData, newPointData);
  vtkNew(vtkCellData, newCellData);

  newPointData->CopyAllocate(pd->GetPointData(),
                             pd->GetNumberOfPoints());
  newCellData->CopyAllocate(pd->GetCellData(),
                            pd->GetNumberOfPoints());

  vtkIdType nlinepts, *linepts;
  for (int i=0; i<pd->GetNumberOfCells(); i++)
  {
    pd->GetCellPoints(i, nlinepts, linepts);

    vtkNew(vtkIdList, frontNeighbors);
    pd->GetPointCells(linepts[0], frontNeighbors);

    vtkNew(vtkIdList, backNeighbors);
    pd->GetPointCells(linepts[nlinepts-1], backNeighbors);

    if (frontNeighbors->GetNumberOfIds() > 1)
    {
      double firstPt[3];
      pd->GetPoint(linepts[0], firstPt);
      double firstPtRadius = pd->GetPointData()->GetArray(this->RadiusArrayName)->GetTuple1(linepts[0]);


      int startPt = -1;
      double startPCoord = 0.0;
      double totalDist = 0.0;
      double pt0[3], pt1[3];
      double radius0, radius1;
      for (int j=0; j<nlinepts-1; j++)
      {
        pd->GetPoint(linepts[j], pt0);
        pd->GetPoint(linepts[j+1], pt1);

        double segmentLength = std::sqrt(vtkMath::Distance2BetweenPoints(pt0,pt1));
        totalDist += segmentLength;

        if (totalDist > firstPtRadius)
        {
          radius0 = pd->GetPointData()->GetArray(this->RadiusArrayName)->GetTuple1(linepts[j]);
          radius1 = pd->GetPointData()->GetArray(this->RadiusArrayName)->GetTuple1(linepts[j+1]);

          double stepSize, pcoordStepSize;
          int numberOfSteps;
          double subPt0[3], subPt1[3];
          double subLength;
          stepSize = 1E-2 * (radius0 + radius1) / 2.0;
          numberOfSteps = (int)ceil((segmentLength * segmentLength) / stepSize);
          stepSize = (segmentLength * segmentLength) / numberOfSteps;
          pcoordStepSize = 1.0 / (double)numberOfSteps;

          double subDist = 0.0;
          double pcoord = 0.0;
          int s;
          for (s=0; s<numberOfSteps; s++)
          {
            for (int d=0; d<3; d++)
            {
              subPt0[d] = pt0[d] + pcoord * (pt1[d] - pt0[d]);
              subPt1[d] = pt0[d] + (pcoord + pcoordStepSize) * (pt1[d] - pt0[d]);
            }

            subLength = std::sqrt(vtkMath::Distance2BetweenPoints(subPt0, subPt1));
            subDist += subLength;

            if (totalDist + subDist > firstPtRadius)
            {
              pcoord += pcoordStepSize;
              break;
            }

            pcoord += pcoordStepSize;
          }

          startPt = j;
          startPCoord = pcoord;
          break;
        }
      }

      if (startPt == -1)
      {
        startPt = 1;
        startPCoord = 0.0;
      }

      vtkNew(vtkIdList, newPointIds);
      int newPointId = newPoints->InsertNextPoint(firstPt);
      newPointData->CopyData(pd->GetPointData(), linepts[0], newPointId);
      newPointIds->InsertNextId(newPointId);

      double secondPt[3];
      pd->GetPoint(linepts[startPt], firstPt);
      pd->GetPoint(linepts[startPt+1], secondPt);

      double weights[2]; weights[0] = 1.0-startPCoord;  weights[1] = startPCoord;

      double newFirstPt[3];
      for (int j=0; j<3; j++)
      {
        newFirstPt[j] = firstPt[j] + startPCoord * (secondPt[j] - firstPt[j]);
      }

      newPointId = newPoints->InsertNextPoint(newFirstPt);
      newPointData->CopyData(pd->GetPointData(), linepts[startPt], newPointId);
      newPointIds->InsertNextId(newPointId);

      vtkNew(vtkIdList, interpIds);
      interpIds->SetNumberOfIds(2);
      interpIds->SetId(0, linepts[startPt]);
      interpIds->SetId(1, linepts[startPt+1]);

      for (int j=0; j<newPointData->GetNumberOfArrays(); j++)
      {

        newPointData->GetArray(j)->InterpolateTuple(newPointId,
            interpIds, pd->GetPointData()->GetArray(j), weights);
      }

      for (int j=startPt + 1; j<nlinepts; j++)
      {
        pd->GetPoint(linepts[j], pt0);
        newPointId = newPoints->InsertNextPoint(pt0);
        newPointData->CopyData(pd->GetPointData(), linepts[j], newPointId);
        newPointIds->InsertNextId(newPointId);
      }

      vtkNew(vtkPolyLine, newLine);
      newLine->GetPointIds()->DeepCopy(newPointIds);
      int newCellId = newCells->InsertNextCell(newLine);
      newCellData->CopyData(pd->GetCellData(), i, newCellId);
    }
    else
    {
      // Just copy points and data and whatnot
      int newPointId;
      vtkNew(vtkIdList, newPointIds);
      for (int j=0; j<nlinepts; j++)
      {
        newPointId = newPoints->InsertNextPoint(pd->GetPoint(linepts[j]));
        newPointData->CopyData(pd->GetPointData(), linepts[j], newPointId);
        newPointIds->InsertNextId(newPointId);
      }

      vtkNew(vtkPolyLine, newLine);
      newLine->GetPointIds()->DeepCopy(newPointIds);
      int newCellId = newCells->InsertNextCell(newLine);
      newCellData->CopyData(pd->GetCellData(), i, newCellId);
    }
  }

  newPointData->Squeeze();
  newCellData->Squeeze();

  pd->Reset();
  pd->SetPoints(newPoints);
  pd->SetLines(newCells);

  pd->GetPointData()->PassData(newPointData);
  pd->GetCellData()->PassData(newCellData);

  vtkNew(vtkCleanPolyData, cleaner);
  cleaner->SetInputData(pd);
  cleaner->Update();

  pd->DeepCopy(cleaner->GetOutput());

  return SV_OK;
}
