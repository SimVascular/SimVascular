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

#include "vtkSVUpdeSmoothing.h"

#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkObjectFactory.h"
#include "vtkPolyData.h"
#include "vtkCellArray.h"
#include "vtkFeatureEdges.h"
#include "vtkIntArray.h"
#include "vtkDoubleArray.h"
#include "vtkCellData.h"
#include "vtkPointData.h"
#include "vtkSmartPointer.h"
#include "vtkSmoothPolyDataFilter.h"
#include "vtkFloatArray.h"
#include "vtkPolyDataNormals.h"
#include "vtkGenericCell.h"
#include "vtkLine.h"
#include "vtkMath.h"
#include "vtkTriangle.h"
#include "vtkTriangleFilter.h"
#include "vtkXMLPolyDataWriter.h"
#include "vtkWindowedSincPolyDataFilter.h"

#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"
#include "vtkSVMathUtils.h"
#include "vtkSVLocalSmoothPolyDataFilter.h"

#include <iostream>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVUpdeSmoothing);

// ----------------------
// Constructor
// ----------------------
vtkSVUpdeSmoothing::vtkSVUpdeSmoothing()
{
  this->WorkPd = vtkPolyData::New();
  this->SourcePd = NULL;

  this->CellLocator = vtkCellLocator::New();
  this->SourceCellNormals = NULL;
  this->SourcePointNormals = NULL;
  this->OriginalCellNormals = NULL;
  this->OriginalPointNormals = NULL;

  this->UseInputAsSource = 0;
  this->NumberOfOuterSmoothOperations = 1;
  this->NumberOfInnerSmoothOperations = 500;
  this->Alpha = 0.5;
  this->Beta = 0.8;

  this->SmoothPointArrayName = NULL;
}

// ----------------------
// Destructor
// ----------------------
vtkSVUpdeSmoothing::~vtkSVUpdeSmoothing()
{
  if (this->WorkPd != NULL)
  {
    this->WorkPd->Delete();
    this->WorkPd = NULL;
  }

  if (this->SourcePd != NULL)
  {
    this->SourcePd->Delete();
    this->SourcePd = NULL;
  }

  if (this->CellLocator != NULL)
  {
    this->CellLocator->Delete();
    this->CellLocator = NULL;
  }

  if (this->SmoothPointArrayName != NULL)
  {
    delete [] this->SmoothPointArrayName;
    this->SmoothPointArrayName = NULL;
  }
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVUpdeSmoothing::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);

  os << indent << "Number of outer smooth operations: " << this->NumberOfOuterSmoothOperations << "\n";
  os << indent << "Number of inner smooth operations: " << this->NumberOfInnerSmoothOperations << "\n";
}

// ----------------------
// RequestData
// ----------------------
int vtkSVUpdeSmoothing::RequestData(vtkInformation *vtkNotUsed(request),
                                           vtkInformationVector **inputVector,
                                           vtkInformationVector *outputVector)
{
  // get the input and output
  vtkPolyData *input = vtkPolyData::GetData(inputVector[0]);
  vtkPolyData *output = vtkPolyData::GetData(outputVector);

  // Define variables used by the algorithm
  vtkNew(vtkPoints, inpts);
  vtkNew(vtkCellArray, inPolys);
  vtkIdType numPts, numPolys;
  vtkIdType newId, cellId,pointId;

  //Get input points, polys and set the up in the vtkPolyData mesh
  inpts = input->GetPoints();
  inPolys = input->GetPolys();

  //Get the number of Polys for scalar  allocation
  numPolys = input->GetNumberOfPolys();
  numPts = input->GetNumberOfPoints();

  //Check the input to make sure it is there
  if (numPolys < 1)
  {
      vtkDebugMacro("No input!");
      return SV_OK;
  }

  this->WorkPd->DeepCopy(input);
  this->WorkPd->BuildLinks();

  vtkSVGeneralUtils::GiveIds(this->WorkPd, "TmpInternalIds");

  if (this->UseInputAsSource)
  {
    if (this->SourcePd == NULL)
    {
      this->SourcePd = vtkPolyData::New();
    }

    vtkNew(vtkTriangleFilter, triangulator);
    triangulator->SetInputData(this->WorkPd);
    triangulator->Update();

    //this->SourcePd->DeepCopy(this->WorkPd);
    this->SourcePd->DeepCopy(triangulator->GetOutput());
    this->SourcePd->BuildLinks();

    //srand(time(NULL));
    //for (int i=0; i<this->SourcePd->GetNumberOfPoints(); i++)
    //{
    //  if (i != 12 && i != 22 && i != 27 && i != 33)
    //    continue;

    //  fprintf(stdout,"WHWHW: %d\n", i);
    //  double x = 6.0 * ((double) rand()) / RAND_MAX;
    //  double y = 6.0 * ((double) rand()) / RAND_MAX;
    //  fprintf(stdout,"RAND X: %.6f\n", x);
    //  fprintf(stdout,"RAND Y: %.6f\n", y);
    //  double z = 0.0;

    //  this->SourcePd->GetPoints()->SetPoint(i, x, y, z);
    //}

    //vtkNew(vtkXMLPolyDataWriter, writer);
    //writer->SetInputData(this->SourcePd);
    //writer->SetFileName("/Users/adamupdegrove/Desktop/tried.vtp");
    //writer->Write();
  }

  // Build locator if source given
  if (this->SourcePd != NULL)
  {
    this->CellLocator->SetDataSet(this->SourcePd);
    this->CellLocator->BuildLocator();

    vtkNew(vtkPolyDataNormals, sNormaler);
    sNormaler->SetInputData(this->SourcePd);
    sNormaler->SplittingOff();
    sNormaler->ComputeCellNormalsOn();
    sNormaler->ComputePointNormalsOn();
    sNormaler->FlipNormalsOn();
    sNormaler->Update();

    this->SourcePd->DeepCopy(sNormaler->GetOutput());
    this->SourcePd->BuildLinks();

    this->SourceCellNormals =
      this->SourcePd->GetCellData()->GetArray("Normals");
    this->SourcePointNormals =
      this->SourcePd->GetPointData()->GetArray("Normals");
  }

  vtkNew(vtkPolyDataNormals, oNormaler);
  oNormaler->SetInputData(this->WorkPd);
  oNormaler->SplittingOff();
  oNormaler->ComputeCellNormalsOn();
  oNormaler->ComputePointNormalsOn();
  oNormaler->Update();

  this->WorkPd->DeepCopy(oNormaler->GetOutput());
  this->WorkPd->BuildLinks();

  this->OriginalPointNormals =
    this->WorkPd->GetPointData()->GetArray("Normals");
  this->OriginalCellNormals =
    this->WorkPd->GetCellData()->GetArray("Normals");

  // Set fixed points
  this->FixedPoints.clear();
  this->FixedPoints.resize(numPts, 0);

  // ========================= FEATURE EDGES ===============================
  this->SetSmoothPointArrayName("SmoothPoints");
  vtkNew(vtkPolyData, idPd);
  idPd->DeepCopy(this->WorkPd);
  vtkSVGeneralUtils::GiveIds(idPd, "TmpInternalIds");

  this->SetSmoothPointArrayName("SmoothPoints");
  vtkNew(vtkIntArray, tmpSmoothPointArray);
  tmpSmoothPointArray->SetNumberOfTuples(idPd->GetNumberOfPoints());
  tmpSmoothPointArray->FillComponent(0, 1);
  tmpSmoothPointArray->SetName("SmoothPoints");

  vtkNew(vtkFeatureEdges, featurer);
  featurer->SetInputData(idPd);
  featurer->NonManifoldEdgesOff();
  featurer->ManifoldEdgesOff();
  featurer->BoundaryEdgesOff();
  featurer->FeatureEdgesOn();
  featurer->SetFeatureAngle(70.0);
  featurer->Update();

  vtkNew(vtkPolyData, featurePd);
  featurePd->DeepCopy(featurer->GetOutput());

  int realPtId;
  for (int i=0; i<featurePd->GetNumberOfPoints(); i++)
  {
    realPtId = featurePd->GetPointData()->GetArray("TmpInternalIds")->GetTuple1(i);
    this->FixedPoints[realPtId] = 1;
    tmpSmoothPointArray->SetTuple1(realPtId, 0);
  }
  this->WorkPd->GetPointData()->AddArray(tmpSmoothPointArray);
  // ========================= FEATURE EDGES ===============================


  if (this->SmoothPointArrayName != NULL)
  {
    this->SmoothPointArray = vtkIntArray::SafeDownCast(this->WorkPd->GetPointData()->GetArray(this->SmoothPointArrayName));

    if (this->SmoothPointArray == NULL)
    {
      vtkErrorMacro("Error getting array indicating the points to smooth on mesh");
      return SV_ERROR;
    }

    for (int i=0; i<this->WorkPd->GetNumberOfPoints(); i++)
    {
      pointId = this->SmoothPointArray->GetTuple1(i);

      if (pointId != 1)
      {
        this->FixedPoints[pointId] = 1;
      }
    }
  }

  //// =========================== INITIAL ONE ===============================
  //if (this->SourcePd != NULL)
  //{
  //  int subId;
  //  double pt[3];
  //  double a0, a1, a2;
  //  double closestPt[3], distance;
  //  double closeTriPts[3][3], xyTriPts[3][3];
  //  vtkIdType npts, *pts;
  //  vtkIdType closestCellId;
  //  vtkNew(vtkGenericCell, genericCell);

  //  vtkNew(vtkIntArray, pointCase);
  //  pointCase->SetNumberOfTuples(numPts);
  //  this->CellsOnSource.clear();
  //  this->CellsOnSource.resize(numPts);
  //  for (int i=0; i<numPts; i++)
  //  {
  //    this->WorkPd->GetPoint(i, pt);
  //    this->CellLocator->FindClosestPoint(pt, closestPt, genericCell, closestCellId, subId, distance);

  //    // Get bary coordinates of tri
  //    this->SourcePd->GetCellPoints(closestCellId, npts, pts);
  //    for (int j=0; j<3; j++)
  //    {
  //      this->SourcePd->GetPoint(pts[j], closeTriPts[j]);
  //    }
  //    vtkSVGeneralUtils::GetBarycentricCoordinates(closestPt, closeTriPts[0],
  //                                                 closeTriPts[1], closeTriPts[2],
  //                                                 a0, a1, a2);

  //    cellEdgeNeighbors->Reset();
  //    allCapableNeighbors->Reset();

  //    if (a0 > -1.0e-5 && a0 < 1.0e-5)
  //    {
  //      this->SourcePd->GetCellEdgeNeighbors(-1, pts[0], pts[1], cellEdgeNeighbors);
  //      for (int j=0; j<cellEdgeNeighbors->GetNumberOfIds(); j++)
  //      {
  //        allCapableNeighbors->InsertUniqueId(cellEdgeNeighbors->GetId(j));
  //      }
  //    }
  //    if (a1 > -1.0e-5 && a1 < 1.0e-5)
  //    {
  //      this->SourcePd->GetCellEdgeNeighbors(-1, pts[1], pts[2], cellEdgeNeighbors);
  //      for (int j=0; j<cellEdgeNeighbors->GetNumberOfIds(); j++)
  //      {
  //        allCapableNeighbors->InsertUniqueId(cellEdgeNeighbors->GetId(j));
  //      }
  //    }
  //    if (a2 > -1.0e-5 && a2 < 1.0e-5)
  //    {
  //      this->SourcePd->GetCellEdgeNeighbors(-1, pts[0], pts[2], cellEdgeNeighbors);
  //      for (int j=0; j<cellEdgeNeighbors->GetNumberOfIds(); j++)
  //      {
  //        allCapableNeighbors->InsertUniqueId(cellEdgeNeighbors->GetId(i));
  //      }
  //    }

  //    for (int j=0; j<allCapableNeighbors->GetNumberOfIds(); j++)
  //    {
  //      this->CellsOnSource[i].push_back(allCapableNeighbors->GetId(j));
  //    }

  //  }

  //  this->WorkPd->GetPointData()->AddArray(pointCase);

  //}
  //// =========================== INITIAL ONE ===============================

  //// =========================== TEST UNT ===============================
  int numCells = this->WorkPd->GetNumberOfCells();
  this->WorkPd->BuildLinks();

  vtkNew(vtkDoubleArray, shapeImproveFunction);
  shapeImproveFunction->SetNumberOfTuples(numPts);
  shapeImproveFunction->SetName("ShapeImproveFunction");

  vtkNew(vtkDoubleArray, shapeImproveDirection);
  shapeImproveDirection->SetNumberOfComponents(3);
  shapeImproveDirection->SetNumberOfTuples(numPts);
  shapeImproveDirection->SetName("ShapeImproveDirection");

  this->PointCells.clear();
  this->PointCells.resize(numPts);
  vtkNew(vtkIdList, pointCellIds);
  for (int i=0; i<numPts; i++)
  {
    this->WorkPd->GetPointCells(i, pointCellIds);
    for (int j=0; j<pointCellIds->GetNumberOfIds(); j++)
    {
      this->PointCells[i].push_back(pointCellIds->GetId(j));
    }
  }

  this->CellPoints.clear();
  this->CellPoints.resize(numPts);
  vtkIdType npts, *pts;
  for (int i=0; i<numCells; i++)
  {
    this->WorkPd->GetCellPoints(i, npts, pts);
    for (int j=0; j<npts; j++)
    {
      this->CellPoints[i].push_back(pts[j]);
    }
  }

  int allGood = 0;
  int maxIters = 1;
  int iter = 0;

  while (!allGood && iter < maxIters + 1)
  {
    fprintf(stdout,"OUTER ITER: %d\n", iter);
    allGood = 1;
    if ( this->UntangleSurface(shapeImproveFunction, shapeImproveDirection) != SV_OK)
    {
      vtkErrorMacro("Failed to untagnel surface in maximum number of iterations\n");
      //return SV_ERROR;
    }

    if (iter < maxIters)
    {
      if (this->SmoothSurface(shapeImproveFunction, shapeImproveDirection) != SV_OK)
      {
        allGood = 0;
      }
    }
    iter++;
  }

  if (!allGood)
  {
    vtkErrorMacro("Failed to smooth surface to desired tolerance in maximum number of iterations\n");
    //return SV_ERROR;
  }

  this->WorkPd->GetPointData()->AddArray(shapeImproveFunction);
  this->WorkPd->GetPointData()->AddArray(shapeImproveDirection);
  //// =========================== TEST UNT ===============================

  std::vector<std::vector<int> > connectedCellIds(numPts);
  std::vector<std::vector<int> > connectedPointIds(numPts);
  vtkNew(vtkIdList, usedPtIds);
  for (int i=0; i<numPts; i++)
  {
    //fprintf(stdout,"POINT %d of %d\n", i, numPts);
    usedPtIds->Reset();
    this->WorkPd->GetPointCells(i, pointCellIds);

    for (int j=0; j<pointCellIds->GetNumberOfIds(); j++)
    {
      cellId = pointCellIds->GetId(j);
      connectedCellIds[i].push_back(cellId);

      this->WorkPd->GetCellPoints(cellId, npts, pts);

      for (int k=0; k<npts; k++)
      {
        pointId = pts[k];

        if (usedPtIds->IsId(pointId) == -1)
        {
          usedPtIds->InsertNextId(pointId);
          connectedPointIds[i].push_back(pointId);
        }
      }
    }
  }

  this->WorkPd->BuildLinks();
  vtkNew(vtkPolyData, tmp);
  tmp->DeepCopy(this->WorkPd);

  vtkNew(vtkPolyData, savePd);
  for (int i=0; i<this->NumberOfOuterSmoothOperations; i++)
  {
    fprintf(stdout,"SMOOTIHNG ITER: %d\n", i);
    savePd->DeepCopy(tmp);

    vtkNew(vtkSVLocalSmoothPolyDataFilter, smoother);
    smoother->SetInputData(tmp);
    smoother->SetNumberOfIterations(this->NumberOfInnerSmoothOperations);
    if (this->SmoothPointArrayName != NULL)
    {
      smoother->SetUsePointArray(1);
      smoother->SetSmoothPointArrayName(this->SmoothPointArrayName);
    }
    //if (this->SourcePd != NULL)
    //{
    //  smoother->SetSourceData(this->SourcePd);
    //}
    smoother->Update();

    tmp->DeepCopy(smoother->GetOutput());

    vtkNew(vtkPolyDataNormals, tmpNormaler);
    tmpNormaler->SetInputData(tmp);
    tmpNormaler->SplittingOff();
    tmpNormaler->ComputeCellNormalsOn();
    tmpNormaler->ComputePointNormalsOn();
    tmpNormaler->Update();

    vtkDataArray *tPtNormals =
      tmpNormaler->GetOutput()->GetPointData()->GetArray("Normals");
    vtkDataArray *tCellNormals =
      tmpNormaler->GetOutput()->GetCellData()->GetArray("Normals");

    //// ================ OLD FIND CLOSEST ===================================
    //if (this->SourcePd != NULL)
    //{
    //  int numPts = tmp->GetNumberOfPoints();
    //  int subId;
    //  double smoothPt[3];
    //  double closestPt[3], distance;
    //  vtkIdType closestCellId;
    //  vtkNew(vtkGenericCell, genericCell);

    //  double moveDot;
    //  double oNormal[3];
    //  double moveVec[3];
    //  for (int j=0; j<numPts; j++)
    //  {
    //    tmp->GetPoint(j, smoothPt);
    //    this->CellLocator->FindClosestPoint(smoothPt, closestPt, genericCell, closestCellId, subId, distance);
    //    vtkMath::Subtract(smoothPt, closestPt, moveVec);
    //    vtkMath::Normalize(moveVec);

    //    this->OriginalPointNormals->GetTuple(j, oNormal);

    //    moveDot = vtkMath::Dot(oNormal, moveVec);
    //    if (moveDot < 0)
    //    {
    //      tmp->GetPoints()->SetPoint(j, smoothPt);
    //    }
    //    else
    //    {
    //      tmp->GetPoints()->SetPoint(j, closestPt);
    //    }
    //  }
    //}
    //// ================ OLD FIND CLOSEST ===================================

    //if (this->SourcePd != NULL)
    //{
    //  int worked;
    //  int numPts = tmp->GetNumberOfPoints();
    //  int subId;
    //  double t;
    //  double pCoords[3];
    //  double lineEndPt[3];
    //  double smoothPt[3];
    //  double closestPt[3], distance;
    //  double closestPtAlongLine[3];
    //  vtkIdType closestCellId;
    //  vtkNew(vtkGenericCell, genericCell);

    //  double moveDot;
    //  double oNormal[3];
    //  double tNormal[3];
    //  double moveVec[3];
    //  for (int j=0; j<numPts; j++)
    //  {
    //    tmp->GetPoint(j, smoothPt);
    //    this->CellLocator->FindClosestPoint(smoothPt, closestPt, genericCell, closestCellId, subId, distance);
    //    vtkMath::Subtract(smoothPt, closestPt, moveVec);
    //    vtkMath::Normalize(moveVec);

    //    this->OriginalPointNormals->GetTuple(j, oNormal);

    //    moveDot = vtkMath::Dot(oNormal, moveVec);

    //    tPtNormals->GetTuple(j, tNormal);

    //    if (moveDot < 0)
    //    {
    //      vtkMath::MultiplyScalar(tNormal, length);
    //    }
    //    else
    //    {
    //      vtkMath::MultiplyScalar(tNormal, -1.0*length);
    //    }

    //    vtkMath::Add(smoothPt, tNormal, lineEndPt);

    //    worked  = this->CellLocator->IntersectWithLine(smoothPt, lineEndPt, 1.0e-6, t, closestPtAlongLine, pCoords, subId, closestCellId, genericCell);
    //    if (worked)
    //    {
    //      tmp->GetPoints()->SetPoint(j, closestPtAlongLine);
    //    }
    //    else
    //    {
    //      vtkWarningMacro("Was not able to find point on source surface along smoothed point normal vector. NumberOfInnerSmoothOperations is likely set too high");
    //      tmp->GetPoints()->SetPoint(j, closestPt);
    //    }
    //  }
    //}


    //vtkNew(vtkPolyData, smoothedPd);
    //smoothedPd->DeepCopy(smoother->GetOutput());

    //vtkNew(vtkPolyDataNormals, smoothNormaler);
    //smoothNormaler->SetInputData(smoothedPd);
    //smoothNormaler->SplittingOff();
    //smoothNormaler->ComputeCellNormalsOn();
    //smoothNormaler->ComputePointNormalsOn();
    //smoothNormaler->Update();

    //vtkDataArray *sPtNormals =
    //  smoothNormaler->GetOutput()->GetPointData()->GetArray("Normals");
    //vtkDataArray *this->SourceCellNormals =
    //  smoothNormaler->GetOutput()->GetCellData()->GetArray("Normals");

    //int numPoints = tmp->GetNumberOfPoints();

    //smoothedPd->BuildLinks();

    //double maxAngle = -1.0;
    //double prevMaxAngle = 95.0;
    //for (int i=0; i<numPoints; i++)
    //{
    //  if (this->FixedPoints[i])
    //  {
    //    continue;
    //  }

    //  double oNormal[3];
    //  this->OriginalPointNormals->GetTuple(i, oNormal);

    //  double tNormal[3];
    //  tPtNormals->GetTuple(i, tNormal);

    //  double sNormal[3];
    //  sPtNormals->GetTuple(i, sNormal);

    //  double oPt[3];
    //  this->WorkPd->GetPoint(i, oPt);

    //  double tPt[3];
    //  tmp->GetPoint(i, tPt);

    //  double sPt[3];
    //  smoothedPd->GetPoint(i, sPt);

    //  // ======================OLD ALPHA BETA CODE==========================
    //  //double oVec[3];
    //  //for (int j=0; j<3; j++)
    //  //  oVec[j] = sPt[j] - (this->Alpha * oPt[j] + ((1 - this->Alpha) * tPt[j]));

    //  //double normalDot = vtkMath::Dot(sNormal, oVec);
    //  //vtkMath::MultiplyScalar(sNormal, normalDot);

    //  //double tangVec[3];
    //  //vtkMath::Subtract(oVec, sNormal, tangVec);
    //  // ======================OLD ALPHA BETA CODE==========================

    //  // ======================TRYING NEIGHBOR AVG=========================

    //  // POINT AVERAGE
    //  int numNeighs = 0;
    //  double avgAngleDiff = 0;
    //  double neighborNormal[3];
    //  std::vector<double> neighborAngleDiffs(connectedPointIds[i].size());
    //  for (int j=0; j<connectedPointIds[i].size(); j++)
    //  {
    //    if (this->FixedPoints[connectedPointIds[i][j]])
    //    {
    //      continue;
    //    }

    //    tPtNormals->GetTuple(connectedPointIds[i][j], neighborNormal);

    //    neighborAngleDiffs[j] = vtkMath::AngleBetweenVectors(tNormal, neighborNormal);

    //    avgAngleDiff += neighborAngleDiffs[j];

    //    numNeighs++;
    //  }

    //  if (numNeighs != 0)
    //  {
    //    avgAngleDiff /= numNeighs;
    //  }
    //  else
    //  {
    //    avgAngleDiff = 0.0;
    //  }

    //  // =============================STD DEV===============================
    //  //double stdDevAngleDiff = 0;
    //  //for (int j=0; j<connectedPointIds[i].size(); j++)
    //  //{
    //  //  stdDevAngleDiff += std::pow(neighborAngleDiffs[j] - avgAngleDiff, 2);
    //  //}

    //  //stdDevAngleDiff = std::sqrt(stdDevAngleDiff / connectedPointIds[i].size());

    //  //fprintf(stdout,"AVG ANGLE %.6f\n", 180.0*avgAngleDiff/SV_PI);
    //  //fprintf(stdout,"STD DEV ANGLE %.6f\n", 180.0*stdDevAngleDiff/SV_PI);
    //  // =============================STD DEV===============================

    //  //// CELL AVERAGE
    //  //double avgAngleDiff = 0;
    //  //double neighborNormal[3];
    //  //std::vector<double> neighborAngleDiffs(connectedCellIds[i].size());
    //  //for (int j=0; j<connectedCellIds[i].size(); j++)
    //  //{
    //  //  tCellNormals->GetTuple(connectedCellIds[i][j], neighborNormal);

    //  //  neighborAngleDiffs[j] = vtkMath::AngleBetweenVectors(tNormal, neighborNormal);

    //  //  avgAngleDiff += neighborAngleDiffs[j];
    //  //}

    //  //avgAngleDiff /= connectedCellIds[i].size();


    //  double sVec[3];
    //  vtkMath::Subtract(sPt, tPt, sVec);

    //  double normalDot = vtkMath::Dot(tNormal, sVec);
    //  vtkMath::MultiplyScalar(tNormal, normalDot);

    //  double tangVec[3];
    //  vtkMath::Subtract(sVec, tNormal, tangVec);

    //  // ======================OLD ALPHA BETA CODE==========================
    //  //double addVec[3];
    //  //for (int j=0; j<3; j++)
    //  //  addVec[j] = this->Beta * oVec[j] + ((1 - this->Beta) * oNormal[j]);

    //  //double finalPt[3];
    //  //for (int j=0; j<3; j++)
    //  //  finalPt[j] = sPt[j] - addVec[j];
    //  // ======================OLD ALPHA BETA CODE==========================

    //  double tangPt[3];
    //  vtkMath::Add(tPt, tangVec, tangPt);

    //  avgAngleDiff = avgAngleDiff * 180.0/SV_PI;
    //  if (avgAngleDiff > maxAngle)
    //    maxAngle = avgAngleDiff;

    //  double lowStop = 10.0;
    //  double highStop = 30.0;

    //  if (avgAngleDiff > highStop)
    //    avgAngleDiff = highStop;
    //  if (avgAngleDiff < lowStop)
    //    avgAngleDiff = lowStop;

    //  double moveValue = (avgAngleDiff-lowStop)/(highStop - lowStop);

    //  double moveVec[3];
    //  vtkMath::Subtract(sPt, tangPt, moveVec);

    //  vtkMath::MultiplyScalar(moveVec, moveValue);

    //  double finalPt[3];
    //  vtkMath::Add(tangPt, moveVec, finalPt);

    //  tmp->GetPoints()->SetPoint(i, finalPt);

    //}

    //fprintf(stdout,"MAX ANGLE DIFF: %.6f\n", maxAngle);
    //if (maxAngle > prevMaxAngle)
    //{
    //  tmp->DeepCopy(savePd);
    //  break;
    //}

    //prevMaxAngle = maxAngle;
  }

  //output->DeepCopy(tmp);
  output->DeepCopy(this->WorkPd);
  output->GetCellData()->RemoveArray("TmpInternalIds");
  output->GetPointData()->RemoveArray("TmpInternalIds");

  return SV_OK;
}

// ----------------------
// UntangleSurface
// ----------------------
int vtkSVUpdeSmoothing::UntangleSurface(vtkDoubleArray *shapeImproveFunction,
                                        vtkDoubleArray *shapeImproveDirection)
{
  int numPolys = this->WorkPd->GetNumberOfPolys();
  int numPts = this->WorkPd->GetNumberOfPoints();

  int pointCellStatus;
  double pointImprove = 0;
  double pointImproveDir[3];
  double testPointImprove = 0;
  double testPointImproveDir[3];

  int subId;
  double pt0[3];
  double normDot;
  double newPt[3];
  double normal[3];
  double sourcePts[3][3];
  double tangentImproveDir[3];
  double closestPt[3], distance;
  vtkIdType closestCellId;
  vtkNew(vtkGenericCell, genericCell);

  vtkIdType nspts, *spts;
  vtkIdType ntpts, *tpts;
  vtkNew(vtkIdList, allCapableNeighbors);
  vtkNew(vtkIdList, cellEdgeNeighbors);
  double maxBad = 0.0;
  double avgBad = 0.0;
  int lesser = 0;
  int greater = 0;

  double moveStep = 0.001;

  int allGood = 0;
  int maxIters = 100;
  int iter = 0;

  while(!allGood && iter < maxIters)
  {
    allGood = 1;

    // TODO ADJUST FOR NO SOURCE BEING PROVIDED
    // First make sure there are no inverted points
    for (int i=0; i<numPts; i++)
    {
      if (this->FixedPoints[i])
      {
        continue;
      }

      pointImprove = 0;
      for (int j=0; j<3; j++)
      {
        pointImproveDir[j] = 0.0;
      }

      this->WorkPd->GetPoint(i, pt0);

      if (this->SourcePd != NULL)
      {
        this->CellLocator->FindClosestPoint(pt0, closestPt, genericCell, closestCellId, subId, distance);

        pointCellStatus = 0;
        if (this->PointCellStatus(closestPt, closestCellId, pointCellStatus)  != SV_OK)
        {
          vtkErrorMacro("Point is technically outside the cell it was found to be closest to");
          return SV_ERROR;
        }

        this->SourceCellNormals->GetTuple(closestCellId, normal);
      }
      else
      {
        this->OriginalPointNormals->GetTuple(i, normal);
      }

      this->CheckVertexInverted(i, normal, pointImprove, pointImproveDir);

      if (pointImprove != 0.0)
      {
        allGood = 0;

        vtkMath::Normalize(pointImproveDir);
        //vtkMath::MultiplyScalar(pointImproveDir, -1.0);

        fprintf(stdout,"MOVING POINT: %d\n", i);
        fprintf(stdout,"POINT FIX INVERSION DIR: %.6f %.6f %.6f\n", pointImproveDir[0], pointImproveDir[1], pointImproveDir[2]);

        //this->SourcePointNormals->GetTuple(i, normal);
        vtkMath::Normalize(normal);
        normDot = vtkMath::Dot(pointImproveDir, normal);

        vtkMath::MultiplyScalar(normal, normDot);
        //fprintf(stdout,"NORM DOT: %6f\n", normDot);
        vtkMath::Subtract(pointImproveDir, normal, tangentImproveDir);
        vtkMath::Normalize(tangentImproveDir);

        vtkMath::MultiplyScalar(tangentImproveDir, moveStep);
        vtkMath::Add(pt0, tangentImproveDir, newPt);
        fprintf(stdout,"TANGENT IMP DIR: %.6f %.6f %.6f\n", tangentImproveDir[0], tangentImproveDir[1], tangentImproveDir[2]);

        this->WorkPd->GetPoints()->SetPoint(i, newPt);
        //this->CheckVertexInverted(i, testPointImprove, testPointImproveDir);

        //if (testPointImprove > pointImprove)
        //{
        //  this->WorkPd->GetPoints()->SetPoint(i, pt0);
        //}
      }

      shapeImproveDirection->SetTuple(i, tangentImproveDir);
      shapeImproveFunction->SetTuple1(i, pointImprove);
    }
    iter++;
  }

  if (!allGood)
  {
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// SmoothSurface
// ----------------------
int vtkSVUpdeSmoothing::SmoothSurface(vtkDoubleArray *shapeImproveFunction,
                                      vtkDoubleArray *shapeImproveDirection)
{
  int numPolys = this->WorkPd->GetNumberOfPolys();
  int numPts = this->WorkPd->GetNumberOfPoints();

  int dirWorks;
  int pointCellStatus;
  int cellId, pointIndex;
  double pointImprove = 0;
  double pointImproveDir[3];
  double testPointImprove = 0;
  double testPointImproveDir[3];
  double testPt[3];

  int subId;
  int edgeStatus;
  double pt0[3];
  double normDot;
  double newPt[3];
  double normal[3];
  double smoothPt[3];
  double sourcePts[3][3];
  double tangentImproveDir[3];
  double closestPt[3], distance;
  vtkIdType closestCellId;
  vtkNew(vtkGenericCell, genericCell);

  vtkIdType nspts, *spts;
  vtkIdType ntpts, *tpts;
  vtkNew(vtkIdList, allCapableNeighbors);
  vtkNew(vtkIdList, cellEdgeNeighbors);
  double maxBad = 0.0;
  double avgBad = 0.0;
  int lesser = 0;
  int greater = 0;

  double moveStep = 0.001;

  int allGood = 0;
  int maxIters = 100;
  int iter = 0;

  while (!allGood && iter < maxIters)
  {
    maxBad = 0;
    avgBad = 0;
    lesser = 0;
    greater = 0;
    for (int i=0; i<numPts; i++)
    {
      if (this->FixedPoints[i])
      {
        continue;
      }

      pointImprove = 0;
      for (int j=0; j<3; j++)
      {
        pointImproveDir[j] = 0.0;
      }

      this->ComputeVertexCondition(i, pointImprove, pointImproveDir);
      vtkMath::Normalize(pointImproveDir);
      vtkMath::MultiplyScalar(pointImproveDir, -1.0);

      fprintf(stdout,"MOVING POINT: %d\n", i);
      fprintf(stdout,"POINT IMP DIR: %.6f %.6f %.6f\n", pointImproveDir[0], pointImproveDir[1], pointImproveDir[2]);
//=============================ONE==========================================
      if (this->SourcePd == NULL)
      {
        vtkMath::MultiplyScalar(pointImproveDir, moveStep);
        this->WorkPd->GetPoint(i, pt0);
        vtkMath::Add(pt0, pointImproveDir, newPt);

        this->WorkPd->GetPoints()->SetPoint(i, newPt);
        this->ComputeVertexCondition(i, testPointImprove, testPointImproveDir);

        if (testPointImprove > pointImprove)
        {
          this->WorkPd->GetPoints()->SetPoint(i, pt0);
          greater++;
        }
        else
        {
          lesser++;
        }
      }

//=============================TWO==========================================

      //this->SourcePointNormals->GetTuple(i, normal);
      //vtkMath::Normalize(normal);
      //normDot = vtkMath::Dot(pointImproveDir, normal);

      //vtkMath::MultiplyScalar(normal, normDot);
      ////fprintf(stdout,"NORM DOT: %6f\n", normDot);
      //vtkMath::Subtract(pointImproveDir, normal, tangentImproveDir);
      //vtkMath::Normalize(tangentImproveDir);

      //vtkMath::MultiplyScalar(tangentImproveDir, moveStep);
      //this->WorkPd->GetPoint(i, pt0);
      //vtkMath::Add(pt0, tangentImproveDir, newPt);
      //fprintf(stdout,"TANGENT IMP DIR: %.6f %.6f %.6f\n", tangentImproveDir[0], tangentImproveDir[1], tangentImproveDir[2]);

      //this->WorkPd->GetPoints()->SetPoint(i, newPt);
      //this->ComputeVertexCondition(i, testPointImprove, testPointImproveDir);

      //if (testPointImprove > pointImprove)
      //{
      //  this->WorkPd->GetPoints()->SetPoint(i, pt0);
      //  greater++;
      //}
      //else
      //{
      //  lesser++;
      //}

//=============================THREE==========================================
      else
      {
        // Now time to move point
        this->WorkPd->GetPoint(i, pt0);

        this->CellLocator->FindClosestPoint(pt0, closestPt, genericCell, closestCellId, subId, distance);
        //fprintf(stdout,"  CLOSEST CELL: %d\n", closestCellId);

        pointCellStatus = 0;
        if (this->PointCellStatus(closestPt, closestCellId, pointCellStatus)  != SV_OK)
        {
          vtkErrorMacro("Point is technically outside the cell it was found to be closest to");
          return SV_ERROR;
        }

        fprintf(stdout,"  POINT CELL STATUS: %d\n", pointCellStatus);

        allCapableNeighbors->Reset();

        this->SourcePd->GetCellPoints(closestCellId, nspts, spts);

        // Within cell
        allCapableNeighbors->InsertNextId(closestCellId);

        // On edges
        if (pointCellStatus == 1)
        {
          fprintf(stdout,"  ON EDGE\n");
          cellEdgeNeighbors->Reset();
          this->SourcePd->GetCellEdgeNeighbors(closestCellId, spts[1], spts[2], cellEdgeNeighbors);
          for (int k=0; k<cellEdgeNeighbors->GetNumberOfIds(); k++)
          {
            allCapableNeighbors->InsertNextId(cellEdgeNeighbors->GetId(k));
          }
        }
        if (pointCellStatus == 2)
        {
          fprintf(stdout,"  ON EDGE\n");
          cellEdgeNeighbors->Reset();
          this->SourcePd->GetCellEdgeNeighbors(closestCellId, spts[0], spts[2], cellEdgeNeighbors);
          for (int k=0; k<cellEdgeNeighbors->GetNumberOfIds(); k++)
          {
            allCapableNeighbors->InsertNextId(cellEdgeNeighbors->GetId(k));
          }
        }
        if (pointCellStatus == 4)
        {
          fprintf(stdout,"  ON EDGE\n");
          cellEdgeNeighbors->Reset();
          this->SourcePd->GetCellEdgeNeighbors(closestCellId, spts[0], spts[1], cellEdgeNeighbors);
          for (int k=0; k<cellEdgeNeighbors->GetNumberOfIds(); k++)
          {
            allCapableNeighbors->InsertNextId(cellEdgeNeighbors->GetId(k));
          }
        }

        // On verts
        if (pointCellStatus == 3)
        {
          // on vertex 2
          fprintf(stdout,"  ON VERTEX 2\n");
          this->SourcePd->GetPointCells(spts[2], allCapableNeighbors);
        }

        if (pointCellStatus == 5)
        {
          // on vertex 1
          fprintf(stdout,"  ON VERTEX 1\n");
          this->SourcePd->GetPointCells(spts[1], allCapableNeighbors);
        }

        if (pointCellStatus == 6)
        {
          fprintf(stdout,"  ON VERTEX 0\n");
          // on vertex 0
          this->SourcePd->GetPointCells(spts[0], allCapableNeighbors);
        }

        for (int k=0; k<allCapableNeighbors->GetNumberOfIds(); k++)
        {
          cellId = allCapableNeighbors->GetId(k);
          this->SourcePd->GetCellPoints(cellId, ntpts, tpts);

          this->SourceCellNormals->GetTuple(cellId, normal);
          vtkMath::Normalize(normal);
          normDot = vtkMath::Dot(pointImproveDir, normal);

          vtkMath::MultiplyScalar(normal, normDot);
          vtkMath::Subtract(pointImproveDir, normal, tangentImproveDir);
          vtkMath::Normalize(tangentImproveDir);

          fprintf(stdout,"  --------------------TESTING CELL: %d\n", cellId);
          //dirWorks = 0;
          if (pointCellStatus == 1)
          {
            for (int l=0; l<ntpts; l++)
            {
              if (tpts[l] == spts[1] || tpts[l] == spts[2])
              {
                if (tpts[(l+1)%ntpts] == spts[1] || tpts[(l+1)%ntpts] == spts[2])
                {
                  dirWorks = this->MovePointFromEdgeToEdge(closestPt, tpts[l], tpts[(l+1)%ntpts], tpts[(l+2)%ntpts], tangentImproveDir, newPt, edgeStatus);
                }
              }
            }
          }
          else if (pointCellStatus == 2)
          {
            for (int l=0; l<ntpts; l++)
            {
              if (tpts[l] == spts[0] || tpts[l] == spts[2])
              {
                if (tpts[(l+1)%ntpts] == spts[0] || tpts[(l+1)%ntpts] == spts[2])
                {
                  dirWorks = this->MovePointFromEdgeToEdge(closestPt, tpts[l], tpts[(l+1)%ntpts], tpts[(l+2)%ntpts], tangentImproveDir, newPt, edgeStatus);
                }
              }
            }
          }
          else if (pointCellStatus == 4)
          {
            for (int l=0; l<ntpts; l++)
            {
              if (tpts[l] == spts[0] || tpts[l] == spts[1])
              {
                if (tpts[(l+1)%ntpts] == spts[0] || tpts[(l+1)%ntpts] == spts[1])
                {
                  dirWorks = this->MovePointFromEdgeToEdge(closestPt, tpts[l], tpts[(l+1)%ntpts], tpts[(l+2)%ntpts], tangentImproveDir, newPt, edgeStatus);
                }
              }
            }
          }
          else if (pointCellStatus == 3)
          {
            for (int l=0; l<ntpts; l++)
            {
              if (tpts[l] != spts[2])
                continue;
              dirWorks = this->MovePointFromPointToEdge(closestPt, tpts[l], tpts[(l+1)%ntpts], tpts[(l+2)%ntpts], tangentImproveDir, newPt, edgeStatus);
            }
          }
          else if (pointCellStatus == 5)
          {
            for (int l=0; l<ntpts; l++)
            {
              if (tpts[l] != spts[1])
                continue;
              dirWorks = this->MovePointFromPointToEdge(closestPt, tpts[l], tpts[(l+1)%ntpts], tpts[(l+2)%ntpts], tangentImproveDir, newPt, edgeStatus);
            }
          }
          else if (pointCellStatus == 6)
          {
            for (int l=0; l<ntpts; l++)
            {
              if (tpts[l] != spts[0])
                continue;
              dirWorks = this->MovePointFromPointToEdge(closestPt, tpts[l], tpts[(l+1)%ntpts], tpts[(l+2)%ntpts], tangentImproveDir, newPt, edgeStatus);
            }
          }
          else
          {
            dirWorks = this->MovePointToEdge(closestPt, cellId, tangentImproveDir, newPt, edgeStatus);
          }

          if (!dirWorks)
          {
            fprintf(stdout,"  DIRECTION ON CELL %d DIDNT WORK\n", cellId);
            continue;
          }

          fprintf(stdout,"  FOUND CELL IN WHICH PROJECTED DIRECTION LIES: %d\n", cellId);
          // iterate till get to good spot
          double segmentLength = vtkSVMathUtils::Distance(closestPt, newPt);
          fprintf(stdout,  "  CLOSEST POINT: %.6f %.6f %.6f\n", closestPt[0], closestPt[1], closestPt[2]);
          fprintf(stdout,  "  NEW     POINT: %.6f %.6f %.6f\n", newPt[0], newPt[1], newPt[2]);
          double stepSize = segmentLength * 0.001;
          int numberOfSteps = (int) ceil(segmentLength/stepSize);
          stepSize = segmentLength /numberOfSteps;
          fprintf(stdout,"  STEP SIZE: %.6f\n", stepSize);
          double dirLength = 0.0;

          int m;
          double newOptDir[3];
          double funcVal = pointImprove;
          double newFuncVal;
          for (m=0; m<numberOfSteps; m++)
          {
            dirLength += stepSize;
            this->MovePointDistance(closestPt, tangentImproveDir, dirLength, newPt);

            this->WorkPd->GetPoints()->SetPoint(i, newPt);

            this->ComputeVertexCondition(i, newFuncVal, newOptDir);
            fprintf(stdout,"  INNER STEP %d OF %d; OLD: %.6f, NEW: %.6f\n", m, numberOfSteps, funcVal, newFuncVal);

            if (newFuncVal > funcVal)
            {
              this->WorkPd->GetPoints()->SetPoint(i, closestPt);
              break;
            }
            else
            {
              funcVal = newFuncVal;

              for (int n=0; n<3; n++)
              {
                closestPt[n] = newPt[n];
              }
            }
          }

          fprintf(stdout,"  NUMBER OF STEPS: %d, OUT OF %d\n", m, numberOfSteps);
          if (m >= numberOfSteps-1)
          {
            fprintf(stdout,"  MADE IT TO EDGE!\n");
          }
          if (m > 0)
          {
            this->WorkPd->GetPoints()->SetPoint(i, closestPt);
            break;
          }
        }
      }
      shapeImproveDirection->SetTuple(i, tangentImproveDir);
      shapeImproveFunction->SetTuple1(i, pointImprove);

      if (pointImprove > maxBad)
        maxBad = pointImprove;
      avgBad += pointImprove;
    }

    avgBad /= numPts;
    fprintf(stdout,"==========================ITER %d MAX CONDITION: %.6f, AVG CONDITION=======================: %.6f\n", iter, maxBad, avgBad);
    fprintf(stdout,"==========================================STEP SIZE: %.3e==================================\n", moveStep);
    //fprintf(stdout,"ITER %d MAX CONDITION: %.6f, AVG CONDITION: %.6f, STEPSIZE: %.6f\n", iter, maxBad, avgBad, moveStep);
    fprintf(stdout,"WORSE: %d, BETTER: %d\n", greater, lesser);
    if (lesser == 0)
    {
      moveStep*=0.1;
    }
    if (greater == 0)
    {
      moveStep*=10;
    }
    if (moveStep < 1.0e-6)
      allGood = 1;

    iter++;
  }

  if (!allGood)
  {
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// CheckVertexInverted
// ----------------------
int vtkSVUpdeSmoothing::CheckVertexInverted(int ptId, double compareNormal[3], double &vertexCondition, double optDirection[3])
{
  for (int j=0; j<3; j++)
  {
    optDirection[j] = 0.0;
  }

  int numpts;
  int cellId, pointIndex;
  int tmpPtIds[3];
  int ptIds[3], oppPtId;
  double f, df[3];
  double pts[3][3], oppositePt[3];
  for (int j=0; j<this->PointCells[ptId].size(); j++)
  {
    //fprintf(stdout,"START POINT: %d\n", ptId);
    cellId = this->PointCells[ptId][j];
    for (int k=0; k<this->CellPoints[cellId].size(); k++)
    {
      if (this->CellPoints[cellId][k] == ptId)
      {
        pointIndex = k;
        break;
      }
    }

    //fprintf(stdout,"CELL: %d\n", cellId);
    numpts = this->CellPoints[cellId].size();
    for (int k=0; k<this->CellPoints[cellId].size(); k++)
    {
      if (k != pointIndex)
      {
        continue;
      }
      //if (k == (pointIndex+2)%numpts)
      //{
      //  continue;
      //}

      // doing with repsect to point ptId2
      tmpPtIds[0] = this->CellPoints[cellId][(k+3)%numpts];
      tmpPtIds[1] = this->CellPoints[cellId][(k+1)%numpts];
      tmpPtIds[2] = this->CellPoints[cellId][k];
      oppPtId     = this->CellPoints[cellId][(k+2)%numpts];

      for (int l=0; l<1; l++)
      {
        ptIds[0] = tmpPtIds[l];
        ptIds[1] = tmpPtIds[(l+1)%3];
        ptIds[2] = tmpPtIds[(l+2)%3];

        //fprintf(stdout,"POINT 0: %d\n", ptIds[0]);
        //fprintf(stdout,"POINT 1: %d\n", ptIds[1]);
        //fprintf(stdout,"POINT 2: %d\n", ptIds[2]);

        this->WorkPd->GetPoint(ptIds[0], pts[0]);
        this->WorkPd->GetPoint(ptIds[1], pts[1]);
        this->WorkPd->GetPoint(ptIds[2], pts[2]);
        this->WorkPd->GetPoint(oppPtId, oppositePt);

        this->ComputeUntanglingFunction(pts[0], pts[1], pts[2], compareNormal, SV_PI/2.0, f);

        vertexCondition += f;

        if (f != 0.0)
          fprintf(stdout,"CELL %d IS FLIPPED\n", cellId);
        this->ComputeUntanglingDerivatives(pts[0], pts[1], pts[2], compareNormal, SV_PI/2.0, df);

        vertexCondition += f;
        for (int m=0; m<3; m++)
        {
          optDirection[m] += df[m];
        }
      }
    }
  }

  return SV_OK;
}

// ----------------------
// ComputeVertexCondition
// ----------------------
int vtkSVUpdeSmoothing::ComputeVertexCondition(int ptId, double &vertexCondition, double optDirection[3])
{
  vertexCondition = 0;
  for (int j=0; j<3; j++)
  {
    optDirection[j] = 0.0;
  }

  int numpts;
  int cellId, pointIndex;
  int tmpPtIds[3];
  int ptIds[3], oppPtId;
  double f, df[3], untangleF;
  double pts[3][3], oppositePt[3], untangle_dF[3];
  for (int j=0; j<this->PointCells[ptId].size(); j++)
  {
    //fprintf(stdout,"START POINT: %d\n", ptId);
    cellId = this->PointCells[ptId][j];
    for (int k=0; k<this->CellPoints[cellId].size(); k++)
    {
      if (this->CellPoints[cellId][k] == ptId)
      {
        pointIndex = k;
        break;
      }
    }

    //fprintf(stdout,"CELL: %d\n", cellId);
    numpts = this->CellPoints[cellId].size();
    for (int k=0; k<this->CellPoints[cellId].size(); k++)
    {
      if (k != pointIndex)
      {
        continue;
      }
      //if (k == (pointIndex+2)%numpts)
      //{
      //  continue;
      //}

      // doing with repsect to point ptId2
      tmpPtIds[0] = this->CellPoints[cellId][(k+3)%numpts];
      tmpPtIds[1] = this->CellPoints[cellId][(k+1)%numpts];
      tmpPtIds[2] = this->CellPoints[cellId][k];
      oppPtId     = this->CellPoints[cellId][(k+2)%numpts];

      untangleF = 0.0;
      for (int l=0; l<1; l++)
      {
        ptIds[0] = tmpPtIds[l];
        ptIds[1] = tmpPtIds[(l+1)%3];
        ptIds[2] = tmpPtIds[(l+2)%3];

        //fprintf(stdout,"POINT 0: %d\n", ptIds[0]);
        //fprintf(stdout,"POINT 1: %d\n", ptIds[1]);
        //fprintf(stdout,"POINT 2: %d\n", ptIds[2]);

        this->WorkPd->GetPoint(ptIds[0], pts[0]);
        this->WorkPd->GetPoint(ptIds[1], pts[1]);
        this->WorkPd->GetPoint(ptIds[2], pts[2]);
        this->WorkPd->GetPoint(oppPtId, oppositePt);

        // Compute function
        this->ComputeShapeImprovementFunction(pts[0], pts[1], pts[2], oppositePt, f);
        //fprintf(stdout,"This f: %.6f\n", f);

        // Compute direction
        this->ComputeShapeImprovementDerivatives(pts[0], pts[1], pts[2], oppositePt, df);

        //fprintf(stdout,"  NUTS PT %d: %.6f, %.6f %.6f %.6f\n", ptIds[2], f, df[0], df[1], df[2]);
        vertexCondition += f;
        for (int m=0; m<3; m++)
        {
          optDirection[m] += df[m];
        }
      }
    }

  }

  return SV_OK;
}


// ----------------------
// RunFilter
// ----------------------
int vtkSVUpdeSmoothing::RunFilter(vtkPolyData *original, vtkPolyData *output)
{
  // Compute metrics that we can per point first rather than per cell

  int numCells = this->WorkPd->GetNumberOfCells();
  int numPts = this->WorkPd->GetNumberOfPoints();

  vtkNew(vtkPolyData, tmp);
  tmp->DeepCopy(this->WorkPd);

  int iter = 0;
  int minCell;
  vtkIdType npts, *pts;
  double newPt[3];
  double minFunc, funcVal, moveDist;
  double pt0[3], pt1[3], pt2[3], oppositePt[3];
  double moveThresh = 0.1;
  double maxMove = 1.0;
  while(maxMove > moveThresh)
  {
    maxMove = 0.0;

    for (int i=0; i<numCells; i++)
    {
      tmp->GetCellPoints(i, npts, pts);

      for (int j=0; j<npts; j++)
      {
        tmp->GetPoint(pts[j], pt0);
        tmp->GetPoint(pts[(j+1)%npts], oppositePt);
        tmp->GetPoint(pts[(j+2)%npts], pt1);
        tmp->GetPoint(pts[(j+3)%npts], pt2);

        minFunc = VTK_SV_LARGE_DOUBLE;
        minCell = -1;
        for (int j=0; j<this->CellsOnSource[i].size(); j++)
        {
          //this->ComputeObjectiveFunction(pt0, pt1, pt2, oppositePt, this->CellsOnSource[i][j], funcVal);

          if (funcVal < minFunc)
          {
            minFunc = funcVal;
            minCell = this->CellsOnSource[i][j];
          }

        }

        this->ComputeOptimizationPoint(i, pt0, pt1, pt2, oppositePt, minCell, newPt);

        moveDist = vtkSVMathUtils::Distance(pt2, newPt);
        if (moveDist > maxMove)
        {
          maxMove = moveDist;
        }

        tmp->GetPoints()->SetPoint(i, newPt);
      }

      iter++;
    }

    vtkDebugMacro("ITERATION " << iter << " MAX MOVE DIST: " << maxMove);
  }

  return SV_OK;
}

// ----------------------
// ComputeOptimizationPoint
// ----------------------
int vtkSVUpdeSmoothing::ComputeOptimizationPoint(int pointId, double pt0[3], double pt1[3], double pt2[3],
                                                 double oppositePt[3], int sourceCell, double newPt[3])
{
  int minCell;
  int done, iter;
  int numberOfSteps;
  int edgeStatus = 0;
  int atOptimumLocation = 0;
  vtkIdType npts, *pts;
  double funcVal, newFuncVal;
  double minFunc;
  double newDir[3];
  double currentPt[3];
  double pointOnEdge[3];
  double segmentLength, stepSize, dirLength;
  vtkNew(vtkIdList, cellEdgeNeighbors);
  vtkNew(vtkIdList, allCapableNeighbors);
  for (int i=0; i<3; i++)
  {
    pt2[i] = currentPt[i];
  }

  while (!atOptimumLocation)
  {
    this->SourcePd->GetCellPoints(sourceCell, npts, pts);

    this->ComputeOptimizationDirection(pt0, pt1, currentPt, oppositePt, sourceCell, newDir);

    //this->ComputeObjectiveFunction(pt0, pt1, currentPt, oppositePt, sourceCell, funcVal);

    edgeStatus = 0;
    this->MovePointToEdge(currentPt, sourceCell, newDir, pointOnEdge, edgeStatus);

    //this->ComputeObjectiveFunction(pt0, pt1, pointOnEdge, oppositePt, sourceCell, newFuncVal);

    if (newFuncVal > funcVal)
    {
      atOptimumLocation = 1;
      // iterate till get to optimal point

      done = 0;
      iter = 1;

      segmentLength = vtkSVMathUtils::Distance(currentPt, pointOnEdge);
      stepSize = segmentLength * 0.01;
      numberOfSteps = (int) ceil(segmentLength/stepSize);
      stepSize = segmentLength /numberOfSteps;
      dirLength = 0.0;

      for (int j=0; j<numberOfSteps; j++)
      {
        dirLength += stepSize;
        //this->MovePointDistance(currentPt, newDir, dirLength, newPt);

        //this->ComputeObjectiveFunction(pt0, pt1, currentPt, oppositePt, sourceCell, newFuncVal);

        if (newFuncVal > funcVal)
        {
          break;
        }
        else
        {
          funcVal = newFuncVal;

          for (int k=0; k<3; k++)
          {
            currentPt[j] = newPt[j];
          }
        }
      }

      for (int i=0; i<3; i++)
      {
        newPt[i] = currentPt[i];
      }
    }
    else
    {
      cellEdgeNeighbors->Reset();
      allCapableNeighbors->Reset();

      if (edgeStatus & 1)
      {
        this->SourcePd->GetCellEdgeNeighbors(sourceCell, pts[0], pts[1], cellEdgeNeighbors);
        for (int i=0; i<cellEdgeNeighbors->GetNumberOfIds(); i++)
        {
          allCapableNeighbors->InsertNextId(cellEdgeNeighbors->GetId(i));
        }
      }
      if (edgeStatus & 2)
      {
        this->SourcePd->GetCellEdgeNeighbors(sourceCell, pts[1], pts[2], cellEdgeNeighbors);
        for (int i=0; i<cellEdgeNeighbors->GetNumberOfIds(); i++)
        {
          allCapableNeighbors->InsertNextId(cellEdgeNeighbors->GetId(i));
        }
      }
      if (edgeStatus & 4)
      {
        this->SourcePd->GetCellEdgeNeighbors(sourceCell, pts[0], pts[2], cellEdgeNeighbors);
        for (int i=0; i<cellEdgeNeighbors->GetNumberOfIds(); i++)
        {
          allCapableNeighbors->InsertNextId(cellEdgeNeighbors->GetId(i));
        }
      }

      this->CellsOnSource[pointId].clear();
      for (int i=0; i<allCapableNeighbors->GetNumberOfIds(); i++)
      {
        this->CellsOnSource[pointId].push_back(allCapableNeighbors->GetId(i));
      }

      minFunc = VTK_SV_LARGE_DOUBLE;
      for (int i=0; i<this->CellsOnSource[pointId].size(); i++)
      {
        //this->ComputeObjectiveFunction(pt0, pt1, pointOnEdge, oppositePt, this->CellsOnSource[i][j], funcVal);

        if (funcVal < minFunc)
        {
          minFunc = funcVal;
          minCell = this->CellsOnSource[pointId][i];
        }
      }

      sourceCell = minCell;
      for (int j=0; j<3; j++)
      {
        currentPt[j] = pointOnEdge[j];
      }
    }
  }

  return SV_OK;

}

// ----------------------
// ComputeOptimizationDirection
// ----------------------
int vtkSVUpdeSmoothing::ComputeOptimizationDirection(double pt0[3], double pt1[3], double currentPt[3], double oppositePt[3], int sourceCell, double newDir[3])
{

  double angle = 90.0;
  this->ComputeUntanglingDerivatives(pt0, pt1, currentPt, oppositePt, angle, newDir);

  return SV_OK;
}

// ----------------------
// PointCellStatus
// ----------------------
int vtkSVUpdeSmoothing::PointCellStatus(double currentPt[3], int sourceCell, int &pointCellStatus)
{
  vtkIdType npts, *pts;
  double sourcePts[3][3];
  this->SourcePd->GetCellPoints(sourceCell, npts, pts);

  for (int i=0; i<npts; i++)
  {
    this->SourcePd->GetPoint(pts[i], sourcePts[i]);
  }

  double a0, a1, a2;
  vtkSVGeneralUtils::GetBarycentricCoordinates(currentPt, sourcePts[0],
                                               sourcePts[1], sourcePts[2],
                                               a0, a1, a2);

  if (a0 <= -1.0e-6 || a1 <= -1.0e-6 || a2 < -1.0e-6)
  {
    return SV_ERROR;
  }

  pointCellStatus = 0;
  if (a0 < 1.0e-6 && a0 > -1.0e-6)
  {
    pointCellStatus += 1;
  }
  if (a1 < 1.0e-6 && a1 > -1.0e-6)
  {
    pointCellStatus += 2;
  }
  if (a2 < 1.0e-6 && a2 > -1.0e-6)
  {
    pointCellStatus += 4;
  }

  return SV_OK;
}

// ----------------------
// EdgeStatusWithDir
// ----------------------
int vtkSVUpdeSmoothing::EdgeStatusWithDir(double currentPt[3], int sourceCell, double moveDir[3], int &edgeStatus)
{
  vtkIdType npts, *pts;
  double sourcePts[3][3];
  this->SourcePd->GetCellPoints(sourceCell, npts, pts);

  for (int i=0; i<npts; i++)
  {
    this->SourcePd->GetPoint(pts[i], sourcePts[i]);
  }

  double l0 = vtkSVMathUtils::Distance(sourcePts[0], sourcePts[1]);
  double l1 = vtkSVMathUtils::Distance(sourcePts[1], sourcePts[2]);
  double l2 = vtkSVMathUtils::Distance(sourcePts[2], sourcePts[0]);

  double lineLength = l0 + l1 + l2;
  vtkMath::Normalize(moveDir);
  vtkMath::MultiplyScalar(moveDir, lineLength);

  double outPt[3];
  vtkMath::Add(currentPt, moveDir, outPt);

  // TODO: NEEDS TO BE ADDED FOR OLDER VTK TYPES

  double u0, v0;
  //int onEdge0 = vtkLine::Intersection3D(sourcePts[0], sourcePts[1], currentPt, outPt, u0, v0);

  double u1, v1;
  //int onEdge1 = vtkLine::Intersection3D(sourcePts[1], sourcePts[2], currentPt, outPt, u1, v1);

  double u2, v2;
  //int onEdge2 = vtkLine::Intersection3D(sourcePts[2], sourcePts[0], currentPt, outPt, u2, v2);

  //edgeStatus = 0;
  //if (onEdge0 == 2)
  //{
  //  edgeStatus += 1;
  //}

  //if (onEdge1 == 2)
  //{
  //  edgeStatus += 2;
  //}

  //if (onEdge2 == 2)
  //{
  //  edgeStatus += 4;
  //}

  //if (onEdge0 != 2 && onEdge1 != 2 && onEdge2 != 2)
  //{
  //  //vtkErrorMacro("Point and direction do not intersect with triangle");
  //  return SV_ERROR;
  //}

  return SV_OK;
}

// ----------------------
// MovePointDistance
// ----------------------
int vtkSVUpdeSmoothing::MovePointDistance(double currentPt[3], double moveDir[3], double length, double newPt[3])
{
  double copyDir[3];
  for (int i=0; i<3; i++)
  {
   copyDir[i] = moveDir[i];
  }

  vtkMath::Normalize(copyDir);
  vtkMath::MultiplyScalar(copyDir, length);

  vtkMath::Add(currentPt, copyDir, newPt);

  return SV_OK;
}

// ----------------------
// MovePointToEdge
// ----------------------
int vtkSVUpdeSmoothing::MovePointToEdge(double currentPt[3], int sourceCell, double moveDir[3], double newPt[3], int &edgeStatus)
{

  vtkIdType npts, *pts;
  double sourcePts[3][3];
  this->SourcePd->GetCellPoints(sourceCell, npts, pts);

  for (int i=0; i<npts; i++)
  {
    this->SourcePd->GetPoint(pts[i], sourcePts[i]);
  }

  double l0 = vtkSVMathUtils::Distance(sourcePts[0], sourcePts[1]);
  double l1 = vtkSVMathUtils::Distance(sourcePts[1], sourcePts[2]);
  double l2 = vtkSVMathUtils::Distance(sourcePts[2], sourcePts[0]);

  double lineLength = l0 + l1 + l2;
  vtkMath::Normalize(moveDir);
  vtkMath::MultiplyScalar(moveDir, lineLength);

  double outPt[3];
  vtkMath::Add(currentPt, moveDir, outPt);

  double u0, v0;
  //int onEdge0 = vtkLine::Intersection3D(sourcePts[0], sourcePts[1], currentPt, outPt, u0, v0);

  double u1, v1;
  //int onEdge1 = vtkLine::Intersection3D(sourcePts[1], sourcePts[2], currentPt, outPt, u1, v1);

  double u2, v2;
  //int onEdge2 = vtkLine::Intersection3D(sourcePts[2], sourcePts[0], currentPt, outPt, u2, v2);

  //fprintf(stdout,"  U0: %.6f V0: %.6f, U1: %.6f V1: %.6f, U2: %.6f V2: %.6f\n", u0, v0, u1, v1, u2, v2);
  edgeStatus = 0;
  //if (u0 >= -1.0e-6 && u0 <= 1.0+1.0e-6 && v0 >= -1.0e-6 && v0 <= 1.0+1.0e-6)
  //{
  //if (onEdge0 == 2)
  //{
  //  for (int i=0; i<3; i++)
  //  {
  //    newPt[i] = sourcePts[0][i] + u0 * l0 * (sourcePts[1][i] - sourcePts[0][i]);
  //  }
  //  edgeStatus += 1;
  //}
  ////fprintf(stdout,"  ON EDGE 0: %d\n", onEdge0);

  ////if (u1 >= -1.0e-6 && u1 <= 1.0+1.0e-6 && v1 >= -1.0e-6 && v1 <= 1.0+1.0e-6)
  ////{
  //if (onEdge1 == 2)
  //{
  //  for (int i=0; i<3; i++)
  //  {
  //    newPt[i] = sourcePts[1][i] + u1 * l1 * (sourcePts[2][i] - sourcePts[1][i]);
  //  }
  //  edgeStatus += 2;
  //}
  ////fprintf(stdout,"  ON EDGE 1: %d\n", onEdge1);

  ////if (u2 >= -1.0e-6 && u2 <= 1.0+1.0e-6 && v2 >= -1.0e-6 && v2 <= 1.0+1.0e-6)
  ////{
  //if (onEdge2 == 2)
  //{
  //  for (int i=0; i<3; i++)
  //  {
  //    newPt[i] = sourcePts[2][i] + u2 * l2 * (sourcePts[0][i] - sourcePts[2][i]);
  //  }
  //  edgeStatus += 4;
  //}
  ////fprintf(stdout,"  ON EDGE 2: %d\n", onEdge2);

  //if (onEdge0 != 2 && onEdge1 != 2 && onEdge2 != 2)
  //{
  //  //vtkErrorMacro("Point and direction do not intersect with triangle");
  //  return SV_ERROR;
  //}

  return SV_OK;
}

// ----------------------
// MovePointFromEdgeToEdge
// ----------------------
int vtkSVUpdeSmoothing::MovePointFromEdgeToEdge(double currentPt[3], int ptId0, int ptId1, int ptId2, double moveDir[3], double newPt[3], int &edgeStatus)
{
  double sourcePts[3][3];

  this->SourcePd->GetPoint(ptId0, sourcePts[0]);
  this->SourcePd->GetPoint(ptId1, sourcePts[1]);
  this->SourcePd->GetPoint(ptId2, sourcePts[2]);

  double l0 = vtkSVMathUtils::Distance(sourcePts[0], sourcePts[1]);
  double l1 = vtkSVMathUtils::Distance(sourcePts[1], sourcePts[2]);
  double l2 = vtkSVMathUtils::Distance(sourcePts[2], sourcePts[0]);

  double lineLength = l0 + l1 + l2;
  vtkMath::Normalize(moveDir);
  vtkMath::MultiplyScalar(moveDir, lineLength);

  double outPt[3];
  vtkMath::Add(currentPt, moveDir, outPt);

  double u0, v0;
  //int onEdge0 = vtkLine::Intersection3D(sourcePts[1], sourcePts[2], currentPt, outPt, u0, v0);

  double u1, v1;
  //int onEdge1 = vtkLine::Intersection3D(sourcePts[2], sourcePts[0], currentPt, outPt, u1, v1);

  //fprintf(stdout,"  U0: %.6f V0: %.6f, U1: %.6f V1: %.6f\n", u0, v0, u1, v1);
  edgeStatus = 0;
  //if (u0 >= -1.0e-6 && u0 <= 1.0+1.0e-6 && v0 >= -1.0e-6 && v0 <= 1.0+1.0e-6)
  //{
  //if (onEdge0 == 2)
  //{
  //  for (int i=0; i<3; i++)
  //  {
  //    newPt[i] = sourcePts[0][i] + u0 * l0 * (sourcePts[1][i] - sourcePts[0][i]);
  //  }
  //  edgeStatus += 1;
  //}
  ////fprintf(stdout,"  ON EDGE 0: %d\n", onEdge0);

  ////if (u1 >= -1.0e-6 && u1 <= 1.0+1.0e-6 && v1 >= -1.0e-6 && v1 <= 1.0+1.0e-6)
  ////{
  //if (onEdge1 == 2)
  //{
  //  for (int i=0; i<3; i++)
  //  {
  //    newPt[i] = sourcePts[1][i] + u1 * l1 * (sourcePts[2][i] - sourcePts[1][i]);
  //  }
  //  edgeStatus += 2;
  //}
  ////fprintf(stdout,"  ON EDGE 1: %d\n", onEdge1);

  //if (onEdge0 != 2 && onEdge1 != 2)
  //{
  //  //vtkErrorMacro("Point and direction do not intersect with triangle");
  //  return SV_ERROR;
  //}

  return SV_OK;
}

// ----------------------
// MovePointFromPointToEdge
// ----------------------
int vtkSVUpdeSmoothing::MovePointFromPointToEdge(double currentPt[3], int ptId0, int ptId1, int ptId2, double moveDir[3], double newPt[3], int &edgeStatus)
{

  double sourcePts[3][3];

  this->SourcePd->GetPoint(ptId0, sourcePts[0]);
  this->SourcePd->GetPoint(ptId1, sourcePts[1]);
  this->SourcePd->GetPoint(ptId2, sourcePts[2]);

  double l0 = vtkSVMathUtils::Distance(sourcePts[0], sourcePts[1]);
  double l1 = vtkSVMathUtils::Distance(sourcePts[1], sourcePts[2]);
  double l2 = vtkSVMathUtils::Distance(sourcePts[2], sourcePts[0]);

  double lineLength = l0 + l1 + l2;
  vtkMath::Normalize(moveDir);
  vtkMath::MultiplyScalar(moveDir, lineLength);

  double outPt[3];
  vtkMath::Add(currentPt, moveDir, outPt);

  double u0, v0;
  //int onEdge0 = vtkLine::Intersection3D(sourcePts[1], sourcePts[2], currentPt, outPt, u0, v0);

  //fprintf(stdout,"DO THEY INTERSECT: %d\n", onEdge0);
  //fprintf(stdout,"CHECKING TO SEE IF LINE FROM POINTS: %.6f %6.f %6.f -> %.6f %.6f %6.f\n", currentPt[0], currentPt[1], currentPt[2], outPt[0], outPt[1], outPt[2]);
  //fprintf(stdout,"INTERSECT WITH LINE FROM DIS POINTS: %.6f %6.f %6.f -> %.6f %.6f %6.f\n", sourcePts[1][0], sourcePts[1][1], sourcePts[1][2],sourcePts[2][0],sourcePts[2][1],sourcePts[2][2]);
  //fprintf(stdout,"  U0: %.6f V0: %.6f\n", u0, v0);
  edgeStatus = 0;
  //if (u0 >= -1.0e-6 && u0 <= 1.0+1.0e-6 && v0 >= -1.0e-6 && v0 <= 1.0+1.0e-6)
  //{
  //if (onEdge0 == 2)
  //{
  //  for (int i=0; i<3; i++)
  //  {
  //    newPt[i] = sourcePts[0][i] + u0 * l0 * (sourcePts[1][i] - sourcePts[0][i]);
  //  }
  //  edgeStatus += 1;
  //}
  //fprintf(stdout,"  ON EDGE 0: %d\n", onEdge0);

  //if (onEdge0 != 2)
  //{
  //  //vtkErrorMacro("Point and direction do not intersect with triangle");
  //  return SV_ERROR;
  //}

  return SV_OK;
}

// ----------------------
// ComputeUntanglingDerivatives
// ----------------------
int vtkSVUpdeSmoothing::ComputeUntanglingDerivatives(double pt0[3], double pt1[3], double pt2[3], double compareNormal[3], double theta, double newDir[3])

{
  // Point 2 is the focus point
  double pPt0[3], pPt1[3], pPt2[3];
  vtkSVGeneralUtils::GetParametricPoints(pt0, pt1, pt2, pPt0, pPt1, pPt2);

  double J0[4], J1[4], J2[4];
  this->GetJacobians(pPt0, pPt1, pPt2, J0, J1, J2);

  double v0[3], v1[3], v2[3], v3[3];
  for (int j=0; j<3; j++)
  {
    v0[j] = pt2[j] - pt1[j];
    v1[j] = pt0[j] - pt2[j];
  }

  double normal[3];
  vtkMath::Cross(v0, v1, normal);
  vtkMath::Normalize(normal);

  // TODO PAPER COULD BE WRONG ABOUT THESE
  double dNdX[3], dNdY[3], dNdZ[3];
  dNdX[0] = 0.0;
  dNdX[1] = pt1[2] - pt0[2];
  dNdX[2] = pt0[1] - pt1[1];

  dNdY[0] = pt0[2] - pt1[2];
  dNdY[1] = 0.0;
  dNdY[2] = pt1[0] - pt0[0];
  //dNdY[2] = pt0[1] - pt1[1];

  dNdZ[0] = pt1[1] - pt0[1];
  dNdZ[1] = pt0[0] - pt1[0];
  dNdZ[2] = 0.0;

  double detJacobian = this->Determinant(J0);
  //detJacobian = pPt1[0] * pPt2[1];

  double dJdX, dJdY, dJdZ;
  this->GetJacobianDerivatives(pt0, pt1, pt2,
                               pPt0, pPt1, pPt2,
                               dJdX, dJdY, dJdZ);

  double dAdX0 = 0.0, dAdY0 = 0.0, dAdZ0 = 0.0, normalDot = 0.0;
  for (int i=0; i<3; i++)
  {
    dAdX0 += normal[i] * dNdX[i] * detJacobian;

    dAdY0 += normal[i] * dNdY[i] * detJacobian;

    dAdZ0 += normal[i] * dNdZ[i] * detJacobian;

    normalDot += normal[i] * compareNormal[i];
  }

  // Get alphas
  //double dAdX = dAdX0 + (normalDot - std::cos(theta)) * dJdX;
  //double dAdY = dAdY0 + (normalDot - std::cos(theta)) * dJdY;
  //double dAdZ = dAdZ0 + (normalDot - std::cos(theta)) * dJdZ;
  double dAdX = dAdX0 - (normalDot - std::cos(theta)) * dJdX;
  double dAdY = dAdY0 - (normalDot - std::cos(theta)) * dJdY;
  double dAdZ = dAdZ0 - (normalDot - std::cos(theta)) * dJdZ;

  // Get new move direction
  double alpha = (normalDot - std::cos(theta)) * detJacobian;
  //newDir[0] = dAdX * (fabs(alpha)/alpha + 1.0);
  //newDir[1] = dAdY * (fabs(alpha)/alpha + 1.0);
  //newDir[2] = dAdZ * (fabs(alpha)/alpha + 1.0);
  newDir[0] = dAdX * (fabs(alpha)/alpha - 1.0);
  newDir[1] = dAdY * (fabs(alpha)/alpha - 1.0);
  newDir[2] = dAdZ * (fabs(alpha)/alpha - 1.0);

  return SV_OK;
}

// ----------------------
// ComputeJacobianDerivatives
// ----------------------
int vtkSVUpdeSmoothing::GetJacobianDerivatives(double pt0[3], double pt1[3], double pt2[3],
                                               double pPt0[3], double pPt1[3], double pPt2[3],
                                               double &dJdX, double &dJdY, double &dJdZ)
{
  double xi = pPt2[0];
  double eta = pPt2[1];

  double l0 = pPt1[0];

  //fprintf(stdout,"x0: %.6f, x1: %.6f, x2: %.6f\n", pt0[0], pt1[0], pt2[0]);
  //fprintf(stdout,"y0: %.6f, y1: %.6f, y2: %.6f\n", pt0[1], pt1[1], pt2[1]);
  //fprintf(stdout,"z0: %.6f, z1: %.6f, z2: %.6f\n", pt0[2], pt1[2], pt2[2]);
  dJdX = ((l0 * (pt2[0] - pt0[0])) - (xi * (pt1[0] - pt0[0]))) / eta;
  //fprintf(stdout,"DJdX: %.6f\n", dJdX);

  dJdY = ((l0 * (pt2[1] - pt0[1])) - (xi * (pt1[1] - pt0[1]))) / eta;
  //fprintf(stdout,"DJdY: %.6f\n", dJdY);

  dJdZ = ((l0 * (pt2[2] - pt0[2])) - (xi * (pt1[2] - pt0[2]))) / eta;
  //fprintf(stdout,"DJdZ: %.6f\n", dJdZ);


  return SV_OK;
}

// ----------------------
// ComputeShapeImprovementDerivatives
// ----------------------
int vtkSVUpdeSmoothing::ComputeShapeImprovementDerivatives(double pt0[3], double pt1[3], double pt2[3], double oppositePt[3],
                                                           double dK[3])
{
  // Point 2 is the focus point

  double pPt0[3], pPt1[3], pPt2[3];
  vtkSVGeneralUtils::GetParametricPoints(pt0, pt1, pt2, pPt0, pPt1, pPt2);

  // Jacobian
  //
  double J0[4], J1[4], J2[4];
  this->GetJacobians(pPt0, pPt1, pPt2, J0, J1, J2);

  double detJacobian = this->Determinant(J0);
  //detJacobian = pPt1[0] * pPt2[1];

  // Jacobian derivatives, only valid for point 2 in the current parametric format
  double dJdX, dJdY, dJdZ;
  this->GetJacobianDerivatives(pt0, pt1, pt2,
                               pPt0, pPt1, pPt2,
                               dJdX, dJdY, dJdZ);

  // Frobenius norm
  double l0 = vtkSVMathUtils::Distance(pPt0, pPt1);
  double l1 = vtkSVMathUtils::Distance(pPt1, pPt2);
  double l2 = vtkSVMathUtils::Distance(pPt2, pPt0);

  double F0 = pow(l0, 2.0) + pow(l2, 2.0);
  double F1 = pow(l0, 2.0) + pow(l1, 2.0);
  double F2 = pow(l1, 2.0) + pow(l2, 2.0);

  //// Frobenius norm derivatives
  //// p0
  //double dF0dX = 2 * (pt2[0] - pt0[0]);
  //double dF0dY = 2 * (pt2[1] - pt0[1]);
  //double dF0dZ = 2 * (pt2[2] - pt0[2]);

  //// p1
  //double dF1dX = 2 * (pt2[0] - pt1[0]);
  //double dF1dY = 2 * (pt2[1] - pt1[1]);
  //double dF1dZ = 2 * (pt2[2] - pt1[2]);

  // p2
  double dF2dX = 2 * (2*pt2[0] - (pt0[0] + pt1[0]));
  double dF2dY = 2 * (2*pt2[1] - (pt0[1] + pt1[1]));
  double dF2dZ = 2 * (2*pt2[2] - (pt0[2] + pt1[2]));

  //fprintf(stdout,"DET JAC: %.6f\n", detJacobian);
  //fprintf(stdout,"dF2dX: %.6f\n", dF2dX);
  //fprintf(stdout,"dF2dY: %.6f\n", dF2dY);
  //fprintf(stdout,"dF2dZ: %.6f\n", dF2dZ);
  //fprintf(stdout,"dJdX:  %.6f\n", dJdX);
  //fprintf(stdout,"dJdY:  %.6f\n", dJdY);
  //fprintf(stdout,"dJdZ:  %.6f\n", dJdZ);
  //fprintf(stdout,"F2:    %.6f\n", F2);

  //dK[0] = ((detJacobian * dF0dX) + (dJdX * F0)) / (2 * pow(detJacobian, 2.0));
  //dK[1] = ((detJacobian * dF0dY) + (dJdY * F0)) / (2 * pow(detJacobian, 2.0));
  //dK[2] = ((detJacobian * dF0dZ) + (dJdZ * F0)) / (2 * pow(detJacobian, 2.0));

  //dK[0] = ((detJacobian * dF1dX) + (dJdX * F1)) / (2 * pow(detJacobian, 2.0));
  //dK[1] = ((detJacobian * dF1dY) + (dJdY * F1)) / (2 * pow(detJacobian, 2.0));
  //dK[2] = ((detJacobian * dF1dZ) + (dJdZ * F1)) / (2 * pow(detJacobian, 2.0));

  dK[0] = ((detJacobian * dF2dX) - (dJdX * F2)) / (2 * pow(detJacobian, 2.0));
  dK[1] = ((detJacobian * dF2dY) - (dJdY * F2)) / (2 * pow(detJacobian, 2.0));
  dK[2] = ((detJacobian * dF2dZ) - (dJdZ * F2)) / (2 * pow(detJacobian, 2.0));


  return SV_OK;
}

double vtkSVUpdeSmoothing::Determinant(double mat[4])
{
  return ((mat[0] * mat[3]) - (mat[1] * mat[2]));
}

int vtkSVUpdeSmoothing::GetJacobians(double pPt0[3], double pPt1[3], double pPt2[3],
                                     double J0[4], double J1[4], double J2[4])
{
  double xi = pPt2[0];
  double eta = pPt2[1];

  double l0 = pPt1[0];

  J0[0] = l0;
  J0[1] = xi;
  J0[2] = 0.0;
  J0[3] = eta;

  J1[0] = xi - l0;
  J1[1] = -1.0 * l0;
  J1[2] = eta;
  J1[3] = 0.0;

  J2[0] = -1.0 * xi;
  J2[1] = l0 - xi;
  J2[2] = -eta;
  J2[3] = -eta;

  return SV_OK;
}

int vtkSVUpdeSmoothing::ComputeUntanglingFunction(double pt0[3], double pt1[3], double pt2[3], double compareNormal[3], double theta, double &f)
{
  double pPt0[3], pPt1[3], pPt2[3];
  vtkSVGeneralUtils::GetParametricPoints(pt0, pt1, pt2, pPt0, pPt1, pPt2);

  double J0[4], J1[4], J2[4];
  this->GetJacobians(pPt0, pPt1, pPt2, J0, J1, J2);

  double v0[3], v1[3];
  for (int j=0; j<3; j++)
  {
    v0[j] = pt2[j] - pt1[j];
    v1[j] = pt0[j] - pt2[j];
  }

  double normal[3];
  vtkMath::Cross(v0, v1, normal);
  vtkMath::Normalize(normal);

  double detJacobian = this->Determinant(J0);
  //detJacobian = pPt1[0] * pPt2[1];

  double normalDot = 0.0;
  for (int i=0; i<3; i++)
  {
    normalDot += normal[i] * compareNormal[i];
  }

  // Get alpha
  double alpha = (normalDot - std::cos(theta)) * detJacobian;

  f = fabs(alpha) - alpha;

  return SV_OK;
}

int vtkSVUpdeSmoothing::ComputeShapeImprovementFunction(double pt0[3], double pt1[3], double pt2[3], double oppositePt[3], double &f)
{
  double pPt0[3], pPt1[3], pPt2[3];
  vtkSVGeneralUtils::GetParametricPoints(pt0, pt1, pt2, pPt0, pPt1, pPt2);

  double J0[4], J1[4], J2[4];
  this->GetJacobians(pPt0, pPt1, pPt2, J0, J1, J2);

  double detJacobian = this->Determinant(J0);

  double l0 = vtkSVMathUtils::Distance(pPt0, pPt1);
  double l1 = vtkSVMathUtils::Distance(pPt1, pPt2);
  double l2 = vtkSVMathUtils::Distance(pPt2, pPt0);

  double F0 = pow(l0, 2.0) + pow(l2, 2.0);
  double F1 = pow(l0, 2.0) + pow(l1, 2.0);
  double F2 = pow(l1, 2.0) + pow(l2, 2.0);

  //f = F0/ (2 *detJacobian);
  //f = F1/ (2 *detJacobian);
  f = F2/ (2 *detJacobian);

  return SV_OK;
}
