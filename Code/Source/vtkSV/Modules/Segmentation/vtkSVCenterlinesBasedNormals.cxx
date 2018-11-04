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

#include "vtkSVCenterlinesBasedNormals.h"

#include "vtkCellData.h"
#include "vtkDoubleArray.h"
#include "vtkErrorCode.h"
#include "vtkIdList.h"
#include "vtkIntArray.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPointLocator.h"
#include "vtkSmartPointer.h"
#include "vtkTriangle.h"

#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"
#include "vtkSVMathUtils.h"
#include "vtkSVIOUtils.h"

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVCenterlinesBasedNormals);

const double vtkSVCenterlinesBasedNormals::GlobalCoords[3][3] =
  {
    {1.0, 0.0, 0.0},
    {0.0, 1.0, 0.0},
    {0.0, 0.0, 1.0},
  };

// ----------------------
// Constructor
// ----------------------
vtkSVCenterlinesBasedNormals::vtkSVCenterlinesBasedNormals()
{
  this->WorkPd =        vtkPolyData::New();
  this->CenterlinesPd = NULL;

  this->CellArrayName =        NULL;
  this->NewCellArrayName =     NULL;
  this->PointArrayName =       NULL;
  this->GroupIdsArrayName =    NULL;
  this->InternalIdsArrayName = NULL;

  this->CellArray = NULL;
  this->PointArray = NULL;
  this->NewCellArray = vtkDoubleArray::New();

  this->UsePointArray =      0;
  this->UseCellArray  =      1;
}

// ----------------------
// Destructor
// ----------------------
vtkSVCenterlinesBasedNormals::~vtkSVCenterlinesBasedNormals()
{
  if (this->WorkPd != NULL)
  {
    this->WorkPd->Delete();
    this->WorkPd = NULL;
  }

  if (this->NewCellArray != NULL)
  {
    this->NewCellArray->Delete();
    this->NewCellArray = NULL;
  }

  if (this->CellArrayName != NULL)
  {
    delete [] this->CellArrayName;
    this->CellArrayName = NULL;
  }
  if (this->NewCellArrayName != NULL)
  {
    delete [] this->NewCellArrayName;
    this->NewCellArrayName = NULL;
  }
  if (this->PointArrayName != NULL)
  {
    delete [] this->PointArrayName;
    this->PointArrayName = NULL;
  }
  if (this->GroupIdsArrayName != NULL)
  {
    delete [] this->GroupIdsArrayName;
    this->GroupIdsArrayName = NULL;
  }
  if (this->InternalIdsArrayName != NULL)
  {
    delete [] this->InternalIdsArrayName;
    this->InternalIdsArrayName = NULL;
  }
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVCenterlinesBasedNormals::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);

  if (this->CellArrayName != NULL)
    os << indent << "Cell data array name: " << this->CellArrayName << "\n";
  if (this->NewCellArrayName != NULL)
    os << indent << "Cell data array name: " << this->NewCellArrayName << "\n";
  if (this->PointArrayName != NULL)
    os << indent << "Point data array name: " << this->PointArrayName << "\n";
  if (this->GroupIdsArrayName != NULL)
    os << indent << "Point data array name: " << this->GroupIdsArrayName << "\n";
  if (this->InternalIdsArrayName != NULL)
    os << indent << "Point data array name: " << this->InternalIdsArrayName << "\n";

  os << indent << "Use cell array: " << this->UseCellArray << "\n";
  os << indent << "Use point array: " << this->UsePointArray << "\n";
}

// ----------------------
// RequestData
// ----------------------
int vtkSVCenterlinesBasedNormals::RequestData(vtkInformation *vtkNotUsed(request),
                                 vtkInformationVector **inputVector,
                                 vtkInformationVector *outputVector)
{
  // get the input and output
  vtkPolyData *input1 = vtkPolyData::GetData(inputVector[0]);
  vtkPolyData *output = vtkPolyData::GetData(outputVector);

  // Copy the input to operate on
  this->WorkPd->DeepCopy(input1);

  if (this->PrepFilter() != SV_OK)
  {
    vtkErrorMacro("Error in preprocessing the polydata\n");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }

  if (this->RunFilter() != SV_OK)
  {
    vtkErrorMacro("Error when running main operation\n");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }

  output->DeepCopy(this->WorkPd);
  return SV_OK;
}

// ----------------------
// PrepFilter
// ----------------------
int vtkSVCenterlinesBasedNormals::PrepFilter()
{
  if (this->CenterlinesPd == NULL)
  {
    vtkErrorMacro("Centerlines must be provided");
    return SV_ERROR;
  }
  if (this->UseCellArray && this->UsePointArray)
  {
    vtkErrorMacro("Can only use points or cells, not both");
    return SV_ERROR;
  }

  if (this->GroupIdsArrayName == NULL)
  {
    vtkErrorMacro("GroupIds array name must be given");
    return SV_ERROR;
  }

  if (this->InternalIdsArrayName == NULL)
  {
    vtkDebugMacro("Internal ids array name not given, setting to TmpInternalIds");
    this->InternalIdsArrayName = new char[strlen("TmpInternalIds") + 1];
    strcpy(this->InternalIdsArrayName, "TmpInternalIds");
  }

  if (this->UseCellArray)
  {
    if (this->CellArrayName == NULL)
    {
      vtkDebugMacro("Cell Array Name not given, setting to CellNormals");
      this->CellArrayName = new char[strlen("CellNormals") + 1];
      strcpy(this->CellArrayName, "CellNormals");
    }

    if (vtkSVGeneralUtils::CheckArrayExists(this->WorkPd, 1, this->CellArrayName) != SV_OK)
    {
      vtkErrorMacro("Array named "<< this->CellArrayName << "not found on polydata");
      return SV_ERROR;
    }

    this->CellArray = vtkDoubleArray::SafeDownCast(this->WorkPd->GetCellData()->GetArray(this->CellArrayName));

    if (this->NewCellArrayName == NULL)
    {
      vtkDebugMacro("New Cell Array Name not given, setting to CenterlinesCellNormals");
      this->NewCellArrayName = new char[strlen("CenterlinesCellNormals") + 1];
      strcpy(this->NewCellArrayName, "CenterlinesCellNormals");
    }

    this->NewCellArray->SetName(this->NewCellArrayName);
    this->NewCellArray->SetNumberOfComponents(this->CellArray->GetNumberOfComponents());
    this->NewCellArray->SetNumberOfTuples(this->CellArray->GetNumberOfTuples());
  }
  else if (this->UsePointArray)
  {
    if (this->PointArrayName == NULL)
    {
      vtkDebugMacro("Point Array Name not given, setting to PointNormals");
      this->PointArrayName = new char[strlen("PointNormals") + 1];
      strcpy(this->PointArrayName, "PointNormals");
    }

    if (vtkSVGeneralUtils::CheckArrayExists(this->WorkPd, 0, this->PointArrayName) != SV_OK)
    {
      vtkErrorMacro("Array named "<< this->PointArrayName << "not found on polydata");
      return SV_ERROR;
    }

    this->PointArray = vtkDoubleArray::SafeDownCast(this->WorkPd->GetPointData()->GetArray(this->PointArrayName));
  }
  else
  {
    vtkErrorMacro("Must use either point or cell data");
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// RunFilter
// ----------------------
int vtkSVCenterlinesBasedNormals::RunFilter()
{
  vtkNew(vtkPolyData, centerlinesWorkPd);
  centerlinesWorkPd->DeepCopy(this->CenterlinesPd);

  //vtkNew(vtkIntArray, lineIndicator);
  //lineIndicator->SetNumberOfTuples(centerlinesWorkPd->GetNumberOfCells());

  //int numCells =  centerlinesWorkPd->GetNumberOfCells();
  //int numPoints = centerlinesWorkPd->GetNumberOfPoints();

  //centerlinesWorkPd->BuildLinks();
  //this->WorkPd->BuildLinks();
  //for (int i=0; i<numCells; i++)
  //{
  //  vtkIdType npts, *pts;
  //  centerlinesWorkPd->GetCellPoints(i, npts, pts);

  //  vtkNew(vtkIdList, pt0Cells);
  //  vtkNew(vtkIdList, ptNCells);
  //  centerlinesWorkPd->GetPointCells(pts[0], pt0Cells);
  //  centerlinesWorkPd->GetPointCells(pts[npts-1], ptNCells);

  //  if (pt0Cells->GetNumberOfIds() == 1 && ptNCells->GetNumberOfIds() == 1)
  //  {
  //    if (numCells != 1)
  //    {
  //      vtkErrorMacro("Disconnected centerline, fix centerlines before proceeding!");
  //      return SV_ERROR;
  //    }
  //    lineIndicator->SetTuple1(i, 0);
  //  }
  //  else if (pt0Cells->GetNumberOfIds() == 1)
  //  {
  //    // Centerlines is a terminating branch
  //    lineIndicator->SetTuple1(i, 1);
  //  }
  //  else if (ptNCells->GetNumberOfIds() == 1)
  //  {
  //    // Centerlines is a terminating branch
  //    lineIndicator->SetTuple1(i, 2);
  //  }
  //  else
  //  {
  //    // Internal centerline
  //    lineIndicator->SetTuple1(i, 3);
  //  }
  //}

  //vtkNew(vtkDoubleArray, localArrayX);
  //vtkNew(vtkDoubleArray, localArrayY);
  //vtkNew(vtkDoubleArray, localArrayZ);
  //vtkNew(vtkDoubleArray, rotMatrix);
  //localArrayX->SetNumberOfComponents(3);
  //localArrayX->SetNumberOfTuples(numPoints);
  //localArrayY->SetNumberOfComponents(3);
  //localArrayY->SetNumberOfTuples(numPoints);
  //localArrayZ->SetNumberOfComponents(3);
  //localArrayZ->SetNumberOfTuples(numPoints);
  //rotMatrix->SetNumberOfComponents(9);
  //rotMatrix->SetNumberOfTuples(numPoints);
  //for (int i=0; i<3; i++)
  //{
  //  localArrayX->FillComponent(i, -1);
  //  localArrayY->FillComponent(i, -1);
  //  localArrayZ->FillComponent(i, -1);
  //}

  //vtkNew(vtkDoubleArray, startXArray);
  //startXArray->SetNumberOfComponents(3);
  //startXArray->SetNumberOfTuples(numPoints);
  //for (int i=0; i<numCells; i++)
  //{
  //  int lineType = lineIndicator->GetTuple1(i);

  //  // cell points
  //  vtkIdType npts, *pts;
  //  centerlinesWorkPd->GetCellPoints(i, npts, pts);

  //  double startX[3];
  //  if (lineType == 0)
  //  {
  //    startX[0] = 1.0; startX[1] = 0.0; startX[2] = 0.0;
  //  }
  //  else
  //  {
  //    if (lineType == 1)
  //      this->FlipLinePoints(centerlinesWorkPd, i);

  //    // point cells
  //    vtkNew(vtkIdList, pointCells);
  //    centerlinesWorkPd->GetPointCells(pts[0], pointCells);

  //    if (pointCells->GetNumberOfIds() < 3)
  //    {
  //      vtkErrorMacro("Incorrect valence of branching centerline point");
  //      return SV_ERROR;
  //    }
  //    else
  //    {

  //      double verts[3][3];
  //      for (int j=0; j<3; j++)
  //      {
  //        vtkIdType ncpts, *cpts;
  //        centerlinesWorkPd->GetCellPoints(pointCells->GetId(j), ncpts, cpts);

  //        for (int k=0; k<ncpts; k++)
  //        {
  //          if (cpts[k] != pts[0])
  //          {
  //            centerlinesWorkPd->GetPoint(cpts[k], verts[j]);
  //          }
  //        }
  //      }
  //      // Compute the start vector
  //      this->ComputeStartVector(verts, startX);
  //    }

  //  }

  //  startXArray->SetTuple(i, startX);
  //}

  //for (int i=0; i<numCells; i++)
  //{
  //  int lineType = lineIndicator->GetTuple1(i);

  //  // cell points
  //  vtkIdType npts, *pts;
  //  centerlinesWorkPd->GetCellPoints(i, npts, pts);

  //  // start vector
  //  double startX[3];
  //  startXArray->GetTuple(i, startX);

  //  double localZ[3], localX[3], localY[3];
  //  double pt0[3], pt1[3];
  //  for (int j=0; j<npts-1; j++)
  //  {
  //    centerlinesWorkPd->GetPoint(pts[j], pt0);
  //    centerlinesWorkPd->GetPoint(pts[j+1], pt1);
  //    vtkMath::Subtract(pt1, pt0, localZ);
  //    vtkMath::Normalize(localZ);
  //    if (j==0)
  //    {
  //      this->ComputeLocalCoordinateSystem(localZ, startX, localX, localY);

  //      localArrayX->SetTuple(pts[j], localX);
  //      localArrayY->SetTuple(pts[j], localY);
  //      localArrayZ->SetTuple(pts[j], localZ);
  //      localArrayX->SetTuple(pts[j+1], localX);
  //      localArrayY->SetTuple(pts[j+1], localY);
  //      localArrayZ->SetTuple(pts[j+1], localZ);
  //    }
  //    else if (j > (npts-1)/3. && lineType == 3)
  //    {
  //      vtkNew(vtkIdList, pointCells);
  //      centerlinesWorkPd->GetPointCells(pts[npts-1], pointCells);
  //      int cellId;
  //      for (int k=0; k<pointCells->GetNumberOfIds(); k++)
  //      {
  //        cellId = pointCells->GetId(k);
  //        if (cellId != i)
  //          break;
  //      }
  //      // start
  //      localArrayX->GetTuple(pts[j], startX);

  //      // end
  //      double endX[3];
  //      startXArray->GetTuple(cellId, endX);

  //      // See if it aligns
  //      double checkDir = vtkMath::Dot(startX, endX);
  //      if (checkDir < 0)
  //        vtkMath::MultiplyScalar(endX, -1);

  //      // difference
  //      double diff[3];
  //      vtkMath::Subtract(endX, startX, diff);
  //      vtkMath::Normalize(diff);

  //      // Get portion left
  //      double newX[3];
  //      fprintf(stdout,"J: %d NPTS: %d\n", j, npts-1);
  //      vtkMath::MultiplyScalar(diff, (j-((npts-1)/3.))/(2.*(npts-1)/3.));
  //      fprintf(stdout,"WHAT: %.5f\n", (j-((npts-1)/3.))/(2.*(npts-1)/3.));
  //      vtkMath::Add(startX, diff, newX);
  //      vtkMath::Normalize(newX);

  //      // Do the dirt
  //      fprintf(stdout,"Branch %d using start of %d\n", i, cellId);
  //      fprintf(stdout,"Start X: %.6f %.6f %.6f\n", startX[0], startX[1], startX[2]);
  //      fprintf(stdout,"End X:   %.6f %.6f %.6f\n", endX[0], endX[1], endX[2]);
  //      fprintf(stdout,"New X:   %.6f %.6f %.6f\n", newX[0], newX[1], newX[2]);
  //      this->ComputeLocalCoordinateSystem(localZ, newX, localX, localY);

  //      localArrayX->SetTuple(pts[j+1], localX);
  //      localArrayY->SetTuple(pts[j+1], localY);
  //      localArrayZ->SetTuple(pts[j+1], localZ);
  //    }
  //    else
  //    {
  //      localArrayX->GetTuple(pts[j], startX);
  //      this->ComputeLocalCoordinateSystem(localZ, startX, localX, localY);

  //      localArrayX->SetTuple(pts[j+1], localX);
  //      localArrayY->SetTuple(pts[j+1], localY);
  //      localArrayZ->SetTuple(pts[j+1], localZ);
  //    }

  //    double locMat[9];
  //    this->ComputeRotationMatrix(localX, localY, localZ, locMat);
  //    if (j==0)
  //      rotMatrix->SetTuple(pts[j], locMat);
  //    rotMatrix->SetTuple(pts[j+1], locMat);
  //  }
  //}

  //int numPolyCells = this->WorkPd->GetNumberOfCells();

  ////-----------------------------------------------------------------------V
  //localArrayX->SetName("LocalX");
  //centerlinesWorkPd->GetPointData()->AddArray(localArrayX);
  //localArrayY->SetName("LocalY");
  //centerlinesWorkPd->GetPointData()->AddArray(localArrayY);
  //localArrayZ->SetName("LocalZ");
  //centerlinesWorkPd->GetPointData()->AddArray(localArrayZ);
  //std::string filename = "/Users/adamupdegrove/Desktop/tmp/MyTEST.vtp";
  //vtkSVIOUtils::WriteVTPFile(filename, centerlinesWorkPd);
  ////-----------------------------------------------------------------------V

  vtkDoubleArray *localArrayX = vtkDoubleArray::SafeDownCast(centerlinesWorkPd->GetPointData()->GetArray("LocalX"));
  vtkDoubleArray *localArrayY = vtkDoubleArray::SafeDownCast(centerlinesWorkPd->GetPointData()->GetArray("LocalY"));
  vtkDoubleArray *localArrayZ = vtkDoubleArray::SafeDownCast(centerlinesWorkPd->GetPointData()->GetArray("LocalZ"));
  vtkDoubleArray *rotMatrix = vtkDoubleArray::SafeDownCast(centerlinesWorkPd->GetPointData()->GetArray("RotationMatrix"));

  vtkNew(vtkIdList, centerlineGroupIds);
  for (int i=0; i<this->WorkPd->GetCellData()->GetArray(this->GroupIdsArrayName)->GetNumberOfTuples(); i++)
  {
    centerlineGroupIds->InsertUniqueId(static_cast<vtkIdType>(vtkMath::Round(this->WorkPd->GetCellData()->GetArray(this->GroupIdsArrayName)->GetComponent(i,0))));
  }
  int numGroups = centerlineGroupIds->GetNumberOfIds();

  vtkNew(vtkPolyData, tmpPd);
  vtkSVGeneralUtils::GiveIds(centerlinesWorkPd, this->InternalIdsArrayName, tmpPd);

  vtkNew(vtkPolyData, polyTmpPd);
  vtkSVGeneralUtils::GiveIds(this->WorkPd, this->InternalIdsArrayName, polyTmpPd);
  for (int i=0; i<numGroups; i++)
  {
    int groupId = centerlineGroupIds->GetId(i);

    vtkNew(vtkPolyData, centerlineBranchPd);
    vtkSVGeneralUtils::ThresholdPd(tmpPd, groupId, groupId, 1, this->GroupIdsArrayName, centerlineBranchPd);


    vtkNew(vtkPolyData, branchPd);
    vtkSVGeneralUtils::ThresholdPd(polyTmpPd, groupId, groupId, 1, this->GroupIdsArrayName, branchPd);
    branchPd->BuildLinks();

    vtkNew(vtkPointLocator, locator);
    locator->SetDataSet(centerlineBranchPd);
    locator->BuildLocator();
    for (int j=0; j<branchPd->GetNumberOfCells(); j++)
    {
      // Get cell point coords
      double pts[3][3];
      vtkIdType npts, *ptids;
      branchPd->GetCellPoints(j, npts, ptids);
      for (int k=0; k<npts; k++)
        branchPd->GetPoint(ptids[k], pts[k]);

      // Get center
      double center[3];
      vtkTriangle::TriangleCenter(pts[0], pts[1], pts[2], center);
      int closestPt = locator->FindClosestPoint(center);

      int realId = centerlineBranchPd->GetPointData()->GetArray(this->InternalIdsArrayName)->GetTuple1(closestPt);

      double rotVal[9];
      rotMatrix->GetTuple(realId, rotVal);

      double currNormal[3];
      int realCellId = branchPd->GetCellData()->GetArray(this->InternalIdsArrayName)->GetTuple1(j);
      this->CellArray->GetTuple(realCellId, currNormal);

      double newNormal[3];
      for (int k=0; k<3; k++)
      {
        newNormal[k] = rotVal[k*3]*currNormal[0] +
                       rotVal[(k*3)+1]*currNormal[1] +
                       rotVal[(k*3)+2]*currNormal[2];
      }
      this->NewCellArray->SetTuple(realCellId, newNormal);
    }
  }

  this->WorkPd->GetCellData()->AddArray(this->NewCellArray);

  return SV_OK;
}

// ----------------------
// ComputeStartVector
// ----------------------
int vtkSVCenterlinesBasedNormals::ComputeStartVector(const double verts[3][3],
                                                     double startVec[3])
{
  double vec0[3], vec1[3];
  vtkMath::Subtract(verts[1], verts[0], vec0);
  vtkMath::Subtract(verts[2], verts[0], vec1);

  vtkMath::Normalize(vec0);
  vtkMath::Normalize(vec1);

  vtkMath::Cross(vec0, vec1, startVec);
  vtkMath::Normalize(startVec);

  return SV_OK;
}

// ----------------------
// ComputeLocalCoordinateSystem
// ----------------------
int vtkSVCenterlinesBasedNormals::ComputeLocalCoordinateSystem(const double vz[3],
                                                               const double vstart[3],
                                                               double vx[3],
                                                               double vy[3])
{
	double tempArray[3];
  double tempLength = vtkMath::Dot(vstart, vz);
	for (int i = 0; i < 3; i++)
		tempArray[i] = tempLength * vz[i];

  vtkMath::Subtract(vstart, tempArray, vx);
  vtkMath::Normalize(vx);

  vtkMath::Cross(vz, vx, vy);
  vtkMath::Normalize(vy);

  return SV_OK;
}

// ----------------------
// ComputeRotationMatrix
// ----------------------
int vtkSVCenterlinesBasedNormals::ComputeRotationMatrix(const double vx[3],
                                                        const double vy[3],
                                                        const double vz[3],
                                                        double rotMatrix[9])
{
  rotMatrix[0] = vx[0]*this->GlobalCoords[0][0] +
                 vx[1]*this->GlobalCoords[0][1] +
                 vx[2]*this->GlobalCoords[0][2];
  rotMatrix[1] = vx[0]*this->GlobalCoords[1][0] +
                 vx[1]*this->GlobalCoords[1][1] +
                 vx[2]*this->GlobalCoords[1][2];
  rotMatrix[2] = vx[0]*this->GlobalCoords[2][0] +
                 vx[1]*this->GlobalCoords[2][1] +
                 vx[2]*this->GlobalCoords[2][2];

  rotMatrix[3] = vy[0]*this->GlobalCoords[0][0] +
                 vy[1]*this->GlobalCoords[0][1] +
                 vy[2]*this->GlobalCoords[0][2];
  rotMatrix[4] = vy[0]*this->GlobalCoords[1][0] +
                 vy[1]*this->GlobalCoords[1][1] +
                 vy[2]*this->GlobalCoords[1][2];
  rotMatrix[5] = vy[0]*this->GlobalCoords[2][0] +
                 vy[1]*this->GlobalCoords[2][1] +
                 vy[2]*this->GlobalCoords[2][2];

  rotMatrix[6] = vz[0]*this->GlobalCoords[0][0] +
                 vz[1]*this->GlobalCoords[0][1] +
                 vz[2]*this->GlobalCoords[0][2];
  rotMatrix[7] = vz[0]*this->GlobalCoords[1][0] +
                 vz[1]*this->GlobalCoords[1][1] +
                 vz[2]*this->GlobalCoords[1][2];
  rotMatrix[8] = vz[0]*this->GlobalCoords[2][0] +
                 vz[1]*this->GlobalCoords[2][1] +
                 vz[2]*this->GlobalCoords[2][2];

  return SV_OK;
}

// ----------------------
// FlipLinePoints
// ----------------------
int vtkSVCenterlinesBasedNormals::FlipLinePoints(vtkPolyData *pd, const int cellId)
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

  return SV_OK;
}
