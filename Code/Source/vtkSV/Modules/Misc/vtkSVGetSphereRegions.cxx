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

#include "vtkSVGetSphereRegions.h"

#include "vtkCellArray.h"
#include "vtkCellData.h"
#include "vtkCellDataToPointData.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkDoubleArray.h"
#include "vtkEdgeTable.h"
#include "vtkErrorCode.h"
#include "vtkFloatArray.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkIntArray.h"
#include "vtkMath.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPolyData.h"
#include "vtkPolygon.h"
#include "vtkSmartPointer.h"
#include "vtkStreamingDemandDrivenPipeline.h"
#include "vtkThreshold.h"
#include "vtkUnstructuredGrid.h"

#include "vtkSVGlobals.h"
#include "vtkSVGeneralUtils.h"

#include <iostream>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVGetSphereRegions);

// ----------------------
// Constructor
// ----------------------
vtkSVGetSphereRegions::vtkSVGetSphereRegions()
{
    this->CellArrayName    = NULL;
    this->PointArrayName   = NULL;
    this->OutCellArrayName = NULL;

    this->SphereRadius = 0.0;
}

// ----------------------
// Destructor
// ----------------------
vtkSVGetSphereRegions::~vtkSVGetSphereRegions()
{
  if (this->CellArrayName != NULL)
  {
    delete [] this->CellArrayName;
    this->CellArrayName = NULL;
  }
  if (this->PointArray != NULL)
  {
    delete [] this->PointArrayName;
    this->PointArrayName = NULL;
  }
  if (this->OutCellArrayName != NULL)
  {
    delete [] this->OutCellArrayName;
    this->OutCellArrayName = NULL;
  }
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVGetSphereRegions::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);

  os << indent << "Sphere Radius: " << this->SphereRadius << "\n";
  if (this->CellArrayName != NULL)
    os << indent << "Cell array name: " << this->CellArrayName << "\n";
  if (this->PointArrayName != NULL)
    os << indent << "Point array name: " << this->PointArrayName << "\n";
  if (this->OutCellArrayName != NULL)
    os << indent << "Out cell array name: " << this->OutCellArrayName << "\n";
}

// ----------------------
// RequestData
// ----------------------
int vtkSVGetSphereRegions::RequestData(vtkInformation *vtkNotUsed(request),
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
      this->SetErrorCode(vtkErrorCode::UserError + 1);
      return SV_ERROR;
    }

    if (this->PointArrayName == NULL)
    {
      std::cout<<"No PointArrayName given." << endl;
      this->SetErrorCode(vtkErrorCode::UserError + 1);
      return SV_ERROR;
    }
    if (this->CellArrayName == NULL)
    {
      std::cout<<"No CellArrayName given." << endl;
      this->SetErrorCode(vtkErrorCode::UserError + 1);
      return SV_ERROR;
    }
    if (this->GetArrays(input,0) != 1)
    {
      std::cout<<"No Point Array Named "<<this->PointArrayName<<" on surface"<<endl;
      this->SetErrorCode(vtkErrorCode::UserError + 1);
      return SV_ERROR;
    }
    if (this->GetArrays(input,1) != 1)
    {
      std::cout<<"No Cell Array Named "<<this->CellArrayName<<" on surface"<<endl;
      this->SetErrorCode(vtkErrorCode::UserError + 1);
      return SV_ERROR;
    }
    if (this->OutCellArrayName == 0)
    {
      std::cout<<"Need array name for output cell data"<<endl;
      this->SetErrorCode(vtkErrorCode::UserError + 1);
      return SV_ERROR;
    }

    input->BuildLinks();
    vtkNew(vtkPolyData, linepd);
    int numloops = 0;
    this->GetClosedEdgeLoops(input,linepd,&numloops);
    this->SetSphereRegions(input,linepd,numloops);

    output->DeepCopy(input);
    return SV_OK;
}

// ----------------------
// GetArrays
// ----------------------
int vtkSVGetSphereRegions::GetArrays(vtkPolyData *object,int type)
{
  vtkIdType i;
  int numArrays;

  // Set array name
  std::string arrayName;
  if (type == 0)
    arrayName = this->PointArrayName;
  else
    arrayName = this->CellArrayName;

  // Check if array exists
  int exists = vtkSVGeneralUtils::CheckArrayExists(object, type, arrayName);

  if (exists)
  {
    if (type == 0)
    {
      // Get point array
      this->PointArray = vtkIntArray::SafeDownCast(
	  object->GetPointData()->GetArray(this->PointArrayName));
    }
    else
    {
      // Get cell array
      this->CellArray = vtkIntArray::SafeDownCast(
	  object->GetCellData()->GetArray(this->CellArrayName));
    }

  }

  return exists;
}

// ----------------------
// GetClosedEdgeLoops
// ----------------------
int vtkSVGetSphereRegions::GetClosedEdgeLoops(vtkPolyData *pd,vtkPolyData *linepd,
    int *numLoops)
{
  // Set up vtk objects
  vtkNew(vtkIdList, pointCells);
  vtkNew(vtkIdList, cellPoints);
  vtkNew(vtkIdList, cellNeighs);
  vtkNew(vtkPoints, pts);
  vtkNew(vtkCellArray, lines);
  vtkNew(vtkIntArray, scalars);

  // Get points
  int numPts = pd->GetNumberOfPoints();
  int *checked = new int[numPts];

  // Set up iterators
  vtkIdType nextPt=-1,prevPt=-1,prevId = -1;
  vtkIdType cellId = 0;
  vtkIdType pointId = 0;
  vtkIdType loopId = -1;
  int foundpt = 0;
  int iter = 0;

  // Initialize checking to 0
  for (int i=0;i < numPts;i++)
    checked[i] = 0;

  // Loop around num points
  pd->BuildLinks();
  for (int i=0;i < numPts;i++)
  {

    // If it hasn't been checked
    if (checked[i] == 0)
    {

      // Set checked
      checked[i] = 1;

      // Get value of point array
      if (this->PointArray->GetValue(i) == 1)
      {
        // Set up while vars
        nextPt = i;
        pd->GetPointCells(i,pointCells);
        prevId = pts->InsertNextPoint(pd->GetPoint(i));
        prevPt = i;
        loopId++;
        iter =0;

        // Loop around this loop
        while ((nextPt != i || iter == 0) && iter < 3000)
        {
          // See if we found a point
          foundpt = 0;
          for (int j=0; j < pointCells->GetNumberOfIds();j++)
          {
            // Get point cell
            cellId = pointCells->GetId(j);

            // Get cell points
            pd->GetCellPoints(cellId,cellPoints);

            // Loop around points
            for (int k=0; k < cellPoints->GetNumberOfIds();k++)
            {

              // Get point id
              pointId = cellPoints->GetId(k);

              // Check to see if point is one we want
              if (pointId != prevPt && pointId != nextPt &&
            this->PointArray->GetValue(pointId) == 1)
              {

                // Get the cell edge neighbors
                pd->GetCellEdgeNeighbors(-1,nextPt,pointId,cellNeighs);
                if (this->CellArray->GetValue(cellNeighs->GetId(0)) ==
                    this->CellArray->GetValue(cellNeighs->GetId(1)))
                {
                  // See if the two cells containing these points has same value
                  // We can step out if they do
                  continue;
                }

                // Otherwise, we did, check this point
                checked[pointId] = 1;
                prevPt = nextPt;
                nextPt = pointId;

                // Set the point in the loop
                vtkIdType tmpId = pts->InsertNextPoint(pd->GetPoint(pointId));
                lines->InsertNextCell(2);
                lines->InsertCellPoint(prevId);
                lines->InsertCellPoint(tmpId);

                // Set the loop id
                scalars->InsertNextValue(loopId);
                prevId = tmpId;
                foundpt=1;
                break;
              }
            }

            // We found a point!
            if (foundpt)
            {
              // get the cells for next iter
              pd->GetPointCells(nextPt,pointCells);
              break;
            }
          }
          iter++;
        }
      }
    }
  }

  // Set our loops
  linepd->SetPoints(pts);
  linepd->SetLines(lines);
  scalars->SetName("LoopId");
  linepd->GetCellData()->AddArray(scalars);
  linepd->BuildLinks();

  *numLoops = loopId+1;

  delete [] checked;
  return SV_OK;
}

// ----------------------
// SetSphereRegions
// ----------------------
int vtkSVGetSphereRegions::SetSphereRegions(vtkPolyData *pd, vtkPolyData *lines,
    int numLoops)
{
  // Set up vtk objects
  vtkNew(vtkThreshold, thresholder);
  vtkNew(vtkDataSetSurfaceFilter, surfacer);
  vtkNew(vtkIntArray, loopids);
  vtkNew(vtkPolyData, tmpPD);

  // Points and new data
  double pt1[3];
  double pt2[3];
  double *xc,*xy,*xz,*radius;
  xc = new double[numLoops];
  xy = new double[numLoops];
  xz = new double[numLoops];
  radius = new double[numLoops];

  // Set up loop thresholder
  thresholder->SetInputData(lines);
  thresholder->SetInputArrayToProcess(0,0,0,1,"LoopId");
  for (int i=0;i<numLoops;i++)
  {
    // Threshold the loop
    thresholder->ThresholdBetween(i,i);
    thresholder->Update();

    // Get surface
    surfacer->SetInputData(thresholder->GetOutput());
    surfacer->Update();
    tmpPD = surfacer->GetOutput();

    // Set up our centroid calculator
    double sx=0,sy=0,sz=0,sL=0;

    // Loop through points
    int numPts = tmpPD->GetNumberOfPoints();
    for (int j=0;j<numPts-1;j++)
    {
      // Get points in cell
      tmpPD->GetPoint(j,pt1);
      tmpPD->GetPoint(j+1,pt2);

      // Calculate length as well as component wise distance
      double L = sqrt(pow(pt2[0]-pt1[0],2) + pow(pt2[1]-pt1[1],2) + pow(pt2[2]-pt1[2],2));
      sx += ((pt1[0] + pt2[0])/2.0)*L;
      sy += ((pt1[1] + pt2[1])/2.0)*L;
      sz += ((pt1[2] + pt2[2])/2.0)*L;
      sL += L;
    }
    // Set our new sphere center
    xc[i] = sx/sL; xy[i] = sy/sL; xz[i] = sz/sL;

    // Lets find the maximum distance point for our radius
    double maxDist = 0.0;
    for (int j=0;j<numPts;j++)
    {
      // Get point
      tmpPD->GetPoint(j,pt1);

      // Get distance
      double dist = sqrt(pow(xc[i]-pt1[0],2) + pow(xy[i]-pt1[1],2) + pow(xz[i]-pt1[2],2));

      // Update distance if larger
      if (dist > maxDist)
        maxDist = dist;
    }

    // Set the radius of this sphere
    radius[i] = maxDist;
  }

  // Set up sphere cells
  vtkNew(vtkIntArray, sphereCells);
  int numArrays = pd->GetCellData()->GetNumberOfArrays();
  int exists = 0;

  // Loop through arrays, update if it exists, otherwise, start it up
  for (int i=0;i<numArrays;i++)
  {
    if (!strcmp(pd->GetCellData()->GetArrayName(i),
	  this->OutCellArrayName))
    {
      exists = 1;
    }
  }
  if (exists)
    sphereCells = vtkIntArray::SafeDownCast(pd->GetCellData()->GetArray(this->OutCellArrayName));
  else
  {
    sphereCells->SetNumberOfTuples(pd->GetNumberOfCells());
    for (vtkIdType id=0;id< pd->GetNumberOfCells();id++)
      sphereCells->InsertValue(id,0);
  }

  // Time to set our cells in sphere radius
  vtkIdType npts,*pts;
  double centroid[3];
  for (vtkIdType cellId=0;cellId < pd->GetNumberOfCells();cellId++)
  {
    // Get cell points
    pd->GetCellPoints(cellId,npts,pts);

    // Set up cell pts
    vtkNew(vtkPoints, polyPts);
    vtkNew(vtkIdTypeArray, polyPtIds);
    for (int i=0;i<npts;i++)
    {

      // Get point
      pd->GetPoint(pts[i],pt1);

      // Loop through out loops
      for (int j=0;j < numLoops;j++)
      {
        // Find idstance
        double dist = sqrt(pow(xc[j]-pt1[0],2) + pow(xy[j]-pt1[1],2) + pow(xz[j]-pt1[2],2));

        // If it inside our sphere, set this piece
        if (dist < this->SphereRadius)
          sphereCells->InsertValue(cellId,1);
      }
    }
  }

  // Cleanup
  delete [] xc;
  delete [] xy;
  delete [] xz;
  delete [] radius;

  // Remove the existing array if it is  there
  if (exists)
    pd->GetCellData()->RemoveArray(this->OutCellArrayName);

  // Set the new array for output points marking bifurcation region
  sphereCells->SetName(this->OutCellArrayName);
  pd->GetCellData()->AddArray(sphereCells);

  return SV_OK;
}
