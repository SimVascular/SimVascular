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

/** @file vtkGetSphereRegions.cxx
 *  @brief This implements the vtkGetSphereRegions filter as a class
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 */

#include "vtkGetSphereRegions.h"

#include "vtkFloatArray.h"
#include "vtkMath.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkObjectFactory.h"
#include "vtkPolyData.h"
#include "vtkUnstructuredGrid.h"
#include "vtkSmartPointer.h"
#include "vtkStreamingDemandDrivenPipeline.h"
#include "vtkCellArray.h"
#include "vtkIntArray.h"
#include "vtkDoubleArray.h"
#include "vtkCellData.h"
#include "vtkPointData.h"
#include "vtkCellDataToPointData.h"
#include "vtkEdgeTable.h"
#include "vtkXMLPolyDataWriter.h"
#include "vtkThreshold.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkPolygon.h"

#include <iostream>

vtkCxxRevisionMacro(vtkGetSphereRegions, "$Revision: 0.0 $");
vtkStandardNewMacro(vtkGetSphereRegions);

vtkGetSphereRegions::vtkGetSphereRegions()
{
    this->CellArrayName = 0;
    this->PointArrayName = 0;
    this->OutCellArrayName = 0;

    this->SphereRadius = 0.0;
}

vtkGetSphereRegions::~vtkGetSphereRegions()
{
//    if (this->CellArray)
//      this->CellArray->Delete();
//    if (this->PointArray)
//      this->PointArray->Delete();
}

void vtkGetSphereRegions::PrintSelf(ostream& os, vtkIndent indent)
{
}

// Generate Separated Surfaces with Region ID Numbers
int vtkGetSphereRegions::RequestData(
                                 vtkInformation *vtkNotUsed(request),
                                 vtkInformationVector **inputVector,
                                 vtkInformationVector *outputVector)
{
    // get the input and output
    vtkPolyData *input = vtkPolyData::GetData(inputVector[0]);
    vtkPolyData *output = vtkPolyData::GetData(outputVector);
    
    // Define variables used by the algorithm
    vtkSmartPointer<vtkPoints> inpts = vtkSmartPointer<vtkPoints>::New();
    vtkSmartPointer<vtkCellArray> inPolys = vtkSmartPointer<vtkCellArray>::New();
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
	return 1;
    }

    if (this->GetArrays(input,0) != 1)
    {
      std::cout<<"No Point Array Named "<<this->PointArrayName<<" on surface"<<endl;
      return 0;
    }
    if (this->GetArrays(input,1) != 1)
    {
      std::cout<<"No Cell Array Named "<<this->CellArrayName<<" on surface"<<endl;
      return 0;
    }
    if (this->OutCellArrayName == 0)
    {
      std::cout<<"Need array name for output cell data"<<endl;
      return 0;
    }

    input->BuildLinks();
    vtkSmartPointer<vtkPolyData> linepd = 
      vtkSmartPointer<vtkPolyData>::New();
    int numloops = 0;
    this->GetClosedEdgeLoops(input,linepd,&numloops);
    this->SetSphereRegions(input,linepd,numloops);

    output->DeepCopy(input);
    return 1;
}

int vtkGetSphereRegions::GetArrays(vtkPolyData *object,int type)
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
	    this->PointArrayName))
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
	    this->CellArrayName))
      {
	exists = 1;
      }
    }
  }

  if (exists)
  {
    if (type == 0)
    {
      this->PointArray = vtkIntArray::SafeDownCast(
	  object->GetPointData()->GetArray(this->PointArrayName));
    }
    else
    {
      this->CellArray = vtkIntArray::SafeDownCast(
	  object->GetCellData()->GetArray(this->CellArrayName));
    }

  }

  return exists;
}

int vtkGetSphereRegions::GetClosedEdgeLoops(vtkPolyData *pd,vtkPolyData *linepd,
    int *numLoops)
{
  vtkSmartPointer<vtkIdList> pointCells = 
    vtkSmartPointer<vtkIdList>::New();
  vtkSmartPointer<vtkIdList> cellPoints = 
    vtkSmartPointer<vtkIdList>::New();
  vtkSmartPointer<vtkIdList> cellNeighs = 
    vtkSmartPointer<vtkIdList>::New();
  vtkSmartPointer<vtkPoints> pts = 
    vtkSmartPointer<vtkPoints>::New();
  vtkSmartPointer<vtkCellArray> lines = 
    vtkSmartPointer<vtkCellArray>::New();
  vtkSmartPointer<vtkIntArray> scalars = 
    vtkSmartPointer<vtkIntArray>::New();
 
  int numPts = pd->GetNumberOfPoints();
  int *checked = new int[numPts];
  vtkIdType nextPt=-1,prevPt=-1,prevId = -1;
  vtkIdType cellId = 0;
  vtkIdType pointId = 0;
  vtkIdType loopId = -1;
  int foundpt = 0;
  int iter = 0;

  for (int i=0;i < numPts;i++)
  {
    checked[i] = 0;
  }

  pd->BuildLinks();
  for (int i=0;i < numPts;i++)
  {
    if (checked[i] == 0)
    {
      checked[i] = 1;
      if (this->PointArray->GetValue(i) == 1)
      {
	nextPt = i;
        pd->GetPointCells(i,pointCells);
	prevId = pts->InsertNextPoint(pd->GetPoint(i));
	prevPt = i;
	loopId++;
	iter =0;
	while ((nextPt != i || iter == 0) && iter < 3000)
	{
	  foundpt = 0;
	  for (int j=0; j < pointCells->GetNumberOfIds();j++)
	  {
	    cellId = pointCells->GetId(j);
	    pd->GetCellPoints(cellId,cellPoints);
	    for (int k=0; k < cellPoints->GetNumberOfIds();k++)
	    {
	      pointId = cellPoints->GetId(k);
	      if (pointId != prevPt && pointId != nextPt && 
		  this->PointArray->GetValue(pointId) == 1)
	      {

		//std::cout<<"PointID "<<pointId<<endl;
		pd->GetCellEdgeNeighbors(-1,nextPt,pointId,cellNeighs);
		if (this->CellArray->GetValue(cellNeighs->GetId(0)) == 
		    this->CellArray->GetValue(cellNeighs->GetId(1))) 
		{
		  continue;
		}

		checked[pointId] = 1;
		prevPt = nextPt;
		nextPt = pointId;
		vtkIdType tmpId = pts->InsertNextPoint(pd->GetPoint(pointId));
		lines->InsertNextCell(2);
		lines->InsertCellPoint(prevId);
		lines->InsertCellPoint(tmpId);
		scalars->InsertNextValue(loopId);
		prevId = tmpId;
		foundpt=1;
		break;
	      }
	    }
	    if (foundpt)
	    {
	      //std::cout<<"Breaking!"<<endl;
	      pd->GetPointCells(nextPt,pointCells);
	      break;
	    }
	  }
	  iter++;
	}
      }
    }
  }

  linepd->SetPoints(pts);
  linepd->SetLines(lines);
  scalars->SetName("LoopId");
  linepd->GetCellData()->AddArray(scalars);
  linepd->BuildLinks();

  *numLoops = loopId+1;

  delete [] checked;
  return 1;
}

int vtkGetSphereRegions::SetSphereRegions(vtkPolyData *pd, vtkPolyData *lines,
    int numLoops)
{
  vtkSmartPointer<vtkThreshold> thresholder = 
    vtkSmartPointer<vtkThreshold>::New();
  vtkSmartPointer<vtkDataSetSurfaceFilter> surfacer = 
    vtkSmartPointer<vtkDataSetSurfaceFilter>::New();
  vtkSmartPointer<vtkIntArray> loopids = 
    vtkSmartPointer<vtkIntArray>::New();
  vtkSmartPointer<vtkPolyData> tmpPD = 
    vtkSmartPointer<vtkPolyData>::New();
  double pt1[3];
  double pt2[3];
  double *xc,*xy,*xz,*radius;
  xc = new double[numLoops];
  xy = new double[numLoops];
  xz = new double[numLoops];
  radius = new double[numLoops];

  std::cout<<"Num Loops "<<numLoops<<endl;
  thresholder->SetInputData(lines);
  thresholder->SetInputArrayToProcess(0,0,0,1,"LoopId");
  for (int i=0;i<numLoops;i++)
  {
    thresholder->ThresholdBetween(i,i);
    thresholder->Update();

    surfacer->SetInputData(thresholder->GetOutput());
    surfacer->Update();
    tmpPD = surfacer->GetOutput();

    double sx=0,sy=0,sz=0,sL=0;

    int numPts = tmpPD->GetNumberOfPoints();
    for (int j=0;j<numPts-1;j++)
    {
      tmpPD->GetPoint(j,pt1);
      tmpPD->GetPoint(j+1,pt2);

      double L = sqrt(pow(pt2[0]-pt1[0],2) + pow(pt2[1]-pt1[1],2) + pow(pt2[2]-pt1[2],2));
      sx += ((pt1[0] + pt2[0])/2.0)*L;
      sy += ((pt1[1] + pt2[1])/2.0)*L;
      sz += ((pt1[2] + pt2[2])/2.0)*L;
      sL += L;
    }
    xc[i] = sx/sL; xy[i] = sy/sL; xz[i] = sz/sL;
    std::cout<<"Sphere Center "<<i<<" is "<<xc[i]<<" "<<xy[i]<<" "<<xz[i]<<endl;
    double maxDist = 0.0;
    for (int j=0;j<numPts;j++)
    {
      tmpPD->GetPoint(j,pt1);

      double dist = sqrt(pow(xc[i]-pt1[0],2) + pow(xy[i]-pt1[1],2) + pow(xz[i]-pt1[2],2));
      if (dist > maxDist)
	maxDist = dist;
    }
    radius[i] = maxDist;
    std::cout<<"With Radius "<<radius[i]<<endl;
  }

  vtkSmartPointer<vtkIntArray> sphereCells = 
    vtkSmartPointer<vtkIntArray>::New();
  int numArrays = pd->GetCellData()->GetNumberOfArrays();
  int exists = 0;
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

  vtkIdType npts,*pts;
  double centroid[3];
  for (vtkIdType cellId=0;cellId < pd->GetNumberOfCells();cellId++)
  {
    pd->GetCellPoints(cellId,npts,pts);
    vtkSmartPointer<vtkPoints> polyPts = vtkSmartPointer<vtkPoints>::New();
    vtkSmartPointer<vtkIdTypeArray> polyPtIds = vtkSmartPointer<vtkIdTypeArray>::New();
    for (int i=0;i<npts;i++)
    {
      //polyPtIds->InsertValue(i,i);
      //polyPts->InsertNextPoint(pd->GetPoint(pts[i]));
    //vtkPolygon::ComputeCentroid(polyPtIds,polyPts,centroid);
      pd->GetPoint(pts[i],pt1);
      for (int j=0;j < numLoops;j++)
      {
        double dist = sqrt(pow(xc[j]-pt1[0],2) + pow(xy[j]-pt1[1],2) + pow(xz[j]-pt1[2],2));
        //if (dist < 1.5*radius[j])
        if (dist < this->SphereRadius)
	  sphereCells->InsertValue(cellId,1);
      }
    }
  }

  delete [] xc;
  delete [] xy;
  delete [] xz;
  delete [] radius;

  if (exists)
    pd->GetCellData()->RemoveArray(this->OutCellArrayName);
  sphereCells->SetName(this->OutCellArrayName);
  pd->GetCellData()->AddArray(sphereCells);

  return 1;
}
