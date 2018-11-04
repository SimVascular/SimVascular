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

#include "vtkSVGetBoundaryFaces.h"

#include "vtkCellArray.h"
#include "vtkCellData.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkErrorCode.h"
#include "vtkFeatureEdges.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkIntArray.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPointLocator.h"
#include "vtkPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkTriangle.h"
#include "vtkTriangleStrip.h"
#include "vtkUnsignedCharArray.h"
#include "vtkUnstructuredGrid.h"

#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"
#include "vtkSVMathUtils.h"

#include <iostream>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVGetBoundaryFaces);

// ----------------------
// Constructor
// ----------------------
vtkSVGetBoundaryFaces::vtkSVGetBoundaryFaces()
{
    this->FeatureAngle = 50.0;
    this->NewScalars = vtkIntArray::New();
    this->WorkPd = vtkPolyData::New();
    this->BoundaryLines = vtkPolyData::New();

    this->CheckCells = vtkIdList::New();
    this->CheckCells2 = vtkIdList::New();
    this->CheckCellsCareful = vtkIdList::New();
    this->CheckCellsCareful2 = vtkIdList::New();

    this->BoundaryPointArray = vtkIntArray::New();
    this->BoundaryCellArray = vtkIntArray::New();

    this->RegionAreas = vtkDoubleArray::New();

    this->checked = NULL;
    this->checkedcarefully = NULL;
    this->pointMapper = NULL;

    this->NumberOfRegions = 0;

    this->BoundaryEdges    = 0;
    this->ManifoldEdges    = 0;
    this->NonManifoldEdges = 0;
    this->FeatureEdges     = 1;

    this->RegionIdsArrayName = NULL;

    this->ExtractLargestRegion = 0;
}

// ----------------------
// Destructor
// ----------------------
vtkSVGetBoundaryFaces::~vtkSVGetBoundaryFaces()
{
    if (this->NewScalars)
    {
      this->NewScalars->Delete();
      this->NewScalars = NULL;
    }
    if (this->WorkPd)
    {
      this->WorkPd->Delete();
      this->WorkPd = NULL;
    }
    if (this->BoundaryLines)
    {
      this->BoundaryLines->Delete();
      this->BoundaryLines = NULL;
    }
    if (this->BoundaryPointArray)
    {
      this->BoundaryPointArray->Delete();
      this->BoundaryPointArray = NULL;
    }
    if (this->BoundaryCellArray)
    {
      this->BoundaryCellArray->Delete();
      this->BoundaryCellArray = NULL;
    }
    if (this->RegionAreas)
    {
      this->RegionAreas->Delete();
      this->RegionAreas = NULL;
    }
    if (this->CheckCells)
    {
      this->CheckCells->Delete();
      this->CheckCells = NULL;
    }
    if (this->CheckCells2)
    {
      this->CheckCells2->Delete();
      this->CheckCells2 = NULL;
    }
    if (this->CheckCellsCareful)
    {
      this->CheckCellsCareful->Delete();
      this->CheckCellsCareful = NULL;
    }
    if (this->CheckCellsCareful2)
    {
      this->CheckCellsCareful2->Delete();
      this->CheckCellsCareful2 = NULL;
    }

    if (this->checked != NULL)
      delete [] this->checked;
    if (this->checkedcarefully != NULL)
      delete [] this->checkedcarefully;
    if (this->pointMapper != NULL)
      delete [] this->pointMapper;

    if (this->RegionIdsArrayName != NULL)
    {
      delete [] this->RegionIdsArrayName;
      this->RegionIdsArrayName = NULL;
    }
}

void vtkSVGetBoundaryFaces::PrintSelf(ostream& os, vtkIndent indent)
{
    this->Superclass::PrintSelf(os,indent);
    os << indent << "Feature Angle: " << this->FeatureAngle << "\n";
}

// ----------------------
// RequestData
// ----------------------
int vtkSVGetBoundaryFaces::RequestData(vtkInformation *vtkNotUsed(request),
                                       vtkInformationVector **inputVector,
                                       vtkInformationVector *outputVector)
{
    // get the input and output
    vtkPolyData *input = vtkPolyData::GetData(inputVector[0]);
    vtkPolyData *output = vtkPolyData::GetData(outputVector);

    // Define variables used by the algorithm
    int reg = 0;
    int pt = 0;
    vtkNew(vtkPoints,  inpts);
    vtkNew(vtkCellArray,  inPolys);
    vtkPoints *newPts;
    vtkIdType numPts, numPolys;
    vtkIdType newId, cellId;

    //Get input points, polys and set the up in the vtkPolyData mesh
    inpts = input->GetPoints();
    inPolys = input->GetPolys();
    this->WorkPd->SetPoints(inpts);
    this->WorkPd->SetPolys(inPolys);
    //Build Links in the mesh to be able to perform complex polydata processes;
    this->WorkPd->BuildLinks();

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

    //Set up Region scalar for each surface
    this->NewScalars->SetNumberOfTuples(numPolys);

    //Set up Feature Edges for Boundary Edge Detection
    vtkPolyData* inputCopy = input->NewInstance();
    inputCopy->ShallowCopy(input);

    //Set the Data to hold onto given Point Markers
    vtkNew(vtkFeatureEdges, boundaries);
    inputCopy->GlobalReleaseDataFlagOff();
    boundaries->SetInputData(inputCopy);
    boundaries->SetBoundaryEdges(this->BoundaryEdges);
    boundaries->SetManifoldEdges(this->ManifoldEdges);
    boundaries->SetNonManifoldEdges(this->NonManifoldEdges);
    boundaries->SetFeatureEdges(this->FeatureEdges);
    boundaries->SetFeatureAngle(this->FeatureAngle);
    inputCopy->Delete();
    boundaries->Update();

    // Set the boundary lines
    this->BoundaryLines->DeepCopy(boundaries->GetOutput());

    // Initialize the arrays to be used in the flood fills
    this->SetBoundaryArrays();

    vtkDebugMacro("Starting Boundary Face Separation");
    //Set Region value of each cell to be zero initially
    for(cellId = 0; cellId < numPolys ; cellId ++)
      this->NewScalars->InsertValue(cellId, reg);

    //Go through each cell and perfrom region identification proces
    for (cellId=0; cellId< numPolys; cellId++)
    {
       //Check to make sure the value of the region at this cellId hasn't been set
       if (this->NewScalars->GetValue(cellId) == 0)
       {
         reg++;
         this->CheckCells->InsertNextId(cellId);
               double area = 0.0;
         //Call function to find all cells within certain region
         this->FindBoundaryRegion(reg, 1, area);
         this->RegionAreas->InsertNextValue(area);
         this->CheckCells->Reset();
         this->CheckCells2->Reset();
         this->CheckCellsCareful->Reset();
         this->CheckCellsCareful2->Reset();
       }
    }

    // Check to see if anything left
    int extraregion=0;
    for(cellId = 0; cellId < numPolys ; cellId ++)
    {
      double area = 0.0;
      if (this->checked[cellId] == 0 || this->checkedcarefully[cellId] == 0)
      {
	      this->NewScalars->InsertValue(cellId,reg+1);
        this->AddCellArea(cellId, area);
	      extraregion=1;
      }
      this->RegionAreas->InsertNextValue(area);
    }
    if (extraregion)
    {
      vtkDebugMacro("I am incrementing region");
      reg++;
    }

    //Copy all the input geometry and data to the output
    output->SetPoints(inpts);
    output->SetPolys(inPolys);
    output->GetPointData()->PassData(input->GetPointData());
    output->GetCellData()->PassData(input->GetCellData());

    //Add the new scalars array to the output
    this->NewScalars->SetName(this->RegionIdsArrayName);
    output->GetCellData()->AddArray(this->NewScalars);
    output->GetCellData()->SetActiveScalars(this->RegionIdsArrayName);

    // If extracting largets region, get it out
    if (this->ExtractLargestRegion)
    {
      double maxVal = 0.0;
      int maxRegion = -1;
      for (int i=0; i<reg; i++)
      {
        if (this->RegionAreas->GetValue(i) > maxVal)
        {
          maxVal = this->RegionAreas->GetValue(i);
          maxRegion = i+1;
        }
      }
      vtkSVGeneralUtils::ThresholdPd(output, maxRegion, maxRegion, 1, this->RegionIdsArrayName);
    }

    // Total number of regions
    this->NumberOfRegions = reg;

    return SV_OK;
}

void vtkSVGetBoundaryFaces::FindBoundaryRegion(int reg, int start, double &area)
{
  //Variables used in function
  int i;
  vtkIdType j,k,l,cellId;
  vtkIdType *pts = 0;
  vtkIdType npts = 0;
  vtkIdType numNei, nei, p1, p2, nIds, neis;

  //Id List to store neighbor cells for each set of nodes and a cell
  vtkNew(vtkIdList, neighbors);
  vtkNew(vtkIdList, tmp);

  //Variable for accessing neiIds list
  vtkIdType sz = 0;

  //Variables for the boundary cells adjacent to the boundary point
  vtkNew(vtkIdList, bLinesOne);
  vtkNew(vtkIdList, bLinesTwo);

  vtkIdType numCheckCells;
  //Get neighboring cell for each pair of points in current cell
  while ((numCheckCells = this->CheckCells->GetNumberOfIds()) > 0)
  {
    for (int c=0;c<numCheckCells;c++)
    {
      cellId = this->CheckCells->GetId(c);
      //Get the three points of the cell
      this->WorkPd->GetCellPoints(cellId,npts,pts);
      if (this->checked[cellId] == 0)
      {
        //Mark cell as checked and insert the fillnumber value to cell
        this->NewScalars->InsertValue(cellId,reg);
        this->AddCellArea(cellId, area);
        this->checked[cellId] = 1;
        for (i=0; i < npts; i++)
        {
          p1 = pts[i];
          //Get the cells attached to each point
          this->WorkPd->GetPointCells(p1,neighbors);
          numNei = neighbors->GetNumberOfIds();

          //For each neighboring cell
          for (j=0;j < numNei;j++)
          {
            //If this cell is close to a boundary
            if (this->BoundaryCellArray->GetValue(neighbors->GetId(j)))
            {
              //If this cell hasn't been checked already
              if (this->checkedcarefully[neighbors->GetId(j)] == 0)
              {
                //Add this cell to the careful check cells list and run
                //the region finding tip toe code
                this->CheckCellsCareful->
                InsertNextId(neighbors->GetId(j));
                this->FindBoundaryRegionTipToe(reg, area);

                this->CheckCellsCareful->Reset();
                this->CheckCellsCareful2->Reset();
              }
            }
            //Cell needs to be added to check list
            else
            {
              this->CheckCells2->InsertNextId(neighbors->GetId(j));
            }
          }
        }
      }
      //If the start cell is a boundary cell
      else if (this->checkedcarefully[cellId] == 0 && start)
      {
        // Reset the check cell list and start a careful search
        start=0;
        this->CheckCells->Reset();
        this->CheckCellsCareful->InsertNextId(cellId);
        this->FindBoundaryRegionTipToe(reg, area);
      }
    }

    //Swap the current check list to the full check list and continue
    tmp = this->CheckCells;
    this->CheckCells = this->CheckCells2;
    this->CheckCells2 = tmp;
    tmp->Reset();
  }
}

void vtkSVGetBoundaryFaces::FindBoundaryRegionTipToe(int reg, double &area)
{
  //Variables used in function
  int i;
  vtkIdType j,k,l;
  vtkIdType *pts = 0;
  vtkIdType npts = 0;
  vtkIdType cellId;
  vtkIdType numNei, nei, p1, p2, nIds, neiId;

  //Id List to store neighbor cells for each set of nodes and a cell
  vtkNew(vtkIdList, tmp);
  vtkNew(vtkIdList, neiIds);

  //Variable for accessing neiIds list
  vtkIdType sz = 0;

  //Variables for the boundary cells adjacent to the boundary point
  vtkNew(vtkIdList, bLinesOne);
  vtkNew(vtkIdList, bLinesTwo);

  vtkIdType numCheckCells;
  //Get neighboring cell for each pair of points in current cell
  //While there are still cells to be checked
  while ((numCheckCells = this->CheckCellsCareful->GetNumberOfIds()) > 0)
  {
    for (int c=0;c<numCheckCells;c++)
    {
      neiIds->Reset();
      cellId = this->CheckCellsCareful->GetId(c);
      //Get the three points of the cell
      this->WorkPd->GetCellPoints(cellId,npts,pts);
      if (this->checkedcarefully[cellId] == 0)
      {
        //Update this cell to have been checked carefully and assign it
        //with the fillnumber scalar
        this->NewScalars->InsertValue(cellId,reg);
        this->AddCellArea(cellId, area);
        this->checkedcarefully[cellId] = 1;
        //For each edge of the cell
        vtkDebugMacro("Checking edges of cell " << cellId);
        for (i=0; i < npts; i++)
        {
          p1 = pts[i];
          p2 = pts[(i+1)%(npts)];

          vtkNew(vtkIdList, neighbors);
          //Initial check to make sure the cell is in fact a face cell
          this->WorkPd->GetCellEdgeNeighbors(cellId,p1,p2,neighbors);
          numNei = neighbors->GetNumberOfIds();

          //Check to make sure it is an oustide surface cell,
          //i.e. one neighbor
          if (numNei==1)
          {
            int count = 0;
            //Check to see if cell is on the boundary,
            //if it is get adjacent lines
            if (this->BoundaryPointArray->GetValue(p1) == 1)
              count++;

            if (this->BoundaryPointArray->GetValue(p2) == 1)
              count++;

            nei=neighbors->GetId(0);
            //if cell is not on the boundary, add new cell to check list
            if (count < 2)
              neiIds->InsertNextId(nei);

            //if cell is on boundary, check to make sure it isn't
            //false positive; don't add to check list. This is done by
            //getting the boundary lines attached to each point, then
            //intersecting the two lists. If the result is zero, then this
            //is a false positive
            else
            {
              this->BoundaryLines->BuildLinks();
              vtkIdType bPt1 = pointMapper[p1];
              this->BoundaryLines->GetPointCells(bPt1,bLinesOne);

              vtkIdType bPt2 = pointMapper[p2];
              this->BoundaryLines->GetPointCells(bPt2,bLinesTwo);

              bLinesOne->IntersectWith(bLinesTwo);
              //Cell is false positive. Add to check list.
              if (bLinesOne->GetNumberOfIds() == 0)
              {
                neiIds->InsertNextId(nei);
              }
            }
          }
        }

        nIds = neiIds->GetNumberOfIds();
        if (nIds>0)
        {
          //Add all Ids in current list to global list of Ids
          for (k=0; k< nIds;k++)
          {
            neiId = neiIds->GetId(k);
            if (this->checkedcarefully[neiId]==0)
            {
              this->CheckCellsCareful2->InsertNextId(neiId);
            }
            else if (this->checked[neiId]==0)
            {
              this->CheckCells2->InsertNextId(neiId);
            }
          }
        }
      }
    }

    //Add current list of checked cells to the full list and continue
    tmp = this->CheckCellsCareful;
    this->CheckCellsCareful = this->CheckCellsCareful2;
    this->CheckCellsCareful2 = tmp;
    tmp->Reset();
  }
}


void vtkSVGetBoundaryFaces::SetBoundaryArrays()
{
  //Variables used in the function
  double pt[3];
  vtkIdType pointId,bp,bp2,i;
  vtkNew(vtkIdList, bpCellIds);
  //Point locator to find points on mesh that are the points on the boundary
  //lines
  vtkNew(vtkPointLocator, pointLocator);
  pointLocator->SetDataSet(this->WorkPd);
  pointLocator->BuildLocator();

  // Get number of points and cells
  int numMeshPoints = this->WorkPd->GetNumberOfPoints();
  int numMeshCells = this->WorkPd->GetNumberOfCells();

  // Set up check arrays
  this->checked = new int[numMeshCells];
  this->checkedcarefully = new int[numMeshCells];
  this->pointMapper = new int[numMeshCells];
  for (int i =0;i<numMeshCells;i++)
    this->checked[i] = 0;

  // Set up boundary arrays
  this->BoundaryPointArray->SetNumberOfTuples(numMeshPoints);
  this->BoundaryPointArray->FillComponent(0, 0);
  this->BoundaryCellArray->SetNumberOfTuples(numMeshCells);
  this->BoundaryCellArray->FillComponent(0,0);

  // Number of boundary line points
  int numPoints = this->BoundaryLines->GetNumberOfPoints();

  for (pointId = 0;pointId < numPoints;pointId++)
  {
    this->BoundaryLines->GetPoint(pointId,pt);
    //Find point on mesh
    bp = pointLocator->FindClosestPoint(pt);
    this->pointMapper[bp] = pointId;
    this->BoundaryPointArray->InsertValue(bp,1);
    this->WorkPd->GetPointCells(bp,bpCellIds);
    //Set the point mapping array
    //Assign each cell attached to this point as a boundary cell
    for (i = 0;i < bpCellIds->GetNumberOfIds();i++)
    {
      this->BoundaryCellArray->InsertValue(bpCellIds->GetId(i),1);
      this->checked[bpCellIds->GetId(i)] = 1;
    }
  }

  // Flip the values of checked carefully
  for (int i=0;i < numMeshCells;i++)
  {
    if (this->checked[i] == 0)
      this->checkedcarefully[i] = 1;
    else
      this->checkedcarefully[i] = 0;
  }
}

int vtkSVGetBoundaryFaces::AddCellArea(const int cellId, double &area)
{
  // Get cell points
  vtkIdType npts, *pts;
  this->WorkPd->GetCellPoints(cellId, npts, pts);

  // Get points
  double pt0[3], pt1[3], pt2[3];
  this->WorkPd->GetPoint(pts[0], pt0);
  this->WorkPd->GetPoint(pts[1], pt1);
  this->WorkPd->GetPoint(pts[2], pt2);

  // Calculate are of triangle
  area += std::abs(vtkSVMathUtils::ComputeTriangleArea(pt0, pt1, pt2));
  return SV_OK;
}
