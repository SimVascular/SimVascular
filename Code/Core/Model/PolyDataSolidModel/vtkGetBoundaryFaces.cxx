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

/** @file vtkGetBoundaryFaces.cxx
 *  @brief This implements the vtkGetBoundaryFaces filter as a class
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 */

#include "vtkGetBoundaryFaces.h"

#include "vtkExtractEdges.h"
#include "vtkFloatArray.h"
#include "vtkMath.h"
#include "vtkMergePoints.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkObjectFactory.h"
#include "vtkPolyData.h"
#include "vtkPolygon.h"
#include "vtkSmartPointer.h"
#include "vtkStreamingDemandDrivenPipeline.h"
#include "vtkTriangleStrip.h"
#include "vtkUnsignedCharArray.h"
#include "vtkCellArray.h"
#include "vtkIntArray.h"
#include "vtkCellData.h"
#include "vtkPointData.h"
#include "vtkIncrementalPointLocator.h"
#include "vtkFeatureEdges.h"
#include "vtkCellLocator.h"

#include <iostream>

#include "cv_polydatasolid_utils.h"
vtkCxxRevisionMacro(vtkGetBoundaryFaces, "$Revision: 0.0 $");
vtkStandardNewMacro(vtkGetBoundaryFaces);

// Construct object with feature angle = 50.0;
vtkGetBoundaryFaces::vtkGetBoundaryFaces()
{
/** 
 * @brief member data that is the Feature Edges filter. Extracts edges!
 */
    this->boundaries = vtkFeatureEdges::New();
/** 
 * @Default angle of extraction. Can be easily changed by setting Feature 
 * Angle
 */
    this->FeatureAngle = 50.0;
/** 
 * @brief Integer Array containing the values associated with the region 
 * number at each face
 */
    this->newScalars = vtkIntArray::New();
/** 
 * @brief PolyData that contains the output mesh with region scalars
 */
    this->mesh = vtkPolyData::New();
/** 
 * @brief The series of points and lines output from the Feature Edges filter
 */
    this->boundaryLines = vtkPolyData::New();
/** 
 * @brief The list of cell Ids that have been checked
 */
    this->CheckCells = vtkIdList::New();
    this->CheckCells2 = vtkIdList::New();
    this->CheckCellsCareful = vtkIdList::New();
    this->CheckCellsCareful2 = vtkIdList::New();

    this->BoundaryPointArray = vtkIntArray::New();
    this->BoundaryCellArray = vtkIntArray::New();

    this->checked = NULL;
    this->checkedcarefully = NULL;
    this->pointMapper = NULL;

    this->NumberOfRegions = 0;
}

vtkGetBoundaryFaces::~vtkGetBoundaryFaces()
{
    if (this->boundaries)
        this->boundaries->Delete();

    if (this->newScalars)
	this->newScalars->Delete();

    if (this->mesh)
	this->mesh->Delete();

    if (this->boundaryLines)
	this->boundaryLines->Delete();

    if (this->BoundaryPointArray)
      this->BoundaryPointArray->Delete();

    if (this->BoundaryCellArray)
      this->BoundaryCellArray->Delete();

    if (this->CheckCells)
	this->CheckCells->Delete();

    if (this->CheckCells2)
	this->CheckCells2->Delete();

    if (this->CheckCellsCareful)
	this->CheckCellsCareful->Delete();

    if (this->CheckCellsCareful2)
	this->CheckCellsCareful2->Delete();

    if (this->checked != NULL)
      delete [] this->checked;
    if (this->checkedcarefully != NULL)
      delete [] this->checkedcarefully;
    if (this->pointMapper != NULL)
      delete [] this->pointMapper;
}

void vtkGetBoundaryFaces::PrintSelf(ostream& os, vtkIndent indent)
{
    this->Superclass::PrintSelf(os,indent);
    os << indent << "Feature Angle: " << this->FeatureAngle << "\n";
}

// Generate Separated Surfaces with Region ID Numbers
int vtkGetBoundaryFaces::RequestData(
                                 vtkInformation *vtkNotUsed(request),
                                 vtkInformationVector **inputVector,
                                 vtkInformationVector *outputVector)
{
    // get the input and output
    vtkPolyData *input = vtkPolyData::GetData(inputVector[0]);
    vtkPolyData *output = vtkPolyData::GetData(outputVector);
    
    // Define variables used by the algorithm
    int reg = 0;                                          
    int pt = 0;                                          
    vtkSmartPointer<vtkPoints> inpts = vtkSmartPointer<vtkPoints>::New();
    vtkSmartPointer<vtkCellArray> inPolys = vtkSmartPointer<vtkCellArray>::New();
    vtkPoints *newPts;
    vtkIdType numPts, numPolys;
    vtkIdType newId, cellId;

    //Get input points, polys and set the up in the vtkPolyData mesh
    inpts = input->GetPoints();
    inPolys = input->GetPolys();
    this->mesh->SetPoints(inpts);
    this->mesh->SetPolys(inPolys);
    //Build Links in the mesh to be able to perform complex polydata processes;
    this->mesh->BuildLinks();

    //Get the number of Polys for scalar  allocation
    numPolys = input->GetNumberOfPolys();
    numPts = input->GetNumberOfPoints();

    //Check the input to make sure it is there
    if (numPolys < 1)               
    {
        vtkDebugMacro("No input!");
	return 1;
    }

    //Set up Region scalar for each surface
    this->newScalars->SetNumberOfTuples(numPolys);

    //Set up Feature Edges for Boundary Edge Detection
    vtkPolyData* inputCopy = input->NewInstance();
    inputCopy->ShallowCopy(input);
    //Set the Data to hold onto given Point Markers
    inputCopy->GlobalReleaseDataFlagOff();
    this->boundaries->SetInputData(inputCopy);
    this->boundaries->BoundaryEdgesOff();
    this->boundaries->ManifoldEdgesOff();
    this->boundaries->NonManifoldEdgesOff();
    this->boundaries->FeatureEdgesOn();
    this->boundaries->SetFeatureAngle(this->FeatureAngle);
    inputCopy->Delete();
    this->boundaries->Update();

    this->boundaryLines->DeepCopy(this->boundaries->GetOutput());
    //this->boundaryLines->BuildLinks();
    //std::cout<<"Number Points: "<<this->boundaryLines->GetNumberOfPoints()<<endl;
 
    this->SetBoundaryArrays();

    vtkDebugMacro("Starting Boundary Face Separation");
    //Set Region value of each cell to be zero initially
    for(cellId = 0; cellId < numPolys ; cellId ++)
    {
        this->newScalars->InsertValue(cellId, reg);
    }

    //Go through each cell and perfrom region identification proces
    for (cellId=0; cellId< numPolys; cellId++)
    {
       //Check to make sure the value of the region at this cellId hasn't been set
       if (this->newScalars->GetValue(cellId) == 0)
       {
	   reg++;
	   this->CheckCells->InsertNextId(cellId);
	   //Call function to find all cells within certain region
	   this->FindBoundaryRegion(reg,1);
	   this->CheckCells->Reset();
	   this->CheckCells2->Reset();
	   this->CheckCellsCareful->Reset();
	   this->CheckCellsCareful2->Reset();
       }
    }
    int extraregion=0;
    for(cellId = 0; cellId < numPolys ; cellId ++)
    {
      if (this->checked[cellId] == 0 || this->checkedcarefully[cellId] == 0)
      {
	this->newScalars->InsertValue(cellId,reg+1);
	extraregion=1;
      }
    }
    if (extraregion)
    {
      std::cout<<"I am incrementing region"<<endl;
      reg++;
    }

    //Copy all the input geometry and data to the output
    output->SetPoints(inpts);
    output->SetPolys(inPolys);
    output->GetPointData()->PassData(input->GetPointData());
    output->GetCellData()->PassData(input->GetCellData());

    //Add the new scalars array to the output
    this->newScalars->SetName("ModelFaceID");
    output->GetCellData()->AddArray(this->newScalars);
    output->GetCellData()->SetActiveScalars("ModelFaceID");

    this->NumberOfRegions = reg;

    return 1;
}

void vtkGetBoundaryFaces::FindBoundaryRegion(int reg,int start)
{
    //Variables used in function
    int i;
    vtkIdType j,k,l,cellId; 
    vtkIdType *pts = 0;
    vtkIdType npts = 0;
    vtkIdType numNei, nei, p1, p2, nIds, neis;
    
    //Id List to store neighbor cells for each set of nodes and a cell
    vtkSmartPointer<vtkIdList> neighbors = vtkSmartPointer<vtkIdList>::New();
    vtkSmartPointer<vtkIdList> tmp = vtkSmartPointer<vtkIdList>::New();

    //Variable for accessing neiIds list
    vtkIdType sz = 0;

    //Variables for the boundary cells adjacent to the boundary point
    vtkSmartPointer<vtkIdList> bLinesOne = vtkSmartPointer<vtkIdList>::New();
    vtkSmartPointer<vtkIdList> bLinesTwo = vtkSmartPointer<vtkIdList>::New();

    vtkIdType numCheckCells;
    //Get neighboring cell for each pair of points in current cell
    while ((numCheckCells = this->CheckCells->GetNumberOfIds()) > 0)
    {
      for (int c=0;c<numCheckCells;c++)
      {
	cellId = this->CheckCells->GetId(c);
	//Get the three points of the cell
	this->mesh->GetCellPoints(cellId,npts,pts);
	if (this->checked[cellId] == 0)
	{
	  //Mark cell as checked and insert the fillnumber value to cell
          this->newScalars->InsertValue(cellId,reg);
	  this->checked[cellId] = 1;
          for (i=0; i < npts; i++)
          {
	    p1 = pts[i];
	    //Get the cells attached to each point
	    this->mesh->GetPointCells(p1,neighbors);
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
		  this->FindBoundaryRegionTipToe(reg);

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
	//This statement is for if the start cell is a boundary cell
	else if (this->checkedcarefully[cellId] == 0 && start)
	{
	  //std::cout<<"Aqui senor"<<endl;
	  start=0;
	  this->CheckCells->Reset();
          //std::cout<<"I have been added begin"<<cellId<<endl;
	  this->CheckCellsCareful->InsertNextId(cellId);
	  this->FindBoundaryRegionTipToe(reg);
	}
      }

      //Swap the current check list to the full check list and continue
      tmp = this->CheckCells;
      this->CheckCells = this->CheckCells2;
      this->CheckCells2 = tmp;
      tmp->Reset();
    }

}

void vtkGetBoundaryFaces::FindBoundaryRegionTipToe(int reg)
{
  //std::cout<<"Am tip toeing"<<endl;
    //Variables used in function
    int i;
    vtkIdType j,k,l; 
    vtkIdType *pts = 0;
    vtkIdType npts = 0;
    vtkIdType cellId;
    vtkIdType numNei, nei, p1, p2, nIds, neiId;

    //Id List to store neighbor cells for each set of nodes and a cell
    vtkSmartPointer<vtkIdList> tmp = vtkSmartPointer<vtkIdList>::New();
    vtkSmartPointer<vtkIdList> neiIds = vtkSmartPointer<vtkIdList>::New();

    //Variable for accessing neiIds list
    vtkIdType sz = 0;
    
    //Variables for the boundary cells adjacent to the boundary point
    vtkSmartPointer<vtkIdList> bLinesOne = vtkSmartPointer<vtkIdList>::New();
    vtkSmartPointer<vtkIdList> bLinesTwo = vtkSmartPointer<vtkIdList>::New();

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
	this->mesh->GetCellPoints(cellId,npts,pts);
	if (this->checkedcarefully[cellId] == 0)
	{
          //Update this cell to have been checked carefully and assign it 
	  //with the fillnumber scalar
          this->newScalars->InsertValue(cellId,reg);
	  this->checkedcarefully[cellId] = 1;
	  //For each edge of the cell
	  //std::cout<<"Checking edges of cell "<<cellId<<endl;
          for (i=0; i < npts; i++)
          {
	    p1 = pts[i];
	    p2 = pts[(i+1)%(npts)];

            vtkSmartPointer<vtkIdList> neighbors = 
	      vtkSmartPointer<vtkIdList>::New();
	    //Initial check to make sure the cell is in fact a face cell
	    this->mesh->GetCellEdgeNeighbors(cellId,p1,p2,neighbors);
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
	      {
		neiIds->InsertNextId(nei);
	      }
	      //if cell is on boundary, check to make sure it isn't 
	      //false positive; don't add to check list. This is done by
	      //getting the boundary lines attached to each point, then 
	      //intersecting the two lists. If the result is zero, then this
	      //is a false positive
	      else
	      {
                this->boundaryLines->BuildLinks();
		vtkIdType bPt1 = pointMapper[p1];
		this->boundaryLines->GetPointCells(bPt1,bLinesOne);

		vtkIdType bPt2 = pointMapper[p2];
		this->boundaryLines->GetPointCells(bPt2,bLinesTwo);

		bLinesOne->IntersectWith(bLinesTwo);
		//Cell is false positive. Add to check list. 
		if (bLinesOne->GetNumberOfIds() == 0)
		{
	          //std::cout<<"False positive! "<<nei<<endl;
		  neiIds->InsertNextId(nei);
		}
		//else
		  //std::cout<<"I have not been added because false"<<endl;
	      }
	    }
	    else
	    {
	      //cout<<"NumNei is not 1"<<endl;
	      //cout<<"Number of Neighbors "<<numNei<<endl;
	      //cout<<"Cell is "<<cellId<<endl;
	      for (k=0;k<numNei;k++)
	      {
	        //cout<<"Id!!! "<<neighbors->GetId(k)<<endl;
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


void vtkGetBoundaryFaces::SetBoundaryArrays()
{
  //Variables used in the function
  double pt[3];
  vtkIdType pointId,bp,bp2,i;
  vtkSmartPointer<vtkIdList> bpCellIds = 
    vtkSmartPointer<vtkIdList>::New();
  //Point locator to find points on mesh that are the points on the boundary
  //lines
  vtkSmartPointer<vtkPointLocator> pointLocator = 
    vtkSmartPointer<vtkPointLocator>::New();
  pointLocator->SetDataSet(this->mesh);
  pointLocator->BuildLocator();

  int numMeshPoints = this->mesh->GetNumberOfPoints();
  int numMeshCells = this->mesh->GetNumberOfCells();
  this->checked = new vtkIdType[numMeshCells];
  this->checkedcarefully = new vtkIdType[numMeshCells];
  this->pointMapper = new vtkIdType[numMeshCells];
  this->BoundaryPointArray->SetNumberOfTuples(numMeshPoints);
  this->BoundaryCellArray->SetNumberOfTuples(numMeshCells);
  
  for (int i =0;i<numMeshPoints;i++)
  {
    this->BoundaryPointArray->InsertValue(i,0);
  }
  for (int i =0;i<numMeshCells;i++)
  {
    this->checked[i] = 0;
    this->BoundaryCellArray->InsertValue(i,0);
  }

  int numPoints = this->boundaryLines->GetNumberOfPoints();

  for (pointId = 0;pointId < numPoints;pointId++)
  {
    this->boundaryLines->GetPoint(pointId,pt);
    //Find point on mesh 
    bp = pointLocator->FindClosestPoint(pt);
    this->pointMapper[bp] = pointId;
    this->BoundaryPointArray->InsertValue(bp,1);
    this->mesh->GetPointCells(bp,bpCellIds);
    //Set the point mapping array
    //Assign each cell attached to this point as a boundary cell
    for (i = 0;i < bpCellIds->GetNumberOfIds();i++)
    { 
      this->BoundaryCellArray->InsertValue(bpCellIds->GetId(i),1);
      this->checked[bpCellIds->GetId(i)] = 1;
    }
  }

  for (int i=0;i < numMeshCells;i++)
  {
    if (this->checked[i] == 0)
      this->checkedcarefully[i] = 1;
    else
      this->checkedcarefully[i] = 0;
  }
}
