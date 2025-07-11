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

#include "vtkSVLocalSmoothPolyDataFilter.h"

#include "vtkCellArray.h"
#include "vtkCellData.h"
#include "vtkCellLocator.h"
#include "vtkErrorCode.h"
#include "vtkExecutive.h"
#include "vtkFloatArray.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkMath.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPolyData.h"
#include "vtkPolygon.h"
#include "vtkSmartPointer.h"
#include "vtkTriangleFilter.h"

#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"

vtkStandardNewMacro(vtkSVLocalSmoothPolyDataFilter);

// The following code defines a helper class for performing mesh smoothing
// across the surface of another mesh.
typedef struct _vtkSmoothPoint { //; prevent man page generation
    vtkIdType     cellId;  // cell
    int     subId;   // cell sub id
    double   p[3];    // parametric coords in cell
} vtkSmoothPoint;

class vtkSVLocalSmoothPoints { //;prevent man page generation
public:
  vtkSVLocalSmoothPoints();
  ~vtkSVLocalSmoothPoints()
    {
    delete [] this->Array;
    };
  vtkIdType GetNumberOfPoints() {return this->MaxId + 1;};
  vtkSmoothPoint *GetSmoothPoint(vtkIdType i) {return this->Array + i;};
  vtkSmoothPoint *InsertSmoothPoint(vtkIdType ptId)
    {
    if ( ptId >= this->Size )
      {
      this->Resize(ptId+1);
      }
    if ( ptId > this->MaxId )
      {
      this->MaxId = ptId;
      }
    return this->Array + ptId;
    }
  vtkSmoothPoint *Resize(vtkIdType sz); //reallocates data
  void Reset() {this->MaxId = -1;};

  vtkSmoothPoint *Array;  // pointer to data
  vtkIdType MaxId;              // maximum index inserted thus far
  vtkIdType Size;               // allocated size of data
  vtkIdType Extend;             // grow array by this amount
};

vtkSVLocalSmoothPoints::vtkSVLocalSmoothPoints()
{
  this->MaxId = -1;
  this->Array = new vtkSmoothPoint[1000];
  this->Size = 1000;
  this->Extend = 5000;
}

vtkSmoothPoint *vtkSVLocalSmoothPoints::Resize(vtkIdType sz)
{
  vtkSmoothPoint *newArray;
  vtkIdType newSize;

  if (sz >= this->Size)
    {
    newSize = this->Size +
      this->Extend*(((sz-this->Size)/this->Extend)+1);
    }
  else
    {
    newSize = sz;
    }

  newArray = new vtkSmoothPoint[newSize];

  memcpy(newArray, this->Array,
         (sz < this->Size ? sz : this->Size) * sizeof(vtkSmoothPoint));

  this->Size = newSize;
  delete [] this->Array;
  this->Array = newArray;

  return this->Array;
}

// The following code defines methods for the vtkSVLocalSmoothPolyDataFilter class
//

// Construct object with number of iterations 20; relaxation factor .01;
// feature edge smoothing turned off; feature
// angle 45 degrees; edge angle 15 degrees; and boundary smoothing turned
// on. Error scalars and vectors are not generated (by default). The
// convergence criterion is 0.0 of the bounding box diagonal.
vtkSVLocalSmoothPolyDataFilter::vtkSVLocalSmoothPolyDataFilter()
{
  this->Convergence = 0.0; //goes to number of specied iterations
  this->NumberOfIterations = 20;

  this->RelaxationFactor = .01;

  this->FeatureAngle = 45.0;
  this->EdgeAngle = 15.0;
  this->FeatureEdgeSmoothing = 0;
  this->BoundarySmoothing = 1;

  this->GenerateErrorScalars = 0;
  this->GenerateErrorVectors = 0;

  this->OutputPointsPrecision = vtkAlgorithm::DEFAULT_PRECISION;

  // optional second input
  this->SetNumberOfInputPorts(2);

  this->SmoothCellArrayName  = nullptr;
  this->SmoothPointArrayName = nullptr;
  this->ConstrainArrayName   = nullptr;
  this->UseCellArray = 0;
  this->UsePointArray = 0;

  this->ConstrainAllPoints = 1;
}

vtkSVLocalSmoothPolyDataFilter::~vtkSVLocalSmoothPolyDataFilter()
{
  if (this->SmoothCellArrayName != nullptr)
  {
    delete [] this->SmoothCellArrayName;
    this->SmoothCellArrayName = nullptr;
  }
  if (this->SmoothPointArrayName != nullptr)
  {
    delete [] this->SmoothPointArrayName;
    this->SmoothPointArrayName = nullptr;
  }
  if (this->ConstrainArrayName != nullptr)
  {
    delete [] this->ConstrainArrayName;
    this->ConstrainArrayName = nullptr;
  }
}

void vtkSVLocalSmoothPolyDataFilter::SetSourceData(vtkPolyData *source)
{
  this->SetInputData(1, source);
}

vtkPolyData *vtkSVLocalSmoothPolyDataFilter::GetSource()
{
  if (this->GetNumberOfInputConnections(1) < 1)
    {
    return nullptr;
    }
  return vtkPolyData::SafeDownCast(
    this->GetExecutive()->GetInputData(1, 0));
}

#define VTK_SIMPLE_VERTEX 0
#define VTK_FIXED_VERTEX 1
#define VTK_FEATURE_EDGE_VERTEX 2
#define VTK_BOUNDARY_EDGE_VERTEX 3

// Special structure for marking vertices
typedef struct _vtkMeshVertex
  {
  char      type;
  int      constrain;
  vtkIdList *edges; // connected edges (list of connected point ids)
} vtkMeshVertex, *vtkMeshVertexPtr;

int vtkSVLocalSmoothPolyDataFilter::RequestData(
  vtkInformation *vtkNotUsed(request),
  vtkInformationVector **inputVector,
  vtkInformationVector *outputVector)
{
  // get the info objects
  vtkInformation *inInfo = inputVector[0]->GetInformationObject(0);
  vtkInformation *sourceInfo = inputVector[1]->GetInformationObject(0);
  vtkInformation *outInfo = outputVector->GetInformationObject(0);

  // get the input and output
  vtkPolyData *input = vtkPolyData::SafeDownCast(
    inInfo->Get(vtkDataObject::DATA_OBJECT()));
  vtkPolyData *source = 0;
  if (sourceInfo)
    {
    source = vtkPolyData::SafeDownCast(
      sourceInfo->Get(vtkDataObject::DATA_OBJECT()));
    }
  vtkPolyData *output = vtkPolyData::SafeDownCast(
    outInfo->Get(vtkDataObject::DATA_OBJECT()));

  vtkIdType numPts, numCells, i, numPolys, numStrips;
  int j, k;
  vtkIdType npts = 0;
  const vtkIdType *pts;
  vtkIdType p1, p2;
  double x[3], y[3], deltaX[3], xNew[3], conv, maxDist, dist, factor;
  double x1[3], x2[3], x3[3], l1[3], l2[3];
  double CosFeatureAngle; //Cosine of angle between adjacent polys
  double CosEdgeAngle; // Cosine of angle between adjacent edges
  double closestPt[3], dist2, *w = nullptr;
  int iterationNumber;
  vtkIdType numSimple=0, numBEdges=0, numFixed=0, numFEdges=0;
  vtkPolyData *inMesh, *Mesh;
  vtkPoints *inPts;
  vtkTriangleFilter *toTris=nullptr;
  vtkCellArray *inVerts, *inLines, *inPolys, *inStrips;
  vtkPoints *newPts;
  vtkMeshVertexPtr Verts;
  vtkCellLocator *cellLocator=nullptr;

  // Check input
  //
  numPts=input->GetNumberOfPoints();
  numCells=input->GetNumberOfCells();
  if (numPts < 1 || numCells < 1)
    {
    vtkErrorMacro(<<"No data to smooth!");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
    }
  if (this->UsePointArray)
  {
    if (this->SmoothPointArrayName == nullptr)
    {
      std::cout<<"No PointArrayName given." << endl;
      this->SetErrorCode(vtkErrorCode::UserError + 1);
      return SV_ERROR;
    }
    if (this->GetSmoothArrays(input,0) != 1)
    {
      vtkErrorMacro("Need point array on mesh to be able to local smooth");
      this->SetErrorCode(vtkErrorCode::UserError + 1);
      return SV_ERROR;
    }
  }
  if (this->UseCellArray)
  {
    if (this->SmoothCellArrayName == nullptr)
    {
      std::cout<<"No PointArrayName given." << endl;
      this->SetErrorCode(vtkErrorCode::UserError + 1);
      return SV_ERROR;
    }
    if (this->GetSmoothArrays(input,1) != 1)
    {
      vtkErrorMacro("Need cell array on mesh to be able to local smooth");
      this->SetErrorCode(vtkErrorCode::UserError + 1);
      return SV_ERROR;
    }
  }
  if (this->ConstrainAllPoints != 1)
  {
    if (this->GetSmoothArrays(input,1) != 1)
    {
      vtkErrorMacro("Need cell array to constrain points to mesh");
      this->SetErrorCode(vtkErrorCode::UserError + 1);
      return SV_ERROR;
    }
  }

  CosFeatureAngle = cos( vtkMath::RadiansFromDegrees( this->FeatureAngle) );
  CosEdgeAngle =    cos( vtkMath::RadiansFromDegrees( this->EdgeAngle) );

  vtkDebugMacro(<<"Smoothing " << numPts << " vertices, " << numCells
               << " cells with:\n"
               << "\tConvergence= " << this->Convergence << "\n"
               << "\tIterations= " << this->NumberOfIterations << "\n"
               << "\tRelaxation Factor= " << this->RelaxationFactor << "\n"
               << "\tEdge Angle= " << this->EdgeAngle << "\n"
               << "\tBoundary Smoothing " << (this->BoundarySmoothing ? "On\n" : "Off\n")
               << "\tFeature Edge Smoothing " << (this->FeatureEdgeSmoothing ? "On\n" : "Off\n")
               << "\tError Scalars " << (this->GenerateErrorScalars ? "On\n" : "Off\n")
               << "\tError Vectors " << (this->GenerateErrorVectors ? "On\n" : "Off\n"));

  if ( this->NumberOfIterations <= 0 || this->RelaxationFactor == 0.0)
    { //don't do anything! pass data through
    output->CopyStructure(input);
    output->GetPointData()->PassData(input->GetPointData());
    output->GetCellData()->PassData(input->GetCellData());
    return SV_OK;
    }

  // Peform topological analysis. What we're gonna do is build a connectivity
  // array of connected vertices. The outcome will be one of three
  // classifications for a vertex: VTK_SIMPLE_VERTEX, VTK_FIXED_VERTEX. or
  // VTK_EDGE_VERTEX. Simple vertices are smoothed using all connected
  // vertices. FIXED vertices are never smoothed. Edge vertices are smoothed
  // using a subset of the attached vertices.
  //
  input->BuildLinks();
  int *fixedPoint = new int[numPts];
  for (int i=0;i<numPts;i++)
    fixedPoint[i] = 0;
  this->SetFixedPoints(input,fixedPoint);
  vtkDebugMacro(<<"Analyzing topology...");
  Verts = new vtkMeshVertex[numPts];
  for (i=0; i<numPts; i++)
    {
    Verts[i].type = VTK_SIMPLE_VERTEX; //can smooth
    Verts[i].constrain = 1;
    Verts[i].edges = nullptr;
    if (fixedPoint[i])
      Verts[i].type = VTK_FIXED_VERTEX;
    }

  inPts = input->GetPoints();
  conv = this->Convergence * input->GetLength();

  // check vertices first. Vertices are never smoothed_--------------
  for (inVerts=input->GetVerts(), inVerts->InitTraversal();
  inVerts->GetNextCell(npts,pts); )
    {
    for (j=0; j<npts; j++)
      {
      Verts[pts[j]].type = VTK_FIXED_VERTEX;
      }
    }
  this->UpdateProgress(0.10);

  // now check lines. Only manifold lines can be smoothed------------
  for (inLines=input->GetLines(), inLines->InitTraversal();
  inLines->GetNextCell(npts,pts); )
    {
    for (j=0; j<npts; j++)
      {
      if ( Verts[pts[j]].type == VTK_SIMPLE_VERTEX )
        {
        if ( j == (npts-1) ) //end-of-line marked FIXED
          {
          Verts[pts[j]].type = VTK_FIXED_VERTEX;
          }
        else if ( j == 0 ) //beginning-of-line marked FIXED
          {
          Verts[pts[0]].type = VTK_FIXED_VERTEX;
          inPts->GetPoint(pts[0],x2);
          inPts->GetPoint(pts[1],x3);
          }
        else //is edge vertex (unless already edge vertex!)
          {
          Verts[pts[j]].type = VTK_FEATURE_EDGE_VERTEX;
          Verts[pts[j]].edges = vtkIdList::New();
          Verts[pts[j]].edges->SetNumberOfIds(2);
          Verts[pts[j]].edges->SetId(0,pts[j-1]);
          Verts[pts[j]].edges->SetId(1,pts[j+1]);
          }
        } //if simple vertex

      else if ( Verts[pts[j]].type == VTK_FEATURE_EDGE_VERTEX )
        { //multiply connected, becomes fixed!
        Verts[pts[j]].type = VTK_FIXED_VERTEX;
        Verts[pts[j]].edges->Delete();
        Verts[pts[j]].edges = nullptr;
        }

      } //for all points in this line
    } //for all lines
  this->UpdateProgress(0.25);

  // now polygons and triangle strips-------------------------------
  inPolys=input->GetPolys();
  numPolys = inPolys->GetNumberOfCells();
  inStrips=input->GetStrips();
  numStrips = inStrips->GetNumberOfCells();

  if ( numPolys > 0 || numStrips > 0 )
    { //build cell structure
    vtkCellArray *polys;
    vtkIdType cellId;
    int numNei, nei, edge;
    vtkIdType numNeiPts;
    const vtkIdType *neiPts;
    double normal[3], neiNormal[3];
    vtkIdList *neighbors;

    neighbors = vtkIdList::New();
    neighbors->Allocate(VTK_CELL_SIZE);

    inMesh = vtkPolyData::New();
    inMesh->SetPoints(inPts);
    inMesh->SetPolys(inPolys);
    Mesh = inMesh;

    if ( (numStrips = inStrips->GetNumberOfCells()) > 0 )
      { // convert data to triangles
      inMesh->SetStrips(inStrips);
      toTris = vtkTriangleFilter::New();
      toTris->SetInputData(inMesh);
      toTris->Update();
      Mesh = toTris->GetOutput();
      }

    Mesh->BuildLinks(); //to do neighborhood searching
    polys = Mesh->GetPolys();
    this->UpdateProgress(0.375);

    for (cellId=0, polys->InitTraversal(); polys->GetNextCell(npts,pts);
    cellId++)
      {
      for (i=0; i < npts; i++)
        {
        p1 = pts[i];
        p2 = pts[(i+1)%npts];

        if ( Verts[p1].edges == nullptr )
          {
          Verts[p1].edges = vtkIdList::New();
          Verts[p1].edges->Allocate(16,6);
          }
        if ( Verts[p2].edges == nullptr )
          {
          Verts[p2].edges = vtkIdList::New();
          Verts[p2].edges->Allocate(16,6);
          }

        Mesh->GetCellEdgeNeighbors(cellId,p1,p2,neighbors);
        numNei = neighbors->GetNumberOfIds();

        edge = VTK_SIMPLE_VERTEX;
        if ( numNei == 0 )
          {
          edge = VTK_BOUNDARY_EDGE_VERTEX;
          }

        else if ( numNei >= 2 )
          {
          // check to make sure that this edge hasn't been marked already
          for (j=0; j < numNei; j++)
            {
            if ( neighbors->GetId(j) < cellId )
              {
              break;
              }
            }
          if ( j >= numNei )
            {
            edge = VTK_FEATURE_EDGE_VERTEX;
            }
          }

        else if ( numNei == 1 && (nei=neighbors->GetId(0)) > cellId )
          {
          vtkPolygon::ComputeNormal(inPts,npts,pts,normal);
          Mesh->GetCellPoints(nei,numNeiPts,neiPts);
          vtkPolygon::ComputeNormal(inPts,numNeiPts,neiPts,neiNormal);

          if ( this->FeatureEdgeSmoothing &&
          vtkMath::Dot(normal,neiNormal) <= CosFeatureAngle )
            {
            edge = VTK_FEATURE_EDGE_VERTEX;
            }
          }
        else // a visited edge; skip rest of analysis
          {
          continue;
          }

        if ( edge && Verts[p1].type == VTK_SIMPLE_VERTEX )
          {
          Verts[p1].edges->Reset();
          Verts[p1].edges->InsertNextId(p2);
          Verts[p1].type = edge;
          }
        else if ( (edge && Verts[p1].type == VTK_BOUNDARY_EDGE_VERTEX) ||
        (edge && Verts[p1].type == VTK_FEATURE_EDGE_VERTEX) ||
        (!edge && Verts[p1].type == VTK_SIMPLE_VERTEX ) )
          {
          Verts[p1].edges->InsertNextId(p2);
          if ( Verts[p1].type && edge == VTK_BOUNDARY_EDGE_VERTEX )
            {
            Verts[p1].type = VTK_BOUNDARY_EDGE_VERTEX;
            }
          }

        if ( edge && Verts[p2].type == VTK_SIMPLE_VERTEX )
          {
          Verts[p2].edges->Reset();
          Verts[p2].edges->InsertNextId(p1);
          Verts[p2].type = edge;
          }
        else if ( (edge && Verts[p2].type == VTK_BOUNDARY_EDGE_VERTEX ) ||
        (edge && Verts[p2].type == VTK_FEATURE_EDGE_VERTEX) ||
        (!edge && Verts[p2].type == VTK_SIMPLE_VERTEX ) )
          {
          Verts[p2].edges->InsertNextId(p1);
          if ( Verts[p2].type && edge == VTK_BOUNDARY_EDGE_VERTEX )
            {
            Verts[p2].type = VTK_BOUNDARY_EDGE_VERTEX;
            }
          }
        }
      }

    inMesh->Delete();
    if (toTris) {toTris->Delete();}

    neighbors->Delete();
    }//if strips or polys

  this->UpdateProgress(0.50);

  //post-process edge vertices to make sure we can smooth them
  for (i=0; i<numPts; i++)
    {
    if ( Verts[i].type == VTK_SIMPLE_VERTEX )
      {
      numSimple++;
      }

    else if ( Verts[i].type == VTK_FIXED_VERTEX )
      {
      numFixed++;
      }

    else if ( Verts[i].type == VTK_FEATURE_EDGE_VERTEX ||
    Verts[i].type == VTK_BOUNDARY_EDGE_VERTEX )
      { //see how many edges; if two, what the angle is

      if ( !this->BoundarySmoothing &&
      Verts[i].type == VTK_BOUNDARY_EDGE_VERTEX )
        {
        Verts[i].type = VTK_FIXED_VERTEX;
        numBEdges++;
        }

      else if ( (npts = Verts[i].edges->GetNumberOfIds()) != 2 )
        {
        Verts[i].type = VTK_FIXED_VERTEX;
        numFixed++;
        }

      else //check angle between edges
        {
        inPts->GetPoint(Verts[i].edges->GetId(0),x1);
        inPts->GetPoint(i,x2);
        inPts->GetPoint(Verts[i].edges->GetId(1),x3);

        for (k=0; k<3; k++)
          {
          l1[k] = x2[k] - x1[k];
          l2[k] = x3[k] - x2[k];
          }
        if ( vtkMath::Normalize(l1) >= 0.0 &&
             vtkMath::Normalize(l2) >= 0.0 &&
             vtkMath::Dot(l1,l2) < CosEdgeAngle)
          {
          numFixed++;
          Verts[i].type = VTK_FIXED_VERTEX;
          }
        else
          {
          if ( Verts[i].type == VTK_FEATURE_EDGE_VERTEX )
            {
            numFEdges++;
            }
          else
            {
            numBEdges++;
            }
          }
        }//if along edge
      }//if edge vertex
    }//for all points

  vtkDebugMacro(<<"Found\n\t" << numSimple << " simple vertices\n\t"
                << numFEdges << " feature edge vertices\n\t"
                << numBEdges << " boundary edge vertices\n\t"
                << numFixed << " fixed vertices\n\t");

  vtkDebugMacro(<<"Beginning smoothing iterations...");

  // We've setup the topology...now perform Laplacian smoothing
  //
  newPts = vtkPoints::New();

  // Set the desired precision for the points in the output.
  if(this->OutputPointsPrecision == vtkAlgorithm::DEFAULT_PRECISION)
    {
    newPts->SetDataType(inPts->GetDataType());
    }
  else if(this->OutputPointsPrecision == vtkAlgorithm::SINGLE_PRECISION)
    {
    newPts->SetDataType(VTK_FLOAT);
    }
  else if(this->OutputPointsPrecision == vtkAlgorithm::DOUBLE_PRECISION)
    {
    newPts->SetDataType(VTK_DOUBLE);
    }

  newPts->SetNumberOfPoints(numPts);

  // If Source defined, we do constrained smoothing (that is, points are
  // constrained to the surface of the mesh object).
  if ( source )
    {
      this->SmoothPoints = new vtkSVLocalSmoothPoints;
      vtkSmoothPoint *sPtr;
      cellLocator = vtkCellLocator::New();
      w = new double[source->GetMaxCellSize()];

      cellLocator->SetDataSet(source);
      cellLocator->BuildLocator();

      for (i=0; i < numPts; i++)
	{
	sPtr = this->SmoothPoints->InsertSmoothPoint(i);
	cellLocator->FindClosestPoint(inPts->GetPoint(i), closestPt,
				      sPtr->cellId, sPtr->subId, dist2);
	newPts->SetPoint(i, closestPt);
	}
    }
  else //smooth normally
    {
    for (i=0; i<numPts; i++) //initialize to old coordinates
      {
      newPts->SetPoint(i,inPts->GetPoint(i));
      }
    }

  factor = this->RelaxationFactor;
  for ( maxDist=VTK_DOUBLE_MAX, iterationNumber=0;
  maxDist > conv && iterationNumber < this->NumberOfIterations;
  iterationNumber++ )
    {

    if ( iterationNumber && !(iterationNumber % 5) )
      {
      this->UpdateProgress (0.5 + 0.5*iterationNumber/this->NumberOfIterations);
      if (this->GetAbortExecute())
        {
        break;
        }
      }

    maxDist=0.0;
    for (i=0; i<numPts; i++)
      {
      if ( Verts[i].type != VTK_FIXED_VERTEX && Verts[i].edges != nullptr &&
      (npts = Verts[i].edges->GetNumberOfIds()) > 0 )
        {
        newPts->GetPoint(i, x); //use current points
        deltaX[0] = deltaX[1] = deltaX[2] = 0.0;
        for (j=0; j<npts; j++)
          {
          newPts->GetPoint(Verts[i].edges->GetId(j), y);
          for (k=0; k<3; k++)
            {
            deltaX[k] += (y[k] - x[k]) / npts;
            }
          }//for all connected points

        for (k=0;k<3;k++)
          {
          xNew[k] = x[k] + factor * deltaX[k];
          }

        // Constrain point to surface
        if ( source )
          {
            int movepoint = 0;
	    if (this->ConstrainAllPoints == 0)
	    {
	      if (Verts[i].constrain == 0)
	        movepoint = 1;
	    }
	    else
	      movepoint = 1;

            if (movepoint)
	    {
	    vtkSmoothPoint *sPtr = this->SmoothPoints->GetSmoothPoint(i);
	    vtkCell *cell=nullptr;

	    if ( sPtr->cellId >= 0 ) //in cell
	      {
	      cell = source->GetCell(sPtr->cellId);
	      }

	    if ( !cell || cell->EvaluatePosition(xNew, closestPt,
	    sPtr->subId, sPtr->p, dist2, w) == 0)
	      { // not in cell anymore
	      cellLocator->FindClosestPoint(xNew, closestPt, sPtr->cellId,
					    sPtr->subId, dist2);
	      }
	    for (k=0; k<3; k++)
	      {
	      xNew[k] = closestPt[k];
	      }
	    }
          }

        newPts->SetPoint(i,xNew);
        if ( (dist = vtkMath::Norm(deltaX)) > maxDist )
          {
          maxDist = dist;
          }
        }//if can move point
      }//for all points
    } //for not converged or within iteration count

  vtkDebugMacro(<<"Performed " << iterationNumber << " smoothing passes");
  if ( source )
    {
    cellLocator->Delete();
    delete this->SmoothPoints;
    delete [] w;
    }

  // Update output. Only point coordinates have changed.
  //
  output->GetPointData()->PassData(input->GetPointData());
  output->GetCellData()->PassData(input->GetCellData());

  if ( this->GenerateErrorScalars )
    {
    vtkFloatArray *newScalars = vtkFloatArray::New();
    newScalars->SetNumberOfTuples(numPts);
    for (i=0; i<numPts; i++)
      {
      inPts->GetPoint(i,x1);
      newPts->GetPoint(i,x2);
      newScalars->SetComponent(i,0,
                               sqrt(vtkMath::Distance2BetweenPoints(x1,x2)));
      }
    int idx = output->GetPointData()->AddArray(newScalars);
    output->GetPointData()->SetActiveAttribute(idx, vtkDataSetAttributes::SCALARS);
    newScalars->Delete();
    }

  if ( this->GenerateErrorVectors )
    {
    vtkFloatArray *newVectors = vtkFloatArray::New();
    newVectors->SetNumberOfComponents(3);
    newVectors->SetNumberOfTuples(numPts);
    for (i=0; i<numPts; i++)
      {
      inPts->GetPoint(i,x1);
      newPts->GetPoint(i,x2);
      for (j=0; j<3; j++)
        {
        x3[j] = x2[j] - x1[j];
        }
      newVectors->SetTuple(i,x3);
      }
    output->GetPointData()->SetVectors(newVectors);
    newVectors->Delete();
    }

  output->SetPoints(newPts);
  newPts->Delete();

  output->SetVerts(input->GetVerts());
  output->SetLines(input->GetLines());
  output->SetPolys(input->GetPolys());
  output->SetStrips(input->GetStrips());

  //free up connectivity storage
  for (i=0; i<numPts; i++)
    {
    if ( Verts[i].edges != nullptr )
      {
      Verts[i].edges->Delete();
      Verts[i].edges = nullptr;
      }
    }
  delete [] Verts;

  return SV_OK;
}

int vtkSVLocalSmoothPolyDataFilter::FillInputPortInformation(int port,
                                                      vtkInformation *info)
{
  if (!this->Superclass::FillInputPortInformation(port, info))
    {
    return SV_ERROR;
    }

  if (port == 1)
    {
    info->Set(vtkAlgorithm::INPUT_IS_OPTIONAL(), 1);
    }
  return SV_OK;
}

void vtkSVLocalSmoothPolyDataFilter::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);

  os << indent << "Convergence: " << this->Convergence << "\n";
  os << indent << "Number of Iterations: " << this->NumberOfIterations << "\n";
  os << indent << "Relaxation Factor: " << this->RelaxationFactor << "\n";
  os << indent << "Feature Edge Smoothing: " << (this->FeatureEdgeSmoothing ? "On\n" : "Off\n");
  os << indent << "Feature Angle: " << this->FeatureAngle << "\n";
  os << indent << "Edge Angle: " << this->EdgeAngle << "\n";
  os << indent << "Boundary Smoothing: " << (this->BoundarySmoothing ? "On\n" : "Off\n");
  os << indent << "Generate Error Scalars: " << (this->GenerateErrorScalars ? "On\n" : "Off\n");
  os << indent << "Generate Error Vectors: " << (this->GenerateErrorVectors ? "On\n" : "Off\n");
  if ( this->GetSource() )
    {
      os << indent << "Source: " << static_cast<void *>(this->GetSource()) << "\n";
    }
  else
    {
    os << indent << "Source (none)\n";
    }

  os << indent << "Output Points Precision: " << this->OutputPointsPrecision << "\n";
}

int vtkSVLocalSmoothPolyDataFilter::GetSmoothArrays(vtkPolyData *object, int type)
{
  vtkIdType i;
  int numArrays;

  // Set array name
  std::string arrayName;
  if (type == 0)
    arrayName = this->SmoothPointArrayName;
  else
    arrayName = this->SmoothCellArrayName;

  // Check if array exists
  int exists = vtkSVGeneralUtils::CheckArrayExists(object, type, arrayName);

  if (exists)
  {
    if (type == 0)
    {
	    //std::cout<<"Point Array exists!"<<endl;
      this->SmoothPointArray = vtkIntArray::SafeDownCast(
	  object->GetPointData()->GetArray(this->SmoothPointArrayName));
    }
    else
    {
	    //std::cout<<"Cell Array exists!"<<endl;
      this->SmoothCellArray = vtkIntArray::SafeDownCast(
	  object->GetCellData()->GetArray(this->SmoothCellArrayName));
    }

  }

  return exists;
}

int vtkSVLocalSmoothPolyDataFilter::SetFixedPoints(vtkPolyData *pd,int *fixedPoint)
{
  int numPts = pd->GetNumberOfPoints();
  int numCells = pd->GetNumberOfCells();
  vtkIdType p1,p2;

  if (this->UsePointArray)
  {
    for (vtkIdType pointId = 0;pointId < numPts;pointId++)
    {
      if (this->SmoothPointArray->GetValue(pointId) != 1)
        fixedPoint[pointId] = 1;
    }
  }
  if (this->UseCellArray)
  {
    vtkIdType npts;
    const vtkIdType *pts;
    for (vtkIdType cellId = 0;cellId < numCells; cellId ++)
    {
      pd->GetCellPoints(cellId,npts,pts);
      for (int i=0; i < npts;i++)
      {
        p1 = pts[i];
        p2 = pts[(i+1)%(npts)];
        vtkNew(vtkIdList, neighbors);
        pd->GetCellEdgeNeighbors(cellId,p1,p2,neighbors);
        vtkIdType numNei = neighbors->GetNumberOfIds();
        if (numNei > 0)
        {
          vtkIdType neighCell = neighbors->GetId(0);
          if (this->SmoothCellArray->GetValue(neighCell) != 1)
          {
            fixedPoint[p1] = 1;
            fixedPoint[p2] = 1;
          }
        }
      }
    }
  }

  return SV_OK;
}
