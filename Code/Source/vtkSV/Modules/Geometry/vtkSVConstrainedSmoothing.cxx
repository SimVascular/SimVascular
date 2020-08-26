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

/** @file vtkSVConstrainedSmoothing.cxx
 *  @brief This implements the vtkSVConstrainedSmoothing filter
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 */

#include "vtkSVConstrainedSmoothing.h"

#include "vtkCellArray.h"
#include "vtkCellData.h"
#include "vtkDoubleArray.h"
#include "vtkErrorCode.h"
#include "vtkFloatArray.h"
#include "vtkGenericCell.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkIntArray.h"
#include "vtkMath.h"
#include "vtkObjectFactory.h"
#include "vtkPolyData.h"
#include "vtkPolyDataNormals.h"
#include "vtkPointData.h"
#include "vtkSmartPointer.h"

#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"
#include "vtkSVMathUtils.h"
#include "vtkSVSparseMatrix.h"

#include <iostream>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVConstrainedSmoothing);

// ----------------------
// Constructor
// ----------------------
vtkSVConstrainedSmoothing::vtkSVConstrainedSmoothing()
{
    this->CellArrayName  = NULL;
    this->PointArrayName = NULL;

    this->UsePointArray = 0;
    this->UseCellArray  = 0;

    this->Weight = 0.0;
    this->NumSmoothOperations = 5;
    this->NumGradientSolves = 20;

    this->fixedPt = NULL;
    this->NumFixedPoints = 0;
}

// ----------------------
// Destructor
// ----------------------
vtkSVConstrainedSmoothing::~vtkSVConstrainedSmoothing()
{
  if (this->CellArrayName != NULL)
  {
    delete [] this->CellArrayName;
    this->CellArrayName = NULL;
  }
  if (this->PointArrayName != NULL)
  {
    delete [] this->PointArrayName;
    this->PointArrayName = NULL;
  }
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVConstrainedSmoothing::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);

  if (this->CellArrayName != NULL)
    os << indent << "Cell array name: " << this->CellArrayName << "\n";
  if (this->PointArrayName != NULL)
    os << indent << "Point array name: " << this->PointArrayName << "\n";

  os << indent << "Weight: " << this->Weight << "\n";

  os << indent << "Use point array: " << this->UsePointArray << "\n";
  os << indent << "Use cell array: " << this->UseCellArray << "\n";

  os << indent << "Number of smooth operations: " << this->NumSmoothOperations << "\n";
  os << indent << "Number of conjugate gradient iterations: " << this->NumGradientSolves << "\n";
}

// ----------------------
// RequestData
// ----------------------
int vtkSVConstrainedSmoothing::RequestData(vtkInformation *vtkNotUsed(request),
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
      vtkErrorMacro("No input!");
      this->SetErrorCode(vtkErrorCode::UserError + 1);
      return SV_OK;
    }

    if (this->UsePointArray)
    {
      if (this->PointArrayName == NULL)
      {
        vtkErrorMacro("No PointArrayName given.");
        this->SetErrorCode(vtkErrorCode::UserError + 1);
        return SV_ERROR;
      }
      if (this->GetArrays(input,0) != 1)
      {
        std::cout<<"No Point Array Named "<<this->PointArrayName<<" on surface"<<endl;
        this->SetErrorCode(vtkErrorCode::UserError + 1);
        return SV_ERROR;
      }
    }
    if (this->UseCellArray)
    {
      if (this->CellArrayName == NULL)
      {
        std::cout<<"No CellArrayName given." << endl;
        this->SetErrorCode(vtkErrorCode::UserError + 1);
        return SV_ERROR;
      }
      if (this->GetArrays(input,1) != 1)
      {
        std::cout<<"No Cell Array Named "<<this->CellArrayName<<" on surface"<<endl;
        this->SetErrorCode(vtkErrorCode::UserError + 1);
        return SV_ERROR;
      }
    }

    input->BuildLinks();
    this->SetFixedPoints(input);
    vtkNew(vtkPolyData, tmp);
    tmp->DeepCopy(input);
    for (int i=0;i<this->NumSmoothOperations;i++)
      this->ConstainedSmooth(input,tmp);

    delete [] this->fixedPt;
    output->DeepCopy(tmp);
    return SV_OK;
}

// ----------------------
// GetArrays
// ----------------------
int vtkSVConstrainedSmoothing::GetArrays(vtkPolyData *object,int type)
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

// ----------------------
// SetFixedPoints
// ----------------------
int vtkSVConstrainedSmoothing::SetFixedPoints(vtkPolyData *pd)
{
  int numPoints = pd->GetNumberOfPoints();
  int numCells = pd->GetNumberOfCells();
  vtkIdType p1,p2;
  this->fixedPt = new int[numPoints];

  for (vtkIdType pointId = 0;pointId < numPoints;pointId++)
  {
    this->fixedPt[pointId] = 0;
  }

  if (this->UsePointArray)
  {
    for (vtkIdType pointId = 0;pointId < numPoints;pointId++)
    {
      if (this->PointArray->GetValue(pointId) != 1)
      {
        this->fixedPt[pointId] = 1;
        this->NumFixedPoints++;
      }
    }
  }

  if (this->UseCellArray)
  {
    vtkIdType npts,*pts;
    for (vtkIdType cellId = 0;cellId < numCells;cellId++)
    {
      pd->GetCellPoints(cellId,npts,pts);
      for (int i=0;i<npts;i++)
      {
        p1 = pts[i];
        p2 = pts[(i+1)%(npts)];
        vtkNew(vtkIdList, neighbors);
        pd->GetCellEdgeNeighbors(cellId,p1,p2,neighbors);
        vtkIdType numNei = neighbors->GetNumberOfIds();
        if (numNei > 0)
        {
          vtkIdType neighCell = neighbors->GetId(0);
          if (this->CellArray->GetValue(neighCell) != 1)
          {
            if (this->fixedPt[p1] != 1)
            {
              this->fixedPt[p1] = 1;
              this->NumFixedPoints++;
            }
            if (this->fixedPt[p2] != 1)
            {
              this->fixedPt[p2] = 1;
              this->NumFixedPoints++;
            }
          }
        }
      }
    }
  }

  return SV_OK;
}

// ----------------------
// ConstrainedSmooth
// ----------------------
int vtkSVConstrainedSmoothing::ConstainedSmooth(vtkPolyData *original,vtkPolyData *current)
{
  double pt[3];
  double smoothpt[3];
  double origpt[3];
  int numPoints = current->GetNumberOfPoints();
  vtkNew(vtkPolyDataNormals, normaler);
  normaler->SetInputData(current);
  normaler->SplittingOff();
  normaler->Update();

  vtkFloatArray *normals;
  normals = vtkFloatArray::SafeDownCast(
    normaler->GetOutput()->GetPointData()->GetNormals());
  //std::cout<<"num normals: "<<normals->GetNumberOfTuples()<<endl;
  //std::cout<<"Num points: "<<numPoints<<endl;

  int totalEqs = numPoints*6 - this->NumFixedPoints;
  //Set up spartse matrix for conjugate gradient solve
  vtkNew(vtkSVSparseMatrix, A);
  A->SetMatrixSize(numPoints*6, numPoints*3);
  std::vector<double> b(numPoints*6);
  std::vector<double> x(numPoints*3);

  int subId;
  double distance[3];
  double closestPt[3];
  double normal[3];
  double neighborpt[3];
  double newnormal[3];
  vtkIdType closestCell;
  vtkNew(vtkGenericCell, genericCell);
  current->BuildLinks();
  for (vtkIdType pointId = 0;pointId < numPoints; pointId++)
  {
    current->GetPoint(pointId,pt);
    original->GetPoint(pointId,origpt);
    normals->GetTuple(pointId,normal);
    for (int i=0;i<3;i++)
      distance[i] = origpt[i] - pt[i];
    double direction = vtkMath::Dot(normal, distance);
    double norm = vtkMath::Normalize(normal);
    double dist = sqrt(pow(distance[0],2) +
		                   pow(distance[1],2) +
		                   pow(distance[2],2));
    for (int i=0;i<3;i++)
    {
      double weighting = normal[i];
      if (direction <= 0)
        weighting *= 0.0;

      weighting = weighting*this->Weight;
      weighting = weighting*(dist);

      int x_loc = ((int) pointId)*3 + i;
      A->SetElement(x_loc,x_loc,1);
      b[x_loc] = pt[i]  + weighting;
      x[x_loc] = pt[i];
    }

  }

  for (vtkIdType pointId = 0;pointId < numPoints; pointId++)
  {
    if (!this->fixedPt[pointId])
    {
      for (int i=0;i<3;i++)
      {
        int x_row = numPoints*3 + ((int) pointId)*3 + i;
        int x_column = ((int) pointId)*3 + i;
        A->SetElement(x_row,x_column,1);
        b[x_row] = 0.0;
      }

      std::set<vtkIdType> neighborPts;
      this->GetAttachedPoints(current,pointId,&neighborPts);
      int numNeighborPts = neighborPts.size();
      //std::cout<<"Checking Neighbor Points ";
      std::set<vtkIdType>::iterator it;
      it = neighborPts.begin();
      while (it != neighborPts.end())
      {
        for (int i=0;i<3;i++)
        {
          int x_row = numPoints*3 + ((int) pointId)*3 + i;
          int x_column = ((int) *it)*3 + i;
          double value = -1.0/numNeighborPts;
          A->SetElement(x_row,x_column,value);
        }
        ++it;
      }
    }
  }

  vtkSVMathUtils::ConjugateGradient(A,&b[0],this->NumGradientSolves,&x[0], 1.0e-8);
  //Not necessary, just to check how well satisfied
  //std::vector<double> c(totalEqs);
  //A->MultiplyColumn(&x[0],&c[0]);

  vtkNew(vtkPoints, newPoints);

  double newpt[3];
  for (vtkIdType pointId = 0; pointId < numPoints; pointId++)
  {
    if (this->fixedPt[pointId])
      current->GetPoint(pointId,newpt);
    else
    {
      for (int i=0;i<3;i++)
      {
        int x_loc = 3*((int) pointId) + i;
        newpt[i] = x[x_loc];
      }
    }
    newPoints->InsertPoint(pointId,newpt);
  }
  current->SetPoints(newPoints);

  return SV_OK;
}

// ----------------------
// GetAttachedPoints
// ----------------------
int vtkSVConstrainedSmoothing::GetAttachedPoints(vtkPolyData *pd, vtkIdType nodeId,std::set<vtkIdType> *attachedPts)
{
  vtkIdType npts,*pts;
  vtkNew(vtkIdList, pointCells);
  //Do Before function call!
  pd->GetPointCells(nodeId,pointCells);
  for (int i=0;i<pointCells->GetNumberOfIds();i++)
  {
    vtkIdType cellId = pointCells->GetId(i);
    pd->GetCellPoints(cellId,npts,pts);
    for (int j=0;j<npts;j++)
    {
      vtkIdType pointId = pts[j];
      if (pointId != nodeId)
      {
	attachedPts->insert(pointId);
      }
    }
  }
  return SV_OK;
}
