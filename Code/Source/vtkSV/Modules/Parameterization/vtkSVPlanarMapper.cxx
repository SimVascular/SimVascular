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

#include "vtkSVPlanarMapper.h"

#include "vtkCellData.h"
#include "vtkErrorCode.h"
#include "vtkIdFilter.h"
#include "vtkIdList.h"
#include "vtkMath.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPolyData.h"
#include "vtkPolyDataNormals.h"
#include "vtkSmartPointer.h"
#include "vtkUnstructuredGrid.h"

#include "vtkSVBoundaryMapper.h"
#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"
#include "vtkSVMathUtils.h"

#include <iostream>
#include <sstream>
#include <cmath>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVPlanarMapper);


// ----------------------
// Constructor
// ----------------------
vtkSVPlanarMapper::vtkSVPlanarMapper()
{
  this->RemoveInternalIds = 1;

  this->InitialPd     = vtkPolyData::New();
  this->WorkPd        = vtkPolyData::New();
  this->PlanarPd      = vtkPolyData::New();
  this->EdgeTable     = vtkEdgeTable::New();
  this->EdgeWeights   = vtkFloatArray::New();
  this->EdgeNeighbors = vtkIntArray::New(); this->EdgeNeighbors->SetNumberOfComponents(2);
  this->IsBoundary    = vtkIntArray::New();
  this->Boundaries    = vtkPolyData::New();
  this->BoundaryLoop  = vtkPolyData::New();
  this->ATutte        = vtkSVSparseMatrix::New();
  this->AHarm         = vtkSVSparseMatrix::New();

  this->BoundaryMapper = NULL;

  this->InternalIdsArrayName = NULL;

  this->Lambda = 0.5;
  this->Mu     = 0.5;

  this->Dir0 = 0;
  this->Dir1 = 1;
  this->Dir2 = 2;
}

// ----------------------
// Constructor
// ----------------------
vtkSVPlanarMapper::~vtkSVPlanarMapper()
{
  if (this->InitialPd != NULL)
  {
    this->InitialPd->Delete();
    this->InitialPd = NULL;
  }
  if (this->WorkPd != NULL)
  {
    this->WorkPd->Delete();
    this->WorkPd = NULL;
  }
  if (this->PlanarPd != NULL)
  {
    this->PlanarPd->Delete();
    this->PlanarPd = NULL;
  }
  if (this->EdgeTable != NULL)
  {
    this->EdgeTable->Delete();
    this->EdgeTable = NULL;
  }
  if (this->EdgeWeights != NULL)
  {
    this->EdgeWeights->Delete();
    this->EdgeWeights = NULL;
  }
  if (this->EdgeNeighbors != NULL)
  {
    this->EdgeNeighbors->Delete();
    this->EdgeNeighbors = NULL;
  }
  if (this->IsBoundary != NULL)
  {
    this->IsBoundary->Delete();
    this->IsBoundary = NULL;
  }
  if (this->Boundaries != NULL)
  {
    this->Boundaries->Delete();
    this->Boundaries = NULL;
  }
  if (this->BoundaryLoop != NULL)
  {
    this->BoundaryLoop->Delete();
    this->BoundaryLoop = NULL;
  }

  if (this->InternalIdsArrayName)
  {
    delete [] this->InternalIdsArrayName;
    this->InternalIdsArrayName = NULL;
  }

  if (this->ATutte != NULL)
  {
    this->ATutte->Delete();
    this->ATutte = NULL;
  }
  if (this->AHarm != NULL)
  {
    this->AHarm->Delete();
    this->AHarm = NULL;
  }

  if (this->BoundaryMapper != NULL)
  {
    this->BoundaryMapper->Delete();
    this->BoundaryMapper = NULL;
  }
}

// ----------------------
// Constructor
// ----------------------
void vtkSVPlanarMapper::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);

  if (this->InternalIdsArrayName != NULL)
    os << indent << "Internal Ids array name: " << this->InternalIdsArrayName << "\n";
}

// ----------------------
// RequestData
// ----------------------
int vtkSVPlanarMapper::RequestData(vtkInformation *vtkNotUsed(request),
                                   vtkInformationVector **inputVector,
                                   vtkInformationVector *outputVector)
{
  // get the input and output
  vtkPolyData *input = vtkPolyData::GetData(inputVector[0]);
  vtkPolyData *output = vtkPolyData::GetData(outputVector);

  //Copy the input to operate on
  this->InitialPd->DeepCopy(input);

  //Copy information to the working polydata
  this->WorkPd->DeepCopy(this->InitialPd);
  this->PlanarPd->DeepCopy(this->InitialPd);

  if (this->PrepFilter() != SV_OK)
  {
    vtkErrorMacro("Error when mapping");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }

  if (this->RunFilter() != SV_OK)
  {
    vtkErrorMacro("Error when mapping");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }

  if (this->RemoveInternalIds)
  {
    this->WorkPd->GetPointData()->RemoveArray(this->InternalIdsArrayName);
    this->WorkPd->GetCellData()->RemoveArray(this->InternalIdsArrayName);
  }
  output->DeepCopy(this->PlanarPd);
  return SV_OK;
}

// ----------------------
// PrepFilter
// ----------------------
int vtkSVPlanarMapper::PrepFilter()
{
  // Get number of points and cells
  vtkIdType numPolys = this->InitialPd->GetNumberOfPolys();
  vtkIdType numPoints = this->InitialPd->GetNumberOfPoints();
  //Check the input to make sure it is there
  if (numPolys < 1)
  {
    vtkDebugMacro("No input!");
    return SV_ERROR;
  }

  //Check the input to make sure it is manifold and a triangulated surface
  if (vtkSVGeneralUtils::CheckSurface(this->InitialPd) != SV_OK)
  {
    vtkErrorMacro("Error when checking input surface");
    return SV_ERROR;
  }

  // Check if internal id array name is given
  if (!this->InternalIdsArrayName)
  {
    vtkDebugMacro("Internal Ids Array Name not given, setting to InternalIds");
    this->InternalIdsArrayName = new char[strlen("InternalIds") + 1];
    strcpy(this->InternalIdsArrayName, "InternalIds");
  }
  // Check if array internal ids is already on pd
  if (vtkSVGeneralUtils::CheckArrayExists(this->WorkPd, 0, this->InternalIdsArrayName))
  {
    this->RemoveInternalIds = 0;
  }
  else
  {
    vtkNew(vtkIdFilter, ider);
    ider->SetInputData(this->WorkPd);
    ider->SetIdsArrayName(this->InternalIdsArrayName);
    ider->Update();
    this->WorkPd->DeepCopy(ider->GetOutput());
  }

  //Create the edge table for the input surface
  this->WorkPd->BuildLinks();
  if (!vtkSVGeneralUtils::CreateEdgeTable(this->WorkPd, this->EdgeTable, this->EdgeWeights,
                             this->EdgeNeighbors, this->IsBoundary))
  {
    vtkErrorMacro("Could not create edge table");
    return SV_ERROR;
  }

  // Set the size of the matrices
  this->ATutte->SetMatrixSize(numPoints, numPoints);
  this->AHarm->SetMatrixSize(numPoints, numPoints);
  this->Xu.resize(numPoints, 0.0);
  this->Xv.resize(numPoints, 0.0);
  this->Bu.resize(numPoints, 0.0);
  this->Bv.resize(numPoints, 0.0);

  return SV_OK;
}

// ----------------------
// RunFilter
// ----------------------
int vtkSVPlanarMapper::RunFilter()
{
  // Set boundaries using given boundary mapper
  if (this->SetBoundaries() != SV_OK)
  {
    vtkErrorMacro("Error in mapping");
    return SV_ERROR;
  }

  // Set internal nodes
  if (this->SetInternalNodes() != SV_OK)
  {
    vtkErrorMacro("Error setting internal nodes");
    return SV_ERROR;
  }

  // Solve the system
  if (this->SolveSystem() != SV_OK)
  {
    vtkErrorMacro("Error solving system");
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// SetBoundaries
// ----------------------
int vtkSVPlanarMapper::SetBoundaries()
{
  // Set up the boundary mapper, should already have most data set to
  // it, but need to apply last little bit
  this->BoundaryMapper->SetInputData(this->WorkPd);
  this->BoundaryMapper->SetEdgeTable(this->EdgeTable);
  this->BoundaryMapper->SetInternalIdsArrayName(this->InternalIdsArrayName);
  this->BoundaryMapper->Update();

  // Get the output from the boundary mapper
  vtkNew(vtkPolyData, boundaryPd);
  boundaryPd->DeepCopy(this->BoundaryMapper->GetOutput());
  // Check if array internal ids is already on pd
  if (vtkSVGeneralUtils::CheckArrayExists(boundaryPd, 0, this->InternalIdsArrayName) == 0)
  {
    vtkErrorMacro("No internal ids array name on boundary pd");
    return SV_ERROR;
  }

  // Get the original point ids from boundary
  vtkDataArray *originalIds = boundaryPd->GetPointData()->GetArray(this->InternalIdsArrayName);

  // Loop through points
  int numBoundPts = boundaryPd->GetNumberOfPoints();
  for (int i=0; i<numBoundPts; i++)
  {
    // Get point
    int id = originalIds->GetTuple1(i);
    double pt[3];
    boundaryPd->GetPoint(i, pt);

    // Set diagonal to be 1 for boundary points
    this->AHarm->SetElement(id, id, 1.0);
    this->ATutte->SetElement(id, id, 1.0);

    // Set right hand side to be point on boundary
    this->Bu[id] = pt[this->Dir0];
    this->Bv[id] = pt[this->Dir1];
  }

  return SV_OK;
}

// ----------------------
// SetInternalNodes
// ----------------------
int vtkSVPlanarMapper::SetInternalNodes()
{
  // Get number of points
  int numPoints = this->WorkPd->GetNumberOfPoints();

  vtkNew(vtkPolyData, boundaryPd);
  boundaryPd->DeepCopy(this->BoundaryMapper->GetOutput());
  double centroid[3];
  vtkSVGeneralUtils::GetCentroidOfPoints(boundaryPd->GetPoints(), centroid);

  // Loop through points
  for (int i=0; i<numPoints; i++)
  {
    // If its not on the boundary, process
    if (this->IsBoundary->GetValue(i) == 0)
    {
      // Get the weight of point
      double tot_weight = 0.0;
      double tot_tutte_weight = 0.0;

      // Get points sharing edge with point
      vtkNew(vtkIdList, pointNeighbors);
      vtkSVGeneralUtils::GetPointNeighbors(i, this->WorkPd, pointNeighbors);

      // Get weight of edges from edge table
      double weight_tot;
      for (int j=0; j<pointNeighbors->GetNumberOfIds(); j++)
      {
        // neighbor point id
        int p1 = pointNeighbors->GetId(j);

        // Get edge info
        vtkIdType edgeId = this->EdgeTable->IsEdge(i, p1);
        int edgeNeighbor = this->EdgeNeighbors->GetComponent(edgeId, 1);
        double weight    = this->EdgeWeights->GetValue(edgeId);

        // if no edge neighbor, then we can leave
        if (edgeNeighbor == -1)
          continue;

        // Set the harmonic weight and tutte weight for i,j
        this->AHarm->SetElement(i,p1, weight);
        this->ATutte->SetElement(i, p1, 1.0);

        // Update the total harmonic and tutte weight for point i
        tot_weight -= weight;
        tot_tutte_weight -= 1.0;
      }

      // Set the total harmonic and tutte weight for i,i
      double pt[3];
      this->WorkPd->GetPoint(i, pt);
      this->AHarm->SetElement(i, i, tot_weight);
      this->ATutte->SetElement(i, i, tot_tutte_weight);

      // Set initial values for solution vector
      this->Xu[i] = centroid[this->Dir0]; //pt[this->Dir0];
      this->Xv[i] = centroid[this->Dir1]; //pt[this->Dir1];
    }
  }

  return SV_OK;
}

// ----------------------
// SolveSystem
// ----------------------
int vtkSVPlanarMapper::SolveSystem()
{
  int numPoints = this->WorkPd->GetNumberOfPoints();

  double epsilon = 1.0e-8;

  vtkSVMathUtils::ConjugateGradient(this->ATutte, &this->Bu[0], numPoints,
                                    &this->Xu[0], epsilon);
  vtkSVMathUtils::ConjugateGradient(this->AHarm,  &this->Bu[0], numPoints,
                                    &this->Xu[0], epsilon);
  vtkSVMathUtils::ConjugateGradient(this->ATutte, &this->Bv[0], numPoints,
                                    &this->Xv[0], epsilon);
  vtkSVMathUtils::ConjugateGradient(this->AHarm,  &this->Bv[0], numPoints,
                                    &this->Xv[0], epsilon);

  // Get pt from boundary for stationary dir axis
  double origPt[3];
  this->BoundaryMapper->GetOutput()->GetPoint(0, origPt);

  for (int i=0; i<numPoints; i++)
  {

    // New pt
    double pt[3];
    pt[this->Dir0] = this->Xu[i];
    pt[this->Dir1] = this->Xv[i];
    pt[this->Dir2] = origPt[this->Dir2];
    this->PlanarPd->GetPoints()->SetPoint(i, pt);
  }

  vtkNew(vtkPolyDataNormals, normaler);
  normaler->SetInputData(this->PlanarPd);
  normaler->SplittingOff();
  normaler->ComputePointNormalsOff();
  normaler->ComputeCellNormalsOn();
  normaler->Update();

  this->PlanarPd->DeepCopy(normaler->GetOutput());
  return SV_OK;
}

// ----------------------
// InvertSystem
// ----------------------
int vtkSVPlanarMapper::InvertSystem(std::vector<std::vector<double> > &mat,
                                  std::vector<std::vector<double> > &invMat)
{
  int nr = mat.size();
  int nc = mat[0].size();
  if (nr != nc)
  {
    //vtkErrorMacro("Matrix is not square");
    return SV_ERROR;
  }

  double **inMat  = new double*[nr];
  double **outMat = new double*[nr];

  for (int i=0; i<nr; i++)
  {
    inMat[i]  = new double[nc];
    outMat[i] = new double[nc];
  }

  for (int i=0; i<nr; i++)
  {
    for (int j=0; j<nc; j++)
    {
      inMat[i][j] = mat[i][j];
    }
  }

  if (vtkMath::InvertMatrix(inMat, outMat, nr) == 0)
  {
    for (int i=0; i<nr; i++)
    {
      delete [] inMat[i];
      delete [] outMat[i];
    }
    delete [] inMat;
    delete [] outMat;
    //vtkErrorMacro("vtkMath could not invert matrix");
    return SV_ERROR;
  }

  for (int i=0; i<nr; i++)
  {
    for (int j=0; j<nr; j++)
    {
      invMat[i][j] = outMat[i][j];
    }
  }

  for (int i=0; i<nc; i++)
  {
    delete [] inMat[i];
    delete [] outMat[i];
  }
  delete [] inMat;
  delete [] outMat;
  return SV_OK;
}
