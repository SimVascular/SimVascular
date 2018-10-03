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

#include "vtkSVNURBSVolume.h"

#include "vtkAppendFilter.h"
#include "vtkArrayExtents.h"
#include "vtkArrayRange.h"
#include "vtkCellArray.h"
#include "vtkCleanPolyData.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkSmartPointer.h"
#include "vtkSparseArray.h"

#include "vtkSVCleanUnstructuredGrid.h"
#include "vtkSVGlobals.h"
#include "vtkSVNURBSUtils.h"
// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVNURBSVolume);

// ----------------------
// Constructor
// ----------------------
vtkSVNURBSVolume::vtkSVNURBSVolume()
{
  this->NumberOfUControlPoints = 0;
  this->NumberOfVControlPoints = 0;
  this->NumberOfWControlPoints = 0;
  this->NumberOfUKnotPoints    = 0;
  this->NumberOfVKnotPoints    = 0;
  this->NumberOfWKnotPoints    = 0;
  this->UDegree                = 0;
  this->VDegree                = 0;
  this->WDegree                = 0;
  this->UClamped               = 1;
  this->VClamped               = 1;
  this->WClamped               = 1;
  this->UClosed                = 0;
  this->VClosed                = 0;
  this->WClosed                = 0;

  this->ControlPointGrid    = vtkSVControlGrid::New();

  for (int i=0; i<3; i++)
    this->UVWKnotVectors[i] = vtkDoubleArray::New();

  this->UKnotVector = this->UVWKnotVectors[0];
  this->VKnotVector = this->UVWKnotVectors[1];
  this->WKnotVector = this->UVWKnotVectors[2];

  this->VolumeRepresentation = vtkUnstructuredGrid::New();
}

// ----------------------
// Destructor
// ----------------------
vtkSVNURBSVolume::~vtkSVNURBSVolume()
{
  if (this->ControlPointGrid != NULL)
  {
    this->ControlPointGrid->Delete();
  }
  for (int i=0; i<3; i++)
  {
    if (this->UVWKnotVectors[i] != NULL)
    {
      this->UVWKnotVectors[i]->Delete();
    }
  }

  if (this->VolumeRepresentation != NULL)
  {
    this->VolumeRepresentation->Delete();
  }
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVNURBSVolume::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "Number of control points in u direction: " << this->NumberOfUControlPoints << "\n";
  os << indent << "Number of knot points in u direction: " << this->NumberOfUKnotPoints << "\n";
  os << indent << "U Degree: " << this->UDegree << "\n";
  os << indent << "U Clamped: " << this->UClamped << "\n";
  os << indent << "U Closed: " << this->UClosed << "\n";
  os << "\n";
  os << indent << "Number of control points in v direction: " << this->NumberOfVControlPoints << "\n";
  os << indent << "Number of knot points in v direction: " << this->NumberOfVKnotPoints << "\n";
  os << indent << "V Degree: " << this->VDegree << "\n";
  os << indent << "V Clamped: " << this->VClamped << "\n";
  os << indent << "V Closed: " << this->VClosed << "\n";
  os << "\n";
  os << indent << "Number of control points in w direction: " << this->NumberOfWControlPoints << "\n";
  os << indent << "Number of knot points in w direction: " << this->NumberOfWKnotPoints << "\n";
  os << indent << "W Degree: " << this->WDegree << "\n";
  os << indent << "W Clamped: " << this->WClamped << "\n";
  os << indent << "W Closed: " << this->WClosed << "\n";
}


// ----------------------
// DeepCopy
// ----------------------
void vtkSVNURBSVolume::DeepCopy(vtkSVNURBSVolume *src)
{
  this->Superclass::DeepCopy(src);

  this->SetNumberOfUControlPoints(src->GetNumberOfUControlPoints());
  this->SetNumberOfVControlPoints(src->GetNumberOfVControlPoints());
  this->SetNumberOfWControlPoints(src->GetNumberOfWControlPoints());
  this->SetNumberOfUKnotPoints(src->GetNumberOfUKnotPoints());
  this->SetNumberOfVKnotPoints(src->GetNumberOfVKnotPoints());
  this->SetNumberOfWKnotPoints(src->GetNumberOfWKnotPoints());
  this->SetUDegree(src->GetUDegree());
  this->SetVDegree(src->GetVDegree());
  this->SetWDegree(src->GetWDegree());

  this->ControlPointGrid->DeepCopy(src->GetControlPointGrid());
  this->UKnotVector->DeepCopy(src->GetUKnotVector());
  this->VKnotVector->DeepCopy(src->GetVKnotVector());
  this->WKnotVector->DeepCopy(src->GetWKnotVector());

  this->VolumeRepresentation->DeepCopy(src->GetVolumeRepresentation());
}

// ----------------------
// Iniitialize
// ----------------------
void vtkSVNURBSVolume::Initialize()
{
  this->Superclass::Initialize();
}

// ----------------------
// GetData
// ----------------------
vtkSVNURBSVolume* vtkSVNURBSVolume::GetData(vtkInformation* info)
{
  return info? vtkSVNURBSVolume::SafeDownCast(info->Get(DATA_OBJECT())) : 0;
}

// ----------------------
// GetData
// ----------------------
vtkSVNURBSVolume* vtkSVNURBSVolume::GetData(vtkInformationVector* v, int i)
{
  return vtkSVNURBSVolume::GetData(v->GetInformationObject(i));
}

// ----------------------
// SetUKnotVector
// ----------------------
int vtkSVNURBSVolume::SetUKnotVector(vtkDoubleArray *knots)
{
  this->UKnotVector->DeepCopy(knots);
  this->NumberOfUKnotPoints = this->UKnotVector->GetNumberOfTuples();
  return SV_OK;
}

// ----------------------
// SetVKnotVector
// ----------------------
int vtkSVNURBSVolume::SetVKnotVector(vtkDoubleArray *knots)
{
  this->VKnotVector->DeepCopy(knots);
  this->NumberOfVKnotPoints = this->VKnotVector->GetNumberOfTuples();
  return SV_OK;
}

// ----------------------
// SetWKnotVector
// ----------------------
int vtkSVNURBSVolume::SetWKnotVector(vtkDoubleArray *knots)
{
  this->WKnotVector->DeepCopy(knots);
  this->NumberOfVKnotPoints = this->WKnotVector->GetNumberOfTuples();
  return SV_OK;
}

// ----------------------
// SetKnotVector
// ----------------------
void vtkSVNURBSVolume::SetKnotVector(vtkDoubleArray *knotVector, const int dim)
{
  // Get number of knots
  int nKnot = knotVector->GetNumberOfTuples();

  // Copy knots
  this->UVWKnotVectors[dim]->DeepCopy(knotVector);

  // Update number of knots
  if (dim == 0)
    this->NumberOfUKnotPoints = nKnot;
  else if (dim == 1)
    this->NumberOfVKnotPoints = nKnot;
  else
    this->NumberOfWKnotPoints = nKnot;
}


// ----------------------
// SetControlPointGrid
// ----------------------
int vtkSVNURBSVolume::SetControlPointGrid(vtkSVControlGrid *controlPoints)
{
  this->ControlPointGrid->DeepCopy(controlPoints);
  int dim[3];
  controlPoints->GetDimensions(dim);
  this->ControlPointGrid->SetDimensions(dim);
  this->NumberOfUControlPoints = dim[0];
  this->NumberOfVControlPoints = dim[1];
  this->NumberOfWControlPoints = dim[2];

  return SV_OK;
}

// ----------------------
// SetControlPoints
// ----------------------
void vtkSVNURBSVolume::SetControlPoints(vtkStructuredGrid *points3d)
{
  // Get dimensions
  int dim[3];
  points3d->GetDimensions(dim);

  // Set points from structured grid
  this->ControlPointGrid->SetPoints(points3d->GetPoints());
  this->ControlPointGrid->SetDimensions(dim);

  // Set weigths in u and v direction
  this->ControlPointGrid->GetPointData()->GetArray("Weights")
    ->SetNumberOfTuples(dim[0]*dim[1]*dim[2]);
  this->ControlPointGrid->GetPointData()->GetArray("Weights")
    ->FillComponent(0, 1.0);

  // Update number of control points
  this->NumberOfUControlPoints = dim[0];
  this->NumberOfVControlPoints = dim[1];
  this->NumberOfWControlPoints = dim[2];
}


// ----------------------
// GenerateVolumeRepresentation
// ----------------------
int vtkSVNURBSVolume::GenerateVolumeRepresentation(const double uSpacing,
		                                                const double vSpacing,
                                                    const double wSpacing)
{
  // Get number of control points and knots
  int dim[3];
  this->ControlPointGrid->GetDimensions(dim);
  this->NumberOfUControlPoints = dim[0];
  this->NumberOfVControlPoints = dim[1];
  this->NumberOfWControlPoints = dim[2];
  this->NumberOfUKnotPoints = this->UKnotVector->GetNumberOfTuples();
  this->NumberOfVKnotPoints = this->VKnotVector->GetNumberOfTuples();
  this->NumberOfWKnotPoints = this->WKnotVector->GetNumberOfTuples();
  int nUCon  = this->NumberOfUControlPoints;
  int nVCon  = this->NumberOfVControlPoints;
  int nWCon  = this->NumberOfWControlPoints;
  int nUKnot = this->NumberOfUKnotPoints;
  int nVKnot = this->NumberOfVKnotPoints;
  int nWKnot = this->NumberOfWKnotPoints;
  if (nUCon == 0 || nVCon == 0 || nWCon == 0)
  {
    vtkErrorMacro("No control points");
    return SV_ERROR;
  }
  if (nUKnot == 0 || nVKnot == 0 || nWKnot == 0)
  {
    vtkErrorMacro("No knot points");
    return SV_ERROR;
  }

  // Using clamped formula for degree of curve
  int p = nUKnot - nUCon - 1;
  int q = nVKnot - nVCon - 1;
  int r = nWKnot - nWCon - 1;

  //If nCon - 1 < p, not possible with clamping
  //If nCon - 1 = p, bezier with clamping
  //If nCon - 1 > p, fantastic

  // U direction!
  // -----------------------------------------------------------------------
  int numUDiv = ceil(1.0/uSpacing);
  vtkNew(vtkDoubleArray, uEvals);
  vtkSVNURBSUtils::LinSpace(0, 1, numUDiv, uEvals);

  // Get sparse array for Nu
  vtkNew(vtkSparseArray<double>, Nus);
  Nus->Resize(numUDiv, p+2);

  // Get sparse array for basis functions
  vtkNew(vtkSparseArray<double>, NUfinal);
  NUfinal->Resize(numUDiv, nUCon);

  // Loop through control points
  for (int i=0; i<nUCon; i++)
  {
    if (vtkSVNURBSUtils::BasisEvaluationVec(this->UKnotVector, p,
                                       i, uEvals, Nus) != SV_OK)
    {
      return SV_ERROR;
    }
    // for each sampling get the final basis functions
    for (int j=0; j<numUDiv; j++)
    {
      NUfinal->SetValue(j, i, Nus->GetValue(j, 0));
    }
  }

  // Last value should be 1
  NUfinal->SetValue(numUDiv-1, nUCon-1, 1.0);

  // V direction!
  // -----------------------------------------------------------------------
  int numVDiv = ceil(1.0/vSpacing);
  vtkNew(vtkDoubleArray, vEvals);
  vtkSVNURBSUtils::LinSpace(0, 1, numVDiv, vEvals);

  // Get sparse array for Nv
  vtkNew(vtkSparseArray<double>, Nvs);
  Nvs->Resize(numVDiv, q+2);

  // Get sparse array for basis functions
  vtkNew(vtkSparseArray<double>, NVfinal);
  NVfinal->Resize(numVDiv, nVCon);

  // Loop through control points
  for (int i=0; i<nVCon; i++)
  {
    // Evaluate the basis functions
    if (vtkSVNURBSUtils::BasisEvaluationVec(this->VKnotVector, q,
                                       i, vEvals, Nvs) != SV_OK)
    {
      return SV_ERROR;
    }
    // for each sampling get the final basis functions
    double ratVal = 0.0;
    for (int j=0; j<numVDiv; j++)
    {
      NVfinal->SetValue(j, i, Nvs->GetValue(j, 0));
    }
  }

  // Last value should be 1
  NVfinal->SetValue(numVDiv-1, nVCon-1, 1.0);

  // W direction!
  // -----------------------------------------------------------------------
  int numWDiv = ceil(1.0/wSpacing);
  vtkNew(vtkDoubleArray, wEvals);
  vtkSVNURBSUtils::LinSpace(0, 1, numWDiv, wEvals);

  // Get sparse array for Nw
  vtkNew(vtkSparseArray<double>, Nws);
  Nws->Resize(numWDiv, r+2);

  // Get sparse array for basis functions
  vtkNew(vtkSparseArray<double>, NWfinal);
  NWfinal->Resize(numWDiv, nWCon);

  // Loop through control points
  for (int i=0; i<nWCon; i++)
  {
    if (vtkSVNURBSUtils::BasisEvaluationVec(this->WKnotVector, r,
                                       i, wEvals, Nws) != SV_OK)
    {
      return SV_ERROR;
    }
    // for each sampling get the final basis functions
    for (int j=0; j<numWDiv; j++)
    {
      NWfinal->SetValue(j, i, Nws->GetValue(j, 0));
    }
  }

  // Last value should be 1
  NWfinal->SetValue(numWDiv-1, nWCon-1, 1.0);

  vtkNew(vtkSparseArray<double>, NVfinalT);
  vtkSVNURBSUtils::MatrixTranspose(NVfinal, 0, NVfinalT);

  vtkNew(vtkSparseArray<double>, NWfinalT);
  vtkSVNURBSUtils::MatrixTranspose(NWfinal, 0, NWfinalT);
  //Get the physical points on the surface!
  // -----------------------------------------------------------------------
  // When dealing with the rational of NURBS, need to multiply points by
  // weights when sending through matrix multiplication. However, still need
  // fourth spot in point, weight vector because in the end, we will need
  // to divide by the total weight
  vtkArrayExtents size;
  size.SetDimensions(4);
  size.SetExtent(0, vtkArrayRange(0, numUDiv));
  size.SetExtent(1, vtkArrayRange(0, numVDiv));
  size.SetExtent(2, vtkArrayRange(0, nWCon));
  size.SetExtent(3, vtkArrayRange(0, 4));
  vtkNew(vtkDenseArray<double>, tmpW);
  tmpW->Resize(size);

  for (int i=0; i<nWCon; i++)
  {
    vtkNew(vtkDenseArray<double>, tmpControlGrid);
    vtkSVNURBSUtils::ControlGridToTypedArraySPECIAL(this->ControlPointGrid, 0, 1, 2, i, tmpControlGrid);

    // Do first matrix multiply with u basis functions
    vtkNew(vtkDenseArray<double>, tmpUGrid);
    if (vtkSVNURBSUtils::MatrixMatrixMultiply(NUfinal, 0, 1, tmpControlGrid, 1, 4, tmpUGrid) != SV_OK)
    {
      fprintf(stderr, "Error in matrix multiply\n");
      return SV_ERROR;
    }
    // Do second matrix multiply with v basis functions
    vtkNew(vtkDenseArray<double>, tmpVGrid);
    if (vtkSVNURBSUtils::MatrixMatrixMultiply(tmpUGrid, 1, 4, NVfinalT, 0, 1, tmpVGrid) != SV_OK)
    {
      fprintf(stderr, "Error in matrix multiply\n");
      return SV_ERROR;
    }

    vtkSVNURBSUtils::SetMatrixOfDim4Grid(tmpVGrid, tmpW, 0, 1, 2, i, 4);
  }

  size.SetExtent(0, vtkArrayRange(0, numUDiv));
  size.SetExtent(1, vtkArrayRange(0, numVDiv));
  size.SetExtent(2, vtkArrayRange(0, numWDiv));
  size.SetExtent(3, vtkArrayRange(0, 4));
  vtkNew(vtkDenseArray<double>, fullGrid);
  fullGrid->Resize(size);
  for (int i=0; i<numUDiv; i++)
  {
    vtkNew(vtkDenseArray<double>, tmpWGrid);
    vtkSVNURBSUtils::GetMatrixOfDim4Grid(tmpW, 1, 2, 0, i, 4, tmpWGrid);

    // Do second matrix multiply with v basis functions
    vtkNew(vtkDenseArray<double>, tmpVWGrid);
    if (vtkSVNURBSUtils::MatrixMatrixMultiply(tmpWGrid, 1, 4, NWfinalT, 0, 1, tmpVWGrid) != SV_OK)
    {
      fprintf(stderr, "Error in matrix multiply\n");
      return SV_ERROR;
    }

    vtkSVNURBSUtils::SetMatrixOfDim4Grid(tmpVWGrid, fullGrid, 1, 2, 0, i, 4);
  }
  // Set up final grid of points
  vtkNew(vtkStructuredGrid, finalGrid);
  vtkNew(vtkPoints, tmpVPoints);
  finalGrid->SetPoints(tmpVPoints);
  vtkSVNURBSUtils::TypedArrayToStructuredGridRational(fullGrid, finalGrid);

  // Get grid connectivity for pointset
  vtkNew(vtkAppendFilter, converter);
  converter->SetInputData(finalGrid);
  converter->Update();

  // Clean the volume in case of duplicate points (closed volume)
  vtkNew(vtkSVCleanUnstructuredGrid, cleaner);
  cleaner->SetInputData(converter->GetOutput());
  cleaner->Update();

  // Get clean output and build links
  this->VolumeRepresentation->DeepCopy(cleaner->GetOutput());
  this->VolumeRepresentation->BuildLinks();

  return SV_OK;
}
