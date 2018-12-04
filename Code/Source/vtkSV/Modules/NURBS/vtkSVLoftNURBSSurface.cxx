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

#include "vtkSVLoftNURBSSurface.h"

#include "vtkAlgorithmOutput.h"
#include "vtkCellData.h"
#include "vtkErrorCode.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkMath.h"
#include "vtkObjectFactory.h"
#include "vtkPolyData.h"
#include "vtkPointData.h"
#include "vtkSmartPointer.h"
#include "vtkStreamingDemandDrivenPipeline.h"
#include "vtkTrivialProducer.h"

#include "vtkSVGlobals.h"
#include "vtkSVMathUtils.h"
#include "vtkSVNURBSUtils.h"

#include <string>
#include <sstream>
#include <iostream>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVLoftNURBSSurface);

// ----------------------
// Constructor
// ----------------------
vtkSVLoftNURBSSurface::vtkSVLoftNURBSSurface()
{
  this->UDegree = 2;
  this->VDegree = 2;

  this->PolyDataUSpacing = 0.1;
  this->PolyDataVSpacing = 0.1;

  this->StartUDerivatives = vtkDoubleArray::New();
  this->StartVDerivatives = vtkDoubleArray::New();
  this->EndUDerivatives   = vtkDoubleArray::New();
  this->EndVDerivatives   = vtkDoubleArray::New();

  this->StartUDerivatives->SetNumberOfComponents(3);
  this->StartUDerivatives->SetNumberOfTuples(1);
  this->StartVDerivatives->SetNumberOfComponents(3);
  this->StartVDerivatives->SetNumberOfTuples(1);
  this->EndUDerivatives->SetNumberOfComponents(3);
  this->EndUDerivatives->SetNumberOfTuples(1);
  this->EndVDerivatives->SetNumberOfComponents(3);
  this->EndVDerivatives->SetNumberOfTuples(1);

  this->InputGrid = vtkStructuredGrid::New();
  this->Surface = vtkSVNURBSSurface::New();

  this->UKnotSpanType        = NULL;
  this->VKnotSpanType        = NULL;

  this->UParametricSpanType = NULL;
  this->VParametricSpanType = NULL;
}

// ----------------------
// Destructor
// ----------------------
vtkSVLoftNURBSSurface::~vtkSVLoftNURBSSurface()
{
  if (this->Surface != NULL)
  {
    this->Surface->Delete();
  }
  if (this->StartUDerivatives != NULL)
  {
    this->StartUDerivatives->Delete();
  }
  if (this->StartVDerivatives != NULL)
  {
    this->StartVDerivatives->Delete();
  }
  if (this->EndUDerivatives != NULL)
  {
    this->EndUDerivatives->Delete();
  }
  if (this->EndVDerivatives != NULL)
  {
    this->EndVDerivatives->Delete();
  }

  if (this->UKnotSpanType != NULL)
  {
    delete [] this->UKnotSpanType;
    this->UKnotSpanType = NULL;
  }
  if (this->VKnotSpanType != NULL)
  {
    delete [] this->VKnotSpanType;
    this->VKnotSpanType = NULL;
  }
  if (this->UParametricSpanType != NULL)
  {
    delete [] this->UParametricSpanType;
    this->UParametricSpanType = NULL;
  }
  if (this->VParametricSpanType != NULL)
  {
    delete [] this->VParametricSpanType;
    this->VParametricSpanType = NULL;
  }
}

// ----------------------
// RequestData
// ----------------------
int vtkSVLoftNURBSSurface::RequestData(
    vtkInformation *vtkNotUsed(request),
    vtkInformationVector **inputVector,
    vtkInformationVector *outputVector)
{
  // get the info object
  // get the ouptut
  vtkStructuredGrid *input = vtkStructuredGrid::GetData(inputVector[0]);
  vtkPolyData *output = vtkPolyData::GetData(outputVector, 0);

  this->InputGrid->DeepCopy(input);

  if (this->UKnotSpanType == NULL ||
      this->VKnotSpanType == NULL)
  {
    vtkErrorMacro("Need to provide knot span types for u, v directions");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }

  if (this->UParametricSpanType == NULL ||
      this->VParametricSpanType == NULL)
  {
    vtkErrorMacro("Need to provide parametric span types for u, v directions");
    this->SetErrorCode(vtkErrorCode::UserError + 2);
    return SV_ERROR;
  }

  // TODO: Need to make sure knot span and parameteric span types are set
  if (this->LoftNURBS(this->InputGrid, output) != SV_OK)
  {
    vtkErrorMacro("Could not loft surface");
    this->SetErrorCode(vtkErrorCode::UserError + 3);
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVLoftNURBSSurface::PrintSelf(ostream& os,
    vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);

  os << indent << "U Degree: " << this->UDegree << "\n";
  os << indent << "U Knot span type: " << this->UKnotSpanType << "\n";
  os << indent << "U Parametric values span type: " << this->UParametricSpanType << "\n";
  for (int i=0; i<this->StartUDerivatives->GetNumberOfTuples(); i++)
  {
    double tup[3]; this->StartUDerivatives->GetTuple(i, tup);
    os << indent << "Start U Derivative " << i << ": " << tup[0] << " ";
    os << tup[1] << " " << tup[2] << "\n";
  }
  for (int i=0; i<this->EndUDerivatives->GetNumberOfTuples(); i++)
  {
    double tup[3]; this->EndUDerivatives->GetTuple(i, tup);
    os << indent << "End V Derivative " << i << ": " << tup[0] << " ";
    os << tup[1] << " " << tup[2] << "\n";
  }
  os << "\n";

  os << indent << "V Degree: " << this->UDegree << "\n";
  os << indent << "V Knot span type: " << this->VKnotSpanType << "\n";
  os << indent << "V Parametric values span type: " << this->VParametricSpanType << "\n";
  for (int i=0; i<this->StartVDerivatives->GetNumberOfTuples(); i++)
  {
    double tup[3]; this->StartVDerivatives->GetTuple(i, tup);
    os << indent << "Start V Derivative " << i << ": " << tup[0] << " ";
    os << tup[1] << " " << tup[2] << "\n";
  }
  for (int i=0; i<this->EndVDerivatives->GetNumberOfTuples(); i++)
  {
    double tup[3]; this->EndVDerivatives->GetTuple(i, tup);
    os << indent << "End V Derivative " << i << ": " << tup[0] << " ";
    os << tup[1] << " " << tup[2] << "\n";
  }
}

// ----------------------
// FillInputPortInformation
// ----------------------
int vtkSVLoftNURBSSurface::FillInputPortInformation(
    int port, vtkInformation *info)
{
  info->Set(vtkAlgorithm::INPUT_REQUIRED_DATA_TYPE(), "vtkStructuredGrid");
  return SV_OK;
}

// ----------------------
// LoftNURBS
// ----------------------
int vtkSVLoftNURBSSurface::LoftNURBS(vtkStructuredGrid *input,
    vtkPolyData *outputPD)
{
  // Get number of control points and degree
  int dim[3];
  input->GetDimensions(dim);
  int nUCon = dim[0];
  int nVCon = dim[1];
  int p     = this->UDegree;
  int q     = this->VDegree;

  if (dim[2] != 1)
  {
    vtkErrorMacro("Structured grid should only have two dimensions for lofting of surface.");
    return SV_ERROR;
  }

  // Get knot span and parametric span types
  std::string kutype = this->UKnotSpanType;
  std::string kvtype = this->VKnotSpanType;
  std::string putype = this->UParametricSpanType;
  std::string pvtype = this->VParametricSpanType;

  // Check that the number of inputs enough for degree
  if (p > nUCon)
  {
    vtkErrorMacro("Need to either decrease degree given or number of inputs in U direction");
    return SV_ERROR;
  }
  if (q > nVCon)
  {
    vtkErrorMacro("Need to either decrease degree given or number of inputs in V direction");
    return SV_ERROR;
  }

  // Set the temporary control points
  vtkNew(vtkPoints, tmpUPoints);
  tmpUPoints->SetNumberOfPoints(nUCon);
  for (int i=0; i<nUCon; i++)
  {
    int pos[3]; pos[0] = i; pos[1] = 0; pos[2] = 0;
    int ptId = vtkStructuredData::ComputePointId(dim, pos);
    tmpUPoints->SetPoint(i, input->GetPoint(ptId));
  }

  // Get the input point set u representation
  vtkNew(vtkDoubleArray, U);
  if (vtkSVNURBSUtils::GetUs(tmpUPoints, putype, U) != SV_OK)
  {
    return SV_ERROR;
  }
  //fprintf(stdout,"U:\n");
  //vtkSVNURBSUtils::PrintArray(U);

  // Get the knots in the u direction
  vtkNew(vtkDoubleArray, uKnots);
  if (vtkSVNURBSUtils::GetKnots(U, p, kutype, uKnots) != SV_OK)
  {
    vtkErrorMacro("Error getting knots");
    return SV_ERROR;
  }
  //fprintf(stdout,"X knots\n");
  //vtkSVNURBSUtils::PrintArray(uKnots);
  //

  vtkNew(vtkPoints, tmpVPoints);
  tmpVPoints->SetNumberOfPoints(nVCon);
  for (int i=0; i<nVCon; i++)
  {
    int pos[3]; pos[0] = 0; pos[1] = i; pos[2] = 0;
    int ptId = vtkStructuredData::ComputePointId(dim, pos);
    tmpVPoints->SetPoint(i, input->GetPoint(ptId));
  }
  // Get the input point set v representation
  vtkNew(vtkDoubleArray, V);
  if (vtkSVNURBSUtils::GetUs(tmpVPoints, pvtype, V) != SV_OK)
  {
    return SV_ERROR;
  }
  //fprintf(stdout,"V:\n");
  //vtkSVNURBSUtils::PrintArray(V);

  // Get the knots in the v direction
  vtkNew(vtkDoubleArray, vKnots);
  if (vtkSVNURBSUtils::GetKnots(V, q, kvtype, vKnots) != SV_OK)
  {
    vtkErrorMacro("Error getting knots");
    return SV_ERROR;
  }
  //fprintf(stdout,"Y knots\n");
  //vtkSVNURBSUtils::PrintArray(vKnots);

  // Get derivatives in fomrat we need
  vtkNew(vtkDoubleArray, DU0); DU0->DeepCopy(this->StartUDerivatives);
  vtkNew(vtkDoubleArray, DUN); DUN->DeepCopy(this->EndUDerivatives);
  if (!strncmp(kutype.c_str(), "derivative", 10))
  {
    // Get default derivatives if we need!
    if (DU0->GetNumberOfTuples() == 1 ||
        DUN->GetNumberOfTuples() == 1)
    {
      this->GetDefaultDerivatives(input, 0, DU0, DUN);
    }
  }

  // Get derivatives in format we need
  vtkNew(vtkDoubleArray, DV0); DV0->DeepCopy(this->StartVDerivatives);
  vtkNew(vtkDoubleArray, DVN); DVN->DeepCopy(this->EndVDerivatives);
  if (!strncmp(kvtype.c_str(), "derivative", 10))
  {
    // Get default derivatives if we need!
    if (DV0->GetNumberOfTuples() == 1 ||
        DVN->GetNumberOfTuples() == 1)
    {
      this->GetDefaultDerivatives(input, 1, DV0, DVN);
    }
  }

  // Get the control points of surface, lengthy operation in vtkSVNURBSUtils
  vtkNew(vtkStructuredGrid, cPoints);
  vtkNew(vtkDoubleArray, uWeights);
  vtkNew(vtkDoubleArray, vWeights);
  if (vtkSVNURBSUtils::GetControlPointsOfSurface(input, U, V, uWeights, vWeights,
                                               uKnots, vKnots, p, q, kutype, kvtype,
                                               DU0, DUN, DV0, DVN, cPoints) != SV_OK)
  {
    return SV_ERROR;
  }

  // Set the knot vectors and control points
  this->Surface->SetKnotVector(uKnots, 0);
  this->Surface->SetKnotVector(vKnots, 1);
  this->Surface->SetControlPoints(cPoints);
  this->Surface->SetUDegree(p);
  this->Surface->SetUDegree(q);

  // Get the polydata representation from the NURBS Surface
  this->Surface->GeneratePolyDataRepresentation(this->PolyDataUSpacing, this->PolyDataVSpacing);
  outputPD->DeepCopy(this->Surface->GetSurfaceRepresentation());

  return SV_OK;
}

// ----------------------
// GetDefaultDerivatives
// ----------------------
int vtkSVLoftNURBSSurface::GetDefaultDerivatives(vtkStructuredGrid *input, const int comp, vtkDoubleArray *D0out, vtkDoubleArray *DNout)
{
  // Get dimensions
  int dim[3];
  input->GetDimensions(dim);

  // Get number of values and derivatives from dim
  int numVals   = dim[comp];
  int numDerivs = dim[(comp+1)%2];

  // Set number of tuples for derivatives
  D0out->SetNumberOfTuples(numDerivs);
  DNout->SetNumberOfTuples(numDerivs);
  for (int i=0; i<numDerivs; i++)
  {
    int pos[3]; pos[2] = 0;
    pos[(comp+1)%2] = i;

    // Get the point id
    double pt0[3]; pos[comp] = 0;
    int ptId = vtkStructuredData::ComputePointId(dim, pos);
    input->GetPoint(ptId, pt0);

    // Get the point id
    double pt1[3]; pos[comp] = 1;
    ptId = vtkStructuredData::ComputePointId(dim, pos);
    input->GetPoint(ptId, pt1);

    // Get the point id
    double ptnm1[3]; pos[comp] = numVals - 1;
    ptId = vtkStructuredData::ComputePointId(dim, pos);
    input->GetPoint(ptId, ptnm1);

    // Get the point id
    double ptnm2[3]; pos[comp] = numVals - 2;
    ptId = vtkStructuredData::ComputePointId(dim, pos);
    input->GetPoint(ptId, ptnm2);

    // From point ids, compute vectors at ends of data
    double D0[3], DN[3];
    vtkMath::Subtract(pt1, pt0, D0);
    vtkMath::Subtract(ptnm1, ptnm2, DN);

    // Set tuples
    D0out->SetTuple(i, D0);
    DNout->SetTuple(i, DN);
  }

  return SV_OK;
}
