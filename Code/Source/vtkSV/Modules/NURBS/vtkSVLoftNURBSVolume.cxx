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

#include "vtkSVLoftNURBSVolume.h"

#include "vtkAlgorithmOutput.h"
#include "vtkCellData.h"
#include "vtkErrorCode.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkMath.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkUnstructuredGrid.h"
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
vtkStandardNewMacro(vtkSVLoftNURBSVolume);

// ----------------------
// Constructor
// ----------------------
vtkSVLoftNURBSVolume::vtkSVLoftNURBSVolume()
{
  this->UDegree = 2;
  this->VDegree = 2;
  this->WDegree = 2;

  this->UnstructuredGridUSpacing = 0.1;
  this->UnstructuredGridVSpacing = 0.1;
  this->UnstructuredGridWSpacing = 0.1;

  this->StartUDerivatives = vtkStructuredGrid::New();
  this->StartVDerivatives = vtkStructuredGrid::New();
  this->StartWDerivatives = vtkStructuredGrid::New();
  this->EndUDerivatives   = vtkStructuredGrid::New();
  this->EndVDerivatives   = vtkStructuredGrid::New();
  this->EndWDerivatives   = vtkStructuredGrid::New();

  this->InputGrid = vtkStructuredGrid::New();
  this->Volume = vtkSVNURBSVolume::New();

  this->UKnotSpanType        = NULL;
  this->VKnotSpanType        = NULL;
  this->WKnotSpanType        = NULL;

  this->UParametricSpanType = NULL;
  this->VParametricSpanType = NULL;
  this->WParametricSpanType = NULL;
}

// ----------------------
// Destructor
// ----------------------
vtkSVLoftNURBSVolume::~vtkSVLoftNURBSVolume()
{
  if (this->InputGrid != NULL)
  {
    this->InputGrid->Delete();
  }
  if (this->Volume != NULL)
  {
    this->Volume->Delete();
  }
  if (this->StartUDerivatives != NULL)
  {
    this->StartUDerivatives->Delete();
  }
  if (this->StartVDerivatives != NULL)
  {
    this->StartVDerivatives->Delete();
  }
  if (this->StartWDerivatives != NULL)
  {
    this->StartWDerivatives->Delete();
  }
  if (this->EndUDerivatives != NULL)
  {
    this->EndUDerivatives->Delete();
  }
  if (this->EndVDerivatives != NULL)
  {
    this->EndVDerivatives->Delete();
  }
  if (this->EndWDerivatives != NULL)
  {
    this->EndWDerivatives->Delete();
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
  if (this->WKnotSpanType != NULL)
  {
    delete [] this->WKnotSpanType;
    this->WKnotSpanType = NULL;
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
  if (this->WParametricSpanType != NULL)
  {
    delete [] this->WParametricSpanType;
    this->WParametricSpanType = NULL;
  }
}

// ----------------------
// RequestData
// ----------------------
int vtkSVLoftNURBSVolume::RequestData(
    vtkInformation *vtkNotUsed(request),
    vtkInformationVector **inputVector,
    vtkInformationVector *outputVector)
{
  // get the info object
  // get the ouptut
  vtkStructuredGrid *input = vtkStructuredGrid::GetData(inputVector[0]);
  vtkUnstructuredGrid *output = vtkUnstructuredGrid::GetData(outputVector, 0);

  this->InputGrid->DeepCopy(input);

  if (this->InputGrid == NULL)
  {
    vtkErrorMacro("Need to set the input grid");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }

  if (this->UKnotSpanType == NULL ||
      this->VKnotSpanType == NULL ||
      this->WKnotSpanType == NULL)
  {
    vtkErrorMacro("Need to provide knot span types for u, v, w directions");
    this->SetErrorCode(vtkErrorCode::UserError + 2);
    return SV_ERROR;
  }

  if (this->UParametricSpanType == NULL ||
      this->VParametricSpanType == NULL ||
      this->WParametricSpanType == NULL)
  {
    vtkErrorMacro("Need to provide parametric span types for u, v, w directions");
    this->SetErrorCode(vtkErrorCode::UserError + 3);
    return SV_ERROR;
  }

  // TODO: Need to make sure knot span and parameteric span types are set
  if (this->LoftNURBS(this->InputGrid, output) != SV_OK)
  {
    vtkErrorMacro("Could not loft surface");
    this->SetErrorCode(vtkErrorCode::UserError + 4);
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVLoftNURBSVolume::PrintSelf(ostream& os,
    vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);

  os << indent << "U Degree: " << this->UDegree << "\n";
  os << indent << "U Knot span type: " << this->UKnotSpanType << "\n";
  os << indent << "U Parametric values span type: " << this->UParametricSpanType << "\n";
  for (int i=0; i<this->StartUDerivatives->GetNumberOfPoints(); i++)
  {
    double tup[3]; this->StartUDerivatives->GetPoint(i, tup);
    os << indent << "Start U Derivative " << i << ": " << tup[0] << " ";
    os << tup[1] << " " << tup[2] << "\n";
  }
  for (int i=0; i<this->EndUDerivatives->GetNumberOfPoints(); i++)
  {
    double tup[3]; this->EndUDerivatives->GetPoint(i, tup);
    os << indent << "End U Derivative " << i << ": " << tup[0] << " ";
    os << tup[1] << " " << tup[2] << "\n";
  }
  os << "\n";

  os << indent << "V Degree: " << this->VDegree << "\n";
  os << indent << "V Knot span type: " << this->VKnotSpanType << "\n";
  os << indent << "V Parametric values span type: " << this->VParametricSpanType << "\n";
  for (int i=0; i<this->StartVDerivatives->GetNumberOfPoints(); i++)
  {
    double tup[3]; this->StartVDerivatives->GetPoint(i, tup);
    os << indent << "Start V Derivative " << i << ": " << tup[0] << " ";
    os << tup[1] << " " << tup[2] << "\n";
  }
  for (int i=0; i<this->EndVDerivatives->GetNumberOfPoints(); i++)
  {
    double tup[3]; this->EndVDerivatives->GetPoint(i, tup);
    os << indent << "End V Derivative " << i << ": " << tup[0] << " ";
    os << tup[1] << " " << tup[2] << "\n";
  }

  os << indent << "W Degree: " << this->WDegree << "\n";
  os << indent << "W Knot span type: " << this->WKnotSpanType << "\n";
  os << indent << "W Parametric values span type: " << this->WParametricSpanType << "\n";
  for (int i=0; i<this->StartWDerivatives->GetNumberOfPoints(); i++)
  {
    double tup[3]; this->StartWDerivatives->GetPoint(i, tup);
    os << indent << "Start W Derivative " << i << ": " << tup[0] << " ";
    os << tup[1] << " " << tup[2] << "\n";
  }
  for (int i=0; i<this->EndWDerivatives->GetNumberOfPoints(); i++)
  {
    double tup[3]; this->EndWDerivatives->GetPoint(i, tup);
    os << indent << "End W Derivative " << i << ": " << tup[0] << " ";
    os << tup[1] << " " << tup[2] << "\n";
  }
}

// ----------------------
// FillInputPortInformation
// ----------------------
int vtkSVLoftNURBSVolume::FillInputPortInformation(
    int port, vtkInformation *info)
{
  info->Set(vtkAlgorithm::INPUT_REQUIRED_DATA_TYPE(), "vtkStructuredGrid");
  return SV_OK;
}

// ----------------------
// LoftNURBS
// ----------------------
int vtkSVLoftNURBSVolume::LoftNURBS(vtkStructuredGrid *input,
    vtkUnstructuredGrid *outputUG)
{
  // Get number of control points and degree
  int dim[3];
  input->GetDimensions(dim);
  int nUCon = dim[0];
  int nVCon = dim[1];
  int nWCon = dim[2];
  int p     = this->UDegree;
  int q     = this->VDegree;
  int r     = this->WDegree;

  // Get knot span and parametric span types
  std::string kutype = this->UKnotSpanType;
  std::string kvtype = this->VKnotSpanType;
  std::string kwtype = this->WKnotSpanType;
  std::string putype = this->UParametricSpanType;
  std::string pvtype = this->VParametricSpanType;
  std::string pwtype = this->WParametricSpanType;

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
  if (r > nWCon)
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
    fprintf(stderr,"Error getting knots\n");
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
    fprintf(stderr,"Error getting knots\n");
    return SV_ERROR;
  }
  //fprintf(stdout,"Y knots\n");
  //vtkSVNURBSUtils::PrintArray(vKnots);

  vtkNew(vtkPoints, tmpWPoints);
  tmpWPoints->SetNumberOfPoints(nWCon);
  for (int i=0; i<nWCon; i++)
  {
    int pos[3]; pos[0] = 0; pos[1] = 0; pos[2] = i;
    int ptId = vtkStructuredData::ComputePointId(dim, pos);
    tmpWPoints->SetPoint(i, input->GetPoint(ptId));
  }
  // Get the input point set v representation
  vtkNew(vtkDoubleArray, W);
  if (vtkSVNURBSUtils::GetUs(tmpWPoints, pwtype, W) != SV_OK)
  {
    return SV_ERROR;
  }
  //fprintf(stdout,"W:\n");
  //vtkSVNURBSUtils::PrintArray(W);

  // Get the knots in the w direction
  vtkNew(vtkDoubleArray, wKnots);
  if (vtkSVNURBSUtils::GetKnots(W, r, kwtype, wKnots) != SV_OK)
  {
    fprintf(stderr,"Error getting knots\n");
    return SV_ERROR;
  }
  //fprintf(stdout,"Z knots\n");
  //vtkSVNURBSUtils::PrintArray(wKnots);

  // Get derivatives in fomrat we need
  vtkNew(vtkStructuredGrid, DU0); DU0->DeepCopy(this->StartUDerivatives);
  vtkNew(vtkStructuredGrid, DUN); DUN->DeepCopy(this->EndUDerivatives);
  if (!strncmp(kutype.c_str(), "derivative", 10))
  {
    // Get default derivatives if we need!
    if (DU0->GetNumberOfPoints() == 0 ||
        DUN->GetNumberOfPoints() == 0)
    {
      vtkDebugMacro("Getting default derivatives");
      vtkNew(vtkPoints, DU0Points);
      vtkNew(vtkPoints, DUNPoints);
      DU0->SetPoints(DU0Points);
      DUN->SetPoints(DUNPoints);
      this->GetDefaultDerivatives(input, 0, DU0, DUN);
    }
  }

  // Get derivatives in format we need
  vtkNew(vtkStructuredGrid, DV0); DV0->DeepCopy(this->StartVDerivatives);
  vtkNew(vtkStructuredGrid, DVN); DVN->DeepCopy(this->EndVDerivatives);
  if (!strncmp(kvtype.c_str(), "derivative", 10))
  {
    // Get default derivatives if we need!
    if (DV0->GetNumberOfPoints() == 0 ||
        DVN->GetNumberOfPoints() == 0)
    {
      vtkDebugMacro("Getting default derivatives");
      vtkNew(vtkPoints, DV0Points);
      vtkNew(vtkPoints, DVNPoints);
      DV0->SetPoints(DV0Points);
      DVN->SetPoints(DVNPoints);
      this->GetDefaultDerivatives(input, 1, DV0, DVN);
    }
  }

  // Get derivatives in format we need
  vtkNew(vtkStructuredGrid, DW0); DW0->DeepCopy(this->StartWDerivatives);
  vtkNew(vtkStructuredGrid, DWN); DWN->DeepCopy(this->EndWDerivatives);
  if (!strncmp(kwtype.c_str(), "derivative", 10))
  {
    // Get default derivatives if we need!
    if (DW0->GetNumberOfPoints() == 0 ||
        DWN->GetNumberOfPoints() == 0)
    {
      vtkDebugMacro("Getting default derivatives");
      vtkNew(vtkPoints, DW0Points);
      vtkNew(vtkPoints, DWNPoints);
      DW0->SetPoints(DW0Points);
      DWN->SetPoints(DWNPoints);
      this->GetDefaultDerivatives(input, 2, DW0, DWN);
    }
  }

  // Get the control points of surface, lengthy operation in vtkSVNURBSUtils
  vtkNew(vtkStructuredGrid, cPoints);
  vtkNew(vtkDoubleArray, uWeights);
  vtkNew(vtkDoubleArray, vWeights);
  vtkNew(vtkDoubleArray, wWeights);
  if (vtkSVNURBSUtils::GetControlPointsOfVolume(input, U, V, W,
                                                uWeights, vWeights, wWeights,
                                                uKnots, vKnots, wKnots,
                                                p, q, r, kutype, kvtype, kwtype,
                                                DU0, DUN, DV0, DVN, DW0, DWN,
                                                cPoints) != SV_OK)
  {
    return SV_ERROR;
  }

  // Set the knot vectors and control points
  this->Volume->SetKnotVector(uKnots, 0);
  this->Volume->SetKnotVector(vKnots, 1);
  this->Volume->SetKnotVector(wKnots, 2);
  this->Volume->SetControlPoints(cPoints);
  this->Volume->SetUDegree(p);
  this->Volume->SetVDegree(q);
  this->Volume->SetWDegree(r);
  //fprintf(stdout,"X knots\n");
  //vtkSVNURBSUtils::PrintArray(uKnots);
  //fprintf(stdout,"Y knots\n");
  //vtkSVNURBSUtils::PrintArray(vKnots);
  //fprintf(stdout,"Z knots\n");
  //vtkSVNURBSUtils::PrintArray(wKnots);

  // Get the unstructuredgird representation from the NURBS Volume
  this->Volume->GenerateVolumeRepresentation(this->UnstructuredGridUSpacing, this->UnstructuredGridVSpacing, this->UnstructuredGridWSpacing);
  outputUG->DeepCopy(this->Volume->GetVolumeRepresentation());

  return SV_OK;
}

// ----------------------
// GetDefaultDerivatives
// ----------------------
int vtkSVLoftNURBSVolume::GetDefaultDerivatives(vtkStructuredGrid *input, const int comp, vtkStructuredGrid *D0out, vtkStructuredGrid *DNout)
{
  // Get dimensions
  int dim[3];
  input->GetDimensions(dim);

  // Get number of values and derivatives from dim
  int numVals    = dim[comp];
  int numXDerivs = dim[(comp+1)%3];
  int numYDerivs = dim[(comp+2)%3];

  // Set number of tuples for derivatives
  int dim2D[3];
  dim2D[0] = numXDerivs;
  dim2D[1] = numYDerivs;
  dim2D[2] = 1;
  D0out->SetDimensions(dim2D);
  D0out->GetPoints()->SetNumberOfPoints(numXDerivs*numYDerivs);
  DNout->SetDimensions(dim2D);
  DNout->GetPoints()->SetNumberOfPoints(numXDerivs*numYDerivs);

  // Set tuples
  for (int i=0; i<numXDerivs; i++)
  {
    for (int j=0; j<numYDerivs; j++)
    {
      int pos[3];
      pos[(comp+1)%3] = i;
      pos[(comp+2)%3] = j;

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

      // From point ids, compute vectors at ends of data
      pos[comp] = 0;
      ptId = vtkStructuredData::ComputePointId(dim, pos);

      D0out->GetPoints()->SetPoint(ptId, D0);
      DNout->GetPoints()->SetPoint(ptId, DN);
    }
  }

  return SV_OK;
}
