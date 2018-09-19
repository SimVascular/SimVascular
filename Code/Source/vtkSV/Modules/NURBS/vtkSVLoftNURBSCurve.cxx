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

#include "vtkSVLoftNURBSCurve.h"

#include "vtkCellData.h"
#include "vtkErrorCode.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkMath.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPolyData.h"
#include "vtkSmartPointer.h"

#include "vtkSVGlobals.h"
#include "vtkSVNURBSCurve.h"
#include "vtkSVNURBSUtils.h"

#include <string>
#include <sstream>
#include <iostream>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVLoftNURBSCurve);

// ----------------------
// Constructor
// ----------------------
vtkSVLoftNURBSCurve::vtkSVLoftNURBSCurve()
{
  this->SetNumberOfInputPorts(1);

  this->Degree = 2;
  this->PolyDataSpacing = 0.1;
  double neg[3];
  for (int i=0; i<3; i++)
  {
    neg[i] = -1.0;
  }
  this->SetStartDerivative(neg);
  this->SetEndDerivative(neg);

  this->Curve = vtkSVNURBSCurve::New();

  this->KnotSpanType       = NULL;
  this->ParametricSpanType = NULL;
}

// ----------------------
// Destructor
// ----------------------
vtkSVLoftNURBSCurve::~vtkSVLoftNURBSCurve()
{
  if (this->Curve != NULL)
  {
    this->Curve->Delete();
  }
}

// ----------------------
// RequestData
// ----------------------
int vtkSVLoftNURBSCurve::RequestData(
    vtkInformation *vtkNotUsed(request),
    vtkInformationVector **inputVector,
    vtkInformationVector *outputVector)
{
  // get the info object
  // get the ouptut
  vtkPolyData *input = vtkPolyData::GetData(inputVector[0], 0);
  vtkPolyData *output = vtkPolyData::GetData(outputVector, 0);

  if (this->KnotSpanType == NULL)
  {
    vtkErrorMacro("Need to provide knot span type");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }

  if (this->ParametricSpanType == NULL)
  {
    vtkErrorMacro("Need to provide parametric span type");
    this->SetErrorCode(vtkErrorCode::UserError + 2);
    return SV_ERROR;
  }

  if (this->LoftNURBS(input, output) != SV_OK)
  {
    vtkErrorMacro("Lofting failed!");
    this->SetErrorCode(vtkErrorCode::UserError + 3);
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVLoftNURBSCurve::PrintSelf(ostream& os,
    vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);
  os << indent << "Degree: " << this->Degree << "\n";
  os << indent << "Knot span type: " << this->KnotSpanType << "\n";
  os << indent << "Parametric values span type: " << this->ParametricSpanType << "\n";
  os << indent << "Start Derivative: " << this->StartDerivative[0] << " ";
  os << this->StartDerivative[1] << " " << this->StartDerivative[2] << "\n";
  os << indent << "End Derivative: " << this->EndDerivative[0] << " ";
  os << this->EndDerivative[1] << " " << this->EndDerivative[2] << "\n";
}

// ----------------------
// FillInputPortInformation
// ----------------------
int vtkSVLoftNURBSCurve::FillInputPortInformation(
    int port, vtkInformation *info)
{
  if (!this->Superclass::FillInputPortInformation(port, info))
    {
    return SV_ERROR;
    }
  return SV_OK;
}

// ----------------------
// LoftNURBS
// ----------------------
int vtkSVLoftNURBSCurve::LoftNURBS(vtkPolyData *input, vtkPolyData *outputPD)
{
  // Get number of control points and degree of surface
  int nCon = input->GetNumberOfPoints();
  int p    = this->Degree;

  // Get the span types
  std::string ktype = this->KnotSpanType;
  std::string ptype = this->ParametricSpanType;

  // Get the input point set u representation
  vtkNew(vtkDoubleArray, U);
  if (vtkSVNURBSUtils::GetUs(input->GetPoints(), ptype, U) != SV_OK)
    return SV_ERROR;

  // Get the knots
  vtkNew(vtkDoubleArray, knots);
  if (vtkSVNURBSUtils::GetKnots(U, p, ktype, knots) != SV_OK)
  {
    vtkErrorMacro("Error getting knots");
    return SV_ERROR;
  }

  // Set weigths to equal weighting
  vtkNew(vtkDoubleArray, weights);
  weights->SetNumberOfTuples(nCon);
  weights->FillComponent(0 , 1.0);

  // If using derivatives, set the right format for derivatives
  double D0[3], DN[3];
  if (!strncmp(ktype.c_str(), "derivative", 10))
  {
    int neg = 0;
    for (int i=0; i<3; i++)
    {
      D0[i] = this->StartDerivative[i];
      DN[i] = this->EndDerivative[i];
      if (D0[i] == -1 || DN[i] == -1)
        neg = 1;
    }
    // If non provided, set our own!
    if (neg == 1)
      this->GetDefaultDerivatives(input->GetPoints(), D0, DN);
  }

  // Get the control points, lengthy operation in vtkNURBSUtils
  vtkNew(vtkPoints, cpoints);
  if (vtkSVNURBSUtils::GetControlPointsOfCurve(input->GetPoints(), U, weights,
                                             knots, p, ktype, D0, DN, cpoints) != SV_OK)
  {
    return SV_ERROR;
  }

  // Set the output curve knots and control points
  Curve->SetKnotVector(knots);
  Curve->SetControlPoints(cpoints);

  // Generate the polydata representation finally :)
  Curve->GeneratePolyDataRepresentation(this->PolyDataSpacing);
  outputPD->DeepCopy(Curve->GetCurveRepresentation());

  return SV_OK;
}

// ----------------------
// LoftNURBS
// ----------------------
int vtkSVLoftNURBSCurve::GetDefaultDerivatives(vtkPoints *points, double D0[3], double DN[3])
{
  // Get number of points
  int n = points->GetNumberOfPoints();
  double p0[3];
  double p1[3];
  double pnm1[3];
  double pnm2[3];

  // Get points
  points->GetPoint(0, p0);
  points->GetPoint(1, p1);
  points->GetPoint(n-1,pnm1);
  points->GetPoint(n-2,pnm2);

  // Get vectors between beginning and end points
  for (int i=0; i<3; i++)
  {
    D0[i] = p1[i] - p0[i];
    DN[i] = pnm1[i] - pnm2[i];
  }

  // Normalize default vectors
  vtkMath::Normalize(D0);
  vtkMath::Normalize(DN);

  return SV_OK;
}
