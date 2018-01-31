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

#include "vtkSVNURBSUtils.h"

#include "vtkDataArray.h"
#include "vtkObjectFactory.h"
#include "vtkMath.h"
#include "vtkPoints.h"
#include "vtkSmartPointer.h"
#include "vtkSparseArray.h"
#include "vtkSVGlobals.h"
#include "vtkStructuredData.h"

#include <cassert>
#include <cmath>
#include <string>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVNURBSUtils);

// ----------------------
// Constructor
// ----------------------
vtkSVNURBSUtils::vtkSVNURBSUtils()
{
}

// ----------------------
// Destructor
// ----------------------
vtkSVNURBSUtils::~vtkSVNURBSUtils()
{
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVNURBSUtils::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);
}

// ----------------------
// LinSpace
// ----------------------
int vtkSVNURBSUtils::LinSpace(double min, double max, int num, vtkDoubleArray *result)
{
  result->SetNumberOfTuples(num);
  double div = (max-min)/(num-1);
  for (int i=0; i<num; i++)
  {
    result->SetTuple1(i, min + div*i);
  }

  return SV_OK;
}

// ----------------------
// LinSpaceClamp
// ----------------------
int vtkSVNURBSUtils::LinSpaceClamp(double min, double max, int num, int p, vtkDoubleArray *result)
{
  result->SetNumberOfTuples(num);
  int numinterior = num - 2*(p+1);
  double div = (max-min)/(numinterior+1);
  for (int i=0; i<num; i++)
  {
    if (i < numinterior + p + 1)
    {
      result->SetTuple1(i, 0.0);
    }
    else
    {
      result->SetTuple1(i, 1.0);
    }
  }
  int count = 1;
  for (int i=p+1; i<numinterior+p+1; i++)
  {
    result->SetTuple1(i, div*count);
    count++;
  }

  return SV_OK;
}

// ----------------------
// GetAvgKnots
// ----------------------
int vtkSVNURBSUtils::GetAvgKnots(double min, double max, int num, int p, vtkDoubleArray *U, vtkDoubleArray *knots)
{
  int nCon = U->GetNumberOfTuples();
  knots->SetNumberOfTuples(num);
  int numinterior = num - 2*(p+1);
  double div = (max-min)/(numinterior-1);
  for (int i=0; i<num; i++)
  {
    if (i < numinterior + p + 1)
    {
      knots->SetTuple1(i, 0.0);
    }
    else
    {
      knots->SetTuple1(i, 1.0);
    }
  }
  for (int i=1; i<nCon-p; i++)
  {
    for (int j=i; j<i+p; j++)
    {
      double val0 = knots->GetTuple1(i+p) + U->GetTuple1(j);
      knots->SetTuple1(i+p, val0);
    }
    double val1 = (1.0/p) * knots->GetTuple1(i+p);
    knots->SetTuple1(i+p, val1);
  }

  return SV_OK;
}

// ----------------------
// GetEndDerivKnots
// ----------------------
int vtkSVNURBSUtils::GetEndDerivKnots(double min, double max, int num, int p, vtkDoubleArray *U, vtkDoubleArray *knots)
{
  int nCon = U->GetNumberOfTuples();
  knots->SetNumberOfTuples(num);
  int numinterior = num - 2*(p+1);
  double div = (max-min)/(numinterior-1);
  for (int i=0; i<num; i++)
  {
    if (i < numinterior + p + 1)
    {
      knots->SetTuple1(i, 0.0);
    }
    else
    {
      knots->SetTuple1(i, 1.0);
    }
  }
  for (int i=0; i<nCon-p+1; i++)
  {
    for (int j=i; j<i+p; j++)
    {
      double val0 = knots->GetTuple1(i+p+1) + U->GetTuple1(j);
      knots->SetTuple1(i+p+1, val0);
    }
    double val1 = (1.0/p) * knots->GetTuple1(i+p+1);
    knots->SetTuple1(i+p+1, val1);
  }
  for (int i=0; i<num; i++)
  {
    if (i >= numinterior + p + 1)
    {
      knots->SetTuple1(i, 1.0);
    }
  }
  return SV_OK;
}

// ----------------------
// GetChordSpacedUs
// ----------------------
int vtkSVNURBSUtils::GetChordSpacedUs(vtkPoints *xyz, int num, vtkDoubleArray *U)
{
  double d=0;
  vtkNew(vtkDoubleArray, dists);
  dists->SetNumberOfValues(num-1);

  for (int i=1; i<num; i++)
  {
    double pt0[3], pt1[3];
    xyz->GetPoint(i-1, pt0);
    xyz->GetPoint(i, pt1);
    double dist = sqrt(pow(pt1[0] - pt0[0], 2) +
                       pow(pt1[1] - pt0[1], 2) +
                       pow(pt1[2] - pt0[2], 2));
    d += dist;
    dists->InsertTuple1(i-1, dist);
  }

  U->SetNumberOfTuples(num);
  U->SetTuple1(0, 0.0);
  double new_u = 0.0;
  for (int i=1; i<num-1; i++)
  {
    double dist = dists->GetTuple1(i-1);
    new_u += dist/d;
    U->SetTuple1(i, new_u);
  }
  U->SetTuple1(num-1, 1.0);

  return SV_OK;
}

// ----------------------
// GetCentripetalSpacedUs
// ----------------------
int vtkSVNURBSUtils::GetCentripetalSpacedUs(vtkPoints *xyz, int num, vtkDoubleArray *U)
{
  double d=0;
  vtkNew(vtkDoubleArray,dists);
  dists->SetNumberOfValues(num-1);

  for (int i=1; i<num; i++)
  {
    double pt0[3], pt1[3];
    xyz->GetPoint(i-1, pt0);
    xyz->GetPoint(i, pt1);
    double dist = sqrt(pow(pt1[0] - pt0[0], 2) +
                       pow(pt1[1] - pt0[1], 2) +
                       pow(pt1[2] - pt0[2], 2));
    d += sqrt(dist);
    dists->InsertTuple1(i-1, dist);
  }

  U->SetNumberOfTuples(num);
  U->SetTuple1(0, 0.0);
  double new_u = 0.0;
  for (int i=1; i<num-1; i++)
  {
    double dist = dists->GetTuple1(i-1);
    new_u += sqrt(dist)/d;
    U->SetTuple1(i, new_u);
  }
  U->SetTuple1(num-1, 1.0);

  return SV_OK;
}

// ----------------------
// GetUs
// ----------------------
int vtkSVNURBSUtils::GetUs(vtkPoints *xyz, std::string type, vtkDoubleArray *U)
{
  int nCon = xyz->GetNumberOfPoints();

  if (!strncmp(type.c_str(), "equal", 5))
  {
    vtkSVNURBSUtils::LinSpace(0, 1, nCon, U);
  }
  else if (!strncmp(type.c_str(), "chord", 5))
  {
    vtkSVNURBSUtils::GetChordSpacedUs(xyz, nCon, U);
  }
  else if (!strncmp(type.c_str(), "centripetal", 11))
  {
    vtkSVNURBSUtils::GetCentripetalSpacedUs(xyz, nCon, U);
  }
  else
  {
    fprintf(stderr,"Type %s is not recognized\n", type.c_str());
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// GetKnots
// ----------------------
int vtkSVNURBSUtils::GetKnots(vtkDoubleArray *U, int p, std::string type, vtkDoubleArray *knots)
{
  int nCon  = U->GetNumberOfTuples();
  int nKnot = nCon + p + 1;

  if (!strncmp(type.c_str(), "equal", 5))
  {
    vtkSVNURBSUtils::LinSpaceClamp(0, 1, nKnot, p, knots);
  }
  else if (!strncmp(type.c_str(), "average", 7))
  {
    vtkSVNURBSUtils::GetAvgKnots(0, 1, nKnot, p, U, knots);
  }
  else if (!strncmp(type.c_str(), "derivative", 10))
  {
    nKnot = nKnot + 2;
    vtkSVNURBSUtils::GetEndDerivKnots(0, 1, nKnot, p, U, knots);
  }
  else
  {
    fprintf(stderr,"Type %s is not recognized\n", type.c_str());
    return SV_ERROR;
  }
  fprintf(stdout,"Length of Knots: %lld\n", knots->GetNumberOfTuples());

  return SV_OK;
}

// ----------------------
// GetZeroBasisFunctions
// ----------------------
int vtkSVNURBSUtils::GetZeroBasisFunctions(vtkDoubleArray *U, vtkDoubleArray *knots,
                                         vtkTypedArray<double> *N0)
{
  int nCon  = U->GetNumberOfTuples();
  int nKnot = knots->GetNumberOfTuples();

  vtkNew(vtkIntArray, greater);
  vtkNew(vtkIntArray, less);
  vtkNew(vtkIntArray, spots);
  vtkNew(vtkDoubleArray, knotsShift);
  knotsShift->SetNumberOfTuples(nKnot);
  knotsShift->SetTuple1(nKnot-1, -1);
  for (int i=0; i<nKnot-1; i++)
    knotsShift->SetTuple1(i, knots->GetTuple1(i+1));

  for (int i=0; i<nCon; i++)
  {
    double val = U->GetTuple1(i);
    vtkSVNURBSUtils::WhereGreaterEqual(val, knots, greater);
    vtkSVNURBSUtils::WhereLess(val, knotsShift, less);
    vtkSVNURBSUtils::Intersect1D(greater, less, spots);
    for (int j=0; j<nKnot-1; j++)
    {
      N0->SetValue(i, j, spots->GetValue(j));
    }
  }

  return SV_OK;
}

// ----------------------
// GetPBasisFunctions
// ----------------------
int vtkSVNURBSUtils::GetPBasisFunctions(vtkDoubleArray *U, vtkDoubleArray *knots,
                                      const int p,
                                      vtkTypedArray<double> *NP)
{
  int nCon  = U->GetNumberOfTuples();
  int nKnot = knots->GetNumberOfTuples();

  //Get zero order basis function first
  vtkNew(vtkSparseArray<double>, N0);
  N0->Resize(nCon, nKnot-1);
  if (vtkSVNURBSUtils::GetZeroBasisFunctions(U, knots, N0) != SV_OK)
  {
    return SV_ERROR;
  }

  //Set original size to the size of the zero basis function set
  //The size will reduce by one each iteration through the basis until
  //the correct degree basis functions are met
  vtkNew(vtkDoubleArray, sub0);
  vtkNew(vtkDoubleArray, sub1);
  vtkNew(vtkDoubleArray, term0);
  vtkNew(vtkDoubleArray, term1);

  double **tmpN = new double*[nCon];
  for (int i=0; i<nCon; i++)
  {
    tmpN[i] = new double[nKnot-1];
    for (int j=0; j<nKnot-1; j++)
      tmpN[i][j] =  N0->GetValue(i, j);
  }

  int blength = nKnot;
  for (int i=1; i<p+1; i++)
  {
    blength -= 1;
    for (int j=0; j<blength-1; j++)
    {
      double k0 = knots->GetTuple1(i+j);
      double k1 = knots->GetTuple1(j);
      double k2 = knots->GetTuple1(i+j+1);
      double k3 = knots->GetTuple1(j+1);
      double denom0  = k0 - k1;
      double denom1  = k2 - k3;
      if (denom0 != 0.0)
      {
        vtkSVNURBSUtils::AddVal1D(U, k1, -1.0, sub0);
        vtkSVNURBSUtils::MultiplyVal1D(sub0, 1.0/(denom0), term0);
      }
      else
      {
        term0->SetNumberOfTuples(blength-1);
        term0->FillComponent(0, 0.0);
      }
      if (denom1 != 0.0)
      {
        vtkSVNURBSUtils::AddVal1D(k2, U, -1.0, sub1);
        vtkSVNURBSUtils::MultiplyVal1D(sub1, 1.0/(denom1), term1);
      }
      else
      {
        term1->SetNumberOfTuples(blength-1);
        term1->FillComponent(0, 0.0);
      }
      for (int k=0; k<nCon; k++)
      {
        double final0 = term0->GetTuple1(k) * (tmpN[k][j]);
        double final1 = term1->GetTuple1(k) * (tmpN[k][j+1]);
        tmpN[k][j] = final0 + final1;
      }
    }
  }
  NP->Resize(nCon, blength-1);
  for (int i=0; i<nCon; i++)
  {
    for (int j=0; j<blength-1; j++)
    {
      NP->SetValue(i, j, tmpN[i][j]);
    }
  }

  for (int i=0; i<nCon; i++)
    delete [] tmpN[i];
  delete [] tmpN;
  return SV_OK;
}

// ----------------------
// GetControlPointOfCurve
// ----------------------
int vtkSVNURBSUtils::GetControlPointsOfCurve(vtkPoints *points, vtkDoubleArray *U, vtkDoubleArray *weights,
                                          vtkDoubleArray *knots,
                                          const int p, std::string ktype, const double D0[3], const double DN[3],
                                          vtkPoints *cPoints)
{
  int nCon = points->GetNumberOfPoints();

  vtkNew(vtkSparseArray<double>, NPTmp);
  vtkNew(vtkSparseArray<double>, NPFinal);
  if( vtkSVNURBSUtils::GetPBasisFunctions(U, knots, p, NPTmp) != SV_OK)
  {
    return SV_ERROR;
  }
  NPTmp->SetValue(NPTmp->GetExtents()[0].GetSize()-1, NPTmp->GetExtents()[1].GetSize()-1, 1.0);

  vtkNew(vtkDenseArray<double>, pointArrayTmp);
  vtkNew(vtkDenseArray<double>, pointArrayFinal);
  vtkNew(vtkDenseArray<double>, cPointArray);
  if (vtkSVNURBSUtils::PointsToTypedArray(points, pointArrayTmp) != SV_OK)
  {
    return SV_ERROR;
  }

  if (!strncmp(ktype.c_str(), "derivative", 10))
  {
    vtkSVNURBSUtils::SetCurveEndDerivatives(NPTmp, pointArrayTmp, p, D0, DN, U, knots,
                                          NPFinal, pointArrayFinal);
  }
  else
  {
    vtkSVNURBSUtils::DeepCopy(NPTmp, NPFinal);
    vtkSVNURBSUtils::DeepCopy(pointArrayTmp, pointArrayFinal);
  }

  vtkNew(vtkSparseArray<double>, NPinv);
  if (vtkSVNURBSUtils::InvertSystem(NPFinal, NPinv) != SV_OK)
  {
    fprintf(stderr,"System could not be inverted\n");
    return SV_ERROR;
  }
  if (vtkSVNURBSUtils::MatrixVecMultiply(NPinv, 0, pointArrayFinal, 1, cPointArray) != SV_OK)
  {
    return SV_ERROR;
  }

  if (vtkSVNURBSUtils::TypedArrayToPoints(cPointArray, cPoints) != SV_OK)
  {
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// GetControlPointOfSurface
// ----------------------
int vtkSVNURBSUtils::GetControlPointsOfSurface(vtkStructuredGrid *points, vtkDoubleArray *U,
                                             vtkDoubleArray *V, vtkDoubleArray *uWeights,
                                             vtkDoubleArray *vWeights, vtkDoubleArray *uKnots,
                                             vtkDoubleArray *vKnots, const int p, const int q,
                                             std::string kutype, std::string kvtype,
                                             vtkDoubleArray *DU0, vtkDoubleArray *DUN,
                                             vtkDoubleArray *DV0, vtkDoubleArray *DVN,
                                             vtkStructuredGrid *cPoints)
{
  int dim[3];
  points->GetDimensions(dim);
  int nUCon = dim[0];
  int nVCon = dim[1];


  vtkNew(vtkSparseArray<double>, NPUTmp);
  vtkNew(vtkSparseArray<double>, NPUFinal);
  if( vtkSVNURBSUtils::GetPBasisFunctions(U, uKnots, p, NPUTmp) != SV_OK)
  {
    return SV_ERROR;
  }
  NPUTmp->SetValue(NPUTmp->GetExtents()[0].GetSize()-1, NPUTmp->GetExtents()[1].GetSize()-1, 1.0);

  vtkNew(vtkSparseArray<double>, NPVTmp);
  vtkNew(vtkSparseArray<double>, NPVFinal);
  if( vtkSVNURBSUtils::GetPBasisFunctions(V, vKnots, q, NPVTmp) != SV_OK)
  {
    return SV_ERROR;
  }
  NPVTmp->SetValue(NPVTmp->GetExtents()[0].GetSize()-1, NPVTmp->GetExtents()[1].GetSize()-1, 1.0);

  vtkNew(vtkDenseArray<double>, pointMatTmp);
  vtkNew(vtkDenseArray<double>, pointMatFinal);
  vtkSVNURBSUtils::StructuredGridToTypedArray(points, pointMatTmp);

  if (!strncmp(kvtype.c_str(), "derivative", 10)
      || !strncmp(kutype.c_str(), "derivative", 10))
  {
    vtkNew(vtkDenseArray<double>, DU0Vec);
    vtkSVNURBSUtils::DoubleArrayToTypedArray(DU0, DU0Vec);
    vtkNew(vtkDenseArray<double>, DUNVec);
    vtkSVNURBSUtils::DoubleArrayToTypedArray(DUN, DUNVec);
    vtkNew(vtkDenseArray<double>, DV0Vec);
    vtkSVNURBSUtils::DoubleArrayToTypedArray(DV0, DV0Vec);
    vtkNew(vtkDenseArray<double>, DVNVec);
    vtkSVNURBSUtils::DoubleArrayToTypedArray(DVN, DVNVec);
    vtkSVNURBSUtils::SetSurfaceEndDerivatives(NPUTmp, NPVTmp, pointMatTmp, p, q,
                                            kutype, kvtype,
                                            DU0Vec, DUNVec, DV0Vec, DVNVec, U, V,
                                            uKnots, vKnots,
                                            NPUFinal, NPVFinal, pointMatFinal);
  }
  else
  {
    vtkSVNURBSUtils::DeepCopy(NPUTmp, NPUFinal);
    vtkSVNURBSUtils::DeepCopy(NPVTmp, NPVFinal);
    vtkSVNURBSUtils::DeepCopy(pointMatTmp, pointMatFinal);
  }

  //fprintf(stdout,"Basis functions U:\n");
  //vtkSVNURBSUtils::PrintMatrix(NPUFinal);
  vtkNew(vtkSparseArray<double>, NPUinv);
  if (vtkSVNURBSUtils::InvertSystem(NPUFinal, NPUinv) != SV_OK)
  {
    fprintf(stderr,"System could not be inverted\n");
    return SV_ERROR;
  }

  //fprintf(stdout,"Basis functions V:\n");
  //vtkSVNURBSUtils::PrintMatrix(NPVFinal);
  vtkNew(vtkSparseArray<double>, NPVinv);
  if (vtkSVNURBSUtils::InvertSystem(NPVFinal, NPVinv) != SV_OK)
  {
    fprintf(stderr,"System could not be inverted\n");
    return SV_ERROR;
  }


  //fprintf(stdout,"Inverted system U:\n");
  //vtkSVNURBSUtils::PrintMatrix(NPUinv);
  //fprintf(stdout,"Inverted system V:\n");
  //vtkSVNURBSUtils::PrintMatrix(NPVinv);
  vtkNew(vtkDenseArray<double>, tmpUGrid);
  if (vtkSVNURBSUtils::MatrixMatrixMultiply(NPUinv, 0, pointMatFinal, 1, tmpUGrid) != SV_OK)
  {
    fprintf(stderr, "Error in matrix multiply\n");
    return SV_ERROR;
  }
  vtkNew(vtkDenseArray<double>, tmpUGridT);
  vtkSVNURBSUtils::MatrixTranspose(tmpUGrid, 1, tmpUGridT);
  vtkNew(vtkDenseArray<double>, tmpVGrid);
  if (vtkSVNURBSUtils::MatrixMatrixMultiply(NPVinv, 0, tmpUGridT, 1, tmpVGrid) != SV_OK)
  {
    fprintf(stderr, "Error in matrix multiply\n");
    return SV_ERROR;
  }

  vtkNew(vtkPoints, finalPoints);
  cPoints->SetPoints(finalPoints);
  vtkNew(vtkDenseArray<double>, tmpVGridT);
  vtkSVNURBSUtils::MatrixTranspose(tmpVGrid, 1, tmpVGridT);
  vtkSVNURBSUtils::TypedArrayToStructuredGrid(tmpVGridT, cPoints);
  //fprintf(stdout,"Final structured grid of control points\n");
  //vtkSVNURBSUtils::PrintStructuredGrid(cPoints);

  return SV_OK;
}

// ----------------------
// GetCurveEndDerivatives
// ----------------------
int vtkSVNURBSUtils::SetCurveEndDerivatives(vtkTypedArray<double> *NP, vtkTypedArray<double> *points,
		                          const int p, const double D0[3],
                                          const double DN[3], vtkDoubleArray *U, vtkDoubleArray *knots,
                                          vtkTypedArray<double> *newNP, vtkTypedArray<double> *newPoints)
{
  vtkSVNURBSUtils::AddDerivativeRows(NP, newNP, p, knots);

  vtkSVNURBSUtils::AddDerivativePoints(points, p, D0, DN, U, knots, newPoints);

  return SV_OK;
}

// ----------------------
// AddDerivativePoints
// ----------------------
int vtkSVNURBSUtils::AddDerivativePoints(vtkTypedArray<double> *points,
		                       const int p, const double D0[3],
                                       const double DN[3], vtkDoubleArray *U, vtkDoubleArray *knots,
                                       vtkTypedArray<double> *newPoints)
{
  int nKnot = knots->GetNumberOfTuples();
  int n = nKnot - (p + 1);
  newPoints->Resize(n, 3);

  //Add extra derivative in points
  double d0val = U->GetTuple1(p+1)/p;
  double dNval = (1 - U->GetTuple1(n - p - 4))/p;

  //Set first spot
  for (int i=0; i<3; i++)
  {
    double val = points->GetValue(0, i);
    newPoints->SetValue(0, i, val);
  }

  //Set SPECIAL second spot
  for (int i=0; i<3; i++)
  {
    double val = d0val * D0[i];
    newPoints->SetValue(1, i, val);
  }

  //Set rest of matrix
  for (int i=2; i<n-2; i++)
  {
    for (int j=0; j<3; j++)
    {
      double val = points->GetValue(i-1, j);
      newPoints->SetValue(i, j, val);
    }
  }

  //Set SPECIAL second to last row
  for (int i=0 ; i<3; i++)
  {
    double val = dNval *DN[i];
    newPoints->SetValue(n-2, i, val);
  }

  //Set last row
  for (int i=0; i<3; i++)
  {
    double val = points->GetValue(n - 3, i);
    newPoints->SetValue(n - 1, i, val);
  }

  return SV_OK;
}

// ----------------------
// SetSurfaceEndDerivatives
// ----------------------
int vtkSVNURBSUtils::SetSurfaceEndDerivatives(vtkTypedArray<double> *NPU, vtkTypedArray<double> *NPV,
                                            vtkTypedArray<double> *points,
		                            const int p, const int q,
                                            std::string kutype, std::string kvtype,
                                            vtkTypedArray<double> *DU0, vtkTypedArray<double> *DUN,
                                            vtkTypedArray<double> *DV0, vtkTypedArray<double> *DVN,
                                            vtkDoubleArray *U, vtkDoubleArray *V,
                                            vtkDoubleArray *uKnots, vtkDoubleArray *vKnots,
                                            vtkTypedArray<double> *newNPU, vtkTypedArray<double> *newNPV,
                                            vtkTypedArray<double> *newPoints)
{
  if (!strncmp(kutype.c_str(), "derivative", 10))
  {
    vtkSVNURBSUtils::AddDerivativeRows(NPU, newNPU, p, uKnots);
  }
  else
  {
    vtkSVNURBSUtils::DeepCopy(NPU, newNPU);
  }
  if (!strncmp(kvtype.c_str(), "derivative", 10))
  {
    vtkSVNURBSUtils::AddDerivativeRows(NPV, newNPV, q, vKnots);
  }
  else
  {
    vtkSVNURBSUtils::DeepCopy(NPV, newNPV);
  }

  int nUKnot = uKnots->GetNumberOfTuples();
  int nVKnot = vKnots->GetNumberOfTuples();
  int nu = nUKnot - (p + 1);
  int nv = nVKnot - (q + 1);
  newPoints->Resize(nu, nv, 3);

  int npu = points->GetExtents()[0].GetSize();
  int npv = points->GetExtents()[1].GetSize();
  vtkNew(vtkDenseArray<double>, tmp0Points);
  vtkNew(vtkDenseArray<double>, tmp1Points);
  vtkNew(vtkDenseArray<double>, tmp2Points);
  vtkNew(vtkDenseArray<double>, tmp3Points);
  vtkNew(vtkDenseArray<double>, tmp4Points);
  if (!strncmp(kutype.c_str(), "derivative", 10))
  {
    tmp2Points->Resize(nu, npv, 3);
    for (int i=0; i<npv; i++)
    {
      vtkSVNURBSUtils::GetMatrixComp(points, i, 0, 1, tmp0Points);
      double du0[3], duN[3];
      for (int j=0; j<3; j++)
      {
        du0[j] = DU0->GetValue(i, j);
        duN[j] = DUN->GetValue(i, j);
      }
      vtkSVNURBSUtils::AddDerivativePoints(tmp0Points, p, du0, duN, U, uKnots, tmp1Points);
      vtkSVNURBSUtils::SetMatrixComp(tmp1Points, i, 0, 1, tmp2Points);
    }
    npu += 2;
  }
  else
  {
    vtkSVNURBSUtils::DeepCopy(points, tmp2Points);
  }

  if (!strncmp(kvtype.c_str(), "derivative", 10))
  {
    int count = 0;
    for (int i=0; i<npu; i++)
    {
      double dv0[3], dvN[3];
      if ((i == 1 || i == nu - 2) && !strncmp(kutype.c_str(), "derivative", 10))
      {
        for (int j=0; j<3; j++)
        {
          dv0[j] = 0.0;
          dvN[j] = 0.0;
        }
      }
      else
      {
        for (int j=0; j<3; j++)
        {
          dv0[j] = DV0->GetValue(count, j);
          dvN[j] = DVN->GetValue(count, j);
        }
        count++;
      }
      vtkSVNURBSUtils::GetMatrixComp(tmp2Points, i, 1, 1, tmp3Points);
      vtkSVNURBSUtils::AddDerivativePoints(tmp3Points, q, dv0, dvN, V, vKnots, tmp4Points);
      vtkSVNURBSUtils::SetMatrixComp(tmp4Points, i, 1, 1, newPoints);
    }
  }
  else
  {
    vtkSVNURBSUtils::DeepCopy(tmp2Points, newPoints);
  }

  return SV_OK;
}

// ----------------------
// AddDerivativeRows
// ----------------------
int vtkSVNURBSUtils::AddDerivativeRows(vtkTypedArray<double> *NP, vtkTypedArray<double> *newNP,
                                     const int p, vtkDoubleArray *knots)
{
  int nKnot = knots->GetNumberOfTuples();
  int n = nKnot - (p + 1);
  newNP->Resize(n, n);

  //Set first row
  for (int i=0; i<n; i++)
  {
    double val = NP->GetValue(0, i);
    newNP->SetValue(0, i, val);
  }

  //Set SPECIAL second row
  newNP->SetValue(1, 0, -1.0);
  newNP->SetValue(1, 1, 1.0);

  //Set the center of the matrix:
  for (int i=2; i<n-2; i++)
  {
    for (int j=0; j<n; j++)
    {
      double val = NP->GetValue(i-1, j);
      newNP->SetValue(i, j, val);
    }
  }

  //Set SPECIAL second to last row
  newNP->SetValue(n-2, n-2, -1.0);
  newNP->SetValue(n-2, n-1, 1.0);

  //Set last row
  for (int i=0; i<n; i++)
  {
    double val = NP->GetValue(n-3, i);
    newNP->SetValue(n-1, i, val);
  }

  return SV_OK;
}

// ----------------------
// DeepCopy
// ----------------------
int vtkSVNURBSUtils::DeepCopy(vtkTypedArray<double> *input, vtkTypedArray<double> *output)
{
  int dims = input->GetDimensions();
  int dim[3];
  for (int i=0; i<dims; i++)
  {
    dim[i] = input->GetExtents()[i].GetSize();
  }
  if (dims == 1)
  {
    output->Resize(dim[0]);
  }
  else if (dims == 2)
  {
    output->Resize(dim[0], dim[1]);
  }
  else if (dims == 3)
  {
    output->Resize(dim[0], dim[1], dim[2]);
  }

  for (int i=0; i<dim[0]; i++)
  {
    if (dims == 1)
    {
      double val = input->GetValue(i);
      output->SetValue(i, val);
    }
    else
    {
      for (int j=0; j<dim[1]; j++)
      {
        if (dims == 2)
        {
          double val = input->GetValue(i,j);
          output->SetValue(i, j, val);
        }
        else
        {
          for (int k=0; k<dim[2]; k++)
          {
            double val = input->GetValue(i, j, k);
            output->SetValue(i, j, k, val);
          }
        }
      }
    }
  }

  return SV_OK;
}

// ----------------------
// InvertSystem
// ----------------------
int vtkSVNURBSUtils::InvertSystem(vtkTypedArray<double> *NP, vtkTypedArray<double> *NPinv)
{
  int nr = NP->GetExtents()[0].GetSize();
  int nc = NP->GetExtents()[1].GetSize();
  if (nr != nc)
  {
    fprintf(stderr,"Matrix is not square, can't invert\n");
    return SV_ERROR;
  }

  double **inMat = new double*[nr];
  double **outMat = new double*[nr];
  for (int i=0; i<nr; i++)
  {
    inMat[i]  = new double[nc];
    outMat[i]  = new double[nc];
  }

  for (int i=0; i<nr; i++)
  {
    for (int j=0; j<nc; j++)
    {
      inMat[i][j] = NP->GetValue(i, j);
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
    return SV_ERROR;
  }

  NPinv->Resize(nr, nc);
  for (int i=0; i<nr; i++)
  {
    for (int j=0; j<nc; j++)
    {
      NPinv->SetValue(i, j, outMat[i][j]);
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

// ----------------------
// BasisEvaluation
// ----------------------
int vtkSVNURBSUtils::BasisEvaluation(vtkDoubleArray *knots, int p, int kEval, double uEval,
                                   vtkDoubleArray *Nu)
{
  Nu->SetNumberOfTuples(p+2);

  double *uLeft  = new double[p+1];
  double *uRight = new double[p+1];
  for (int i=0; i<p+1; i++)
  {
    Nu->SetTuple1(i, 0.0);
  }
  Nu->SetTuple1(0, 1.0);

  for (int i=1; i<p+1; i++)
  {
    uLeft[i]  = uEval - knots->GetTuple1(kEval+1-i);
    uRight[i] = knots->GetTuple1(kEval+i) - uEval;
    double saved = 0.0;
    for (int j=0; j<i; j++)
    {
      double temp = Nu->GetTuple1(j) / (uRight[j+1] + uLeft[i+j]);
      Nu->SetTuple1(j, saved + uRight[j+1]*temp);
      saved = uLeft[i-j]*temp;
    }
    Nu->SetTuple1(i, saved);
  }

  delete [] uLeft;
  delete [] uRight;

  return SV_OK;
}

// ----------------------
// BasisEvaluationVec
// ----------------------
int vtkSVNURBSUtils::BasisEvaluationVec(vtkDoubleArray *knots, int p, int kEval, vtkDoubleArray *uEvals,
                                      vtkTypedArray<double> *Nus)
{
  int nU    = uEvals->GetNumberOfTuples();
  int nKnot = knots->GetNumberOfTuples();
  int nCon  = nKnot - p - 1;

  vtkNew(vtkIntArray, less);
  vtkNew(vtkIntArray, greater);
  vtkNew(vtkIntArray, fillspots);
  for (int i=0; i<p+1; i++)
  {
    vtkSVNURBSUtils::WhereLessEqual(knots->GetTuple1(kEval+i), uEvals, less);
    vtkSVNURBSUtils::WhereGreater(knots->GetTuple1(kEval+i+1), uEvals, greater);
    vtkSVNURBSUtils::Intersect1D(greater, less, fillspots);
    for (int j=0; j<nU; j++)
    {
      Nus->SetValue(j, i, fillspots->GetTuple1(j));
    }
  }

  vtkNew(vtkDoubleArray, saved);
  vtkNew(vtkDoubleArray, uRights);
  vtkNew(vtkDoubleArray, uLefts);
  vtkNew(vtkDoubleArray, tempVec);
  saved->SetNumberOfTuples(nU);
  tempVec->SetNumberOfTuples(nU);
  for (int i=1; i<p+1; i++)
  {
    double denom = knots->GetTuple1(kEval + i) - knots->GetTuple1(kEval);
    for (int j=0; j<nU; j++)
    {
      if (Nus->GetValue(j, 0) != 0.0)
      {
        double numer = (uEvals->GetTuple1(j) - knots->GetTuple1(kEval)) * Nus->GetValue(j, 0);
        saved->SetTuple1(j, numer/denom);
      }
      else
      {
        saved->SetTuple1(j, 0.0);
      }
    }
    for (int j=0; j<p-i+1; j++)
    {
      double uLeft  = knots->GetTuple1(kEval+j+1);
      double uRight = knots->GetTuple1(kEval+i+j+1);
      vtkSVNURBSUtils::AddVal1D(uRight, uEvals, -1.0, uRights);
      vtkSVNURBSUtils::AddVal1D(uEvals, uLeft, -1.0, uLefts);
      for (int k=0; k<nU; k++)
      {
        if (Nus->GetValue(k, j+1) != 0.0)
        {
          double temp = (Nus->GetValue(k, j+1)) / (uRight - uLeft);
          tempVec->SetTuple1(k, temp);
        }
        else
        {
          tempVec->SetTuple1(k, -1);
        }
      }
      for (int k=0; k<nU; k++)
      {
        double temp = tempVec->GetTuple1(k);
        if (temp != -1)
        {
          double newVal = saved->GetTuple1(k) + (uRights->GetTuple1(k)*temp);
          Nus->SetValue(k, j, newVal);
          saved->SetTuple1(k, uLefts->GetTuple1(k)*temp);
        }
        else
        {
          Nus->SetValue(k, j, saved->GetTuple1(k));
          saved->SetTuple1(k, 0.0);
        }
      }
    }
  }


  return SV_OK;
}

// ----------------------
// FindSpan
// ----------------------
int vtkSVNURBSUtils::FindSpan(int p, double u, vtkDoubleArray *knots, int &span)
{
  int nKnot = knots->GetNumberOfTuples();
  int nCon = nKnot - p - 1;

  if (u == knots->GetTuple1(nCon))
  {
    span = nCon - 1;
    return SV_OK;
  }
  int low = p;
  int high = nCon;
  int mid = (low+high)/2;

  while (u < knots->GetTuple1(mid) || u >= knots->GetTuple1(mid+1))
  {
    if (u <knots->GetTuple1(mid))
      high = mid;
    else
      low = mid;
    mid = (low+high)/2;
  }
  span = mid;
  return SV_OK;
}

// ----------------------
// FindSpan
// ----------------------
int vtkSVNURBSUtils::GetMultiplicity(vtkDoubleArray *array, vtkIntArray *multiplicity,
                                     vtkDoubleArray *singleValues)
{
  // Reset the return arrays
  multiplicity->Reset();
  singleValues->Reset();

  // Number of values
  int numVals = array->GetNumberOfTuples();
  if (numVals == 0)
    return SV_ERROR;

  // Loop through values
  for (int i=0; i<numVals-1; i++)
  {
    int count = 1;
    while(array->GetTuple1(i+1) == array->GetTuple1(i) && i<numVals)
    {
      count++;
      i++;
    }

    // Update mults
    multiplicity->InsertNextTuple1(count);
    singleValues->InsertNextTuple1(array->GetTuple1(i));
  }

  // Number of mult vals
  int numMults = singleValues->GetNumberOfTuples();

  // Set the last spot of the array if needed
  if (array->GetTuple1(numVals-1) != singleValues->GetTuple1(numMults-1)){
    // Add new mult because not equal
    multiplicity->InsertNextTuple1(1);
    singleValues->InsertNextTuple1(array->GetTuple1(numVals-1));
  }

  return SV_OK;
}

// ----------------------
// MatrixPointsMultiply
// ----------------------
int vtkSVNURBSUtils::MatrixPointsMultiply(vtkTypedArray<double>* mat, vtkPoints *pointVec, vtkPoints *output)
{

  int nr = mat->GetExtents()[0].GetSize();
  int nc = mat->GetExtents()[1].GetSize();
  if (nc != pointVec->GetNumberOfPoints())
  {
    fprintf(stderr,"Matrix vector dimensions do not match\n");
    fprintf(stderr,"Matrix: %d by %d, Vec: %lld\n", nr, nc, pointVec->GetNumberOfPoints());
    return SV_ERROR;
  }

  vtkNew(vtkPoints, tmpPoints);
  tmpPoints->SetNumberOfPoints(nr);
  for (int i=0; i<nr; i++)
  {
    double updatePt[3];
    for (int j=0; j<3; j++)
    {
      updatePt[j] = 0.0;
    }
    for (int j=0; j<nc; j++)
    {
      double newPt[3];
      double bVal = mat->GetValue(i, j);
      pointVec->GetPoint(j, newPt);
      for (int k=0; k<3; k++)
      {
        updatePt[k] += newPt[k] * bVal;
      }
    }
    tmpPoints->SetPoint(i, updatePt);
  }
  output->DeepCopy(tmpPoints);

  return SV_OK;
}

// ----------------------
// MatrixVecMultiply
// ----------------------
int vtkSVNURBSUtils::MatrixVecMultiply(vtkTypedArray<double>* mat, const int matIsPoints,
                                     vtkTypedArray<double> *vec, const int vecIsPoints,
                                     vtkTypedArray<double> *output)
{

  int nrM = mat->GetExtents()[0].GetSize();
  int ncM = mat->GetExtents()[1].GetSize();
  if (matIsPoints && mat->GetExtents()[2].GetSize() != 3)
  {
    fprintf(stderr,"Third dimension of matrix should contain xyz coordinates, but doesn't!\n");
    return SV_ERROR;
  }

  int nrV = vec->GetExtents()[0].GetSize();
  if (vecIsPoints && vec->GetExtents()[1].GetSize() != 3)
  {
    fprintf(stderr,"Second dimension of vector should contain xyz coordinates, but doesn't!\n");
    return SV_ERROR;
  }

  if (ncM != nrV)
  {
    fprintf(stderr,"Matrix vector dimensions do not match\n");
    fprintf(stderr,"Matrix: %d by %d, Vec: %d\n", nrM, ncM, nrV);
    return SV_ERROR;
  }

  int either = 0;
  output->Resize(nrM);
  if (matIsPoints)
  {
    either = 1;
    output->Resize(nrM, 3);
  }
  if (vecIsPoints)
  {
    either = 1;
    output->Resize(nrM, 3);
  }

  for (int i=0; i<nrM; i++)
  {
    double updateVal[3];
    for (int j=0; j<3; j++)
    {
      updateVal[j] = 0.0;
    }
    for (int j=0; j<ncM; j++)
    {
      double matVal[3];
      double vecVal[3];
      for (int k=0; k<3; k++)
      {
        if (matIsPoints)
        {
          matVal[k] = mat->GetValue(i, j, k);
        }
        else
        {
          matVal[k] = mat->GetValue(i, j);
        }
        if (vecIsPoints)
        {
          vecVal[k] = vec->GetValue(j, k);
        }
        else
        {
          vecVal[k] = vec->GetValue(j);
        }
      }
      for (int k=0; k<3; k++)
      {
        updateVal[k] += matVal[k] * vecVal[k];
      }
    }
    if (either == 1)
    {
      for (int j=0; j<3; j++)
      {
        output->SetValue(i, j, updateVal[j]);
      }
    }
    else
    {
      output->SetValue(i, updateVal[0]);
    }
  }

  return SV_OK;
}

// ----------------------
// MatrixMatrixMultiply
// ----------------------
int vtkSVNURBSUtils::MatrixMatrixMultiply(vtkTypedArray<double> *mat0, const int mat0IsPoints,
                                        vtkTypedArray<double> *mat1, const int mat1IsPoints,
                                        vtkTypedArray<double> *output)
{
  int nrM0 = mat0->GetExtents()[0].GetSize();
  int ncM0 = mat0->GetExtents()[1].GetSize();
  if (mat0IsPoints && mat0->GetExtents()[2].GetSize() != 3)
  {
    fprintf(stderr,"Third dimension of matrix should contain xyz coordinates, but doesn't!\n");
    return SV_ERROR;
  }

  int nrM1 = mat1->GetExtents()[0].GetSize();
  int ncM1 = mat1->GetExtents()[1].GetSize();
  if (mat1IsPoints && mat1->GetExtents()[2].GetSize() != 3)
  {
    fprintf(stderr,"Third dimension of matrix should contain xyz coordinates, but doesn't!\n");
    return SV_ERROR;
  }

  if (ncM0 != nrM1)
  {
    fprintf(stderr,"Matrix matrix dimensions do not match\n");
    fprintf(stderr,"Matrix 0: %d by %d, Matrix 1: %d by %d\n", nrM0, ncM0, nrM1, ncM1);
    return SV_ERROR;
  }

  int either = 0;
  if (mat0IsPoints || mat1IsPoints)
  {
    either = 1;
    output->Resize(nrM0, ncM1, 3);
  }
  else
  {
    output->Resize(nrM0, ncM1);
  }

  if (!mat0IsPoints && !mat1IsPoints)
  {
    vtkSVNURBSUtils::MatrixMatrixForDGEMM(mat0, mat1, output);
  }
  else if (mat0IsPoints && mat1IsPoints)
  {
    vtkSVNURBSUtils::PointMatrixPointMatrixForDGEMM(mat0, mat1, output);
  }
  else if (mat0IsPoints)
  {
    vtkSVNURBSUtils::PointMatrixMatrixForDGEMM(mat0, mat1, output);
  }
  else
  {
    vtkSVNURBSUtils::MatrixPointMatrixForDGEMM(mat0, mat1, output);
  }

  return SV_OK;
}

// ----------------------
// MatrixMatrixForDGEMM
// ----------------------
int vtkSVNURBSUtils::MatrixMatrixForDGEMM(vtkTypedArray<double> *mat0,
                                        vtkTypedArray<double> *mat1,
                                        vtkTypedArray<double> *output)
{
  int nrM0 = mat0->GetExtents()[0].GetSize();
  int ncM0 = mat0->GetExtents()[1].GetSize();
  int nrM1 = mat1->GetExtents()[0].GetSize();
  int ncM1 = mat1->GetExtents()[1].GetSize();

  if (ncM0 != nrM1)
  {
    fprintf(stderr,"Matrix matrix dimensions do not match\n");
    fprintf(stderr,"Matrix 0: %d by %d, Matrix 1: %d by %d\n", nrM0, ncM0, nrM1, ncM1);
    return SV_ERROR;
  }

  double *mat0Vec = new double[nrM0*ncM0];
  double *mat1Vec = new double[nrM1*ncM1];
  double *outVec  = new double[nrM0*ncM1];

  vtkSVNURBSUtils::MatrixToVector(mat0, mat0Vec);
  vtkSVNURBSUtils::MatrixToVector(mat1, mat1Vec);
  if (vtkSVNURBSUtils::DGEMM(mat0Vec, nrM0, ncM0,
                           mat1Vec, nrM1, ncM1, outVec) != SV_OK)
  {
    delete [] mat0Vec;
    delete [] mat1Vec;
    delete [] outVec;
    return SV_ERROR;
  }
  vtkSVNURBSUtils::VectorToMatrix(outVec, nrM0, ncM1, output);

  delete [] mat0Vec;
  delete [] mat1Vec;
  delete [] outVec;

  return SV_OK;
}

// ----------------------
// PointMatrixMatrixForDGEMM
// ----------------------
int vtkSVNURBSUtils::PointMatrixPointMatrixForDGEMM(vtkTypedArray<double> *mat0,
                                             vtkTypedArray<double> *mat1,
                                             vtkTypedArray<double> *output)
{
  int nrM0 = mat0->GetExtents()[0].GetSize();
  int ncM0 = mat0->GetExtents()[1].GetSize();
  int nrM1 = mat1->GetExtents()[0].GetSize();
  int ncM1 = mat1->GetExtents()[1].GetSize();
  if (mat0->GetExtents()[2].GetSize() != 3)
  {
    fprintf(stderr,"Third dimension of matrix should contain xyz coordinates, but doesn't!\n");
    return SV_ERROR;
  }

  if (ncM0 != nrM1)
  {
    fprintf(stderr,"Matrix matrix dimensions do not match\n");
    fprintf(stderr,"Matrix 0: %d by %d, Matrix 1: %d by %d\n", nrM0, ncM0, nrM1, ncM1);
    return SV_ERROR;
  }

  double *mat0Vecs[3], *mat1Vecs[3], *outVecs[3];
  for (int i=0; i<3; i++)
  {
    mat0Vecs[i] = new double[nrM0*ncM0];
    mat1Vecs[i] = new double[nrM1*ncM1];
    outVecs[i]  = new double[nrM0*ncM1];
  }
  vtkSVNURBSUtils::PointMatrixToVectors(mat0, mat0Vecs);
  vtkSVNURBSUtils::PointMatrixToVectors(mat1, mat1Vecs);
  for (int i=0; i<3; i++)
  {
    if (vtkSVNURBSUtils::DGEMM(mat0Vecs[i], nrM0, ncM0,
                             mat1Vecs[i], nrM1, ncM1, outVecs[i]) != SV_OK)
    {
      for (int i=0; i<3; i++)
      {
        delete [] mat0Vecs[i];
        delete [] mat1Vecs[i];
        delete [] outVecs[i];
      }
      return SV_ERROR;
    }
  }
  vtkSVNURBSUtils::VectorsToPointMatrix(outVecs, nrM0, ncM1, output);

  for (int i=0; i<3; i++)
  {
    delete [] mat0Vecs[i];
    delete [] mat1Vecs[i];
    delete [] outVecs[i];
  }

  return SV_OK;
}

// ----------------------
// PointMatrixMatrixForDGEMM
// ----------------------
int vtkSVNURBSUtils::PointMatrixMatrixForDGEMM(vtkTypedArray<double> *mat0,
                                             vtkTypedArray<double> *mat1,
                                             vtkTypedArray<double> *output)
{
  int nrM0 = mat0->GetExtents()[0].GetSize();
  int ncM0 = mat0->GetExtents()[1].GetSize();
  int nrM1 = mat1->GetExtents()[0].GetSize();
  int ncM1 = mat1->GetExtents()[1].GetSize();
  if (mat0->GetExtents()[2].GetSize() != 3)
  {
    fprintf(stderr,"Third dimension of matrix should contain xyz coordinates, but doesn't!\n");
    return SV_ERROR;
  }

  if (ncM0 != nrM1)
  {
    fprintf(stderr,"Matrix matrix dimensions do not match\n");
    fprintf(stderr,"Matrix 0: %d by %d, Matrix 1: %d by %d\n", nrM0, ncM0, nrM1, ncM1);
    return SV_ERROR;
  }

  double *mat1Vec = new double[nrM1*ncM1];
  double *mat0Vecs[3], *outVecs[3];
  for (int i=0; i<3; i++)
  {
    mat0Vecs[i] = new double[nrM0*ncM0];
    outVecs[i]  = new double[nrM0*ncM1];
  }
  vtkSVNURBSUtils::PointMatrixToVectors(mat0, mat0Vecs);
  vtkSVNURBSUtils::MatrixToVector(mat1, mat1Vec);
  for (int i=0; i<3; i++)
  {
    if (vtkSVNURBSUtils::DGEMM(mat0Vecs[i], nrM0, ncM0,
                             mat1Vec, nrM1, ncM1, outVecs[i]) != SV_OK)
    {
      delete [] mat1Vec;
      for (int i=0; i<3; i++)
      {
        delete [] mat0Vecs[i];
        delete [] outVecs[i];
      }
      return SV_ERROR;
    }
  }
  vtkSVNURBSUtils::VectorsToPointMatrix(outVecs, nrM0, ncM1, output);

  delete [] mat1Vec;
  for (int i=0; i<3; i++)
  {
    delete [] mat0Vecs[i];
    delete [] outVecs[i];
  }

  return SV_OK;
}

// ----------------------
// MatrixPointMatrixForDGEMM
// ----------------------
int vtkSVNURBSUtils::MatrixPointMatrixForDGEMM(vtkTypedArray<double> *mat0,
                                       vtkTypedArray<double> *mat1,
                                       vtkTypedArray<double> *output)
{
  int nrM0 = mat0->GetExtents()[0].GetSize();
  int ncM0 = mat0->GetExtents()[1].GetSize();
  int nrM1 = mat1->GetExtents()[0].GetSize();
  int ncM1 = mat1->GetExtents()[1].GetSize();
  if (mat1->GetExtents()[2].GetSize() != 3)
  {
    fprintf(stderr,"Third dimension of matrix should contain xyz coordinates, but doesn't!\n");
    return SV_ERROR;
  }

  if (ncM0 != nrM1)
  {
    fprintf(stderr,"Matrix matrix dimensions do not match\n");
    fprintf(stderr,"Matrix 0: %d by %d, Matrix 1: %d by %d\n", nrM0, ncM0, nrM1, ncM1);
    return SV_ERROR;
  }

  double *mat0Vec = new double[nrM0*ncM0];
  double *mat1Vecs[3], *outVecs[3];
  for (int i=0; i<3; i++)
  {
    mat1Vecs[i] = new double[nrM1*ncM1];
    outVecs[i]  = new double[nrM0*ncM1];
  }
  vtkSVNURBSUtils::MatrixToVector(mat0, mat0Vec);
  vtkSVNURBSUtils::PointMatrixToVectors(mat1, mat1Vecs);
  for (int i=0; i<3; i++)
  {
    if (vtkSVNURBSUtils::DGEMM(mat0Vec, nrM0, ncM0,
                             mat1Vecs[i], nrM1, ncM1, outVecs[i]) != SV_OK)
    {
      delete [] mat0Vec;
      for (int i=0; i<3; i++)
      {
        delete [] mat1Vecs[i];
        delete [] outVecs[i];
      }
      return SV_ERROR;
    }
  }
  vtkSVNURBSUtils::VectorsToPointMatrix(outVecs, nrM0, ncM1, output);

  delete [] mat0Vec;
  for (int i=0; i<3; i++)
  {
    delete [] mat1Vecs[i];
    delete [] outVecs[i];
  }

  return SV_OK;
}

// ----------------------
// GetMatrixComp
// ----------------------
int vtkSVNURBSUtils::GetMatrixComp(vtkTypedArray<double> *mat,  const int loc, const int comp, const int matIsPoints, vtkTypedArray<double> *vec)
{
  int numVals = mat->GetExtents()[comp].GetSize();
  int check = mat->GetExtents()[2].GetSize();
  if (matIsPoints && check != 3)
  {
    fprintf(stderr,"Third dimension of matrix should contain xyz coordinates\n");
    return SV_ERROR;
  }

  if (matIsPoints)
  {
    vec->Resize(numVals, 3);
  }
  else
  {
    vec->Resize(numVals);
  }
  for (int i=0; i<numVals; i++)
  {
    double val[3];
    if (comp == 0)
    {
      if (matIsPoints)
      {
        for (int j=0; j<3; j++)
        {
          val[j] = mat->GetValue(i, loc, j);
        }
      }
      else
      {
        val[0] = mat->GetValue(i, loc);
      }
    }
    else if (comp == 1)
    {
      if (matIsPoints)
      {
        for (int j=0; j<3; j++)
        {
          val[j] = mat->GetValue(loc, i, j);
        }
      }
      else
      {
        val[0] = mat->GetValue(loc, i);
      }
    }
    if (matIsPoints)
    {
      for (int j=0; j<3; j++)
      {
        vec->SetValue(i, j, val[j]);
      }
    }
    else
    {
      vec->SetValue(i, val[0]);
    }
  }

  return SV_OK;
}

// ----------------------
// SetMatrixComp
// ----------------------
int vtkSVNURBSUtils::SetMatrixComp(vtkTypedArray<double> *vec,  const int loc, const int comp, const int matIsPoints, vtkTypedArray<double> *mat)
{
  int numVals = vec->GetExtents()[0].GetSize();
  int dim[3];
  int cSize = mat->GetExtents()[comp].GetSize();
  int check = mat->GetExtents()[2].GetSize();
  if (cSize != numVals)
  {
    fprintf(stderr,"Number of values in array and component of matrix are not equal\n");
    fprintf(stderr,"Size of Matrix in comp direction: %d\n", cSize);
    fprintf(stderr,"Size of Vector: %d\n", numVals);
    return SV_ERROR;
  }
  if (matIsPoints && check != 3)
  {
    fprintf(stderr,"Third dimension of matrix should contain xyz coordinates\n");
    return SV_ERROR;
  }

  for (int i=0; i<numVals; i++)
  {
    double val[3];
    if (matIsPoints)
    {
      for (int j=0 ;j<3; j++)
        val[j] = vec->GetValue(i, j);
    }
    else
      val[0] = vec->GetValue(i);
    if (comp == 0)
    {
      if (matIsPoints)
      {
        for (int j=0; j<3; j++)
          mat->SetValue(i, loc, j, val[j]);
      }
      else
        mat->SetValue(i, loc, val[0]);
    }
    else if (comp == 1)
    {
      if (matIsPoints)
      {
        for (int j=0; j<3; j++)
          mat->SetValue(loc, i, j, val[j]);
      }
      else
        mat->SetValue(loc, i, val[0]);
    }
  }

  return SV_OK;
}

// ----------------------
// StructuredGridToTypedArray
// ----------------------
int vtkSVNURBSUtils::StructuredGridToTypedArray(vtkStructuredGrid *grid, vtkTypedArray<double> *output)
{
  int dim[3];
  grid->GetDimensions(dim);

  if (dim[2] != SV_OK)
  {
    fprintf(stderr,"3 Dimensions are not yet supported\n");
    return SV_ERROR;
  }

  //2D array with third dimensions the coordinates
  output->Resize(dim[0], dim[1], 3);

  for (int i=0; i<dim[0]; i++)
  {
    for (int j=0; j<dim[1]; j++)
    {
      int pos[3]; pos[2] =0;
      pos[0] = i;
      pos[1] = j;
      int ptId = vtkStructuredData::ComputePointId(dim, pos);
      double pt[3];

      grid->GetPoint(ptId, pt);
      for (int k=0; k<3; k++)
      {
        output->SetValue(i, j, k, pt[k]);
      }
    }
  }

  return SV_OK;
}

// ----------------------
// PointsToTypedArray
// ----------------------
int vtkSVNURBSUtils::PointsToTypedArray(vtkPoints *points, vtkTypedArray<double> *output)
{
  int numPoints = points->GetNumberOfPoints();

  //2D array with third dimensions the coordinates
  output->Resize(numPoints, 3);

  for (int i=0; i<numPoints; i++)
  {
    double pt[3];
    points->GetPoint(i, pt);
    for (int j=0; j<3; j++)
    {
      output->SetValue(i, j, pt[j]);
    }
  }

  return SV_OK;
}

// ----------------------
// DoubleArrayToTypedArray
// ----------------------
int vtkSVNURBSUtils::DoubleArrayToTypedArray(vtkDoubleArray *input, vtkTypedArray<double> *output)
{
  int numVals  = input->GetNumberOfTuples();
  int numComps = input->GetNumberOfComponents();

  output->Resize(numVals, numComps);
  for (int i=0; i< numVals; i++)
  {
    for (int j=0; j< numComps; j++)
    {
      double val = input->GetComponent(i, j);
      output->SetValue(i, j, val);
    }
  }

  return SV_OK;
}


// ----------------------
// TypedArrayToPoints
// ----------------------
int vtkSVNURBSUtils::TypedArrayToPoints(vtkTypedArray<double> *array, vtkPoints *output)
{
  int numVals = array->GetExtents()[0].GetSize();

  output->SetNumberOfPoints(numVals);
  for (int i=0; i<numVals; i++)
  {
    double pt[3];
    for (int j=0; j<3; j++)
    {
      pt[j] = array->GetValue(i, j);
    }
    output->SetPoint(i, pt);
  }

  return SV_OK;
}

// ----------------------
// TypedArrayToStructuredGrid
// ----------------------
int vtkSVNURBSUtils::TypedArrayToStructuredGrid(vtkTypedArray<double> *array, vtkStructuredGrid *output)
{
  int dims = array->GetDimensions();
  //2D array with third dimensions the coordinates
  int dim[3];
  for (int i=0; i<3; i++)
  {
    dim[i] = array->GetExtents()[i].GetSize();
  }

  if (dims > 3)
  {
    fprintf(stderr,"3 Dimensions are not yet supported\n");
    return SV_ERROR;
  }
  if (dim[2] != 3)
  {
    fprintf(stderr,"Third dimension should have xyz coordinates\n");
    return SV_ERROR;
  }

  output->SetDimensions(dim[0], dim[1], 1);
  output->GetPoints()->SetNumberOfPoints(dim[0]*dim[1]);

  for (int i=0; i<dim[0]; i++)
  {
    for (int j=0; j<dim[1]; j++)
    {
      int pos[3]; pos[2] =0;
      pos[0] = i;
      pos[1] = j;
      int ptId = vtkStructuredData::ComputePointId(dim, pos);
      double pt[3];
      for (int k=0; k<3; k++)
      {
        pt[k] = array->GetValue(i, j, k);
      }
      output->GetPoints()->SetPoint(ptId, pt);
    }
  }

    return SV_OK;
}

// ----------------------
// PolyDatasToStructuredGrid
// ----------------------
int vtkSVNURBSUtils::PolyDatasToStructuredGrid(vtkPolyData **inputs, const int numInputs, vtkStructuredGrid *points)
{
  int numPoints = inputs[0]->GetNumberOfPoints();
  for (int i=0; i<numInputs; i++)
  {
    if (numPoints != inputs[i]->GetNumberOfPoints())
    {
      fprintf(stderr,"Input segments do not have the same number of points, cannot loft\n");
      return SV_ERROR;
    }
  }

  int dim[3];
  dim[0] = numInputs;
  dim[1] = numPoints;
  dim[2] = 1;

  vtkNew(vtkPoints, tmpPoints);
  tmpPoints->SetNumberOfPoints(numInputs*numPoints);
  for (int i=0; i<numInputs; i++)
  {
    for (int j=0; j<numPoints; j++)
    {
      int pos[3]; pos[0] = i; pos[1] = j; pos[2] = 0;
      int ptId = vtkStructuredData::ComputePointId(dim, pos);
      double pt[3];
      inputs[i]->GetPoint(j, pt);
      tmpPoints->SetPoint(ptId, pt);;
    }
  }
  points->SetPoints(tmpPoints);
  points->SetDimensions(dim);

  return SV_OK;
}

// ----------------------
// Intersect1D
// ----------------------
int vtkSVNURBSUtils::Intersect1D(vtkIntArray *v0, vtkIntArray *v1, vtkIntArray *result)
{
  int numVals0 = v0->GetNumberOfTuples();
  int numVals1 = v1->GetNumberOfTuples();
  if (numVals0 != numVals1)
  {
    fprintf(stderr,"Cannot do accurate comparison! Vectors are different lengths\n");
    return SV_ERROR;
  }
  result->SetNumberOfValues(numVals1);
  for (int i=0; i< numVals1; i++)
  {
    int val0 = v0->GetValue(i);
    int val1 = v1->GetValue(i);
    if (val0 && val1)
    {
      result->SetValue(i, 1);
    }
    else
    {
      result->SetValue(i, 0);
    }
  }

  return SV_OK;
}

// ----------------------
// Add1D
// ----------------------
int vtkSVNURBSUtils::Add1D(vtkDoubleArray *v0, vtkDoubleArray *v1, double scalar, vtkDoubleArray *result)
{
  int numVals0 = v0->GetNumberOfTuples();
  int numVals1 = v1->GetNumberOfTuples();
  if (numVals0 != numVals1)
  {
    fprintf(stderr,"Cannot do accurate comparison! Vectors are different lengths\n");
    return SV_ERROR;
  }
  result->SetNumberOfValues(numVals1);
  for (int i=0; i< numVals1; i++)
  {
    double val0 = v0->GetValue(i);
    double val1 = v1->GetValue(i);
    result->SetTuple1(i, val0 + scalar*val1);
  }

  return SV_OK;
}

// ----------------------
// AddVal1D
// ----------------------
int vtkSVNURBSUtils::AddVal1D(vtkDoubleArray *v0, double val, double scalar, vtkDoubleArray *result)
{
  int numVals = v0->GetNumberOfTuples();
  result->SetNumberOfValues(numVals);
  for (int i=0; i< numVals; i++)
  {
    double val0 = v0->GetTuple1(i);
    result->SetTuple1(i, val0 + scalar*val);
  }

  return SV_OK;
}

// ----------------------
// AddVal1D
// ----------------------
int vtkSVNURBSUtils::AddVal1D(double val, vtkDoubleArray *v0, double scalar, vtkDoubleArray *result)
{
  int numVals = v0->GetNumberOfTuples();
  result->SetNumberOfValues(numVals);
  for (int i=0; i< numVals; i++)
  {
    double val0 = v0->GetTuple1(i);
    result->SetTuple1(i, val + scalar*val0);
  }

  return SV_OK;
}

// ----------------------
// MultiplyVal1D
// ----------------------
int vtkSVNURBSUtils::MultiplyVal1D(vtkDoubleArray *v0, double val, vtkDoubleArray *result)
{
  int numVals = v0->GetNumberOfTuples();
  result->SetNumberOfValues(numVals);
  for (int i=0; i< numVals; i++)
  {
    double val0 = v0->GetTuple1(i);
    result->SetTuple1(i, val0 * val);
  }

  return SV_OK;
}

// ----------------------
// WhereGreaterEqual
// ----------------------
int vtkSVNURBSUtils::WhereGreaterEqual(double val, vtkDoubleArray *in, vtkIntArray *out)
{
  int numVals = in->GetNumberOfTuples();
  out->SetNumberOfTuples(numVals);

  for (int i=0; i<numVals; i++)
  {
    double compVal = in->GetTuple1(i);
    if (val >= compVal)
    {
      out->SetValue(i, 1);
    }
    else
    {
      out->SetValue(i, 0);
    }
  }

  return SV_OK;
}

// ----------------------
// WhereGreater
// ----------------------
int vtkSVNURBSUtils::WhereGreater(double val, vtkDoubleArray *in, vtkIntArray *out)
{
  int numVals = in->GetNumberOfTuples();
  out->SetNumberOfTuples(numVals);

  for (int i=0; i<numVals; i++)
  {
    double compVal = in->GetTuple1(i);
    if (val > compVal)
    {
      out->SetValue(i, 1);
    }
    else
    {
      out->SetValue(i, 0);
    }
  }

  return SV_OK;
}

// ----------------------
// WhereLessEqual
// ----------------------
int vtkSVNURBSUtils::WhereLessEqual(double val, vtkDoubleArray *in, vtkIntArray *out)
{
  int numVals = in->GetNumberOfTuples();
  out->SetNumberOfTuples(numVals);

  for (int i=0; i<numVals; i++)
  {
    double compVal = in->GetTuple1(i);
    if (val <= compVal)
    {
      out->SetValue(i, 1);
    }
    else
    {
      out->SetValue(i, 0);
    }
  }

  return SV_OK;
}

// ----------------------
// WhereLess
// ----------------------
int vtkSVNURBSUtils::WhereLess(double val, vtkDoubleArray *in, vtkIntArray *out)
{
  int numVals = in->GetNumberOfTuples();
  out->SetNumberOfTuples(numVals);

  for (int i=0; i<numVals; i++)
  {
    double compVal = in->GetTuple1(i);
    if (val < compVal)
    {
      out->SetValue(i, 1);
    }
    else
    {
      out->SetValue(i, 0);
    }
  }

  return SV_OK;
}

// ----------------------
// WhereEqual
// ----------------------
int vtkSVNURBSUtils::WhereEqual(double val, vtkDoubleArray *in, vtkIntArray *out)
{
  int numVals = in->GetNumberOfTuples();
  out->SetNumberOfTuples(numVals);

  for (int i=0; i<numVals; i++)
  {
    double compVal = in->GetTuple1(i);
    if (val == compVal)
    {
      out->SetValue(i, 1);
    }
    else
    {
      out->SetValue(i, 0);
    }
  }

  return SV_OK;
}

// ----------------------
// WhereNotEqual
// ----------------------
int vtkSVNURBSUtils::WhereNotEqual(double val, vtkDoubleArray *in, vtkIntArray *out)
{
  int numVals = in->GetNumberOfTuples();
  out->SetNumberOfTuples(numVals);

  for (int i=0; i<numVals; i++)
  {
    double compVal = in->GetTuple1(i);
    if (val != compVal)
    {
      out->SetValue(i, 1);
    }
    else
    {
      out->SetValue(i, 0);
    }
  }

  return SV_OK;
}

// ----------------------
// PrintArray
// ----------------------
int vtkSVNURBSUtils::PrintArray(vtkIntArray *arr)
{
  int num = arr->GetNumberOfTuples();
  fprintf(stdout,"Array: %d tuples\n", num);
  fprintf(stdout,"----------------------------------------------------------\n");
  for (int i=0; i<num; i++)
  {
    fprintf(stdout,"%.4f ", arr->GetTuple1(i));
  }
  fprintf(stdout,"\n");
  fprintf(stdout,"----------------------------------------------------------\n");

  return SV_OK;
}

// ----------------------
// PrintArray
// ----------------------
int vtkSVNURBSUtils::PrintArray(vtkDoubleArray *arr)
{
  int num = arr->GetNumberOfTuples();
  fprintf(stdout,"Array: %d tuples\n", num);
  fprintf(stdout,"----------------------------------------------------------\n");
  for (int i=0; i<num; i++)
  {
    fprintf(stdout,"%.4f ", arr->GetTuple1(i));
  }
  fprintf(stdout,"\n");
  fprintf(stdout,"----------------------------------------------------------\n");

  return SV_OK;
}

// ----------------------
// PrintVector
// ----------------------
int vtkSVNURBSUtils::PrintVector(vtkTypedArray<double> *vec)
{
  int dims = vec->GetDimensions();
  int num = vec->GetExtents()[0].GetSize();
  fprintf(stdout,"Array: %d tuples\n", num);
  fprintf(stdout,"----------------------------------------------------------\n");
  for (int i=0; i<num; i++)
  {
    fprintf(stdout,"| ");
    if (dims > 1)
    {
      for (int j=0; j<3; j++)
      {
        fprintf(stdout,"%.4f ", vec->GetValue(i, j));
      }
    }
    else
    {
      fprintf(stdout,"%.4f ", vec->GetValue(i));
    }
    fprintf(stdout,"|");
  }
  fprintf(stdout,"\n");
  fprintf(stdout,"----------------------------------------------------------\n");

  return SV_OK;
}

// ----------------------
// PrintMatrix
// ----------------------
int vtkSVNURBSUtils::PrintMatrix(vtkTypedArray<double> *mat)
{
  int dims = mat->GetDimensions();
  int nr = mat->GetExtents()[0].GetSize();
  int nc = mat->GetExtents()[1].GetSize();
  fprintf(stdout,"Matrix: %d by %d\n", nr, nc);
  fprintf(stdout,"----------------------------------------------------------\n");
  for (int i=0; i<nr; i++)
  {
    for (int j=0; j<nc; j++)
    {
      fprintf(stdout,"| ");
      if (dims > 2)
      {
        for (int k=0; k<3; k++)
        {
          fprintf(stdout,"%.4f ", mat->GetValue(i, j, k));
        }
      }
      else
      {
        fprintf(stdout,"%.4f ", mat->GetValue(i, j));
      }
      fprintf(stdout,"|");
    }
    fprintf(stdout,"\n");
  }
  fprintf(stdout,"----------------------------------------------------------\n");

  return SV_OK;
}

// ----------------------
// PrintStructuredGrid
// ----------------------
int vtkSVNURBSUtils::PrintStructuredGrid(vtkStructuredGrid *mat)
{
  int dim[3];
  mat->GetDimensions(dim);
  fprintf(stdout,"Matrix: %d by %d\n", dim[0], dim[1]);
  fprintf(stdout,"----------------------------------------------------------\n");
  for (int i=0; i<dim[0]; i++)
  {
    for (int j=0; j<dim[1]; j++)
    {
      int pos[3]; pos[0] = i; pos[1] = j; pos[2] = 0;
      int ptId = vtkStructuredData::ComputePointId(dim, pos);
      double pt[3];
      mat->GetPoint(ptId, pt);
      fprintf(stdout,"| %.4f %.4f %.4f |", pt[0], pt[1], pt[2]);
    }
    fprintf(stdout,"\n");
  }
  fprintf(stdout,"----------------------------------------------------------\n");

  return SV_OK;
}

// ----------------------
// PrintPoints
// ----------------------
int vtkSVNURBSUtils::PrintPoints(vtkPoints *points)
{
  int np = points->GetNumberOfPoints();
  fprintf(stdout,"Points: %d points\n", np);
  fprintf(stdout,"----------------------------------------------------------\n");
  for (int i=0; i<np; i++)
  {
    double pt[3];
    points->GetPoint(i, pt);
    fprintf(stdout,"Pt %d: ", i);
    for (int j=0; j<3; j++)
    {
      fprintf(stdout,"%.4f ",pt[j]);
    }
    fprintf(stdout,"\n");
  }
  fprintf(stdout,"----------------------------------------------------------\n");

  return SV_OK;
}

// ----------------------
// StructuredGridTranspose
// ----------------------
int vtkSVNURBSUtils::StructuredGridTranspose(vtkStructuredGrid *sg, vtkStructuredGrid *newSg)
{
  int dim[3];
  sg->GetDimensions(dim);
  newSg->SetDimensions(dim[1], dim[0], 1);
  vtkNew(vtkPoints, tmpPoints);
  tmpPoints->SetNumberOfPoints(sg->GetNumberOfPoints());


  for (int i=0; i<dim[0]; i++)
  {
    for (int j=0; j<dim[1]; j++)
    {
      int pos[3]; pos[2] = 0;
      pos[0] = i;
      pos[1] = j;
      int ptId = vtkStructuredData::ComputePointId(dim, pos);
      double pt[3];
      sg->GetPoint(ptId, pt);
      pos[0] = j;
      pos[1] = i;
      int newPtId = vtkStructuredData::ComputePointId(dim, pos);
      tmpPoints->SetPoint(newPtId, pt);
    }
  }

  newSg->SetPoints(tmpPoints);

  int newDims[3];
  return SV_OK;
}

// ----------------------
// MatrixTranspose
// ----------------------
int vtkSVNURBSUtils::MatrixTranspose(vtkTypedArray<double> *mat, const int matIsPoints, vtkTypedArray<double> *newMat)
{
  int nr = mat->GetExtents()[0].GetSize();
  int nc = mat->GetExtents()[1].GetSize();
  int np = mat->GetExtents()[2].GetSize();
  if (matIsPoints && np != 3)
  {
    fprintf(stderr,"Third dimension of matrix should contain xyz coordinates, but doesn't!\n");
    return SV_ERROR;
  }

  if (matIsPoints)
  {
    newMat->Resize(nc, nr, 3);
  }
  else
  {
    newMat->Resize(nc, nr);
  }

  for (int i=0; i<nr; i++)
  {
    for (int j=0; j<nc; j++)
    {
      if (matIsPoints)
      {
        for (int k=0; k<3; k++)
        {
          double val = mat->GetValue(i, j, k);
          newMat->SetValue(j, i, k, val);
        }
      }
      else
      {
        double val = mat->GetValue(i, j);
        newMat->SetValue(j, i, val);
      }
    }
  }

  return SV_OK;
}

// ----------------------
// MatrixToVector
// ----------------------
int vtkSVNURBSUtils::MatrixToVector(vtkTypedArray<double> *mat, double *matVec)
{
  int nr = mat->GetExtents()[0].GetSize();
  int nc = mat->GetExtents()[1].GetSize();

  for (int i=0; i<nc; i++)
  {
    for (int j=0; j<nr; j++)
    {
      matVec[i*nr+j] = mat->GetValue(j, i);
    }
  }

  return SV_OK;
}

// ----------------------
// VectorToMatrix
// ----------------------
int vtkSVNURBSUtils::VectorToMatrix(double *matVec, const int nr, const int nc, vtkTypedArray<double> *mat)
{
  mat->Resize(nr, nc);

  for (int i=0; i<nc; i++)
  {
    for (int j=0; j<nr; j++)
    {
      double val = matVec[i*nr+j];
      mat->SetValue(j, i, val);
    }
  }

  return SV_OK;
}

// ----------------------
// PointMatrixToVectors
// ----------------------
int vtkSVNURBSUtils::PointMatrixToVectors(vtkTypedArray<double> *mat, double *matVecs[3])
{
  int nr = mat->GetExtents()[0].GetSize();
  int nc = mat->GetExtents()[1].GetSize();
  int np = mat->GetExtents()[2].GetSize();
  if (np != 3)
  {
    fprintf(stderr,"Third dimension of matrix should contain xyz coordinates, but doesn't!\n");
    return SV_ERROR;
  }

  for (int i=0; i<nc; i++)
  {
    for (int j=0; j<nr; j++)
    {
      for (int k=0; k<3; k++)
      {
        matVecs[k][i*nr+j] = mat->GetValue(j, i, k);
      }
    }
  }

  return SV_OK;
}

// ----------------------
// VectorsToPointMatrix
// ----------------------
int vtkSVNURBSUtils::VectorsToPointMatrix(double *matVecs[3], const int nr, const int nc, vtkTypedArray<double> *mat)
{
  for (int i=0; i<nc; i++)
  {
    for (int j=0; j<nr; j++)
    {
      for (int k=0; k<3; k++)
      {
        double val = matVecs[k][i*nr+j];
        mat->SetValue(j, i, k, val);
      }
    }
  }

  return SV_OK;
}

// ----------------------
// DGEMM
// ----------------------
int vtkSVNURBSUtils::DGEMM(const double *A, const int nrA, const int ncA,
                         const double *B, const int nrB, const int ncB,
                         double *C)
{
  if (ncA != nrB)
  {
    fprintf(stderr,"Matrix dims do not match, cannot perform operation\n");
    return SV_ERROR;
  }
  for (int i=0; i<ncB; i++)
  {
    for (int j=0; j<nrA; j++)
    {
      C[i+ncB*j] = 0.0;
    }
  }

  //fprintf(stdout,"A Mat Dims: %d %d\n", nrA, ncA);
  //fprintf(stdout,"B Mat Dims: %d %d\n", nrB, ncB);
  /* For each row i of A */
  for (int i = 0; i < nrA; ++i)
  {
  /* For each column j of B */
    for (int j = 0; j < ncB; ++j)
    {
           /* Compute C(i,j) */
      double cij = C[i+j*nrA];
      for(int k = 0; k < ncA; k++)
      {
        //fprintf(stdout,"A[%d] + B[%d] + ",i+k*nrA, k+j*nrB);
        cij += A[i+k*nrA] * B[k+j*nrB];
      }
      //fprintf(stdout," 0 -> C[%d]\n",i+j*nrA);
      C[i+j*nrA] = cij;
    }
  }
  //fprintf(stdout,"End\n");

  return SV_OK;
}

// ----------------------
// Print2DArray
// ----------------------
int vtkSVNURBSUtils::Print2DArray(const double *arr, const int nr, const int nc)
{
  fprintf(stdout,"Matrix: %d by %d\n", nr, nc);
  fprintf(stdout,"----------------------------------------------------------\n");
  for (int i=0; i<nr*nc; i++)
  {
    fprintf(stdout,"| %.4f |\n", arr[i]);
  }
  fprintf(stdout,"----------------------------------------------------------\n");

  return SV_OK;
}
