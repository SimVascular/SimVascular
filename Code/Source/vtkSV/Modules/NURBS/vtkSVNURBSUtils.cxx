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
#include "vtkMath.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPoints.h"
#include "vtkSmartPointer.h"
#include "vtkSparseArray.h"
#include "vtkStructuredData.h"

#include "vtkSVGlobals.h"
#include "vtkSVIOUtils.h"
#include "vtkSVMathUtils.h"
#include "vtkSVNURBSCurve.h"
#include "vtkSVNURBSSurface.h"

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
int vtkSVNURBSUtils::LinSpace(const double min, const double max, const int num, vtkDoubleArray *result)
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
int vtkSVNURBSUtils::LinSpaceClamp(const double min, const double max, const int num, const int p, vtkDoubleArray *result)
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
// /details "equal" just gives an equally spaced parameter vector. "chord"
// computes the total eucledian distance between the given points. The
// spacing of the vetor U then reflects the distance of the points based
// on the chord length. "centripetal" is very similar to "chord", but
// is better for input data with lots of curvature.
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
  //fprintf(stdout,"Length of Knots: %lld\n", knots->GetNumberOfTuples());

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
// GetControlPointsOfCurve
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
// CurveRemoveKnot
// ----------------------
int vtkSVNURBSUtils::CurveRemoveKnot(vtkSVControlGrid *controlPoints, vtkDoubleArray *knots,
                                     const int degree,
                                     const double removeValue, const int removeIndex,
                                     const int currentMultiplicity,
                                     const int numberOfRemovals,
                                     const double tol,
                                     vtkSVControlGrid *newControlPoints, vtkDoubleArray *newKnots)
{
  // Get dimensions of control point grid
  int dims[3];
  controlPoints->GetDimensions(dims);

  // Copy control points and knots to new objects
  vtkNew(vtkSVControlGrid, PW);
  vtkSVNURBSUtils::GetPWFromP(controlPoints, PW);
  vtkNew(vtkDoubleArray, tmpKnots);
  tmpKnots->DeepCopy(knots);

  // Set values used by alg to more concise vars
  int n    = dims[0]-1;
  int p    = degree;
  double u = removeValue;
  int r    = removeIndex;
  int num  = numberOfRemovals;
  int s    = currentMultiplicity;

  int m     = n+p+1;
  int ord   = p+1;
  int fout  = (2*r-s-p)/2;
  int last  = r-s;
  int first = r-p;

  // Double check to see if correct vals were given
  if (knots->GetNumberOfTuples() != m+1)
  {
    fprintf(stderr,"Invalid number of control points given with knot span\n");
    return SV_ERROR;
  }

  if (knots->GetTuple1(r) != u)
  {
    fprintf(stderr,"Incorrect index %d given for knot value %f\n", r, u);
    return SV_ERROR;
  }

  // Compute a tolerance for points
  double origin[3]; origin[0] = 0.0; origin[1] = 0.0; origin[2] = 0.0;
  double minWeight = VTK_SV_LARGE_DOUBLE;
  double maxDist   = 0.0;
  for (int i=0; i<=n; i++)
  {
    double pw[4];
    controlPoints->GetControlPoint(i, 0, 0, pw);
    if (pw[3] < minWeight)
      minWeight = pw[3];
    double dist = vtkSVMathUtils::Distance(pw, origin, 3) > maxDist;
    if (dist > maxDist)
      maxDist = dist;
  }
  double tolerance = tol*minWeight/(1+fabs(maxDist));

  vtkNew(vtkDoubleArray, tmpPoints);
  tmpPoints->SetNumberOfComponents(4);
  tmpPoints->SetNumberOfTuples(2*p+1);

  // Set iter vars
  int i;
  int j;
  int ii;
  int jj;
  int t;

  for (t=0; t<num; t++)
  {
    // Get difference between indices of tmpPoints and control points
    int off = first-1;

    // Set tmpPoints
    double pw0[4], pw1[4];
    PW->GetControlPoint(off, 0, 0, pw0);
    PW->GetControlPoint(last+1, 0, 0, pw1);
    tmpPoints->SetTuple(0, pw0);
    tmpPoints->SetTuple(last+1-off, pw1);

    // Update iter vars
    i=first;
    j=last;
    ii=1;
    jj=last-off;
    int remFlag = 0;

    // Go through and compute control points for each removal
    while (j-i > t)
    {
      double alpha0 = (u-tmpKnots->GetTuple1(i))/(tmpKnots->GetTuple1(i+ord+t)-tmpKnots->GetTuple1(i));
      double alpha1 = (u-tmpKnots->GetTuple1(j-t))/(tmpKnots->GetTuple1(j+ord)-tmpKnots->GetTuple1(j-t));

      // Set tmpPoints control points
      double pw2[4], pw3[4], newPoint0[4];
      tmpPoints->GetTuple(ii-1, pw2);
      PW->GetControlPoint(i, 0, 0, pw3);
      vtkSVMathUtils::Add(pw3, 1.0, pw2, -1.0*(1.0-alpha0), 4, newPoint0);
      vtkSVMathUtils::MultiplyScalar(newPoint0, 1.0/alpha0, 4);
      tmpPoints->SetTuple(ii, newPoint0);

      double pw4[4], pw5[4], newPoint1[4];
      tmpPoints->GetTuple(jj+1, pw4);
      PW->GetControlPoint(j, 0, 0, pw5);
      vtkSVMathUtils::Add(pw5, 1.0, pw4, -1.0*alpha1, 4, newPoint1);
      vtkSVMathUtils::MultiplyScalar(newPoint1, 1.0/(1.0-alpha1), 4);
      tmpPoints->SetTuple(jj, newPoint1);

      // edit iter vars
      i++;
      ii++;
      j--;
      jj--;
    }

    // Check to see if we can remove knot
    if (j-i < t)
    {
      double pw2[4], pw3[4];
      tmpPoints->GetTuple(ii-1, pw2);
      tmpPoints->GetTuple(jj+1, pw3);
      //if (vtkSVMathUtils::Distance(pw2, pw3, 4) <= tolerance)
        remFlag = 1;
    }
    else
    {
      double alpha0 = (u-tmpKnots->GetTuple1(i))/(tmpKnots->GetTuple1(i+ord+t)-tmpKnots->GetTuple1(i));

      double pw2[4], pw3[4], pw4[4], testPoint[4];
      PW->GetControlPoint(i, 0, 0, pw2);
      tmpPoints->GetTuple(ii+t+1, pw3);
      tmpPoints->GetTuple(ii-1, pw4);
      vtkSVMathUtils::Add(pw3, alpha0, pw4, 1.0-alpha0, 4, testPoint);

      //if (vtkSVMathUtils::Distance(pw2, testPoint, 4) <= tolerance)
        remFlag = 1;
    }

    // Check if knot can be removed
    if (remFlag == 0)
      break;
    else
    {
      i = first;
      j = last;

      // Save new control points from tmpPoints
      while (j-i > t)
      {
        double pw2[4], pw3[4];
        tmpPoints->GetTuple(i-off, pw2);
        tmpPoints->GetTuple(j-off, pw3);

        PW->SetControlPoint(i, 0, 0, pw2);
        PW->SetControlPoint(j, 0, 0, pw3);
        i++;
        j--;
      }

    }
    first--;
    last++;
  }

  if (t==0)
  {
    fprintf(stderr,"No knots were able to be removed\n");
    return SV_OK;
  }

  // Update new knots
  newKnots->SetNumberOfTuples(m+1-t);
  for (int k=0; k<=m-t; k++)
    newKnots->SetTuple1(k, tmpKnots->GetTuple1(k));
  for (int k=r+1; k<=m; k++)
    newKnots->SetTuple1(k-t, tmpKnots->GetTuple1(k));

  // Update iter vars
  j = fout;
  i = fout;
  for (int k=1; k<t; k++)
  {
    if (k%2 == 1)
      i++;
    else
      j--;
  }

  // Update new control points
  newControlPoints->GetPoints()->SetNumberOfPoints(n+1-t);
  newControlPoints->SetDimensions(n+1-t, 1, 1);

  for (int k=0; k<j; k++)
  {
    double pw[4];
    PW->GetControlPoint(k, 0, 0, pw);
    vtkMath::MultiplyScalar(pw, 1./pw[3]);
    newControlPoints->SetControlPoint(k, 0, 0, pw);
  }

  for (int k=i+1; k<=n; k++)
  {
    double pw[4];
    PW->GetControlPoint(k, 0, 0, pw);
    vtkMath::MultiplyScalar(pw, 1./pw[3]);
    newControlPoints->SetControlPoint(j, 0, 0, pw);
    j++;
  }

  return SV_OK;
}

// ----------------------
// CurveBezierExtraction
// ----------------------
int vtkSVNURBSUtils::CurveBezierExtraction(vtkSVControlGrid *controlPoints, vtkDoubleArray *knots,
                                           const int degree,
                                           vtkSVNURBSCollection *curves)
{
  // Get dimensions of control point grid
  int dims[3];
  controlPoints->GetDimensions(dims);

  // Really quick, convert all points to pw
  vtkNew(vtkSVControlGrid, PW);
  vtkSVNURBSUtils::GetPWFromP(controlPoints, PW);

  // Set values used by alg to more concise vars
  int n = dims[0]-1;
  int p = degree;

  int m  = n+p+1;
  int a  = p;
  int b  = p+1;

  // Double check to see if correct vals were given
  if (knots->GetNumberOfTuples() != m+1)
  {
    fprintf(stderr,"Invalid number of control points given with knot span\n");
    return SV_ERROR;
  }

  // Set up new knots, just bezier knot span
  vtkNew(vtkDoubleArray, newKnots);
  vtkSVNURBSUtils::LinSpaceClamp(0, 1, 2*(p+1), p, newKnots);

  // New control points, updated each time for new bezier curve
  vtkNew(vtkSVControlGrid, newControlPoints);
  newControlPoints->SetNumberOfControlPoints(p+1);
  newControlPoints->SetDimensions(p+1, 1, 1);
  vtkNew(vtkDoubleArray, tmpPoints);
  tmpPoints->SetNumberOfComponents(4);
  tmpPoints->SetNumberOfTuples(p-1);

  // Set first p+1 control points
  for(int i=0; i<=p; i++)
  {
    double pw[4];
    PW->GetControlPoint(i, 0, 0, pw);
    newControlPoints->SetControlPoint(i, 0, 0, pw);
  }

  // Initiate array for alphas
  vtkNew(vtkDoubleArray, alphas);
  alphas->SetNumberOfTuples(m);

  while (b < m)
  {

    // Check multiplicity
    int i = b;
    while (b < m && knots->GetTuple1(b+1) == knots->GetTuple1(b))
      b++;
    int mult = b-i+1;
    if (mult < p)
    {
      // Alpha numerator
      double numer = knots->GetTuple1(b) - knots->GetTuple1(a);
      // Compute all alphas
      for (int j=p; j>mult; j--)
        alphas->SetTuple1(j-mult-1, numer/(knots->GetTuple1(a+j)-knots->GetTuple1(a)));

      int r = p-mult;

      // Insert the knot the number of times needed to get to p
      for (int j=1; j<=r; j++)
      {
        int save = r-j;
        int s = mult+j;
        for (int k=p; k>=s; k--)
        {
          double alpha = alphas->GetTuple1(k-s);
          double pw0[4], pw1[4], newPoint[4];
          newControlPoints->GetControlPoint(k, 0, 0, pw0);
          newControlPoints->GetControlPoint(k-1, 0, 0, pw1);
          vtkSVMathUtils::Add(pw0, alpha, pw1, 1.0-alpha, 4, newPoint);
          newControlPoints->SetControlPoint(k, 0, 0, newPoint);

        }
        if (b < m)
        {
          double pw[4];
          newControlPoints->GetControlPoint(p, 0, 0, pw);
          tmpPoints->SetTuple(save, pw);
        }
      }
    }

    vtkNew(vtkSVControlGrid, tmpControl);
    vtkSVNURBSUtils::GetPFromPW(newControlPoints, tmpControl);

    vtkNew(vtkSVNURBSCurve, newCurve);
    newCurve->SetControlPointGrid(tmpControl);
    newCurve->SetKnotVector(newKnots);
    newCurve->SetDegree(p);
    curves->AddItem(newCurve);

    // Set up next curve
    if (b < m)
    {
      for (int k=0; k<p-1; k++)
      {
        double pw[4];
        tmpPoints->GetTuple(k, pw);
        newControlPoints->SetControlPoint(k, 0, 0, pw);

      }
      for (i=p-mult; i<=p; i++)
      {
        double pw[4];
        PW->GetControlPoint(b-p+i, 0, 0, pw);
        newControlPoints->SetControlPoint(i, 0, 0, pw);
      }
      a=b;
      b++;
    }
  }

  return SV_OK;
}

// ----------------------
// CurveDecreaseDegree
// ----------------------
int vtkSVNURBSUtils::CurveDecreaseDegree(vtkSVControlGrid *controlPoints, vtkDoubleArray *knots,
                                         const int degree,
                                         const double tolerance,
                                         vtkSVControlGrid *newControlPoints, vtkDoubleArray *newKnots)
{
  // Get dimensions of control point grid
  int dims[3];
  controlPoints->GetDimensions(dims);

  // Really quick, convert all points to pw
  vtkNew(vtkSVControlGrid, PW);
  vtkSVNURBSUtils::GetPWFromP(controlPoints, PW);

  // Set values used by alg to more concise vars
  int n = dims[0]-1;
  int p = degree;

  int m  = n+p+1;
  int ph = p-1;

  // Double check to see if correct vals were given
  if (knots->GetNumberOfTuples() != m+1)
  {
    fprintf(stderr,"Invalid number of control points given with knot span\n");
    return SV_ERROR;
  }

  // Compute s by cmputing the number of nonnon-repeated internal knot vals
  vtkNew(vtkIntArray, multiplicity);
  vtkNew(vtkDoubleArray, singleValues);
  vtkSVNURBSUtils::GetMultiplicity(knots, multiplicity, singleValues);

  // S s is length of mult vector minus 2 (p+1 ends)
  int s = multiplicity->GetNumberOfTuples() - 2;
  int mhat = m-(s+2);
  int nhat = n-(s+1);

  // Set up new knots
  newKnots->SetNumberOfTuples(mhat+1);

  // New control points, updated each time for new bezier curve
  newControlPoints->SetNumberOfControlPoints(nhat+1);
  newControlPoints->SetDimensions(nhat+1, 1, 1);

  // Bezier control points, degree p
  vtkNew(vtkDoubleArray, bpts);
  bpts->SetNumberOfComponents(4);
  bpts->SetNumberOfTuples(p+1);

  // Bezier control points, degree p-1
  vtkNew(vtkDoubleArray, rbpts);
  rbpts->SetNumberOfComponents(4);
  rbpts->SetNumberOfTuples(p);

  // Leftmost control pionts of next Bezier segment
  vtkNew(vtkDoubleArray, nextbpts);
  nextbpts->SetNumberOfComponents(4);
  nextbpts->SetNumberOfTuples(p-1);

  // Alpha values for knot insertion
  vtkNew(vtkDoubleArray, alphas);
  alphas->SetNumberOfTuples(p-1);

  // Error values
  vtkNew(vtkDoubleArray, error);
  error->SetNumberOfTuples(m);
  error->FillComponent(0, 0.0);

  // Set up iter vars
  int mh   = ph;
  int kind = ph+1;
  int r    = -1;
  int a    = p;
  int b    = p+1;
  int cind = 1;
  int mult = p;

  // Pass the first control point
  double firstpw[4];
  PW->GetControlPoint(0, 0, 0, firstpw);
  newControlPoints->SetControlPoint(0, 0, 0, firstpw);

  // Set unchanging knot points
  for (int i=0; i<=ph; i++)
    newKnots->SetTuple1(i, knots->GetTuple1(0));

  // And first bezier points
  for (int i=0; i<=p; i++)
  {
    double pw[4];
    PW->GetControlPoint(i, 0, 0, pw);
    bpts->SetTuple(i, pw);
  }

  // Loop through knot span
  while (b<m)
  {
    int i=b;
    while (b<m && knots->GetTuple1(b) == knots->GetTuple1(b+1))
      b++;
    int mult = b-i+1;
    mh       = mh+mult-1;
    int oldr = r;
    r = p-mult;

    // Set up left vars
    int lbz;
    if (oldr > 0)
      lbz = (oldr+2)/2;
    else
      lbz = 1;

    // Insert the knot r times
    if (r > 0)
    {
      double numer = knots->GetTuple1(b) - knots->GetTuple1(a);
      for (int k=p; k>mult; k--)
      {
        double newVal = numer/(knots->GetTuple1(a+k)-knots->GetTuple1(a));
        alphas->SetTuple1(k-mult-1, newVal);
      }
      for (int j=1; j<=r; j++)
      {
        int save = r-j;
        int mulj = mult+j;
        for (int k=p; k>=mulj; k--)
        {
          double alpha = alphas->GetTuple1(k-mulj);
          double pw0[4], pw1[4], newPoint[4];
          bpts->GetTuple(k, pw0);
          bpts->GetTuple(k-1, pw1);
          vtkSVMathUtils::Add(pw0, alpha, pw1, 1.0-alpha, 4, newPoint);
          bpts->SetTuple(k, newPoint);
        }
        double pw[4];
        bpts->GetTuple(p, pw);
        nextbpts->SetTuple(save, pw);
      }
    }

    // Now reduce the degree
    double maxError = 0.0;
    vtkSVNURBSUtils::BezierCurveDecreaseDegree(bpts, maxError, rbpts);
    error->SetTuple1(a, error->GetTuple1(a)+maxError);

    if (error->GetTuple1(a) > tolerance)
    {
      fprintf(stderr,"Error %.5f is greater than tolerance %.5f\n", error->GetTuple1(a), tolerance);
      fprintf(stderr,"Curve is not reducable, before knot removal\n");
      return SV_ERROR;
    }

    // Now remove knots
    if (oldr > 0)
    {
      int first = kind;
      int last  = kind;

      for (int k=0; k<oldr; k++)
      {
        i      = first;
        int j  = last;
        int kj = j-kind;

        while (j-i > k)
        {
            double alpha = (knots->GetTuple1(a)-newKnots->GetTuple1(i-1))/
                           (knots->GetTuple1(b)-newKnots->GetTuple1(i-1));
            double beta  = (knots->GetTuple1(a)-newKnots->GetTuple1(j-k-1))/
                           (knots->GetTuple1(b)-newKnots->GetTuple1(j-k-1));

            double pw0[4], pw1[4], newPoint0[4];
            newControlPoints->GetControlPoint(i-1, 0, 0, pw0);
            newControlPoints->GetControlPoint(i-2, 0, 0, pw1);
            vtkSVMathUtils::Add(pw0, 1.0, pw1, -1.0*(1.0-alpha), 4, newPoint0);
            vtkSVMathUtils::MultiplyScalar(newPoint0, alpha, 4);
            newControlPoints->SetControlPoint(i-1, 0, 0, newPoint0);

            double pw2[4], pw3[4], newPoint1[4];
            rbpts->GetTuple(kj, pw2);
            rbpts->GetTuple(kj+1, pw3);
            vtkSVMathUtils::Add(pw2, 1.0, pw3, -1.0*beta, 4, newPoint1);
            vtkSVMathUtils::MultiplyScalar(newPoint1, 1.0-beta, 4);
            rbpts->SetTuple(kj, newPoint1);
          i++;
          j--;
          kj--;
        }
        // Compute error bounds
        double eb;
        if (j-i < k)
        {
          double pw0[4], pw1[4];
          newControlPoints->GetControlPoint(i-2, 0, 0, pw0);
          rbpts->GetTuple(kj+1, pw1);
          eb = vtkSVMathUtils::Distance(pw0, pw1, 4);
        }
        else
        {
          double delta = (knots->GetTuple1(a)-newKnots->GetTuple1(i-1))/
                         (knots->GetTuple1(b)-newKnots->GetTuple1(i-1));

          double pw0[4], pw1[4], pw2[4], newPoint[4];
          rbpts->GetTuple(kj+1, pw0);
          newControlPoints->GetControlPoint(i-2, 0, 0, pw1);
          vtkSVMathUtils::Add(pw0, delta, pw1, 1.0-delta, 4, newPoint);

          newControlPoints->GetControlPoint(i-1, 0, 0, pw2);
          eb = vtkSVMathUtils::Distance(pw2, newPoint, 4);
        }

        // Update errors
        int K = a+oldr-k;
        int q = (2*p-k+1)/2;
        int L = K-q;
        for (int ii=L; ii<=a; ii++)
        {
          error->SetTuple1(ii, error->GetTuple1(ii)+eb);
          if (error->GetTuple1(ii) > tolerance)
          {
            fprintf(stderr,"Error %.5f is greater than tolerance %.5f\n", error->GetTuple1(ii), tolerance);
            fprintf(stderr,"Curve is not reducable, after knot removal\n");
            return SV_ERROR;
          }
        }
        first--;
        last++;
      }
      cind = i-1;
    }

    // Update end of knots
    if (a != p)
    {
      for (i=0; i<ph-oldr; i++)
      {
        newKnots->SetTuple1(kind, knots->GetTuple1(a));
        kind++;
      }
    }

    for (i=lbz; i<=ph; i++)
    {
      double pw[4];
      rbpts->GetTuple(i, pw);
      newControlPoints->SetControlPoint(cind, 0, 0, pw);
      cind++;
    }

    // Now set up for next pass;
    if (b<m)
    {
      for (i=0; i<r; i++)
      {
        double pw[4];
        nextbpts->GetTuple(i, pw);
        bpts->SetTuple(i, pw);
      }
      for (i=0; i<=p; i++)
      {
        double pw[4];
        PW->GetControlPoint(b-p+i, 0, 0, pw);
        bpts->SetTuple(i, pw);
      }
      a=b;
      b++;
    }
    else
    {
      // Update remaining knots
      for (i=0; i<=ph; i++)
        newKnots->SetTuple1(kind+i, knots->GetTuple1(b));
    }
  }
  int nh = mh-ph-1;

  if (mh != mhat)
  {
    fprintf(stderr,"Something went wrong: mhat %d does not equal iter m %d\n", mhat, mh);
    return SV_ERROR;
  }
  if (nh != nhat)
  {
    fprintf(stderr,"Something went wrong: mhat %d does not equal iter m %d\n", nhat, nh);
    return SV_ERROR;
  }

  // Realy quick, convert everything back to just p
  vtkSVNURBSUtils::GetPFromPW(newControlPoints);

  return SV_OK;
}

// ----------------------
// BezierCurveDecreaseDegree
// ----------------------
int vtkSVNURBSUtils::BezierCurveDecreaseDegree(vtkDoubleArray *controlPoints,
                                               double &maxError,
                                               vtkDoubleArray *newControlPoints)
{
  // Points are already assumed to be in PW format; if they are not, convert
  // them to the format (w*x, w*y, w*z, w)
  // Get dimensions of control point grid
  int n = controlPoints->GetNumberOfTuples();

  // Set values used by alg to more concise vars
  int p = n-1;
  int r = (p-1)/2;

  // Set up alphas
  vtkNew(vtkDoubleArray, alphas);
  alphas->SetNumberOfTuples(p);
  for (int i=0; i<=p; i++)
    alphas->SetTuple1(i, 1.0*i/p);

  //Transfer end points
  newControlPoints->SetTuple(0, controlPoints->GetTuple(0));
  newControlPoints->SetTuple(p-1, controlPoints->GetTuple(p));

  // Do ops if even or odd
  if (p%2 == 0)
  {
    // even
    for (int i=1; i<=r; i++)
    {
      double alpha=alphas->GetTuple1(i);
      double pw0[4], pw1[4], newPoint[4];
      controlPoints->GetTuple(i, pw0);
      newControlPoints->GetTuple(i-1, pw1);
      vtkSVMathUtils::Add(pw0, 1.0, pw1, -1.0*alpha, 4, newPoint);
      vtkSVMathUtils::MultiplyScalar(newPoint, 1.0-alpha, 4);
      newControlPoints->SetTuple(i, newPoint);
    }
    for (int i=p-2; i>=r+1; i--)
    {
      double alpha=alphas->GetTuple1(i+1);
      double pw0[4], pw1[4], newPoint[4];
      controlPoints->GetTuple(i+1, pw0);
      newControlPoints->GetTuple(i+1, pw1);
      vtkSVMathUtils::Add(pw0, 1.0, pw1, -1.0*(1.0-alpha), 4, newPoint);
      vtkSVMathUtils::MultiplyScalar(newPoint, alpha, 4);
      newControlPoints->SetTuple(i, newPoint);
    }
  }
  else
  {
    //odd
    for (int i=1; i<=r-1; i++)
    {
      double alpha=alphas->GetTuple1(i);
      double pw0[4], pw1[4], newPoint[4];
      controlPoints->GetTuple(i, pw0);
      newControlPoints->GetTuple(i-1, pw1);
      vtkSVMathUtils::Add(pw0, 1.0, pw1, -1.0*alpha, 4, newPoint);
      vtkSVMathUtils::MultiplyScalar(newPoint, 1.0-alpha, 4);
      newControlPoints->SetTuple(i, newPoint);
    }
    for (int i=p-2; i>=r+1; i--)
    {
      double alpha=alphas->GetTuple1(i+1);
      double pw0[4], pw1[4], newPoint[4];
      controlPoints->GetTuple(i+1, pw0);
      newControlPoints->GetTuple(i+1, pw1);
      vtkSVMathUtils::Add(pw0, 1.0, pw1, -1.0*(1.0-alpha), 4, newPoint);
      vtkSVMathUtils::MultiplyScalar(newPoint, alpha, 4);
      newControlPoints->SetTuple(i, newPoint);
    }

    double alphar=alphas->GetTuple1(r);
    double pwl0[4], pwl1[4], newPointL[4];
    controlPoints->GetTuple(r, pwl0);
    newControlPoints->GetTuple(r-1, pwl1);
    vtkSVMathUtils::Add(pwl0, 1.0, pwl1, -1.0*alphar, 4, newPointL);
    vtkSVMathUtils::MultiplyScalar(newPointL, 1.0-alphar, 4);

    double alpharp1 = alphas->GetTuple1(r+1);
    double pwr0[4], pwr1[4], newPointR[4];
    controlPoints->GetTuple(r+1, pwr0);
    newControlPoints->GetTuple(r+1, pwr1);
    vtkSVMathUtils::Add(pwr0, 1.0, pwr1, -1.0*(-1.0-alpharp1), 4, newPointR);
    vtkSVMathUtils::MultiplyScalar(newPointR, alpharp1, 4);

    double newPointMid[4];
    vtkSVMathUtils::Add(newPointL, 0.5, newPointR, 0.5, 4, newPointMid);
    newControlPoints->SetTuple(r, newPointMid);
  }

  return SV_OK;
}

// ----------------------
// BezierNURBSDecreaseDegree
// ----------------------
int vtkSVNURBSUtils::BezierNURBSDecreaseDegree(vtkSVControlGrid *controlPoints,
                                               const int decreaseDirection,
                                               double &maxError,
                                               vtkSVControlGrid *newControlPoints)
{
  // Points are already assumed to be in PW format; if they are not, convert
  // them to the format (w*x, w*y, w*z, w)
  // Get dimensions of control point grid
  int dims[3];
  controlPoints->GetDimensions(dims);
  int dir = decreaseDirection;

  if (dir == 0)
  {
    // Set values used by alg to more concise vars
    int p = dims[0]-1;
    int m = dims[1]-1;
    int r = (p-1)/2;

    // Set up alphas
    vtkNew(vtkDoubleArray, alphas);
    alphas->SetNumberOfTuples(p);
    for (int i=0; i<=p; i++)
      alphas->SetTuple1(i, 1.0*i/p);

    //Transfer end points
    for (int col=0; col<=m; col++)
    {
      double pw0[4], pw1[4];
      controlPoints->GetControlPoint(0, col, 0, pw0);
      controlPoints->GetControlPoint(p, col, 0, pw1);
      newControlPoints->SetControlPoint(0, col, 0, pw0);
      newControlPoints->SetControlPoint(p-1, col, 0, pw1);
    }

    // Do ops if even or odd
    if (p%2 == 0)
    {
      // even
      for (int col=0; col<=m; col++)
      {
        for (int i=1; i<=r; i++)
        {
          double alpha=alphas->GetTuple1(i);
          double pw0[4], pw1[4], newPoint[4];
          controlPoints->GetControlPoint(i, col, 0, pw0);
          newControlPoints->GetControlPoint(i-1, col, 0, pw1);
          vtkSVMathUtils::Add(pw0, 1.0, pw1, -1.0*alpha, 4, newPoint);
          vtkSVMathUtils::MultiplyScalar(newPoint, 1.0-alpha, 4);
          newControlPoints->SetControlPoint(i, col, 0, newPoint);
        }
        for (int i=p-2; i>=r+1; i--)
        {
          double alpha=alphas->GetTuple1(i+1);
          double pw0[4], pw1[4], newPoint[4];
          controlPoints->GetControlPoint(i+1, col, 0, pw0);
          newControlPoints->GetControlPoint(i+1, col, 0, pw1);
          vtkSVMathUtils::Add(pw0, 1.0, pw1, -1.0*(1.0-alpha), 4, newPoint);
          vtkSVMathUtils::MultiplyScalar(newPoint, alpha, 4);
          newControlPoints->SetControlPoint(i, col, 0, newPoint);
        }
      }
    }
    else
    {
      //odd
      for (int col=0; col<=m; col++)
      {
        for (int i=1; i<=r-1; i++)
        {
          double alpha=alphas->GetTuple1(i);
          double pw0[4], pw1[4], newPoint[4];
          controlPoints->GetControlPoint(i, col, 0, pw0);
          newControlPoints->GetControlPoint(i-1, col, 0, pw1);
          vtkSVMathUtils::Add(pw0, 1.0, pw1, -1.0*alpha, 4, newPoint);
          vtkSVMathUtils::MultiplyScalar(newPoint, 1.0-alpha, 4);
          newControlPoints->SetControlPoint(i, col, 0, newPoint);
        }

        for (int i=p-2; i>=r+1; i--)
        {
          double alpha=alphas->GetTuple1(i+1);
          double pw0[4], pw1[4], newPoint[4];
          controlPoints->GetControlPoint(i+1, col, 0, pw0);
          newControlPoints->GetControlPoint(i+1, col, 0, pw1);
          vtkSVMathUtils::Add(pw0, 1.0, pw1, -1.0*(1.0-alpha), 4, newPoint);
          vtkSVMathUtils::MultiplyScalar(newPoint, alpha, 4);
          newControlPoints->SetControlPoint(i, col, 0, newPoint);
        }

        double alphar=alphas->GetTuple1(r);
        double pwl0[4], pwl1[4], newPointL[4];
        controlPoints->GetControlPoint(r, col, 0, pwl0);
        newControlPoints->GetControlPoint(r-1, col, 0, pwl1);
        vtkSVMathUtils::Add(pwl0, 1.0, pwl1, -1.0*alphar, 4, newPointL);
        vtkSVMathUtils::MultiplyScalar(newPointL, 1.0-alphar, 4);

        double alpharp1 = alphas->GetTuple1(r+1);
        double pwr0[4], pwr1[4], newPointR[4];
        controlPoints->GetControlPoint(r+1, col, 0, pwr0);
        newControlPoints->GetControlPoint(r+1, col, 0, pwr1);
        vtkSVMathUtils::Add(pwr0, 1.0, pwr1, -1.0*(-1.0-alpharp1), 4, newPointR);
        vtkSVMathUtils::MultiplyScalar(newPointR, alpharp1, 4);

        double newPointMid[4];
        vtkSVMathUtils::Add(newPointL, 0.5, newPointR, 0.5, 4, newPointMid);
        newControlPoints->SetControlPoint(r, col, 0, newPointMid);
      }
    }
  }
  else if (dir == 1)
  {
    // Set values used by alg to more concise vars
    int q = dims[1]-1;
    int n = dims[0]-1;
    int r = (q-1)/2;

    // Set up alphas
    vtkNew(vtkDoubleArray, alphas);
    alphas->SetNumberOfTuples(q);
    for (int i=0; i<=q; i++)
      alphas->SetTuple1(i, 1.0*i/q);

    //Transfer end points
    for (int row=0; row<=n; row++)
    {
      double pw0[4], pw1[4];
      controlPoints->GetControlPoint(row, 0, 0, pw0);
      controlPoints->GetControlPoint(row, q, 0, pw1);
      newControlPoints->SetControlPoint(row, 0, 0, pw0);
      newControlPoints->SetControlPoint(row, q-1, 0, pw1);
    }

    // Do ops if even or odd
    if (q%2 == 0)
    {
      // even
      for (int row=0; row<=n; row++)
      {
        for (int i=1; i<=r; i++)
        {
          double alpha=alphas->GetTuple1(i);
          double pw0[4], pw1[4], newPoint[4];
          controlPoints->GetControlPoint(row, i, 0, pw0);
          newControlPoints->GetControlPoint(row, i-1, 0, pw1);
          vtkSVMathUtils::Add(pw0, 1.0, pw1, -1.0*alpha, 4, newPoint);
          vtkSVMathUtils::MultiplyScalar(newPoint, 1.0-alpha, 4);
          newControlPoints->SetControlPoint(row, i, 0, newPoint);
        }
        for (int i=q-2; i>=r+1; i--)
        {
          double alpha=alphas->GetTuple1(i+1);
          double pw0[4], pw1[4], newPoint[4];
          controlPoints->GetControlPoint(row, i+1, 0, pw0);
          newControlPoints->GetControlPoint(row, i+1, 0, pw1);
          vtkSVMathUtils::Add(pw0, 1.0, pw1, -1.0*(1.0-alpha), 4, newPoint);
          vtkSVMathUtils::MultiplyScalar(newPoint, alpha, 4);
          newControlPoints->SetControlPoint(row, i, 0, newPoint);
        }
      }
    }
    else
    {
      //odd
      for (int row=0; row<=n; row++)
      {
        for (int i=1; i<=r-1; i++)
        {
          double alpha=alphas->GetTuple1(i);
          double pw0[4], pw1[4], newPoint[4];
          controlPoints->GetControlPoint(row, i, 0, pw0);
          newControlPoints->GetControlPoint(row, i-1, 0, pw1);
          vtkSVMathUtils::Add(pw0, 1.0, pw1, -1.0*alpha, 4, newPoint);
          vtkSVMathUtils::MultiplyScalar(newPoint, 1.0-alpha, 4);
          newControlPoints->SetControlPoint(row, i, 0, newPoint);
        }

        for (int i=q-2; i>=r+1; i--)
        {
          double alpha=alphas->GetTuple1(i+1);
          double pw0[4], pw1[4], newPoint[4];
          controlPoints->GetControlPoint(row, i+1, 0, pw0);
          newControlPoints->GetControlPoint(row, i+1, 0, pw1);
          vtkSVMathUtils::Add(pw0, 1.0, pw1, -1.0*(1.0-alpha), 4, newPoint);
          vtkSVMathUtils::MultiplyScalar(newPoint, alpha, 4);
          newControlPoints->SetControlPoint(row, i, 0, newPoint);
        }

        double alphar=alphas->GetTuple1(r);
        double pwl0[4], pwl1[4], newPointL[4];
        controlPoints->GetControlPoint(row, r, 0, pwl0);
        newControlPoints->GetControlPoint(row, r-1, 0, pwl1);
        vtkSVMathUtils::Add(pwl0, 1.0, pwl1, -1.0*alphar, 4, newPointL);
        vtkSVMathUtils::MultiplyScalar(newPointL, 1.0-alphar, 4);

        double alpharp1 = alphas->GetTuple1(r+1);
        double pwr0[4], pwr1[4], newPointR[4];
        controlPoints->GetControlPoint(row, r+1, 0, pwr0);
        newControlPoints->GetControlPoint(row, r+1, 0, pwr1);
        vtkSVMathUtils::Add(pwr0, 1.0, pwr1, -1.0*(-1.0-alpharp1), 4, newPointR);
        vtkSVMathUtils::MultiplyScalar(newPointR, alpharp1, 4);

        double newPointMid[4];
        vtkSVMathUtils::Add(newPointL, 0.5, newPointR, 0.5, 4, newPointMid);
        newControlPoints->SetControlPoint(row, r, 0, newPointMid);
      }
    }
  }

  return SV_OK;
}

// ----------------------
// SurfaceDecreaseDegree
// ----------------------
int vtkSVNURBSUtils::DecreaseDegree(vtkSVControlGrid *controlPoints,
                                    vtkDoubleArray *uKnots, const int uDegree,
                                    vtkDoubleArray *vKnots, const int vDegree,
                                    const int decreaseDirection,
                                    const double tolerance,
                                    vtkSVControlGrid *newControlPoints,
                                    vtkDoubleArray *newUKnots,
                                    vtkDoubleArray *newVKnots)
{
  // Get dimensions of control point grid
  int dims[3];
  controlPoints->GetDimensions(dims);

  // Really quick, convert all points to pw
  vtkNew(vtkSVControlGrid, PW);
  vtkSVNURBSUtils::GetPWFromP(controlPoints, PW);

  // Set values used by alg to more concise vars
  int n   = dims[0]-1;
  int m   = dims[1]-1;
  int p   = uDegree;
  int q   = vDegree;
  int dir = decreaseDirection;

  int nuk = n+p+1;
  int nvk = n+p+1;
  int ph = p-1;
  int qh = q-1;

  if (dir == 0)
  {
    // Double check to see if correct vals were given
    if (uKnots->GetNumberOfTuples() != m+1)
    {
      fprintf(stderr,"Invalid number of control points given with knot span\n");
      return SV_ERROR;
    }

    // Compute s by cmputing the number of nonnon-repeated internal knot vals
    vtkNew(vtkIntArray, multiplicity);
    vtkNew(vtkDoubleArray, singleValues);
    vtkSVNURBSUtils::GetMultiplicity(uKnots, multiplicity, singleValues);

    // S s is length of mult vector minus 2 (p+1 ends)
    int s      = multiplicity->GetNumberOfTuples() - 2;
    int nukhat = nuk-(s+2);
    int nhat   = n-(s+1);

    // Set up new uKnots
    newUKnots->SetNumberOfTuples(nukhat+1);
    if (newVKnots != NULL && vKnots != NULL)
      newVKnots->DeepCopy(vKnots);

    // New control points, updated each time for new bezier curve
    newControlPoints->SetNumberOfControlPoints((nhat+1)*(m+1));
    newControlPoints->SetDimensions(nhat+1, m+1, 1);

    // Bezier control points, degree p
    vtkNew(vtkSVControlGrid, bpts);
    bpts->SetNumberOfControlPoints((p+1)*(m+1));
    bpts->SetDimensions(p+1, m+1, 1);

    // Bezier control points, degree p-1
    vtkNew(vtkSVControlGrid, rbpts);
    rbpts->SetNumberOfControlPoints((p)*(m+1));
    rbpts->SetDimensions(p, m+1, 1);

    // Leftmost control pionts of next Bezier segment
    vtkNew(vtkSVControlGrid, nextbpts);
    nextbpts->SetNumberOfControlPoints((p-1)*(m+1));
    rbpts->SetDimensions(p-1, m+1, 1);

    // Alpha values for knot insertion
    vtkNew(vtkDoubleArray, alphas);
    alphas->SetNumberOfTuples(p-1);

    // Error values
    vtkNew(vtkDoubleArray, error);
    error->SetNumberOfComponents(m+1);
    error->SetNumberOfTuples(nuk);
    error->FillComponent(0, 0.0);

    // Set up iter vars
    int nukh = ph;
    int kind = ph+1;
    int r    = -1;
    int a    = p;
    int b    = p+1;
    int cind = 1;
    int mult = p;

    // Pass the first control point
    for (int col=0; col<=m; col++)
    {
      double firstpw[4];
      PW->GetControlPoint(0, col, 0, firstpw);
      newControlPoints->SetControlPoint(0, col, 0, firstpw);
    }

    // Set unchanging knot points
    for (int i=0; i<=ph; i++)
      newUKnots->SetTuple1(i, uKnots->GetTuple1(0));

    // And first bezier points
    for (int i=0; i<=p; i++)
    {
      for (int col=0; col<=m; col++)
      {
        double pw[4];
        PW->GetControlPoint(i, col, 0, pw);
        bpts->SetControlPoint(i, col, 0, pw);
      }
    }

    // Loop through knot span
    while (b<nuk)
    {
      int i=b;
      while (b<nuk && uKnots->GetTuple1(b) == uKnots->GetTuple1(b+1))
        b++;
      int mult = b-i+1;
      nukh     = nukh+mult-1;
      int oldr = r;
      r = p-mult;

      // Set up left vars
      int lbz;
      if (oldr > 0)
        lbz = (oldr+2)/2;
      else
        lbz = 1;

      // Insert the knot r times
      if (r > 0)
      {
        double numer = uKnots->GetTuple1(b) - uKnots->GetTuple1(a);
        for (int k=p; k>mult; k--)
        {
          double newVal = numer/(uKnots->GetTuple1(a+k)-uKnots->GetTuple1(a));
          alphas->SetTuple1(k-mult-1, newVal);
        }
        for (int j=1; j<=r; j++)
        {
          int save = r-j;
          int mulj = mult+j;
          for (int k=p; k>=mulj; k--)
          {
            double alpha = alphas->GetTuple1(k-mulj);
            for (int col=0; col<=m; col++)
            {
              double pw0[4], pw1[4], newPoint[4];
              bpts->GetControlPoint(k, col, 0, pw0);
              bpts->GetControlPoint(k-1, col, 0, pw1);
              vtkSVMathUtils::Add(pw0, alpha, pw1, 1.0-alpha, 4, newPoint);
              bpts->SetControlPoint(k, col, 0, newPoint);
            }
          }
          for (int col=0; col<=m; col++)
          {
            double pw[4];
            bpts->GetControlPoint(p, col, 0, pw);
            nextbpts->SetControlPoint(save, col, 0, pw);
          }
        }
      }

      // Now reduce the degree
      for (int col=0; col<=m; col++)
      {
        double maxError = 0.0;
        vtkSVNURBSUtils::BezierNURBSDecreaseDegree(bpts, dir, maxError, rbpts);
        error->SetComponent(a, col, error->GetComponent(a, col)+maxError);

        if (error->GetComponent(a, col) > tolerance)
        {
          fprintf(stderr,"Error %.5f is greater than tolerance %.5f\n", error->GetComponent(a, col), tolerance);
          fprintf(stderr,"Curve is not reducable, before knot removal\n");
          return SV_ERROR;
        }
      }

      // Now remove uKnots
      if (oldr > 0)
      {
        int first = kind;
        int last  = kind;

        for (int k=0; k<oldr; k++)
        {
          i      = first;
          int j  = last;
          int kj = j-kind;

          while (j-i > k)
          {
            double alpha = (uKnots->GetTuple1(a)-newUKnots->GetTuple1(i-1))/
                           (uKnots->GetTuple1(b)-newUKnots->GetTuple1(i-1));
            double beta  = (uKnots->GetTuple1(a)-newUKnots->GetTuple1(j-k-1))/
                           (uKnots->GetTuple1(b)-newUKnots->GetTuple1(j-k-1));

            for (int col=0; col<=m; col++)
            {
              double pw0[4], pw1[4], newPoint0[4];
              newControlPoints->GetControlPoint(i-1, col, 0, pw0);
              newControlPoints->GetControlPoint(i-2, col, 0, pw1);
              vtkSVMathUtils::Add(pw0, 1.0, pw1, -1.0*(1.0-alpha), 4, newPoint0);
              vtkSVMathUtils::MultiplyScalar(newPoint0, alpha, 4);
              newControlPoints->SetControlPoint(i-1, col, 0, newPoint0);

              double pw2[4], pw3[4], newPoint1[4];
              rbpts->GetControlPoint(kj, col, 0, pw2);
              rbpts->GetControlPoint(kj+1, col, 0, pw3);
              vtkSVMathUtils::Add(pw2, 1.0, pw3, -1.0*beta, 4, newPoint1);
              vtkSVMathUtils::MultiplyScalar(newPoint1, 1.0-beta, 4);
              rbpts->SetControlPoint(kj, col, 0, newPoint1);
            }
            i++;
            j--;
            kj--;
          }
          // Compute error bounds
          vtkNew(vtkDoubleArray, eb);
          eb->SetNumberOfTuples(m+1);
          if (j-i < k)
          {
            for (int col=0; col<=m; col++)
            {
              double pw0[4], pw1[4];
              newControlPoints->GetControlPoint(i-2, col, 0, pw0);
              rbpts->GetControlPoint(kj+1, col, 0, pw1);
              eb->SetTuple1(col, vtkSVMathUtils::Distance(pw0, pw1, 4));
            }
          }
          else
          {
            double delta = (uKnots->GetTuple1(a)-newUKnots->GetTuple1(i-1))/
                           (uKnots->GetTuple1(b)-newUKnots->GetTuple1(i-1));

            for (int col=0; col<=m; col++)
            {
              double pw0[4], pw1[4], pw2[4], newPoint[4];
              rbpts->GetControlPoint(kj+1, col, 0, pw0);
              newControlPoints->GetControlPoint(i-2, col, 0, pw1);
              vtkSVMathUtils::Add(pw0, delta, pw1, 1.0-delta, 4, newPoint);

              newControlPoints->GetControlPoint(i-1, col, 0, pw2);
              eb->SetTuple1(col, vtkSVMathUtils::Distance(pw2, newPoint, 4));
            }
          }

          // Update errors
          int K = a+oldr-k;
          int q = (2*p-k+1)/2;
          int L = K-q;
          for (int ii=L; ii<=a; ii++)
          {
            for (int col=0; col<=m; col++)
            {
              error->SetComponent(ii, col, error->GetComponent(ii, col)+eb->GetTuple1(col));
              if (error->GetComponent(ii, col) > tolerance)
              {
                fprintf(stderr,"Error %.5f is greater than tolerance %.5f\n", error->GetComponent(ii, col), tolerance);
                fprintf(stderr,"Curve is not reducable, after knot removal\n");
                return SV_ERROR;
              }
            }
          }
          first--;
          last++;
        }
        cind = i-1;
      }

      // Update end of knots
      if (a != p)
      {
        for (i=0; i<ph-oldr; i++)
        {
          newUKnots->SetTuple1(kind, uKnots->GetTuple1(a));
          kind++;
        }
      }

      for (i=lbz; i<=ph; i++)
      {
        for (int col=0; col<=m; col++)
        {
          double pw[4];
          rbpts->GetControlPoint(i, col, 0, pw);
          newControlPoints->SetControlPoint(cind, col, 0, pw);
          cind++;
        }
      }

      // Now set up for next pass;
      if (b<nuk)
      {
        for (i=0; i<r; i++)
        {
          for (int col=0; col<=m; col++)
          {
            double pw[4];
            nextbpts->GetControlPoint(i, col, 0, pw);
            bpts->SetControlPoint(i, col, 0, pw);
          }
        }
        for (i=0; i<=p; i++)
        {
          for (int col=0; col<=m; col++)
          {
            double pw[4];
            PW->GetControlPoint(b-p+i, col, 0, pw);
            bpts->SetControlPoint(i, col, 0, pw);
          }
        }
        a=b;
        b++;
      }
      else
      {
        // Update remaining knots
        for (i=0; i<=ph; i++)
          newUKnots->SetTuple1(kind+i, uKnots->GetTuple1(b));
      }
    }
    int nh = nukh-ph-1;

    if (nukh != nukhat)
    {
      fprintf(stderr,"Something went wrong: nukhat %d does not equal iter nukh %d\n", nukhat, nukh);
      return SV_ERROR;
    }
    if (nh != nhat)
    {
      fprintf(stderr,"Something went wrong: nhat %d does not equal iter n %d\n", nhat, nh);
      return SV_ERROR;
    }
  }
  else if (dir == 1)
  {
    // Double check to see if correct vals were given
    if (vKnots->GetNumberOfTuples() != n+1)
    {
      fprintf(stderr,"Invalid number of control points given with knot span\n");
      return SV_ERROR;
    }

    // Compute s by cmputing the number of nonnon-repeated internal knot vals
    vtkNew(vtkIntArray, multiplicity);
    vtkNew(vtkDoubleArray, singleValues);
    vtkSVNURBSUtils::GetMultiplicity(uKnots, multiplicity, singleValues);

    // S s is length of mult vector minus 2 (p+1 ends)
    int s      = multiplicity->GetNumberOfTuples() - 2;
    int nvkhat = nvk-(s+2);
    int mhat   = m-(s+1);

    // Set up new uKnots
    newUKnots->SetNumberOfTuples(nvkhat+1);
    if (newVKnots != NULL && vKnots != NULL)
      newVKnots->DeepCopy(vKnots);

    // New control points, updated each time for new bezier curve
    newControlPoints->SetNumberOfControlPoints((mhat+1)*(n+1));
    newControlPoints->SetDimensions(n+1, mhat+1, 1);

    // Bezier control points, degree p
    vtkNew(vtkSVControlGrid, bpts);
    bpts->SetNumberOfControlPoints((q+1)*(n+1));
    bpts->SetDimensions(n+1, q+1, 1);

    // Bezier control points, degree p-1
    vtkNew(vtkSVControlGrid, rbpts);
    rbpts->SetNumberOfControlPoints((q)*(n+1));
    rbpts->SetDimensions(n+q, q, 1);

    // Leftmost control pionts of next Bezier segment
    vtkNew(vtkSVControlGrid, nextbpts);
    nextbpts->SetNumberOfControlPoints((q-1)*(n+1));
    rbpts->SetDimensions(n+1, q-1, 1);

    // Alpha values for knot insertion
    vtkNew(vtkDoubleArray, alphas);
    alphas->SetNumberOfTuples(q-1);

    // Error values
    vtkNew(vtkDoubleArray, error);
    error->SetNumberOfComponents(n+1);
    error->SetNumberOfTuples(nvk);
    error->FillComponent(0, 0.0);

    // Set up iter vars
    int nvkh = qh;
    int kind = qh+1;
    int r    = -1;
    int a    = q;
    int b    = q+1;
    int cind = 1;
    int mult = q;

    // Pass the first control point
    for (int row=0; row<=n; row++)
    {
      double firstpw[4];
      PW->GetControlPoint(row, 0, 0, firstpw);
      newControlPoints->SetControlPoint(row, 0, 0, firstpw);
    }

    // Set unchanging knot points
    for (int i=0; i<=qh; i++)
      newUKnots->SetTuple1(i, uKnots->GetTuple1(0));

    // And first bezier points
    for (int i=0; i<=q; i++)
    {
      for (int row=0; row<=n; row++)
      {
        double pw[4];
        PW->GetControlPoint(row, i, 0, pw);
        bpts->SetControlPoint(row, i, 0, pw);
      }
    }

    // Loop through knot span
    while (b<nvk)
    {
      int i=b;
      while (b<nvk && uKnots->GetTuple1(b) == uKnots->GetTuple1(b+1))
        b++;
      int mult = b-i+1;
      nvkh     = nvkh+mult-1;
      int oldr = r;
      r = q-mult;

      // Set up left vars
      int lbz;
      if (oldr > 0)
        lbz = (oldr+2)/2;
      else
        lbz = 1;

      // Insert the knot r times
      if (r > 0)
      {
        double numer = uKnots->GetTuple1(b) - uKnots->GetTuple1(a);
        for (int k=q; k>mult; k--)
        {
          double newVal = numer/(uKnots->GetTuple1(a+k)-uKnots->GetTuple1(a));
          alphas->SetTuple1(k-mult-1, newVal);
        }
        for (int j=1; j<=r; j++)
        {
          int save = r-j;
          int mulj = mult+j;
          for (int k=q; k>=mulj; k--)
          {
            double alpha = alphas->GetTuple1(k-mulj);
            for (int row=0; row<=n; row++)
            {
              double pw0[4], pw1[4], newPoint[4];
              bpts->GetControlPoint(row, k, 0, pw0);
              bpts->GetControlPoint(row, k-1, 0, pw1);
              vtkSVMathUtils::Add(pw0, alpha, pw1, 1.0-alpha, 4, newPoint);
              bpts->SetControlPoint(row, k, 0, newPoint);
            }
          }
          for (int row=0; row<=n; row++)
          {
            double pw[4];
            bpts->GetControlPoint(row, q, 0, pw);
            nextbpts->SetControlPoint(row, save, 0, pw);
          }
        }
      }

      // Now reduce the degree
      for (int row=0; row<=n; row++)
      {
        double maxError = 0.0;
        vtkSVNURBSUtils::BezierNURBSDecreaseDegree(bpts, dir, maxError, rbpts);
        error->SetComponent(a, row, error->GetComponent(a, row)+maxError);

        if (error->GetComponent(a, row) > tolerance)
        {
          fprintf(stderr,"Error %.5f is greater than tolerance %.5f\n", error->GetComponent(a, row), tolerance);
          fprintf(stderr,"Curve is not reducable, before knot removal\n");
          return SV_ERROR;
        }
      }

      // Now remove uKnots
      if (oldr > 0)
      {
        int first = kind;
        int last  = kind;

        for (int k=0; k<oldr; k++)
        {
          i      = first;
          int j  = last;
          int kj = j-kind;

          while (j-i > k)
          {
            double alpha = (uKnots->GetTuple1(a)-newUKnots->GetTuple1(i-1))/
                           (uKnots->GetTuple1(b)-newUKnots->GetTuple1(i-1));
            double beta  = (uKnots->GetTuple1(a)-newUKnots->GetTuple1(j-k-1))/
                           (uKnots->GetTuple1(b)-newUKnots->GetTuple1(j-k-1));

            for (int row=0; row<=n; row++)
            {
              double pw0[4], pw1[4], newPoint0[4];
              newControlPoints->GetControlPoint(row, i-1, 0, pw0);
              newControlPoints->GetControlPoint(row, i-2, 0, pw1);
              vtkSVMathUtils::Add(pw0, 1.0, pw1, -1.0*(1.0-alpha), 4, newPoint0);
              vtkSVMathUtils::MultiplyScalar(newPoint0, alpha, 4);
              newControlPoints->SetControlPoint(row, i-1, 0, newPoint0);

              double pw2[4], pw3[4], newPoint1[4];
              rbpts->GetControlPoint(row, kj, 0, pw2);
              rbpts->GetControlPoint(row, kj+1, 0, pw3);
              vtkSVMathUtils::Add(pw2, 1.0, pw3, -1.0*beta, 4, newPoint1);
              vtkSVMathUtils::MultiplyScalar(newPoint1, 1.0-beta, 4);
              rbpts->SetControlPoint(row, kj, 0, newPoint1);
            }
            i++;
            j--;
            kj--;
          }
          // Compute error bounds
          vtkNew(vtkDoubleArray, eb);
          eb->SetNumberOfTuples(n+1);
          if (j-i < k)
          {
            for (int row=0; row<=n; row++)
            {
              double pw0[4], pw1[4];
              newControlPoints->GetControlPoint(row, i-2, 0, pw0);
              rbpts->GetControlPoint(row, kj+1, 0, pw1);
              eb->SetTuple1(row, vtkSVMathUtils::Distance(pw0, pw1, 4));
            }
          }
          else
          {
            double delta = (uKnots->GetTuple1(a)-newUKnots->GetTuple1(i-1))/
                           (uKnots->GetTuple1(b)-newUKnots->GetTuple1(i-1));

            for (int row=0; row<=n; row++)
            {
              double pw0[4], pw1[4], pw2[4], newPoint[4];
              rbpts->GetControlPoint(row, kj+1, 0, pw0);
              newControlPoints->GetControlPoint(row, i-2, 0, pw1);
              vtkSVMathUtils::Add(pw0, delta, pw1, 1.0-delta, 4, newPoint);

              newControlPoints->GetControlPoint(row, i-1, 0, pw2);
              eb->SetTuple1(row, vtkSVMathUtils::Distance(pw2, newPoint, 4));
            }
          }

          // Update errors
          int K = a+oldr-k;
          int q = (2*q-k+1)/2;
          int L = K-q;
          for (int ii=L; ii<=a; ii++)
          {
            for (int row=0; row<=n; row++)
            {
              error->SetComponent(ii, row, error->GetComponent(ii, m)+eb->GetTuple1(row));
              if (error->GetComponent(ii, row) > tolerance)
              {
                fprintf(stderr,"Error %.5f is greater than tolerance %.5f\n", error->GetComponent(ii, row), tolerance);
                fprintf(stderr,"Curve is not reducable, after knot removal\n");
                return SV_ERROR;
              }
            }
          }
          first--;
          last++;
        }
        cind = i-1;
      }

      // Update end of knots
      if (a != q)
      {
        for (i=0; i<qh-oldr; i++)
        {
          newUKnots->SetTuple1(kind, uKnots->GetTuple1(a));
          kind++;
        }
      }

      for (i=lbz; i<=qh; i++)
      {
        for (int row=0; row<=n; row++)
        {
          double pw[4];
          rbpts->GetControlPoint(row, i, 0, pw);
          newControlPoints->SetControlPoint(row, cind, 0, pw);
          cind++;
        }
      }


      // Now set up for next pass;
      if (b<nvk)
      {
        for (i=0; i<r; i++)
        {
          for (int row=0; row<=n; row++)
          {
            double pw[4];
            nextbpts->GetControlPoint(row, i, 0, pw);
            bpts->SetControlPoint(row, i, 0, pw);
          }
        }
        for (i=0; i<=q; i++)
        {
          for (int row=0; row<=n; row++)
          {
            double pw[4];
            PW->GetControlPoint(row, b-q+i, 0, pw);
            bpts->SetControlPoint(row, i, 0, pw);
          }
        }
        a=b;
        b++;
      }
      else
      {
        // Update remaining knots
        for (i=0; i<=qh; i++)
          newUKnots->SetTuple1(kind+i, uKnots->GetTuple1(b));
      }
    }
    int mh = nvkh-qh-1;

    if (nvkh != nvkhat)
    {
      fprintf(stderr,"Something went wrong: nvkhat %d does not equal iter nvkh %d\n", nvkhat, nvkh);
      return SV_ERROR;
    }
    if (mh != mhat)
    {
      fprintf(stderr,"Something went wrong: mhat %d does not equal iter n %d\n", mhat, mh);
      return SV_ERROR;
    }
  }

  // Realy quick, convert everything back to just p
  vtkSVNURBSUtils::GetPFromPW(newControlPoints);

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

  int nUCon = pointMatFinal->GetExtents()[0].GetSize();
  int nVCon = pointMatFinal->GetExtents()[1].GetSize();

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
  if (vtkSVNURBSUtils::MatrixMatrixMultiply(NPUinv, 0, 1, pointMatFinal, 1, 3, tmpUGrid) != SV_OK)
  {
    fprintf(stderr, "Error in matrix multiply\n");
    return SV_ERROR;
  }
  vtkNew(vtkDenseArray<double>, tmpUGridT);
  vtkSVNURBSUtils::MatrixTranspose(tmpUGrid, 1, tmpUGridT);
  vtkNew(vtkDenseArray<double>, tmpVGrid);
  if (vtkSVNURBSUtils::MatrixMatrixMultiply(NPVinv, 0, 1, tmpUGridT, 1, 3, tmpVGrid) != SV_OK)
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
// GetControlPointOfVolume
// ----------------------
int vtkSVNURBSUtils::GetControlPointsOfVolume(vtkStructuredGrid *points,
                                              vtkDoubleArray *U,
                                              vtkDoubleArray *V, vtkDoubleArray *W,
                                              vtkDoubleArray *uWeights,
                                              vtkDoubleArray *vWeights,
                                              vtkDoubleArray *wWeights,
                                              vtkDoubleArray *uKnots,
                                              vtkDoubleArray *vKnots,
                                              vtkDoubleArray *wKnots,
                                              const int p, const int q,
                                              const int r,
                                              std::string kutype,
                                              std::string kvtype,
                                              std::string kwtype,
                                              vtkStructuredGrid *DU0,
                                              vtkStructuredGrid *DUN,
                                              vtkStructuredGrid *DV0,
                                              vtkStructuredGrid *DVN,
                                              vtkStructuredGrid *DW0,
                                              vtkStructuredGrid *DWN,
                                              vtkStructuredGrid *cPoints)
{
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

  vtkNew(vtkSparseArray<double>, NPWTmp);
  vtkNew(vtkSparseArray<double>, NPWFinal);
  if( vtkSVNURBSUtils::GetPBasisFunctions(W, wKnots, r, NPWTmp) != SV_OK)
  {
    return SV_ERROR;
  }
  NPWTmp->SetValue(NPWTmp->GetExtents()[0].GetSize()-1, NPWTmp->GetExtents()[1].GetSize()-1, 1.0);

  vtkNew(vtkDenseArray<double>, pointMatTmp);
  vtkNew(vtkDenseArray<double>, pointMatFinal);
  vtkSVNURBSUtils::StructuredGridToTypedArray(points, pointMatTmp);

  if (!strncmp(kutype.c_str(), "derivative", 10)
      || !strncmp(kvtype.c_str(), "derivative", 10)
      || !strncmp(kwtype.c_str(), "derivative", 10))
  {
    vtkNew(vtkDenseArray<double>, DU0Mat);
    vtkSVNURBSUtils::StructuredGridToTypedArray(DU0, DU0Mat);
    vtkNew(vtkDenseArray<double>, DUNMat);
    vtkSVNURBSUtils::StructuredGridToTypedArray(DUN, DUNMat);
    vtkNew(vtkDenseArray<double>, DV0Mat);
    vtkSVNURBSUtils::StructuredGridToTypedArray(DV0, DV0Mat);
    vtkNew(vtkDenseArray<double>, DVNMat);
    vtkSVNURBSUtils::StructuredGridToTypedArray(DVN, DVNMat);
    vtkNew(vtkDenseArray<double>, DW0Mat);
    vtkSVNURBSUtils::StructuredGridToTypedArray(DW0, DW0Mat);
    vtkNew(vtkDenseArray<double>, DWNMat);
    vtkSVNURBSUtils::StructuredGridToTypedArray(DWN, DWNMat);
    vtkSVNURBSUtils::SetVolumeEndDerivatives(NPUTmp, NPVTmp, NPWTmp, pointMatTmp,
                                             p, q, r, kutype, kvtype, kwtype,
                                             DU0Mat, DUNMat,  DV0Mat, DVNMat,
                                             DW0Mat, DWNMat, U, V, W,
                                             uKnots, vKnots, wKnots,
                                             NPUFinal, NPVFinal, NPWFinal, pointMatFinal);
  }
  else
  {
    vtkSVNURBSUtils::DeepCopy(NPUTmp, NPUFinal);
    vtkSVNURBSUtils::DeepCopy(NPVTmp, NPVFinal);
    vtkSVNURBSUtils::DeepCopy(NPWTmp, NPWFinal);
    vtkSVNURBSUtils::DeepCopy(pointMatTmp, pointMatFinal);
  }

  int nUCon = pointMatFinal->GetExtents()[0].GetSize();
  int nVCon = pointMatFinal->GetExtents()[1].GetSize();
  int nWCon = pointMatFinal->GetExtents()[2].GetSize();

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

  //fprintf(stdout,"Basis functions W:\n");
  //vtkSVNURBSUtils::PrintMatrix(NPWFinal);
  vtkNew(vtkSparseArray<double>, NPWinv);
  if (vtkSVNURBSUtils::InvertSystem(NPWFinal, NPWinv) != SV_OK)
  {
    fprintf(stderr,"System could not be inverted\n");
    return SV_ERROR;
  }

  //fprintf(stdout,"Inverted system U:\n");
  //vtkSVNURBSUtils::PrintMatrix(NPUinv);
  //fprintf(stdout,"Inverted system V:\n");
  //vtkSVNURBSUtils::PrintMatrix(NPVinv);
  //fprintf(stdout,"Inverted system W:\n");
  //vtkSVNURBSUtils::PrintMatrix(NPWinv);

  vtkNew(vtkSparseArray<double>, NPVinvT);
  vtkSVNURBSUtils::MatrixTranspose(NPVinv, 0, NPVinvT);

  vtkNew(vtkSparseArray<double>, NPWinvT);
  vtkSVNURBSUtils::MatrixTranspose(NPWinv, 0, NPWinvT);

  int numUDiv = NPUinv->GetExtents()[0].GetSize();
  int numVDiv = NPVinv->GetExtents()[0].GetSize();
  int numWDiv = NPWinv->GetExtents()[0].GetSize();

  vtkArrayExtents size;
  size.SetDimensions(4);
  size.SetExtent(0, vtkArrayRange(0, numUDiv));
  size.SetExtent(1, vtkArrayRange(0, numVDiv));
  size.SetExtent(2, vtkArrayRange(0, nWCon));
  size.SetExtent(3, vtkArrayRange(0, 3));
  vtkNew(vtkDenseArray<double>, tmpW);
  tmpW->Resize(size);

  for (int i=0; i<nWCon; i++)
  {
    vtkNew(vtkDenseArray<double>, tmpWMat);
    vtkSVNURBSUtils::GetMatrixOfDim4Grid(pointMatFinal, 0, 1, 2, i, 3, tmpWMat);


    vtkNew(vtkDenseArray<double>, tmpUGrid);
    if (vtkSVNURBSUtils::MatrixMatrixMultiply(NPUinv, 0, 1, tmpWMat, 1, 3, tmpUGrid) != SV_OK)
    {
      fprintf(stderr, "Error in matrix multiply\n");
      return SV_ERROR;
    }
    vtkNew(vtkDenseArray<double>, tmpVGrid);
    if (vtkSVNURBSUtils::MatrixMatrixMultiply(tmpUGrid, 1, 3, NPVinvT, 0, 1, tmpVGrid) != SV_OK)
    {
      fprintf(stderr, "Error in matrix multiply\n");
      return SV_ERROR;
    }

    vtkSVNURBSUtils::SetMatrixOfDim4Grid(tmpVGrid, tmpW, 0, 1, 2, i, 3);
  }

  size.SetExtent(0, vtkArrayRange(0, numUDiv));
  size.SetExtent(1, vtkArrayRange(0, numVDiv));
  size.SetExtent(2, vtkArrayRange(0, numWDiv));
  size.SetExtent(3, vtkArrayRange(0, 3));
  vtkNew(vtkDenseArray<double>, fullGrid);
  fullGrid->Resize(size);

  for (int i=0; i<numUDiv; i++)
  {
    vtkNew(vtkDenseArray<double>, tmpWGrid);
    vtkSVNURBSUtils::GetMatrixOfDim4Grid(tmpW, 1, 2, 0, i, 3, tmpWGrid);

    // Do second matrix multiply with v basis functions
    vtkNew(vtkDenseArray<double>, tmpVWGrid);
    if (vtkSVNURBSUtils::MatrixMatrixMultiply(tmpWGrid, 1, 3, NPWinvT, 0, 1, tmpVWGrid) != SV_OK)
    {
      fprintf(stderr, "Error in matrix multiply\n");
      return SV_ERROR;
    }

    vtkSVNURBSUtils::SetMatrixOfDim4Grid(tmpVWGrid, fullGrid, 1, 2, 0, i, 3);
  }

  vtkNew(vtkPoints, finalPoints);
  cPoints->SetPoints(finalPoints);
  vtkSVNURBSUtils::TypedArrayToStructuredGrid(fullGrid, cPoints);
  //fprintf(stdout,"Final structured grid of control points\n");
  //vtkSVNURBSUtils::PrintStructuredGrid(cPoints);

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
// SetVolumeEndDerivatives
// ----------------------
int vtkSVNURBSUtils::SetVolumeEndDerivatives(vtkTypedArray<double> *NPU,
                                             vtkTypedArray<double> *NPV,
                                             vtkTypedArray<double> *NPW,
                                             vtkTypedArray<double> *points,
                                             const int p,  const int q, const int r,
                                             std::string kutype, std::string kvtype,
                                             std::string kwtype,
                                             vtkTypedArray<double> *DU0,
                                             vtkTypedArray<double> *DUN,
                                             vtkTypedArray<double> *DV0,
                                             vtkTypedArray<double> *DVN,
                                             vtkTypedArray<double> *DW0,
                                             vtkTypedArray<double> *DWN,
                                             vtkDoubleArray *U,
                                             vtkDoubleArray *V,
                                             vtkDoubleArray *W,
                                             vtkDoubleArray *uKnots,
                                             vtkDoubleArray *vKnots,
                                             vtkDoubleArray *wKnots,
                                             vtkTypedArray<double> *newNPU,
                                             vtkTypedArray<double> *newNPV,
                                             vtkTypedArray<double> *newNPW,
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
  if (!strncmp(kwtype.c_str(), "derivative", 10))
  {
    vtkSVNURBSUtils::AddDerivativeRows(NPW, newNPW, r, wKnots);
  }
  else
  {
    vtkSVNURBSUtils::DeepCopy(NPW, newNPW);
  }

  int nUKnot = uKnots->GetNumberOfTuples();
  int nVKnot = vKnots->GetNumberOfTuples();
  int nWKnot = wKnots->GetNumberOfTuples();
  int nu = nUKnot - (p + 1);
  int nv = nVKnot - (q + 1);
  int nw = nWKnot - (r + 1);
  vtkArrayExtents size;
  size.SetDimensions(4);
  size.SetExtent(0, vtkArrayRange(0, nu));
  size.SetExtent(1, vtkArrayRange(0, nv));
  size.SetExtent(2, vtkArrayRange(0, nw));
  size.SetExtent(3, vtkArrayRange(0, 3));
  newPoints->Resize(size);

  int npu = points->GetExtents()[0].GetSize();
  int npv = points->GetExtents()[1].GetSize();
  int npw = points->GetExtents()[2].GetSize();
  vtkNew(vtkDenseArray<double>, tmp0Points);
  vtkNew(vtkDenseArray<double>, tmp1Points);
  vtkNew(vtkDenseArray<double>, tmp2Points);
  vtkNew(vtkDenseArray<double>, tmp3Points);
  vtkNew(vtkDenseArray<double>, tmp4Points);
  vtkNew(vtkDenseArray<double>, tmp5Points);
  vtkNew(vtkDenseArray<double>, tmp6Points);
  vtkNew(vtkDenseArray<double>, tmp7Points);
  if (!strncmp(kutype.c_str(), "derivative", 10))
  {
    vtkArrayExtents size2;
    size2.SetDimensions(4);
    size2.SetExtent(0, vtkArrayRange(0, nu));
    size2.SetExtent(1, vtkArrayRange(0, npv));
    size2.SetExtent(2, vtkArrayRange(0, npw));
    size2.SetExtent(3, vtkArrayRange(0, 3));
    tmp2Points->Resize(size2);

    for (int i=0; i<npv; i++)
    {
      vtkSVNURBSUtils::GetMatrixOfDim4Grid(points, 0, 2, 1, i, 3, tmp0Points);
      vtkNew(vtkDenseArray<double>, tmpMat);
      tmpMat->Resize(nu, npw, 3);

      for (int j=0; j<npw; j++)
      {
        double du0[3], duN[3];
        for (int k=0; k<3; k++)
        {
          du0[k] = DU0->GetValue(i, j, k);
          duN[k] = DUN->GetValue(i, j, k);
        }

        vtkNew(vtkDenseArray<double>, tmpMatComp);
        vtkSVNURBSUtils::GetMatrixComp(tmp0Points, j, 0, 1, tmpMatComp);
        vtkSVNURBSUtils::AddDerivativePoints(tmpMatComp, p, du0, duN, U, uKnots, tmp1Points);
        vtkSVNURBSUtils::SetMatrixComp(tmp1Points, j, 0, 1, tmpMat);
      }
      vtkSVNURBSUtils::SetMatrixOfDim4Grid(tmpMat, tmp2Points, 0, 2, 1, i, 3);
    }
    npu += 2;
  }
  else
  {
    vtkSVNURBSUtils::DeepCopy(points, tmp2Points);
  }

  if (!strncmp(kvtype.c_str(), "derivative", 10))
  {
    vtkArrayExtents size5;
    size5.SetDimensions(4);
    size5.SetExtent(0, vtkArrayRange(0, npu));
    size5.SetExtent(1, vtkArrayRange(0, nv));
    size5.SetExtent(2, vtkArrayRange(0, npw));
    size5.SetExtent(3, vtkArrayRange(0, 3));
    tmp5Points->Resize(size5);

    for (int i=0; i<npw; i++)
    {
      vtkSVNURBSUtils::GetMatrixOfDim4Grid(tmp2Points, 1, 0, 2, i, 3, tmp3Points);
      vtkNew(vtkDenseArray<double>, tmpMat);
      tmpMat->Resize(nv, npu, 3);

      int count = 0;
      for (int j=0; j<npu; j++)
      {
        double dv0[3], dvN[3];
        if ((j == 1 || j == nu - 2) && !strncmp(kutype.c_str(), "derivative", 10))
        {
          for (int k=0; k<3; k++)
          {
            dv0[k] = 0.0;
            dvN[k] = 0.0;
          }
        }
        else
        {
          for (int k=0; k<3; k++)
          {
            dv0[k] = DV0->GetValue(i, count, k);
            dvN[k] = DVN->GetValue(i, count, k);
          }
          count++;
        }

        vtkNew(vtkDenseArray<double>, tmpMatComp);
        vtkSVNURBSUtils::GetMatrixComp(tmp3Points, j, 0, 1, tmpMatComp);
        vtkSVNURBSUtils::AddDerivativePoints(tmpMatComp, q, dv0, dvN, V, vKnots, tmp4Points);
        vtkSVNURBSUtils::SetMatrixComp(tmp4Points, j, 0, 1, tmpMat);
      }
      vtkSVNURBSUtils::SetMatrixOfDim4Grid(tmpMat, tmp5Points, 1, 0, 2, i, 3);
    }
    npv += 2;
  }
  else
  {
    vtkSVNURBSUtils::DeepCopy(tmp2Points, tmp5Points);
  }

  if (!strncmp(kwtype.c_str(), "derivative", 10))
  {
    int uCount = 0;
    for (int i=0; i<npu; i++)
    {
      vtkSVNURBSUtils::GetMatrixOfDim4Grid(tmp5Points, 2, 1, 0, i, 3, tmp6Points);
      vtkNew(vtkDenseArray<double>, tmpMat);
      tmpMat->Resize(nw, npv, 3);

      int vCount = 0;
      for (int j=0; j<npv; j++)
      {
        double dw0[3], dwN[3];
        if ((i == 1 || i == nu - 2) && !strncmp(kutype.c_str(), "derivative", 10))
        {
          for (int k=0; k<3; k++)
          {
            dw0[k] = 0.0;
            dwN[k] = 0.0;
          }
        }
        else if ((j == 1 || j == nv - 2) && !strncmp(kvtype.c_str(), "derivative", 10))
        {
          for (int k=0; k<3; k++)
          {
            dw0[k] = 0.0;
            dwN[k] = 0.0;
          }
        }
        else
        {
          for (int k=0; k<3; k++)
          {
            dw0[k] = DW0->GetValue(uCount, vCount, k);
            dwN[k] = DWN->GetValue(uCount, vCount, k);
          }
        }

        if (!((j == 1 || j == nv - 2) && !strncmp(kvtype.c_str(), "derivative", 10)))
          vCount++;

        vtkNew(vtkDenseArray<double>, tmpMatComp);
        vtkSVNURBSUtils::GetMatrixComp(tmp6Points, j, 0, 1, tmpMatComp);
        vtkSVNURBSUtils::AddDerivativePoints(tmpMatComp, r, dw0, dwN, W, wKnots, tmp7Points);
        vtkSVNURBSUtils::SetMatrixComp(tmp7Points, j, 0, 1, tmpMat);
      }
      vtkSVNURBSUtils::SetMatrixOfDim4Grid(tmpMat, newPoints, 2, 1, 0, i, 3);

      if (!((i == 1 || i == nu - 2) && !strncmp(kutype.c_str(), "derivative", 10)))
        uCount++;
    }
    npw += 2;
  }
  else
  {
    vtkSVNURBSUtils::DeepCopy(tmp5Points, newPoints);
  }

  return SV_OK;
}

// ----------------------
// InsertKnot
// ----------------------
int vtkSVNURBSUtils::InsertKnot(vtkSVControlGrid *controlPoints,
                                vtkDoubleArray *uKnots, const int uDegree,
                                vtkDoubleArray *vKnots, const int vDegree,
                                const int insertDirection,
                                const double insertValue, const int span,
                                const int currentMultiplicity,
                                const int numberOfInserts,
                                vtkSVControlGrid *newControlPoints,
                                vtkDoubleArray *newUKnots, vtkDoubleArray *newVKnots)
{
  // Get dimensions of control point grid
  int dims[3];
  controlPoints->GetDimensions(dims);

  vtkNew(vtkSVControlGrid, PW);
  vtkSVNURBSUtils::GetPWFromP(controlPoints, PW);

  // Set values used by alg to more concise vars
  int np   = dims[0]-1;
  int p    = uDegree;
  int mp   = dims[1]-1;
  int q    = vDegree;
  int dir  = insertDirection;
  double u = insertValue;
  int k    = span;
  int s    = currentMultiplicity;
  int r    = numberOfInserts;

  // Can't possibly add more knots at location than the degree of the curve
  if ( (r+s) > p && dir == 0)
  {
    fprintf(stderr, "Error: number of inserts and current multiplicity cannot exceed the degree of the curve\n");
  }
  if ( (r+s) > q && dir == 1)
  {
    fprintf(stderr, "Error: number of inserts and current multiplicity cannot exceed the degree of the curve\n");
  }

  // Double check to see if correct vals were given
  if ((np+1)*(mp+1) != PW->GetNumberOfPoints())
  {
    fprintf(stderr,"Invalid number of control points in u or v direction given\n");
    return SV_ERROR;
  }

  if (dir == 0)
  {
    // number of knots
    int nuk = np+p+1;
    int nup  = np+r;

    // Set output number of points and knots
    newControlPoints->SetNumberOfControlPoints((nup+1)*(mp+1));
    newControlPoints->SetDimensions(nup+1, mp+1, 1);
    newUKnots->SetNumberOfTuples(nuk+r+1);

    // Double check to see if correct vals were given
    if (uKnots->GetNumberOfTuples() != nuk+1)
    {
      fprintf(stderr,"Invalid number of control points given with knot span\n");
      return SV_ERROR;
    }

    vtkNew(vtkDoubleArray, tmpPoints);
    tmpPoints->SetNumberOfComponents(4);
    tmpPoints->SetNumberOfTuples(p+1);
    // Set unchanging knots
    for (int i=0; i<=k; i++)
      newUKnots->SetTuple1(i, uKnots->GetTuple1(i));

    // Set the new knot r times
    for (int i=1; i<=r; i++)
      newUKnots->SetTuple1(k+i, u);

    // Set the rest of the new knot span
    for (int i=k+1; i<=nuk; i++)
      newUKnots->SetTuple1(i+r, uKnots->GetTuple1(i));

    // Copy the v knot vector to output
    if (newVKnots != NULL && vKnots != NULL)
      newVKnots->DeepCopy(vKnots);

    // Set up alpha array
    double **alpha = new double*[p-s];
    for (int i=0; i<p-s; i++)
      alpha[i] = new double[r+1];

    // Preload all the alphas
    for (int j=1; j<=r; j++)
    {
      int L = k-p+j;
      for (int i=0; i<=p-j-s; i++)
        alpha[i][j] = (u - uKnots->GetTuple1(L+i))/(uKnots->GetTuple1(i+k+1)-uKnots->GetTuple1(L+i));
    }

    // Insert knot each column
    for (int col=0; col<=mp; col++)
    {
      // Set unchanging control points
      for (int i=0; i<=k-p; i++)
      {
        double pw[4];
        PW->GetControlPoint(i, col, 0, pw);
        newControlPoints->SetControlPoint(i, col, 0, pw);
      }

      for (int i=k-s; i<=np; i++)
      {
        double pw[4];
        PW->GetControlPoint(i, col, 0, pw);
        newControlPoints->SetControlPoint(i+r, col, 0, pw);
      }

      // Set up tmp points
      for (int i=0; i<=p-s; i++)
      {
        double pw[4];
        PW->GetControlPoint(k-p+i, col, 0, pw);
        tmpPoints->SetTuple(i, pw);
      }

      // Insert the new knot r times
      int L=0;
      for (int j=1; j<=r; j++)
      {
        L = k-p+j;
        for (int i=0; i<=p-j-s; i++)
        {
          double pt0[4], pt1[4], newPoint[4];
          tmpPoints->GetTuple(i+1, pt0);
          tmpPoints->GetTuple(i, pt1);
          vtkSVMathUtils::Add(pt0, alpha[i][j], pt1, 1.0-alpha[i][j], 4, newPoint);
          tmpPoints->SetTuple(i, newPoint);

          // Fill the control point grid with the tmp points
          newControlPoints->SetControlPoint(L, col, 0, tmpPoints->GetTuple(0));
          newControlPoints->SetControlPoint(k+r-j-s, col, 0, tmpPoints->GetTuple(p-j-s));
        }
      }
      for (int i=L+1; i<k-s; i++)
        newControlPoints->SetControlPoint(i, col, 0, tmpPoints->GetTuple(i-L));

    }

    // Clean up alphas
    for (int i=0; i<p-s; i++)
      delete [] alpha[i];
    delete [] alpha;

  }
  // Other dir!!
  else if (dir == 1)
  {
    // number of knots
    int nuk = mp+q+1;
    int nup  = mp+r;

    // Set output number of points and knots
    newControlPoints->SetNumberOfControlPoints((nup+1)*(np+1));
    newControlPoints->SetDimensions(np+1, nup+1, 1);
    newVKnots->SetNumberOfTuples(nuk+r+1);

    // Double check to see if correct vals were given
    if (vKnots->GetNumberOfTuples() != nuk+1)
    {
      fprintf(stderr,"Invalid number of control points given with knot span\n");
      return SV_ERROR;
    }

    vtkNew(vtkDoubleArray, tmpPoints);
    tmpPoints->SetNumberOfComponents(4);
    tmpPoints->SetNumberOfTuples(p+1);
    // Set unchanging knots
    for (int i=0; i<=k; i++)
      newVKnots->SetTuple1(i, vKnots->GetTuple1(i));

    // Set the new knot r times
    for (int i=1; i<=r; i++)
      newVKnots->SetTuple1(k+i, u);

    // Set the rest of the new knot span
    for (int i=k+1; i<=nuk; i++)
      newVKnots->SetTuple1(i+r, vKnots->GetTuple1(i));

    // Copy the u knot vector to output
    if (newUKnots != NULL && uKnots != NULL)
      newUKnots->DeepCopy(uKnots);

    // Set up alpha array
    double **alpha = new double*[q-s];
    for (int i=0; i<q-s; i++)
      alpha[i] = new double[r+1];

    // Preload all the alphas
    for (int j=1; j<=r; j++)
    {
      int L = k-q+j;
      for (int i=0; i<=q-j-s; j++)
        alpha[i][j] = (u - vKnots->GetTuple1(L+i))/(vKnots->GetTuple1(i+k+1)-vKnots->GetTuple1(L+i));
    }

    // Insert knot each row
    for (int row=0; row<=np; row++)
    {
      // Set unchanging control points
      for (int i=0; i<=k-q; i++)
      {
        double pw[4];
        PW->GetControlPoint(row, i, 0, pw);
        newControlPoints->SetControlPoint(row, i, 0, pw);
      }

      for (int i=k-s; i<=mp; i++)
      {
        double pw[4];
        PW->GetControlPoint(row, i, 0, pw);
        newControlPoints->SetControlPoint(row, i+r, 0, pw);
      }

      // Set up tmp points
      for (int i=0; i<=q-s; i++)
      {
        double pw[4];
        PW->GetControlPoint(row, k-q+i, 0, pw);
        tmpPoints->SetTuple(i, pw);
      }

      // Insert the new knot r times
      int L=0;
      for (int j=1; j<=r; j++)
      {
        L = k-q+j;
        for (int i=0; i<=q-j-s; i++)
        {
          double pt0[4], pt1[4], newPoint[4];
          tmpPoints->GetTuple(i+1, pt0);
          tmpPoints->GetTuple(i, pt1);
          vtkSVMathUtils::Add(pt0, alpha[i][j], pt1, 1.0-alpha[i][j], 4, newPoint);
          tmpPoints->SetTuple(i, newPoint);

          // Fill the control point grid with the tmp points
          newControlPoints->SetControlPoint(row, L, 0, tmpPoints->GetTuple(0));
          newControlPoints->SetControlPoint(row, k+r-j-s, 0, tmpPoints->GetTuple(q-j-s));
        }
      }

      for (int i=L+1; i<k-s; i++)
        newControlPoints->SetControlPoint(row, i, 0, tmpPoints->GetTuple(i-L));
    }

    // Clean up alphas
    for (int i=0; i<q-s; i++)
      delete [] alpha[i];
    delete [] alpha;
  }

  // Realy quick, convert everything back to just p
  vtkSVNURBSUtils::GetPFromPW(newControlPoints);

  return SV_OK;
}

// ----------------------
// KnotRefinement
// ----------------------
int vtkSVNURBSUtils::KnotRefinement(vtkSVControlGrid *controlPoints,
                                    vtkDoubleArray *uKnots, const int uDegree,
                                    vtkDoubleArray *vKnots, const int vDegree,
                                    const int insertDirection,
                                    vtkDoubleArray *insertKnots,
                                    vtkSVControlGrid *newControlPoints,
                                    vtkDoubleArray *newUKnots,
                                    vtkDoubleArray *newVKnots)
{
  // Get dimensions of control point grid
  int dims[3];
  controlPoints->GetDimensions(dims);

  // Really quick, convert all points to pw
  vtkNew(vtkSVControlGrid, PW);
  vtkSVNURBSUtils::GetPWFromP(controlPoints, PW);

  // Set values used by alg to more concise vars
  int n = dims[0]-1;
  int p = uDegree;
  int m = dims[1]-1;
  int q = vDegree;
  int dir = insertDirection;
  int r = insertKnots->GetNumberOfTuples()-1;

  // Number knots
  int nuk = n+p+1;
  int nvk = m+q+1;

  if (dir == 0)
  {
    // Double check to see if correct vals were given
    if (uKnots->GetNumberOfTuples() != nuk+1)
    {
      fprintf(stderr,"Invalid number of control points given with u knot span\n");
      return SV_ERROR;
    }

    int a, b;
    vtkSVNURBSUtils::FindSpan(p, insertKnots->GetTuple1(0), uKnots, a);
    vtkSVNURBSUtils::FindSpan(p, insertKnots->GetTuple1(r), uKnots, b);
    b++;

    newControlPoints->SetNumberOfControlPoints((n+r+2)*(m+1));
    newControlPoints->SetDimensions(n+r+2, m+1, 1);
    newUKnots->SetNumberOfTuples(nuk+r+2);

    // Pass unchanging control points before new
    for (int col=0; col<=m; col++)
    {
      for (int i=0; i<=a-p; i++)
      {
        double pw[4];
        PW->GetControlPoint(i, col, 0, pw);
        newControlPoints->SetControlPoint(i, col, 0, pw);
      }

      // Set unchanging control points after new
      for (int i=b-1; i<=n; i++)
      {
        double pw[4];
        PW->GetControlPoint(i, col, 0, pw);
        newControlPoints->SetControlPoint(i+r+1, col, 0, pw);
      }
    }

    // Set unchanging knots before new ones
    for (int i=0; i<=a; i++)
      newUKnots->SetTuple1(i, uKnots->GetTuple1(i));

    // Set unchanging knots after new ones
    for (int i=b+p; i<=nuk; i++)
      newUKnots->SetTuple1(i+r+1, uKnots->GetTuple1(i));

    // Copy v knot span to new v knots span
    if(newVKnots != NULL && vKnots != NULL)
      newVKnots->DeepCopy(vKnots);

    // Set iter vars
    int i=b+p-1;
    int k=b+p+r;

    // Loop through and calc new cps
    for (int j=r; j>=0; j--)
    {
      while(insertKnots->GetTuple1(j) <= uKnots->GetTuple1(i) && i > a)
      {
        for (int col=0; col<=m; col++)
        {
          double pw[4];
          PW->GetControlPoint(i-p-1, col, 0, pw);
          newControlPoints->SetControlPoint(k-p-1, col, 0, pw);
        }
        newUKnots->SetTuple1(k, uKnots->GetTuple1(i));
        k--;
        i--;
      }
      for (int col=0; col<=m; col++)
      {
        double pdw[4];
        newControlPoints->GetControlPoint(k-p, col, 0, pdw);
        newControlPoints->SetControlPoint(k-p-1, col, 0, pdw);
      }
      for (int l=1; l<=p; l++)
      {
        int ind = k-p+l;
        double alpha = newUKnots->GetTuple1(k+l) - insertKnots->GetTuple1(j);
        if (fabs(alpha) >= -1.e-10 && fabs(alpha) <= 1.0e-10)
        {
          for (int col=0; col<=m; col++)
          {
            double pw[4];
            newControlPoints->GetControlPoint(ind, col, 0, pw);
            newControlPoints->SetControlPoint(ind-1, col, 0, pw);
          }
        }
        else
        {
          alpha = alpha/(newUKnots->GetTuple1(k+l) - uKnots->GetTuple1(i-p+l));
          for (int col=0; col<=m; col++)
          {
            double pw0[4], pw1[4], newPoint[4];
            newControlPoints->GetControlPoint(ind-1, col, 0, pw0);
            newControlPoints->GetControlPoint(ind, col, 0, pw1);
            vtkSVMathUtils::Add(pw0, alpha, pw1, 1.0-alpha, 4, newPoint);
            newControlPoints->SetControlPoint(ind-1, col, 0, newPoint);
          }
        }
      }
      newUKnots->SetTuple1(k, insertKnots->GetTuple1(j));
      k--;
    }
  }
  else if (dir == 1)
  {
    // Double check to see if correct vals were given
    if (vKnots->GetNumberOfTuples() != nvk+1)
    {
      fprintf(stderr,"Invalid number of control points given with v knot span\n");
      return SV_ERROR;
    }

    int a, b;
    vtkSVNURBSUtils::FindSpan(q, insertKnots->GetTuple1(0), vKnots, a);
    vtkSVNURBSUtils::FindSpan(q, insertKnots->GetTuple1(r), vKnots, b);
    b++;

    newControlPoints->SetNumberOfControlPoints((m+r+2)*(n+1));
    newControlPoints->SetDimensions(n+1, m+r+2, 1);
    newVKnots->SetNumberOfTuples(nvk+r+2);

    // Pass unchanging control points before new
    for (int row=0; row<=n; row++)
    {
      for (int i=0; i<=a-q; i++)
      {
        double pw[4];
        PW->GetControlPoint(row, i, 0, pw);
        newControlPoints->SetControlPoint(row, i, 0, pw);
      }

      // Set unchanging control points after new
      for (int i=b-1; i<=m; i++)
      {
        double pw[4];
        PW->GetControlPoint(row, i, 0, pw);
        newControlPoints->SetControlPoint(row, i+r+1, 0, pw);
      }
    }

    // Set unchanging knots before new ones
    for (int i=0; i<=a; i++)
      newVKnots->SetTuple1(i, vKnots->GetTuple1(i));

    // Set unchanging knots after new ones
    for (int i=b+q; i<=nvk; i++)
      newVKnots->SetTuple1(i+r+1, vKnots->GetTuple1(i));

    // Copy u knot span to new u knots span
    if (newUKnots != NULL && uKnots != NULL)
      newUKnots->DeepCopy(uKnots);

    // Set iter vars
    int i=b+q-1;
    int k=b+q+r;

    // Loop through and calc new cps
    for (int j=r; j>=0; j--)
    {
      while(insertKnots->GetTuple1(j) <= vKnots->GetTuple1(i) && i > a)
      {
        for (int row=0; row<=n; row++)
        {
          double pw[4];
          PW->GetControlPoint(row, i-q-1, 0, pw);
          newControlPoints->SetControlPoint(row, k-q-1, 0, pw);
        }
        newVKnots->SetTuple1(k, vKnots->GetTuple1(i));
        k--;
        i--;
      }
      for (int row=0; row<=n; row++)
      {
        double pdw[4];
        newControlPoints->GetControlPoint(row, k-q, 0, pdw);
        newControlPoints->SetControlPoint(row, k-q-1, 0, pdw);
      }
      for (int l=1; l<=q; l++)
      {
        int ind = k-q+l;
        double alpha = newVKnots->GetTuple1(k+l) - insertKnots->GetTuple1(j);
        if (fabs(alpha) >= -1.e-10 && fabs(alpha) <= 1.0e-10)
        {
          for (int row=0; row<=n; row++)
          {
            double pw[4];
            newControlPoints->GetControlPoint(row, ind, 0, pw);
            newControlPoints->SetControlPoint(row, ind-1, 0, pw);
          }
        }
        else
        {
          alpha = alpha/(newVKnots->GetTuple1(k+l) - vKnots->GetTuple1(i-q+l));
          for (int row=0; row<=n; row++)
          {
            double pw0[4], pw1[4], newPoint[4];
            newControlPoints->GetControlPoint(row, ind-1, 0, pw0);
            newControlPoints->GetControlPoint(row, ind, 0, pw1);
            vtkSVMathUtils::Add(pw0, alpha, pw1, 1.0-alpha, 4, newPoint);
            newControlPoints->SetControlPoint(row, ind-1, 0, newPoint);
          }
        }
      }
      newVKnots->SetTuple1(k, insertKnots->GetTuple1(j));
      k--;
    }
  }

  // Convert back from weighted points
  vtkSVNURBSUtils::GetPFromPW(newControlPoints);

  return SV_OK;
}


// SurfaceBezierExtraction
// ----------------------
int vtkSVNURBSUtils::SurfaceBezierExtraction(vtkSVControlGrid *controlPoints,
                                             vtkDoubleArray *uKnots, const int uDegree,
                                             vtkDoubleArray *vKnots, const int vDegree,
                                             const int extractDirection,
                                             vtkSVNURBSCollection *surfaces)
{
  // Get dimensions of control point grid
  int dims[3];
  controlPoints->GetDimensions(dims);

  // Really quick, convert all points to pw
  vtkNew(vtkSVControlGrid, PW);
  vtkSVNURBSUtils::GetPWFromP(controlPoints, PW);

  // Set values used by alg to more concise vars
  int n    = dims[0]-1;
  int m    = dims[1]-1;
  int p    = uDegree;
  int q    = vDegree;
  int dir = extractDirection;

  int nuk = n+p+1;
  int nvk = m+q+1;

  // Double check to see if correct vals were given
  if (uKnots->GetNumberOfTuples() != nuk+1)
  {
    fprintf(stderr,"Invalid number of control points given with knot span\n");
    return SV_ERROR;
  }
  // Double check to see if correct vals were given
  if (vKnots->GetNumberOfTuples() != nvk+1)
  {
    fprintf(stderr,"Invalid number of control points given with knot span\n");
    return SV_ERROR;
  }

  if (dir == 0)
  {
    if (p == n)
    {
      // direction is already Bezier!
      vtkNew(vtkSVNURBSSurface, newSurface);
      newSurface->SetControlPointGrid(controlPoints);
      newSurface->SetUKnotVector(uKnots);
      newSurface->SetVKnotVector(vKnots);
      newSurface->SetUDegree(p);
      newSurface->SetVDegree(p);
      surfaces->AddItem(newSurface);
      return SV_OK;
    }
    // Knots span locations
    int a  = p;
    int b  = p+1;

    // Set up new knots, just bezier knot span
    vtkNew(vtkDoubleArray, newUKnots);
    vtkSVNURBSUtils::LinSpaceClamp(0, 1, 2*(p+1), p, newUKnots);

    // Copy the v knots
    vtkNew(vtkDoubleArray, newVKnots);
    newVKnots->DeepCopy(vKnots);

    // New control points, updated each time for new bezier curve
    vtkNew(vtkSVControlGrid, newControlPoints);
    newControlPoints->SetNumberOfControlPoints((p+1)*(m+1));
    newControlPoints->SetDimensions(p+1, m+1, 1);

    vtkNew(vtkSVControlGrid, tmpPoints);
    tmpPoints->SetNumberOfControlPoints((p-1)*(m+1));
    tmpPoints->SetDimensions(p-1, m+1, 1);

    // Set first p+1 control points
    for(int i=0; i<=p; i++)
    {
      for (int col=0; col<=m; col++)
      {
        double pw[4];
        PW->GetControlPoint(i, col, 0, pw);
        newControlPoints->SetControlPoint(i, col, 0, pw);
      }
    }

    // Initiate array for alphas
    vtkNew(vtkDoubleArray, alphas);
    alphas->SetNumberOfTuples(nuk);

    while (b < nuk)
    {

      // Check multiplicity
      int i = b;
      while (b < nuk && uKnots->GetTuple1(b+1) == uKnots->GetTuple1(b))
        b++;
      int mult = b-i+1;
      if (mult < p)
      {
        // Alpha numerator
        double numer = uKnots->GetTuple1(b) - uKnots->GetTuple1(a);
        // Compute all alphas
        for (int j=p; j>mult; j--)
          alphas->SetTuple1(j-mult-1, numer/(uKnots->GetTuple1(a+j)-uKnots->GetTuple1(a)));

        int r = p-mult;

        // Insert the knot the number of times needed to get to p
        for (int j=1; j<=r; j++)
        {
          int save = r-j;
          int s = mult+j;
          for (int k=p; k>=s; k--)
          {
            double alpha = alphas->GetTuple1(k-s);
            for (int col=0; col<=m; col++)
            {
              double pw0[4], pw1[4], newPoint[4];
              newControlPoints->GetControlPoint(k, col, 0, pw0);
              newControlPoints->GetControlPoint(k-1, col, 0, pw1);
              vtkSVMathUtils::Add(pw0, alpha, pw1, 1.0-alpha, 4, newPoint);
              newControlPoints->SetControlPoint(k, col, 0, newPoint);
            }
          }
          if (b < nuk)
          {
            for (int col=0; col<=m; col++)
            {
              double pw[4];
              newControlPoints->GetControlPoint(p, col, 0, pw);
              tmpPoints->SetControlPoint(save, col, 0, pw);
            }
          }
        }
      }

      vtkNew(vtkSVControlGrid, tmpControl);
      vtkSVNURBSUtils::GetPFromPW(newControlPoints, tmpControl);

      vtkNew(vtkSVNURBSSurface, newSurface);
      newSurface->SetControlPointGrid(tmpControl);
      newSurface->SetUKnotVector(newUKnots);
      newSurface->SetVKnotVector(newVKnots);
      newSurface->SetUDegree(p);
      newSurface->SetVDegree(q);
      surfaces->AddItem(newSurface);

      // Set up next curve
      if (b < nuk)
      {
        for (int k=0; k<p-1; k++)
        {
          for (int col=0; col<=m; col++)
          {
            double pw[4];
            tmpPoints->GetControlPoint(k, col, 0, pw);
            newControlPoints->SetControlPoint(k, col, 0, pw);
          }

        }
        for (i=p-mult; i<=p; i++)
        {
          for (int col=0; col<=m; col++)
          {
            double pw[4];
            PW->GetControlPoint(b-p+i, col, 0, pw);
            newControlPoints->SetControlPoint(i, col, 0, pw);
          }
        }
        a=b;
        b++;
      }
    }
  }
  else if (dir == 1)
  {
    if (q == m)
    {
      // direction is already Bezier!
      vtkNew(vtkSVNURBSSurface, newSurface);
      newSurface->SetControlPointGrid(controlPoints);
      newSurface->SetUKnotVector(uKnots);
      newSurface->SetVKnotVector(vKnots);
      newSurface->SetUDegree(p);
      newSurface->SetVDegree(p);
      surfaces->AddItem(newSurface);
      return SV_OK;
    }
    // Knots span locations
    int a  = q;
    int b  = q+1;

    // Set up new knots, just bezier knot span
    vtkNew(vtkDoubleArray, newVKnots);
    vtkSVNURBSUtils::LinSpaceClamp(0, 1, 2*(q+1), q, newVKnots);

    // Copy the v knots
    vtkNew(vtkDoubleArray, newUKnots);
    newUKnots->DeepCopy(uKnots);

    // New control points, updated each time for new bezier curve
    vtkNew(vtkSVControlGrid, newControlPoints);
    newControlPoints->SetNumberOfControlPoints((n+1)*(q+1));
    newControlPoints->SetDimensions(n+1, q+1, 1);

    vtkNew(vtkSVControlGrid, tmpPoints);
    tmpPoints->SetNumberOfControlPoints((q-1)*(n+1));
    tmpPoints->SetDimensions(n+1, q-1, 1);


    // Set first p+1 control points
    for(int i=0; i<=q; i++)
    {
      for (int row=0; row<=n; row++)
      {
        double pw[4];
        PW->GetControlPoint(row, i, 0, pw);
        newControlPoints->SetControlPoint(row, i, 0, pw);
      }
    }

    // Initiate array for alphas
    vtkNew(vtkDoubleArray, alphas);
    alphas->SetNumberOfTuples(nvk);

    while (b < nvk)
    {

      // Check multiplicity
      int i = b;
      while (b < nvk && vKnots->GetTuple1(b+1) == vKnots->GetTuple1(b))
        b++;
      int mult = b-i+1;
      if (mult < q)
      {
        // Alpha numerator
        double numer = vKnots->GetTuple1(b) - vKnots->GetTuple1(a);
        // Compute all alphas
        for (int j=q; j>mult; j--)
          alphas->SetTuple1(j-mult-1, numer/(vKnots->GetTuple1(a+j)-vKnots->GetTuple1(a)));

        int r = q-mult;

        // Insert the knot the number of times needed to get to p
        for (int j=1; j<=r; j++)
        {
          int save = r-j;
          int s = mult+j;
          for (int k=q; k>=s; k--)
          {
            double alpha = alphas->GetTuple1(k-s);
            for (int row=0; row<=m; row++)
            {
              double pw0[4], pw1[4], newPoint[4];
              newControlPoints->GetControlPoint(row, k, 0, pw0);
              newControlPoints->GetControlPoint(row, k-1, 0, pw1);
              vtkSVMathUtils::Add(pw0, alpha, pw1, 1.0-alpha, 4, newPoint);
              newControlPoints->SetControlPoint(row, k, 0, newPoint);
            }
          }
          if (b < nvk)
          {
            for (int row=0; row<=n; row++)
            {
              double pw[4];
              newControlPoints->GetControlPoint(row, q, 0, pw);
              tmpPoints->SetControlPoint(row, save, 0, pw);
            }
          }
        }
      }

      vtkNew(vtkSVControlGrid, tmpControl);
      vtkSVNURBSUtils::GetPFromPW(newControlPoints, tmpControl);

      vtkNew(vtkSVNURBSSurface, newSurface);
      newSurface->SetControlPointGrid(tmpControl);
      newSurface->SetUKnotVector(newUKnots);
      newSurface->SetVKnotVector(newVKnots);
      newSurface->SetUDegree(p);
      newSurface->SetVDegree(q);
      surfaces->AddItem(newSurface);

      // Set up next curve
      if (b < nvk)
      {
        for (int k=0; k<q-1; k++)
        {
          for (int row=0; row<=n; row++)
          {
            double pw[4];
            tmpPoints->GetControlPoint(row, k, 0, pw);
            newControlPoints->SetControlPoint(row, k, 0, pw);
          }

        }
        for (i=q-mult; i<=q; i++)
        {
          for (int row=0; row<=n; row++)
          {
            double pw[4];
            PW->GetControlPoint(row, b-q+i, 0, pw);
            newControlPoints->SetControlPoint(row, i, 0, pw);
          }
        }
        a=b;
        b++;
      }
    }
  }


  return SV_OK;
}

// ----------------------
// IncreaseDegree
// ----------------------
int vtkSVNURBSUtils::IncreaseDegree(vtkSVControlGrid *controlPoints,
                                    vtkDoubleArray *uKnots, const int uDegree,
                                    vtkDoubleArray *vKnots, const int vDegree,
                                    const int increaseDirection,
                                    const int numberOfIncreases,
                                    vtkSVControlGrid *newControlPoints,
                                    vtkDoubleArray *newUKnots,
                                    vtkDoubleArray *newVKnots)
{
  // Get dimensions of control point grid
  int dims[3];
  controlPoints->GetDimensions(dims);

  // Really quick, convert all points to pw
  vtkNew(vtkSVControlGrid, PW);
  vtkSVNURBSUtils::GetPWFromP(controlPoints, PW);

  // Set values used by alg to more concise vars
  int n   = dims[0]-1;
  int m   = dims[1]-1;
  int p   = uDegree;
  int q   = vDegree;
  int t   = numberOfIncreases;
  int dir = increaseDirection;

  int nuk = n+p+1;
  int nvk = m+q+1;

  if (dir == 0)
  {
    // Double check to see if correct vals were given
    if (uKnots->GetNumberOfTuples() != nuk+1)
    {
      fprintf(stderr,"Invalid number of control points given with knot span\n");
      return SV_ERROR;
    }
    // Compute s by cmputing the number of non-repeated internal knot vals
    vtkNew(vtkIntArray, multiplicity);
    vtkNew(vtkDoubleArray, singleValues);
    vtkSVNURBSUtils::GetMultiplicity(uKnots, multiplicity, singleValues);

    // S s is length of mult vector minus 2 (p+1 ends)
    int s = multiplicity->GetNumberOfTuples() - 2;
    int nukhat = nuk+t*(s+2);
    int nhat = n+t*(s+1);

    // New degree
    int ph  = p+t;
    int ph2 = ph/2;

    // Set up new knots, just bezier knot span
    newUKnots->SetNumberOfTuples(nukhat+1);
    if (newVKnots != NULL && vKnots != NULL)
      newVKnots->DeepCopy(vKnots);

    // New control points, updated each time for new bezier curve
    newControlPoints->SetNumberOfControlPoints((nhat+1)*(m+1));
    newControlPoints->SetDimensions(nhat+1, m+1, 1);

    // Set up tmp arrays
    // Coeffecients for degree elevation
    vtkNew(vtkDoubleArray, bezalphas);
    bezalphas->SetNumberOfComponents(p+1);
    bezalphas->SetNumberOfTuples(p+t+1);

    // Bezier control strips, degree p
    vtkNew(vtkSVControlGrid, bpts);
    bpts->SetNumberOfControlPoints((p+1)*(m+1));
    bpts->SetDimensions(p+1, m+1, 1);

    // Bezier control points, degree p+t
    vtkNew(vtkSVControlGrid, ebpts);
    ebpts->SetNumberOfControlPoints((p+t+1)*(m+1));
    ebpts->SetDimensions(p+t+1, m+1, 1);

    // Leftmost control pionts of next Bezier segment
    vtkNew(vtkSVControlGrid, nextbpts);
    nextbpts->SetNumberOfControlPoints((p-1)*(m+1));
    nextbpts->SetDimensions(p-1, m+1, 1);

    // Alpha values for knot insertion
    vtkNew(vtkDoubleArray, alphas);
    alphas->SetNumberOfTuples(p-1);

    //Compute coefficients
    bezalphas->SetComponent(0, 0, 1.0);
    bezalphas->SetComponent(ph, p, 1.0);
    for (int i=1; i<=ph2; i++)
    {
      double inv = 1.0/vtkSVMathUtils::Binom(ph, i);
      int mpi = svminimum(p, i);
      for (int j=svmaximum(0, i-t); j<=mpi; j++)
      {
        double newVal = inv*vtkSVMathUtils::Binom(p, j)*vtkSVMathUtils::Binom(t, i-j);
        bezalphas->SetComponent(i, j, newVal);
      }
    }

    for (int i=ph2+1; i<=ph-1; i++)
    {
      int mpi = svminimum(p, i);
      for (int j=svmaximum(0, i-t); j<=mpi; j++)
        bezalphas->SetComponent(i, j, bezalphas->GetComponent(ph-i, p-j));
    }

    // Set up iter vars
    int nukh   =  ph;
    int kind =  ph+1;
    int r    = -1;
    int a    =  p;
    int b    =  p+1;
    int cind =  1;
    double ua = uKnots->GetTuple1(0);

    // Pass the first control point
    for (int col=0; col<=m; col++)
    {
      double firstpw[4];
      PW->GetControlPoint(0, col, 0, firstpw);
      newControlPoints->SetControlPoint(0, col, 0, firstpw);
    }

    for (int i=0; i<=ph; i++)
      newUKnots->SetTuple1(i, ua);

    // Initialize the first bezier segment.
    for (int i=0; i<=p; i++)
    {
      for (int col=0; col<=m; col++)
      {
        double pw[4];
        PW->GetControlPoint(i, col , 0, pw);
        bpts->SetControlPoint(i, col, 0, pw);
      }
    }

    // Loop through knot vector
    while (b<nuk)
    {
      // Calculate mult
      int i=b;
      while (b<nuk && uKnots->GetTuple1(b) == uKnots->GetTuple1(b+1))
        b++;

      // Set up iter vars
      int mul   = b-i+1;
      nukh        = nukh+mul+t;
      double ub = uKnots->GetTuple1(b);
      int oldr  = r;
      r = p-mul;

      // r multiplicities
      int lbz;
      if (oldr > 0)
        lbz = (oldr+2)/2;
      else
        lbz = 1;
      int rbz;
      if (r >0)
        rbz = ph-(r+1)/2;
      else
        rbz = ph;

      // Insert this knot r times
      // Extract bezier segment
      if (r>0)
      {
        double numer = ub - ua;
        for (int k=p; k>mul; k--)
          alphas->SetTuple1(k-mul-1, numer/(uKnots->GetTuple1(a+k)-ua));

        for (int j=1; j<=r; j++)
        {
          int save = r-j;
          int mulj = mul+j;
          for (int k=p; k>=mulj; k--)
          {
            double alpha = alphas->GetTuple1(k-mulj);
            for (int col=0; col<=m; col++)
            {
              double pw0[4], pw1[4], newPoint[4];
              bpts->GetControlPoint(k, col, 0,  pw0);
              bpts->GetControlPoint(k-1, col, 0, pw1);
              vtkSVMathUtils::Add(pw0, alpha, pw1, 1.0-alpha, 4, newPoint);
              bpts->SetControlPoint(k, col, 0, newPoint);
            }
          }
          for (int col=0; col<=m; col++)
          {
            double pw[4];
            bpts->GetControlPoint(p, col, 0,  pw);
            nextbpts->SetControlPoint(save, col, 0,  pw);
          }
        }
      }

      // Elevate degree of segment
      for (int i=lbz; i<=ph; i++)
      {
        for (int col=0; col<=m; col++)
        {
          double zero[4] = {0.0, 0.0, 0.0, 0.0};
          ebpts->SetControlPoint(i, col, 0,  zero);
        }
        int mpi = svminimum(p, i);
        for (int j=svmaximum(0, i-t); j<=mpi; j++)
        {
          for (int col=0; col<=m; col++)
          {
            double pw0[4], pw1[4], newPoint[4];
            int dimers[3];
            ebpts->GetDimensions(dimers);
            ebpts->GetControlPoint(i, col, 0,  pw0);
            bpts->GetControlPoint(j, col, 0, pw1);
            vtkSVMathUtils::Add(pw0, 1.0, pw1, bezalphas->GetComponent(i, j), 4, newPoint);
            ebpts->SetControlPoint(i, col, 0, newPoint);
          }
        }
      }

      // Now remove unnecessary knots
      if (oldr>1)
      {
        // Set up iter vars
        int first = kind-2;
        int last  = kind;

        double den = ub - ua;
        double bet = (ub-newUKnots->GetTuple1(kind-1))/den;

        // Loop through
        for (int tr=1; tr<oldr; tr++)
        {
          int i = first;
          int j = last;
          int kj = j-kind+1;

          // Compute the new control points
          while (j-i > tr)
          {
            // Suspiscious
            if (i<cind)
            {
              double alpha = (ub-newUKnots->GetTuple1(i))/(ua-newUKnots->GetTuple1(i));
              for (int col=0; col<=m; col++)
              {
                double pw0[4], pw1[4], newPoint[4];
                newControlPoints->GetControlPoint(i, col, 0, pw0);
                newControlPoints->GetControlPoint(i-1, col, 0, pw1);
                vtkSVMathUtils::Add(pw0, alpha, pw1, 1.0-alpha, 4, newPoint);
                newControlPoints->SetControlPoint(i, col, 0, newPoint);
              }
            }
            if (j>=lbz)
            {
              if (j-tr<=kind-ph+oldr)
              {
                double gam = (ub-newUKnots->GetTuple1(j-tr))/den;
                for (int col=0; col<=m; col++)
                {
                  double pw0[4], pw1[4], newPoint[4];
                  ebpts->GetControlPoint(kj, col, 0, pw0);
                  ebpts->GetControlPoint(kj+1, col, 0, pw1);
                  vtkSVMathUtils::Add(pw0, gam, pw1, 1.0-gam, 4, newPoint);
                  ebpts->SetControlPoint(kj, col, 0, newPoint);
                }
              }
              else
              {
                for (int col=0; col<=m; col++)
                {
                  double pw0[4], pw1[4], newPoint[4];
                  ebpts->GetControlPoint(kj, col, 0, pw0);
                  ebpts->GetControlPoint(kj+1, col, 0, pw1);
                  vtkSVMathUtils::Add(pw0, bet, pw1, 1.0-bet, 4, newPoint);
                  ebpts->SetControlPoint(kj, col, 0, newPoint);
                }
              }
            }
            i++;
            j--;
            kj--;
          }
          first--;
          last++;
        }
      }

      // Load knot ua for the end of the span
      if (a!=p)
      {
        for (int i=0; i<ph-oldr; i++)
        {
          newUKnots->SetTuple1(kind, ua);
          kind++;
        }
      }

      // Load remaining control points
      for (int j=lbz; j<=rbz; j++)
      {
        for (int col=0; col<=m; col++)
        {
          double pw[4];
          ebpts->GetControlPoint(j, col, 0, pw);
          newControlPoints->SetControlPoint(cind, col, 0, pw);
        }
        cind++;
      }

      // Set up the next b points
      if (b<nuk)
      {
        for (int j=0; j<r; j++)
        {
          for (int col=0; col<=m; col++)
          {
            double pw[4];
            nextbpts->GetControlPoint(j, col, 0,  pw);
            bpts->SetControlPoint(j, col, 0,  pw);
          }
        }
        for (int j=r; j<=p; j++)
        {
          for (int col=0; col<=m; col++)
          {
            double pw[4];
            PW->GetControlPoint(b-p+j, col, 0, pw);
            bpts->SetControlPoint(j, col, 0,  pw);
          }
        }
        a=b;
        b++;
        ua=ub;
      }
      else
      {
        // This is the end
        for (int i=0; i<=ph; i++)
          newUKnots->SetTuple1(kind+i, ub);
      }
    }
    int nh = nukh-ph-1;

    if (nukh != nukhat)
    {
      fprintf(stderr,"Something went wrong: nukhat %d does not equal iter m %d\n", nukhat, nukh);
      return SV_ERROR;
    }
    if (nh != nhat)
    {
      fprintf(stderr,"Something went wrong: nukhat %d does not equal iter m %d\n", nhat, nh);
      return SV_ERROR;
    }

  }
  else if (dir == 1)
  {
    if (vKnots->GetNumberOfTuples() != nvk+1)
    {
      fprintf(stderr,"Invalid number of control points given with knot span\n");
      return SV_ERROR;
    }

    // Compute s by cmputing the number of non-repeated internal knot vals
    vtkNew(vtkIntArray, multiplicity);
    vtkNew(vtkDoubleArray, singleValues);
    vtkSVNURBSUtils::GetMultiplicity(vKnots, multiplicity, singleValues);

    // S s is length of mult vector minus 2 (p+1 ends)
    int s = multiplicity->GetNumberOfTuples() - 2;
    int nvkhat = nvk+t*(s+2);
    int nhat = m+t*(s+1);

    // New degree
    int qh  = q+t;
    int qh2 = qh/2;

    // Set up new knots, just bezier knot span
    newVKnots->SetNumberOfTuples(nvkhat+1);
    if (newUKnots != NULL && uKnots != NULL)
      newUKnots->DeepCopy(uKnots);

    // New control points, updated each time for new bezier curve
    newControlPoints->SetNumberOfControlPoints((nhat+1)*(n+1));
    newControlPoints->SetDimensions(n+q, nhat+1, 1);

    // Set up tmp arrays
    // Coeffecients for degree elevation
    vtkNew(vtkDoubleArray, bezalphas);
    bezalphas->SetNumberOfComponents(q+1);
    bezalphas->SetNumberOfTuples(q+t+1);

    // Bezier control strips, degree p
    vtkNew(vtkSVControlGrid, bpts);
    bpts->SetNumberOfControlPoints((q+1)*(n+1));
    bpts->SetDimensions(n+1, q+1, 1);

    // Bezier control points, degree p+t
    vtkNew(vtkSVControlGrid, ebpts);
    ebpts->SetNumberOfControlPoints((q+t+1)*(n+1));
    ebpts->SetDimensions(n+1, q+t+1, 1);

    // Leftmost control pionts of next Bezier segment
    vtkNew(vtkSVControlGrid, nextbpts);
    nextbpts->SetNumberOfControlPoints((q-1)*(n+1));
    nextbpts->SetDimensions(n+1, q-1, 1);

    // Alpha values for knot insertion
    vtkNew(vtkDoubleArray, alphas);
    alphas->SetNumberOfTuples(q-1);

    //Compute coefficients
    bezalphas->SetComponent(0, 0, 1.0);
    bezalphas->SetComponent(qh, q, 1.0);
    for (int i=1; i<=qh2; i++)
    {
      double inv = 1.0/vtkSVMathUtils::Binom(qh, i);
      int mpi = svminimum(q, i);
      for (int j=svmaximum(0, i-t); j<=mpi; j++)
      {
        double newVal = inv*vtkSVMathUtils::Binom(q, j)*vtkSVMathUtils::Binom(t, i-j);
        bezalphas->SetComponent(i, j, newVal);
      }
    }

    for (int i=qh2+1; i<=qh-1; i++)
    {
      int mpi = svminimum(q, i);
      for (int j=svmaximum(0, i-t); j<=mpi; j++)
        bezalphas->SetComponent(i, j, bezalphas->GetComponent(qh-i, q-j));
    }

    // Set up iter vars
    int nvkh   =  qh;
    int kind =  qh+1;
    int r    = -1;
    int a    =  q;
    int b    =  q+1;
    int cind =  1;
    double va = vKnots->GetTuple1(0);

    // Pass the first control point
    for (int row=0; row<=n; row++)
    {
      double firstpw[4];
      PW->GetControlPoint(row, 0, 0, firstpw);
      newControlPoints->SetControlPoint(row, 0, 0, firstpw);
    }

    for (int i=0; i<=qh; i++)
      newVKnots->SetTuple1(i, va);

    // Initialize the first bezier segment.
    for (int i=0; i<=q; i++)
    {
      for (int row=0; row<=n; row++)
      {
        double pw[4];
        PW->GetControlPoint(row, i, 0, pw);
        bpts->SetControlPoint(row, i, 0, pw);
      }
    }

    // Loop through knot vector
    while (b<nvk)
    {
      // Calculate mult
      int i=b;
      while (b<nvk && vKnots->GetTuple1(b) == vKnots->GetTuple1(b+1))
        b++;

      // Set up iter vars
      int mul   = b-i+1;
      nvkh        = nvkh+mul+t;
      double vb = vKnots->GetTuple1(b);
      int oldr  = r;
      r = q-mul;

      // r multiplicities
      int lbz;
      if (oldr > 0)
        lbz = (oldr+2)/2;
      else
        lbz = 1;
      int rbz;
      if (r >0)
        rbz = qh-(r+1)/2;
      else
        rbz = qh;

      // Insert this knot r times
      // Extract bezier segment
      if (r>0)
      {
        double numer = vb - va;
        for (int k=q; k>mul; k--)
          alphas->SetTuple1(k-mul-1, numer/(vKnots->GetTuple1(a+k)-va));

        for (int j=1; j<=r; j++)
        {
          int save = r-j;
          int mulj = mul+j;
          for (int k=q; k>=mulj; k--)
          {
            double alpha = alphas->GetTuple1(k-mulj);
            for (int row=0; row<=n; row++)
            {
              double pw0[4], pw1[4], newPoint[4];
              bpts->GetControlPoint(row, k, 0,  pw0);
              bpts->GetControlPoint(row, k-1, 0, pw1);
              vtkSVMathUtils::Add(pw0, alpha, pw1, 1.0-alpha, 4, newPoint);
              bpts->SetControlPoint(row, k, 0, newPoint);
            }
          }
          for (int row=0; row<=n; row++)
          {
            double pw[4];
            bpts->GetControlPoint(row, q, 0,  pw);
            nextbpts->SetControlPoint(row, save, 0,  pw);
          }
        }
      }

      // Elevate degree of segment
      for (int i=lbz; i<=qh; i++)
      {
        for (int row=0; row<=n; row++)
        {
          double zero[4] = {0.0, 0.0, 0.0, 0.0};
          ebpts->SetControlPoint(row, i, 0,  zero);
        }
        int mpi = svminimum(q, i);
        for (int j=svmaximum(0, i-t); j<=mpi; j++)
        {
          for (int row=0; row<=n; row++)
          {
            double pw0[4], pw1[4], newPoint[4];
            int dimers[3];
            ebpts->GetDimensions(dimers);
            ebpts->GetControlPoint(row, i, 0,  pw0);
            bpts->GetControlPoint(row, j, 0, pw1);
            vtkSVMathUtils::Add(pw0, 1.0, pw1, bezalphas->GetComponent(i, j), 4, newPoint);
            ebpts->SetControlPoint(row, i, 0, newPoint);
          }
        }
      }

      // Now remove unnecessary knots
      if (oldr>1)
      {
        // Set up iter vars
        int first = kind-2;
        int last  = kind;

        double den = vb - va;
        double bet = (vb-newVKnots->GetTuple1(kind-1))/den;

        // Loop through
        for (int tr=1; tr<oldr; tr++)
        {
          int i = first;
          int j = last;
          int kj = j-kind+1;

          // Compute the new control points
          while (j-i > tr)
          {
            // Suspiscious
            if (i<cind)
            {
              double alpha = (vb-newVKnots->GetTuple1(i))/(va-newVKnots->GetTuple1(i));
              for (int row=0; row<=n; row++)
              {
                double pw0[4], pw1[4], newPoint[4];
                newControlPoints->GetControlPoint(row, i, 0, pw0);
                newControlPoints->GetControlPoint(row, i-1, 0, pw1);
                vtkSVMathUtils::Add(pw0, alpha, pw1, 1.0-alpha, 4, newPoint);
                newControlPoints->SetControlPoint(row, i, 0, newPoint);
              }
            }
            if (j>=lbz)
            {
              if (j-tr<=kind-qh+oldr)
              {
                double gam = (vb-newVKnots->GetTuple1(j-tr))/den;
                for (int row=0; row<=n; row++)
                {
                  double pw0[4], pw1[4], newPoint[4];
                  ebpts->GetControlPoint(row, kj, 0, pw0);
                  ebpts->GetControlPoint(row, kj+1, 0, pw1);
                  vtkSVMathUtils::Add(pw0, gam, pw1, 1.0-gam, 4, newPoint);
                  ebpts->SetControlPoint(row, kj, 0, newPoint);
                }
              }
              else
              {
                for (int row=0; row<=n; row++)
                {
                  double pw0[4], pw1[4], newPoint[4];
                  ebpts->GetControlPoint(row, kj, 0, pw0);
                  ebpts->GetControlPoint(row, kj+1, 0, pw1);
                  vtkSVMathUtils::Add(pw0, bet, pw1, 1.0-bet, 4, newPoint);
                  ebpts->SetControlPoint(row, kj, 0, newPoint);
                }
              }
            }
            i++;
            j--;
            kj--;
          }
          first--;
          last++;
        }
      }

      // Load knot va for the end of the span
      if (a!=q)
      {
        for (int i=0; i<qh-oldr; i++)
        {
          newVKnots->SetTuple1(kind, va);
          kind++;
        }
      }

      // Load remaining control points
      for (int j=lbz; j<=rbz; j++)
      {
        for (int row=0; row<=n; row++)
        {
          double pw[4];
          ebpts->GetControlPoint(row, j, 0, pw);
          newControlPoints->SetControlPoint(row, cind, 0, pw);
        }
        cind++;
      }

      // Set up the next b points
      if (b<nvk)
      {
        for (int j=0; j<r; j++)
        {
          for (int row=0; row<=n; row++)
          {
            double pw[4];
            nextbpts->GetControlPoint(row, j, 0,  pw);
            bpts->SetControlPoint(row, j, 0,  pw);
          }
        }
        for (int j=r; j<=q; j++)
        {
          for (int row=0; row<=n; row++)
          {
            double pw[4];
            PW->GetControlPoint(row, b-q+j, 0, pw);
            bpts->SetControlPoint(j, 0, 0, pw);
          }
        }
        a=b;
        b++;
        va=vb;
      }
      else
      {
        // This is the end
        for (int i=0; i<=qh; i++)
          newVKnots->SetTuple1(kind+i, vb);
      }
    }
    int nh = nvkh-qh-1;

    if (nvkh != nvkhat)
    {
      fprintf(stderr,"Something went wrong: nvkhat %d does not equal iter m %d\n", nvkhat, nvkh);
      return SV_ERROR;
    }
    if (nh != nhat)
    {
      fprintf(stderr,"Something went wrong: nvkhat %d does not equal iter m %d\n", nhat, nh);
      return SV_ERROR;
    }

  }

  // Realy quick, convert everything back to just p
  vtkSVNURBSUtils::GetPFromPW(newControlPoints);

  return SV_OK;
}

// ----------------------
// RemoveKnot
// ----------------------
int vtkSVNURBSUtils::RemoveKnot(vtkSVControlGrid *controlPoints,
                                vtkDoubleArray *uKnots, const int uDegree,
                                vtkDoubleArray *vKnots, const int vDegree,
                                const int removeDirection,
                                const double removeValue, const int removeIndex,
                                const int currentMultiplicity,
                                const int numberOfRemovals, const double tol,
                                vtkSVControlGrid *newControlPoints,
                                vtkDoubleArray *newUKnots, vtkDoubleArray *newVKnots)
{
  // Get dimensions of control point grid
  int dims[3];
  controlPoints->GetDimensions(dims);

  // Copy control points and knots to new objects
  vtkNew(vtkSVControlGrid, PW);
  vtkSVNURBSUtils::GetPWFromP(controlPoints, PW);

  // Set values used by alg to more concise vars
  int n    = dims[0]-1;
  int m    = dims[1]-1;
  int p    = uDegree;
  int q    = vDegree;
  double u = removeValue;
  int r    = removeIndex;
  int num  = numberOfRemovals;
  int s    = currentMultiplicity;
  int dir  = removeDirection;

  int nuk   = n+p+1;
  int nvk   = m+q+1;

  if (dir == 0)
  {
    int ord   = p+1;
    int fout  = (2*r-s-p)/2;
    int last  = r-s;
    int first = r-p;

    // Double check to see if correct vals were given
    if (uKnots->GetNumberOfTuples() != nuk+1)
    {
      fprintf(stderr,"Invalid number of control points given with knot span\n");
      return SV_ERROR;
    }

    if (uKnots->GetTuple1(r) != u)
    {
      fprintf(stderr,"Incorrect index %d given for knot value %f\n", r, u);
      return SV_ERROR;
    }

    // The new knots and old knots
    vtkNew(vtkDoubleArray, tmpKnots);
    tmpKnots->DeepCopy(uKnots);
    if (newVKnots != NULL && vKnots != NULL)
      newVKnots->DeepCopy(vKnots);

    // Compute a tolerance for points
    double origin[3]; origin[0] = 0.0; origin[1] = 0.0; origin[2] = 0.0;
    double minWeight = VTK_SV_LARGE_DOUBLE;
    double maxDist   = 0.0;
    for (int i=0; i<=n; i++)
    {
      for (int col=0; col<=m; col++)
      {
        double pw[4];
        controlPoints->GetControlPoint(i, col, 0, pw);
        if (pw[3] < minWeight)
          minWeight = pw[3];
        double dist = vtkSVMathUtils::Distance(pw, origin, 3) > maxDist;
        if (dist > maxDist)
          maxDist = dist;
      }
    }
    double tolerance = tol*minWeight/(1+fabs(maxDist));

    vtkNew(vtkSVControlGrid, tmpPoints);
    tmpPoints->SetNumberOfControlPoints((2*p+1)*(m+1));
    tmpPoints->SetDimensions(2*p+1, m+1, 1);

    // Set iter vars
    int i;
    int j;
    int ii;
    int jj;
    int t;

    for (t=0; t<num; t++)
    {
      // Get difference between indices of tmpPoints and control points
      int off = first-1;

      for (int col=0; col<=m; col++)
      {
        // Set tmpPoints
        double pw0[4], pw1[4];
        PW->GetControlPoint(off, col, 0, pw0);
        PW->GetControlPoint(last+1, col, 0, pw1);
        tmpPoints->SetControlPoint(0, col, 0, pw0);
        tmpPoints->SetControlPoint(last+1-off, col, 0, pw1);
      }

      // Update iter vars
      i=first;
      j=last;
      ii=1;
      jj=last-off;
      int remFlag = 0;

      // Go through and compute control points for each removal
      while (j-i > t)
      {
        double alpha0 = (u-tmpKnots->GetTuple1(i))/(tmpKnots->GetTuple1(i+ord+t)-tmpKnots->GetTuple1(i));
        double alpha1 = (u-tmpKnots->GetTuple1(j-t))/(tmpKnots->GetTuple1(j+ord)-tmpKnots->GetTuple1(j-t));

        for (int col=0; col<=m; col++)
        {
          // Set tmpPoints control points
          double pw2[4], pw3[4], newPoint0[4];
          tmpPoints->GetControlPoint(ii-1, col, 0, pw2);
          PW->GetControlPoint(i, col, 0, pw3);
          vtkSVMathUtils::Add(pw3, 1.0, pw2, -1.0*(1.0-alpha0), 4, newPoint0);
          vtkSVMathUtils::MultiplyScalar(newPoint0, 1.0/alpha0, 4);
          tmpPoints->SetControlPoint(ii, col, 0, newPoint0);

          double pw4[4], pw5[4], newPoint1[4];
          tmpPoints->GetControlPoint(jj+1, col, 0, pw4);
          PW->GetControlPoint(j, col, 0, pw5);
          vtkSVMathUtils::Add(pw5, 1.0, pw4, -1.0*alpha1, 4, newPoint1);
          vtkSVMathUtils::MultiplyScalar(newPoint1, 1.0/(1.0-alpha1), 4);
          tmpPoints->SetControlPoint(jj, col, 0, newPoint1);
        }

        // edit iter vars
        i++;
        ii++;
        j--;
        jj--;
      }

      // Check to see if we can remove knot
      if (j-i < t)
      {
        for (int col=0; col<=m; col++)
        {
          double pw2[4], pw3[4];
          tmpPoints->GetControlPoint(ii-1, col, 0, pw2);
          tmpPoints->GetControlPoint(jj+1, col, 0, pw3);
          //if (vtkSVMathUtils::Distance(pw2, pw3, 4) <= tolerance)
            remFlag = 1;
        }
      }
      else
      {
        double alpha0 = (u-tmpKnots->GetTuple1(i))/(tmpKnots->GetTuple1(i+ord+t)-tmpKnots->GetTuple1(i));

        for (int col=0; col<=m; col++)
        {
          double pw2[4], pw3[4], pw4[4], testPoint[4];
          PW->GetControlPoint(i, 0, 0, pw2);
          tmpPoints->GetControlPoint(ii+t+1, col, 0, pw3);
          tmpPoints->GetControlPoint(ii-1, col, 0, pw4);
          vtkSVMathUtils::Add(pw3, alpha0, pw4, 1.0-alpha0, 4, testPoint);

          //if (vtkSVMathUtils::Distance(pw2, testPoint, 4) <= tolerance)
            remFlag = 1;
        }
      }

      // Check if knot can be removed
      if (remFlag == 0)
        break;
      else
      {
        i = first;
        j = last;

        // Save new control points from tmpPoints
        while (j-i > t)
        {
          for (int col=0; col<=m; col++)
          {
            double pw2[4], pw3[4];
            tmpPoints->GetControlPoint(i-off, col, 0, pw2);
            tmpPoints->GetControlPoint(j-off, col, 0, pw3);

            PW->SetControlPoint(i, col, 0, pw2);
            PW->SetControlPoint(j, col, 0, pw3);
          }
          i++;
          j--;
        }

      }
      first--;
      last++;
    }

    if (t==0)
    {
      fprintf(stderr,"No knots were able to be removed\n");
      return SV_OK;
    }

    // Update new knots
    newUKnots->SetNumberOfTuples(nuk+1-t);
    for (int k=0; k<=nuk-t; k++)
      newUKnots->SetTuple1(k, tmpKnots->GetTuple1(k));
    for (int k=r+1; k<=nuk; k++)
      newUKnots->SetTuple1(k-t, tmpKnots->GetTuple1(k));

    // Update iter vars
    j = fout;
    i = fout;
    for (int k=1; k<t; k++)
    {
      if (k%2 == 1)
        i++;
      else
        j--;
    }

    // Update new control points
    newControlPoints->GetPoints()->SetNumberOfPoints((n+1-t)*(m+1));
    newControlPoints->SetDimensions(n+1-t, m+1, 1);

    for (int k=0; k<j; k++)
    {
      for (int col=0; col<=m; col++)
      {
        double pw[4];
        PW->GetControlPoint(k, col, 0, pw);
        vtkMath::MultiplyScalar(pw, 1./pw[3]);
        newControlPoints->SetControlPoint(k, col, 0, pw);
      }
    }

    for (int k=i+1; k<=n; k++)
    {
      for (int col=0; col<=m; col++)
      {
        double pw[4];
        PW->GetControlPoint(k, col, 0, pw);
        vtkMath::MultiplyScalar(pw, 1./pw[3]);
        newControlPoints->SetControlPoint(j, col, 0, pw);
      }
      j++;
    }
  }
  else if (dir == 1)
  {
    int ord   = q+1;
    int fout  = (2*r-s-q)/2;
    int last  = r-s;
    int first = r-q;

    // Double check to see if correct vals were given
    if (vKnots->GetNumberOfTuples() != nvk+1)
    {
      fprintf(stderr,"Invalid number of control points given with knot span\n");
      return SV_ERROR;
    }

    if (vKnots->GetTuple1(r) != u)
    {
      fprintf(stderr,"Incorrect index %d given for knot value %f\n", r, u);
      return SV_ERROR;
    }

    // The new knots and old knots
    vtkNew(vtkDoubleArray, tmpKnots);
    tmpKnots->DeepCopy(vKnots);
    if (newUKnots != NULL && uKnots != NULL)
      newUKnots->DeepCopy(uKnots);

    // Compute a tolerance for points
    double origin[3]; origin[0] = 0.0; origin[1] = 0.0; origin[2] = 0.0;
    double minWeight = VTK_SV_LARGE_DOUBLE;
    double maxDist   = 0.0;
    for (int i=0; i<=n; i++)
    {
      for (int row=0; row<=n; row++)
      {
        double pw[4];
        controlPoints->GetControlPoint(row, i, 0, pw);
        if (pw[3] < minWeight)
          minWeight = pw[3];
        double dist = vtkSVMathUtils::Distance(pw, origin, 3) > maxDist;
        if (dist > maxDist)
          maxDist = dist;
      }
    }
    double tolerance = tol*minWeight/(1+fabs(maxDist));

    vtkNew(vtkSVControlGrid, tmpPoints);
    tmpPoints->SetNumberOfControlPoints((2*q+1)*(n+1));
    tmpPoints->SetDimensions(n+1, 2*q+1, 1);

    // Set iter vars
    int i;
    int j;
    int ii;
    int jj;
    int t;

    for (t=0; t<num; t++)
    {
      // Get difference between indices of tmpPoints and control points
      int off = first-1;

      for (int row=0; row<=n; row++)
      {
        // Set tmpPoints
        double pw0[4], pw1[4];
        PW->GetControlPoint(row, off, 0, pw0);
        PW->GetControlPoint(row, last+1, 0, pw1);
        tmpPoints->SetControlPoint(row, 0, 0, pw0);
        tmpPoints->SetControlPoint(row, last+1-off, 0, pw1);
      }

      // Update iter vars
      i=first;
      j=last;
      ii=1;
      jj=last-off;
      int remFlag = 0;

      // Go through and compute control points for each removal
      while (j-i > t)
      {
        double alpha0 = (u-tmpKnots->GetTuple1(i))/(tmpKnots->GetTuple1(i+ord+t)-tmpKnots->GetTuple1(i));
        double alpha1 = (u-tmpKnots->GetTuple1(j-t))/(tmpKnots->GetTuple1(j+ord)-tmpKnots->GetTuple1(j-t));

        for (int row=0; row<=n; row++)
        {
          // Set tmpPoints control points
          double pw2[4], pw3[4], newPoint0[4];
          tmpPoints->GetControlPoint(row, ii-1, 0, pw2);
          PW->GetControlPoint(row, i, 0, pw3);
          vtkSVMathUtils::Add(pw3, 1.0, pw2, -1.0*(1.0-alpha0), 4, newPoint0);
          vtkSVMathUtils::MultiplyScalar(newPoint0, 1.0/alpha0, 4);
          tmpPoints->SetControlPoint(row, ii, 0, newPoint0);

          double pw4[4], pw5[4], newPoint1[4];
          tmpPoints->GetControlPoint(row, jj+1, 0, pw4);
          PW->GetControlPoint(row, j, 0, pw5);
          vtkSVMathUtils::Add(pw5, 1.0, pw4, -1.0*alpha1, 4, newPoint1);
          vtkSVMathUtils::MultiplyScalar(newPoint1, 1.0/(1.0-alpha1), 4);
          tmpPoints->SetControlPoint(row, jj, 0, newPoint1);
        }

        // edit iter vars
        i++;
        ii++;
        j--;
        jj--;
      }

      // Check to see if we can remove knot
      if (j-i < t)
      {
        for (int row=0; row<=n; row++)
        {
          double pw2[4], pw3[4];
          tmpPoints->GetControlPoint(row, ii-1, 0, pw2);
          tmpPoints->GetControlPoint(row, jj+1, 0, pw3);
          //if (vtkSVMathUtils::Distance(pw2, pw3, 4) <= tolerance)
            remFlag = 1;
        }
      }
      else
      {
        double alpha0 = (u-tmpKnots->GetTuple1(i))/(tmpKnots->GetTuple1(i+ord+t)-tmpKnots->GetTuple1(i));

        for (int row=0; row<=n; row++)
        {
          double pw2[4], pw3[4], pw4[4], testPoint[4];
          PW->GetControlPoint(i, 0, 0, pw2);
          tmpPoints->GetControlPoint(row, ii+t+1, 0, pw3);
          tmpPoints->GetControlPoint(row, ii-1, 0, pw4);
          vtkSVMathUtils::Add(pw3, alpha0, pw4, 1.0-alpha0, 4, testPoint);

          //if (vtkSVMathUtils::Distance(pw2, testPoint, 4) <= tolerance)
            remFlag = 1;
        }
      }

      // Check if knot can be removed
      if (remFlag == 0)
        break;
      else
      {
        i = first;
        j = last;

        // Save new control points from tmpPoints
        while (j-i > t)
        {
          for (int row=0; row<=n; row++)
          {
            double pw2[4], pw3[4];
            tmpPoints->GetControlPoint(row, i-off, 0, pw2);
            tmpPoints->GetControlPoint(row, j-off, 0, pw3);

            PW->SetControlPoint(row, i, 0, pw2);
            PW->SetControlPoint(row, j, 0, pw3);
          }
          i++;
          j--;
        }

      }
      first--;
      last++;
    }

    if (t==0)
    {
      fprintf(stderr,"No knots were able to be removed\n");
      return SV_OK;
    }

    // Update new knots
    newVKnots->SetNumberOfTuples(nvk+1-t);
    for (int k=0; k<=nvk-t; k++)
      newVKnots->SetTuple1(k, tmpKnots->GetTuple1(k));
    for (int k=r+1; k<=nvk; k++)
      newVKnots->SetTuple1(k-t, tmpKnots->GetTuple1(k));

    // Update iter vars
    j = fout;
    i = fout;
    for (int k=1; k<t; k++)
    {
      if (k%2 == 1)
        i++;
      else
        j--;
    }

    // Update new control points
    newControlPoints->GetPoints()->SetNumberOfPoints((n+1)*(m+1-t));
    newControlPoints->SetDimensions(n+1, m+1-t, 1);

    for (int k=0; k<j; k++)
    {
      for (int row=0; row<=n; row++)
      {
        double pw[4];
        PW->GetControlPoint(row, k, 0, pw);
        vtkMath::MultiplyScalar(pw, 1./pw[3]);
        newControlPoints->SetControlPoint(row, k, 0, pw);
      }
    }

    for (int k=i+1; k<=m; k++)
    {
      for (int row=0; row<=n; row++)
      {
        double pw[4];
        PW->GetControlPoint(row, k, 0, pw);
        vtkMath::MultiplyScalar(pw, 1./pw[3]);
        newControlPoints->SetControlPoint(row, j, 0, pw);
      }
      j++;
    }
  }

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
  int dim[4];
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
  else if (dims == 4)
  {
    vtkArrayExtents size;
    size.SetDimensions(4);
    size.SetExtent(0, vtkArrayRange(0, dim[0]));
    size.SetExtent(1, vtkArrayRange(0, dim[1]));
    size.SetExtent(2, vtkArrayRange(0, dim[2]));
    size.SetExtent(3, vtkArrayRange(0, dim[3]));

    output->Resize(size);
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
            if (dims == 3)
            {
              double val = input->GetValue(i, j, k);
              output->SetValue(i, j, k, val);
            }
            else
            {
              for (int l=0; l<dim[3]; l++)
              {
                vtkArrayCoordinates loc;
                loc.SetDimensions(4);
                loc.SetCoordinate(0, i);
                loc.SetCoordinate(1, j);
                loc.SetCoordinate(2, k);
                loc.SetCoordinate(3, l);

                double val = input->GetValue(loc);
                output->SetValue(loc, val);
              }
            }
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
int vtkSVNURBSUtils::FindSpan(const int p, const double u, vtkDoubleArray *knots, int &span)
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
// FindKnotMultiplicity
// ----------------------
int vtkSVNURBSUtils::FindKnotMultiplicity(const int knotIndex, const double u, vtkDoubleArray *knots, int &mult)
{
  mult = 0;
  if(knots->GetTuple1(knotIndex) == u)
  {
    int i=knotIndex;
    int count = 1;
    while(knots->GetTuple1(i+1) == knots->GetTuple1(i) &&
          i < knots->GetNumberOfTuples())
    {
      count++;
      i++;
    }
    mult = count;
  }
  else
  {
    fprintf(stderr,"Incorrect index %d given for knot %f\n", knotIndex, u);
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// GetMultiplicity
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
    while(array->GetTuple1(i+1) == array->GetTuple1(i) && i<numVals-1)
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
  if (array->GetTuple1(numVals-1) != singleValues->GetTuple1(numMults-1))
  {
    // Add new mult because not equal
    multiplicity->InsertNextTuple1(1);
    singleValues->InsertNextTuple1(array->GetTuple1(numVals-1));
  }

  return SV_OK;
}

// ----------------------
// GetPWFromP
// ----------------------
int vtkSVNURBSUtils::GetPWFromP(vtkSVControlGrid *controlPoints)
{
  // Realy quick, convert all points to pw
  vtkDataArray *weights = controlPoints->GetPointData()->GetArray("Weights");
  for (int i=0; i<controlPoints->GetNumberOfPoints(); i++)
  {
    double pt[3];
    controlPoints->GetPoints()->GetPoint(i, pt);
    double weight = weights->GetTuple1(i);
    vtkMath::MultiplyScalar(pt, weight);
    controlPoints->GetPoints()->SetPoint(i, pt);
  }

  return SV_OK;
}


// ----------------------
// GetPWFromP
// ----------------------
int vtkSVNURBSUtils::GetPWFromP(vtkSVControlGrid *controlPoints, vtkSVControlGrid *controlPointWeights)
{
  // Realy quick, convert all points to pw
  controlPointWeights->SetNumberOfControlPoints(controlPoints->GetNumberOfPoints());
  controlPointWeights->SetDimensions(controlPoints->GetDimensions());
  vtkDataArray *weights = controlPoints->GetPointData()->GetArray("Weights");
  vtkDataArray *newWeights = controlPointWeights->GetPointData()->GetArray("Weights");
  for (int i=0; i<controlPoints->GetNumberOfPoints(); i++)
  {
    double pt[3];
    controlPoints->GetPoints()->GetPoint(i, pt);
    double weight = weights->GetTuple1(i);
    vtkMath::MultiplyScalar(pt, weight);
    controlPointWeights->GetPoints()->SetPoint(i, pt);
    newWeights->SetTuple1(i, weight);
  }

  return SV_OK;
}

// ----------------------
// GetPFromPW
// ----------------------
int vtkSVNURBSUtils::GetPFromPW(vtkSVControlGrid *controlPoints)
{
  // Realy quick, convert all points to pw
  vtkDataArray *weights = controlPoints->GetPointData()->GetArray("Weights");
  for (int i=0; i<controlPoints->GetNumberOfPoints(); i++)
  {
    double pt[3];
    controlPoints->GetPoints()->GetPoint(i, pt);
    double weight = weights->GetTuple1(i);
    vtkMath::MultiplyScalar(pt, 1./weight);
    controlPoints->GetPoints()->SetPoint(i, pt);
  }

  return SV_OK;
}

// ----------------------
// GetPFromPW
// ----------------------
int vtkSVNURBSUtils::GetPFromPW(vtkSVControlGrid *controlPointWeights, vtkSVControlGrid *controlPoints)
{
  // Realy quick, convert all points to pw
  controlPoints->SetNumberOfControlPoints(controlPointWeights->GetNumberOfPoints());
  controlPoints->SetDimensions(controlPointWeights->GetDimensions());
  vtkDataArray *weights = controlPointWeights->GetPointData()->GetArray("Weights");
  vtkDataArray *newWeights = controlPoints->GetPointData()->GetArray("Weights");
  for (int i=0; i<controlPointWeights->GetNumberOfPoints(); i++)
  {
    double pt[3];
    controlPointWeights->GetPoints()->GetPoint(i, pt);
    double weight = weights->GetTuple1(i);
    vtkMath::MultiplyScalar(pt, 1./weight);
    controlPoints->GetPoints()->SetPoint(i, pt);
    newWeights->SetTuple1(i, weight);
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
int vtkSVNURBSUtils::MatrixMatrixMultiply(vtkTypedArray<double> *mat0, const int mat0IsPoints, const int point0Dims,
                                        vtkTypedArray<double> *mat1, const int mat1IsPoints, const int point1Dims,
                                        vtkTypedArray<double> *output)
{
  int nrM0 = mat0->GetExtents()[0].GetSize();
  int ncM0 = mat0->GetExtents()[1].GetSize();
  if (mat0IsPoints && mat0->GetExtents()[2].GetSize() != point0Dims)
  {
    fprintf(stderr,"Third dimension of matrix should contain %d dims, but doesn't!\n", point0Dims);
    return SV_ERROR;
  }

  int nrM1 = mat1->GetExtents()[0].GetSize();
  int ncM1 = mat1->GetExtents()[1].GetSize();
  if (mat1IsPoints && mat1->GetExtents()[2].GetSize() != point1Dims)
  {
    fprintf(stderr,"Third dimension of matrix should contain %d dims, but doesn't!\n", point1Dims);
    return SV_ERROR;
  }

  if (ncM0 != nrM1)
  {
    fprintf(stderr,"Matrix matrix dimensions do not match\n");
    fprintf(stderr,"Matrix 0: %d by %d, Matrix 1: %d by %d\n", nrM0, ncM0, nrM1, ncM1);
    return SV_ERROR;
  }

  int either = 0;
  if (mat0IsPoints)
  {
    either = 1;
    output->Resize(nrM0, ncM1, point0Dims);
  }
  else if (mat1IsPoints)
  {
    either = 1;
    output->Resize(nrM0, ncM1, point1Dims);
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
    vtkSVNURBSUtils::PointMatrixPointMatrixForDGEMM(mat0, mat1, point0Dims, output);
  }
  else if (mat0IsPoints)
  {
    vtkSVNURBSUtils::PointMatrixMatrixForDGEMM(mat0, mat1, point0Dims, output);
  }
  else
  {
    vtkSVNURBSUtils::MatrixPointMatrixForDGEMM(mat0, mat1, point1Dims, output);
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
// PointMatrixPointMatrixForDGEMM
// ----------------------
int vtkSVNURBSUtils::PointMatrixPointMatrixForDGEMM(vtkTypedArray<double> *mat0,
                                             vtkTypedArray<double> *mat1,
                                             const int pointDims,
                                             vtkTypedArray<double> *output)
{
  int nrM0 = mat0->GetExtents()[0].GetSize();
  int ncM0 = mat0->GetExtents()[1].GetSize();
  int nrM1 = mat1->GetExtents()[0].GetSize();
  int ncM1 = mat1->GetExtents()[1].GetSize();
  if (mat0->GetExtents()[2].GetSize() != pointDims)
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

  double **mat0Vecs = new double*[pointDims];
  double **mat1Vecs = new double*[pointDims];
  double **outVecs  = new double*[pointDims];
  for (int i=0; i<pointDims; i++)
  {
    mat0Vecs[i] = new double[nrM0*ncM0];
    mat1Vecs[i] = new double[nrM1*ncM1];
    outVecs[i]  = new double[nrM0*ncM1];
  }
  vtkSVNURBSUtils::PointMatrixToVectors(mat0, mat0Vecs);
  vtkSVNURBSUtils::PointMatrixToVectors(mat1, mat1Vecs);
  for (int i=0; i<pointDims; i++)
  {
    if (vtkSVNURBSUtils::DGEMM(mat0Vecs[i], nrM0, ncM0,
                             mat1Vecs[i], nrM1, ncM1, outVecs[i]) != SV_OK)
    {
      for (int i=0; i<pointDims; i++)
      {
        delete [] mat0Vecs[i];
        delete [] mat1Vecs[i];
        delete [] outVecs[i];
      }
      delete [] mat0Vecs;
      delete [] mat1Vecs;
      delete [] outVecs;
      return SV_ERROR;
    }
  }
  vtkSVNURBSUtils::VectorsToPointMatrix(outVecs, nrM0, ncM1, pointDims, output);

  for (int i=0; i<pointDims; i++)
  {
    delete [] mat0Vecs[i];
    delete [] mat1Vecs[i];
    delete [] outVecs[i];
  }
      delete [] mat0Vecs;
      delete [] mat1Vecs;
      delete [] outVecs;

  return SV_OK;
}

// ----------------------
// PointMatrixMatrixForDGEMM
// ----------------------
int vtkSVNURBSUtils::PointMatrixMatrixForDGEMM(vtkTypedArray<double> *mat0,
                                             vtkTypedArray<double> *mat1,
                                             const int pointDims,
                                             vtkTypedArray<double> *output)
{
  int nrM0 = mat0->GetExtents()[0].GetSize();
  int ncM0 = mat0->GetExtents()[1].GetSize();
  int nrM1 = mat1->GetExtents()[0].GetSize();
  int ncM1 = mat1->GetExtents()[1].GetSize();
  if (mat0->GetExtents()[2].GetSize() != pointDims)
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

  double *mat1Vec   = new double[nrM1*ncM1];
  double **mat0Vecs = new double*[pointDims];
  double **outVecs  = new double*[pointDims];
  for (int i=0; i<pointDims; i++)
  {
    mat0Vecs[i] = new double[nrM0*ncM0];
    outVecs[i]  = new double[nrM0*ncM1];
  }
  vtkSVNURBSUtils::PointMatrixToVectors(mat0, mat0Vecs);
  vtkSVNURBSUtils::MatrixToVector(mat1, mat1Vec);
  for (int i=0; i<pointDims; i++)
  {
    if (vtkSVNURBSUtils::DGEMM(mat0Vecs[i], nrM0, ncM0,
                             mat1Vec, nrM1, ncM1, outVecs[i]) != SV_OK)
    {
      delete [] mat1Vec;
      for (int i=0; i<pointDims; i++)
      {
        delete [] mat0Vecs[i];
        delete [] outVecs[i];
      }
      delete [] mat0Vecs;
      delete [] outVecs;
      return SV_ERROR;
    }
  }
  vtkSVNURBSUtils::VectorsToPointMatrix(outVecs, nrM0, ncM1, pointDims, output);

  delete [] mat1Vec;
  for (int i=0; i<pointDims; i++)
  {
    delete [] mat0Vecs[i];
    delete [] outVecs[i];
  }
  delete [] mat0Vecs;
  delete [] outVecs;

  return SV_OK;
}

// ----------------------
// MatrixPointMatrixForDGEMM
// ----------------------
int vtkSVNURBSUtils::MatrixPointMatrixForDGEMM(vtkTypedArray<double> *mat0,
                                       vtkTypedArray<double> *mat1,
                                       const int pointDims,
                                       vtkTypedArray<double> *output)
{
  int nrM0 = mat0->GetExtents()[0].GetSize();
  int ncM0 = mat0->GetExtents()[1].GetSize();
  int nrM1 = mat1->GetExtents()[0].GetSize();
  int ncM1 = mat1->GetExtents()[1].GetSize();
  if (mat1->GetExtents()[2].GetSize() != pointDims)
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

  double *mat0Vec   = new double[nrM0*ncM0];
  double **mat1Vecs = new double*[pointDims];
  double **outVecs  = new double*[pointDims];
  for (int i=0; i<pointDims; i++)
  {
    mat1Vecs[i] = new double[nrM1*ncM1];
    outVecs[i]  = new double[nrM0*ncM1];
  }
  vtkSVNURBSUtils::MatrixToVector(mat0, mat0Vec);
  vtkSVNURBSUtils::PointMatrixToVectors(mat1, mat1Vecs);
  for (int i=0; i<pointDims; i++)
  {
    if (vtkSVNURBSUtils::DGEMM(mat0Vec, nrM0, ncM0,
                             mat1Vecs[i], nrM1, ncM1, outVecs[i]) != SV_OK)
    {
      delete [] mat0Vec;
      for (int i=0; i<pointDims; i++)
      {
        delete [] mat1Vecs[i];
        delete [] outVecs[i];
      }
      delete [] mat1Vecs;
      delete [] outVecs;
      return SV_ERROR;
    }
  }
  vtkSVNURBSUtils::VectorsToPointMatrix(outVecs, nrM0, ncM1, pointDims, output);

  delete [] mat0Vec;
  for (int i=0; i<pointDims; i++)
  {
    delete [] mat1Vecs[i];
    delete [] outVecs[i];
  }
  delete [] mat1Vecs;
  delete [] outVecs;

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

//// ----------------------
//// StructuredGridToTypedArray
//// ----------------------
//int vtkSVNURBSUtils::StructuredGridToTypedArray(vtkStructuredGrid *grid, vtkTypedArray<double> *output)
//{
//  int dim[3];
//  grid->GetDimensions(dim);
//
//  if (dim[2] != 1)
//  {
//    fprintf(stderr,"3 Dimensions are not yet supported\n");
//    return SV_ERROR;
//  }
//
//  //2D array with third dimensions the coordinates
//  output->Resize(dim[0], dim[1], 3);
//
//  for (int i=0; i<dim[0]; i++)
//  {
//    for (int j=0; j<dim[1]; j++)
//    {
//      int pos[3]; pos[2] =0;
//      pos[0] = i;
//      pos[1] = j;
//      int ptId = vtkStructuredData::ComputePointId(dim, pos);
//      double pt[3];
//
//      grid->GetPoint(ptId, pt);
//      for (int k=0; k<3; k++)
//      {
//        output->SetValue(i, j, k, pt[k]);
//      }
//    }
//  }
//
//  return SV_OK;
//}

// ----------------------
// StructuredGridToTypedArray
// ----------------------
int vtkSVNURBSUtils::StructuredGridToTypedArray(vtkStructuredGrid *grid, vtkTypedArray<double> *output)
{
  int dim[3];
  grid->GetDimensions(dim);

  int dims;
  if (dim[2] == 1 && dim[1] == 1 && dim[0] != 1)
    dims = 1;
  else if (dim[2] == 1 && dim[1] != 1 && dim[0] != 1)
    dims = 2;
  else if (dim[2] != 1 && dim[1] != 1 && dim[0] != 1)
    dims = 3;
  else
  {
    fprintf(stderr,"Please provide a regular structured grid\n");
    return SV_ERROR;
  }

  //2D array with third dimensions the coordinates
  vtkArrayExtents size;
  size.SetDimensions(dims+1);
  for (int i=0; i<dims; i++)
    size.SetExtent(i, vtkArrayRange(0, dim[i]));
  size.SetExtent(dims, vtkArrayRange(0, 3));

  output->Resize(size);

  for (int i=0; i<dim[0]; i++)
  {
    for (int j=0; j<dim[1]; j++)
    {
      for (int k=0; k<dim[2]; k++)
      {
        int pos[3];
        pos[0] = i;
        pos[1] = j;
        pos[2] = k;
        int ptId = vtkStructuredData::ComputePointId(dim, pos);
        double pt[3];

        grid->GetPoint(ptId, pt);
        vtkArrayCoordinates loc;
        loc.SetDimensions(dims+1);
        loc.SetCoordinate(0, i);
        if (dims >= 2)
         loc.SetCoordinate(1, j);
        if (dims == 3)
         loc.SetCoordinate(2, k);

        for (int l=0; l<3; l++)
        {
          loc.SetCoordinate(dims, l);
          output->SetValue(loc, pt[l]);
        }
      }
    }
  }

  return SV_OK;
}

// ----------------------
// ControlGridToTypedArray
// ----------------------
int vtkSVNURBSUtils::ControlGridToTypedArraySPECIAL(vtkSVControlGrid *grid, vtkTypedArray<double> *output)
{
  int dim[3];
  grid->GetDimensions(dim);

  if (dim[2] != 1)
  {
    fprintf(stderr,"3 Dimensions are not yet supported\n");
    return SV_ERROR;
  }

  //Get weights
  vtkDataArray *weights = grid->GetPointData()->GetArray("Weights");
  if (weights == NULL)
  {
    fprintf(stderr,"No weights on control point grid\n");
    return SV_ERROR;
  }

  //2D array with third dimensions the coordinates + weights
  output->Resize(dim[0], dim[1], 4);

  for (int i=0; i<dim[0]; i++)
  {
    for (int j=0; j<dim[1]; j++)
    {
      int pos[3]; pos[2] =0;
      pos[0] = i;
      pos[1] = j;
      int ptId = vtkStructuredData::ComputePointId(dim, pos);
      double pw[4];

      grid->GetPoint(ptId, pw);
      pw[3] = weights->GetTuple1(ptId);

      // This needs to be noted! We are doing this for the reationality of
      // surfaces. Multiplying point by weight, but also keeping the fourth
      // location so that in the end we have a sum of the weights as well.
      vtkMath::MultiplyScalar(pw, pw[3]);

      for (int k=0; k<4; k++)
      {
        output->SetValue(i, j, k, pw[k]);
      }
    }
  }

  return SV_OK;
}

// ----------------------
// ControlGridToTypedArray
// ----------------------
int vtkSVNURBSUtils::ControlGridToTypedArraySPECIAL(vtkSVControlGrid *grid,  const int dim0, const int dim1, const int dim2, const int comp2, vtkTypedArray<double> *output)
{
  int dim[3];
  grid->GetDimensions(dim);
  //Get weights
  vtkDataArray *weights = grid->GetPointData()->GetArray("Weights");
  if (weights == NULL)
  {
    fprintf(stderr,"No weights on control point grid\n");
    return SV_ERROR;
  }

  //2D array with third dimensions the coordinates + weights
  output->Resize(dim[dim0], dim[dim1], 4);

  for (int i=0; i<dim[dim0]; i++)
  {
    for (int j=0; j<dim[dim1]; j++)
    {
      int pos[3]; pos[dim2] = comp2;
      pos[dim0] = i;
      pos[dim1] = j;
      int ptId = vtkStructuredData::ComputePointId(dim, pos);
      double pw[4];

      grid->GetPoint(ptId, pw);
      pw[3] = weights->GetTuple1(ptId);

      // This needs to be noted! We are doing this for the reationality of
      // NURBS. Multiplying point by weight, but also keeping the fourth
      // location so that in the end we have a sum of the weights as well.
      vtkMath::MultiplyScalar(pw, pw[3]);

      for (int k=0; k<4; k++)
      {
        vtkArrayCoordinates loc;
        loc.SetDimensions(3);
        loc.SetCoordinate(dim0, i);
        loc.SetCoordinate(dim1, j);
        loc.SetCoordinate(dim2, k);
        output->SetValue(loc, pw[k]);
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
  int dim[4];
  for (int i=0; i<dims; i++)
  {
    dim[i] = array->GetExtents()[i].GetSize();
  }

  if (dims == 2)
  {
    if (dim[1] != 3)
    {
      fprintf(stderr,"Second dimension should have xyz coordinates\n");
      return SV_ERROR;
    }
    dim[1] = 1; dim[2] = 1;
  }
  else if (dims == 3)
  {
    if (dim[2] != 3)
    {
      fprintf(stderr,"Third dimension should have xyz coordinates\n");
      return SV_ERROR;
    }
    dim[2] = 1;
  }
  else if (dims == 4)
  {
    if (dim[3] != 3)
    {
      fprintf(stderr,"Fourth dimension should have xyz coordinates\n");
      return SV_ERROR;
    }
  }
  else
  {
    fprintf(stderr,"This dimension not supported\n");
    return SV_ERROR;
  }

  output->SetDimensions(dim[0], dim[1], dim[2]);
  output->GetPoints()->SetNumberOfPoints(dim[0]*dim[1]*dim[2]);

  for (int i=0; i<dim[0]; i++)
  {
    for (int j=0; j<dim[1]; j++)
    {
      for (int k=0; k<dim[2]; k++)
      {
        int pos[3];
        pos[0] = i;
        pos[1] = j;
        pos[2] = k;
        int ptId = vtkStructuredData::ComputePointId(dim, pos);

        double pt[3];
        vtkArrayCoordinates loc;
        loc.SetDimensions(dims);
        loc.SetCoordinate(0, i);
        if (dims >= 3)
          loc.SetCoordinate(1, j);
        if (dims == 4)
          loc.SetCoordinate(2, k);

        for (int l=0; l<3; l++)
        {
          loc.SetCoordinate(dims-1, l);
          pt[l] = array->GetValue(loc);
        }
        output->GetPoints()->SetPoint(ptId, pt);
      }
    }
  }

    return SV_OK;
}

// ----------------------
// SetMatrixOfDim4Grid
// ----------------------
int vtkSVNURBSUtils::SetMatrixOfDim4Grid(vtkTypedArray<double> *matrix, vtkTypedArray<double> *grid, const int dim0, const int dim1, const int dim2, const int comp2, const int num3)
{
  int matrixDims = matrix->GetDimensions();
  int mdim[3];
  for (int i=0; i<3; i++)
  {
    mdim[i] = matrix->GetExtents()[i].GetSize();
  }
  if (matrixDims > 3)
  {
    fprintf(stderr,"More than 3 Dimensions are not supported\n");
    return SV_ERROR;
  }
  if (mdim[2] != num3)
  {
    fprintf(stderr,"Third dimension doesn't match\n");
    return SV_ERROR;
  }

  int gridDims = grid->GetDimensions();
  int gdim[4];
  for (int i=0; i<4; i++)
  {
    gdim[i] = grid->GetExtents()[i].GetSize();
  }
  if (gridDims > 4)
  {
    fprintf(stderr,"More than 4 Dimensions are not supported\n");
    return SV_ERROR;
  }
  if (gdim[3] != num3)
  {
    fprintf(stderr,"Fourth dimension doesn't match %d\n", gdim[3]);
    return SV_ERROR;
  }

  if (mdim[0] != gdim[dim0])
  {
    fprintf(stderr,"Setting %d dimension of grid with size %d, but given matrix with first dimension %d\n", dim0, gdim[dim0], mdim[0]);
  }

  if (mdim[1] != gdim[dim1])
  {
    fprintf(stderr,"Setting %d dimension of grid with size %d, but given matrix with first dimension %d\n", dim1, gdim[dim1], mdim[1]);
  }

  for (int i=0; i<mdim[0]; i++)
  {
    for (int j=0; j<mdim[1]; j++)
    {
      for (int k=0; k<num3; k++)
      {
        double val = matrix->GetValue(i, j, k);

        vtkArrayCoordinates loc;
        loc.SetDimensions(4);
        loc.SetCoordinate(dim0, i);
        loc.SetCoordinate(dim1, j);
        loc.SetCoordinate(dim2, comp2);
        loc.SetCoordinate(3, k);
        grid->SetValue(loc, val);
      }
    }
  }

  return SV_OK;
}

// ----------------------
// GetMatrixOfDim4Grid
// ----------------------
int vtkSVNURBSUtils::GetMatrixOfDim4Grid(vtkTypedArray<double> *grid, const int dim0, const int dim1, const int dim2, const int comp2, const int num3, vtkTypedArray<double> *matrix)
{
  int gridDims = grid->GetDimensions();
  int gdim[4];
  for (int i=0; i<4; i++)
  {
    gdim[i] = grid->GetExtents()[i].GetSize();
  }
  if (gridDims > 4)
  {
    fprintf(stderr,"More than 4 Dimensions are not supported\n");
    return SV_ERROR;
  }
  if (gdim[3] != num3)
  {
    fprintf(stderr,"Fourth dimension should have not equal\n");
    return SV_ERROR;
  }

  matrix->Resize(gdim[dim0], gdim[dim1], num3);

  for (int i=0; i<gdim[dim0]; i++)
  {
    for (int j=0; j<gdim[dim1]; j++)
    {
      for (int k=0; k<num3; k++)
      {
        vtkArrayCoordinates loc;
        loc.SetDimensions(4);
        loc.SetCoordinate(dim0, i);
        loc.SetCoordinate(dim1, j);
        loc.SetCoordinate(dim2, comp2);
        loc.SetCoordinate(3, k);
        double val = grid->GetValue(loc);

        matrix->SetValue(i, j, k, val);
      }
    }
  }

  return SV_OK;
}

// ----------------------
// TypedArrayToStructuredGridRational
// ----------------------
int vtkSVNURBSUtils::TypedArrayToStructuredGridRational(vtkTypedArray<double> *array, vtkStructuredGrid *output)
{
  int dims = array->GetDimensions();
  //2D array with third dimensions the coordinates
  int dim[4];
  for (int i=0; i<dims; i++)
  {
    dim[i] = array->GetExtents()[i].GetSize();
  }

  if (dims == 2)
  {
    if (dim[1] != 4)
    {
      fprintf(stderr,"Second dimension should have xyz coordinates and weight\n");
      return SV_ERROR;
    }
    dim[1] = 1; dim[2] = 1;
  }
  else if (dims == 3)
  {
    if (dim[2] != 4)
    {
      fprintf(stderr,"Third dimension should have xyz coordinates and weight\n");
      return SV_ERROR;
    }
    dim[2] = 1;
  }
  else if (dims == 4)
  {
    if (dim[3] != 4)
    {
      fprintf(stderr,"Fourth dimension should have xyz coordinates and weight\n");
      return SV_ERROR;
    }
  }
  else
  {
    fprintf(stderr,"This dimension not supported\n");
    return SV_ERROR;
  }

  output->SetDimensions(dim[0], dim[1], dim[2]);
  output->GetPoints()->SetNumberOfPoints(dim[0]*dim[1]*dim[2]);

  //ND array with third dimensions the coordinates
  for (int i=0; i<dim[0]; i++)
  {
    for (int j=0; j<dim[1]; j++)
    {
      for (int k=0; k<dim[2]; k++)
      {
        int pos[3];
        pos[0] = i;
        pos[1] = j;
        pos[2] = k;
        int ptId = vtkStructuredData::ComputePointId(dim, pos);

        double pt[3];
        vtkArrayCoordinates loc;
        loc.SetDimensions(dims);
        loc.SetCoordinate(0, i);
        if (dims >= 3)
          loc.SetCoordinate(1, j);
        if (dims == 4)
          loc.SetCoordinate(2, k);

        for (int l=0; l<3; l++)
        {
          loc.SetCoordinate(dims-1, l);
          pt[l] = array->GetValue(loc);
        }
        loc.SetCoordinate(dims-1, 3);
        double weight_total = array->GetValue(loc);
        vtkMath::MultiplyScalar(pt, 1./weight_total);

        output->GetPoints()->SetPoint(ptId, pt);
      }
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
int vtkSVNURBSUtils::PrintArray(vtkDataArray *arr)
{
  int num  = arr->GetNumberOfTuples();
  int comp = arr->GetNumberOfComponents();
  fprintf(stdout,"Array: %d tuples, %d components\n", num, comp);
  fprintf(stdout,"----------------------------------------------------------\n");
  for (int i=0; i<num; i++)
  {
    for (int j=0; j<comp; j++)
    {
      fprintf(stdout,"%8.4f ", arr->GetComponent(i, j));
    }
    fprintf(stdout,"\n");
  }
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
        fprintf(stdout,"%8.4f ", vec->GetValue(i, j));
      }
    }
    else
    {
      fprintf(stdout,"%8.4f ", vec->GetValue(i));
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
          fprintf(stdout,"%8.4f ", mat->GetValue(i, j, k));
        }
      }
      else
      {
        fprintf(stdout,"%8.4f ", mat->GetValue(i, j));
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
      fprintf(stdout,"| %8.4f %8.4f %8.4f |", pt[0], pt[1], pt[2]);
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
      fprintf(stdout,"%8.4f ",pt[j]);
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
int vtkSVNURBSUtils::PointMatrixToVectors(vtkTypedArray<double> *mat, double **matVecs)
{
  int nr = mat->GetExtents()[0].GetSize();
  int nc = mat->GetExtents()[1].GetSize();
  int np = mat->GetExtents()[2].GetSize();

  for (int i=0; i<nc; i++)
  {
    for (int j=0; j<nr; j++)
    {
      for (int k=0; k<np; k++)
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
int vtkSVNURBSUtils::VectorsToPointMatrix(double **matVecs, const int nr, const int nc, const int np, vtkTypedArray<double> *mat)
{
  for (int i=0; i<nc; i++)
  {
    for (int j=0; j<nr; j++)
    {
      for (int k=0; k<np; k++)
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
    fprintf(stdout,"| %8.4f |\n", arr[i]);
  }
  fprintf(stdout,"----------------------------------------------------------\n");

  return SV_OK;
}
