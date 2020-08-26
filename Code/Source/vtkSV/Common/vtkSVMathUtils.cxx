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

#include "vtkSVMathUtils.h"

#include "vtkSVSparseMatrix.h"

#include "vtkSmartPointer.h"
#include "vtkSVGlobals.h"
#include <cmath>
#include <cstdio>

#include <algorithm>
#include <cmath>

// ----------------------
// Multiply_ATA_b
// ----------------------
/// \details Multiply A^tA with b.
void vtkSVMathUtils::Multiply_ATA_b(vtkSVSparseMatrix *a_trans,
                                    vtkSVSparseMatrix *a,
                                    const double *b, double *c)
{
  double *temp = new double[a->GetNumberOfRows()];

  a->MultiplyColumn(b, temp);
  a_trans->MultiplyColumn(temp, c);

  delete [] temp;
}

// ----------------------
// InnerProduct
// ----------------------
double vtkSVMathUtils::InnerProduct(const double a[], const double b[], int n, double &product)
{
  product = 0.0;
  for (int c = 0; c < n; c++)
    product += a[c] * b[c];
  return product;
}

// ----------------------
// Add
// ----------------------
void vtkSVMathUtils::Add(const double a[], const double alpha, const double b[], const double beta, const int n, double c[])
{
  for (int i = 0; i < n; i++)
    c[i] = a[i] * alpha + b[i] * beta;
}

// ----------------------
// Add
// ----------------------
void vtkSVMathUtils::Add(const double a[], const double b[], const double beta, const int n, double c[])
{
  for (int i = 0; i < n; i++)
    c[i] = a[i] + b[i] * beta;
}

// ----------------------
// Add
// ----------------------
int vtkSVMathUtils::Add(double a[], double b[], double result[], const int size)
{
  for (int i=0; i<size; i++)
    result[i] = a[i]+b[i];

  return SV_OK;
}

// ----------------------
// Subtract
// ----------------------
int vtkSVMathUtils::Subtract(double a[], double b[], double result[], const int size)
{
  for (int i=0; i<size; i++)
    result[i] = a[i]-b[i];

  return SV_OK;
}

// ----------------------
// MultiplyScalar
// ----------------------
int vtkSVMathUtils::MultiplyScalar(double a[], double scalar, const int size)
{
  for (int i=0; i<size; i++)
    a[i] = a[i]*scalar;

  return SV_OK;
}

// ----------------------
// ConjugateGradient
// ----------------------
int vtkSVMathUtils::ConjugateGradient(vtkSVSparseMatrix *a,
                                       const double *b,
                                       int num_iterations,
                                       double *x, const double epsilon)
{
  vtkNew(vtkSVSparseMatrix, a_trans);
  a->Transpose(a_trans);

  double *a_trans_b = new double[a_trans->GetNumberOfRows()];
  a_trans->MultiplyColumn(b, a_trans_b);

  // Solve a_trans * a * x = a_trans_b.
  double *r = new double[a_trans->GetNumberOfRows()];
  double *p = new double[a_trans->GetNumberOfRows()];

  double *temp = new double[a_trans->GetNumberOfRows()];

  // temp = A'A * x
  vtkSVMathUtils::Multiply_ATA_b(a_trans, a, x, temp);

  // r = A'b - temp
  vtkSVMathUtils::Add(a_trans_b, temp, -1.0, a_trans->GetNumberOfRows(), r);

  // p = r
  std::copy(r, r + a_trans->GetNumberOfRows(), p);

  // rs_old = r' * r
  double rs_old = 0.0;
  vtkSVMathUtils::InnerProduct(r, r, a_trans->GetNumberOfRows(), rs_old);

  if (sqrt(rs_old) < epsilon) {
    //printf("The initial solution is good.\n");
    delete [] r;
    delete [] p;
    delete [] temp;
    delete [] a_trans_b;
    return rs_old;
  }

  int iteration = 0;
  for (iteration = 0;
       iteration < num_iterations && iteration < a_trans->GetNumberOfRows();
       iteration++)
  {
    // temp = A'A * p
    vtkSVMathUtils::Multiply_ATA_b(a_trans, a, p, temp);

    // alpha = rs_old / (p' * temp)
    double alpha_den = 0.0;
    vtkSVMathUtils::InnerProduct(p, temp, a_trans->GetNumberOfRows(), alpha_den);
    double alpha = rs_old / alpha_den;

    // x = x + alpha * p
    vtkSVMathUtils::Add(x, p, alpha, a_trans->GetNumberOfRows(), x);

    // r = r - alpha * temp
    vtkSVMathUtils::Add(r, temp, -alpha, a_trans->GetNumberOfRows(), r);

    // rs_new = r' * r
    double rs_new = 0.0;
    vtkSVMathUtils::InnerProduct(r, r, a_trans->GetNumberOfRows(), rs_new);

    // Traditionally, if norm(rs_new) is small enough, the iteration can stop.
    if (sqrt(rs_new) < epsilon)
    {
      /// DEBUG ///
      //printf("rs_new = %.20lf\n", rs_new);

      break;
    }

    // p = r + (rs_new / rs_old) * p
    vtkSVMathUtils::Add(r, p, rs_new / rs_old, a_trans->GetNumberOfRows(), p);

    // rs_old = rs_new
    rs_old = rs_new;

    /// DEBUG ///
    //printf("  rs_old = %.20lf\n", rs_old);
  }

  /// DEBUG ///
  //printf("rs_old = %.20lf\n", rs_old);
  //printf("iterations = %d\n", iteration);

  delete [] r;
  delete [] p;
  delete [] temp;
  delete [] a_trans_b;

  return rs_old;
}

// ----------------------
// ComputeTriangleArea
// ----------------------
double vtkSVMathUtils::ComputeTriangleArea(double pt0[3], double pt1[3],
                                           double pt2[3])
{
  double area = 0.0;
  area += (pt0[0]*pt1[1])-(pt1[0]*pt0[1]);
  area += (pt1[0]*pt2[1])-(pt2[0]*pt1[1]);
  area += (pt2[0]*pt0[1])-(pt0[0]*pt2[1]);
  area *= 0.5;

  return area;
}

// ----------------------
// Distance
// ----------------------
double vtkSVMathUtils::Distance(const double pt0[3], const double pt1[3])
{
  return sqrt(pow(pt1[0] - pt0[0], 2.0) +
              pow(pt1[1] - pt0[1], 2.0) +
              pow(pt1[2] - pt0[2], 2.0));
}

// ----------------------
// Distance
// ----------------------
double vtkSVMathUtils::Distance(const double pt0[3], const double pt1[3], const int size)
{
  double val = 0.0;
  for (int i=0; i<size; i++)
    val += (pow(pt1[i] - pt0[i], 2.0));
  return sqrt(val);
}

// ----------------------
// VectorDotProduct
// ----------------------
int vtkSVMathUtils::VectorDotProduct(vtkDataArray *v0, vtkDataArray *v1, double product[], int numVals, int numComps)
{
  // Initialize product to zero
  for (int i=0; i<numComps; i++)
    product[i] = 0.0;

  // Loop through all tuples
  for (int i=0; i<numVals; i++)
  {
    // Loop through all components
    for (int j=0; j<numComps; j++)
    {
      double val0, val1;
      val0 = v0->GetComponent(i, j);
      val1 = v1->GetComponent(i, j);
      product[j] += val0 * val1;
    }
  }

  return SV_OK;
}

// ----------------------
// VectorAdd
// ----------------------
int vtkSVMathUtils::VectorAdd(vtkDataArray *v0, vtkDataArray *v1, double scalar, vtkDataArray *result, int numVals, int numComps)
{
  // Loop through all tuples
  for (int i=0; i<numVals; i++)
  {
    // Loop through all components
    for (int j=0; j<numComps; j++)
    {
      double val0, val1;
      val0 = v0->GetComponent(i, j);
      val1 = v1->GetComponent(i, j);
      double sum = val0 + scalar * val1;
      result->SetComponent(i, j, sum);
    }
  }

  return SV_OK;
}

// ----------------------
// Binom
// ----------------------
double vtkSVMathUtils::Binom(const int n, const int k)
{
  if (n>=k)
  {
    int den=1;
    int num=1;
    for (int i=1; i<=k; i++)
      den*=i;
    for (int j=n-k+1; j<=n; j++)
      num*=j;

    return num/den;
  }
  return 0.0;
}
