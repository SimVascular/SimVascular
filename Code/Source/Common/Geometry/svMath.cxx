// Author: Mingcheng Chen (linyufly@gmail.com)
/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved. 
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
 *
 *=========================================================================*/

#include "svMath.h"

#include "sparse_matrix.h"

#include <cmath>
#include <cstdio>

#include <algorithm>

namespace {

const double kEpsilon = 1e-8;

// Multiply A^tA with b.
void multiply_ata_b(
    const SparseMatrix &a_trans, const SparseMatrix &a,
    const double *b, double *c) {
  double *temp = new double[a.get_num_rows()];

  a.multiply_column(b, temp);
  a_trans.multiply_column(temp, c);

  delete [] temp;
}

double inner_product(const double *a, const double *b, int n) {
  double result = 0.0;
  for (int c = 0; c < n; c++) {
    result += a[c] * b[c];
  }
  return result;
}

void add(const double *a, const double *b, double beta, int n, double *c) {
  for (int i = 0; i < n; i++) {
    c[i] = a[i] + b[i] * beta;
  }
}

}

void svMath::conjugate_gradient(
    const SparseMatrix &a, const double *b, int num_iterations, double *x) {
  SparseMatrix a_trans = a.transpose();

  double *a_trans_b = new double[a_trans.get_num_rows()];
  a_trans.multiply_column(b, a_trans_b);

  // Solve a_trans * a * x = a_trans_b.
  double *r = new double[a_trans.get_num_rows()];
  double *p = new double[a_trans.get_num_rows()];

  double *temp = new double[a_trans.get_num_rows()];

  // temp = A'A * x
  multiply_ata_b(a_trans, a, x, temp);

  // r = A'b - temp
  add(a_trans_b, temp, -1.0, a_trans.get_num_rows(), r);

  // p = r
  std::copy(r, r + a_trans.get_num_rows(), p); 

  // rs_old = r' * r
  double rs_old = inner_product(r, r, a_trans.get_num_rows());

  if (sqrt(rs_old) < kEpsilon) {
    printf("The initial solution is good.\n");
    return;
  }

  for (int iteration = 0;
       iteration < num_iterations && iteration < a_trans.get_num_rows();
       iteration++) {
    // temp = A'A * p
    multiply_ata_b(a_trans, a, p, temp);

    // alpha = rs_old / (p' * temp)
    double alpha = rs_old / inner_product(p, temp, a_trans.get_num_rows());

    // x = x + alpha * p
    add(x, p, alpha, a_trans.get_num_rows(), x);

    // r = r - alpha * temp
    add(r, temp, -alpha, a_trans.get_num_rows(), r);

    // rs_new = r' * r
    double rs_new = inner_product(r, r, a_trans.get_num_rows());

    // Traditionally, if norm(rs_new) is small enough, the iteration can stop.
    if (sqrt(rs_new) < kEpsilon) {
      /// DEBUG ///
      printf("rs_new = %.20lf\n", rs_new);

      break;
    }

    // p = r + (rs_new / rs_old) * p
    add(r, p, rs_new / rs_old, a_trans.get_num_rows(), p);

    // rs_old = rs_new
    rs_old = rs_new;

    /// DEBUG ///
    printf("  rs_old = %.20lf\n", rs_old);
  }

  /// DEBUG ///
  printf("rs_old = %.20lf\n", rs_old);

  delete [] r;
  delete [] p;
  delete [] temp;
  delete [] a_trans_b;
}

