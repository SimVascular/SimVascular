// Author: Mingcheng Chen (linyufly@gmail.com)

#ifndef SVMATH_H_
#define SVMATH_H_

class SparseMatrix;

class svMath {
 public:
  static void conjugate_gradient(
      const SparseMatrix &a, const double *b, int num_iterations, double *x);
};

#endif  // MATH_H_
