// Author: Mingcheng Chen (linyufly@gmail.com)

#include "sparse_matrix.h"

#include <algorithm>

void SparseMatrix::multiply_column(
    const double *column, double *result) const {
  for (int r = 0; r < num_rows_; r++) {
    result[r] = 0.0;
    for (int c = 0; c < col_[r].size(); c++) {
      result[r] += data_[r][c] * column[col_[r][c]];
    }
  }
}

