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

#ifndef SPARSE_MATRIX_H_
#define SPARSE_MATRIX_H_

#include <vector>

class SparseMatrix {
 public:
  SparseMatrix() {
    num_rows_ = num_cols_ = 0;
  }

  SparseMatrix(int num_rows, int num_cols)
      : num_rows_(num_rows), num_cols_(num_cols) {
    data_.resize(num_rows);
    col_.resize(num_rows);
  }

  void multiply_column(const double *column, double *result) const;

  void set_element(int row, int col, double value) {
    if (value == 0.0) {
      for (int c = 0; c < col_[row].size(); c++) {
        if (col_[row][c] == col) {
          col_[row].erase(col_[row].begin() + c);
          data_[row].erase(data_[row].begin() + c);
          break;
        }
      }
      return;
    }

    for (int c = 0; c < col_[row].size(); c++) {
      if (col_[row][c] == col) {
        data_[row][c] = value;
        return;
      }
    }

    col_[row].push_back(col);
    data_[row].push_back(value);
  }

  double get_element(int row, int col) const {
    for (int c = 0; c < col_[row].size(); c++) {
      if (col_[row][c] == col) {
        return data_[row][c];
      }
    }

    return 0.0;
  } 
 
  SparseMatrix transpose() const {
    SparseMatrix result(num_cols_, num_rows_);

    for (int r = 0; r < num_rows_; r++) {
      for (int c = 0; c < col_[r].size(); c++) {
        result.set_element(col_[r][c], r, data_[r][c]);
      }
    }

    return result;
  }

  int num_elements() const {
    int result = 0;
    for (int r = 0; r < num_rows_; r++) {
      result += col_[r].size();
    }

    return result;
  }

  int get_num_rows() const {
    return num_rows_;
  }

  int get_num_cols() const {
    return num_cols_;
  }

 private:
  std::vector<std::vector<double> > data_;
  std::vector<std::vector<int> > col_;
  int num_rows_, num_cols_;
};

#endif  // SPARSE_MATRIX_H_
