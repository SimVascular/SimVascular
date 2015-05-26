// Author: Mingcheng Chen (linyufly@gmail.com)

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
