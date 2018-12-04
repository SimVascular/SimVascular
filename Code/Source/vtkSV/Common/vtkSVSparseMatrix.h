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

/**
 *  \class  vtkSVSparseMatrix
 *  \brief This is class of useful functions for reading and writing vtk files.
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVSparseMatrix_h
#define vtkSVSparseMatrix_h

#include "vtkObject.h"
#include "vtkSVCommonModule.h" // For export

#include <vector>

class VTKSVCOMMON_EXPORT vtkSVSparseMatrix : public vtkObject
{
public:
  static vtkSVSparseMatrix *New();
  vtkTypeMacro(vtkSVSparseMatrix,vtkObject);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  /// \breif Get/Set for number of rows
  void SetNumberOfRows(int numRows);
  int  GetNumberOfRows() {return NumberOfRows;}
  //@}

  //@{
  /// \brief Get/Set for number of columns
  void SetNumberOfColumns(int numCols) {NumberOfColumns = numCols;}
  int  GetNumberOfColumns() {return NumberOfColumns;}
  //@}

  /// \brief Set the matrix rows and columns
  void SetMatrixSize(int numRows, int numCols);

  /// \brief Multiply a column by the matrix
  ///  \param the column vector to multiply
  ///  \param output the result, must be properly allocated
  void MultiplyColumn(const double *column, double *output) const;

  /// \brief Set an element of the matrix
  void SetElement(int row, int col, double value);

  /// \brief Get an element of the matrix
  double GetElement(int row, int col) const;

  /// \brief Transpose the matrix
  /// \param transpose, the transposed matrix
  int Transpose(vtkSVSparseMatrix *transpose);

  /// \brief Get the total number of non-zero elements in the matrix
  int GetNumberOfElements() const;

protected:
  vtkSVSparseMatrix();
  ~vtkSVSparseMatrix();

  std::vector<std::vector<double> > Data;
  std::vector<std::vector<int> >    Cols;
  int NumberOfRows;
  int NumberOfColumns;

private:
  vtkSVSparseMatrix(const vtkSVSparseMatrix&);  // Not implemented.
  void operator=(const vtkSVSparseMatrix&);  // Not implemented.

};

#endif  // vtkSVSparseMatrix_h
