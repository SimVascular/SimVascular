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

#include "vtkSVSparseMatrix.h"

#include "vtkObjectFactory.h"
#include "vtkSmartPointer.h"
#include "vtkSVGlobals.h"

#include <algorithm>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVSparseMatrix);

// ----------------------
// Constructor
// ----------------------
vtkSVSparseMatrix::vtkSVSparseMatrix()
{
  this->NumberOfRows    = 0;
  this->NumberOfColumns = 0;
}

// ----------------------
// Destructor
// ----------------------
vtkSVSparseMatrix::~vtkSVSparseMatrix()
{
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVSparseMatrix::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);

  os << indent << "Number of rows: " << this->NumberOfRows << "\n";
  os << indent << "Number of columns: " << this->NumberOfColumns << "\n";
}

// ----------------------
// SetNumberOfRows
// ----------------------
void vtkSVSparseMatrix::SetNumberOfRows(int numRows)
{
  this->NumberOfRows = numRows;
  this->Data.resize(numRows);
  this->Cols.resize(numRows);
}

// ----------------------
// SetMatrixSize
// ----------------------
void vtkSVSparseMatrix::SetMatrixSize(int numRows, int numCols)
{
  this->NumberOfRows    = numRows;
  this->NumberOfColumns = numCols;
  this->Data.resize(numRows);
  this->Cols.resize(numRows);
}

// ----------------------
// GetNumberOfElements
// ----------------------
int vtkSVSparseMatrix::GetNumberOfElements() const
{
  int numEls = 0;
  for (int i = 0; i < this->NumberOfRows; i++)
    numEls += this->Cols[i].size();

  return numEls;
}

// ----------------------
// MultiplyColumn
// ----------------------
void vtkSVSparseMatrix::MultiplyColumn(
    const double *column, double *output) const
{
  for (int i = 0; i < this->NumberOfRows; i++)
  {
    output[i] = 0.0;
    for (int j = 0; j < this->Cols[i].size(); j++)
      output[i] += this->Data[i][j] * column[this->Cols[i][j]];
  }
}

// ----------------------
// SetElement
// ----------------------
void vtkSVSparseMatrix::SetElement(int row, int col, double value)
{
  if (value == 0.0) {
    for (int j = 0; j < this->Cols[row].size(); j++)
    {
      if (this->Cols[row][j] == col)
      {
        this->Cols[row].erase(this->Cols[row].begin() + j);
        this->Data[row].erase(this->Data[row].begin() + j);
        break;
      }
    }
    return;
  }

  for (int j = 0; j < this->Cols[row].size(); j++)
  {
    if (this->Cols[row][j] == col)
    {
      this->Data[row][j] = value;
      return;
    }
  }

  this->Cols[row].push_back(col);
  this->Data[row].push_back(value);
}


// ----------------------
// GetElement
// ----------------------
double vtkSVSparseMatrix::GetElement(int row, int col) const
{
  for (int j = 0; j < this->Cols[row].size(); j++)
  {
    if (this->Cols[row][j] == col)
      return this->Data[row][j];
  }
  return 0.0;
}

// ----------------------
// Transpose
// ----------------------
int vtkSVSparseMatrix::Transpose(vtkSVSparseMatrix *transpose)
{
  transpose->SetMatrixSize(this->NumberOfColumns, this->NumberOfRows);

  for (int i = 0; i < this->NumberOfRows; i++)
  {
    for (int j = 0; j < this->Cols[i].size(); j++)
      transpose->SetElement(this->Cols[i][j], i, this->Data[i][j]);
  }
  return SV_OK;
}
