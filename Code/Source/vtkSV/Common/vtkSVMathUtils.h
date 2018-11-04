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
 *  \class  vtkSVMathUtils
 *  \brief This is class of useful functions for doing specialized math operations.
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVMathUtils_h
#define vtkSVMathUtils_h

#include "vtkSVSparseMatrix.h"
#include "vtkSVCommonModule.h" // For export

#include "vtkFloatArray.h"

class VTKSVCOMMON_EXPORT vtkSVMathUtils : public vtkObject
{
public:
  vtkTypeMacro(vtkSVMathUtils,vtkObject);

  /** \brief performs conjugate gradient solve given a sparse matrix,
   *  the right hand side, and the vector to solve for with an intial guess.
   *  \param a The sparse matrix, does not neccesarily need to be square.
   *  \param b Right hand side. Number of values should match number of
   *  columns in sparse matrix.
   *  \param num_iterations Set a maximum number of iterations to use, otherwise
   *  the size of the matrix will be used for max.
   *  \param x Vector to solve for with initial guess. Number of values should
   *  equal the number of columns in the sparse matrix.
   *  \param epsilon Desired residual that the conjugate gradient solve should
   *  reach before exiting. */
  static int ConjugateGradient(vtkSVSparseMatrix *a,
                                const double *b, int num_iterations,
                                double *x, const double epsilon);

  /** \brief Does exactly what it says. Multiplies A transpose with A and then
   *  with column vector b.
   *  \param a_trans the transpose of a.
   *  \param a sparse matrix a.
   *  \param b column vector that is equal in size to the number of rows in a.
   *  \return c column vector containing the result. Should also be equal in
   *  size to the number of rows in a. */
  static void Multiply_ATA_b(vtkSVSparseMatrix *a_trans,
                             vtkSVSparseMatrix *a,
                             const double *b, double *c);

  /** \brief Performs the inner product of two vectors of given size.
   *  \param a first vector.
   *  \param b second vector.
   *  \param n size of the vectors.
   *  \return inner product. */
  static double InnerProduct(const double a[], const double b[], int n, double &product);

  /** \brief Function to add to vectors with the possiblity of multiplying by a scalar.
   *  \param a First vector.
   *  \param b Second vector.
   *  \param beta scalar to be multiplied to be before being added.
   *  \param n size of the vector.
   *  \return c result. */
  static void Add(const double a[], const double alpha, const double b[], const double beta, const int n, double c[]);

  /** \brief Function to add to vectors with the possiblity of multiplying by a scalar.
   *  \param a First vector.
   *  \param alpha scalar to be multiplied to a before being added.
   *  \param b Second vector.
   *  \param beta scalar to be multiplied to b before being added.
   *  \param n size of the vector.
   *  \return c result. */
  static void Add(const double a[], const double b[], const double beta, const int n, double c[]);

  /** \brief Addition of two arrays of given size.
   *  \param a first vector.
   *  \param b second vector.
   *  \param size size of the vectors.
   *  \return result that is same size as a and b. */
  static int Add(double a[], double b[], double result[], const int size);

  /** \brief Subtraction of two arrays of given size.
   *  \param a first vector.
   *  \param b second vector.
   *  \param size size of the vectors.
   *  \return result that is same size as a and b. */
  static int Subtract(double a[], double b[], double result[], const int size);

  /** \brief Multiply value to array of given size; edits in place.
   *  \param a vector to be editted.
   *  \param scalar scalar value to be multiplied to vector a.
   *  \param size size of the vector. */
  static int MultiplyScalar(double a[], double scalar, const int size);

  /** \brief Compute the signed area of a triangle given the three vertices.
   *  Because the signed area is returned, the order that the vertices are given
   *  matters.
   *  \param pt0 first vertex of the triangle.
   *  \param pt1 second vertex of the triangle.
   *  \param pt2 third vertex of the triangle.
   *  \return the signed area. */
  static double ComputeTriangleArea(double pt0[3], double pt1[3], double pt2[3]);

  //@{
  /** \brief Get distance between two 3D points, very simple
   *  \return the unsigned distance */
  static double Distance(const double pt0[3], const double pt1[3]);
  static double Distance(const double pt0[], const double pt1[], const int size);
  //@}

  /** \brief Dot product between each tuple of two data arrays.
   *  \param v0 first vtk array.
   *  \param v2 second vtk array.
   *  \param numVals number of values in v0, v1, product.
   *  \param numComps number of components in v0, v1.
   *  \return product the resultant array; should be same size as v0 and v1. */
  static int VectorDotProduct(vtkDataArray *v0, vtkDataArray *v1, double product[], int numVals, int numComps);

  /** \brief Addition between each tuple of two data arrays.
   *  \param v0 first vtk array.
   *  \param v1 second vtk array.
   *  \param numVals number of values in v0, v1, product.
   *  \param numComps number of components in v0, v1.
   *  \return resultant vector array. */
  static int VectorAdd(vtkDataArray *v0, vtkDataArray *v1, double scalar, vtkDataArray *result, int numVals, int numComps);

  /** \brief Compute the binoial coefficient. */
  static double Binom(const int i, const int j);

protected:
  vtkSVMathUtils();
  ~vtkSVMathUtils();

private:
  vtkSVMathUtils(const vtkSVMathUtils&);  // Not implemented.
  void operator=(const vtkSVMathUtils&);  // Not implemented.
};

#endif  // vtkSVMathUtils_h
