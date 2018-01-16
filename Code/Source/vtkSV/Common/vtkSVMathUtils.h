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
 *  \brief This is class of useful functions for doing specialized math operation.
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

  static int ConjugateGradient(vtkSVSparseMatrix *a,
                                const double *b, int num_iterations,
                                double *x, double epsilon);

  static void Multiply_ATA_b(vtkSVSparseMatrix *a_trans,
                             vtkSVSparseMatrix *a,
                             const double *b, double *c);

  static double InnerProduct(const double *a, const double *b, int n);

  static void Add(const double *a, const double *b, double beta, int n, double *c);

  /** \brief Compute the signed area of a triangle given the three vertices.
   *  \return the signed area. */
  static double ComputeTriangleArea(double pt0[3], double pt1[3], double pt2[3]);

  /** \brief Get distance between two 3D points, very simple
   *  \return the unsigned distance */
  static double Distance(double pt0[3], double pt1[3]);

  /** \brief Dot product between each tuple of two data arrays. */
  static int VectorDotProduct(vtkFloatArray *v0, vtkFloatArray *v1, double product[3], int numVals, int numComps);

  /** \brief Addition between each tuple of two data arrays. */
  static int VectorAdd(vtkFloatArray *v0, vtkFloatArray *v1, double scalar, vtkFloatArray *result, int numVals, int numComps);




protected:
  vtkSVMathUtils();
  ~vtkSVMathUtils();

private:
  vtkSVMathUtils(const vtkSVMathUtils&);  // Not implemented.
  void operator=(const vtkSVMathUtils&);  // Not implemented.
};

#endif  // vtkSVMathUtils_h
