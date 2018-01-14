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
 *  \class vtkSVNURBSUtils
 *  \brief A compilation of static functions to be used in a variety of
 *  NURBS applications
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVNURBSUtils_h
#define vtkSVNURBSUtils_h

#include "vtkObject.h"
#include "vtkSVNURBSModule.h"

#include "vtkDenseArray.h"
#include "vtkIntArray.h"
#include "vtkDoubleArray.h"
#include "vtkPoints.h"
#include "vtkPolyData.h"
#include "vtkStructuredGrid.h"
#include "vtkTypedArray.h"

#include <cassert> // assert() in inline implementations.

class VTKSVNURBS_EXPORT vtkSVNURBSUtils : public vtkObject
{
public:
  static vtkSVNURBSUtils *New();
  vtkTypeMacro(vtkSVNURBSUtils,vtkObject);
  void PrintSelf(ostream& os, vtkIndent indent);

  // TODO: Document functions
  static int GetUs(vtkPoints *xyz, std::string type, vtkDoubleArray *U);
  static int LinSpace(double min, double max, int num, vtkDoubleArray *U);
  static int LinSpaceClamp(double min, double max, int num, int p, vtkDoubleArray *U);
  static int GetAvgKnots(double min, double max, int num, int p, vtkDoubleArray *U,
                         vtkDoubleArray *knots);
  static int GetEndDerivKnots(double min, double max, int num, int p, vtkDoubleArray *U,
                         vtkDoubleArray *knots);
  static int GetChordSpacedUs(vtkPoints *xyz, int num, vtkDoubleArray *U);
  static int GetCentripetalSpacedUs(vtkPoints *xyz, int num, vtkDoubleArray *U);
  static int GetZeroBasisFunctions(vtkDoubleArray *U, vtkDoubleArray *knots,
                                   vtkTypedArray<double> *N0);
  static int GetPBasisFunctions(vtkDoubleArray *u, vtkDoubleArray *knots,
                                const int p,
                                vtkTypedArray<double> *N);
  static int GetControlPointsOfCurve(vtkPoints *points, vtkDoubleArray *U,
                                     vtkDoubleArray *weights, vtkDoubleArray *knots,
                                     const int p,
                                     std::string ktype,
                                     const double D0[3], const double DN[3],
                                     vtkPoints *cPoints);
  static int GetControlPointsOfSurface(vtkStructuredGrid *points, vtkDoubleArray *U,
                                       vtkDoubleArray *V, vtkDoubleArray *uWeights,
                                       vtkDoubleArray *vWeights, vtkDoubleArray *uKnots,
                                       vtkDoubleArray *vKnots, const int p, const int q,
                                       std::string kutype, std::string kvtype,
                                       vtkDoubleArray *DU0, vtkDoubleArray *DUN,
                                       vtkDoubleArray *DV0, vtkDoubleArray *DVN,
                                       vtkStructuredGrid *cPoints);
  static int SetCurveEndDerivatives(vtkTypedArray<double> *NP, vtkTypedArray<double> *points,
		                                const int p, const double D0[3], const double DN[3],
                                    vtkDoubleArray *U, vtkDoubleArray *knots,
                                    vtkTypedArray<double> *newNP, vtkTypedArray<double> *newPoints);
  static int SetSurfaceEndDerivatives(vtkTypedArray<double> *NPU, vtkTypedArray<double> *NPV,
                                      vtkTypedArray<double> *points,
		                                  const int p, const int q,
                                      std::string kutype, std::string kvtype,
                                      vtkTypedArray<double> *DU0, vtkTypedArray<double> *DUN,
                                      vtkTypedArray<double> *DV0, vtkTypedArray<double> *DVN,
                                      vtkDoubleArray *U, vtkDoubleArray *V,
                                      vtkDoubleArray *uKnots, vtkDoubleArray *vKnots,
                                      vtkTypedArray<double> *newNPU, vtkTypedArray<double> *newNPV,
                                      vtkTypedArray<double> *newPoints);
  static int AddDerivativeRows(vtkTypedArray<double> *NP, vtkTypedArray<double> *newNP,
                               const int p, vtkDoubleArray *knots);
  static int AddDerivativePoints(vtkTypedArray<double> *points,
		                             const int p, const double D0[3],
                                 const double DN[3], vtkDoubleArray *U, vtkDoubleArray *knots,
                                 vtkTypedArray<double> *newPoints);
  static int GetKnots(vtkDoubleArray *u, int p, std::string type,
                      vtkDoubleArray *knots);
  static int InvertSystem(vtkTypedArray<double> *NP, vtkTypedArray<double> *NPinv);

  static int BasisEvaluation(vtkDoubleArray *knots, int p, int kEval, double uEval,
                             vtkDoubleArray *Nu);
  static int BasisEvaluationVec(vtkDoubleArray *knots, int p, int kEval, vtkDoubleArray *uEvals,
                             vtkTypedArray<double> *Nus);
  static int FindSpan(int p, double u, vtkDoubleArray *knots, int &span);
  static int GetMultiplicity(vtkDoubleArray *array, vtkIntArray *multiplicity, vtkDoubleArray *singleValues);

  //Conversion functions
  static int PolyDatasToStructuredGrid(vtkPolyData **inputs, const int numInputs, vtkStructuredGrid *points);
  static int StructuredGridToTypedArray(vtkStructuredGrid *grid, vtkTypedArray<double> *output);
  static int TypedArrayToStructuredGrid(vtkTypedArray<double> *array, vtkStructuredGrid *output);
  static int PointsToTypedArray(vtkPoints *points, vtkTypedArray<double> *output);
  static int TypedArrayToPoints(vtkTypedArray<double> *array, vtkPoints *output);
  static int DoubleArrayToTypedArray(vtkDoubleArray *input, vtkTypedArray<double> *output);
  static int MatrixToVector(vtkTypedArray<double> *mat, double *matVec);
  static int VectorToMatrix(double *matVec, const int nr, const int nc, vtkTypedArray<double> *mat);
  static int PointMatrixToVectors(vtkTypedArray<double> *mat, double *matVecs[3]);
  static int VectorsToPointMatrix(double *matVecs[3], const int nr, const int nc, vtkTypedArray<double> *mat);
  static int DeepCopy(vtkTypedArray<double> *input, vtkTypedArray<double> *output);

  //Matrix and vector math
  static int MatrixPointsMultiply(vtkTypedArray<double> *mat, vtkPoints *pointVec, vtkPoints *output);
  static int MatrixVecMultiply(vtkTypedArray<double> *mat, const int matIsPoints,
                               vtkTypedArray<double> *vec, const int vecIsPoints,
                               vtkTypedArray<double> *output);
  static int MatrixMatrixMultiply(vtkTypedArray<double> *mat0, const int mat0IsPoints,
                                  vtkTypedArray<double> *mat1, const int mat1IsPoints,
                                  vtkTypedArray<double> *output);
  static int MatrixMatrixForDGEMM(vtkTypedArray<double> *mat0,
                                  vtkTypedArray<double> *mat1,
                                  vtkTypedArray<double> *output);
  static int PointMatrixPointMatrixForDGEMM(vtkTypedArray<double> *mat0,
                                            vtkTypedArray<double> *mat1,
                                            vtkTypedArray<double> *output);
  static int MatrixPointMatrixForDGEMM(vtkTypedArray<double> *mat0,
                                       vtkTypedArray<double> *mat1,
                                       vtkTypedArray<double> *output);
  static int PointMatrixMatrixForDGEMM(vtkTypedArray<double> *mat0,
                                       vtkTypedArray<double> *mat1,
                                       vtkTypedArray<double> *output);
  static int DGEMM(const double *A, const int nrA, const int ncA,
                   const double *B, const int nrB, const int ncB,
                   double *C);
  static int GetMatrixComp(vtkTypedArray<double> *mat,  const int loc, const int comp,
                           const int matIsPoints, vtkTypedArray<double> *vec);
  static int SetMatrixComp(vtkTypedArray<double> *vec,  const int loc, const int comp,
                           const int matIsPoints, vtkTypedArray<double> *mat);
  static int StructuredGridTranspose(vtkStructuredGrid *sg, vtkStructuredGrid *newSg);
  static int MatrixTranspose(vtkTypedArray<double> *mat, const int matIsPoints,
                             vtkTypedArray<double> *newMat);

  //Simple index-wise vector operations
  static int Add1D(vtkDoubleArray *v0, vtkDoubleArray *v1, double scalar, vtkDoubleArray *result);
  static int AddVal1D(vtkDoubleArray *v0, double val, double scalar, vtkDoubleArray *result);
  static int AddVal1D(double val, vtkDoubleArray *v0, double scalar, vtkDoubleArray *result);
  static int MultiplyVal1D(vtkDoubleArray *v0, double val, vtkDoubleArray *result);
  static int Intersect1D(vtkIntArray *v0, vtkIntArray *v1, vtkIntArray *result);
  static int WhereGreaterEqual(double val, vtkDoubleArray *in, vtkIntArray *out);
  static int WhereGreater(double val, vtkDoubleArray *in, vtkIntArray *out);
  static int WhereLessEqual(double val, vtkDoubleArray *in, vtkIntArray *out);
  static int WhereLess(double val, vtkDoubleArray *in, vtkIntArray *out);
  static int WhereEqual(double val, vtkDoubleArray *in, vtkIntArray *out);
  static int WhereNotEqual(double val, vtkDoubleArray *in, vtkIntArray *out);

  //Print operations
  static int PrintArray(vtkIntArray *arr);
  static int PrintArray(vtkDoubleArray *arr);
  static int PrintVector(vtkTypedArray<double> *vec);
  static int PrintMatrix(vtkTypedArray<double> *mat);
  static int PrintStructuredGrid(vtkStructuredGrid *mat);
  static int PrintPoints(vtkPoints *points);
  static int Print2DArray(const double *arr, const int nr, const int nc);

protected:
  vtkSVNURBSUtils();
  ~vtkSVNURBSUtils();

private:
  vtkSVNURBSUtils(const vtkSVNURBSUtils&);  // Not implemented.
  void operator=(const vtkSVNURBSUtils&);  // Not implemented.
};

#endif
