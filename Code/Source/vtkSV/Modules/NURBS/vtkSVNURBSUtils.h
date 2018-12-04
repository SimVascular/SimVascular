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

#include "vtkSVNURBSModule.h"

#include "vtkDenseArray.h"
#include "vtkDoubleArray.h"
#include "vtkIntArray.h"
#include "vtkObject.h"
#include "vtkPoints.h"
#include "vtkPolyData.h"
#include "vtkStructuredGrid.h"
#include "vtkTypedArray.h"

#include "vtkSVControlGrid.h"
#include "vtkSVNURBSCollection.h"

#include <cassert> // assert() in inline implementations.

class VTKSVNURBS_EXPORT vtkSVNURBSUtils : public vtkObject
{
public:
  static vtkSVNURBSUtils *New();
  vtkTypeMacro(vtkSVNURBSUtils,vtkObject);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  /** \brief Function to get a vector of parameter values based on
   *  given type and three d points.
   *  \param xyz Whether or not the method uses the points to calculate the
   *  new U vector, points need to be given. If you do not have points, simply
   *  use the LinSpace functions.
   *  \param type Type of parameter spacing. Can be "equal", "chord", or "centripetal".
   *  \return U The output parameter values are given in U. */
  static int GetUs(vtkPoints *xyz, std::string type, vtkDoubleArray *U);

  /** \brief Computes a vector of evenly spaced values.
   *  \param min Value to start vector.
   *  \param max Value to end vector.
   *  \param num Total number of values to be in array.
   *  \return U Contains the computed evenly spaced values. */
  static int LinSpace(const double min, const double max, const int num, vtkDoubleArray *U);

  /** \brief Computes a vector of evenly spaced values with clamping at the end which is useful
   *  for creating an array of equally spaced knots of a specified degree.
   *  \param min Value to start vector. Will be repeated p+1 times.
   *  \param max Value to end vector. Will be repeated p+1 times.
   *  \param num Total number of values to be in array.
   *  \param p degree of knot span.
   *  \param U Contains the computed evenly spaced knot span. */
  static int LinSpaceClamp(const double min, const double max, const int num, const int p, vtkDoubleArray *U);

  /** \brief Computes a knot span based on an input array of parameter values. Calculated
   *  in a way to help reduce the possiblity of getting a degenerate system.
   *  \param min Value to start vector. Will be repeated p+1 times.
   *  \param max Value to end vector. Will be repeated p+1 times.
   *  \param num Total number of values to be in array.
   *  \param U Input array of parameter values to base knot span on.
   *  \return knots The resultant knot span. */
  static int GetAvgKnots(double min, double max, int num, int p, vtkDoubleArray *U,
                         vtkDoubleArray *knots);

  /** \brief Computes an average knot span (see GetAvgKnots), and then adds additional knots for end derivative constraints.
   *  in a way to help reduce the possiblity of getting a degenerate system.
   *  \param min Value to start vector. Will be repeated p+1 times.
   *  \param max Value to end vector. Will be repeated p+1 times.
   *  \param num Total number of values to be in array.
   *  \param U Input array of parameter values to base knot span on.
   *  \return knots The resultant knot span. */
  static int GetEndDerivKnots(double min, double max, int num, int p, vtkDoubleArray *U,
                         vtkDoubleArray *knots);

  /** \brief Computes an array of parameter values using a set of input points to
   *  calculate a chord length for parameter spacing.
   *  \param xyz The points used to calculate a chord length and then used
   *  to compute a parameter spacing.
   *  \param num Total number of values to be in array.
   *  \return U Input array of parameter values to base knot span on. */
  static int GetChordSpacedUs(vtkPoints *xyz, int num, vtkDoubleArray *U);

  /** \brief Computes an array of parameter values using a set of input points to
   *  calculate a chord length for parameter spacing.
   *  \param xyz The points used to calculate a chord length and then used
   *  to compute a parameter spacing.
   *  \param num Total number of values to be in array.
   *  \return U Input array of parameter values to base knot span on. */
  static int GetCentripetalSpacedUs(vtkPoints *xyz, int num, vtkDoubleArray *U);

  /** \brief Computes the values of the zero order basis functions for a given
   *  knot span and an array of input parameter values.
   *  \param U The array of input parameter values.
   *  \param knots The knots for the basis functions to be evaluated at.
   *  \return N0 Returns the zero order basis functions. */
  static int GetZeroBasisFunctions(vtkDoubleArray *U, vtkDoubleArray *knots,
                                   vtkTypedArray<double> *N0);

  /** \brief Computes the values of p degree basis functions for a given knot
   *  span and an array of input parameter values.
   *  \param U The array of input parameter values.
   *  \param knots The knots for the basis functions to be evaluated at.
   *  \return N Returns the p order basis functions. */
  static int GetPBasisFunctions(vtkDoubleArray *u, vtkDoubleArray *knots,
                                const int p,
                                vtkTypedArray<double> *N);

  // Arbitrary nurbs modification functions
  /** \brief Inserts a given knot into a nurbs object a given number of times.
   *  \param controlPoints Control points of surface.
   *  \param uKnots The knots of the surface in the u direction.
   *  \param uDegree Degree of the surface in the u direction.
   *  \param vKnots The knots of the surface in the v direction.
   *  \param vDegree Degree of the surface in the v direction.
   *  \param insertDirection The direction the knot should be inserted in.
   *  \param span The span where the knot will be inserted. Use FindSpan.
   *  \param currentMultiplicity The current multiplicity of the insertValue.
   *  \param numberOfInserts Number of times to insert the knot.
   *  \return newControlPoints The new control points after knot insertion.
   *  \return newUKnots The new knot span in the u direction.
   *  \return newVKnots The new knot span in the v direction. */
  static int InsertKnot(vtkSVControlGrid *controlPoints,
                        vtkDoubleArray *uKnots, const int uDegree,
                        vtkDoubleArray *vKnots, const int vDegree,
                        const int insertDirection,
                        const double insertValue, const int span,
                        const int currentMultiplicity,
                        const int numberOfInserts,
                        vtkSVControlGrid *newControlPoints,
                        vtkDoubleArray *newUKnots, vtkDoubleArray *newVKnots);

  /** \brief Performs knot refinement. Takes a set of given knots and inputs
   *  them into the knot span.
   *  \param controlPoints Control points of surface.
   *  \param uKnots The knots of the surface in the u direction.
   *  \param uDegree Degree of the surface in the u direction.
   *  \param vKnots The knots of the surface in the v direction.
   *  \param vDegree Degree of the surface in the v direction.
   *  \param insertKnots The array of knots to be inserted.
   *  \return newControlPoints The new control points after knot insertion.
   *  \return newUKnots The new knot span in the u direction.
   *  \return newVKnots The new knot span in the v direction. */
 static int KnotRefinement(vtkSVControlGrid *controlPoints,
                           vtkDoubleArray *uKnots, const int uDegree,
                           vtkDoubleArray *vKnots, const int vDegree,
                           const int insertDirection,
                           vtkDoubleArray *insertKnots,
                           vtkSVControlGrid *newControlPoints,
                           vtkDoubleArray *newUKnots,
                           vtkDoubleArray *newVKnots);

  /** \brief Increases the degree of a nurbs object by first performing bezier
   *  extraction of the surface knot span, then increasing the degree, and then
   *  removing unnecessary knot points.
   *  \param controlPoints Control points of curve.
   *  \param uKnots The knots of the curve in the u direction.
   *  \param uDegree Degree of the curve in the u direction.
   *  \param vKnots The knots of the curve in the u direction.
   *  \param vDegree Degree of the curve in the u direction.
   *  \param increaseDirection Direction to increase the degree of.
   *  \return newControlPoints The new control points after elevation.
   *  \return newUKnots The new knot span in the u direction.
   *  \return newVKnots The new knot span in the v direction. */
   static int IncreaseDegree(vtkSVControlGrid *controlPoints,
                             vtkDoubleArray *uKnots, const int uDegree,
                             vtkDoubleArray *vKnots, const int vDegree,
                             const int increaseDirection,
                             const int numberOfIncreases,
                             vtkSVControlGrid *newControlPoints,
                             vtkDoubleArray *newUKnots,
                             vtkDoubleArray *newVKnots);

  /** \brief Removes a given knot from a nurbs object.
   *  \param controlPoints Control points of curve.
   *  \param uKnots The knots of the surface in the u direction.
   *  \param uDegree Degree of the surface in the u direction.
   *  \param vKnots The knots of the surface in the v direction.
   *  \param vDegree Degree of the surface in the v direction.
   *  \param removeDirection Direction to remove knot from.
   *  \param removeValue The knot value to be removed.
   *  \param removeIndex The index of the knot to be removed.
   *  \param currentMultiplicity The current multiplicity of the insertValue.
   *  \param numberOfRemovals Number of times to remove the knot.
   *  \return newControlPoints The new control points after knot insertion.
   *  \return newUKnots The new knot span in the u direction.
   *  \return newVKnots The new knot span in the v direction. */
  static int RemoveKnot(vtkSVControlGrid *controlPoints,
                        vtkDoubleArray *uKnots, const int uDegree,
                        vtkDoubleArray *vKnots, const int vDegree,
                        const int removeDirection,
                        const double removeValue, const int removeIndex,
                        const int currentMultiplicity,
                        const int numberOfRemovals,
                        const double tol,
                        vtkSVControlGrid *newControlPoints,
                        vtkDoubleArray *newUKnots, vtkDoubleArray *newVKnots);

  /** \brief Decreases the degree of the NURBS object by first performing bezier
   *  extraction of the curve knot span, the decreasing the degree, and then
   *  removing unnecessary knot points.
   *  \param controlPoints Control points of curve.
   *  \param uKnots The knots of the surface in the u direction.
   *  \param uDegree Degree of the surface in the u direction.
   *  \param vKnots The knots of the surface in the v direction.
   *  \param vDegree Degree of the surface in the v direction.
   *  \param decreaseDirection Direction to decrease.
   *  \param tolerance Tol to withold during reduction.
   *  \return newControlPoints The new control points after reduction.
   *  \return newUKnots The new knot span in the u direction.
   *  \return newVKnots The new knot span in the v direction. */
   static int DecreaseDegree(vtkSVControlGrid *controlPoints,
                             vtkDoubleArray *uKnots, const int uDegree,
                             vtkDoubleArray *vKnots, const int vDegree,
                             const int decreaseDirection,
                             const double tolerance,
                             vtkSVControlGrid *newControlPoints,
                             vtkDoubleArray *newUKnots,
                             vtkDoubleArray *newVKnots);

  /** \brief Decreases the degree of a bezier NURBS by 1.
   *  \param controlPoints Control points of NURBS is control point grid (meant to be used internal to other funcs)
   *  \return maxError defines the error incurred when decreasing degree..
   *  \return newControlPoints The new control points after reduction. */
   static int BezierNURBSDecreaseDegree(vtkSVControlGrid *controlPoints,
                                        const int decreaseDirection,
                                        double &maxError,
                                        vtkSVControlGrid *newControlPoints);

  // Curve functions
  /** \brief Get the control points of a curve given the input data points
   *  an approximating array of paramter values, control point weights (equal
   *  is best), and a knot span.
   *  \param points The input data points to fit given in the correct order.
   *  \param U The array of approximating paramter values.
   *  \param weights Array of weights for the control points. Suggest equal
   *  values.
   *  \param knots The knot span to use for basis function evaluation.
   *  \param p The degree of the curve to use for fitting.
   *  \param ktype The type of knot span. Used primarily if the end derivatives
   *  need to be specified.
   *  \param D0 If ktype is derivative, the derivative vector to be used at the
   *  beginning.
   *  \param DN If ktype is derivative, the derivative vector to be used at the
   *  end.
   *  \return cPoints The control points of the fit curve. */
  static int GetControlPointsOfCurve(vtkPoints *points, vtkDoubleArray *U,
                                     vtkDoubleArray *weights, vtkDoubleArray *knots,
                                     const int p,
                                     std::string ktype,
                                     const double D0[3], const double DN[3],
                                     vtkPoints *cPoints);

  /** \brief Set the specified end derivatives in the linear system.
   *  \param NP The current matrix of basis function evaluations.
   *  \param points The input data points.
   *  \param p The degree of the curve.
   *  \param D0 The derivative vector to be specified at the beginning.
   *  \param DN The derivative vector to be specified at the end.
   *  \param U The array of approximating paramter values.
   *  \param knots The knot span to use for basis function evaluation.
   *  \return newNP The new matrix filled in with the end derivative info.
   *  \return newPoints New list of points with end derivative info added. Essentially
   *  adding the curve boundary conditions to the right hand side. */
  static int SetCurveEndDerivatives(vtkTypedArray<double> *NP, vtkTypedArray<double> *points,
		                                const int p, const double D0[3], const double DN[3],
                                    vtkDoubleArray *U, vtkDoubleArray *knots,
                                    vtkTypedArray<double> *newNP, vtkTypedArray<double> *newPoints);

  /** \brief Removes a given knot from a curve.
   *  \param controlPoints Control points of curve.
   *  \param knots The knots of the curve.
   *  \param degree Degree of the curve.
   *  \param removeValue The knot value to be removed.
   *  \param removeIndex The index of the knot to be removed.
   *  \param currentMultiplicity The current multiplicity of the insertValue.
   *  \param numberOfRemovals Number of times to remove the knot.
   *  \return newControlPoints The new control points after knot insertion.
   *  \return newKnots The new knot span. */
  static int CurveRemoveKnot(vtkSVControlGrid *controlPoints, vtkDoubleArray *knots,
                             const int degree,
                             const double removeValue, const int removeIndex,
                             const int currentMultiplicity,
                             const int numberOfRemovals,
                             const double tol,
                             vtkSVControlGrid *newControlPoints, vtkDoubleArray *newKnots);

  /** \brief Extracts the set of bezier curves from the curve.
   *  \param controlPoints Control points of curve.
   *  \param knots The knots of the curve.
   *  \param degree Degree of the curve.
   *  \return curves The collection of bezier curves. */
  static int CurveBezierExtraction(vtkSVControlGrid *controlPoints, vtkDoubleArray *knots,
                                   const int degree,
                                   vtkSVNURBSCollection *curves);

  /** \brief Decreases the degree of the curve by first performing bezier
   *  extraction of the curve knot span, the decreasing the degree, and then
   *  removing unnecessary knot points.
   *  \param controlPoints Control points of curve.
   *  \param knots The knots of the curve.
   *  \param degree Degree of the curve.
   *  \param tolerance Tol to withold during reduction.
   *  \return newControlPoints The new control points after reduction.
   *  \return newKnots The new knot span. */
   static int CurveDecreaseDegree(vtkSVControlGrid *controlPoints, vtkDoubleArray *knots,
                                  const int degree,
                                  const double tolerance,
                                  vtkSVControlGrid *newControlPoints, vtkDoubleArray *newKnots);

  /** \brief Decreases the degree of a bezier curve by 1.
   *  \param controlPoints Control points of curve as 4 comp array (meant to be used internal to other funcs)
   *  \return maxError defines the error incurred when decreasing degree..
   *  \return newControlPoints The new control points after reduction. */
   static int BezierCurveDecreaseDegree(vtkDoubleArray *controlPoints,
                                        double &maxError,
                                        vtkDoubleArray *newControlPoints);

  // Surface functions
  /** \brief Get the control points of a surface given the input data points
   *  an approximating array of paramter values, control point weights (equal
   *  is best), and a knot span.
   *  \param points The input data points to fit given in the correct order.
   *  \param U The array of approximating paramter values in the u direction.
   *  \param V The array of approximating paramter values in the v direction.
   *  \param uWeights Array of weights for the control points. Suggest equal
   *  values.
   *  \param vWeights Array of weights for the control points. Suggest equal
   *  values.
   *  \param uKnots The knot span to use for basis function evaluation in the
   *  u direction.
   *  \param vKnots The knot span to use for basis function evaluation in the
   *  v direction.
   *  \param p The degree of the surface in the u direction to use for fitting.
   *  \param q The degree of the surface in the v direction to use for fitting.
   *  \param kutype The type of knot span to be used in the u direction. Used
   *  primarily if the end derivatives need to be specified.
   *  \param kvtype The type of knot span to be used in the v direction. Used
   *  primarily if the end derivatives need to be specified.
   *  \param DU0 If kutype is derivative, the derivative vector to be used at the
   *  beginning.
   *  \param DUN If kutype is derivative, the derivative vector to be used at the
   *  end.
   *  \param DV0 If kvtype is derivative, the derivative vector to be used at the
   *  beginning.
   *  \param DVN If kvtype is derivative, the derivative vector to be used at the
   *  end.
   *  \return cPoints The control points of the fit surface. */
  static int GetControlPointsOfSurface(vtkStructuredGrid *points, vtkDoubleArray *U,
                                       vtkDoubleArray *V, vtkDoubleArray *uWeights,
                                       vtkDoubleArray *vWeights, vtkDoubleArray *uKnots,
                                       vtkDoubleArray *vKnots, const int p, const int q,
                                       std::string kutype, std::string kvtype,
                                       vtkDoubleArray *DU0, vtkDoubleArray *DUN,
                                       vtkDoubleArray *DV0, vtkDoubleArray *DVN,
                                       vtkStructuredGrid *cPoints);

  /** \brief Set the specified end derivatives in the linear system for a surface.
   *  \param NPU The current matrix of basis function evaluations in the u direction.
   *  \param NPU The current matrix of basis function evaluations in the v direction.
   *  \param points The input data points.
   *  \param p The degree of the surface in the u direction.
   *  \param q The degree of the surface in the v direction.
   *  \param kutype The type of knot span to be used in the u direction. Used
   *  primarily if the end derivatives need to be specified.
   *  \param kvtype The type of knot span to be used in the v direction. Used
   *  primarily if the end derivatives need to be specified.
   *  \param DU0 The derivative vector to be used at the beginning in the u
   *  direction.
   *  \param DUN The derivative vector to be used at the end in the u direction.
   *  \param DV0 The derivative vector to be used at the beginning in the v
   *  direction.
   *  \param DVN The derivative vector to be used at the end in the v direction.
   *  \param U The array of approximating paramter values in the u direction.
   *  \param V The array of approximating paramter values in the v direction.
   *  \param uKnots The knot span to use for basis function evaluation.
   *  \param vKnots The knot span to use for basis function evaluation.
   *  \return newNPU The new matrix filled in with the end derivative info in the
   *  U direction.
   *  \return newNPU The new matrix filled in with the end derivative info in the
   *  V direction.
   *  \return newPoints New list of points with end derivative info added. Essentially
   *  adding the surface boundary conditions to the right hand side. */
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

  /** \brief Extracts the set of bezier strips from the surface.
   *  \param uKnots The knots of the surface in the u direction.
   *  \param uDegree Degree of the surface in the u direction.
   *  \param vKnots The knots of the surface in the v direction.
   *  \param vDegree Degree of the surface in the v direction.
   *  \param extractDirection Direction to extract.
   *  \return surfaces The collection of bezier strips. */
  static int SurfaceBezierExtraction(vtkSVControlGrid *controlPoints,
                                     vtkDoubleArray *uKnots, const int uDegree,
                                     vtkDoubleArray *vKnots, const int vDegree,
                                     const int extractDirection,
                                     vtkSVNURBSCollection *surface);

  // Volume functions
  /** \brief Get the control points of a volume given the input data points
   *  an approximating array of paramter values, control point weights (equal
   *  is best), and a knot span.
   *  \param points The input data points to fit given in the correct order.
   *  \param U The array of approximating paramter values in the u direction.
   *  \param V The array of approximating paramter values in the v direction.
   *  \param W The array of approximating paramter values in the w direction.
   *  \param uWeights Array of weights for the control points. Suggest equal
   *  values.
   *  \param vWeights Array of weights for the control points. Suggest equal
   *  values.
   *  \param wWeights Array of weights for the control points. Suggest equal
   *  values.
   *  \param uKnots The knot span to use for basis function evaluation in the
   *  u direction.
   *  \param vKnots The knot span to use for basis function evaluation in the
   *  v direction.
   *  \param wKnots The knot span to use for basis function evaluation in the
   *  w direction.
   *  \param p The degree of the volume in the u direction to use for fitting.
   *  \param q The degree of the volume in the v direction to use for fitting.
   *  \param r The degree of the volume in the v direction to use for fitting.
   *  \param kutype The type of knot span to be used in the u direction. Used
   *  primarily if the end derivatives need to be specified.
   *  \param kvtype The type of knot span to be used in the v direction. Used
   *  primarily if the end derivatives need to be specified.
   *  \param kwtype The type of knot span to be used in the v direction. Used
   *  primarily if the end derivatives need to be specified.
   *  \return cPoints The control points of the fit volume. */
  static int GetControlPointsOfVolume(vtkStructuredGrid *points,
                                       vtkDoubleArray *U,
                                       vtkDoubleArray *V, vtkDoubleArray *W,
                                       vtkDoubleArray *uWeights,
                                       vtkDoubleArray *vWeights,
                                       vtkDoubleArray *wWeights,
                                       vtkDoubleArray *uKnots,
                                       vtkDoubleArray *vKnots,
                                       vtkDoubleArray *wKnots,
                                       const int p, const int q,
                                       const int r,
                                       std::string kutype,
                                       std::string kvtype,
                                       std::string kwtype,
                                       vtkStructuredGrid *DU0,
                                       vtkStructuredGrid *DUN,
                                       vtkStructuredGrid *DV0,
                                       vtkStructuredGrid *DVN,
                                       vtkStructuredGrid *DW0,
                                       vtkStructuredGrid *DWN,
                                       vtkStructuredGrid *cPoints);

  /** \brief Set the specified end derivatives in the linear system for a surface.
   *  \param NPU The current matrix of basis function evaluations in the u direction.
   *  \param NPU The current matrix of basis function evaluations in the v direction.
   *  \param NPW The current matrix of basis function evaluations in the w direction.
   *  \param points The input data points.
   *  \param p The degree of the surface in the u direction.
   *  \param q The degree of the surface in the v direction.
   *  \param r The degree of the surface in the w direction.
   *  \param kutype The type of knot span to be used in the u direction. Used
   *  primarily if the end derivatives need to be specified.
   *  \param kvtype The type of knot span to be used in the v direction. Used
   *  primarily if the end derivatives need to be specified.
   *  \param kwtype The type of knot span to be used in the w direction. Used
   *  primarily if the end derivatives need to be specified.
   *  \param DU0 The derivative grid to be used at the beginning in the u
   *  direction.
   *  \param DUN The derivative grid to be used at the end in the u direction.
   *  \param DV0 The derivative grid to be used at the beginning in the v
   *  direction.
   *  \param DWN The derivative grid to be used at the end in the v direction.
   *  \param DW0 The derivative grid to be used at the beginning in the w
   *  direction.
   *  \param DVN The derivative grid to be used at the end in the w direction.
   *  \param U The array of approximating paramter values in the u direction.
   *  \param V The array of approximating paramter values in the v direction.
   *  \param W The array of approximating paramter values in the w direction.
   *  \param uKnots The knot span to use for basis function evaluation.
   *  \param vKnots The knot span to use for basis function evaluation.
   *  \param wKnots The knot span to use for basis function evaluation.
   *  \return newNPU The new matrix filled in with the end derivative info in the
   *  U direction.
   *  \return newNPU The new matrix filled in with the end derivative info in the
   *  V direction.
   *  \return newNPW The new matrix filled in with the end derivative info in the
   *  W direction.
   *  \return newPoints New list of points with end derivative info added. Essentially
   *  adding the surface boundary conditions to the right hand side. */
  static int SetVolumeEndDerivatives(vtkTypedArray<double> *NPU,
                                     vtkTypedArray<double> *NPV,
                                     vtkTypedArray<double> *NPW,
                                     vtkTypedArray<double> *points,
		                                 const int p, const int q, const int r,
                                     std::string kutype, std::string kvtype,
                                     std::string kwtype,
                                     vtkTypedArray<double> *DU0,
                                     vtkTypedArray<double> *DUN,
                                     vtkTypedArray<double> *DV0,
                                     vtkTypedArray<double> *DVN,
                                     vtkTypedArray<double> *DW0,
                                     vtkTypedArray<double> *DWN,
                                     vtkDoubleArray *U,
                                     vtkDoubleArray *V,
                                     vtkDoubleArray *W,
                                     vtkDoubleArray *uKnots,
                                     vtkDoubleArray *vKnots,
                                     vtkDoubleArray *wKnots,
                                     vtkTypedArray<double> *newNPU,
                                     vtkTypedArray<double> *newNPV,
                                     vtkTypedArray<double> *newNPW,
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
  static int FindSpan(const int p, const double u, vtkDoubleArray *knots, int &span);
  static int FindKnotMultiplicity(const int knotIndex, const double u, vtkDoubleArray *knots, int &mult);
  static int GetMultiplicity(vtkDoubleArray *array, vtkIntArray *multiplicity, vtkDoubleArray *singleValues);
  static int GetPWFromP(vtkSVControlGrid *controlPoints);
  static int GetPWFromP(vtkSVControlGrid *controlPoints, vtkSVControlGrid *controlPointWeights);
  static int GetPFromPW(vtkSVControlGrid *controlPoints);
  static int GetPFromPW(vtkSVControlGrid *controlPointWeights, vtkSVControlGrid *controlPoints);

  //Conversion functions
  static int PolyDatasToStructuredGrid(vtkPolyData **inputs, const int numInputs, vtkStructuredGrid *points);
  static int StructuredGridToTypedArray(vtkStructuredGrid *grid, vtkTypedArray<double> *output);
  static int ControlGridToTypedArraySPECIAL(vtkSVControlGrid *grid, vtkTypedArray<double> *output);
  static int ControlGridToTypedArraySPECIAL(vtkSVControlGrid *grid, const int dim0, const int dim1, const int dim2, const int comp2, vtkTypedArray<double> *output);
  static int TypedArrayToStructuredGrid(vtkTypedArray<double> *array, vtkStructuredGrid *output);
  static int TypedArrayToStructuredGridRational(vtkTypedArray<double> *array, vtkStructuredGrid *output);
  static int PointsToTypedArray(vtkPoints *points, vtkTypedArray<double> *output);
  static int TypedArrayToPoints(vtkTypedArray<double> *array, vtkPoints *output);
  static int DoubleArrayToTypedArray(vtkDoubleArray *input, vtkTypedArray<double> *output);
  static int MatrixToVector(vtkTypedArray<double> *mat, double *matVec);
  static int VectorToMatrix(double *matVec, const int nr, const int nc, vtkTypedArray<double> *mat);
  static int PointMatrixToVectors(vtkTypedArray<double> *mat, double **matVecs);
  static int VectorsToPointMatrix(double **matVecs, const int nr, const int nc, const int np,  vtkTypedArray<double> *mat);
  static int GetMatrixOfDim4Grid(vtkTypedArray<double> *grid, const int dim0, const int dim1, const int dim2, const int comp2, const int num3, vtkTypedArray<double> *matrix);
  static int SetMatrixOfDim4Grid(vtkTypedArray<double> *matrix, vtkTypedArray<double> *grid, const int dim0, const int dim1, const int dim2, const int comp2, const int num3);
  static int DeepCopy(vtkTypedArray<double> *input, vtkTypedArray<double> *output);

  //Matrix and vector math
  static int MatrixPointsMultiply(vtkTypedArray<double> *mat, vtkPoints *pointVec, vtkPoints *output);
  static int MatrixVecMultiply(vtkTypedArray<double> *mat, const int matIsPoints,
                               vtkTypedArray<double> *vec, const int vecIsPoints,
                               vtkTypedArray<double> *output);
  static int MatrixMatrixMultiply(vtkTypedArray<double> *mat0, const int mat0IsPoints, const int point0Dims,
                                  vtkTypedArray<double> *mat1, const int mat1IsPoints, const int point1Dims,
                                  vtkTypedArray<double> *output);
  static int MatrixMatrixForDGEMM(vtkTypedArray<double> *mat0,
                                  vtkTypedArray<double> *mat1,
                                  vtkTypedArray<double> *output);
  static int PointMatrixPointMatrixForDGEMM(vtkTypedArray<double> *mat0,
                                            vtkTypedArray<double> *mat1,
                                            const int pointDims,
                                            vtkTypedArray<double> *output);
  static int MatrixPointMatrixForDGEMM(vtkTypedArray<double> *mat0,
                                       vtkTypedArray<double> *mat1,
                                       const int pointDims,
                                       vtkTypedArray<double> *output);
  static int PointMatrixMatrixForDGEMM(vtkTypedArray<double> *mat0,
                                       vtkTypedArray<double> *mat1,
                                       const int pointDims,
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
  static int PrintArray(vtkDataArray *arr);
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
