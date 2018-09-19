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
 *  \class vtkSVNURBSCurve
 *  \brief This is a class to represent a NURBS curve
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVNURBSCurve_h
#define vtkSVNURBSCurve_h

#include "vtkSVNURBSObject.h"
#include "vtkSVNURBSModule.h"

#include "vtkCellArray.h"
#include "vtkDenseArray.h"
#include "vtkDoubleArray.h"
#include "vtkIntArray.h"
#include "vtkPolyData.h"
#include "vtkSVControlGrid.h"

class vtkSVNURBSCollection;

class VTKSVNURBS_EXPORT vtkSVNURBSCurve : public vtkSVNURBSObject
{
public:
  static vtkSVNURBSCurve *New();

  // Constructor
  vtkSVNURBSCurve(int m, vtkPoints *controlPoints, int n, vtkDoubleArray *knotPoints, int deg) {;}
  vtkSVNURBSCurve(int m, vtkPoints *controlPoints, vtkDoubleArray *knotPoints, vtkIntArray *knotMultiplicity, int deg) {;}

  vtkTypeMacro(vtkSVNURBSCurve,vtkSVNURBSObject);
  void PrintSelf(ostream& os, vtkIndent indent) override;

  //@{
  /// \brief Get and set the number of control points for curve
  vtkGetMacro(NumberOfControlPoints, int);
  vtkSetMacro(NumberOfControlPoints, int);
  //@}

  //@{
  /// \brief Get and set the number of knot points for curve
  vtkGetMacro(NumberOfKnotPoints, int);
  vtkSetMacro(NumberOfKnotPoints, int);
  //@}

  //@{
  /// \brief Get and set the degree
  vtkGetMacro(Degree, int);
  vtkSetMacro(Degree, int);
  //@}

  //@{
  /// \brief Get and set the control point grid
  vtkGetObjectMacro(ControlPointGrid, vtkSVControlGrid);
  //@}

  //@{
  /// \brief Get and set the knot vector object
  vtkGetObjectMacro(KnotVector, vtkDoubleArray);
  //@}

  //@{
  // \brief Get and set the PolyData Representation
  vtkGetObjectMacro(CurveRepresentation, vtkPolyData);
  vtkSetObjectMacro(CurveRepresentation, vtkPolyData);
  //@}

  // Initialize
  void Initialize() override;

  //PolyData representation functions
  /** \brief Function to generate polydata representation of nurbs curve. Stored
   *  in CurveRepresentation.
   *  \param spacing Sets the spacing to sample the NURBS at. */
  int GeneratePolyDataRepresentation(const double spacing);

  /** \brief Get structured grid connectivity.
   *  \param connectivity empty cell array to be filled with a structured grid connectivity. */
  int GetStructuredGridConnectivity(const int numPoints, vtkCellArray *connectivity);

  /** \brief Set the control points using a point set. */
  void SetControlPoints(vtkPoints *points1d);

  //Functions to manipulate the geometry

  /** \brief Simply updates the number of control points, number of knot points
   *  and the degree based on the vectors. Shouldn't really need to be called. */
  void UpdateCurve();

  /** \brief Increase the degree of the curve a specified number of times. */
  int IncreaseDegree(const int numberOfIncreases);

  /** \brief Increase the degree of the curve with specified tolerance. */
  int DecreaseDegree(const double tolerance);

  /** \brief single knot value insertion. Does not replace a knot;
   *  for that, use SetKnot; this creates an entirely new knot point in the vector. */
  int InsertKnot(const double newKnot, const int numberOfInserts);

  /** \brief insert multiple knots at the same time; should be an increasing knot
   *  span that is within the bound of the current knots. Make sure this is
   *  done as this is not checked. */
  int InsertKnots(vtkDoubleArray *newKnots);

  /** \brief remove single knot from knot span of specified value. Value must match knots exactly. */
  int RemoveKnot(const double removeKnot, const int numberOfRemovals, const double tol);

  /** \brief remove single knot at a specified location in the knot span. */
  int RemoveKnotAtIndex(const int knotIndex, const int numberOfRemovals, const double tol);

  /** \brief Set the entire knot vector. Must make sure everything is consistent, curve does not check */
  int SetKnotVector(vtkDoubleArray *knots);

  //@{
  /** \brief Function to set/overwrite the knot/s value at the provided index/indices. Index
   *  must be less than span length. */
  int SetKnot(const int index, const double newKnot);
  int SetKnots(vtkIntArray *indices, vtkDoubleArray *newKnots);
  //@}

  //@{
  /** \brief Function to get knot/s at the provided index/indices. */
  int GetKnot(const int index, double &knotVal);
  int GetKnots(vtkIntArray *indices, vtkDoubleArray *knotVals);
  //@}

  /** \brief Set the entire control point grid. Must make sure everything is consistent. Input is not checked. */
  int SetControlPointGrid(vtkSVControlGrid *controlPoints);

  //@{
  /** \brief Function to set control point/s at the provided index/indices. */
  int SetControlPoint(const int index, const double coordinates[3], const double weight);
  int SetControlPoint(const int index, const double pw[4]);
  //@}

  //@{
  /** \brief Get a single control point and its corresponding weight. */
  int GetControlPoint(const int index, double coordinates[3], double &weight);
  int GetControlPoint(const int index, double pw[4]);
  //@}

  //@{
  /** \brief Functions to set/get the weight vectors of the control points. Length
   *  of weights must match the number of control points. */
  int SetWeights(vtkDoubleArray *weights);
  int GetWeights(vtkDoubleArray *weights);
  //}

  //@{
  /** \brief Functions to get and set individual weights in the weight vector. */
  int SetWeight(const int index, const double weight);
  int GetWeight(const int index, double &weight);
  //@}

  /** \brief set NURBS to closed, loops around. */
  void SetClosed(const int closed) {this->Closed = closed;}

  /** \brief set NURBS to clamped, default on. */
  void SetClamped(const int clamped) {this->Clamped = clamped;}

  int MakePeriodic(const int continuity) {return 0;} /**< \brief Unimplemented */

  /** \brief get the knot vector multiplicity. */
  int GetMultiplicity(vtkIntArray *multiplicity, vtkDoubleArray *singleKnots);

  /** \brief decompose the surface using bezier extraction. */
  int ExtractBezierCurves(vtkSVNURBSCollection *curves);

  // Description:
  // Retrieve an instance of this class from an information object.
  static vtkSVNURBSCurve* GetData(vtkInformation* info);
  static vtkSVNURBSCurve* GetData(vtkInformationVector* v, int i=0);

  virtual void DeepCopy(vtkSVNURBSCurve *src);

  virtual std::string GetType() override {return "Curve";}

protected:
  vtkSVNURBSCurve();
  ~vtkSVNURBSCurve();

  int NumberOfControlPoints;
  int NumberOfKnotPoints;
  int Clamped; // Default on and only things supported currently
  int Closed;
  int Degree;

  vtkSVControlGrid *ControlPointGrid;
  vtkDoubleArray *KnotVector;

  vtkPolyData *CurveRepresentation;

private:
  vtkSVNURBSCurve(const vtkSVNURBSCurve&);  // Not implemented.
  void operator=(const vtkSVNURBSCurve&);  // Not implemented.
};

#endif
