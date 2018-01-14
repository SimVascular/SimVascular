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
 *  \class vtkSVNURBSSurface
 *  \brief This is a class to represent a NURBS surface
 *
 *  \author Adam Updegrove
 *  \author updega2@gmail.com
 *  \author UC Berkeley
 *  \author shaddenlab.berkeley.edu
 */

#ifndef vtkSVNURBSSurface_h
#define vtkSVNURBSSurface_h

#include "vtkDataObject.h"
#include "vtkSVNURBSModule.h"

#include "vtkSVControlGrid.h"
#include "vtkDenseArray.h"
#include "vtkDoubleArray.h"
#include "vtkIntArray.h"
#include "vtkPolyData.h"

class VTKSVNURBS_EXPORT vtkSVNURBSSurface : public vtkDataObject
{
public:
  static vtkSVNURBSSurface *New();

  // Constructor
  vtkSVNURBSSurface(int m, vtkPoints *controlPoints, int n, vtkDoubleArray *knotPoints, int deg) {;}
  vtkSVNURBSSurface(int m, vtkPoints *controlPoints, vtkDoubleArray *knotPoints, vtkIntArray *knotMultiplicity, int deg) {;}

  vtkTypeMacro(vtkSVNURBSSurface,vtkDataObject);
  void PrintSelf(ostream& os, vtkIndent indent);

  //@{
  /// \brief Get and set the number of control points for curve
  vtkGetMacro(NumberOfUControlPoints, int);
  vtkSetMacro(NumberOfUControlPoints, int);
  vtkGetMacro(NumberOfVControlPoints, int);
  vtkSetMacro(NumberOfVControlPoints, int);
  //@}


  //@{
  /// \brief Get and set the number of knot points for curve
  vtkGetMacro(NumberOfUKnotPoints, int);
  vtkSetMacro(NumberOfUKnotPoints, int);
  vtkGetMacro(NumberOfVKnotPoints, int);
  vtkSetMacro(NumberOfVKnotPoints, int);
  //@}
  //
  //@{
  /// \brief Get and set the degree
  vtkGetMacro(UDegree, int);
  vtkSetMacro(UDegree, int);
  vtkGetMacro(VDegree, int);
  vtkSetMacro(VDegree, int);
  //@}

  //@{
  // / \brief Set the control point grid
  vtkGetObjectMacro(ControlPointGrid, vtkSVControlGrid);
  vtkSetObjectMacro(ControlPointGrid, vtkSVControlGrid);
  //@}

  //@{
  // / \brief Get and set the knot vector object
  vtkGetObjectMacro(UKnotVector, vtkDoubleArray);
  vtkSetObjectMacro(UKnotVector, vtkDoubleArray);
  vtkGetObjectMacro(VKnotVector, vtkDoubleArray);
  vtkSetObjectMacro(VKnotVector, vtkDoubleArray);
  //@}

  //@{
  // / \brief Get and set the weights
  vtkGetObjectMacro(UWeights, vtkDoubleArray);
  vtkSetObjectMacro(UWeights, vtkDoubleArray);
  vtkGetObjectMacro(VWeights, vtkDoubleArray);
  vtkSetObjectMacro(VWeights, vtkDoubleArray);
  //@}

  //@{
  // / \brief Get the PolyData Representation
  vtkGetObjectMacro(SurfaceRepresentation, vtkPolyData);
  vtkSetObjectMacro(SurfaceRepresentation, vtkPolyData);
  //@}

  // Initialize
  void Initialize();

  //PolyData representation functions
  /** \brief Function to generate polydata representation of nurbs surface. Stored
   *  in SurfaceRepresentation.
   *  \param uSpacing Sets the spacing to sample the NURBS at in the u parameter direction.
   *  \param vSpacing Sets the spacing to sample the NURBS at in the v parameter direction. */
  int GeneratePolyDataRepresentation(const double uSpacing, const double vSpacing);

  //Functions to set control points/knots/etc.
  void SetControlPoints(vtkStructuredGrid *points2d);
  void SetKnotVector(vtkDoubleArray *knotVector, const int dim);

  //Functions to manipulate the geometry
  void UpdateCurve() {} /**< \brief Unimplemented */
  int IncreaseDegree(const int degree, const int dim) {return 0;} /**< \brief Unimplemented */
  int InsertKnot(const double newKnot, const int dim, const double tolerance) {return 0;} /**< \brief Unimplemented */
  int InsertKnots(vtkDoubleArray *newKnots, const int dim, const double tolerance) {return 0;} /**< \brief Unimplemented */
  int RemoveKnot(const int index, const int dim, const double tolerance) {return 0;} /**< \brief Unimplemented */
  int SetKnot(const int index, const int dim, const double newKnot) {return 0;} /**< \brief Unimplemented */
  int SetKnots(vtkIntArray *indices, const int dim, vtkDoubleArray *newKnots) {return 0;} /**< \brief Unimplemented */
  int GetKnot(const int index, const int dim, double &knotVal) {return 0;} /**< \brief Unimplemented */
  int GetKnots(const int indices, const int dim, vtkDoubleArray *knotVals) {return 0;} /**< \brief Unimplemented */

  int SetControlPoint(const int index, const int dim, const double coordinate[3], const double weight) {return 0;} /**< \brief Unimplemented */
  int SetControlPoints(vtkIntArray *indices, const int dim, vtkPoints *coordinates, vtkDoubleArray *weights); /**< \brief Unimplemented */
  int GetControlPoint(const int index, const int dim, double coordinates[3], double &weight) {return 0;} /**< \brief Unimplemented */
  int GetControlPoints(vtkIntArray *indices, const int dim, vtkPoints *coordinates, vtkDoubleArray *weights) {return 0;} /**< \brief Unimplemented */

  int SetWeight(const int index, const int dim, const double weight) {return 0;} /**< \brief Unimplemented */
  int GetWeight(const int index, const int dim, double &weight) {return 0;} /**< \brief Unimplemented */

  void SetClosed(const int closed, const int dim) {;} /**< \brief Unimplemented */
  void SetClamped(const int clamped, const int dim) {;} /**< \brief Unimplemented */
  int MakePeriodic(const int continuity, const int dim) {return 0;} /**< \brief Unimplemented */

  /** \brief get the knot vector multiplicity. */
  int GetUMultiplicity(vtkIntArray *multiplicity, vtkDoubleArray *singleKnots);
  int GetVMultiplicity(vtkIntArray *multiplicity, vtkDoubleArray *singleKnots);
  int GetMultiplicity(const int dim, vtkIntArray *multiplicity, vtkDoubleArray *singleKnots);

  /** \brief Get structured grid connectivity.
   *  \param connectivity empty cell array to be filled with a structured grid connectivity. */
  int GetStructuredGridConnectivity(const int numXPoints, const int numYPoints, vtkCellArray *connectivity);

  // Description:
  // Retrieve an instance of this class from an information object.
  static vtkSVNURBSSurface* GetData(vtkInformation* info);
  static vtkSVNURBSSurface* GetData(vtkInformationVector* v, int i=0);

  virtual void DeepCopy(vtkSVNURBSSurface *src);

protected:
  vtkSVNURBSSurface();
  ~vtkSVNURBSSurface();

  int NumberOfUControlPoints;
  int NumberOfVControlPoints;
  int NumberOfUKnotPoints;
  int NumberOfVKnotPoints;
  int UDegree;
  int VDegree;
  int UClamped; // Default is clamped and only supported way currently
  int VClamped; // Default is clamped and only supported way currently
  int UClosed;
  int VClosed;

  vtkSVControlGrid *ControlPointGrid;
  vtkDoubleArray *UKnotVector;
  vtkDoubleArray *VKnotVector;
  vtkDoubleArray *UVKnotVectors[2];
  vtkDoubleArray *UWeights;
  vtkDoubleArray *VWeights;
  vtkDoubleArray *UVWeights[2];

  vtkPolyData *SurfaceRepresentation;

private:
  vtkSVNURBSSurface(const vtkSVNURBSSurface&);  // Not implemented.
  void operator=(const vtkSVNURBSSurface&);  // Not implemented.
};

#endif
