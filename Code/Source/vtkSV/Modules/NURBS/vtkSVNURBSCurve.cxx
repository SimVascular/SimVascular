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

#include "vtkSVNURBSCurve.h"

#include "vtkCleanPolyData.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkSmartPointer.h"
#include "vtkSparseArray.h"

#include "vtkSVGlobals.h"
#include "vtkSVNURBSUtils.h"

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVNURBSCurve);

// ----------------------
// Constructor
// ----------------------
vtkSVNURBSCurve::vtkSVNURBSCurve()
{
  this->NumberOfControlPoints = 0;
  this->NumberOfKnotPoints    = 0;
  this->Degree                = 0;
  this->Clamped               = 1;
  this->Closed                = 0;

  this->ControlPointGrid   = vtkSVControlGrid::New();
  this->KnotVector         = vtkDoubleArray::New();

  this->CurveRepresentation = vtkPolyData::New();
}

// ----------------------
// Destructor
// ----------------------
vtkSVNURBSCurve::~vtkSVNURBSCurve()
{
  if (this->ControlPointGrid != NULL)
  {
    this->ControlPointGrid->Delete();
  }
  if (this->KnotVector != NULL)
  {
    this->KnotVector->Delete();
  }

  if (this->CurveRepresentation != NULL)
  {
    this->CurveRepresentation->Delete();
  }
}

// ----------------------
// DeepCopy
// ----------------------
void vtkSVNURBSCurve::DeepCopy(vtkSVNURBSCurve *src)
{
  this->Superclass::DeepCopy(src);

  this->SetNumberOfControlPoints(src->GetNumberOfControlPoints());
  this->SetNumberOfKnotPoints(src->GetNumberOfKnotPoints());
  this->SetDegree(src->GetDegree());

  this->ControlPointGrid->DeepCopy(src->GetControlPointGrid());
  this->KnotVector->DeepCopy(src->GetKnotVector());

  this->CurveRepresentation->DeepCopy(src->GetCurveRepresentation());
}


// ----------------------
// PrintSelf
// ----------------------
void vtkSVNURBSCurve::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "Number of control points: " << this->NumberOfControlPoints << "\n";
  os << indent << "Number of knot points: " << this->NumberOfKnotPoints << "\n";
  os << indent << "Degree: " << this->Degree << "\n";
  os << indent << "Clamped: " << this->Clamped << "\n";
  os << indent << "Closed: " << this->Closed << "\n";
}

// ----------------------
// Initialize
// ----------------------
void vtkSVNURBSCurve::Initialize()
{
  this->Superclass::Initialize();
}

// ----------------------
// GetData
// ----------------------
vtkSVNURBSCurve* vtkSVNURBSCurve::GetData(vtkInformation* info)
{
  return info? vtkSVNURBSCurve::SafeDownCast(info->Get(DATA_OBJECT())) : 0;
}

// ----------------------
// GetData
// ----------------------
vtkSVNURBSCurve* vtkSVNURBSCurve::GetData(vtkInformationVector* v, int i)
{
  return vtkSVNURBSCurve::GetData(v->GetInformationObject(i));
}

// ----------------------
// SetControlPoints
// ----------------------
void vtkSVNURBSCurve::SetControlPoints(vtkPoints *points1d)
{
  // Get number of points
  int nCon = points1d->GetNumberOfPoints();

  // Set the dimensions of control point grid
  this->ControlPointGrid->SetDimensions(nCon, 1, 1);

  // Set the points
  this->ControlPointGrid->GetPoints()->SetNumberOfPoints(nCon);
  this->ControlPointGrid->SetPoints(points1d);

  // Loop through points and set weight
  vtkNew(vtkDoubleArray, weights);
  weights->SetNumberOfTuples(nCon);
  for (int i=0; i<nCon; i++)
    weights->SetTuple1(i, 1.0);
  weights->SetName("Weights");

  // Set the array on the grid
  this->ControlPointGrid->GetPointData()->AddArray(weights);

  // Update number of control points
  this->NumberOfControlPoints = nCon;
}

// ----------------------
// UpdateCurve
// ----------------------
void vtkSVNURBSCurve::UpdateCurve()
{
  this->NumberOfControlPoints = this->ControlPointGrid->GetPoints()->GetNumberOfPoints();
  this->NumberOfKnotPoints    = this->KnotVector->GetNumberOfTuples();
  this->Degree = this->NumberOfKnotPoints - this->NumberOfControlPoints - 1;
}

// ----------------------
// IncreaseDegree
// ----------------------
int vtkSVNURBSCurve::IncreaseDegree(const int numberOfIncreases)
{
  // Get current info about curve degree
  int curp = this->Degree;

  // Set up new knots and points
  vtkNew(vtkDoubleArray, newKnots);
  vtkNew(vtkSVControlGrid, newControlPoints);

  // Increase the degree
  vtkDoubleArray *vKnots = NULL; // just one direction for curve
  vtkDoubleArray *newVKnots = NULL; // just one direction for curve
  int q = 0; // just one direction for curve
  int dir = 0; // just one directio for curve
  if (vtkSVNURBSUtils::IncreaseDegree(this->ControlPointGrid,
                                      this->KnotVector, curp,
                                      vKnots, q, dir,
                                      numberOfIncreases,
                                      newControlPoints,
                                      newKnots, newVKnots) != SV_OK)
  {
    vtkErrorMacro("Error in raising the curve degree");
    return SV_ERROR;
  }
  if (newControlPoints->GetNumberOfPoints()+curp+numberOfIncreases+1 != newKnots->GetNumberOfTuples())
  {
    vtkErrorMacro("Error in setting the correct control points and knots during curve degree elevation");
    return SV_ERROR;
  }

  // Replace existing data with new data
  this->SetControlPointGrid(newControlPoints);
  this->SetKnotVector(newKnots);
  this->Degree = curp+numberOfIncreases;

  return SV_OK;
}

// ----------------------
// DecreaseDegree
// ----------------------
int vtkSVNURBSCurve::DecreaseDegree(const double tolerance)
{
  // Get current info about curve degree
  int curp = this->Degree;

  // Set up new knots and points
  vtkNew(vtkDoubleArray, newKnots);
  vtkNew(vtkSVControlGrid, newControlPoints);

  // Decrease the degree
  if (vtkSVNURBSUtils::CurveDecreaseDegree(this->ControlPointGrid, this->KnotVector,
                                           curp, tolerance,
                                           newControlPoints, newKnots) != SV_OK)
  {
    vtkErrorMacro("Error in lowering the curve degree");
    return SV_ERROR;
  }
  if (newControlPoints->GetNumberOfPoints()+curp != newKnots->GetNumberOfTuples())
  {
    vtkErrorMacro("Error in setting the correct control points and knots during curve degree reduction");
    return SV_ERROR;
  }

  // Replace existing data with new data
  this->SetControlPointGrid(newControlPoints);
  this->SetKnotVector(newKnots);
  this->Degree = curp-1;

  return SV_ERROR;
}

// ----------------------
// InsertKnot
// ----------------------
int vtkSVNURBSCurve::InsertKnot(const double newKnot, const int numberOfInserts)
{
  // Get current info about curve degree
  int p = this->Degree;

  // Get the location of the knot
  int span=0;
  vtkSVNURBSUtils::FindSpan(p, newKnot, this->KnotVector, span);

  // If the value at the location is our value, we need to get current mult
  int mult=0;
  if(this->KnotVector->GetTuple1(span) == newKnot)
    vtkSVNURBSUtils::FindKnotMultiplicity(span, newKnot, this->KnotVector, mult);

  // Set up new knots and points
  vtkNew(vtkDoubleArray, newKnots);
  vtkNew(vtkSVControlGrid, newControlPoints);

  // Insert the knot
  vtkDoubleArray *vKnots = NULL; // just one direction for curve
  vtkDoubleArray *newVKnots = NULL; // just one direction for curve
  int q = 0; // just one direction for curve
  int dir = 0; // just one directio for curve
  if (vtkSVNURBSUtils::InsertKnot(this->ControlPointGrid,
                                  this->KnotVector, p,
                                  vKnots, q, dir,
                                  newKnot, span,
                                  mult, numberOfInserts,
                                  newControlPoints, newKnots, newVKnots) != SV_OK)
  {
    vtkErrorMacro("Error on knot insertion");
    return SV_ERROR;
  }

  // Replace existing data with new data
  this->SetControlPointGrid(newControlPoints);
  this->SetKnotVector(newKnots);

  return SV_OK;

}

// ----------------------
// InsertKnots
// ----------------------
int vtkSVNURBSCurve::InsertKnots(vtkDoubleArray *newKnots)
{
  // Get current degree
  int p = this->Degree;

  // Set up new knots and new control points
  vtkNew(vtkDoubleArray, newKnotSpan);
  vtkNew(vtkSVControlGrid, newControlPoints);

  vtkDoubleArray *vKnots = NULL; // just one direction for curve
  vtkDoubleArray *newVKnots = NULL; // just one direction for curve
  int q = 0; // just one direction for curve
  int dir = 0; // just one directio for curve
  if (vtkSVNURBSUtils::KnotRefinement(this->ControlPointGrid,
                                      this->KnotVector, p,
                                      vKnots, q,
                                      dir, newKnots,
                                      newControlPoints,
                                      newKnotSpan, newVKnots) != SV_OK)
  {
    vtkErrorMacro("Error in knot refinement");
    return SV_ERROR;
  }

  // Replace existing data with new data
  this->SetControlPointGrid(newControlPoints);
  this->SetKnotVector(newKnotSpan);

  return SV_OK;
}

// ----------------------
// RemoveKnot
// ----------------------
int vtkSVNURBSCurve::RemoveKnot(const double removeKnot, const int numberOfRemovals, const double tol)
{
  int p = this->Degree;
  int knotIndex = -1;
  for (int i=0; i<this->KnotVector->GetNumberOfTuples(); i++)
  {
    if (this->KnotVector->GetTuple1(i) == removeKnot)
    {
      knotIndex = i;
      break;
    }
  }

  if (knotIndex == -1)
  {
    vtkErrorMacro("Knot value was not found in span");
    return SV_ERROR;
  }

  if (this->RemoveKnotAtIndex(knotIndex, numberOfRemovals, tol) != SV_OK)
    return SV_ERROR;

  return SV_OK;
}

// ----------------------
// RemoveKnot
// ----------------------
int vtkSVNURBSCurve::RemoveKnotAtIndex(const int knotIndex, const int numberOfRemovals, const double tol)
{
  int p = this->Degree;
  double removeKnot = this->KnotVector->GetTuple1(knotIndex);
  int mult;
  if (vtkSVNURBSUtils::FindKnotMultiplicity(knotIndex, removeKnot, this->KnotVector, mult) != SV_OK)
  {
    vtkErrorMacro("Error in getting multiplicity");
    return SV_ERROR;
  }

  // Set up new knots and points
  vtkNew(vtkDoubleArray, newKnots);
  vtkNew(vtkSVControlGrid, newControlPoints);

  // Remove the knot
  vtkDoubleArray *vKnots = NULL; // just one direction for curve
  vtkDoubleArray *newVKnots = NULL; // just one direction for curve
  int q = 0; // just one direction for curve
  int dir = 0; // just one directio for curve
  if (vtkSVNURBSUtils::RemoveKnot(this->ControlPointGrid,
                                  this->KnotVector, p,
                                  vKnots, q,
                                  dir, removeKnot, knotIndex,
                                  mult, numberOfRemovals,
                                  tol,
                                  newControlPoints,
                                  newKnots, newVKnots) != SV_OK)
  {
    vtkErrorMacro("Error on knot insertion");
    return SV_ERROR;
  }

  // Replace existing data with new data
  this->SetControlPointGrid(newControlPoints);
  this->SetKnotVector(newKnots);

  return SV_OK;
}

// ----------------------
// SetKnotVector
// ----------------------
int vtkSVNURBSCurve::SetKnotVector(vtkDoubleArray *knots)
{
  this->KnotVector->DeepCopy(knots);

  this->NumberOfKnotPoints = this->KnotVector->GetNumberOfTuples();
  return SV_OK;
}

// ----------------------
// SetKnot
// ----------------------
int vtkSVNURBSCurve::SetKnot(const int index, const double newKnot)
{
  if (index >= this->NumberOfKnotPoints)
  {
    vtkErrorMacro("Index " << index << " outside length of knot vector: " << this->NumberOfKnotPoints);
    return SV_ERROR;
  }
  if (index > 0)
  {
    if (this->KnotVector->GetTuple1(index-1) > newKnot)
    {
      vtkErrorMacro("Invalid knot inserted; " << newKnot <<
      " must be greater than or equal to previous knot: " << this->KnotVector->GetTuple1(index-1));
      return SV_ERROR;
    }
  }
  this->KnotVector->SetTuple1(index, newKnot);
  return SV_OK;
}

// ----------------------
// SetKnots
// ----------------------
int vtkSVNURBSCurve::SetKnots(vtkIntArray *indices, vtkDoubleArray *newKnots)
{
  int numNewKnots = indices->GetNumberOfTuples();
  for (int i=0; i<numNewKnots; i++)
  {
    if (this->SetKnot(indices->GetTuple1(i), newKnots->GetTuple1(i)) != SV_OK)
      return SV_ERROR;
  }
  return SV_OK;
}

// ----------------------
// GetKnot
// ----------------------
int vtkSVNURBSCurve::GetKnot(const int index, double &knotVal)
{
  if (index >= this->NumberOfKnotPoints)
  {
    vtkErrorMacro("Index " << index << " outside length of knot vector: " << this->NumberOfKnotPoints);
    return SV_ERROR;
  }
  knotVal = this->KnotVector->GetTuple1(index);
  return SV_OK;
}

// ----------------------
// GetKnots
// ----------------------
int vtkSVNURBSCurve::GetKnots(vtkIntArray *indices, vtkDoubleArray *knotVals)
{
  int numKnotVals = indices->GetNumberOfTuples();
  for (int i=0; i<numKnotVals; i++)
  {
    double returnVal;
    if (this->GetKnot(indices->GetTuple1(i), returnVal) != SV_OK)
      return SV_ERROR;
    knotVals->InsertTuple1(i, returnVal);
  }
  return SV_OK;
}

// ----------------------
// SetControlPointGrid
// ----------------------
int vtkSVNURBSCurve::SetControlPointGrid(vtkSVControlGrid *controlPoints)
{
  this->ControlPointGrid->DeepCopy(controlPoints);

  int dims[3];
  this->ControlPointGrid->GetDimensions(dims);
  this->NumberOfControlPoints = dims[0];
  return SV_OK;
}
// ----------------------
// SetControlPoint
// ----------------------
int vtkSVNURBSCurve::SetControlPoint(const int index, const double coordinates[3], const double weight)
{
  this->ControlPointGrid->SetControlPoint(index, 0, 0, coordinates, weight);
  return SV_OK;
}

// ----------------------
// SetControlPoint
// ----------------------
int vtkSVNURBSCurve::SetControlPoint(const int index, const double pw[4])
{
  this->ControlPointGrid->SetControlPoint(index, 0, 0, pw);
  return SV_OK;
}

// ----------------------
// GetControlPoint
// ----------------------
int vtkSVNURBSCurve::GetControlPoint(const int index, double coordinates[3], double &weight)
{
  this->ControlPointGrid->GetControlPoint(index, 0, 0, coordinates, weight);
  return SV_OK;
}

// ----------------------
// GetControlPoint
// ----------------------
int vtkSVNURBSCurve::GetControlPoint(const int index, double pw[4])
{
  this->ControlPointGrid->GetControlPoint(index, 0, 0, pw);
  return SV_OK;
}

// ----------------------
// SetWeights
// ----------------------
int vtkSVNURBSCurve::SetWeights(vtkDoubleArray *weights)
{
  if (weights->GetNumberOfTuples() != this->ControlPointGrid->GetPoints()->GetNumberOfPoints())
  {
    vtkErrorMacro("Cannot set weight vector on control points as number of tuples " << weights->GetNumberOfTuples() << " does not equal the number of control points " << this->ControlPointGrid->GetPoints()->GetNumberOfPoints());
    return SV_ERROR;
  }
  weights->SetName("Weights");
  this->ControlPointGrid->GetPointData()->AddArray(weights);
  return SV_OK;
}

// ----------------------
// GetWeights
// ----------------------
int vtkSVNURBSCurve::GetWeights(vtkDoubleArray *weights)
{
  weights = vtkDoubleArray::SafeDownCast(this->ControlPointGrid->GetPointData()->GetArray("Weights"));
  return SV_OK;
}

// ----------------------
// SetWeight
// ----------------------
int vtkSVNURBSCurve::SetWeight(const int index, const double weight)
{
  vtkDataArray *weights = this->ControlPointGrid->GetPointData()->GetArray("Weights");
  if (index >= weights->GetNumberOfTuples())
  {
    vtkErrorMacro("Index " << index << " outside length of weight vector: " << weights->GetNumberOfTuples());
    return SV_ERROR;
  }
  weights->SetTuple1(index, weight);

  return SV_ERROR;
}

// ----------------------
// GetWeight
// ----------------------
int vtkSVNURBSCurve::GetWeight(const int index, double &weight)
{
  vtkDataArray *weights = this->ControlPointGrid->GetPointData()->GetArray("Weights");
  if (index >= weights->GetNumberOfTuples())
  {
    vtkErrorMacro("Index " << index << " outside length of weight vector: " << weights->GetNumberOfTuples());
    return SV_ERROR;
  }
  weight = weights->GetTuple1(index);

  return SV_ERROR;
}

// ----------------------
// GeneratePolyDataRepresentation
// ----------------------
int vtkSVNURBSCurve::GeneratePolyDataRepresentation(const double spacing)
{
  // Get number of control points and knots
  this->NumberOfControlPoints = this->ControlPointGrid->GetNumberOfPoints();
  this->NumberOfKnotPoints = this->KnotVector->GetNumberOfTuples();
  int nCon  = this->NumberOfControlPoints;
  int nKnot = this->NumberOfKnotPoints;
  if (nCon == 0)
  {
    vtkErrorMacro("No control points");
    return SV_ERROR;
  }
  if (nKnot == 0)
  {
    vtkErrorMacro("No knot points");
    return SV_ERROR;
  }

  // Using clamped formula for degree of curve
  int p = nKnot - nCon - 1;

  // Get number of divisions from spacing
  int numDiv = ceil(1.0/spacing);

  // Get the parameter sampling
  vtkNew(vtkDoubleArray, uEvals);
  vtkSVNURBSUtils::LinSpace(0, 1, numDiv, uEvals);

  //If nCon - 1 < p, not possible with clamping
  //If nCon - 1 = p, bezier with clamping
  //If nCon - 1 > p, fantastic

  // Get sparse array for Nu
  vtkNew(vtkSparseArray<double>, Nus);
  Nus->Resize(numDiv, p+2);

  // Get sparse array for basis functions
  vtkNew(vtkSparseArray<double>, Nfinal);
  Nfinal->Resize(numDiv, nCon);

  // Get double array for rational basis functions
  vtkNew(vtkDoubleArray, Nrational);
  Nrational->SetNumberOfTuples(numDiv);
  Nrational->FillComponent(0, 0.0);

  // Loop through control points
  for (int i=0; i<nCon; i++)
  {
    // Evaluate the basis functions
    if (vtkSVNURBSUtils::BasisEvaluationVec(this->KnotVector, p,
                                       i, uEvals, Nus) != SV_OK)
    {
      return SV_ERROR;
    }
    // for each sampling get the final basis functions
    for (int j=0; j<numDiv; j++)
    {
      Nfinal->SetValue(j, i, Nus->GetValue(j, 0));

    }
  }

  vtkDataArray *weights = this->ControlPointGrid->GetPointData()->GetArray("Weights");
  // Last value should be 1
  Nfinal->SetValue(numDiv-1, nCon-1, 1.0);

  // Multiply by weights for rational curve
  for (int i=0; i<numDiv; i++)
  {
    double ratVal = 0.0;
    for (int j=0; j<nCon; j++)
    {
      double val       = Nfinal->GetValue(i, j);
      double ratWeight = weights->GetTuple1(j);
      ratVal = ratVal + val*ratWeight;
    }
    Nrational->SetTuple1(i, ratVal);
  }
  // Now that we have denominator, calculate numerators!
  for (int i=0; i<numDiv; i++)
  {
    for (int j=0; j<nCon; j++)
    {
      double val       = Nfinal->GetValue(i, j);
      double ratVal    = Nrational->GetTuple1(i);
      double ratWeight = weights->GetTuple1(j);
      Nfinal->SetValue(i, j, val*ratWeight/ratVal);
    }
  }

  // Must multiply the basis functions by our control grid to get the final
  // point set!
  vtkNew(vtkPoints, surfacePoints);
  if(vtkSVNURBSUtils::MatrixPointsMultiply(Nfinal, this->ControlPointGrid->GetPoints(),
                                       surfacePoints) != SV_OK)
  {
    return SV_ERROR;
  }

  // Get connectivity of our point set
  vtkNew(vtkCellArray, surfaceLines);
  this->GetStructuredGridConnectivity(numDiv, surfaceLines);

  // Update the curve representation
  this->CurveRepresentation->SetPoints(surfacePoints);
  this->CurveRepresentation->SetLines(surfaceLines);

  // Clean the curve in case of duplicate points (closed curve)
  vtkNew(vtkCleanPolyData, cleaner);
  cleaner->SetInputData(this->CurveRepresentation);
  cleaner->Update();

  // Get clean output and build links
  this->CurveRepresentation->DeepCopy(cleaner->GetOutput());
  this->CurveRepresentation->BuildLinks();

  return SV_OK;
}

// ----------------------
// GetStructuredGridConnectivity
// ----------------------
int vtkSVNURBSCurve::GetStructuredGridConnectivity(const int numPoints, vtkCellArray *connectivity)
{
  connectivity->Reset();
  vtkNew(vtkIdList, ptIds);
  ptIds->SetNumberOfIds(2);
  for (int i=0; i< numPoints - 1; i++)
  {
    ptIds->SetId(0, i);
    ptIds->SetId(1, i+1);
    connectivity->InsertNextCell(ptIds);
  }

  return SV_OK;
}

// ----------------------
// GetMultiplicity
// ----------------------
int vtkSVNURBSCurve::GetMultiplicity(vtkIntArray *multiplicity, vtkDoubleArray *singleKnots)
{
  vtkSVNURBSUtils::GetMultiplicity(this->KnotVector, multiplicity, singleKnots);
  return SV_OK;
}

// ----------------------
// ExtractBezierCurves
// ----------------------
int vtkSVNURBSCurve::ExtractBezierCurves(vtkSVNURBSCollection *curves)
{
  if (vtkSVNURBSUtils::CurveBezierExtraction(this->ControlPointGrid,
                                             this->KnotVector,
                                             this->Degree,
                                             curves) != SV_OK)
  {
    vtkErrorMacro("Error extracting Bezier curves");
    return SV_ERROR;
  }
  return SV_OK;
}
