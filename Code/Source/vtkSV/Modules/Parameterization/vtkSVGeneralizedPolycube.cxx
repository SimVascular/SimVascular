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

#include "vtkSVGeneralizedPolycube.h"

#include "vtkCellData.h"
#include "vtkIdList.h"
#include "vtkMath.h"
#include "vtkPointData.h"
#include "vtkSmartPointer.h"
#include "vtkSVGlobals.h"

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVGeneralizedPolycube);

// ----------------------
// Constructor
// ----------------------
vtkSVGeneralizedPolycube::vtkSVGeneralizedPolycube()
{
  this->SurgeryLines = vtkPolyData::New();

  vtkNew(vtkPoints, internalPoints);
  this->SetPoints(internalPoints);

  vtkNew(vtkIntArray, cubeType);
  cubeType->SetName("CubeType");
  this->GetCellData()->AddArray(cubeType);

  vtkNew(vtkIntArray, parentDirection);
  parentDirection->SetName("ParentDirection");
  this->GetCellData()->AddArray(parentDirection);
  vtkNew(vtkIntArray, childDirection);
  childDirection->SetName("ChildDirection");
  this->GetCellData()->AddArray(childDirection);

  vtkNew(vtkIntArray, corners);
  corners->SetNumberOfComponents(8);
  corners->SetName("CornerPtIds");
  this->GetCellData()->AddArray(corners);

  vtkNew(vtkIntArray, pointCorners);
  pointCorners->SetNumberOfComponents(1);
  pointCorners->SetName("CornerPtIds");
  this->GetPointData()->AddArray(pointCorners);

  vtkNew(vtkDoubleArray, topNormals);
  topNormals->SetNumberOfComponents(3);
  topNormals->SetName("TopNormal");
  this->GetCellData()->AddArray(topNormals);

  vtkNew(vtkDoubleArray, rightNormals);
  rightNormals->SetNumberOfComponents(3);
  rightNormals->SetName("RightNormal");
  this->GetCellData()->AddArray(rightNormals);
}

// ----------------------
// Destructor
// ----------------------
vtkSVGeneralizedPolycube::~vtkSVGeneralizedPolycube()
{
  if (this->SurgeryLines != NULL)
  {
    this->SurgeryLines->Delete();
    this->SurgeryLines = NULL;
  }
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVGeneralizedPolycube::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);

  os << indent << "Number of grids: " << this->GetNumberOfCells() << "\n";
}

// ----------------------
// Initialize
// ----------------------
void vtkSVGeneralizedPolycube::Initialize()
{
  this->Superclass::Initialize();
}

// ----------------------
// SetNumberOfGrids
// ----------------------
void vtkSVGeneralizedPolycube::SetNumberOfGrids(const int numberOfGrids)
{
  // Make sure we aren't trying to decrease the size
  int numCurrentGrids = this->GetNumberOfGrids();
  if (numberOfGrids <= numCurrentGrids)
  {
    vtkErrorMacro("Can only increase the number of grids, not decrease");
    return;
  }

  // Set up blank ids from points of cells
  vtkNew(vtkIdList, blankIds);
  blankIds->SetNumberOfIds(8);
  for (int i=0; i<8; i++)
    blankIds->SetId(i, 0);

  // Loop through new number of grids
  for (int i=numCurrentGrids; i <numberOfGrids; i++)
  {
    // Set new cell and data on new cell
    this->InsertNextCell(VTK_HEXAHEDRON, blankIds);
    this->GetCellData()->GetArray("CubeType")->InsertNextTuple1(-1);
    this->GetCellData()->GetArray("ParentDirection")->InsertNextTuple1(-1);
    this->GetCellData()->GetArray("ChildDirection")->InsertNextTuple1(-1);

    // Set blank cornder ids
    double blankCorner[8];
    for (int i=0; i<8; i++)
      blankCorner[i] = -1;

    // Set more cell data
    this->GetCellData()->GetArray("CornerPtIds")->InsertNextTuple(blankCorner);
    double topNormal[3]; topNormal[0] = 0.0; topNormal[1] = 0.0; topNormal[2] = 1.0;
    this->GetCellData()->GetArray("TopNormal")->InsertNextTuple(topNormal);
    double rightNormal[3]; rightNormal[0] = 1.0; rightNormal[1] = 0.0; rightNormal[2] = 0.0;
    this->GetCellData()->GetArray("RightNormal")->InsertNextTuple(rightNormal);
  }

  // We modified
  this->Modified();
}

// ----------------------
// GetNumberOfGrids
// ----------------------
int vtkSVGeneralizedPolycube::GetNumberOfGrids()
{
  return this->GetNumberOfCells();
}

// ----------------------
// InsertGridWithCenter
// ----------------------
int vtkSVGeneralizedPolycube::InsertGridWithCenter(const int cellId, const double center[3], const double dims[3], const int cubetype, const int parentdirection, const int childdirection)
{
  // Inserting, must allocate space if not enough already.
  if (cellId >= this->GetNumberOfCells())
    this->SetNumberOfGrids(cellId);

  // Now that there is space, set that grid
  this->SetGridWithCenter(cellId, center, dims, cubetype, parentdirection, childdirection);

  return SV_OK;
}

// ----------------------
// SetGridWithCenter
// ----------------------
int vtkSVGeneralizedPolycube::SetGridWithCenter(const int cellId, const double center[3], const double dims[3], const int cubetype, const int parentdirection, const int childdirection)
{
  // Set the origin based on the center and the dimensions given
  double origin[3];
  for (int i=0; i<3; i++)
    origin[i] = center[i] - dims[i]/2.0;

  // Set the gtid now
  this->SetGridWithOrigin(cellId, origin, dims, cubetype, parentdirection, childdirection);
  return SV_OK;
}

// ----------------------
// InsertGridWithCenter
// ----------------------
int vtkSVGeneralizedPolycube::InsertGridWithOrigin(const int cellId, const double origin[3], const double dims[3], const int cubetype, const int parentdirection, const int childdirection)
{
  // Inserting, must allocate space if not enough already.
  if (cellId >= this->GetNumberOfCells())
    this->SetNumberOfGrids(cellId);

  // Now that there is space, set that grid
  this->SetGridWithOrigin(cellId, origin, dims, cubetype, parentdirection, childdirection);

  return SV_OK;
}

// ----------------------
// SetGridWithCenter
// ----------------------
int vtkSVGeneralizedPolycube::SetGridWithOrigin(const int cellId, const double origin[3], const double dims[3], const int cubetype, const int parentdirection, const int childdirection, const double topNormal[3], const double rightNormal[3], const int corners[8])
{
  // Set cell data for vectors
  this->GetCellData()->GetArray("TopNormal")->SetTuple(cellId, topNormal);
  this->GetCellData()->GetArray("RightNormal")->SetTuple(cellId, rightNormal);

  // Loop through and set corner pt ids
  for (int i=0; i<8; i++)
    this->GetCellData()->GetArray("CornerPtIds")->SetComponent(cellId, i, corners[i]);

  // Set the grid
  this->SetGridWithOrigin(cellId, origin, dims, cubetype, parentdirection, childdirection);

  return SV_OK;
}

// ----------------------
// SetGridWithCenter
// ----------------------
/**
 * \details When setting the cube, we need to make sure they are ordered in
 * a certain way for the point data to actually correspond to something in the
 * physical model. This is how we order the nodes.
 * \verbatim
 *       z  y
 *       | /
 *       0 -- x
 *               2---------1
 *              /|        /|
 *             / |       / |
 *            3---------0  |
 *            |  |      |  |
 *            |  6------|--5
 *            | /       | /
 *            |/        |/
 *            7---------4
 * \endverbatim  */
int vtkSVGeneralizedPolycube::SetGridWithOrigin(const int cellId, const double origin[3], const double dims[3], const int cubetype, const int parentdirection, const int childdirection)
{
  // Set up the points
  vtkNew(vtkPoints, points);
  points->SetNumberOfPoints(8);

  // Get default zero value to get all of our points in a nice array
  double pts[8][3];
  double zero[3];
  for (int i=0; i<3; i++)
    zero[i] = 0.0;

  // Get all pts
  for (int i=0; i<8; i++)
    vtkMath::Add(origin, zero, pts[i]);

  // Set the points according to the point layout
  pts[4][0] = origin[0] + dims[0];

  pts[5][0] = origin[0] + dims[0];
  pts[5][1] = origin[1] + dims[1];

  pts[6][1] = origin[1] + dims[1];

  pts[3][2] = origin[2] + dims[2];

  pts[0][0] = origin[0] + dims[0];
  pts[0][2] = origin[2] + dims[2];

  pts[1][0] = origin[0] + dims[0];
  pts[1][1] = origin[1] + dims[1];
  pts[1][2] = origin[2] + dims[2];

  pts[2][1] = origin[1] + dims[1];
  pts[2][2] = origin[2] + dims[2];

  // Set the points in a vtkPoints
  for (int i=0; i<8; i++)
    points->SetPoint(i, pts[i]);

  // Set the grid
  this->SetGrid(cellId, points, cubetype, parentdirection, childdirection);

  return SV_OK;
}

// ----------------------
// InsertGrid
// ----------------------
int vtkSVGeneralizedPolycube::InsertGrid(const int cellId, vtkPoints *points, const int cubetype, const int parentdirection, const int childdirection)
{
  // Inserting, must allocate space if not enough already.
  if (cellId >= this->GetNumberOfCells())
    this->SetNumberOfGrids(cellId);

  // Now that there is space, set that grid
  this->SetGrid(cellId, points, cubetype, parentdirection, childdirection);

  return SV_OK;
}

// ----------------------
// SetGridWith
// ----------------------
// \details This is the one that actually sets the points!
int vtkSVGeneralizedPolycube::SetGrid(const int cellId, vtkPoints *points, const int cubetype, const int parentdirection, const int childdirection)
{
  // Check to see if the right number of points given
  int numPts = points->GetNumberOfPoints();
  if (numPts != 8)
  {
    vtkErrorMacro("Can only add grid with the 8 corner points");
    return SV_ERROR;
  }

  // Get the inital point id for the cell to reference
  vtkIdType pointIds[8];
  vtkIdType npts = 8;
  int numPCPts = this->GetNumberOfPoints();

  // Loop through points
  for (int i=0; i<numPts; i++)
  {
    // Insert a new point with data
    this->GetPoints()->InsertNextPoint(points->GetPoint(i));
    this->GetPointData()->GetArray("CornerPtIds")->InsertNextTuple1(-1.0);

    // Increment our point reference
    pointIds[i] = numPCPts++;
  }

  // If this is less than number of cells, we gucci
  if (cellId < this->GetNumberOfCells())
    this->ReplaceCell(cellId, npts, pointIds);
  // otherwise, we need to get upset
  else
  {
    vtkErrorMacro("CellId is greater than number of cells, must set number of grids to correct size first");
    return SV_ERROR;
  }

  // Add cell data
  this->GetCellData()->GetArray("CubeType")->SetTuple1(cellId, cubetype);
  this->GetCellData()->GetArray("ParentDirection")->SetTuple1(cellId, parentdirection);
  this->GetCellData()->GetArray("ChildDirection")->SetTuple1(cellId, childdirection);
  this->Modified();

  return SV_OK;
}
