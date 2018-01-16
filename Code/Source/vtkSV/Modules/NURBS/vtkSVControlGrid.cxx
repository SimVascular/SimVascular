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

#include "vtkSVControlGrid.h"

#include "vtkDataArray.h"
#include "vtkDoubleArray.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPoints.h"
#include "vtkSmartPointer.h"
#include "vtkSVGlobals.h"

vtkStandardNewMacro(vtkSVControlGrid);

// ----------------------
// Constructor
// ----------------------
vtkSVControlGrid::vtkSVControlGrid()
{
  vtkNew(vtkPoints, internalPoints);
  this->SetPoints(internalPoints);

  vtkNew(vtkDoubleArray, internalWeights);
  internalWeights->SetName("Weights");
  this->GetPointData()->AddArray(internalWeights);
}

// ----------------------
// Destructor
// ----------------------
vtkSVControlGrid::~vtkSVControlGrid()
{
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVControlGrid::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);
}

// ----------------------
// CopyStructure
// ----------------------
void vtkSVControlGrid::CopyStructure(vtkDataSet *ds)
{
  this->Superclass::CopyStructure(ds);
}

// ----------------------
// Initialize
// ----------------------
void vtkSVControlGrid::Initialize()
{
  this->Superclass::Initialize();
}

// ----------------------
// GetData
// ----------------------
vtkSVControlGrid* vtkSVControlGrid::GetData(vtkInformation* info)
{
  return info? vtkSVControlGrid::SafeDownCast(info->Get(DATA_OBJECT())) : 0;
}

// ----------------------
// GetData
// ----------------------
vtkSVControlGrid* vtkSVControlGrid::GetData(vtkInformationVector* v, int i)
{
  return vtkSVControlGrid::GetData(v->GetInformationObject(i));
}

// ----------------------
// SetControlPoint
// ----------------------
int vtkSVControlGrid::SetControlPoint(const int i, const int j, const int k, const double p[3], const double w)
{
  int ptId;
  this->GetPointId(i, j, k, ptId);
  this->GetPoints()->SetPoint(ptId, p);
  this->GetPointData()->GetArray("Weights")->InsertTuple1(ptId, w);

  return SV_OK;
}

// ----------------------
// SetControlPoint
// ----------------------
int vtkSVControlGrid::SetControlPoint(const int i, const int j, const int k, const double pw[4])
{
  double onlyp[3];
  for (int l=0; l<3; l++)
  {
    onlyp[l] = pw[l];
  }
  double w = pw[3];

  this->SetControlPoint(i, j, k, onlyp, w);

  return SV_OK;
}

// ----------------------
// InsertControlPoint
// ----------------------
int vtkSVControlGrid::InsertControlPoint(const int i, const int j, const int k, const double p[3], const double w)
{
  int dim[3];
  this->GetDimensions(dim);
  if (i >= dim[0])
  {
    dim[0] = i + 1;
  }
  if (j >= dim[1])
  {
    dim[1] = j + 1;
  }
  if (k >= dim[2])
  {
    dim[2] = k + 1;
  }
  this->SetDimensions(dim);
  this->GetPoints()->SetNumberOfPoints((dim[0]*dim[1]*dim[2]));

  this->SetControlPoint(i, j, k, p, w);

  return SV_OK;
}

// ----------------------
// InsertControlPoint
// ----------------------
int vtkSVControlGrid::InsertControlPoint(const int i, const int j, const int k, const double pw[4])
{
  double onlyp[3];
  for (int l=0; l<3; l++)
  {
    onlyp[l] = pw[l];
  }
  double w = pw[3];

  this->InsertControlPoint(i, j, k, onlyp, w);

  return SV_OK;
}

// ----------------------
// GetControlPoint
// ----------------------
int vtkSVControlGrid::GetControlPoint(const int i, const int j, const int k, double p[3], double &w)
{
  int ptId;
  if (this->GetPointId(i, j, k, ptId) != SV_OK)
  {
    vtkErrorMacro("Point not retrieved successfully");
    return SV_ERROR;
  }
  this->GetPoint(ptId, p);
  w = this->GetPointData()->GetArray("Weights")->GetTuple1(ptId);

  return SV_OK;
}

// ----------------------
// GetControlPoint
// ----------------------
int vtkSVControlGrid::GetControlPoint(const int i, const int j, const int k, double pw[4])
{
  double onlyp[3];
  for (int l=0; l<3; l++)
  {
    onlyp[l] = pw[l];
  }
  double w = pw[3];

  if (this->GetControlPoint(i, j, k, onlyp, w) != SV_OK)
    vtkErrorMacro("Point not retrieved successfully");

  return SV_OK;
}

// ----------------------
// GetPointId
// ----------------------
int vtkSVControlGrid::GetPointId(const int i, const int j, const int k, int &ptId)
{
  int extent[6];
  this->GetExtent(extent);

  //fprintf(stdout,"Extents: %d %d %d %d %d %d\n", extent[0],
  //                                                  extent[1],
  //                                                  extent[2],
  //                                                  extent[3],
  //                                                  extent[4],
  //                                                  extent[5]);
  if(i < extent[0] || i > extent[1] ||
     j < extent[2] || j > extent[3] ||
     k < extent[4] || k > extent[5])
    {
    vtkErrorMacro("ERROR: IJK coordinates are outside of grid extent!");
    return SV_ERROR; // out of bounds!
    }

  int pos[3];
  pos[0] = i;
  pos[1] = j;
  pos[2] = k;

  ptId = vtkStructuredData::ComputePointIdForExtent(extent, pos);

  return SV_OK;
}
