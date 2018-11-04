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

#include "vtkSVPlacePointsOnS2.h"

#include "vtkCellData.h"
#include "vtkErrorCode.h"
#include "vtkFloatArray.h"
#include "vtkMath.h"
#include "vtkMatrix4x4.h"
#include "vtkObjectFactory.h"
#include "vtkPolyData.h"
#include "vtkPointData.h"
#include "vtkSmartPointer.h"
#include "vtkTextureMapToSphere.h"
#include "vtkTransform.h"
#include "vtkTransformPolyDataFilter.h"

#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"

#include <iostream>
#include <cmath>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVPlacePointsOnS2);

// ----------------------
// Constructor
// ----------------------
vtkSVPlacePointsOnS2::vtkSVPlacePointsOnS2()
{
  this->InitialPd = vtkPolyData::New();
  this->WorkPd = vtkPolyData::New();

  this->UseCustomAxisAlign = 0;
  this->SetZAxis(0.0, 0.0, 1.0);
  this->SetXAxis(1.0, 0.0, 0.0);
}

// ----------------------
// Destructor
// ----------------------
vtkSVPlacePointsOnS2::~vtkSVPlacePointsOnS2()
{
  if (this->InitialPd != NULL)
  {
    this->InitialPd->Delete();
  }
  if (this->WorkPd != NULL)
  {
    this->WorkPd->Delete();
  }
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVPlacePointsOnS2::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);

  os << indent << "Use custom axis align: " << this->UseCustomAxisAlign << "\n";
  os << indent << "Z axis: " <<
    this->ZAxis[0] << " " << this->ZAxis[1] << " " << this->ZAxis[2] << "\n";
  os << indent << "X axis: " <<
    this->XAxis[0] << " " << this->XAxis[1] << " " << this->XAxis[2] << "\n";
}

// ----------------------
// RequestData
// ----------------------
int vtkSVPlacePointsOnS2::RequestData(vtkInformation *vtkNotUsed(request),
                                      vtkInformationVector **inputVector,
                                      vtkInformationVector *outputVector)
{
  // get the input and output
  vtkPolyData *input1 = vtkPolyData::GetData(inputVector[0]);
  vtkPolyData *output = vtkPolyData::GetData(outputVector);

  //Copy the input to operate on
  this->InitialPd->DeepCopy(input1);

  // Run the filter
  if (this->RunFilter() != SV_OK)
  {
    vtkErrorMacro("Filter failed");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }

  output->DeepCopy(this->WorkPd);
  return SV_OK;
}

// ----------------------
// RunFilter
// ----------------------
int vtkSVPlacePointsOnS2::RunFilter()
{
  // Set the working pd
  this->WorkPd->SetPoints(this->InitialPd->GetPoints());
  this->WorkPd->SetPolys(this->InitialPd->GetPolys());

  // Move to origin
  if (this->MoveToOrigin() != SV_OK)
  {
    vtkErrorMacro("Couldn't move to origin\n");
    return SV_ERROR;
  }

  // Use custom axis to align object at origin
  if (this->UseCustomAxisAlign)
  {
    if (this->RotateToCubeCenterAxis() != SV_OK)
    {
      vtkErrorMacro("Couldn't rotate\n");
      return SV_ERROR;
    }
  }

  // Scale to unit size using bounding box
  if (this->ScaleToUnitCube() != SV_OK)
  {
    vtkErrorMacro("Couldn't scale\n");
    return SV_ERROR;
  }

  // Use texture map to sphere
  if (this->DumbMapToSphere() != SV_OK)
  {
    vtkErrorMacro("Point placement failed");
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// DumbMapToSphere
// ----------------------
int vtkSVPlacePointsOnS2::DumbMapToSphere()
{
  // Texture map
  if (this->TextureMap() != SV_OK)
    return SV_ERROR;

  // Convert texture map to polydat
  if (this->ConvertTextureFieldToPolyData() != SV_OK)
    return SV_ERROR;

  return SV_OK;
}

// ----------------------
// TextureMap
// ----------------------
int vtkSVPlacePointsOnS2::TextureMap()
{
  // Use vtk texture mapper
  vtkNew(vtkTextureMapToSphere, texturer);
  texturer->SetInputData(this->WorkPd);
  texturer->PreventSeamOff();
  texturer->Update();

  this->WorkPd->DeepCopy(texturer->GetOutput());

  return SV_OK;
}

// ----------------------
// ConvertTextureFieldToPolyData
// ----------------------
int vtkSVPlacePointsOnS2::ConvertTextureFieldToPolyData()
{
  // Get texture coordinates
  vtkNew(vtkFloatArray, textureCoords);
  textureCoords = vtkFloatArray::SafeDownCast(this->WorkPd->GetPointData()->GetArray("Texture Coordinates"));

  // Loop through points
  int numPts = this->WorkPd->GetNumberOfPoints();
  for (int i=0; i< numPts; i++)
  {
    // Get tuple at point
    double tPt[2];
    textureCoords->GetTuple(i, tPt);

    // Convert tuple to a 3d point using spherical coordinates
    double pt[3];
    pt[0] = std::sin(SV_PI * tPt[1]) * std::cos(2.0 * SV_PI * tPt[0]);
    pt[1] = std::sin(SV_PI * tPt[1]) * std::sin(2.0 * SV_PI * tPt[0]);
    pt[2] = std::cos(SV_PI * tPt[1]);

    // Set point
    this->WorkPd->GetPoints()->SetPoint(i, pt);
  }

  return SV_OK;
}

// ----------------------
// RotateToCubeCenterAxis
// ----------------------
int vtkSVPlacePointsOnS2::RotateToCubeCenterAxis()
{
  // Default axis
  double realY[3], realZ[3];
  realY[0] = 0.0; realY[1] = 1.0; realY[2] = 0.0;
  realZ[0] = 0.0; realZ[1] = 0.0; realZ[2] = 1.0;

  // Compute new axis using custom axis
  double YAxis[3];
  vtkMath::Normalize(this->XAxis);
  vtkMath::Normalize(this->ZAxis);
  vtkMath::Cross(this->ZAxis, this->XAxis, YAxis);
  vtkMath::Normalize(YAxis);
  double inZ[4], outZ[4], rotZ[3];
  for (int i=0; i<3; i++)
    inZ[i] = this->ZAxis[i];
  inZ[3] = 1.0;

  // Rotate to new location using a rotation matrix
  vtkNew(vtkMatrix4x4, rotMatrix0);
  vtkNew(vtkMatrix4x4, rotMatrix1);
  vtkSVGeneralUtils::GetRotationMatrix(YAxis, realY, rotMatrix0);
  vtkSVGeneralUtils::ApplyRotationMatrix(this->WorkPd, rotMatrix0);
  rotMatrix0->MultiplyPoint(inZ, outZ);
  for (int i=0; i<3; i++)
    rotZ[i] = outZ[i];

  // Need to rotation around the other axis to get to the right orientation
  vtkSVGeneralUtils::GetRotationMatrix(rotZ, realZ, rotMatrix1);
  vtkSVGeneralUtils::ApplyRotationMatrix(this->WorkPd, rotMatrix1);

  return SV_OK;
}

// ----------------------
// MoveToOrigin
// ----------------------
int vtkSVPlacePointsOnS2::MoveToOrigin()
{
  // Get mass center
  double massCenter[3];
  vtkSVGeneralUtils::ComputeMassCenter(this->WorkPd, massCenter);

  // Loop through points
  int numPts = this->WorkPd->GetNumberOfPoints();
  for (int i=0; i<numPts; i++)
  {
    // Get point
    double pt[3];
    this->WorkPd->GetPoint(i, pt);

    // Move point
    double movePt[3];
    for (int j=0; j<3; j++)
      movePt[j] = pt[j] - massCenter[j];

    // Set new point location
    this->WorkPd->GetPoints()->SetPoint(i, movePt);
  }

  return SV_OK;
}

// ----------------------
// ScaleToUnitCube
// ----------------------
int vtkSVPlacePointsOnS2::ScaleToUnitCube()
{
  // GetBounds
  double bounds[6];
  this->WorkPd->GetBounds(bounds);

  // Get scale factors from bounds
  double xScaleFactor = 1.0/(bounds[1]-bounds[0]);
  double yScaleFactor = 1.0/(bounds[3]-bounds[2]);
  double zScaleFactor = 1.0/4*(bounds[5]-bounds[4]);

  // Set up transformer
  vtkNew(vtkTransform, transformer);
  transformer->Scale(xScaleFactor, yScaleFactor, zScaleFactor);
  transformer->Update();

  // Apply transform to polydata
  vtkNew(vtkTransformPolyDataFilter, pdTransformer);
  pdTransformer->SetInputData(this->WorkPd);
  pdTransformer->SetTransform(transformer);
  pdTransformer->Update();

  // Copy the output
  this->WorkPd->DeepCopy(pdTransformer->GetOutput());
  this->WorkPd->BuildLinks();

  return SV_OK;
}
