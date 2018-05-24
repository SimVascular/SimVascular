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

/*===================================================================

The Medical Imaging Interaction Toolkit (MITK)

Copyright (c) German Cancer Research Center,
Division of Medical and Biological Informatics.
All rights reserved.

This software is distributed WITHOUT ANY WARRANTY; without
even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE.

See LICENSE.txt or http://www.mitk.org for details.

===================================================================*/

#include "sv4gui_Surface.h"
#include "mitkInteractionConst.h"
#include "mitkSurfaceOperation.h"

#include <algorithm>
#include <vtkPolyData.h>

static vtkSmartPointer<vtkPolyData> DeepCopy(vtkPolyData* other)
{
  if (other == nullptr)
    return nullptr;

  vtkSmartPointer<vtkPolyData> copy = vtkSmartPointer<vtkPolyData>::New();
  copy->DeepCopy(other);

  return copy;
}

static void Update(vtkPolyData* /*polyData*/)
{
//  if (polyData != NULL)
//    polyData->Update(); //VTK6_TODO vtk pipeline
}

mitk::sv4guiSurface::sv4guiSurface()
  : m_CalculateBoundingBox(false)
{
  this->InitializeEmpty();
}

mitk::sv4guiSurface::sv4guiSurface(const mitk::sv4guiSurface& other)
  : BaseData(other),
    m_LargestPossibleRegion(other.m_LargestPossibleRegion),
    m_RequestedRegion(other.m_RequestedRegion),
    m_CalculateBoundingBox(other.m_CalculateBoundingBox)
{
  if(!other.m_PolyDatas.empty())
  {
    m_PolyDatas.resize(other.m_PolyDatas.size());
    std::transform(other.m_PolyDatas.cbegin(), other.m_PolyDatas.cend(), m_PolyDatas.begin(), DeepCopy);
  }
  else
  {
    this->InitializeEmpty();
  }
}

void mitk::sv4guiSurface::Swap(mitk::sv4guiSurface& other)
{
  std::swap(m_PolyDatas, other.m_PolyDatas);
  std::swap(m_LargestPossibleRegion, other.m_LargestPossibleRegion);
  std::swap(m_RequestedRegion, other.m_RequestedRegion);
  std::swap(m_CalculateBoundingBox, other.m_CalculateBoundingBox);
}

mitk::sv4guiSurface& mitk::sv4guiSurface::operator=(mitk::sv4guiSurface other)
{
  this->Swap(other);
  return *this;
}

mitk::sv4guiSurface::~sv4guiSurface()
{
  this->ClearData();
}

void mitk::sv4guiSurface::ClearData()
{
  m_PolyDatas.clear();

  Superclass::ClearData();
}

const mitk::sv4guiSurface::RegionType& mitk::sv4guiSurface::GetLargestPossibleRegion() const
{
  m_LargestPossibleRegion.SetIndex(3, 0);
  m_LargestPossibleRegion.SetSize(3, GetTimeGeometry()->CountTimeSteps());

  return m_LargestPossibleRegion;
}

const mitk::sv4guiSurface::RegionType& mitk::sv4guiSurface::GetRequestedRegion() const
{
  return m_RequestedRegion;
}

void mitk::sv4guiSurface::InitializeEmpty()
{
  if (!m_PolyDatas.empty())
    this->ClearData();

  Superclass::InitializeTimeGeometry();

  m_PolyDatas.push_back(nullptr);
  m_Initialized = true;
}

void mitk::sv4guiSurface::SetVtkPolyData(vtkPolyData* polyData, unsigned int t)
{
  this->Expand(t + 1);

  if (m_PolyDatas[t] != nullptr)
  {
    if (m_PolyDatas[t].GetPointer() == polyData)
      return;
  }

  m_PolyDatas[t].TakeReference(polyData);

  if(polyData != nullptr)
    polyData->Register(nullptr);

  m_CalculateBoundingBox = true;

  this->Modified();
  this->UpdateOutputInformation();
}

bool mitk::sv4guiSurface::IsEmptyTimeStep(unsigned int t) const
{
  if(!IsInitialized())
    return false;

  vtkPolyData* polyData = const_cast<mitk::sv4guiSurface*>(this)->GetVtkPolyData(t);

  return polyData == nullptr || (
    polyData->GetNumberOfLines() == 0 &&
    polyData->GetNumberOfPolys() == 0 &&
    polyData->GetNumberOfStrips() == 0 &&
    polyData->GetNumberOfVerts() == 0
  );
}

vtkPolyData* mitk::sv4guiSurface::GetVtkPolyData(unsigned int t) const
{
  if (t < m_PolyDatas.size())
  {
    if(m_PolyDatas[t] == nullptr && this->GetSource().IsNotNull())
    {
      RegionType requestedRegion;
      requestedRegion.SetIndex(3, t);
      requestedRegion.SetSize(3, 1);
      this->m_RequestedRegion = requestedRegion;
      this->GetSource()->Update();
    }

    return m_PolyDatas[t].GetPointer();
  }

  return nullptr;
}

void mitk::sv4guiSurface::UpdateOutputInformation()
{
  if (this->GetSource().IsNotNull())
    this->GetSource()->UpdateOutputInformation();

  if (m_CalculateBoundingBox == true && !m_PolyDatas.empty())
    this->CalculateBoundingBox();
  else
    this->GetTimeGeometry()->Update();
}

void mitk::sv4guiSurface::CalculateBoundingBox()
{
  TimeGeometry* timeGeometry = this->GetTimeGeometry();

  if (timeGeometry->CountTimeSteps() != m_PolyDatas.size())
    mitkThrow() << "Number of geometry time steps is inconsistent with number of poly data pointers.";

  for (unsigned int i = 0; i < m_PolyDatas.size(); ++i)
  {
    vtkPolyData* polyData = m_PolyDatas[i].GetPointer();
    double bounds[6] = {0};

    if (polyData != nullptr && polyData->GetNumberOfPoints() > 0)
    {
//      polyData->Update(); //VTK6_TODO vtk pipeline
      polyData->ComputeBounds();
      polyData->GetBounds(bounds);
    }

    BaseGeometry::Pointer geometry = timeGeometry->GetGeometryForTimeStep(i);

    if (geometry.IsNull())
      mitkThrow() << "Time-sliced geometry is invalid (equals NULL).";

    geometry->SetFloatBounds(bounds);
  }

  timeGeometry->Update();
  m_CalculateBoundingBox = false;
}

void mitk::sv4guiSurface::SetRequestedRegionToLargestPossibleRegion()
{
  m_RequestedRegion = GetLargestPossibleRegion();
}

bool mitk::sv4guiSurface::RequestedRegionIsOutsideOfTheBufferedRegion()
{
  RegionType::IndexValueType end = m_RequestedRegion.GetIndex(3) + m_RequestedRegion.GetSize(3);

  if(static_cast<RegionType::IndexValueType>(m_PolyDatas.size()) < end)
    return true;

  for(RegionType::IndexValueType t = m_RequestedRegion.GetIndex(3); t < end; ++t)
  {
    if(m_PolyDatas[t] == nullptr)
      return true;
  }

  return false;
}

bool mitk::sv4guiSurface::VerifyRequestedRegion()
{
  if(m_RequestedRegion.GetIndex(3) >= 0 && m_RequestedRegion.GetIndex(3) + m_RequestedRegion.GetSize(3) <= m_PolyDatas.size())
    return true;

  return false;
}

void mitk::sv4guiSurface::SetRequestedRegion(const itk::DataObject* data )
{
  const mitk::sv4guiSurface *surface = dynamic_cast<const mitk::sv4guiSurface*>(data);

  if (surface != nullptr)
    m_RequestedRegion = surface->GetRequestedRegion();
  else
    mitkThrow() << "Data object used to get requested region is not a mitk::sv4guiSurface.";
}

void mitk::sv4guiSurface::SetRequestedRegion(mitk::sv4guiSurface::RegionType* region)
{
  if (region == nullptr)
    mitkThrow() << "Requested region is invalid (equals NULL)";

  m_RequestedRegion = *region;
}

void mitk::sv4guiSurface::CopyInformation(const itk::DataObject* data)
{
  Superclass::CopyInformation(data);

  const mitk::sv4guiSurface* surface = dynamic_cast<const mitk::sv4guiSurface*>(data);

  if (surface == nullptr)
    mitkThrow() << "Data object used to get largest possible region is not a mitk::sv4guiSurface.";

  m_LargestPossibleRegion = surface->GetLargestPossibleRegion();
}

void mitk::sv4guiSurface::Update()
{
  using ::Update;

  if (this->GetSource().IsNull())
    std::for_each(m_PolyDatas.begin(), m_PolyDatas.end(), Update);

  Superclass::Update();
}

void mitk::sv4guiSurface::Expand(unsigned int timeSteps)
{
  if (timeSteps > m_PolyDatas.size())
  {
    Superclass::Expand(timeSteps);

    m_PolyDatas.resize(timeSteps);
    m_CalculateBoundingBox = true;
  }
}

void mitk::sv4guiSurface::ExecuteOperation(Operation* operation)
{
  switch (operation->GetOperationType())
  {
    case OpSURFACECHANGED:
    {
      mitk::SurfaceOperation* surfaceOperation = dynamic_cast<mitk::SurfaceOperation*>(operation);

      if(surfaceOperation == nullptr)
        break;

      unsigned int timeStep = surfaceOperation->GetTimeStep();

      if(m_PolyDatas[timeStep] != nullptr)
      {
        vtkPolyData* updatedPolyData = surfaceOperation->GetVtkPolyData();

        if(updatedPolyData != nullptr)
        {
          this->SetVtkPolyData(updatedPolyData, timeStep);
          this->CalculateBoundingBox();
          this->Modified();
        }
      }

      break;
    }

    default:
      return;
  }
}

unsigned int mitk::sv4guiSurface::GetSizeOfPolyDataSeries() const
{
  return m_PolyDatas.size();
}

void mitk::sv4guiSurface::Graft(const DataObject* data)
{
  const mitk::sv4guiSurface* surface = dynamic_cast<const mitk::sv4guiSurface*>(data);

  if(surface == nullptr)
    mitkThrow() << "Data object used to graft surface is not a mitk::sv4guiSurface.";

  this->CopyInformation(data);
  m_PolyDatas.clear();

  for (unsigned int i = 0; i < surface->GetSizeOfPolyDataSeries(); ++i)
  {
    m_PolyDatas.push_back(vtkSmartPointer<vtkPolyData>::New());
    m_PolyDatas.back()->DeepCopy(const_cast<mitk::sv4guiSurface*>(surface)->GetVtkPolyData(i));
  }
}

void mitk::sv4guiSurface::PrintSelf(std::ostream& os, itk::Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "\nNumber PolyDatas: " << m_PolyDatas.size() << "\n";

  unsigned int count = 0;

  for (auto it = m_PolyDatas.begin(); it != m_PolyDatas.end(); ++it)
  {
    os << "\n";

    if(*it != nullptr)
    {
      os << indent << "PolyData at time step " << count << ":\n";
      os << indent << "Number of cells: " << (*it)->GetNumberOfCells() << "\n";
      os << indent << "Number of points: " << (*it)->GetNumberOfPoints() << "\n\n";
      os << indent << "VTKPolyData:\n";

      (*it)->Print(os);
    }
    else
    {
      os << indent << "Empty PolyData at time step " << count << "\n";
    }

    ++count;
  }
}

bool mitk::Equal( vtkPolyData* leftHandSide, vtkPolyData* rightHandSide, mitk::ScalarType eps, bool verbose )
{
  if(( leftHandSide == nullptr ) || ( rightHandSide == nullptr ))
  {
    MITK_ERROR << "mitk::Equal( vtkPolyData* leftHandSide, vtkPolyData* rightHandSide, mitk::ScalarType eps, bool verbose ) does not work for NULL pointer input.";
    return false;
  }
  return Equal( *leftHandSide, *rightHandSide, eps, verbose);
}


bool mitk::Equal( vtkPolyData& leftHandSide, vtkPolyData& rightHandSide, mitk::ScalarType eps, bool verbose )
{
  bool noDifferenceFound = true;

  if( ! mitk::Equal( leftHandSide.GetNumberOfCells(), rightHandSide.GetNumberOfCells(), eps, verbose ) )
  {
    if(verbose)
      MITK_INFO << "[Equal( vtkPolyData*, vtkPolyData* )] Number of cells not equal";
    noDifferenceFound = false;
  }

  if( ! mitk::Equal( leftHandSide.GetNumberOfVerts(), rightHandSide.GetNumberOfVerts(), eps, verbose ))
  {
    if(verbose)
      MITK_INFO << "[Equal( vtkPolyData*, vtkPolyData* )] Number of vertices not equal";
    noDifferenceFound = false;
  }

  if( ! mitk::Equal( leftHandSide.GetNumberOfLines(), rightHandSide.GetNumberOfLines(), eps, verbose ) )
  {
    if(verbose)
      MITK_INFO << "[Equal( vtkPolyData*, vtkPolyData* )] Number of lines not equal";
    noDifferenceFound = false;
  }

  if( ! mitk::Equal( leftHandSide.GetNumberOfPolys(), rightHandSide.GetNumberOfPolys(), eps, verbose ) )
  {
    if(verbose)
      MITK_INFO << "[Equal( vtkPolyData*, vtkPolyData* )] Number of polys not equal";
    noDifferenceFound = false;
  }

  if( ! mitk::Equal( leftHandSide.GetNumberOfStrips(), rightHandSide.GetNumberOfStrips(), eps, verbose ) )
  {
    if(verbose)
      MITK_INFO << "[Equal( vtkPolyData*, vtkPolyData* )] Number of strips not equal";
    noDifferenceFound = false;
  }

  {
    unsigned int numberOfPointsRight = rightHandSide.GetPoints()->GetNumberOfPoints();
    unsigned int numberOfPointsLeft = leftHandSide.GetPoints()->GetNumberOfPoints();
    if(! mitk::Equal( numberOfPointsLeft, numberOfPointsRight, eps, verbose ))
    {
      if(verbose)
        MITK_INFO << "[Equal( vtkPolyData*, vtkPolyData* )] Number of points not equal";
      noDifferenceFound = false;
    }
    else
    {
      for( unsigned int i( 0 ); i < numberOfPointsRight; i++ )
      {
        bool pointFound = false;
        double pointOne[3];
        rightHandSide.GetPoints()->GetPoint(i, pointOne);

        for( unsigned int j( 0 ); j < numberOfPointsLeft; j++ )
        {
          double pointTwo[3];
          leftHandSide.GetPoints()->GetPoint(j, pointTwo);

          double x = pointOne[0] - pointTwo[0];
          double y = pointOne[1] - pointTwo[1];
          double z = pointOne[2] - pointTwo[2];
          double distance = x*x + y*y + z*z;

          if( distance < eps )
          {
            pointFound = true;
            break;
          }
        }
        if( !pointFound )
        {
          if(verbose)
          {
            MITK_INFO << "[Equal( vtkPolyData*, vtkPolyData* )] Right hand side point with id " << i << " and coordinates ( "
                    << std::setprecision(12)
                    << pointOne[0] << " ; " << pointOne[1] << " ; " << pointOne[2]
                    << " ) could not be found in left hand side with epsilon " << eps << ".";
          }
          noDifferenceFound = false;
          break;
        }
      }
    }
  }
  return noDifferenceFound;
}

bool mitk::Equal( mitk::sv4guiSurface* leftHandSide, mitk::sv4guiSurface* rightHandSide, mitk::ScalarType eps, bool verbose )
{
  if(( leftHandSide == nullptr ) || ( rightHandSide == nullptr ))
  {
    MITK_ERROR << "mitk::Equal( mitk::sv4guiSurface* leftHandSide, mitk::sv4guiSurface* rightHandSide, mitk::ScalarType eps, bool verbose ) does not work with NULL pointer input.";
    return false;
  }
  return Equal( *leftHandSide, *rightHandSide, eps, verbose);
}


bool mitk::Equal( mitk::sv4guiSurface& leftHandSide, mitk::sv4guiSurface& rightHandSide, mitk::ScalarType eps, bool verbose )
{
  bool noDifferenceFound = true;

  if( ! mitk::Equal( leftHandSide.GetSizeOfPolyDataSeries(), rightHandSide.GetSizeOfPolyDataSeries(), eps, verbose ) )
  {
    if(verbose)
      MITK_INFO << "[Equal( mitk::surface&, mitk::surface& )] Size of PolyData series not equal.";
    return false;
  }

  // No mitk::Equal for TimeGeometry implemented.
  //if( ! mitk::Equal( leftHandSide->GetTimeGeometry(), rightHandSide->GetTimeGeometry(), eps, verbose ) )
  //{
  //  if(verbose)
  //    MITK_INFO << "[Equal( mitk::surface&, mitk::surface& )] Time sliced geometries not equal";
  //  noDifferenceFound = false;
  //}

  for( unsigned int i( 0 ); i < rightHandSide.GetSizeOfPolyDataSeries(); i++ )
  {
    if( ! mitk::Equal( *leftHandSide.GetVtkPolyData( i ), *rightHandSide.GetVtkPolyData( i ), eps, verbose ) )
    {
      if(verbose)
        MITK_INFO << "[Equal( mitk::surface&, mitk::surface& )] Poly datas not equal.";
      noDifferenceFound = false;
    }
  }

  return noDifferenceFound;
}
