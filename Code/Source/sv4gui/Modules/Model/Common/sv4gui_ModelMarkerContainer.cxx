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

#include "sv4gui_ModelMarkerContainer.h"
#include "math.h"

#include <vtkCellLocator.h>
#include <vtkGenericCell.h>

sv4guiModelMarkerContainer::sv4guiModelMarkerContainer()
{
  m_Markers = nullptr;
};

sv4guiModelMarkerContainer::sv4guiModelMarkerContainer(const sv4guiModelMarkerContainer& other) :BaseData(other)
{
};

sv4guiModelMarkerContainer::~sv4guiModelMarkerContainer()
{
};

//------------
// GetMarkers
//------------
//
vtkSmartPointer<vtkPolyData>
sv4guiModelMarkerContainer::GetMarkers()
{
  return m_Markers;
}

//------------
// SetMarkers
//------------
//
void sv4guiModelMarkerContainer::SetMarkers(vtkPolyData* markers)
{
  #define n_debug_SetMarkers
  #ifdef debug_SetMarkers
  std::string msg("[sv4guiModelMarkerContainer::SetMarkers] ");
  std::cout << msg << "========== SetMarkers ==========" << std::endl;
  std::cout << msg << "markers: " << markers << std::endl;
  std::cout << msg << "markers num points: " << markers->GetNumberOfPoints() << std::endl;
  #endif

  if (m_Markers != nullptr) { 
      m_Markers->Delete();
  }

  m_Markers = vtkSmartPointer<vtkPolyData>::New();
  m_Markers->DeepCopy(markers);
  #ifdef debug_SetMarkers
  std::cout << msg << "m_Markers: " << m_Markers << std::endl;
  #endif
}

void sv4guiModelMarkerContainer::AddMarker(const double pt[3])
{
}

//-----------------------
// FindPointOnCenterline
//-----------------------
//
void sv4guiModelMarkerContainer::FindPointOnCenterline(double x, double y, double z, double tol, 
    bool& found, double closestPoint[3], vtkIdType& cellID, int& subID)
{
  found = false;
  if (m_Markers == nullptr) {
      return;
  }

  auto cellLocator = vtkSmartPointer<vtkCellLocator>::New();
  cellLocator->SetDataSet(m_Markers);
  cellLocator->BuildLocator();

  double testPoint[3] = {x, y, z};
  //std::cout << "FindPointOnCenterline] Test point: " << x << " " << y << " " << z << std::endl;

  double closestPointDist2; 
  //vtkIdType cellId; 
  cellLocator->FindClosestPoint(testPoint, closestPoint, cellID, subID, closestPointDist2);
  /*
  std::cout << "[FindPointOnCenterline] Squared distance to closest point: " << closestPointDist2 << std::endl;
  std::cout << "[FindPointOnCenterline] CellId: " << cellId << std::endl;
  std::cout << "[FindPointOnCenterline] subId: " << subId << std::endl;
  */

  if (closestPointDist2 <= tol) {
      found = true;
  }



}


