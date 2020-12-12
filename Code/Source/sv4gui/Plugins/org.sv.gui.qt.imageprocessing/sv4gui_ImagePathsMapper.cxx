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

#include "sv4gui_ImagePathsMapper.h"
#include "sv4gui_ImagePathsContainer.h"
#include "vtkPolyDataMapper.h"
#include "vtkSphereSource.h"
#include "vtkCubeSource.h"
#include <vtkDataSetMapper.h>

sv4guiImagePathsMapper::sv4guiImagePathsMapper()
{
  m_Color[0] = 0.0;
  m_Color[1] = 1.0;
  m_Color[2] = 1.0;
  needsUpdate_ = true;
}

sv4guiImagePathsMapper::~sv4guiImagePathsMapper()
{
}

void sv4guiImagePathsMapper::SetUpdate(bool update)
{
  needsUpdate_ = update;
}

//-------------------------
// GenerateDataForRenderer
//-------------------------
// Generate the data needed for rendering into renderer.
//
void sv4guiImagePathsMapper::GenerateDataForRenderer(mitk::BaseRenderer* renderer)
{
  //std::cout << "===================== sv4guiImagePathsMapper::GenerateDataForRenderer =====================" << std::endl;
  mitk::DataNode* node = GetDataNode();
  if (node == nullptr) {
    return;
  }
  LocalStorage* localStorage = m_LSH.GetLocalStorage(renderer);
  if (localStorage == nullptr) {
    return;
  }

  bool visible = true;
  GetDataNode()->GetVisibility(visible, renderer, "visible");

  if (!visible) {
    localStorage->m_PropAssembly->VisibilityOff();
    return;
  }

  auto container = static_cast< sv4guiImagePathsContainer* >( node->GetData() );

  if (container == nullptr) {
    localStorage->m_PropAssembly->VisibilityOff();
    return;
  }

  // [DaveP] Do we need to remove?
  // local_storage->m_PropAssembly->GetParts()->RemoveAllItems();

  // Show paths.
  //
  const std::vector<sv3::PathElement>& pathElements = container->GetPathElements();

  if (pathElements.size() == 0)  {
      localStorage->m_PropAssembly->GetParts()->RemoveAllItems();
      return;
  }

  if (!needsUpdate_) { 
      localStorage->m_PropAssembly->VisibilityOn();
      return;
  }

  localStorage->m_PropAssembly->GetParts()->RemoveAllItems();
  //double radius = container->GetDistanceMeasure();
  auto spacing = container->GetImageSpacing();
  //double radius = (spacing[0] + spacing[1] + spacing[2]) / 3.0; 
  double radius = *std::max_element(spacing.begin(), spacing.end());

  //std::cout << "[GenerateDataForRenderer] radius: " << radius << std::endl;
  for (auto& pathElem : pathElements) { 
      auto controlPoints = pathElem.GetControlPoints();
      auto pathPoints = pathElem.GetPathPoints();
      for (auto& cpt : controlPoints) { 
          //std::cout << "[GenerateDataForRenderer] Control points: " << cpt[0] << " " << cpt[1] << " " << cpt[2] << std::endl;
          auto actor = AddControlPointMarker(cpt[0], cpt[1], cpt[2], radius);
          localStorage->m_PropAssembly->AddPart(actor);
      }
  }

  needsUpdate_ = false;
  localStorage->m_PropAssembly->VisibilityOn();
}

//-----------------------
// AddControlPointMarker
//-----------------------
//
vtkSmartPointer<vtkActor>
sv4guiImagePathsMapper::AddControlPointMarker(double x, double y, double z, double radius)
{
  auto sphere = vtkSmartPointer<vtkSphereSource>::New();
  sphere->SetRadius(radius);
  sphere->SetCenter(x,y,z);
  sphere->SetPhiResolution(16);
  sphere->SetThetaResolution(32);

  auto sphereMapper = vtkSmartPointer<vtkPolyDataMapper>::New();
  sphereMapper->SetInputConnection(sphere->GetOutputPort());

  auto sphereActor = vtkSmartPointer<vtkActor>::New();
  sphereActor->SetMapper(sphereMapper);
  sphereActor->GetProperty()->SetColor(m_Color);

  return sphereActor;
}

//----------
// SetColor
//----------
//
void sv4guiImagePathsMapper::SetColor(const float red, const float green, const float blue)
{
  m_Color[0] = red;
  m_Color[1] = green;
  m_Color[2] = blue;
}

//-------------
// ResetMapper
//-------------
//
void sv4guiImagePathsMapper::ResetMapper(mitk::BaseRenderer* renderer)
{
  LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
  ls->m_PropAssembly->VisibilityOff();
}

//------------
// GetVtkProp
//------------
//
vtkProp * 
sv4guiImagePathsMapper::GetVtkProp(mitk::BaseRenderer* renderer)
{
  if (renderer == nullptr) {
    return nullptr;
  }
  ResetMapper(renderer);
  GenerateDataForRenderer(renderer);
  LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
  return ls->m_PropAssembly;
}

