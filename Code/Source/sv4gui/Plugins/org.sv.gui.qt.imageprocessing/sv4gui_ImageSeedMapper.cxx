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

#include "sv4gui_ImageSeedMapper.h"
#include "sv4gui_ImageSeedContainer.h"
#include "vtkPolyDataMapper.h"
#include "vtkSphereSource.h"
#include "vtkCubeSource.h"

// Set the colors used to display seed spheres.
double sv4guiImageSeedMapper::START_SEED_COLOR[3] = {0.0, 1.0, 0.0};
double sv4guiImageSeedMapper::START_SEED_HIGHLIGHT_COLOR[3] = {1.0, 1.0, 0.0};
double sv4guiImageSeedMapper::END_SEED_COLOR[3] = {1.0, 0.0, 0.0};
double sv4guiImageSeedMapper::END_SEED_HIGHLIGHT_COLOR[3] = {1.0, 0.5, 0.0};

//--------------
// CreateSphere
//--------------
// Create a sphere at the given location for display in a 2D window.
//
// Arguments:
//   isStartSeed: If true then the sphere is a start sphere and is display with a predefined color.
//
vtkSmartPointer<vtkActor>
sv4guiImageSeedMapper::CreateSphere(double x, double y, double z, double radius, bool isStartSeed, bool active)
{
  auto sphere = vtkSmartPointer<vtkSphereSource>::New();
  sphere->SetRadius(radius);
  sphere->SetCenter(x,y,z);
  sphere->SetPhiResolution(50);
  sphere->SetThetaResolution(100);

  auto sphereMapper = vtkSmartPointer<vtkPolyDataMapper>::New();
  sphereMapper->SetInputConnection(sphere->GetOutputPort());

  auto sphereActor = vtkSmartPointer<vtkActor>::New();
  sphereActor->SetMapper(sphereMapper);
  if (isStartSeed){
    sphereActor->GetProperty()->SetColor(START_SEED_COLOR);
  } else {
    sphereActor->GetProperty()->SetColor(END_SEED_COLOR);
  }

  // Make active seeds brighter.
  if (active) {
    sphereActor->GetProperty()->SetAmbient(0.6);
    //sphereActor->GetProperty()->SetDiffuse(0.3);
    sphereActor->GetProperty()->SetSpecular(0.5);
    sphereActor->GetProperty()->SetSpecularPower(10.0);
  }

  return sphereActor;
}


sv4guiImageSeedMapper::sv4guiImageSeedMapper(){
}

sv4guiImageSeedMapper::~sv4guiImageSeedMapper(){

}

//-------------------------
// GenerateDataForRenderer
//-------------------------
// Create the seed VTK mappers and actors for displaying the seeds as spheres.
//
// Note that this method is called whenever the mouse is moved!
//
void sv4guiImageSeedMapper::GenerateDataForRenderer(mitk::BaseRenderer* renderer)
{
  //std::cout << "===================== sv4guiImageSeedMapper::GenerateDataForRenderer =====================" << std::endl;
  mitk::DataNode* node = GetDataNode();
  if (node == NULL) {
      return;
  }

  LocalStorage* localStorage = m_LSH.GetLocalStorage(renderer);
  bool visible = true;
  GetDataNode()->GetVisibility(visible, renderer, "visible");
  if (!visible) {
      localStorage->m_PropAssembly->VisibilityOff();
      return;
  }

  auto seeds = static_cast<sv4guiImageSeedContainer*>(GetDataNode()->GetData());
  if (seeds == NULL) {
      localStorage->m_PropAssembly->VisibilityOff();
      return;
  }
  localStorage->m_PropAssembly->GetParts()->RemoveAllItems();

  // Iterate over start and end seed points.
  //
  for (auto const& seed : seeds->m_StartSeeds) {
    auto startSeed = std::get<0>(seed.second);
    bool active = (startSeed.id == seeds->m_ActiveStartSeedID);
    auto point = startSeed.point;
    bool isStartSeed = true;
    auto startSphere = CreateSphere(point[0], point[1], point[2], m_seedRadius, isStartSeed, active);
    if (seeds->selectStartSeed && (seeds->selectStartSeedIndex == startSeed.id)) { 
      startSphere->GetProperty()->SetColor(START_SEED_HIGHLIGHT_COLOR);
    }
    localStorage->m_PropAssembly->AddPart(startSphere);

    isStartSeed = false;
    for (auto const& endSeed : std::get<1>(seed.second)) {
      auto point = endSeed.point;
      auto endSphere = CreateSphere(point[0], point[1], point[2], m_seedRadius, isStartSeed, active);
      if (seeds->selectEndSeed && (seeds->selectEndSeedIndex == endSeed.id)) { 
        endSphere->GetProperty()->SetColor(END_SEED_HIGHLIGHT_COLOR);
      }
      localStorage->m_PropAssembly->AddPart(endSphere);
    }
  }

/*
  int numStartSeeds = seeds->getNumStartSeeds();
  for (int i = 0; i < numStartSeeds; i++){
    auto point = seeds->getStartSeed(i);
    bool isStartSeed = true;
    auto startSphere = CreateSphere(point[0], point[1], point[2], m_seedRadius, isStartSeed);
    if (seeds->selectStartSeed && (seeds->selectStartSeedIndex == i)) { 
      startSphere->GetProperty()->SetColor(START_SEED_HIGHLIGHT_COLOR);
    }
    localStorage->m_PropAssembly->AddPart(startSphere);

    int numEndSeeds = seeds->getNumEndSeeds(i);
    isStartSeed = false;

    for (int j = 0; j < numEndSeeds; j++){
      point = seeds->getEndSeed(i,j);
      auto endSphere = CreateSphere(point[0], point[1], point[2], m_seedRadius, isStartSeed);
      if (seeds->selectEndSeed && (seeds->selectEndSeedIndex == j)) { 
        endSphere->GetProperty()->SetColor(END_SEED_HIGHLIGHT_COLOR);
      }
      localStorage->m_PropAssembly->AddPart(endSphere);
    }
  }
*/

  localStorage->m_PropAssembly->VisibilityOn();
}

//-------------
// ResetMapper
//-------------
//
void sv4guiImageSeedMapper::ResetMapper(mitk::BaseRenderer* renderer)
{
  LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
  ls->m_PropAssembly->VisibilityOff();
}

//------------
// GetVtkProp
//------------
//
vtkProp * 
sv4guiImageSeedMapper::GetVtkProp(mitk::BaseRenderer* renderer)
{
  //std::cout << "===================== sv4guiImageSeedMapper::GetVtkProp =====================" << std::endl;
  ResetMapper(renderer);
  GenerateDataForRenderer(renderer);
  LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
  return ls->m_PropAssembly;
}

//-----------------
// createCubeActor
//-----------------
//
vtkSmartPointer<vtkActor> 
sv4guiImageSeedMapper::createCubeActor(double x1, double y1, double z1, double x2, double y2, double z2)
{
  auto cube = vtkSmartPointer<vtkCubeSource>::New();
  cube->SetCenter( (double(x1)+x2)/2, (double(y1)+y2)/2, (double(z1)+z2)/2);
  cube->SetBounds(std::min(x1,x2), std::max(x1,x2), std::min(y1,y2), std::max(y1,y2), std::min(z1,z2), std::max(z1,z2));

  auto mapper = vtkSmartPointer<vtkPolyDataMapper>::New();
  mapper->SetInputConnection(cube->GetOutputPort());

  auto actor = vtkSmartPointer<vtkActor>::New();
  actor->SetMapper(mapper);
  actor->GetProperty()->SetRepresentationToWireframe();

  return actor;
}
