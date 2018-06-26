#include "sv4gui_ImageSeedMapper.h"
#include "sv4gui_ImageSeedContainer.h"
#include "vtkPolyDataMapper.h"
#include "vtkSphereSource.h"
#include "vtkCubeSource.h"

sv4guiImageSeedMapper::sv4guiImageSeedMapper(){
}

sv4guiImageSeedMapper::~sv4guiImageSeedMapper(){

}

void sv4guiImageSeedMapper::GenerateDataForRenderer(mitk::BaseRenderer* renderer){
  //make ls propassembly
  mitk::DataNode* node = GetDataNode();
  if(node==NULL)
      return;

  LocalStorage *ls = m_LSH.GetLocalStorage(renderer);

  bool visible = true;
  GetDataNode()->GetVisibility(visible, renderer, "visible");
  if(!visible)
  {
      ls->m_PropAssembly->VisibilityOff();
      return;
  }

  sv4guiImageSeedContainer* seeds  =
    static_cast< sv4guiImageSeedContainer* >( GetDataNode()->GetData() );

  if(seeds==NULL)
  {
      ls->m_PropAssembly->VisibilityOff();
      return;
  }
  ls->m_PropAssembly->GetParts()->RemoveAllItems();

  auto hoverSphere = createSeedActor(seeds->hoverPoint[0],
    seeds->hoverPoint[1],
    seeds->hoverPoint[2], 2);

  ls->m_PropAssembly->AddPart(hoverSphere);

  int numStartSeeds = seeds->getNumStartSeeds();
  for (int i = 0; i < numStartSeeds; i++){
    auto v = seeds->getStartSeed(i);

    auto startSphere = createSeedActor(v[0],v[1],v[2],0);

    ls->m_PropAssembly->AddPart(startSphere);

    int numEndSeeds = seeds->getNumEndSeeds(i);
    for (int j = 0; j < numEndSeeds; j++){

      auto ve = seeds->getEndSeed(i,j);

      auto endSphere = createSeedActor(ve[0],ve[1],ve[2],1);
      ls->m_PropAssembly->AddPart(endSphere);

      if (m_box){
        auto box = createCubeActor(v[0],v[1],v[2], ve[0], ve[1], ve[2]);
        ls->m_PropAssembly->AddPart(box);
      }
    }
  }

  ls->m_PropAssembly->VisibilityOn();
}

void sv4guiImageSeedMapper::ResetMapper(mitk::BaseRenderer* renderer){
  LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
  ls->m_PropAssembly->VisibilityOff();
}

vtkProp* sv4guiImageSeedMapper::GetVtkProp(mitk::BaseRenderer* renderer){

  ResetMapper(renderer);
  GenerateDataForRenderer(renderer);

  LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
  return ls->m_PropAssembly;
}

vtkSmartPointer<vtkActor> sv4guiImageSeedMapper::createSeedActor(int x, int y, int z, int color){

  vtkSmartPointer<vtkSphereSource> sphere =
    vtkSmartPointer<vtkSphereSource>::New();

  sphere->SetRadius(m_seedRadius);
  sphere->SetCenter(x,y,z);

  vtkSmartPointer<vtkPolyDataMapper> sphereMapper =
    vtkSmartPointer<vtkPolyDataMapper>::New();
  sphereMapper->SetInputConnection(sphere->GetOutputPort());

  vtkSmartPointer<vtkActor> sphere1 = vtkSmartPointer<vtkActor>::New();
  sphere1->SetMapper(sphereMapper);
  if (color == 0){
    sphere1->GetProperty()->SetColor(1,0,0);
  }
  else if (color == 1) {
    sphere1->GetProperty()->SetColor(0,1,0);
  }
  else {
    sphere1->GetProperty()->SetColor(0,0,1);
  }

  sphere1->GetProperty()->SetAmbient(0.3);
  // sphere1->GetProperty()->SetDiffuse(0.0);
  // sphere1->GetProperty()->SetSpecular(0.0);
  // sphere1->GetProperty()->SetSpecularPower(5.0);

  return sphere1;
}

vtkSmartPointer<vtkActor> sv4guiImageSeedMapper::createCubeActor(int x1, int y1, int z1,
   int x2, int y2, int z2){

  vtkSmartPointer<vtkCubeSource> cube =
    vtkSmartPointer<vtkCubeSource>::New();

  cube->SetCenter( (double(x1)+x2)/2, (double(y1)+y2)/2, (double(z1)+z2)/2);
  cube->SetBounds(std::min(x1,x2), std::max(x1,x2),
    std::min(y1,y2), std::max(y1,y2),
    std::min(z1,z2), std::max(z1,z2));

  vtkSmartPointer<vtkPolyDataMapper> Mapper =
    vtkSmartPointer<vtkPolyDataMapper>::New();
  Mapper->SetInputConnection(cube->GetOutputPort());

  vtkSmartPointer<vtkActor> actor = vtkSmartPointer<vtkActor>::New();
  actor->SetMapper(Mapper);

  actor->GetProperty()->SetRepresentationToWireframe();

  return actor;
}
