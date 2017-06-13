#include <svMitkSeg3DVtkMapper3D.h>

#include <vtkPoints.h>
#include <vtkCellArray.h>
#include <vtkProperty.h>
#include <vtkAppendPolyData.h>
#include <vtkSphereSource.h>

svMitkSeg3DVtkMapper3D::svMitkSeg3DVtkMapper3D()
    :m_AlreadyAddParts(false)
{
}

svMitkSeg3DVtkMapper3D::~svMitkSeg3DVtkMapper3D()
{
}

const svMitkSeg3D* svMitkSeg3DVtkMapper3D::GetInput()
{
    return static_cast< const svMitkSeg3D * >( GetDataNode()->GetData() );
}

vtkProp* svMitkSeg3DVtkMapper3D::GetVtkProp(mitk::BaseRenderer* renderer)
{
    return m_LSHandler.GetLocalStorage(renderer)->m_Assembly;
}

void svMitkSeg3DVtkMapper3D::GenerateDataForRenderer( mitk::BaseRenderer *renderer )
{
    mitk::DataNode* node = GetDataNode();
    if( node == NULL )
        return;

    svLocalStorage *ls = m_LSHandler.GetLocalStorage(renderer);

    if(!m_AlreadyAddParts)
    {
        LocalStorage *surfaceLS = m_LSH.GetLocalStorage(renderer);
        ls->m_Assembly->AddPart(surfaceLS->m_Actor);
        ls->m_Assembly->AddPart(ls->m_SelectedSeedActor);
        ls->m_Assembly->AddPart(ls->m_SeedActor);
        ls->m_Assembly->AddPart(ls->m_EndSeedActor);

        m_AlreadyAddParts=true;
    }

    Superclass::GenerateDataForRenderer(renderer);

    bool visible = true;
    node->GetVisibility(visible, renderer, "visible");

    bool showSeeds=false;
    node->GetBoolProperty("seed.visible", showSeeds, renderer);

    if(!visible || !showSeeds)
    {
        ls->m_SelectedSeedActor->VisibilityOff();
        ls->m_SeedActor->VisibilityOff();
        ls->m_EndSeedActor->VisibilityOff();
        return;
    }

    svMitkSeg3D::Pointer input=const_cast< svMitkSeg3D* >( this->GetInput() );
    svSeg3D* seg3D=input->GetSeg3D();

    if(seg3D==NULL)
    {
        ls->m_SelectedSeedActor->VisibilityOff();
        ls->m_SeedActor->VisibilityOff();
        ls->m_EndSeedActor->VisibilityOff();
        return;
    }

    svSeg3DParam param=seg3D->GetParam();
    std::map<int,svSeed> seedMap=param.GetSeedMap();

    if (seedMap.size()==0)
    {
        ls->m_SelectedSeedActor->VisibilityOff();
        ls->m_SeedActor->VisibilityOff();
        ls->m_EndSeedActor->VisibilityOff();
        return;
    }

    vtkSmartPointer<vtkAppendPolyData> selectedSeeds=vtkSmartPointer<vtkAppendPolyData>::New();
    vtkSmartPointer<vtkAppendPolyData> commonSeeds=vtkSmartPointer<vtkAppendPolyData>::New();
    vtkSmartPointer<vtkAppendPolyData> endSeeds=vtkSmartPointer<vtkAppendPolyData>::New();

    int NumberOfSelectedAdded=0;
    int NumberOfCommonAdded=0;
    int NumberOfEndAdded=0;

    for(auto s: seedMap)
    {
        svSeed seed=s.second;

        vtkSmartPointer<vtkSphereSource> sphere = vtkSmartPointer<vtkSphereSource>::New();
        sphere->SetRadius(seed.radius);
        sphere->SetCenter(seed.x,seed.y,seed.z);
        sphere->SetThetaResolution(16);
        sphere->SetPhiResolution(8);

        if (seed.selected)
        {
            selectedSeeds->AddInputConnection(sphere->GetOutputPort());
            ++NumberOfSelectedAdded;
        }
        else if(seed.type=="")
        {
            commonSeeds->AddInputConnection(sphere->GetOutputPort());
            ++NumberOfCommonAdded;
        }
        else
        {
            endSeeds->AddInputConnection(sphere->GetOutputPort());
            ++NumberOfEndAdded;
        }
    }

    if (NumberOfSelectedAdded > 0)
        ls->m_SelectedSeedVtkPolyDataMapper->SetInputConnection(selectedSeeds->GetOutputPort());

    if (NumberOfCommonAdded > 0)
        ls->m_SeedVtkPolyDataMapper->SetInputConnection(commonSeeds->GetOutputPort());

    if (NumberOfEndAdded > 0)
        ls->m_EndSeedVtkPolyDataMapper->SetInputConnection(endSeeds->GetOutputPort());

    double selectedSeedColor[3]={1,0,0};
    mitk::ColorProperty* colorprop = dynamic_cast<mitk::ColorProperty*>(node->GetProperty("seed.color.select", renderer));
    if(colorprop)
    {
        mitk::Color tmpColor=colorprop->GetValue();
        selectedSeedColor[0]=tmpColor[0];
        selectedSeedColor[1]=tmpColor[1];
        selectedSeedColor[2]=tmpColor[2];
    }

    double seedColor[3]={0,1,0};
    colorprop = dynamic_cast<mitk::ColorProperty*>(node->GetProperty("seed.color", renderer));
    if(colorprop)
    {
        mitk::Color tmpColor=colorprop->GetValue();
        seedColor[0]=tmpColor[0];
        seedColor[1]=tmpColor[1];
        seedColor[2]=tmpColor[2];
    }

    double endSeedColor[3]={0,0,1};
    colorprop = dynamic_cast<mitk::ColorProperty*>(node->GetProperty("seed.color.end", renderer));
    if(colorprop)
    {
        mitk::Color tmpColor=colorprop->GetValue();
        endSeedColor[0]=tmpColor[0];
        endSeedColor[1]=tmpColor[1];
        endSeedColor[2]=tmpColor[2];
    }

    ls->m_SelectedSeedActor->GetProperty()->SetColor(selectedSeedColor);
    ls->m_SeedActor->GetProperty()->SetColor(seedColor);
    ls->m_EndSeedActor->GetProperty()->SetColor(endSeedColor);

    if(visible)
    {
        if(NumberOfSelectedAdded)
            ls->m_SelectedSeedActor->VisibilityOn();
        else
            ls->m_SelectedSeedActor->VisibilityOff();

        if(NumberOfCommonAdded)
            ls->m_SeedActor->VisibilityOn();
        else
            ls->m_SeedActor->VisibilityOff();

        if(NumberOfEndAdded)
            ls->m_EndSeedActor->VisibilityOn();
        else
            ls->m_EndSeedActor->VisibilityOff();
    }

}

void svMitkSeg3DVtkMapper3D::ResetMapper( mitk::BaseRenderer* renderer )
{
    Superclass::ResetMapper(renderer);

    svLocalStorage *ls = m_LSHandler.GetLocalStorage(renderer);
    ls->m_SelectedSeedActor->VisibilityOff();
    ls->m_SeedActor->VisibilityOff();
    ls->m_EndSeedActor->VisibilityOff();
}

void svMitkSeg3DVtkMapper3D::SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer, bool overwrite)
{
    node->AddProperty( "seed.visible", mitk::BoolProperty::New(true), renderer, overwrite );

    node->AddProperty( "seed.color.select",mitk::ColorProperty::New(1,0,0),renderer, overwrite );
    node->AddProperty( "seed.color",mitk::ColorProperty::New(0,1,0),renderer, overwrite );
    node->AddProperty( "seed.color.end",mitk::ColorProperty::New(0,0,1),renderer, overwrite );

    Superclass::SetDefaultProperties(node, renderer, overwrite);
}
