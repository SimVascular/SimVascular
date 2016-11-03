
#include "svMitkMeshMapper2D.h"

//mitk includes
#include <mitkDataNode.h>
#include <mitkProperties.h>
#include "mitkVtkPropRenderer.h"
#include <mitkSurface.h>
#include <mitkLookupTableProperty.h>
#include <mitkVtkScalarModeProperty.h>
#include <mitkTransferFunctionProperty.h>

//vtk includes
#include <vtkActor.h>
#include <vtkCutter.h>
#include <vtkLookupTable.h>
#include <vtkPlane.h>
#include <vtkPolyData.h>
#include <vtkAssembly.h>
#include <vtkPointData.h>
#include <vtkTransformPolyDataFilter.h>

svMitkMeshMapper2D::LocalStorage::LocalStorage()
{
    m_PropAssembly = vtkSmartPointer <vtkPropAssembly>::New();
}

svMitkMeshMapper2D::LocalStorage::~LocalStorage()
{
}

const svMitkMesh* svMitkMeshMapper2D::GetInput() const
{
    return static_cast<const svMitkMesh * > ( GetDataNode()->GetData() );
}

svMitkMeshMapper2D::svMitkMeshMapper2D()
{
}

svMitkMeshMapper2D::~svMitkMeshMapper2D()
{
}

void svMitkMeshMapper2D::ResetMapper( mitk::BaseRenderer* renderer )
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
    ls->m_PropAssembly->VisibilityOff();
}

vtkProp* svMitkMeshMapper2D::GetVtkProp(mitk::BaseRenderer * renderer)
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
    return ls->m_PropAssembly;
}

void svMitkMeshMapper2D::GenerateDataForRenderer( mitk::BaseRenderer *renderer )
{
    mitk::DataNode* node = GetDataNode();
    if(node==NULL)
        return;

    LocalStorage* localStorage = m_LSH.GetLocalStorage(renderer);
    bool visible = true;
    node->GetVisibility(visible, renderer, "visible");
    if(!visible)
    {
        localStorage->m_PropAssembly->VisibilityOff();
        return;
    }

    svMitkMesh* mitkMesh  = static_cast<svMitkMesh *>( node->GetData() );
    if(mitkMesh==NULL)
    {
        localStorage->m_PropAssembly->VisibilityOff();
        return;
    }

    int timestep=this->GetTimestep();

    svMesh* mesh=mitkMesh->GetMesh(timestep);
    if(mesh==NULL)
    {
        localStorage->m_PropAssembly->VisibilityOff();
        return;
    }

    //Todo: cut volume mesh, instead of using surface mesh
    vtkSmartPointer<vtkPolyData> surfaceMesh=mesh->GetSurfaceMesh();
    if ((surfaceMesh == NULL) || (surfaceMesh->GetNumberOfPoints() < 1))
    {
        localStorage->m_PropAssembly->VisibilityOff();
        return;
    }

    const mitk::PlaneGeometry* planeGeometry = renderer->GetCurrentWorldPlaneGeometry();
    if( ( planeGeometry == NULL ) || ( !planeGeometry->IsValid() ) || ( !planeGeometry->HasReferenceGeometry() ))
    {
        localStorage->m_PropAssembly->VisibilityOff();
        return;
    }

    double origin[3];
    origin[0] = planeGeometry->GetOrigin()[0];
    origin[1] = planeGeometry->GetOrigin()[1];
    origin[2] = planeGeometry->GetOrigin()[2];

    double normal[3];
    normal[0] = planeGeometry->GetNormal()[0];
    normal[1] = planeGeometry->GetNormal()[1];
    normal[2] = planeGeometry->GetNormal()[2];

    float lineWidth = 1.0f;
    node->GetFloatProperty("line 2D width", lineWidth, renderer);

    localStorage->m_PropAssembly->GetParts()->RemoveAllItems();


    float color[3]= { 1.0f, 1.0f, 1.0f };
    node->GetColor(color, renderer, "color");
    float opacity = 1.0f;
    node->GetOpacity(opacity, renderer, "opacity");

    vtkSmartPointer<vtkPlane> cuttingPlane = vtkSmartPointer<vtkPlane>::New();
    cuttingPlane->SetOrigin(origin);
    cuttingPlane->SetNormal(normal);

    vtkSmartPointer<vtkCutter> cutter = vtkSmartPointer<vtkCutter>::New();
    cutter->SetCutFunction(cuttingPlane);
    vtkSmartPointer<vtkLinearTransform> vtktransform = GetDataNode()->GetVtkTransform(this->GetTimestep());
    vtkSmartPointer<vtkTransformPolyDataFilter> filter = vtkSmartPointer<vtkTransformPolyDataFilter>::New();
    filter->SetTransform(vtktransform);
    filter->SetInputData(surfaceMesh);
    cutter->SetInputConnection(filter->GetOutputPort());
    cutter->Update();

    vtkSmartPointer<vtkPolyDataMapper> mapper = vtkSmartPointer<vtkPolyDataMapper>::New();
    mapper->ScalarVisibilityOff();
    mapper->SetInputConnection(cutter->GetOutputPort());
    ApplyMapperProperties(mapper, renderer);

    vtkSmartPointer<vtkActor> actor= vtkSmartPointer<vtkActor>::New();
    actor->SetMapper(mapper);
    actor->GetProperty()->SetColor(color[0], color[1], color[2]);
    actor->GetProperty()->SetOpacity(opacity);
    actor->GetProperty()->SetLineWidth(lineWidth);
    actor->GetProperty()->SetLighting(0);

    localStorage->m_PropAssembly->AddPart(actor );

    if(visible)
        localStorage->m_PropAssembly->VisibilityOn();
}

void svMitkMeshMapper2D::ApplyMapperProperties(vtkSmartPointer<vtkPolyDataMapper> mapper, mitk::BaseRenderer* renderer)
{
    const mitk::DataNode * node = GetDataNode();

    if(node == NULL)
    {
        return;
    }
    // same block for scalar data rendering as in 3D mapper
    mitk::TransferFunctionProperty::Pointer transferFuncProp;
    this->GetDataNode()->GetProperty(transferFuncProp, "Surface.TransferFunction", renderer);
    if (transferFuncProp.IsNotNull() )
    {
        mapper->SetLookupTable(transferFuncProp->GetValue()->GetColorTransferFunction());
    }

    mitk::LookupTableProperty::Pointer lookupTableProp;
    this->GetDataNode()->GetProperty(lookupTableProp, "LookupTable", renderer);
    if (lookupTableProp.IsNotNull() )
    {
        mapper->SetLookupTable(lookupTableProp->GetLookupTable()->GetVtkLookupTable());
    }

    mitk::LevelWindow levelWindow;
    if(this->GetDataNode()->GetLevelWindow(levelWindow, renderer, "levelWindow"))
    {
        mapper->SetScalarRange(levelWindow.GetLowerWindowBound(),levelWindow.GetUpperWindowBound());
    }
    else if(this->GetDataNode()->GetLevelWindow(levelWindow, renderer))
    {
        mapper->SetScalarRange(levelWindow.GetLowerWindowBound(),levelWindow.GetUpperWindowBound());
    }

    bool scalarVisibility = false;
    this->GetDataNode()->GetBoolProperty("scalar visibility", scalarVisibility);
    mapper->SetScalarVisibility( (scalarVisibility ? 1 : 0) );

    if(scalarVisibility)
    {
        mitk::VtkScalarModeProperty* scalarMode;
        if(this->GetDataNode()->GetProperty(scalarMode, "scalar mode", renderer))
            mapper->SetScalarMode(scalarMode->GetVtkScalarMode());
        else
            mapper->SetScalarModeToDefault();

        bool colorMode = false;
        this->GetDataNode()->GetBoolProperty("color mode", colorMode);
        mapper->SetColorMode( (colorMode ? 1 : 0) );

        double scalarsMin = 0;
        this->GetDataNode()->GetDoubleProperty("ScalarsRangeMinimum", scalarsMin, renderer);

        double scalarsMax = 1.0;
        this->GetDataNode()->GetDoubleProperty("ScalarsRangeMaximum", scalarsMax, renderer);

        mapper->SetScalarRange(scalarsMin,scalarsMax);
    }
}

void svMitkMeshMapper2D::SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer, bool overwrite)
{
    node->AddProperty( "color", mitk::ColorProperty::New(1.0f,1.0f,1.0f), renderer, overwrite );
    node->AddProperty( "opacity", mitk::FloatProperty::New(1.0), renderer, overwrite );
    node->AddProperty( "line 2D width", mitk::FloatProperty::New(1.0f), renderer, overwrite );
    node->AddProperty( "scalar mode", mitk::VtkScalarModeProperty::New(), renderer, overwrite );
    node->AddProperty( "layer", mitk::IntProperty::New(100), renderer, overwrite);
    Superclass::SetDefaultProperties(node, renderer, overwrite);
}
