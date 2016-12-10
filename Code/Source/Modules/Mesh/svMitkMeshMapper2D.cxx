#include "svMitkMeshMapper2D.h"
#include "svMitkMeshMapper3D.h"

#include "svPathElement.h"
#include "svSegmentationUtils.h"
#include "svMath3.h"

//mitk includes
#include <mitkDataNode.h>
#include <mitkProperties.h>
#include "mitkVtkPropRenderer.h"
#include <mitkSurface.h>
#include <mitkLookupTableProperty.h>
#include <mitkVtkScalarModeProperty.h>
#include <mitkTransferFunctionProperty.h>
#include <mitkVtkRepresentationProperty.h>

//vtk includes
#include <vtkActor.h>
#include <vtkCutter.h>
#include <vtkLookupTable.h>
#include <vtkPlane.h>
#include <vtkPolyData.h>
#include <vtkAssembly.h>
#include <vtkPointData.h>
#include <vtkTransformPolyDataFilter.h>
#include <vtkExtractGeometry.h>
#include <vtkDataSetMapper.h>
#include <vtkDataSetSurfaceFilter.h>
#include <vtkPainterPolyDataMapper.h>
#include <vtkPlanes.h>

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

    localStorage->m_PropAssembly->GetParts()->RemoveAllItems();

    double origin[3];
    origin[0] = planeGeometry->GetOrigin()[0];
    origin[1] = planeGeometry->GetOrigin()[1];
    origin[2] = planeGeometry->GetOrigin()[2];

    double normal[3];
    normal[0] = planeGeometry->GetNormal()[0];
    normal[1] = planeGeometry->GetNormal()[1];
    normal[2] = planeGeometry->GetNormal()[2];

    float color[3]= { 1.0f, 1.0f, 1.0f };
    node->GetColor(color, renderer, "color");
    float opacity = 1.0f;
    node->GetOpacity(opacity, renderer, "opacity");

    bool showContour=true;
    if(showContour && mesh->GetSurfaceMesh())
    {
        float lineWidth = 1.0f;
        node->GetFloatProperty("line 2D width", lineWidth, renderer);

        vtkSmartPointer<vtkPlane> cuttingPlane = vtkSmartPointer<vtkPlane>::New();
        cuttingPlane->SetOrigin(origin);
        cuttingPlane->SetNormal(normal);

        vtkSmartPointer<vtkCutter> cutter = vtkSmartPointer<vtkCutter>::New();
        cutter->SetCutFunction(cuttingPlane);
        vtkSmartPointer<vtkLinearTransform> vtktransform = GetDataNode()->GetVtkTransform(this->GetTimestep());
        vtkSmartPointer<vtkTransformPolyDataFilter> filter = vtkSmartPointer<vtkTransformPolyDataFilter>::New();
        filter->SetTransform(vtktransform);
        filter->SetInputData(mesh->GetSurfaceMesh());
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
    }

    bool showInnerMesh=false;
    node->GetBoolProperty("show inner", showInnerMesh, renderer);
    if(showInnerMesh && mesh->GetVolumeMesh())
    {
        bool showEdges=false;
        node->GetBoolProperty("show edges", showEdges, renderer);

        float edgeColor[3]= { 0.0f, 0.0f, 1.0f };
        node->GetColor(edgeColor, renderer, "edge color");

        vtkSmartPointer<vtkPlane> cuttingPlane = vtkSmartPointer<vtkPlane>::New();
        cuttingPlane->SetOrigin(origin);
        cuttingPlane->SetNormal(normal);

        vtkSmartPointer<vtkExtractGeometry> extracter = vtkSmartPointer<vtkExtractGeometry>::New();
        extracter->ExtractBoundaryCellsOn();
        extracter->SetImplicitFunction(cuttingPlane);
        extracter->SetInputData(mesh->GetVolumeMesh());
        extracter->Update();

        vtkSmartPointer<vtkUnstructuredGrid> extracted=extracter->GetOutput();

        cuttingPlane = vtkSmartPointer<vtkPlane>::New();
        cuttingPlane->SetOrigin(origin);
        double inverseNormal[3];
        inverseNormal[0] = -normal[0];
        inverseNormal[1] = -normal[1];
        inverseNormal[2] = -normal[2];
        cuttingPlane->SetNormal(inverseNormal);

        extracter = vtkSmartPointer<vtkExtractGeometry>::New();
        extracter->ExtractBoundaryCellsOn();
        extracter->SetImplicitFunction(cuttingPlane);
        extracter->SetInputData(extracted);
        extracter->Update();

//        svPathElement::svPathPoint pathPoint;
//        pathPoint.pos[0]=origin[0];
//        pathPoint.pos[1]=origin[1];
//        pathPoint.pos[2]=origin[2];
//        pathPoint.tangent[0]=normal[0];
//        pathPoint.tangent[1]=normal[1];
//        pathPoint.tangent[2]=normal[2];
//        pathPoint.tangent.Normalize();
//        pathPoint.rotation=svMath3::GetPerpendicularNormalVector(pathPoint.tangent);

//        vtkSmartPointer<vtkPlanes> planes=vtkSmartPointer<vtkPlanes>::New();
//        planes->SetBounds(-1000, 1000, -1000, 1000, -0.01, 0.01);
//        planes->SetTransform(svSegmentationUtils::GetvtkTransformBox(pathPoint,0.0));

//        vtkSmartPointer<vtkExtractGeometry> extracter = vtkSmartPointer<vtkExtractGeometry>::New();
//        extracter->ExtractBoundaryCellsOn();
//        extracter->SetImplicitFunction(planes);
//        extracter->SetInputData(mesh->GetVolumeMesh());
//        extracter->Update();

        extracted=extracter->GetOutput();

        //show grid
//        vtkSmartPointer<vtkDataSetMapper> meshMapper=vtkSmartPointer<vtkDataSetMapper>::New();
//        meshMapper->SetInputData(extracted);

//        vtkSmartPointer<vtkActor> meshActor= vtkSmartPointer<vtkActor>::New();
//        meshActor->SetMapper(meshMapper);
//        meshActor->GetProperty()->SetColor(color[0], color[1], color[2]);
//        meshActor->GetProperty()->SetOpacity(opacity);
//        meshActor->GetProperty()->SetEdgeColor(edgeColor[0], edgeColor[1], edgeColor[2]);
//        meshActor->GetProperty()->SetEdgeVisibility(1);
//        meshActor->GetProperty()->SetLineWidth(1);

        //show surface
        vtkSmartPointer<vtkDataSetSurfaceFilter> surfaceFilter = vtkSmartPointer<vtkDataSetSurfaceFilter>::New();
        surfaceFilter->SetInputData(extracted);
        surfaceFilter->Update();
        vtkSmartPointer<vtkPolyData> meshPolydata = surfaceFilter->GetOutput();

        vtkSmartPointer<vtkPainterPolyDataMapper> meshMapper = vtkSmartPointer<vtkPainterPolyDataMapper>::New();
        meshMapper->SetInputData(meshPolydata);

        vtkSmartPointer<vtkActor> meshActor= vtkSmartPointer<vtkActor>::New();
        meshActor->SetMapper(meshMapper);

        Superclass::ApplyColorAndOpacityProperties( renderer, meshActor ) ;
        this->ApplyShaderProperties(renderer);
        svMitkMeshMapper3D::ApplyAllProperties(node, renderer, meshMapper, meshActor, NULL, false);
        if(showEdges)
        {
            meshActor->GetProperty()->SetEdgeColor(edgeColor[0], edgeColor[1], edgeColor[2]);
            meshActor->GetProperty()->SetEdgeVisibility(1);
            meshActor->GetProperty()->SetLineWidth(1.0);
        }

        localStorage->m_PropAssembly->AddPart(meshActor );
    }

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
    node->AddProperty( "line 2D width", mitk::FloatProperty::New(2.0f), renderer, overwrite );
    node->AddProperty( "show inner", mitk::BoolProperty::New(false), renderer, overwrite );
    node->AddProperty( "edge color", mitk::ColorProperty::New(0.0f,0.0f,1.0f), renderer, overwrite );
    node->AddProperty( "scalar mode", mitk::VtkScalarModeProperty::New(), renderer, overwrite );
    node->AddProperty( "layer", mitk::IntProperty::New(100), renderer, overwrite);

    svMitkMeshMapper3D::SetDefaultPropertiesForVtkProperty(node,renderer,overwrite); // Shading

    node->AddProperty( "scalar visibility", mitk::BoolProperty::New(false), renderer, overwrite );
    node->AddProperty( "color mode", mitk::BoolProperty::New(false), renderer, overwrite );
    node->AddProperty( "scalar mode", mitk::VtkScalarModeProperty::New(), renderer, overwrite );

    Superclass::SetDefaultProperties(node, renderer, overwrite);
}
