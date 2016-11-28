#include "svModelVtkMapper3D.h"

#include "svModelElementPolyData.h"

#include <mitkDataNode.h>
#include <mitkProperties.h>
#include <mitkColorProperty.h>
#include <mitkLookupTableProperty.h>
#include <mitkVtkRepresentationProperty.h>
#include <mitkVtkInterpolationProperty.h>
#include <mitkVtkScalarModeProperty.h>
#include <mitkClippingProperty.h>
#include <mitkSmartPointerProperty.h>
#include <mitkIShaderRepository.h>
#include <mitkExtractSliceFilter.h>
#include <mitkImageSliceSelector.h>
#include <mitkCoreServices.h>
#include <mitkTransferFunctionProperty.h>
#include <mitkIPropertyDescriptions.h>
#include <mitkIPropertyAliases.h>

#include <vtkActor.h>
#include <vtkProperty.h>
#include <vtkPolyData.h>
#include <vtkPolyDataMapper.h>
#include <vtkPointData.h>
#include <vtkPlaneCollection.h>
#include <vtkSmartPointer.h>
#include <vtkSelectionNode.h>
#include <vtkSelectionNode.h>
#include <vtkSelection.h>
#include <vtkExtractSelection.h>
#include <vtkUnstructuredGrid.h>
#include <vtkDataSetSurfaceFilter.h>
#include <vtkDataSetMapper.h>
#include <vtkIdTypeArray.h>

const svModel* svModelVtkMapper3D::GetInput()
{
    return static_cast<const svModel * > ( GetDataNode()->GetData() );
}

svModelVtkMapper3D::svModelVtkMapper3D()
{
}

svModelVtkMapper3D::~svModelVtkMapper3D()
{
}

void svModelVtkMapper3D::GenerateDataForRenderer(mitk::BaseRenderer* renderer)
{
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

    svModel* model  = const_cast< svModel* >( this->GetInput() );
    if(model==NULL)
    {
        ls->m_PropAssembly->VisibilityOff();
        return;
    }

    int timestep=this->GetTimestep();

    svModelElement* me=model->GetModelElement(timestep);
    if(me==NULL)
    {
        ls->m_PropAssembly->VisibilityOff();
        return;
    }

    vtkSmartPointer<vtkPolyData> wholePolyData=me->GetWholeVtkPolyData();
    if (wholePolyData == NULL)
    {
        ls->m_PropAssembly->VisibilityOff();
        return;
    }

    bool forceShowWholeSurface=false;
    node->GetBoolProperty("show whole surface", forceShowWholeSurface, renderer);

    bool showWholeSurface=false;
    bool showFaces=false;

    if(me->GetFaceNumber()>0)
    {
        showWholeSurface=false||forceShowWholeSurface;
        showFaces=true;
    }else{
        showWholeSurface=true;
        showFaces=false;
    }

    ls->m_PropAssembly->GetParts()->RemoveAllItems();

    float edgeColor[3]= { 0.0f, 0.0f, 1.0f };
    node->GetColor(edgeColor, renderer, "edge color");

    bool showEdges=false;
    node->GetBoolProperty("show edges", showEdges, renderer);

    vtkSmartPointer<vtkPainterPolyDataMapper> mapper = vtkSmartPointer<vtkPainterPolyDataMapper>::New();
    mapper->SetInputData(wholePolyData);

    vtkSmartPointer<vtkActor> actor= vtkSmartPointer<vtkActor>::New();
    actor->SetMapper(mapper);

    Superclass::ApplyColorAndOpacityProperties( renderer, actor ) ;
    ApplyAllProperties(renderer, mapper, actor);

    if(showEdges)
    {
        actor->GetProperty()->SetEdgeColor(edgeColor[0], edgeColor[1], edgeColor[2]);
        actor->GetProperty()->SetEdgeVisibility(1);
        actor->GetProperty()->SetLineWidth(0.5);
    }

    ls->m_Actor=actor;

    if(showWholeSurface)
    {
        ls->m_PropAssembly->AddPart(actor );
    }

    ls->m_FaceActors.clear();
    if(showFaces)
    {
        float selectedColor[3]= { 1.0f, 1.0f, 0.0f };
        node->GetColor(selectedColor, renderer, "face selected color");

        for(int i=0;i<me->GetFaces().size();i++)
        {
            svModelElement::svFace* face=me->GetFaces()[i];
            if(!face)
                continue;

            if(!face->visible)
                continue;

            vtkSmartPointer<vtkPolyData> facePolyData=face->vpd;
            if(!facePolyData)
                continue;

            vtkSmartPointer<vtkPainterPolyDataMapper> faceMapper = vtkSmartPointer<vtkPainterPolyDataMapper>::New();
            faceMapper->SetInputData(facePolyData);

            vtkSmartPointer<vtkActor> faceActor= vtkSmartPointer<vtkActor>::New();
            faceActor->SetMapper(faceMapper);

            ApplyAllProperties(renderer, faceMapper, faceActor);

            if(face->selected){
                faceActor->GetProperty()->SetColor(selectedColor[0], selectedColor[1], selectedColor[2]);
            }else{
                faceActor->GetProperty()->SetColor(face->color[0], face->color[1], face->color[2]);
            }
            faceActor->GetProperty()->SetOpacity(face->opacity);

            if(showEdges)
            {
                faceActor->GetProperty()->SetEdgeColor(edgeColor[0], edgeColor[1], edgeColor[2]);
                faceActor->GetProperty()->SetEdgeVisibility(1);
                faceActor->GetProperty()->SetLineWidth(0.51);
            }

            ls->m_PropAssembly->AddPart(faceActor );

            ls->m_FaceActors.push_back(faceActor);
        }

    }

    //show selected cells
    svModelElementPolyData* mepd=dynamic_cast<svModelElementPolyData*>(me);
    if(mepd&&mepd->GetSelectedCellIDs().size()>0)
    {
        float selectedColor[3]= { 0.0f, 1.0f, 0.0f };
        node->GetColor(selectedColor, renderer, "cell selected color");

        std::vector<int> cellIDs=mepd->GetSelectedCellIDs();

        vtkSmartPointer<vtkIdTypeArray> ids=vtkSmartPointer<vtkIdTypeArray>::New();
        ids->SetNumberOfComponents(1);
        for(int i=0;i<cellIDs.size();i++)
            ids->InsertNextValue(cellIDs[i]);

        vtkSmartPointer<vtkSelectionNode> selectionNode=vtkSmartPointer<vtkSelectionNode>::New();
        //Field Type 0 is CELL
        selectionNode->SetFieldType(0);
        //Content Type 4 is INDICES
        selectionNode->SetContentType(4);
        selectionNode->SetSelectionList(ids);

        vtkSmartPointer<vtkSelection> selection=vtkSmartPointer<vtkSelection>::New();
        selection->AddNode(selectionNode);

        vtkSmartPointer<vtkExtractSelection> extractSelection=vtkSmartPointer<vtkExtractSelection>::New();
        extractSelection->SetInputData(0, mepd->GetWholeVtkPolyData());
        extractSelection->SetInputData(1, selection);
        extractSelection->Update();

        vtkSmartPointer<vtkUnstructuredGrid> selected=vtkSmartPointer<vtkUnstructuredGrid>::New();
        selected->ShallowCopy(extractSelection->GetOutput());

//        vtkSmartPointer<vtkDataSetSurfaceFilter> surfaceFilter = vtkSmartPointer<vtkDataSetSurfaceFilter>::New();
//        surfaceFilter->SetInputData(selected);
//        surfaceFilter->Update();
//        vtkSmartPointer<vtkPolyData> cellPolydata = surfaceFilter->GetOutput();

//        vtkSmartPointer<vtkPainterPolyDataMapper> cellMapper = vtkSmartPointer<vtkPainterPolyDataMapper>::New();
//        cellMapper->SetInputData(cellPolydata);

//        vtkSmartPointer<vtkActor> cellActor= vtkSmartPointer<vtkActor>::New();
//        cellActor->SetMapper(cellMapper);

//        ApplyAllProperties(renderer, cellMapper, cellActor);
//        cellActor->GetProperty()->SetColor(selectedColor[0], selectedColor[1], selectedColor[2]);

        vtkSmartPointer<vtkDataSetMapper> cellMapper=vtkSmartPointer<vtkDataSetMapper>::New();
        cellMapper->SetInputData(selected);

        vtkSmartPointer<vtkActor> cellActor= vtkSmartPointer<vtkActor>::New();
        cellActor->SetMapper(cellMapper);
        cellActor->GetProperty()->SetColor(selectedColor[0], selectedColor[1], selectedColor[2]);
//        cellActor->GetProperty()->SetEdgeColor(selectedColor[0], selectedColor[1], selectedColor[2]);
//        cellActor->GetProperty()->SetEdgeVisibility(1);
//        cellActor->GetProperty()->SetLineWidth(3);

        ls->m_PropAssembly->AddPart(cellActor);
    }

    if(visible)
        ls->m_PropAssembly->VisibilityOn();
}

vtkSmartPointer<vtkActor> svModelVtkMapper3D::GetWholeSurfaceActor(mitk::BaseRenderer* renderer)
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);

    return ls->m_Actor;
}

std::vector<vtkSmartPointer<vtkActor>> svModelVtkMapper3D::GetFaceActors(mitk::BaseRenderer* renderer)
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);

    return ls->m_FaceActors;
}

void svModelVtkMapper3D::ResetMapper( mitk::BaseRenderer* renderer )
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
    ls->m_PropAssembly->VisibilityOff();
}

void svModelVtkMapper3D::ApplyMitkPropertiesToVtkProperty(mitk::DataNode *node, vtkProperty* property, mitk::BaseRenderer* renderer)
{
    // Backface culling
    {
        mitk::BoolProperty::Pointer p;
        node->GetProperty(p, "Backface Culling", renderer);
        bool useCulling = false;
        if(p.IsNotNull())
            useCulling = p->GetValue();
        property->SetBackfaceCulling(useCulling);
    }

    // Colors
    {
        double ambient [3] = { 0.5,0.5,0.0 };
        double diffuse [3] = { 0.5,0.5,0.0 };
        double specular[3] = { 1.0,1.0,1.0 };

        float coeff_ambient = 0.5f;
        float coeff_diffuse = 0.5f;
        float coeff_specular= 0.5f;
        float power_specular=10.0f;

        // Color
        {
            mitk::ColorProperty::Pointer p;
            node->GetProperty(p, "color", renderer);
            if(p.IsNotNull())
            {
                mitk::Color c = p->GetColor();
                ambient[0]=c.GetRed(); ambient[1]=c.GetGreen(); ambient[2]=c.GetBlue();
                diffuse[0]=c.GetRed(); diffuse[1]=c.GetGreen(); diffuse[2]=c.GetBlue();
                // Setting specular color to the same, make physically no real sense, however vtk rendering slows down, if these colors are different.
                specular[0]=c.GetRed(); specular[1]=c.GetGreen(); specular[2]=c.GetBlue();
            }
        }

        // Ambient
        {
            mitk::ColorProperty::Pointer p;
            node->GetProperty(p, "material.ambientColor", renderer);
            if(p.IsNotNull())
            {
                mitk::Color c = p->GetColor();
                ambient[0]=c.GetRed(); ambient[1]=c.GetGreen(); ambient[2]=c.GetBlue();
            }
        }

        // Diffuse
        {
            mitk::ColorProperty::Pointer p;
            node->GetProperty(p, "material.diffuseColor", renderer);
            if(p.IsNotNull())
            {
                mitk::Color c = p->GetColor();
                diffuse[0]=c.GetRed(); diffuse[1]=c.GetGreen(); diffuse[2]=c.GetBlue();
            }
        }

        // Specular
        {
            mitk::ColorProperty::Pointer p;
            node->GetProperty(p, "material.specularColor", renderer);
            if(p.IsNotNull())
            {
                mitk::Color c = p->GetColor();
                specular[0]=c.GetRed(); specular[1]=c.GetGreen(); specular[2]=c.GetBlue();
            }
        }

        // Ambient coeff
        {
            node->GetFloatProperty("material.ambientCoefficient", coeff_ambient, renderer);
        }

        // Diffuse coeff
        {
            node->GetFloatProperty("material.diffuseCoefficient", coeff_diffuse, renderer);
        }

        // Specular coeff
        {
            node->GetFloatProperty("material.specularCoefficient", coeff_specular, renderer);
        }

        // Specular power
        {
            node->GetFloatProperty("material.specularPower", power_specular, renderer);
        }

        property->SetAmbient( coeff_ambient );
        property->SetDiffuse( coeff_diffuse );
        property->SetSpecular( coeff_specular );
        property->SetSpecularPower( power_specular );

        property->SetAmbientColor( ambient );
        property->SetDiffuseColor( diffuse );
        property->SetSpecularColor( specular );
    }

    // Render mode
    {
//        // Opacity
//        {
//            float opacity = 1.0f;
//            if( node->GetOpacity(opacity,renderer) )
//                property->SetOpacity( opacity );
//        }

        // Wireframe line width
        {
            float lineWidth = 1;
            node->GetFloatProperty("material.wireframeLineWidth", lineWidth, renderer);
            property->SetLineWidth( lineWidth );
        }

        // Point size
        {
            float pointSize = 1.0f;
            node->GetFloatProperty("material.pointSize", pointSize, renderer);
            property->SetPointSize(pointSize);
        }

        // Representation
        {
            mitk::VtkRepresentationProperty::Pointer p;
            node->GetProperty(p, "material.representation", renderer);
            if(p.IsNotNull())
                property->SetRepresentation( p->GetVtkRepresentation() );
        }

        // Interpolation
        {
            mitk::VtkInterpolationProperty::Pointer p;
            node->GetProperty(p, "material.interpolation", renderer);
            if(p.IsNotNull())
                property->SetInterpolation( p->GetVtkInterpolation() );
        }
    }
}

void svModelVtkMapper3D::ApplyAllProperties(mitk::BaseRenderer* renderer, vtkSmartPointer<vtkPainterPolyDataMapper> mapper, vtkSmartPointer<vtkActor> actor)
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);

    // Applying shading properties
//    Superclass::ApplyColorAndOpacityProperties( renderer, actor ) ;
    this->ApplyShaderProperties(renderer);
    // VTK Properties
    ApplyMitkPropertiesToVtkProperty( this->GetDataNode(), actor->GetProperty(), renderer );

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

    mitk::SmartPointerProperty::Pointer imagetextureProp =
            dynamic_cast< mitk::SmartPointerProperty * >(GetDataNode()->GetProperty("Surface.Texture", renderer));

    if(imagetextureProp.IsNotNull())
    {
        mitk::Image* miktTexture = dynamic_cast< mitk::Image* >( imagetextureProp->GetSmartPointer().GetPointer() );
        vtkSmartPointer<vtkTexture> vtkTxture = vtkSmartPointer<vtkTexture>::New();
        //Either select the first slice of a volume
        if(miktTexture->GetDimension(2) > 1)
        {
            MITK_WARN << "3D Textures are not supported by VTK and MITK. The first slice of the volume will be used instead!";
            mitk::ImageSliceSelector::Pointer sliceselector = mitk::ImageSliceSelector::New();
            sliceselector->SetSliceNr(0);
            sliceselector->SetChannelNr(0);
            sliceselector->SetTimeNr(0);
            sliceselector->SetInput(miktTexture);
            sliceselector->Update();
            vtkTxture->SetInputData(sliceselector->GetOutput()->GetVtkImageData());
        }
        else //or just use the 2D image
        {
            vtkTxture->SetInputData(miktTexture->GetVtkImageData());
        }
        //pass the texture to the actor
        actor->SetTexture(vtkTxture);
        if(mapper->GetInput()->GetPointData()->GetTCoords() == NULL)
        {
            MITK_ERROR << "Surface.Texture property was set, but there are no texture coordinates. Please provide texture coordinates for the vtkPolyData via vtkPolyData->GetPointData()->SetTCoords().";
        }
        // if no texture is set, this will also remove a previously used texture
        // and reset the actor to it's default behaviour
    } else {
        actor->SetTexture(0);
    }

    // deprecated settings
    bool deprecatedUseCellData = false;
    this->GetDataNode()->GetBoolProperty("deprecated useCellDataForColouring", deprecatedUseCellData);

    bool deprecatedUsePointData = false;
    this->GetDataNode()->GetBoolProperty("deprecated usePointDataForColouring", deprecatedUsePointData);

    if (deprecatedUseCellData)
    {
        mapper->SetColorModeToDefault();
        mapper->SetScalarRange(0,255);
        mapper->ScalarVisibilityOn();
        mapper->SetScalarModeToUseCellData();
        actor->GetProperty()->SetSpecular (1);
        actor->GetProperty()->SetSpecularPower (50);
        actor->GetProperty()->SetInterpolationToPhong();
    }
    else if (deprecatedUsePointData)
    {
        float scalarsMin = 0;
        if (dynamic_cast<mitk::FloatProperty *>(this->GetDataNode()->GetProperty("ScalarsRangeMinimum")) != NULL)
            scalarsMin = dynamic_cast<mitk::FloatProperty*>(this->GetDataNode()->GetProperty("ScalarsRangeMinimum"))->GetValue();

        float scalarsMax = 0.1;
        if (dynamic_cast<mitk::FloatProperty *>(this->GetDataNode()->GetProperty("ScalarsRangeMaximum")) != NULL)
            scalarsMax = dynamic_cast<mitk::FloatProperty*>(this->GetDataNode()->GetProperty("ScalarsRangeMaximum"))->GetValue();

        mapper->SetScalarRange(scalarsMin,scalarsMax);
        mapper->SetColorModeToMapScalars();
        mapper->ScalarVisibilityOn();
        actor->GetProperty()->SetSpecular (1);
        actor->GetProperty()->SetSpecularPower (50);
        actor->GetProperty()->SetInterpolationToPhong();
    }

    int deprecatedScalarMode = VTK_COLOR_MODE_DEFAULT;
    if(this->GetDataNode()->GetIntProperty("deprecated scalar mode", deprecatedScalarMode, renderer))
    {
        mapper->SetScalarMode(deprecatedScalarMode);
        mapper->ScalarVisibilityOn();
        actor->GetProperty()->SetSpecular (1);
        actor->GetProperty()->SetSpecularPower (50);
    }

    // Check whether one or more ClippingProperty objects have been defined for
    // this node. Check both renderer specific and global property lists, since
    // properties in both should be considered.
    const mitk::PropertyList::PropertyMap *rendererProperties = this->GetDataNode()->GetPropertyList( renderer )->GetMap();
    const mitk::PropertyList::PropertyMap *globalProperties = this->GetDataNode()->GetPropertyList( NULL )->GetMap();

    // Add clipping planes (if any)
    ls->m_ClippingPlaneCollection->RemoveAllItems();

    mitk::PropertyList::PropertyMap::const_iterator it;
    for ( it = rendererProperties->begin(); it != rendererProperties->end(); ++it )
    {
        this->CheckForClippingProperty( renderer,(*it).second.GetPointer() );
    }

    for ( it = globalProperties->begin(); it != globalProperties->end(); ++it )
    {
        this->CheckForClippingProperty( renderer,(*it).second.GetPointer() );
    }

    if ( ls->m_ClippingPlaneCollection->GetNumberOfItems() > 0 )
    {
        mapper->SetClippingPlanes( ls->m_ClippingPlaneCollection );
    }
    else
    {
        mapper->RemoveAllClippingPlanes();
    }
}

vtkProp *svModelVtkMapper3D::GetVtkProp(mitk::BaseRenderer *renderer)
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
    return ls->m_PropAssembly;
}

void svModelVtkMapper3D::CheckForClippingProperty( mitk::BaseRenderer* renderer, mitk::BaseProperty *property )
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);

    mitk::ClippingProperty *clippingProperty = dynamic_cast< mitk::ClippingProperty * >( property );

    if ( (clippingProperty != NULL)
         && (clippingProperty->GetClippingEnabled()) )
    {
        const mitk::Point3D &origin = clippingProperty->GetOrigin();
        const mitk::Vector3D &normal = clippingProperty->GetNormal();

        vtkSmartPointer<vtkPlane> clippingPlane = vtkSmartPointer<vtkPlane>::New();
        clippingPlane->SetOrigin( origin[0], origin[1], origin[2] );
        clippingPlane->SetNormal( normal[0], normal[1], normal[2] );

        ls->m_ClippingPlaneCollection->AddItem( clippingPlane );
    }
}

void svModelVtkMapper3D::SetDefaultPropertiesForVtkProperty(mitk::DataNode* node, mitk::BaseRenderer* renderer, bool overwrite)
{
    // Shading
    {
        node->AddProperty( "material.wireframeLineWidth", mitk::FloatProperty::New(1.0f)          , renderer, overwrite );
        node->AddProperty( "material.pointSize"         , mitk::FloatProperty::New(1.0f)          , renderer, overwrite );

        node->AddProperty( "material.ambientCoefficient" , mitk::FloatProperty::New(0.05f)          , renderer, overwrite );
        node->AddProperty( "material.diffuseCoefficient" , mitk::FloatProperty::New(0.9f)          , renderer, overwrite );
        node->AddProperty( "material.specularCoefficient", mitk::FloatProperty::New(1.0f)          , renderer, overwrite );
        node->AddProperty( "material.specularPower"      , mitk::FloatProperty::New(16.0f)          , renderer, overwrite );

        node->AddProperty( "material.representation"      , mitk::VtkRepresentationProperty::New()  , renderer, overwrite );
        node->AddProperty( "material.interpolation"       , mitk::VtkInterpolationProperty::New()   , renderer, overwrite );
    }

    // Shaders
    mitk::IShaderRepository* shaderRepo = mitk::CoreServices::GetShaderRepository();
    if (shaderRepo)
    {
        shaderRepo->AddDefaultProperties(node, renderer, overwrite);
    }
}

void svModelVtkMapper3D::SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer, bool overwrite)
{
    node->AddProperty( "color", mitk::ColorProperty::New(1.0f,1.0f,1.0f), renderer, overwrite );
    node->AddProperty( "opacity", mitk::FloatProperty::New(1.0), renderer, overwrite );

    node->AddProperty( "edge color", mitk::ColorProperty::New(0.0f,0.0f,1.0f), renderer, overwrite );
    node->AddProperty( "show edges", mitk::BoolProperty::New(false), renderer, overwrite );

    node->AddProperty( "show whole surface", mitk::BoolProperty::New(false), renderer, overwrite );
//    node->AddProperty( "show faces", mitk::BoolProperty::New(true), renderer, overwrite );
    node->AddProperty( "face selected color",mitk::ColorProperty::New(1,1,0),renderer, overwrite );
    node->AddProperty( "cell selected color",mitk::ColorProperty::New(0,1,0),renderer, overwrite );

    svModelVtkMapper3D::SetDefaultPropertiesForVtkProperty(node,renderer,overwrite); // Shading

    node->AddProperty( "scalar visibility", mitk::BoolProperty::New(false), renderer, overwrite );
    node->AddProperty( "color mode", mitk::BoolProperty::New(false), renderer, overwrite );
    node->AddProperty( "scalar mode", mitk::VtkScalarModeProperty::New(), renderer, overwrite );

    svModel* model = dynamic_cast<svModel*>(node->GetData());

    if(model)
    {
        svModelElement* modelElement=model->GetModelElement();

        if(modelElement && (modelElement->GetWholeVtkPolyData() != 0) && (modelElement->GetWholeVtkPolyData()->GetPointData() != NULL) && (modelElement->GetWholeVtkPolyData()->GetPointData()->GetScalars() != 0))
        {
            node->AddProperty( "scalar visibility", mitk::BoolProperty::New(true), renderer, overwrite );
            node->AddProperty( "color mode", mitk::BoolProperty::New(true), renderer, overwrite );
        }
    }

    // Backface culling
    node->AddProperty( "Backface Culling", mitk::BoolProperty::New(false), renderer, overwrite );

    Superclass::SetDefaultProperties(node, renderer, overwrite);
}
