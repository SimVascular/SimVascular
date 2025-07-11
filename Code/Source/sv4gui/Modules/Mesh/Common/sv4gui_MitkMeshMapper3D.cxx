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

#include "sv4gui_MitkMeshMapper3D.h"

#include <mitkDataNode.h>
#include <mitkProperties.h>
#include <mitkColorProperty.h>
#include <mitkLookupTableProperty.h>
#include <mitkVtkRepresentationProperty.h>
#include <mitkVtkInterpolationProperty.h>
#include <mitkVtkScalarModeProperty.h>
#include <mitkClippingProperty.h>
#include <mitkSmartPointerProperty.h>
//#include <mitkIShaderRepository.h>
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
#include <vtkExtractGeometry.h>
#include <vtkTexture.h>

const sv4guiMitkMesh* sv4guiMitkMeshMapper3D::GetInput()
{
    return static_cast<const sv4guiMitkMesh * > ( GetDataNode()->GetData() );
}

sv4guiMitkMeshMapper3D::sv4guiMitkMeshMapper3D()
{
}

sv4guiMitkMeshMapper3D::~sv4guiMitkMeshMapper3D()
{
}

void sv4guiMitkMeshMapper3D::GenerateDataForRenderer(mitk::BaseRenderer* renderer)
{
    mitk::DataNode* node = GetDataNode();
    if(node==nullptr)
        return;

    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
    bool visible = true;
    GetDataNode()->GetVisibility(visible, renderer, "visible");
    if(!visible)
    {
        ls->m_PropAssembly->VisibilityOff();
        return;
    }

    sv4guiMitkMesh* mitkMesh  = const_cast< sv4guiMitkMesh* >( this->GetInput() );
    if(mitkMesh==nullptr)
    {
        ls->m_PropAssembly->VisibilityOff();
        return;
    }

    int timestep=this->GetTimestep();

    sv4guiMesh* mesh=mitkMesh->GetMesh(timestep);
    if(mesh==nullptr)
    {
        ls->m_PropAssembly->VisibilityOff();
        return;
    }

    vtkSmartPointer<vtkPolyData> surfaceMesh=mesh->GetSurfaceMesh();
    if (surfaceMesh == nullptr)
    {
        ls->m_PropAssembly->VisibilityOff();
        return;
    }

    ls->m_PropAssembly->GetParts()->RemoveAllItems();

    float edgeColor[3]= { 0.0f, 0.0f, 1.0f };
    node->GetColor(edgeColor, renderer, "edge color");

    bool showEdges=false;
    node->GetBoolProperty("show edges", showEdges, renderer);

#if VTK_MAJOR_VERSION == 6
    vtkSmartPointer<vtkPainterPolyDataMapper> mapper = vtkSmartPointer<vtkPainterPolyDataMapper>::New();
#else
    vtkSmartPointer<vtkOpenGLPolyDataMapper> mapper = vtkSmartPointer<vtkOpenGLPolyDataMapper>::New();
#endif
    mapper->SetInputData(surfaceMesh);

    vtkSmartPointer<vtkActor> actor= vtkSmartPointer<vtkActor>::New();
    actor->SetMapper(mapper);

    Superclass::ApplyColorAndOpacityProperties( renderer, actor ) ;
    this->ApplyShaderProperties(renderer);
    ApplyAllProperties(node, renderer, mapper, actor, &m_LSH, true);

    if(showEdges)
    {
        actor->GetProperty()->SetEdgeColor(edgeColor[0], edgeColor[1], edgeColor[2]);
        actor->GetProperty()->SetEdgeVisibility(1);
        actor->GetProperty()->SetLineWidth(0.5);
    }
    ls->m_Actor=actor;

    ls->m_PropAssembly->AddPart(ls->m_Actor);

    if(visible)
        ls->m_PropAssembly->VisibilityOn();
}

void sv4guiMitkMeshMapper3D::ResetMapper( mitk::BaseRenderer* renderer )
{
    std::cout << "ResetMapper" << std::endl << std::flush;
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
    ls->m_PropAssembly->VisibilityOff();
}

void sv4guiMitkMeshMapper3D::ApplyMitkPropertiesToVtkProperty(mitk::DataNode *node, vtkProperty* property, mitk::BaseRenderer* renderer)
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

#if VTK_MAJOR_VERSION == 6
void sv4guiMitkMeshMapper3D::ApplyAllProperties(mitk::DataNode *node, mitk::BaseRenderer* renderer, vtkSmartPointer<vtkPainterPolyDataMapper> mapper, vtkSmartPointer<vtkActor> actor, mitk::LocalStorageHandler<LocalStorage>* handler, bool clipping)
#else
void sv4guiMitkMeshMapper3D::ApplyAllProperties(mitk::DataNode *node, mitk::BaseRenderer* renderer, vtkSmartPointer<vtkOpenGLPolyDataMapper> mapper, vtkSmartPointer<vtkActor> actor, mitk::LocalStorageHandler<LocalStorage>* handler, bool clipping)
#endif
{
    // Applying shading properties
//    Superclass::ApplyColorAndOpacityProperties( renderer, actor ) ;
//    this->ApplyShaderProperties(renderer);
    // VTK Properties
    ApplyMitkPropertiesToVtkProperty( node, actor->GetProperty(), renderer );

    mitk::TransferFunctionProperty::Pointer transferFuncProp;
    node->GetProperty(transferFuncProp, "Surface.TransferFunction", renderer);
    if (transferFuncProp.IsNotNull() )
    {
        mapper->SetLookupTable(transferFuncProp->GetValue()->GetColorTransferFunction());
    }

    mitk::LookupTableProperty::Pointer lookupTableProp;
    node->GetProperty(lookupTableProp, "LookupTable", renderer);
    if (lookupTableProp.IsNotNull() )
    {
        mapper->SetLookupTable(lookupTableProp->GetLookupTable()->GetVtkLookupTable());
    }

    mitk::LevelWindow levelWindow;
    if(node->GetLevelWindow(levelWindow, renderer, "levelWindow"))
    {
        mapper->SetScalarRange(levelWindow.GetLowerWindowBound(),levelWindow.GetUpperWindowBound());
    }
    else if(node->GetLevelWindow(levelWindow, renderer))
    {
        mapper->SetScalarRange(levelWindow.GetLowerWindowBound(),levelWindow.GetUpperWindowBound());
    }

    bool scalarVisibility = false;
    node->GetBoolProperty("scalar visibility", scalarVisibility);
    mapper->SetScalarVisibility( (scalarVisibility ? 1 : 0) );

    if(scalarVisibility)
    {
        mitk::VtkScalarModeProperty* scalarMode;
        if(node->GetProperty(scalarMode, "scalar mode", renderer))
            mapper->SetScalarMode(scalarMode->GetVtkScalarMode());
        else
            mapper->SetScalarModeToDefault();

        bool colorMode = false;
        node->GetBoolProperty("color mode", colorMode);
        mapper->SetColorMode( (colorMode ? 1 : 0) );

        double scalarsMin = 0;
        node->GetDoubleProperty("ScalarsRangeMinimum", scalarsMin, renderer);

        double scalarsMax = 1.0;
        node->GetDoubleProperty("ScalarsRangeMaximum", scalarsMax, renderer);

        mapper->SetScalarRange(scalarsMin,scalarsMax);
    }

    mitk::SmartPointerProperty::Pointer imagetextureProp =
            dynamic_cast< mitk::SmartPointerProperty * >(node->GetProperty("Surface.Texture", renderer));

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
        if(mapper->GetInput()->GetPointData()->GetTCoords() == nullptr)
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
    node->GetBoolProperty("deprecated useCellDataForColouring", deprecatedUseCellData);

    bool deprecatedUsePointData = false;
    node->GetBoolProperty("deprecated usePointDataForColouring", deprecatedUsePointData);

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
        if (dynamic_cast<mitk::FloatProperty *>(node->GetProperty("ScalarsRangeMinimum")) != nullptr)
            scalarsMin = dynamic_cast<mitk::FloatProperty*>(node->GetProperty("ScalarsRangeMinimum"))->GetValue();

        float scalarsMax = 0.1;
        if (dynamic_cast<mitk::FloatProperty *>(node->GetProperty("ScalarsRangeMaximum")) != nullptr)
            scalarsMax = dynamic_cast<mitk::FloatProperty*>(node->GetProperty("ScalarsRangeMaximum"))->GetValue();

        mapper->SetScalarRange(scalarsMin,scalarsMax);
        mapper->SetColorModeToMapScalars();
        mapper->ScalarVisibilityOn();
        actor->GetProperty()->SetSpecular (1);
        actor->GetProperty()->SetSpecularPower (50);
        actor->GetProperty()->SetInterpolationToPhong();
    }

    int deprecatedScalarMode = VTK_COLOR_MODE_DEFAULT;
    if(node->GetIntProperty("deprecated scalar mode", deprecatedScalarMode, renderer))
    {
        mapper->SetScalarMode(deprecatedScalarMode);
        mapper->ScalarVisibilityOn();
        actor->GetProperty()->SetSpecular (1);
        actor->GetProperty()->SetSpecularPower (50);
    }

    // Check whether one or more ClippingProperty objects have been defined for
    // this node. Check both renderer specific and global property lists, since
    // properties in both should be considered.
    const mitk::PropertyList::PropertyMap *rendererProperties = node->GetPropertyList( renderer )->GetMap();
    const mitk::PropertyList::PropertyMap *globalProperties = node->GetPropertyList( nullptr )->GetMap();

   if(clipping)
   {
       // Add clipping planes (if any)
       LocalStorage *ls = handler->GetLocalStorage(renderer);
       ls->m_ClippingPlaneCollection->RemoveAllItems();

       mitk::PropertyList::PropertyMap::const_iterator it;
       for ( it = rendererProperties->begin(); it != rendererProperties->end(); ++it )
       {
           CheckForClippingProperty( renderer,(*it).second.GetPointer(), handler);
       }

       for ( it = globalProperties->begin(); it != globalProperties->end(); ++it )
       {
           CheckForClippingProperty( renderer,(*it).second.GetPointer(), handler );
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
}

vtkProp *sv4guiMitkMeshMapper3D::GetVtkProp(mitk::BaseRenderer *renderer)
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
    return ls->m_PropAssembly;
}

void sv4guiMitkMeshMapper3D::CheckForClippingProperty( mitk::BaseRenderer* renderer, mitk::BaseProperty *property, mitk::LocalStorageHandler<LocalStorage>* handler )
{
    LocalStorage *ls = handler->GetLocalStorage(renderer);

    mitk::ClippingProperty *clippingProperty = dynamic_cast< mitk::ClippingProperty * >( property );

    if ( (clippingProperty != nullptr)
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

void sv4guiMitkMeshMapper3D::SetDefaultPropertiesForVtkProperty(mitk::DataNode* node, mitk::BaseRenderer* renderer, bool overwrite)
{
    // Shading
    {
        node->AddProperty( "material.wireframeLineWidth", mitk::FloatProperty::New(1.0f)          , renderer, overwrite );
        node->AddProperty( "material.pointSize"         , mitk::FloatProperty::New(1.0f)          , renderer, overwrite );

        node->AddProperty( "material.ambientCoefficient" , mitk::FloatProperty::New(0.05f)          , renderer, overwrite );
        node->AddProperty( "material.diffuseCoefficient" , mitk::FloatProperty::New(0.9f)          , renderer, overwrite );
        node->AddProperty( "material.specularCoefficient", mitk::FloatProperty::New(1.0f)          , renderer, overwrite );
        node->AddProperty( "material.specularPower"      , mitk::FloatProperty::New(16.0f)          , renderer, overwrite );

        mitk::VtkRepresentationProperty::Pointer rep=mitk::VtkRepresentationProperty ::New();
//        rep->SetRepresentationToWireframe();
        node->AddProperty( "material.representation"      , rep  , renderer, overwrite );
        node->AddProperty( "material.interpolation"       , mitk::VtkInterpolationProperty::New()   , renderer, overwrite );
    }

    //// Shaders
    //mitk::IShaderRepository* shaderRepo = mitk::CoreServices::GetShaderRepository();
    //if (shaderRepo)
    //{
    //    shaderRepo->AddDefaultProperties(node, renderer, overwrite);
    //}
}

void sv4guiMitkMeshMapper3D::SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer, bool overwrite)
{
    node->AddProperty( "color", mitk::ColorProperty::New(1.0f,1.0f,1.0f), renderer, overwrite );
    node->AddProperty( "opacity", mitk::FloatProperty::New(1.0), renderer, overwrite );

    node->AddProperty( "edge color", mitk::ColorProperty::New(0.0f,0.0f,1.0f), renderer, overwrite );
    node->AddProperty( "show edges", mitk::BoolProperty::New(true), renderer, overwrite );

    sv4guiMitkMeshMapper3D::SetDefaultPropertiesForVtkProperty(node,renderer,overwrite); // Shading

    node->AddProperty( "scalar visibility", mitk::BoolProperty::New(false), renderer, overwrite );
    node->AddProperty( "color mode", mitk::BoolProperty::New(false), renderer, overwrite );
    node->AddProperty( "scalar mode", mitk::VtkScalarModeProperty::New(), renderer, overwrite );

//    sv4guiMitkMesh* mitkMesh = dynamic_cast<sv4guiMitkMesh*>(node->GetData());

//    if(mitkMesh)
//    {
//        sv4guiMesh* mesh=mitkMesh->GetMesh();

//        if(mesh && (mesh->GetSurfaceMesh() != 0) && (mesh->GetSurfaceMesh()->GetPointData() != nullptr) && (mesh->GetSurfaceMesh()->GetPointData()->GetScalars() != 0))
//        {
            node->AddProperty( "scalar visibility", mitk::BoolProperty::New(true), renderer, overwrite );
            node->AddProperty( "color mode", mitk::BoolProperty::New(true), renderer, overwrite );
//        }
//    }

    // Backface culling
    node->AddProperty( "Backface Culling", mitk::BoolProperty::New(false), renderer, overwrite );

    Superclass::SetDefaultProperties(node, renderer, overwrite);
}
