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

#include "sv4gui_ModelVtkMapper2D.h"

//mitk includes
#include <mitkDataNode.h>
#include <mitkProperties.h>
#include "mitkVtkPropRenderer.h"
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

sv4guiModelVtkMapper2D::LocalStorage::LocalStorage()
{
    m_PropAssembly = vtkSmartPointer <vtkPropAssembly>::New();
}

sv4guiModelVtkMapper2D::LocalStorage::~LocalStorage()
{
}

const sv4guiModel* sv4guiModelVtkMapper2D::GetInput() const
{
    return static_cast<const sv4guiModel * > ( GetDataNode()->GetData() );
}

sv4guiModelVtkMapper2D::sv4guiModelVtkMapper2D()
{
}

sv4guiModelVtkMapper2D::~sv4guiModelVtkMapper2D()
{
}

void sv4guiModelVtkMapper2D::ResetMapper( mitk::BaseRenderer* renderer )
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
    ls->m_PropAssembly->VisibilityOff();
}

vtkProp* sv4guiModelVtkMapper2D::GetVtkProp(mitk::BaseRenderer * renderer)
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
    return ls->m_PropAssembly;
}

//Comment the code below to make sure to always update the renderwindows
//void sv4guiModelVtkMapper2D::Update(mitk::BaseRenderer* renderer)
//{
//    const mitk::DataNode* node = GetDataNode();
//    if( node == NULL )
//        return;

//    bool visible = true;
//    node->GetVisibility(visible, renderer, "visible");
//    if ( !visible )
//        return;

//    sv4guiModel* model  = static_cast<sv4guiModel*>( node->GetData() );
//    if ( model == NULL )
//        return;

//    this->CalculateTimeStep( renderer );

//    const mitk::TimeGeometry *dataTimeGeometry = model->GetTimeGeometry();
//    if ( ( dataTimeGeometry == NULL )
//         || ( dataTimeGeometry->CountTimeSteps() == 0 )
//         || ( !dataTimeGeometry->IsValidTimeStep( this->GetTimestep() ) ) )
//    {
//        return;
//    }

//    model->UpdateOutputInformation();
//    LocalStorage* localStorage = m_LSH.GetLocalStorage(renderer);

//    //check if something important has changed and we need to rerender
//    if ( (localStorage->m_LastUpdateTime < node->GetMTime()) //was the node modified?
//         || (localStorage->m_LastUpdateTime < model->GetPipelineMTime()) //Was the data modified?
//         || (localStorage->m_LastUpdateTime < renderer->GetCurrentWorldPlaneGeometryUpdateTime()) //was the geometry modified?
//         || (localStorage->m_LastUpdateTime < renderer->GetCurrentWorldPlaneGeometry()->GetMTime())
//         || (localStorage->m_LastUpdateTime < node->GetPropertyList()->GetMTime()) //was a property modified?
//         || (localStorage->m_LastUpdateTime < node->GetPropertyList(renderer)->GetMTime()) )
//    {
//        this->GenerateDataForRenderer( renderer );
//    }

//    localStorage->m_LastUpdateTime.Modified();
//}

void sv4guiModelVtkMapper2D::GenerateDataForRenderer( mitk::BaseRenderer *renderer )
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

    sv4guiModel* model  = static_cast<sv4guiModel *>( node->GetData() );
    if(model==NULL)
    {
        localStorage->m_PropAssembly->VisibilityOff();
        return;
    }

    int timestep=this->GetTimestep();

    sv4guiModelElement* me=model->GetModelElement(timestep);
    if(me==NULL)
    {
        localStorage->m_PropAssembly->VisibilityOff();
        return;
    }

    vtkSmartPointer<vtkPolyData> wholePolyData=me->GetWholeVtkPolyData();
    if ((wholePolyData == NULL) || (wholePolyData->GetNumberOfPoints() < 1))
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

    bool showWholeSurface=false;
    node->GetBoolProperty("show whole surface", showWholeSurface, renderer);

    bool showFaces=true;
    node->GetBoolProperty("show faces", showFaces, renderer);

    if(me->GetFaceNumber()==0)
        showWholeSurface=true;

    if(showWholeSurface)
        showFaces=false;

//    bool forceShowWholeSurface=false;
//    node->GetBoolProperty("show whole surface", forceShowWholeSurface, renderer);

//    bool showWholeSurface=false;
//    bool showFaces=false;

//    if(me->GetFaceNumber()>0)
//    {
//        showWholeSurface=false||forceShowWholeSurface;
//        showFaces=true;
//    }else{
//        showWholeSurface=true;
//        showFaces=false;
//    }

    int numProps=localStorage->m_PropAssembly->GetParts()->GetNumberOfItems();
    for(int i=0;i<numProps;i++)
    {
        vtkProp* prop= (vtkProp*)localStorage->m_PropAssembly->GetParts()->GetItemAsObject(i);
        localStorage->m_PropAssembly->RemovePart(prop);
    }

    if(showWholeSurface)
    {
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
        filter->SetInputData(wholePolyData);
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

    if(showFaces)
    {
        float selectedColor[3]= { 1.0f, 1.0f, 0.0f };
        node->GetColor(selectedColor, renderer, "face selected color");

        for(int i=0;i<me->GetFaces().size();i++)
        {
            sv4guiModelElement::svFace* face=me->GetFaces()[i];
            if(!face)
                continue;

            if(!face->visible)
                continue;

            vtkSmartPointer<vtkPolyData> facePolyData=face->vpd;
            if(!facePolyData)
                continue;

            vtkSmartPointer<vtkPlane> cuttingPlane = vtkSmartPointer<vtkPlane>::New();
            cuttingPlane->SetOrigin(origin);
            cuttingPlane->SetNormal(normal);

            vtkSmartPointer<vtkCutter> cutter = vtkSmartPointer<vtkCutter>::New();
            cutter->SetCutFunction(cuttingPlane);
            vtkSmartPointer<vtkLinearTransform> vtktransform = GetDataNode()->GetVtkTransform(this->GetTimestep());
            vtkSmartPointer<vtkTransformPolyDataFilter> filter = vtkSmartPointer<vtkTransformPolyDataFilter>::New();
            filter->SetTransform(vtktransform);
            filter->SetInputData(facePolyData);
            cutter->SetInputConnection(filter->GetOutputPort());
            cutter->Update();

            vtkSmartPointer<vtkPolyDataMapper> mapper = vtkSmartPointer<vtkPolyDataMapper>::New();
            mapper->ScalarVisibilityOff();
            mapper->SetInputConnection(cutter->GetOutputPort());
            ApplyMapperProperties(mapper, renderer);

            vtkSmartPointer<vtkActor> actor= vtkSmartPointer<vtkActor>::New();
            actor->SetMapper(mapper);
            if(face->selected){
                actor->GetProperty()->SetColor(selectedColor[0], selectedColor[1], selectedColor[2]);
            }else{
                actor->GetProperty()->SetColor(face->color[0], face->color[1], face->color[2]);
            }
            actor->GetProperty()->SetOpacity(face->opacity);
            actor->GetProperty()->SetLineWidth(lineWidth);
            actor->GetProperty()->SetLighting(0);

            localStorage->m_PropAssembly->AddPart(actor );
        }

    }

    if(visible)
        localStorage->m_PropAssembly->VisibilityOn();
}

void sv4guiModelVtkMapper2D::ApplyMapperProperties(vtkSmartPointer<vtkPolyDataMapper> mapper, mitk::BaseRenderer* renderer)
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

void sv4guiModelVtkMapper2D::SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer, bool overwrite)
{
    //  mitk::IPropertyAliases* aliases = mitk::CoreServices::GetPropertyAliases();
    node->AddProperty( "color", mitk::ColorProperty::New(1.0f,1.0f,1.0f), renderer, overwrite );
    node->AddProperty( "opacity", mitk::FloatProperty::New(1.0), renderer, overwrite );
    node->AddProperty( "show whole surface", mitk::BoolProperty::New(false), renderer, overwrite );
    node->AddProperty( "show faces", mitk::BoolProperty::New(true), renderer, overwrite );
    node->AddProperty( "face selected color",mitk::ColorProperty::New(1,1,0),renderer, overwrite );
    node->AddProperty( "line 2D width", mitk::FloatProperty::New(2.0f), renderer, overwrite );
    node->AddProperty( "scalar mode", mitk::VtkScalarModeProperty::New(), renderer, overwrite );
    node->AddProperty( "layer", mitk::IntProperty::New(100), renderer, overwrite);
    Superclass::SetDefaultProperties(node, renderer, overwrite);
}
