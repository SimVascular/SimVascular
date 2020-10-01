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

#include "sv4gui_ModelVtkMapper3D.h"

#include "sv4gui_ModelElementPolyData.h"

#include <mitkDataNode.h>
#include <mitkProperties.h>
#include <mitkColorProperty.h>
#include <mitkLookupTableProperty.h>
#include <mitkVtkRepresentationProperty.h>
#include <mitkVtkInterpolationProperty.h>
#include <mitkVtkScalarModeProperty.h>
#include <mitkClippingProperty.h>
#include <mitkSmartPointerProperty.h>
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

const sv4guiModel* sv4guiModelVtkMapper3D::GetInput()
{
    return static_cast<const sv4guiModel * > ( GetDataNode()->GetData() );
}

sv4guiModelVtkMapper3D::sv4guiModelVtkMapper3D()
{
}

sv4guiModelVtkMapper3D::~sv4guiModelVtkMapper3D()
{
}

//-------------------------
// GenerateDataForRenderer
//-------------------------
// Create vtk mappers and actors for each face in the model and
// set face colors, edge visibility, etc.
//
// This method is called when the graphics window is updated (e.g. the view is rotated).
// Mappers and actors are created only if face geometry is modified. The GetFaceMapperAndActor()
// function sets/gets a mapper and actor for each face using the face's polydata as a key
// into a map. When face geometry changes its polydata pointer changes and a new entry for that 
// face is added to the map.
//
// The ResetFaceMapperAndActor() function sets a reference flag for each face to false before
// the list of faces is processed. GetFaceMapperAndActor() sets the reference flag to true for
// each face it adds or retrieves. The UpdateFaceMapperAndActor() function then removes 
// map elements that are no longer used (i.e. the reference flag is false). 
//
void sv4guiModelVtkMapper3D::GenerateDataForRenderer(mitk::BaseRenderer* renderer)
{
    // std::cout << "========== sv4guiModelVtkMapper3D::GenerateDataForRenderer ==========" << std::endl;
    mitk::DataNode* node = GetDataNode();
    if (node == NULL) {
        return;
    }

    auto localStorage = m_LSH.GetLocalStorage(renderer);

    // Check if the geometry is visible (Data Manager check box).
    bool visible = true;
    GetDataNode()->GetVisibility(visible, renderer, "visible");
    if (!visible) {
        localStorage->m_PropAssembly->VisibilityOff();
        return;
    }

    // Get the model.
    auto model  = const_cast< sv4guiModel* >(this->GetInput());
    if (model == nullptr) {
        localStorage->m_PropAssembly->VisibilityOff();
        return;
    }

    // Get the model element which contains face data and vtk polydata.
    int timestep = this->GetTimestep();
    auto modelElem = model->GetModelElement(timestep);
    if (modelElem == nullptr) {
        localStorage->m_PropAssembly->VisibilityOff();
        return;
    }

    // Get the polydata for the entire model (all faces).
    //
    // [TODO:DaveP] I don't think 'wholePolyData'  is ever used. 
    //
    auto wholePolyData = modelElem->GetWholeVtkPolyData();
    if (wholePolyData == NULL) {
        localStorage->m_PropAssembly->VisibilityOff();
        return;
    }

    // Show a single surface or all of the faces with their
    // individual attributes (e.g. color).
    //
    bool showWholeSurface = false;
    node->GetBoolProperty("show whole surface", showWholeSurface, renderer);
    bool showFaces = true;
    node->GetBoolProperty("show faces", showFaces, renderer);

    if (modelElem->GetFaceNumber() == 0) {
        showWholeSurface = true;
    }

    if (showWholeSurface) {
        showFaces = false;
    }

    static int createMapsCount = 0;
    int numProps = localStorage->m_PropAssembly->GetParts()->GetNumberOfItems();
    createMapsCount += 1;
    for (int i = 0; i < numProps; i++) {
        vtkProp3D* prop = (vtkProp3D*)localStorage->m_PropAssembly->GetParts()->GetItemAsObject(i);
        localStorage->m_PropAssembly->RemovePart(prop);
    }

    float edgeColor[3]= { 0.0f, 0.0f, 1.0f };
    node->GetColor(edgeColor, renderer, "edge color");
    bool showEdges = false;
    node->GetBoolProperty("show edges", showEdges, renderer);

    // Create a mapper and actor for 'wholePolyData'.
    //
    // This is used for the 'Show/Full Faces' Model Data Node menu option. 
    //
    if (showWholeSurface) { 
        #if VTK_MAJOR_VERSION == 6
        vtkSmartPointer<vtkPainterPolyDataMapper> mapper = vtkSmartPointer<vtkPainterPolyDataMapper>::New();
        #else
        vtkSmartPointer<vtkOpenGLPolyDataMapper> mapper = vtkSmartPointer<vtkOpenGLPolyDataMapper>::New();
        #endif
        mapper->SetInputData(wholePolyData);

        vtkSmartPointer<vtkActor> actor= vtkSmartPointer<vtkActor>::New();
        actor->SetMapper(mapper);
        Superclass::ApplyColorAndOpacityProperties( renderer, actor ) ;
        ApplyAllProperties(renderer, mapper, actor);

        if (showEdges) {
            actor->GetProperty()->SetEdgeColor(edgeColor[0], edgeColor[1], edgeColor[2]);
            actor->GetProperty()->SetEdgeVisibility(1);
            actor->GetProperty()->SetLineWidth(0.5);
        }

        localStorage->m_Actor=actor;
        localStorage->m_PropAssembly->AddPart(actor);
    }

    // Display model faces.
    //
    localStorage->m_FaceActors.clear();

    if (showFaces) {
        float selectedColor[3] = { 1.0f, 1.0f, 0.0f };
        node->GetColor(selectedColor, renderer, "face selected color");
        // Set face mappers and actors to unreferenced.
        ResetFaceMapperAndActor();
        for (int i = 0; i < modelElem->GetFaces().size(); i++) {
            auto face = modelElem->GetFaces()[i];
            if (!face) {
                continue;
            }

            if (!face->visible) {
                continue;
            }

            auto facePolyData = face->vpd;
            if (!facePolyData) {
                continue;
            }

            // Get a vtk mapper and actor for the face.
            //
            #if VTK_MAJOR_VERSION == 6
            auto faceMapper = vtkSmartPointer<vtkPainterPolyDataMapper>::New();
            #else
            vtkOpenGLPolyDataMapper* faceMapper;
            #endif
            vtkActor* faceActor;
            GetFaceMapperAndActor(face, &faceMapper, &faceActor);
            faceMapper->SetInputData(facePolyData);
            faceMapper->SetScalarVisibility(false);
            faceActor->SetMapper(faceMapper);

            ApplyAllProperties(renderer, faceMapper, faceActor);

            if (face->selected) {
                faceActor->GetProperty()->SetColor(selectedColor[0], selectedColor[1], selectedColor[2]);
            } else {
                faceActor->GetProperty()->SetColor(face->color[0], face->color[1], face->color[2]);
            }
            faceActor->GetProperty()->SetOpacity(face->opacity);

            if (showEdges) {
                faceActor->GetProperty()->SetEdgeColor(edgeColor[0], edgeColor[1], edgeColor[2]);
                faceActor->GetProperty()->SetEdgeVisibility(1);
                faceActor->GetProperty()->SetLineWidth(0.51);
            } else {
                faceActor->GetProperty()->SetEdgeVisibility(0);
            }

            localStorage->m_PropAssembly->AddPart(faceActor);
            localStorage->m_FaceActors.push_back(faceActor);
        }

        // Remove unreferenced face mappers and actors.
        UpdateFaceMapperAndActor();
    }

    // Show selected cells.
    //
    auto mepd = dynamic_cast<sv4guiModelElementPolyData*>(modelElem);
    if (mepd && mepd->GetSelectedCellIDs().size()>0) {
        float selectedColor[3]= { 0.0f, 1.0f, 0.0f };
        node->GetColor(selectedColor, renderer, "cell selected color");
        std::vector<int> cellIDs = mepd->GetSelectedCellIDs();
        vtkSmartPointer<vtkIdTypeArray> ids = vtkSmartPointer<vtkIdTypeArray>::New();
        ids->SetNumberOfComponents(1);
        for (int i = 0; i < cellIDs.size(); i++) {
            ids->InsertNextValue(cellIDs[i]);
        }
        auto selectionNode = vtkSmartPointer<vtkSelectionNode>::New();
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

        vtkSmartPointer<vtkDataSetMapper> cellMapper=vtkSmartPointer<vtkDataSetMapper>::New();
        cellMapper->SetInputData(selected);

        vtkSmartPointer<vtkActor> cellActor= vtkSmartPointer<vtkActor>::New();
        cellActor->SetMapper(cellMapper);
        cellActor->GetProperty()->SetColor(selectedColor[0], selectedColor[1], selectedColor[2]);

        localStorage->m_PropAssembly->AddPart(cellActor);
    }

    if (visible) {
        localStorage->m_PropAssembly->VisibilityOn();
    }
}

//-----------------------
// GetFaceMapperAndActor
//-----------------------
// Get the vtk mapper and actor for the given face.
//
// First try to lookup the mapper and actor in the faceMapperActor[] map
// using the face vtk polydata. If it is not there then add them the faceMapperActor[].
//
// There does not seem to be a way to tell when face data has changed using mitk
// so use the face polydata. That works even when an undo is performed.
//
void sv4guiModelVtkMapper3D::GetFaceMapperAndActor(sv4guiModelElement::svFace* face, 
  vtkOpenGLPolyDataMapper** faceMapper, vtkActor** faceActor)
{
    vtkOpenGLPolyDataMapper* mapper; 
    vtkActor* actor;
    auto facePolyData = face->vpd;

    try {
        auto mapperActor = this->faceMapperActor.at(facePolyData);
        mapper = std::get<FaceMapperActorTupleIndex::MAPPER>(mapperActor);
        actor = std::get<FaceMapperActorTupleIndex::ACTOR>(mapperActor);
        std::get<FaceMapperActorTupleIndex::REF>(this->faceMapperActor[facePolyData]) = true;
    } catch (const std::out_of_range& except) {
        mapper = vtkOpenGLPolyDataMapper::New();
        actor = vtkActor::New();
        this->faceMapperActor[facePolyData] = std::make_tuple(mapper, actor, true);
    }

    *faceMapper = mapper;
    *faceActor = actor;
}

//-------------------------
// ResetFaceMapperAndActor
//-------------------------
// Reset the reference tuple element for each entry in the faceMapperActor[] 
// map to false. 
//
// This is used to determine if entries in faceMapperActor[] are no longer
// being used.
//
void sv4guiModelVtkMapper3D::ResetFaceMapperAndActor()
{
    for (auto& elem : this->faceMapperActor) {
        auto facePolyData = elem.first;
        auto mapperActor = elem.second;
        std::get<FaceMapperActorTupleIndex::REF>(this->faceMapperActor[elem.first]) = false;
    }
}

//--------------------------
// UpdateFaceMapperAndActor
//--------------------------
// Update the faceMapperActor[] map to remove entries that are 
// no longer being referenced. 
//
void sv4guiModelVtkMapper3D::UpdateFaceMapperAndActor()
{
    for (auto it = this->faceMapperActor.cbegin(); it != this->faceMapperActor.cend(); ) {
        auto facePolyData = it->first;
        auto mapperActor = it->second;
        bool ref = std::get<FaceMapperActorTupleIndex::REF>(mapperActor);
        if (!ref) {
            auto mapper = std::get<FaceMapperActorTupleIndex::MAPPER>(mapperActor);
            mapper->Delete();
            auto actor = std::get<FaceMapperActorTupleIndex::ACTOR>(mapperActor);
            actor->Delete();
            this->faceMapperActor.erase(it++); 
        } else {
            ++it;
        }
    }
}

vtkSmartPointer<vtkActor> sv4guiModelVtkMapper3D::GetWholeSurfaceActor(mitk::BaseRenderer* renderer)
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);

    return ls->m_Actor;
}

std::vector<vtkSmartPointer<vtkActor>> sv4guiModelVtkMapper3D::GetFaceActors(mitk::BaseRenderer* renderer)
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);

    return ls->m_FaceActors;
}

void sv4guiModelVtkMapper3D::ResetMapper( mitk::BaseRenderer* renderer )
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
    ls->m_PropAssembly->VisibilityOff();
}

//----------------------------------
// ApplyMitkPropertiesToVtkProperty
//----------------------------------
//
void sv4guiModelVtkMapper3D::ApplyMitkPropertiesToVtkProperty(mitk::DataNode *node, vtkProperty* property, 
  mitk::BaseRenderer* renderer)
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

//--------------------
// ApplyAllProperties
//--------------------
// [TODO:DaveP] what does this do?
//
#if VTK_MAJOR_VERSION == 6
void sv4guiModelVtkMapper3D::ApplyAllProperties(mitk::BaseRenderer* renderer, vtkSmartPointer<vtkPainterPolyDataMapper> mapper, vtkSmartPointer<vtkActor> actor)
#else
void sv4guiModelVtkMapper3D::ApplyAllProperties(mitk::BaseRenderer* renderer, vtkSmartPointer<vtkOpenGLPolyDataMapper> mapper, vtkSmartPointer<vtkActor> actor)
#endif
{
    //std::cout << "----- ApplyAllProperties -----" << std::endl;
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);

    // Applying shading properties
    this->ApplyShaderProperties(renderer);
    ApplyMitkPropertiesToVtkProperty(this->GetDataNode(), actor->GetProperty(), renderer);

    // [TODO:DaveP] I don't know how all of these properties are set, doesn't 
    // seem to affect the display except for mapper->SetScalarVisibility().
    //
    // I will not call this for now since it speeds up the interactive rotation
    // of the model.
    //
    // NOTE: If we don't call all of this code then mapper->SetScalarVisibility(false) 
    // must be called in GenerateDataForRenderer().
    //
    #define nApplyAllProperties_full
    #ifdef ApplyAllProperties_full
    mitk::TransferFunctionProperty::Pointer transferFuncProp;
    this->GetDataNode()->GetProperty(transferFuncProp, "Surface.TransferFunction", renderer);
    if (transferFuncProp.IsNotNull()) {
        mapper->SetLookupTable(transferFuncProp->GetValue()->GetColorTransferFunction());
    }

    mitk::LookupTableProperty::Pointer lookupTableProp;
    this->GetDataNode()->GetProperty(lookupTableProp, "LookupTable", renderer);
    if (lookupTableProp.IsNotNull()) {
        mapper->SetLookupTable(lookupTableProp->GetLookupTable()->GetVtkLookupTable());
    }

    mitk::LevelWindow levelWindow;
    if(this->GetDataNode()->GetLevelWindow(levelWindow, renderer, "levelWindow")) {
        mapper->SetScalarRange(levelWindow.GetLowerWindowBound(),levelWindow.GetUpperWindowBound());
    } else if(this->GetDataNode()->GetLevelWindow(levelWindow, renderer)) {
        mapper->SetScalarRange(levelWindow.GetLowerWindowBound(),levelWindow.GetUpperWindowBound());
    }

    bool scalarVisibility = false;
    this->GetDataNode()->GetBoolProperty("scalar visibility", scalarVisibility);
    mapper->SetScalarVisibility( (scalarVisibility ? 1 : 0) );

    if(scalarVisibility) {
        mitk::VtkScalarModeProperty* scalarMode;
        if(this->GetDataNode()->GetProperty(scalarMode, "scalar mode", renderer)) {
            mapper->SetScalarMode(scalarMode->GetVtkScalarMode());
        } else {
            mapper->SetScalarModeToDefault();
        }
  
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
    #endif
}

vtkProp *sv4guiModelVtkMapper3D::GetVtkProp(mitk::BaseRenderer *renderer)
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
    return ls->m_PropAssembly;
}

void sv4guiModelVtkMapper3D::CheckForClippingProperty( mitk::BaseRenderer* renderer, mitk::BaseProperty *property )
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

void sv4guiModelVtkMapper3D::SetDefaultPropertiesForVtkProperty(mitk::DataNode* node, mitk::BaseRenderer* renderer, bool overwrite)
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

    //// Shaders
    //mitk::IShaderRepository* shaderRepo = mitk::CoreServices::GetShaderRepository();
    //if (shaderRepo)
    //{
    //    shaderRepo->AddDefaultProperties(node, renderer, overwrite);
    //}
}

void sv4guiModelVtkMapper3D::SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer, bool overwrite)
{
    //std::cout << "----- SetDefaultProperties -----" << std::endl;
    node->AddProperty( "color", mitk::ColorProperty::New(1.0f,1.0f,1.0f), renderer, overwrite );
    node->AddProperty( "opacity", mitk::FloatProperty::New(1.0), renderer, overwrite );

    node->AddProperty( "edge color", mitk::ColorProperty::New(0.0f,0.0f,1.0f), renderer, overwrite );
    node->AddProperty( "show edges", mitk::BoolProperty::New(false), renderer, overwrite );

    node->AddProperty( "show whole surface", mitk::BoolProperty::New(false), renderer, overwrite );
    node->AddProperty( "show faces", mitk::BoolProperty::New(true), renderer, overwrite );
    node->AddProperty( "face selected color",mitk::ColorProperty::New(1,1,0),renderer, overwrite );
    node->AddProperty( "cell selected color",mitk::ColorProperty::New(0,1,0),renderer, overwrite );

    sv4guiModelVtkMapper3D::SetDefaultPropertiesForVtkProperty(node,renderer,overwrite); // Shading

    node->AddProperty( "scalar visibility", mitk::BoolProperty::New(false), renderer, overwrite );
    node->AddProperty( "color mode", mitk::BoolProperty::New(false), renderer, overwrite );
    node->AddProperty( "scalar mode", mitk::VtkScalarModeProperty::New(), renderer, overwrite );

    sv4guiModel* model = dynamic_cast<sv4guiModel*>(node->GetData());

    if(model) {
        sv4guiModelElement* modelElement=model->GetModelElement();

        if(modelElement && (modelElement->GetWholeVtkPolyData() != 0) && 
          (modelElement->GetWholeVtkPolyData()->GetPointData() != NULL) && 
          (modelElement->GetWholeVtkPolyData()->GetPointData()->GetScalars() != 0)) {
            node->AddProperty( "scalar visibility", mitk::BoolProperty::New(true), renderer, overwrite );
            node->AddProperty( "color mode", mitk::BoolProperty::New(true), renderer, overwrite );
            //std::cout << "[SetDefaultProperties] set scalar visibility to true " << std::endl;
        }
    }

    // Backface culling
    node->AddProperty( "Backface Culling", mitk::BoolProperty::New(false), renderer, overwrite );

    Superclass::SetDefaultProperties(node, renderer, overwrite);
}
