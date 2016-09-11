#include "svContourModelVtkMapper2D.h"

#include "svContourPolygon.h"

//mitk includes
#include "mitkVtkPropRenderer.h"
#include <mitkDataNode.h>
#include <mitkProperties.h>
#include <mitkPlaneGeometry.h>
#include <mitkLine.h>
#include <mitkGeometryTransformHolder.h>

//vtk includes
#include <vtkActor.h>
#include <vtkPropAssembly.h>
#include <vtkPolyDataMapper.h>
#include <vtkTransform.h>
#include <vtkGlyph3D.h>
#include <vtkTransformFilter.h>
#include <vtkLine.h>
#include <vtkGlyphSource2D.h>
#include <vtkFloatArray.h>
#include <vtkPointData.h>
#include <vtkCellArray.h>

#include <stdlib.h>

svContourModelVtkMapper2D::LocalStorage::LocalStorage()
{
    // points
    m_UnselectedPoints = vtkSmartPointer<vtkPoints>::New();
    m_SelectedPoints = vtkSmartPointer<vtkPoints>::New();
//    m_ContourPoints = vtkSmartPointer<vtkPoints>::New();

    // scales
    m_UnselectedScales = vtkSmartPointer<vtkFloatArray>::New();
    m_SelectedScales = vtkSmartPointer<vtkFloatArray>::New();

    // lines
//    m_ContourLines = vtkSmartPointer<vtkCellArray>::New();

    // glyph source (provides the different shapes)
    m_UnselectedGlyphSource2D = vtkSmartPointer<vtkGlyphSource2D>::New();
    m_SelectedGlyphSource2D = vtkSmartPointer<vtkGlyphSource2D>::New();

    // glyphs
    m_UnselectedGlyph3D = vtkSmartPointer<vtkGlyph3D>::New();
    m_SelectedGlyph3D = vtkSmartPointer<vtkGlyph3D>::New();

    // polydata
    m_VtkUnselectedPointsPolyData = vtkSmartPointer<vtkPolyData>::New();
    m_VtkSelectedPointsPolyData = vtkSmartPointer <vtkPolyData>::New();
    m_VtkContourPolyData = vtkSmartPointer<vtkPolyData>::New();

    // actors
    m_UnselectedActor = vtkSmartPointer <vtkActor>::New();
    m_SelectedActor = vtkSmartPointer <vtkActor>::New();
    m_ContourActor = vtkSmartPointer <vtkActor>::New();

    // mappers
    m_VtkUnselectedPolyDataMapper = vtkSmartPointer<vtkPolyDataMapper>::New();
    m_VtkSelectedPolyDataMapper = vtkSmartPointer<vtkPolyDataMapper>::New();
    m_VtkContourPolyDataMapper = vtkSmartPointer<vtkPolyDataMapper>::New();

    // propassembly
    m_PropAssembly = vtkSmartPointer <vtkPropAssembly>::New();
}

svContourModelVtkMapper2D::LocalStorage::~LocalStorage()
{
}

svContourModelVtkMapper2D::svContourModelVtkMapper2D()
    : m_Contour(nullptr),
      m_ShowPoints(true),
      m_LineWidth(2),
      m_PointLineWidth(1),
      m_Point2DSize(80),
      m_DistanceToPlane(4.0f)
{
}

svContourModelVtkMapper2D::~svContourModelVtkMapper2D()
{
}

void svContourModelVtkMapper2D::ReleaseGraphicsResources(mitk::BaseRenderer* renderer)
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
    ls->m_PropAssembly->ReleaseGraphicsResources(renderer->GetRenderWindow());

    ls->m_UnselectedActor->ReleaseGraphicsResources(renderer->GetRenderWindow());
    ls->m_SelectedActor->ReleaseGraphicsResources(renderer->GetRenderWindow());
    ls->m_ContourActor->ReleaseGraphicsResources(renderer->GetRenderWindow());

}

void svContourModelVtkMapper2D::ResetMapper( mitk::BaseRenderer* renderer )
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
    ls->m_PropAssembly->VisibilityOff();
}

// returns propassembly
vtkProp* svContourModelVtkMapper2D::GetVtkProp(mitk::BaseRenderer * renderer)
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
    return ls->m_PropAssembly;
}

void svContourModelVtkMapper2D::FindContourOnCurrentSlice(mitk::BaseRenderer* renderer, unsigned int t)
{
    svContourModel* input  = static_cast<svContourModel*>(GetDataNode()->GetData());

    // only update the input data, if the property tells us to
    bool update = true;
    this->GetDataNode()->GetBoolProperty("updateDataOnRender", update);
    if (update)
    {
        input->Update();
    }

    svContour* contour=input->GetContour(t);
    const mitk::PlaneGeometry *rendererPlaneGeometry = renderer->GetCurrentWorldPlaneGeometry();
    if(contour!=NULL && rendererPlaneGeometry!=NULL && contour->IsOnPlane(rendererPlaneGeometry,1.0))
    {
        m_Contour=contour;
    }
    else
    {
        m_Contour=NULL;
    }
}

void svContourModelVtkMapper2D::CreateVTKRenderObjects(mitk::BaseRenderer* renderer)
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);

    // initialize polydata here, otherwise we have update problems when
    // executing this function again
    ls->m_VtkUnselectedPointsPolyData = vtkSmartPointer<vtkPolyData>::New();
    ls->m_VtkSelectedPointsPolyData = vtkSmartPointer <vtkPolyData>::New();
    ls->m_VtkContourPolyData = vtkSmartPointer<vtkPolyData>::New();

    if(ls->m_PropAssembly->GetParts()->IsItemPresent(ls->m_UnselectedActor))
        ls->m_PropAssembly->RemovePart(ls->m_UnselectedActor);
    if(ls->m_PropAssembly->GetParts()->IsItemPresent(ls->m_SelectedActor))
        ls->m_PropAssembly->RemovePart(ls->m_SelectedActor);
    if(ls->m_PropAssembly->GetParts()->IsItemPresent(ls->m_ContourActor))
        ls->m_PropAssembly->RemovePart(ls->m_ContourActor);

    int t = this->GetTimestep();

    //Search one available contour for this renderer plane
    const mitk::PlaneGeometry *rendererPlaneGeometry = renderer->GetCurrentWorldPlaneGeometry();
    if(rendererPlaneGeometry==NULL)
        return;

    float point_size = m_Point2DSize*renderer->GetScaleFactorMMPerDisplayUnit();

//    m_Contour=input->GetContourOnPlane(rendererPlaneGeometry,1.0,t);
    FindContourOnCurrentSlice(renderer,t);

    if(m_Contour==NULL){
        ls->m_PropAssembly->VisibilityOff();
        return;
    }

    ls->m_PropAssembly->VisibilityOn();

    // empty point sets, cellarrays, scalars
    ls->m_UnselectedPoints->Reset();
    ls->m_SelectedPoints->Reset();

//    ls->m_ContourPoints->Reset();
//    ls->m_ContourLines->Rest();

    ls->m_UnselectedScales->Reset();
    ls->m_SelectedScales->Reset();

    ls->m_UnselectedScales->SetNumberOfComponents(3);
    ls->m_SelectedScales->SetNumberOfComponents(3);

    //----------------Control Points----------------------------
    int controlPointNumber=m_Contour->GetControlPointNumber();
    int selectedIndex=m_Contour->GetControlPointSelectedIndex();
    for(int i=0; i<controlPointNumber;i++)
    {
        if(i==0||i==1)
        {
            if(m_Contour->GetType()=="Contour"||dynamic_cast<svContourPolygon*>(m_Contour))
            {
                if(!m_Contour->IsClosed()||!m_Contour->IsFinished())
                {
                    continue;
                }
            }
            else if(m_Contour->GetType()=="Ellipse"&&i==1)
            {
                if(!m_Contour->IsClosed()||!m_Contour->IsFinished())
                {
                    continue;
                }
            }
        }

        mitk::Point3D point=m_Contour->GetControlPoint(i);

        if(i==selectedIndex)
        {
            ls->m_SelectedPoints->InsertNextPoint(point[0],point[1],point[2]);
            ls->m_SelectedScales->InsertNextTuple3(point_size,0,0);
        }else{
            ls->m_UnselectedPoints->InsertNextPoint(point[0],point[1],point[2]);
            ls->m_UnselectedScales->InsertNextTuple3(point_size,0,0);
        }
    }

    if(m_Contour->IsPreviewControlPointVisible())
    {
        mitk::Point3D point=m_Contour->GetPreviewControlPoint();
        ls->m_UnselectedPoints->InsertNextPoint(point[0],point[1],point[2]);
        ls->m_UnselectedScales->InsertNextTuple3(point_size,0,0);
    }

    //--------------Contour Lines-----------------------------

    if(m_Contour->GetContourPointNumber()>0)
    {
        ls->m_VtkContourPolyData=m_Contour->CreateVtkPolyDataFromContour();

        ls->m_VtkContourPolyDataMapper->SetInputData(ls->m_VtkContourPolyData);
        ls->m_ContourActor->SetMapper(ls->m_VtkContourPolyDataMapper);
        ls->m_ContourActor->GetProperty()->SetLineWidth(m_LineWidth);

        ls->m_PropAssembly->AddPart(ls->m_ContourActor);
    }

    // the point set must be transformed in order to obtain the appropriate glyph orientation
    // according to the current view
    vtkSmartPointer<vtkTransform> transform = vtkSmartPointer<vtkTransform>::New();
//    vtkSmartPointer<vtkMatrix4x4> a,b = vtkSmartPointer<vtkMatrix4x4>::New();

    const mitk::PlaneGeometry* geo2D = renderer->GetCurrentWorldPlaneGeometry();

    mitk::PlaneGeometry::Pointer pp1 = geo2D->Clone();
    mitk::Vector3D spacing1=pp1->GetSpacing();

    spacing1.Fill((spacing1[0]+spacing1[1]+spacing1[2])/3.0);
    pp1->SetSpacing(spacing1);
    mitk::Point3D org1;
    org1.Fill(0);
    pp1->SetOrigin(org1);
    transform->SetMatrix(pp1->GetVtkMatrix());

    //---- UNSELECTED POINTS  -----//

    // apply properties to glyph
    ls->m_UnselectedGlyphSource2D->SetGlyphTypeToSquare();
    ls->m_UnselectedGlyphSource2D->FilledOff();

    // apply transform
    vtkSmartPointer<vtkTransformFilter> transformFilterU = vtkSmartPointer<vtkTransformFilter>::New();
    transformFilterU->SetInputConnection(ls->m_UnselectedGlyphSource2D->GetOutputPort());
    transformFilterU->SetTransform(transform);

    ls->m_VtkUnselectedPointsPolyData->SetPoints(ls->m_UnselectedPoints);
    ls->m_VtkUnselectedPointsPolyData->GetPointData()->SetVectors(ls->m_UnselectedScales);

    // apply transform of current plane to glyphs
    ls->m_UnselectedGlyph3D->SetSourceConnection(transformFilterU->GetOutputPort());
    ls->m_UnselectedGlyph3D->SetInputData(ls->m_VtkUnselectedPointsPolyData);
    ls->m_UnselectedGlyph3D->SetScaleModeToScaleByVector();
    ls->m_UnselectedGlyph3D->SetVectorModeToUseVector();

    ls->m_VtkUnselectedPolyDataMapper->SetInputConnection(ls->m_UnselectedGlyph3D->GetOutputPort());
    ls->m_UnselectedActor->SetMapper(ls->m_VtkUnselectedPolyDataMapper);
    ls->m_UnselectedActor->GetProperty()->SetLineWidth(m_PointLineWidth);

    ls->m_PropAssembly->AddPart(ls->m_UnselectedActor);

    //---- SELECTED POINTS  -----//

    ls->m_SelectedGlyphSource2D->SetGlyphTypeToSquare();
    ls->m_SelectedGlyphSource2D->FilledOff();

    // apply transform
    vtkSmartPointer<vtkTransformFilter> transformFilterS = vtkSmartPointer<vtkTransformFilter>::New();
    transformFilterS->SetInputConnection(ls->m_SelectedGlyphSource2D->GetOutputPort());
    transformFilterS->SetTransform(transform);

    ls->m_VtkSelectedPointsPolyData->SetPoints(ls->m_SelectedPoints);
    ls->m_VtkSelectedPointsPolyData->GetPointData()->SetVectors(ls->m_SelectedScales);

    // apply transform of current plane to glyphs
    ls->m_SelectedGlyph3D->SetSourceConnection(transformFilterS->GetOutputPort());
    ls->m_SelectedGlyph3D->SetInputData(ls->m_VtkSelectedPointsPolyData);
    ls->m_SelectedGlyph3D->SetScaleModeToScaleByVector();
    ls->m_SelectedGlyph3D->SetVectorModeToUseVector();

    ls->m_VtkSelectedPolyDataMapper->SetInputConnection(ls->m_SelectedGlyph3D->GetOutputPort());
    ls->m_SelectedActor->SetMapper(ls->m_VtkSelectedPolyDataMapper);
    ls->m_SelectedActor->GetProperty()->SetLineWidth(m_PointLineWidth);

    ls->m_PropAssembly->AddPart(ls->m_SelectedActor);

    //duplicated code GenerateDataForRenderer part, fix issue that one renderwindow didn't updata color
    //---------------------------------------
    const mitk::DataNode* node = GetDataNode();
    float color[3]={1.0,1.0,0.0};
    node->GetColor(color,renderer);

    double selectedColor[3]={0,1,0};
    mitk::ColorProperty* colorprop = dynamic_cast<mitk::ColorProperty*>(node->GetProperty("selected.color", renderer));
    if(colorprop)
    {
        mitk::Color tmpColor=colorprop->GetValue();
        selectedColor[0]=tmpColor[0];
        selectedColor[1]=tmpColor[1];
        selectedColor[2]=tmpColor[2];
    }

    double selectedContourColor[3]={1,0,0};
    colorprop = dynamic_cast<mitk::ColorProperty*>(node->GetProperty("selected.contour.color", renderer));
    if(colorprop)
    {
        mitk::Color tmpColor=colorprop->GetValue();
        selectedContourColor[0]=tmpColor[0];
        selectedContourColor[1]=tmpColor[1];
        selectedContourColor[2]=tmpColor[2];
    }

    ls->m_ContourActor->VisibilityOn();
    if(m_Contour==NULL)
    {
//        ls->m_ContourActor->GetProperty()->SetColor(color[0],color[1],color[2]);
    }
    else if(m_Contour->IsSelected()){
        ls->m_ContourActor->GetProperty()->SetColor(selectedContourColor);
    }
//    else if(groupSelected){
//        ls->m_ContourActor->GetProperty()->SetColor(selectedColor);
//    }
    else{
        ls->m_ContourActor->GetProperty()->SetColor(color[0],color[1],color[2]);
    }

    float opacity = 1.0;
    node->GetOpacity(opacity, renderer);
    ls->m_ContourActor->GetProperty()->SetOpacity(opacity);
    //---------------------------------------duplicated end

}

void svContourModelVtkMapper2D::GenerateDataForRenderer( mitk::BaseRenderer *renderer )
{
    mitk::DataNode* node = GetDataNode();
    if( node == NULL )
        return;

    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);

    // check whether the input data has been changed
    bool needGenerateData = ls->IsGenerateDataRequired( renderer, this, GetDataNode() );

    // toggle visibility
    bool visible = true;
    node->GetVisibility(visible, renderer, "visible");
    if(!visible)
    {
        ls->m_UnselectedActor->VisibilityOff();
        ls->m_SelectedActor->VisibilityOff();
        ls->m_ContourActor->VisibilityOff();
        ls->m_PropAssembly->VisibilityOff();
        return;
    }else{
        ls->m_PropAssembly->VisibilityOn();
    }
    node->GetBoolProperty("show points",        m_ShowPoints, renderer);
    node->GetIntProperty("point.size",       m_Point2DSize, renderer);
    node->GetIntProperty("point.linewidth",    m_PointLineWidth, renderer);
    node->GetIntProperty("line.width",          m_LineWidth, renderer);

    float color[3]={1.0,1.0,0.0};
    node->GetColor(color,renderer);

    bool groupSelected=node->IsSelected(renderer);

    double selectedColor[3]={0,1,0};
    mitk::ColorProperty* colorprop = dynamic_cast<mitk::ColorProperty*>(node->GetProperty("selected.color", renderer));
    if(colorprop)
    {
        mitk::Color tmpColor=colorprop->GetValue();
        selectedColor[0]=tmpColor[0];
        selectedColor[1]=tmpColor[1];
        selectedColor[2]=tmpColor[2];
    }

    double selectedContourColor[3]={1,0,0};
    colorprop = dynamic_cast<mitk::ColorProperty*>(node->GetProperty("selected.contour.color", renderer));
    if(colorprop)
    {
        mitk::Color tmpColor=colorprop->GetValue();
        selectedContourColor[0]=tmpColor[0];
        selectedContourColor[1]=tmpColor[1];
        selectedContourColor[2]=tmpColor[2];
    }

    double pointColor[3]={0,1,0};
    colorprop = dynamic_cast<mitk::ColorProperty*>(node->GetProperty("point.color", renderer));
    if(colorprop)
    {
        mitk::Color tmpColor=colorprop->GetValue();
        pointColor[0]=tmpColor[0];
        pointColor[1]=tmpColor[1];
        pointColor[2]=tmpColor[2];
    }


    double selectedPointColor[3]={1,0,0};
    colorprop = dynamic_cast<mitk::ColorProperty*>(node->GetProperty("selected.point.color", renderer));
    if(colorprop)
    {
        mitk::Color tmpColor=colorprop->GetValue();
        selectedPointColor[0]=tmpColor[0];
        selectedPointColor[1]=tmpColor[1];
        selectedPointColor[2]=tmpColor[2];
    }

    float opacity = 1.0;
    node->GetOpacity(opacity, renderer);

    // apply color and opacity
    if(m_ShowPoints)
    {
        ls->m_UnselectedActor->VisibilityOn();
        ls->m_SelectedActor->VisibilityOn();

        ls->m_UnselectedActor->GetProperty()->SetColor(pointColor[0],pointColor[1],pointColor[2]);
        ls->m_UnselectedActor->GetProperty()->SetOpacity(opacity);

        ls->m_SelectedActor->GetProperty()->SetColor(selectedPointColor);
        ls->m_SelectedActor->GetProperty()->SetOpacity(opacity);
    }
    else
    {
        ls->m_UnselectedActor->VisibilityOff();
        ls-> m_SelectedActor->VisibilityOff();
    }

    ls->m_ContourActor->VisibilityOn();
    if(m_Contour==NULL)
    {
//        ls->m_ContourActor->GetProperty()->SetColor(color[0],color[1],color[2]);
    }
    else if(m_Contour->IsSelected()){
        ls->m_ContourActor->GetProperty()->SetColor(selectedContourColor);
    }
//    else if(groupSelected){
//        ls->m_ContourActor->GetProperty()->SetColor(selectedColor);
//    }
    else{
        ls->m_ContourActor->GetProperty()->SetColor(color[0],color[1],color[2]);
    }
    ls->m_ContourActor->GetProperty()->SetOpacity(opacity);

    if(needGenerateData)
    {
        this->CreateVTKRenderObjects(renderer);
    }
}

void svContourModelVtkMapper2D::SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer, bool overwrite)
{
    node->AddProperty( "show points", mitk::BoolProperty::New(true), renderer, overwrite );
    node->AddProperty( "point.size", mitk::IntProperty::New(80), renderer, overwrite );
    node->AddProperty( "point.linewidth", mitk::IntProperty::New(1), renderer, overwrite );

    node->AddProperty( "line.width", mitk::IntProperty::New(2), renderer, overwrite );
    node->AddProperty( "color", mitk::ColorProperty::New(1.0,1.0,0.0), renderer, true );
    node->AddProperty( "point.color",mitk::ColorProperty::New(0,1,0),renderer, overwrite );
    node->AddProperty( "selected.color",mitk::ColorProperty::New(0,1,0),renderer, overwrite );
    node->AddProperty( "selected.contour.color",mitk::ColorProperty::New(1,0,0),renderer, true );
    node->AddProperty( "selected.point.color",mitk::ColorProperty::New(1,0,0),renderer, overwrite );

//    node->AddProperty( "color", mitk::ColorProperty::New(1.0f, 1.0f, 0.0f), renderer, overwrite);
//    node->AddProperty( "opacity", mitk::FloatProperty::New(1.0f), renderer, overwrite );

    Superclass::SetDefaultProperties(node, renderer, overwrite);
}
