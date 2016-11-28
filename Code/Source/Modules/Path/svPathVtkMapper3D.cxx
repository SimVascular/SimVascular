#include "svPathVtkMapper3D.h"

#include "mitkDataNode.h"
#include "mitkProperties.h"
#include "mitkColorProperty.h"
#include "mitkVtkPropRenderer.h"

#include <vtkActor.h>
#include <vtkAppendPolyData.h>
#include <vtkPropAssembly.h>
#include <vtkTubeFilter.h>
#include <vtkRenderer.h>
#include <vtkSphereSource.h>
#include <vtkCubeSource.h>
#include <vtkConeSource.h>
#include <vtkCylinderSource.h>
#include <vtkProperty.h>
#include <vtkPolyDataMapper.h>
#include <vtkCellArray.h>
#include <vtkVectorText.h>
#include <vtkTransform.h>
#include <vtkTransformPolyDataFilter.h>
#include <vtkPolyDataAlgorithm.h>

#include <stdlib.h>

const svPath* svPathVtkMapper3D::GetInput()
{
    return static_cast<const svPath * > ( GetDataNode()->GetData() );
}

svPathVtkMapper3D::svPathVtkMapper3D()
    : m_vtkSelectedPoints(NULL),
      m_vtkUnselectedPoints(NULL),
      m_vtkSplinePoints(NULL),
      m_VtkSelectedPolyDataMapper(NULL),
      m_VtkUnselectedPolyDataMapper(NULL),
      m_VtkSplinePointsPolyDataMapper(NULL),
      m_VtkSplinePolyDataMapper(NULL),
      m_PointSize(2.0),
      m_SplinePointSize(1.0),
      m_SplineRadius(1)
{
    m_PropAssembly = vtkSmartPointer<vtkPropAssembly>::New();

    m_SelectedActor = vtkSmartPointer<vtkActor>::New();
    m_UnselectedActor = vtkSmartPointer<vtkActor>::New();
    m_SplinePointsActor = vtkSmartPointer<vtkActor>::New();
    m_SplineActor = vtkSmartPointer<vtkActor>::New();
}

svPathVtkMapper3D::~svPathVtkMapper3D()
{
}

void svPathVtkMapper3D::ReleaseGraphicsResources(mitk::BaseRenderer* renderer)
{
    m_PropAssembly->ReleaseGraphicsResources(renderer->GetRenderWindow());

    m_SelectedActor->ReleaseGraphicsResources(renderer->GetRenderWindow());
    m_UnselectedActor->ReleaseGraphicsResources(renderer->GetRenderWindow());
    m_SplinePointsActor->ReleaseGraphicsResources(renderer->GetRenderWindow());
    m_SplineActor->ReleaseGraphicsResources(renderer->GetRenderWindow());
}

void svPathVtkMapper3D::CreateVTKRenderObjects()
{
    m_vtkSelectedPoints = vtkSmartPointer<vtkAppendPolyData>::New();
    m_vtkUnselectedPoints = vtkSmartPointer<vtkAppendPolyData>::New();
    m_vtkSplinePoints = vtkSmartPointer<vtkAppendPolyData>::New();

    m_PropAssembly->VisibilityOn();

    if(m_PropAssembly->GetParts()->IsItemPresent(m_SelectedActor))
        m_PropAssembly->RemovePart(m_SelectedActor);
    if(m_PropAssembly->GetParts()->IsItemPresent(m_UnselectedActor))
        m_PropAssembly->RemovePart(m_UnselectedActor);
    if(m_PropAssembly->GetParts()->IsItemPresent(m_SplinePointsActor))
        m_PropAssembly->RemovePart(m_SplinePointsActor);
    if(m_PropAssembly->GetParts()->IsItemPresent(m_SplineActor))
        m_PropAssembly->RemovePart(m_SplineActor);

    // exceptional displaying for PositionTracker -> MouseOrientationTool
    int mapperID;
    bool isInputDevice=false;
    if( this->GetDataNode()->GetBoolProperty("inputdevice",isInputDevice) && isInputDevice )
    {
        if( this->GetDataNode()->GetIntProperty("BaseRendererMapperID",mapperID) && mapperID == 2)
            return; //The event for the PositionTracker came from the 3d widget and  not needs to be displayed
    }

    // get and update the path
    svPath *input  = const_cast<svPath*>(this->GetInput());

    /* only update the input data, if the property tells us to */
    bool update = true;
    this->GetDataNode()->GetBoolProperty("updateDataOnRender", update);
    if (update)
    {
        input->Update();
    }
    int timestep = this->GetTimestep();

    //----------------Control Points---------------------------------

    svPathElement* pathElement=input->GetPathElement(timestep);

    if (pathElement==NULL||(pathElement->GetControlPointNumber()==0 && pathElement->GetPathPointNumber()<2))
    {
        m_PropAssembly->VisibilityOff();
        return;
    }

    int NumberOfSelectedAdded = 0;
    int NumberOfUnselectedAdded = 0;

    //now fill selected and unselected points
    //get size of Points in Property
    m_PointSize = 2.0;
    mitk::FloatProperty::Pointer pointSizeProp = dynamic_cast<mitk::FloatProperty *>(this->GetDataNode()->GetProperty("point size"));
    if ( pointSizeProp.IsNotNull() )
    {
        m_PointSize = pointSizeProp->GetValue();
    }

    for (int i=0; i<pathElement->GetControlPointNumber(); i++)
    {
        vtkSmartPointer<vtkPolyDataAlgorithm> source;
        vtkSmartPointer<vtkCubeSource> cube = vtkSmartPointer<vtkCubeSource>::New();
        cube->SetXLength(m_PointSize/2);
        cube->SetYLength(m_PointSize/2);
        cube->SetZLength(m_PointSize/2);
        mitk::Point3D point=pathElement->GetControlPoint(i);
        //may need to convert to index
        cube->SetCenter(point[0],point[1],point[2]);
        source = cube;

        if (pathElement->IsControlPointSelected(i))
        {
            m_vtkSelectedPoints->AddInputConnection(source->GetOutputPort());
            ++NumberOfSelectedAdded;
        }
        else
        {
            m_vtkUnselectedPoints->AddInputConnection(source->GetOutputPort());
            ++NumberOfUnselectedAdded;
        }
    }

    //now according to number of elements added to selected or unselected, build up the rendering pipeline
    if (NumberOfSelectedAdded > 0)
    {
        m_VtkSelectedPolyDataMapper = vtkSmartPointer<vtkPolyDataMapper>::New();
        m_VtkSelectedPolyDataMapper->SetInputConnection(m_vtkSelectedPoints->GetOutputPort());

        m_SelectedActor = vtkSmartPointer<vtkActor>::New();

        m_SelectedActor->SetMapper(m_VtkSelectedPolyDataMapper);
        m_PropAssembly->AddPart(m_SelectedActor);
    }

    if (NumberOfUnselectedAdded > 0)
    {
        m_VtkUnselectedPolyDataMapper = vtkSmartPointer<vtkPolyDataMapper>::New();
        m_VtkUnselectedPolyDataMapper->SetInputConnection(m_vtkUnselectedPoints->GetOutputPort());

        m_UnselectedActor = vtkSmartPointer<vtkActor>::New();

        m_UnselectedActor->SetMapper(m_VtkUnselectedPolyDataMapper);
        m_PropAssembly->AddPart(m_UnselectedActor);
    }

    //-------------------Spline Points and Spline----------------

    if(pathElement->GetPathPointNumber()>1)
    {
        //create spline
        bool makeSpline = false;
        this->GetDataNode()->GetBoolProperty("show spline", makeSpline);
        if (makeSpline)
        {
            this->CreateSpline();
        }

        bool makeSplinePoints=false;
        this->GetDataNode()->GetBoolProperty("show spline 3D points", makeSplinePoints);

        if(makeSplinePoints)
        {
            m_SplinePointSize = 1.0;
            mitk::FloatProperty::Pointer splinePointSizeProp = dynamic_cast<mitk::FloatProperty *>(this->GetDataNode()->GetProperty("spline point size"));
            if ( splinePointSizeProp.IsNotNull() )
                m_SplinePointSize = splinePointSizeProp->GetValue();

            for (int i=0; i<pathElement->GetPathPointNumber(); i++)
            {
                mitk::Point3D point = pathElement->GetPathPosPoint(i);

                vtkSmartPointer<vtkPolyDataAlgorithm> source;
                vtkSmartPointer<vtkSphereSource> sphere = vtkSmartPointer<vtkSphereSource>::New();
                sphere->SetRadius(m_SplinePointSize);
                //may need to convert to index
                sphere->SetCenter(point[0],point[1],point[2]);

                //MouseOrientation Tool (PositionTracker)
                if(isInputDevice)
                {
                    sphere->SetThetaResolution(10);
                    sphere->SetPhiResolution(10);
                }
                else
                {
                    sphere->SetThetaResolution(20);
                    sphere->SetPhiResolution(20);
                }
                source = sphere;

                m_vtkSplinePoints->AddInputConnection(source->GetOutputPort());
            }

            m_VtkSplinePointsPolyDataMapper = vtkSmartPointer<vtkPolyDataMapper>::New();
            m_VtkSplinePointsPolyDataMapper->SetInputConnection(m_vtkSplinePoints->GetOutputPort());

            //create a new instance of the actor
            m_SplinePointsActor = vtkSmartPointer<vtkActor>::New();

            m_SplinePointsActor->SetMapper(m_VtkSplinePointsPolyDataMapper);
            m_PropAssembly->AddPart(m_SplinePointsActor);
        }

    }

}

void svPathVtkMapper3D::GenerateDataForRenderer( mitk::BaseRenderer *renderer )
{
    bool visible = true;
    GetDataNode()->GetVisibility(visible, renderer, "visible");
    if(!visible)
    {
        m_UnselectedActor->VisibilityOff();
        m_SelectedActor->VisibilityOff();
        m_SplinePointsActor->VisibilityOff();
        m_SplineActor->VisibilityOff();
        return;
    }

    // create new vtk render objects (e.g. sphere for a point)

    SetVtkMapperImmediateModeRendering(m_VtkSelectedPolyDataMapper);
    SetVtkMapperImmediateModeRendering(m_VtkUnselectedPolyDataMapper);
    SetVtkMapperImmediateModeRendering(m_VtkSplinePointsPolyDataMapper);
    SetVtkMapperImmediateModeRendering(m_VtkSplinePolyDataMapper);

    mitk::Mapper::BaseLocalStorage *ls = m_LSH.GetLocalStorage(renderer);
    bool needGenerateData = ls->IsGenerateDataRequired( renderer, this, GetDataNode() );

    if(!needGenerateData)
    {
        mitk::FloatProperty * pointSizeProp = dynamic_cast<mitk::FloatProperty *>(this->GetDataNode()->GetProperty("point size"));
        mitk::FloatProperty * splinePointSizeProp = dynamic_cast<mitk::FloatProperty *>(this->GetDataNode()->GetProperty("spline point size"));
        mitk::FloatProperty * splineSizeProp = dynamic_cast<mitk::FloatProperty *>(this->GetDataNode()->GetProperty("spline size"));

        // only create new vtk render objects if property values were changed
        if(pointSizeProp && m_PointSize!=pointSizeProp->GetValue() )
            needGenerateData = true;
        if(splinePointSizeProp && m_SplinePointSize!=splinePointSizeProp->GetValue() )
            needGenerateData = true;
        if(splineSizeProp && m_SplineRadius!=splineSizeProp->GetValue() )
            needGenerateData = true;
    }

    if(needGenerateData)
    {
        this->CreateVTKRenderObjects();
        ls->UpdateGenerateDataTime();
    }

    this->ApplyAllProperties(renderer);

    bool showPoints = true;
    this->GetDataNode()->GetBoolProperty("show points", showPoints);

    if(showPoints)
    {
        m_UnselectedActor->VisibilityOn();
        m_SelectedActor->VisibilityOn();
    }
    else
    {
        m_UnselectedActor->VisibilityOff();
        m_SelectedActor->VisibilityOff();
    }

    if(dynamic_cast<mitk::FloatProperty *>(this->GetDataNode()->GetProperty("opacity")) != NULL)
    {
        mitk::FloatProperty::Pointer pointOpacity =dynamic_cast<mitk::FloatProperty *>(this->GetDataNode()->GetProperty("opacity"));
        float opacity = pointOpacity->GetValue();
        m_UnselectedActor->GetProperty()->SetOpacity(opacity);
        m_SelectedActor->GetProperty()->SetOpacity(opacity);
        m_SplinePointsActor->GetProperty()->SetOpacity(opacity);
        m_SplineActor->GetProperty()->SetOpacity(opacity);
    }

    bool makeSpline = true;
    this->GetDataNode()->GetBoolProperty("show spline", makeSpline);
    if (makeSpline)
    {
        m_SplineActor->VisibilityOn();
    }
    else
    {
        m_SplineActor->VisibilityOff();
    }

    bool makeSplinePoints = false;
    this->GetDataNode()->GetBoolProperty("show spline 3D points", makeSplinePoints);
    if (makeSplinePoints)
    {
        m_SplinePointsActor->VisibilityOn();
    }
    else
    {
        m_SplinePointsActor->VisibilityOff();
    }

}

void svPathVtkMapper3D::ResetMapper( mitk::BaseRenderer* /*renderer*/ )
{
    m_PropAssembly->VisibilityOff();
}

vtkProp* svPathVtkMapper3D::GetVtkProp(mitk::BaseRenderer * /*renderer*/)
{
    return m_PropAssembly;
}

void svPathVtkMapper3D::UpdateVtkTransform(mitk::BaseRenderer * /*renderer*/)
{
    vtkSmartPointer<vtkLinearTransform> vtktransform =
            this->GetDataNode()->GetVtkTransform(this->GetTimestep());

    m_SelectedActor->SetUserTransform(vtktransform);
    m_UnselectedActor->SetUserTransform(vtktransform);
    m_SplinePointsActor->SetUserTransform(vtktransform);
    m_SplineActor->SetUserTransform(vtktransform);
}

void svPathVtkMapper3D::ApplyAllProperties(mitk::BaseRenderer* renderer)
{
    Superclass::ApplyColorAndOpacityProperties(renderer, m_SplineActor);//may not needed

    double unselectedColor[4]={1.0f,1.0f,0.0f,1.0f};//yellow
    double selectedColor[4]={1.0f,0.0f,0.0f,1.0f};//red
    double splinePointsColor[4]={0.0f,1.0f,0.0f,1.0f};  //green
    double splineColor[4]={1.0f,1.0f,1.0f,1.0f};  //white;

    //different types for color!!!
    mitk::Color tmpColor;
    double opacity = 1.0;

    if (dynamic_cast<mitk::ColorProperty*>(this->GetDataNode()->GetPropertyList(renderer)->GetProperty("unselected color")) != NULL)
    {
        tmpColor = dynamic_cast<mitk::ColorProperty *>(this->GetDataNode()->GetPropertyList(renderer)->GetProperty("unselected color"))->GetValue();
        unselectedColor[0] = tmpColor[0];
        unselectedColor[1] = tmpColor[1];
        unselectedColor[2] = tmpColor[2];
//        unselectedColor[3] = 1.0f; // alpha value
    }
    else if (dynamic_cast<mitk::ColorProperty*>(this->GetDataNode()->GetPropertyList(NULL)->GetProperty("unselected color")) != NULL)
    {
        tmpColor = dynamic_cast<mitk::ColorProperty *>(this->GetDataNode()->GetPropertyList(NULL)->GetProperty("unselected color"))->GetValue();
        unselectedColor[0] = tmpColor[0];
        unselectedColor[1] = tmpColor[1];
        unselectedColor[2] = tmpColor[2];
//        unselectedColor[3] = 1.0f; // alpha value
    }

    if (dynamic_cast<mitk::ColorProperty*>(this->GetDataNode()->GetPropertyList(renderer)->GetProperty("selected color")) != NULL)
    {
        tmpColor = dynamic_cast<mitk::ColorProperty *>(this->GetDataNode()->GetPropertyList(renderer)->GetProperty("selected color"))->GetValue();
        selectedColor[0] = tmpColor[0];
        selectedColor[1] = tmpColor[1];
        selectedColor[2] = tmpColor[2];
//        selectedColor[3] = 1.0f;
    }
    else if (dynamic_cast<mitk::ColorProperty*>(this->GetDataNode()->GetPropertyList(NULL)->GetProperty("selected color")) != NULL)
    {
        tmpColor = dynamic_cast<mitk::ColorProperty *>(this->GetDataNode()->GetPropertyList(NULL)->GetProperty("selected color"))->GetValue();
        selectedColor[0] = tmpColor[0];
        selectedColor[1] = tmpColor[1];
        selectedColor[2] = tmpColor[2];
//        selectedColor[3] = 1.0f;
    }

    if (dynamic_cast<mitk::ColorProperty*>(this->GetDataNode()->GetPropertyList(renderer)->GetProperty("spline point color")) != NULL)
    {
        tmpColor = dynamic_cast<mitk::ColorProperty *>(this->GetDataNode()->GetPropertyList(renderer)->GetProperty("spline point color"))->GetValue();
        splinePointsColor[0] = tmpColor[0];
        splinePointsColor[1] = tmpColor[1];
        splinePointsColor[2] = tmpColor[2];
//        splinePointsColor[3] = 1.0f;
    }
    else if (dynamic_cast<mitk::ColorProperty*>(this->GetDataNode()->GetPropertyList(NULL)->GetProperty("spline point color")) != NULL)
    {
        tmpColor = dynamic_cast<mitk::ColorProperty *>(this->GetDataNode()->GetPropertyList(NULL)->GetProperty("spline point color"))->GetValue();
        splinePointsColor[0] = tmpColor[0];
        splinePointsColor[1] = tmpColor[1];
        splinePointsColor[2] = tmpColor[2];
//        splinePointsColor[3] = 1.0f;
    }

    //check if the node has a color
    float splineColorTMP[4]={1.0f,1.0f,0.0f,1.0f};//yellow
    this->GetDataNode()->GetColor(splineColorTMP, NULL);
    splineColor[0] = splineColorTMP[0];
    splineColor[1] = splineColorTMP[1];
    splineColor[2] = splineColorTMP[2];

    if(dynamic_cast<mitk::FloatProperty *>(this->GetDataNode()->GetPropertyList(renderer)->GetProperty("opacity")) != NULL)
    {
        mitk::FloatProperty::Pointer pointOpacity =dynamic_cast<mitk::FloatProperty *>(this->GetDataNode()->GetPropertyList(renderer)->GetProperty("opacity"));
        opacity = pointOpacity->GetValue();
    }
    else if(dynamic_cast<mitk::FloatProperty *>(this->GetDataNode()->GetPropertyList(NULL)->GetProperty("opacity")) != NULL)
    {
        mitk::FloatProperty::Pointer pointOpacity =dynamic_cast<mitk::FloatProperty *>(this->GetDataNode()->GetPropertyList(NULL)->GetProperty("opacity"));
        opacity = pointOpacity->GetValue();
    }

    m_SelectedActor->GetProperty()->SetColor(selectedColor);
    m_SelectedActor->GetProperty()->SetOpacity(opacity);

    m_UnselectedActor->GetProperty()->SetColor(unselectedColor);
    m_UnselectedActor->GetProperty()->SetOpacity(opacity);

    bool makeSplinePoints = false;
    this->GetDataNode()->GetBoolProperty("show spline 3D points", makeSplinePoints, renderer);
    if(makeSplinePoints && (m_SplinePointsActor != NULL) )
    {
        m_SplinePointsActor->GetProperty()->SetColor(splinePointsColor);
        m_SplinePointsActor->GetProperty()->SetOpacity(opacity);
    }

    bool makeSpline = true;
    this->GetDataNode()->GetBoolProperty("show spline", makeSpline, renderer);
    if(makeSpline && (m_SplineActor != NULL) )
    {
        this->CreateSpline();
        m_SplineActor->GetProperty()->SetColor(splineColor);
        m_SplineActor->GetProperty()->SetOpacity(opacity);
    }

}

void svPathVtkMapper3D::CreateSpline()
{
    int timestep = this->GetTimestep();
    svPath* input  = const_cast<svPath*>(this->GetInput());

    svPathElement* pathElement=input->GetPathElement(timestep);

    if(pathElement&&pathElement->GetPathPointNumber()>1)
    {
        vtkSmartPointer<vtkAppendPolyData> vtkSplinePolyData = vtkSmartPointer<vtkAppendPolyData>::New();
        m_VtkSplinePolyDataMapper = vtkSmartPointer<vtkPolyDataMapper>::New();

        vtkSmartPointer<vtkPoints> points = vtkSmartPointer<vtkPoints>::New();
        vtkSmartPointer<vtkCellArray> lines = vtkSmartPointer<vtkCellArray>::New();

        for (int i=0; i<pathElement->GetPathPointNumber(); i++)
        {
            mitk::Point3D point = pathElement->GetPathPosPoint(i);

            vtkIdType cell[2] = {i-1,i};
            points->InsertPoint(i,point[0],point[1],point[2]);
            if (i>0)
                lines->InsertNextCell(2,cell);
        }

        vtkSmartPointer<vtkPolyData> spline = vtkSmartPointer<vtkPolyData>::New();
        spline->SetPoints(points);
        spline->SetLines(lines);

//        vtkSmartPointer<vtkTubeFilter> tubeFilter = vtkSmartPointer<vtkTubeFilter>::New();
//        tubeFilter->SetNumberOfSides( 12 );
//        tubeFilter->SetInputData(spline);

//        //check for property contoursize.
//        m_SplineRadius = 1;
//        mitk::FloatProperty::Pointer splineSizeProp = dynamic_cast<mitk::FloatProperty *>(this->GetDataNode()->GetProperty("spline size") );

//        if (splineSizeProp.IsNotNull())
//            m_SplineRadius = splineSizeProp->GetValue();

//        tubeFilter->SetRadius( m_SplineRadius );
//        tubeFilter->Update();

//        //add to pipeline
//        vtkSplinePolyData->AddInputConnection(tubeFilter->GetOutputPort());
//        m_VtkSplinePolyDataMapper->SetInputConnection(vtkSplinePolyData->GetOutputPort());
        m_VtkSplinePolyDataMapper->SetInputData(spline);

        int lineWidth=1;
        mitk::IntProperty::Pointer splineSizeProp = dynamic_cast<mitk::IntProperty *>(this->GetDataNode()->GetProperty("line width") );
        if (splineSizeProp.IsNotNull())
            lineWidth = splineSizeProp->GetValue();

        m_SplineActor->SetMapper(m_VtkSplinePolyDataMapper);
        m_SplineActor->GetProperty()->SetLineWidth(lineWidth);
        m_PropAssembly->AddPart(m_SplineActor);

    }

}

void svPathVtkMapper3D::SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer, bool overwrite)
{
//    Superclass::SetDefaultProperties(node, renderer, overwrite);

    node->AddProperty( "show points", mitk::BoolProperty::New(true), renderer, overwrite );
    node->AddProperty( "show spline 3D points", mitk::BoolProperty::New(false), renderer, overwrite );
    node->AddProperty( "point size", mitk::FloatProperty::New(1.0), renderer, overwrite);
    node->AddProperty( "spline point size", mitk::FloatProperty::New(0.5), renderer, overwrite);
    node->AddProperty( "unselected color",mitk::ColorProperty::New(0,0,1),renderer, overwrite );
    node->AddProperty( "selected color",mitk::ColorProperty::New(1,0,0),renderer, overwrite );
    node->AddProperty( "spline point color",mitk::ColorProperty::New(0,1,0),renderer, overwrite );

    node->AddProperty( "show spline", mitk::BoolProperty::New(true), renderer, overwrite );
    node->AddProperty( "line width", mitk::IntProperty::New(1), renderer, overwrite );

//    node->AddProperty( "layer", mitk::IntProperty::New(1), renderer, overwrite );
    node->AddProperty( "color", mitk::ColorProperty::New(1.0f, 1.0f, 0.2f), renderer, overwrite);
    node->AddProperty( "opacity", mitk::FloatProperty::New(1.0f), renderer, overwrite );
//    node->AddProperty( "updateDataOnRender", mitk::BoolProperty::New(true), renderer, overwrite );

    Superclass::SetDefaultProperties(node, renderer, overwrite);


}
