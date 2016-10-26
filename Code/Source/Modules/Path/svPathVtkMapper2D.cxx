#include "svPathVtkMapper2D.h"

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
#include <vtkTextActor.h>
#include <vtkTextProperty.h>
#include <vtkCellArray.h>

#include <stdlib.h>

svPathVtkMapper2D::LocalStorage::LocalStorage()
{
    // points
    m_UnselectedPoints = vtkSmartPointer<vtkPoints>::New();
    m_SelectedPoints = vtkSmartPointer<vtkPoints>::New();
    m_SplinePoints = vtkSmartPointer<vtkPoints>::New();
    m_SplineConnectingPoints = vtkSmartPointer<vtkPoints>::New();

    // scales
    m_UnselectedScales = vtkSmartPointer<vtkFloatArray>::New();
    m_SelectedScales = vtkSmartPointer<vtkFloatArray>::New();
    m_SplinePointsScales = vtkSmartPointer<vtkFloatArray>::New();

    // distances
    //  m_DistancesBetweenPoints = vtkSmartPointer<vtkFloatArray>::New();

    // lines
    m_SplineConnectingLines = vtkSmartPointer<vtkCellArray>::New();

    // glyph source (provides the different shapes)
    m_UnselectedGlyphSource2D = vtkSmartPointer<vtkGlyphSource2D>::New();
    m_SelectedGlyphSource2D = vtkSmartPointer<vtkGlyphSource2D>::New();
    m_SplineGlyphSource2D = vtkSmartPointer<vtkGlyphSource2D>::New();

    // glyphs
    m_UnselectedGlyph3D = vtkSmartPointer<vtkGlyph3D>::New();
    m_SelectedGlyph3D = vtkSmartPointer<vtkGlyph3D>::New();
    m_SplineGlyph3D = vtkSmartPointer<vtkGlyph3D>::New();

    // polydata
    m_VtkUnselectedPointsPolyData = vtkSmartPointer<vtkPolyData>::New();
    m_VtkSelectedPointsPolyData = vtkSmartPointer <vtkPolyData>::New();
    m_VtkSplinePointsPolyData = vtkSmartPointer<vtkPolyData>::New();
    m_VtkSplineConnectingLinesPolyData = vtkSmartPointer<vtkPolyData>::New();

    // actors
    m_UnselectedActor = vtkSmartPointer <vtkActor>::New();
    m_SelectedActor = vtkSmartPointer <vtkActor>::New();
    m_SplinePointsActor = vtkSmartPointer <vtkActor>::New();
    m_SplineConnectingLinesActor = vtkSmartPointer <vtkActor>::New();

    // mappers
    m_VtkUnselectedPolyDataMapper = vtkSmartPointer<vtkPolyDataMapper>::New();
    m_VtkSelectedPolyDataMapper = vtkSmartPointer<vtkPolyDataMapper>::New();
    m_VtkSplinePointsPolyDataMapper = vtkSmartPointer<vtkPolyDataMapper>::New();
    m_VtkSplineConnectingLinesPolyDataMapper = vtkSmartPointer<vtkPolyDataMapper>::New();

    // propassembly
    m_PropAssembly = vtkSmartPointer <vtkPropAssembly>::New();
}

svPathVtkMapper2D::LocalStorage::~LocalStorage()
{
}

const svPath* svPathVtkMapper2D::GetInput() const
{
    return static_cast<const svPath * > ( GetDataNode()->GetData() );
}

// constructor PointSetVtkMapper2D
svPathVtkMapper2D::svPathVtkMapper2D()
    : m_ShowSpline(false),
      m_ShowSplinePoints(true),
      m_ShowPoints(true),
      m_LineWidth(1),
      m_PointLineWidth(1),
      m_Point2DSize(100),
      m_SplinePoint2DSize(60),
      m_DistanceToPlane(4.0f)
{
}

svPathVtkMapper2D::~svPathVtkMapper2D()
{
}

void svPathVtkMapper2D::ResetMapper( mitk::BaseRenderer* renderer )
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
    ls->m_PropAssembly->VisibilityOff();
}

// returns propassembly
vtkProp* svPathVtkMapper2D::GetVtkProp(mitk::BaseRenderer * renderer)
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
    return ls->m_PropAssembly;
}

bool svPathVtkMapper2D::makePerpendicularVector2D(const mitk::Vector2D& in, mitk::Vector2D& out)
{
    // The dot product of orthogonal vectors is zero.
    // In two dimensions the slopes of perpendicular lines are negative reciprocals.
    if((fabs(in[0])>0) && ( (fabs(in[0])>fabs(in[1])) || (in[1] == 0) ) )
    {
        // negative reciprocal
        out[0]=-in[1]/in[0];
        out[1]=1;
        out.Normalize();
        return true;
    }
    else if(fabs(in[1])>0)
    {
        out[0]=1;
        // negative reciprocal
        out[1]=-in[0]/in[1];
        out.Normalize();
        return true;
    }
    else
    {
        return false;
    }
}

void svPathVtkMapper2D::CreateVTKRenderObjects(mitk::BaseRenderer* renderer)
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);

    // initialize polydata here, otherwise we have update problems when
    // executing this function again
    ls->m_VtkUnselectedPointsPolyData = vtkSmartPointer<vtkPolyData>::New();
    ls->m_VtkSelectedPointsPolyData = vtkSmartPointer <vtkPolyData>::New();
    ls->m_VtkSplinePointsPolyData = vtkSmartPointer<vtkPolyData>::New();
    ls->m_VtkSplineConnectingLinesPolyData = vtkSmartPointer<vtkPolyData>::New();

    svPath* input  = const_cast<svPath*>(this->GetInput());

    // only update the input data, if the property tells us to
    bool update = true;
    this->GetDataNode()->GetBoolProperty("updateDataOnRender", update);
    if (update)
    {
        input->Update();
    }

    int timeStep = this->GetTimestep();

    float control_point_size = m_Point2DSize*renderer->GetScaleFactorMMPerDisplayUnit();
    float path_point_size = m_SplinePoint2DSize*renderer->GetScaleFactorMMPerDisplayUnit();

    svPathElement* pathElement=input->GetPathElement(timeStep);

    if (!pathElement||(pathElement->GetControlPointNumber()==0 && pathElement->GetPathPointNumber()<2))
    {
        ls->m_PropAssembly->VisibilityOff();
        return;
    }

    ls->m_PropAssembly->VisibilityOn();

    // empty point sets, cellarrays, scalars
    ls->m_UnselectedPoints->Reset();
    ls->m_SelectedPoints->Reset();

    ls->m_SplinePoints->Reset();
    ls->m_SplineConnectingPoints->Reset();
    ls->m_SplineConnectingLines->Reset();

    ls->m_UnselectedScales->Reset();
    ls->m_SelectedScales->Reset();
    ls->m_SplinePointsScales->Reset();

    ls->m_UnselectedScales->SetNumberOfComponents(3);
    ls->m_SelectedScales->SetNumberOfComponents(3);
    ls->m_SplinePointsScales->SetNumberOfComponents(3);

    //----------------Control Points----------------------------

    if(pathElement->GetControlPointNumber()>0)
    {
        const mitk::PlaneGeometry* geo2D = renderer->GetCurrentWorldPlaneGeometry();
//        vtkLinearTransform* dataNodeTransform = input->GetGeometry()->GetVtkTransform();

        double precisionFactor=0.52;
        m_DistanceToPlane=geo2D->GetExtentInMM( 2 )*precisionFactor;

        for (int i=0;i<pathElement->GetControlPointNumber();i++)
        {
            // get current anchor point in path
            mitk::Point3D point = pathElement->GetControlPoint(i);
            bool selected=pathElement->IsControlPointSelected(i);

            // transform point
//            {
//                float vtkp[3];
//                itk2vtk(point, vtkp);
//                dataNodeTransform->TransformPoint(vtkp, vtkp);
//                vtk2itk(vtkp,point);
//            }
//            p[0] = point[0];
//            p[1] = point[1];
//            p[2] = point[2];

            // compute distance to current plane
            float diff = geo2D->Distance(point);
            diff = diff * diff;

            //draw markers on slices a certain distance away from the points
            //location according to the tolerance threshold (m_DistanceToPlane)
            if(diff < m_DistanceToPlane)
            {
                // is point selected or not?
                if (selected)
                {
                    ls->m_SelectedPoints->InsertNextPoint(point[0],point[1],point[2]);
                    // point is scaled according to its distance to the plane
                    //ls->m_SelectedScales->InsertNextTuple3(m_Point2DSize - (2*diff),0,0);
                    ls->m_SelectedScales->InsertNextTuple3(control_point_size,0,0);
                }
                else
                {
                    ls->m_UnselectedPoints->InsertNextPoint(point[0],point[1],point[2]);
                    // point is scaled according to its distance to the plane
                    //ls->m_UnselectedScales->InsertNextTuple3(m_Point2DSize - (2*diff),0,0);
                    ls->m_UnselectedScales->InsertNextTuple3(control_point_size,0,0);
                }
            }
        }
    }

    //--------------Spline Points and connecting lines-----------------------------
    if(pathElement->GetPathPointNumber()>1)
    {

        int NumberConnectingPoints = 0;
        bool pointsOnSameSideOfPlane = false;

        // first spline point in path
        mitk::Point3D point = pathElement->GetPathPosPoint(0);
        mitk::Point3D lastP = point;          // last visited point (predecessor in point set of "point")

        const mitk::PlaneGeometry* geo2D = renderer->GetCurrentWorldPlaneGeometry();
//        vtkLinearTransform* dataNodeTransform = input->GetGeometry()->GetVtkTransform();

        for (int i=0;i<pathElement->GetPathPointNumber();i++)
        {
            lastP = point;

            // get current spline point in path
            point = pathElement->GetPathPosPoint(i);

            // transform point
//            {
//                float vtkp[3];
//                itk2vtk(point, vtkp);
//                dataNodeTransform->TransformPoint(vtkp, vtkp);
//                vtk2itk(vtkp,point);
//            }
//            p[0] = point[0];
//            p[1] = point[1];
//            p[2] = point[2];

            // compute distance to current plane
            float diff = geo2D->Distance(point);
            diff = diff * diff;

            //draw markers on slices a certain distance away from the points
            //location according to the tolerance threshold (m_DistanceToPlane)
//            if(diff < m_DistanceToPlane)
//            {
//                ls->m_SplinePoints->InsertNextPoint(point[0],point[1],point[2]);
//                // point is scaled according to its distance to the plane
//                ls->m_SplinePointsScales->InsertNextTuple3(m_SplinePoint2DSize - (2*diff),0,0);
//            }

            if(i>0)
            {
                mitk::ScalarType distance =    geo2D->SignedDistance(point);
                mitk::ScalarType lastDistance =    geo2D->SignedDistance(lastP);
                if(distance * lastDistance<=0.0)
                {
                    mitk::Line3D line;
                    line.SetPoint1(lastP);
                    line.SetPoint2(point);

                    mitk::Point3D interP;

                    geo2D->IntersectionPoint(line,interP);
                    ls->m_SplinePoints->InsertNextPoint(interP[0],interP[1],interP[2]);
                    ls->m_SplinePointsScales->InsertNextTuple3(path_point_size,0,0);
                }
            }


//            // lines between points, which intersect the current plane, are drawn
//            if( m_ShowSpline && i > 0 )
//            {
//                mitk::ScalarType distance =    displayGeometry->GetWorldGeometry()->SignedDistance(point);
//                mitk::ScalarType lastDistance =    displayGeometry->GetWorldGeometry()->SignedDistance(lastP);

//                pointsOnSameSideOfPlane = (distance * lastDistance) > 0.5;

//                // Points must be on different side of plane in order to draw a contour.
//                if ( !pointsOnSameSideOfPlane)
//                {
//                    vtkSmartPointer<vtkLine> line = vtkSmartPointer<vtkLine>::New();

//                    ls->m_SplineConnectingPoints->InsertNextPoint(lastP[0],lastP[1],lastP[2]);
//                    line->GetPointIds()->SetId(0, NumberConnectingPoints);
//                    NumberConnectingPoints++;

//                    ls->m_SplineConnectingPoints->InsertNextPoint(point[0], point[1], point[2]);
//                    line->GetPointIds()->SetId(1, NumberConnectingPoints);
//                    NumberConnectingPoints++;

//                    ls->m_SplineConnectingLines->InsertNextCell(line);

//                }
//            }

        }

//        //create lines between the points which intersect the plane
//        if (m_ShowSpline)
//        {
//            ls->m_VtkSplineConnectingLinesPolyData->SetPoints(ls->m_SplineConnectingPoints);
//            ls->m_VtkSplineConnectingLinesPolyData->SetLines(ls->m_SplineConnectingLines);

//            ls->m_VtkSplineConnectingLinesPolyDataMapper->SetInputData(ls->m_VtkSplineConnectingLinesPolyData);
//            ls->m_SplineConnectingLinesActor->SetMapper(ls->m_VtkSplineConnectingLinesPolyDataMapper);
//            ls->m_SplineConnectingLinesActor->GetProperty()->SetLineWidth(m_LineWidth);

//            ls->m_PropAssembly->AddPart(ls->m_SplineConnectingLinesActor);
//        }

    }

    // the point set must be transformed in order to obtain the appropriate glyph orientation
    // according to the current view
    vtkSmartPointer<vtkTransform> transform = vtkSmartPointer<vtkTransform>::New();
//    vtkSmartPointer<vtkMatrix4x4> a,b = vtkSmartPointer<vtkMatrix4x4>::New();

     const mitk::PlaneGeometry* geo2D = renderer->GetCurrentWorldPlaneGeometry();
//    a = geo2D->GetVtkTransform()->GetMatrix();
//    b->DeepCopy( a );

//    // delete transformation from matrix, only take orientation
//    b->SetElement(3,3,1);
//    b->SetElement(2,3,0);
//    b->SetElement(1,3,0);
//    b->SetElement(0,3,0);
//    b->SetElement(3,2,0);
//    b->SetElement(3,1,0);
//    b->SetElement(3,0,0);

//    transform->SetMatrix(  b );


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
    ls->m_UnselectedGlyphSource2D->SetGlyphTypeToDiamond();
    ls->m_UnselectedGlyphSource2D->CrossOn();
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

    ls->m_SelectedGlyphSource2D->SetGlyphTypeToDiamond();
    ls->m_SelectedGlyphSource2D->CrossOn();
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

    //---- SPLINE POINTS  -----//

    // apply properties to glyph
    ls->m_SplineGlyphSource2D->SetGlyphTypeToCross();
    ls->m_SplineGlyphSource2D->FilledOff();

//    ls->m_SplineGlyphSource2D->SetGlyphTypeToCircle();
//    ls->m_SplineGlyphSource2D->FilledOn();

    // apply transform
    vtkSmartPointer<vtkTransformFilter> transformFilterSP = vtkSmartPointer<vtkTransformFilter>::New();
    transformFilterSP->SetInputConnection(ls->m_SplineGlyphSource2D->GetOutputPort());
    transformFilterSP->SetTransform(transform);

    ls->m_VtkSplinePointsPolyData->SetPoints(ls->m_SplinePoints);
    ls->m_VtkSplinePointsPolyData->GetPointData()->SetVectors(ls->m_SplinePointsScales);

    // apply transform of current plane to glyphs
    ls->m_SplineGlyph3D->SetSourceConnection(transformFilterSP->GetOutputPort());
    ls->m_SplineGlyph3D->SetInputData(ls->m_VtkSplinePointsPolyData);
    ls->m_SplineGlyph3D->SetScaleModeToScaleByVector();
    ls->m_SplineGlyph3D->SetVectorModeToUseVector();

    ls->m_VtkSplinePointsPolyDataMapper->SetInputConnection(ls->m_SplineGlyph3D->GetOutputPort());
    ls->m_SplinePointsActor->SetMapper(ls->m_VtkSplinePointsPolyDataMapper);
    ls->m_SplinePointsActor->GetProperty()->SetLineWidth(m_PointLineWidth);

    ls->m_PropAssembly->AddPart(ls->m_SplinePointsActor);
}

void svPathVtkMapper2D::GenerateDataForRenderer( mitk::BaseRenderer *renderer )
{
    const mitk::DataNode* node = GetDataNode();
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
        ls->m_SplinePointsActor->VisibilityOff();
        ls->m_SplineConnectingLinesActor->VisibilityOff();
        ls->m_PropAssembly->VisibilityOff();
        return;
    }else{
        ls->m_PropAssembly->VisibilityOn();
    }
    node->GetBoolProperty("show points",        m_ShowPoints, renderer);
    node->GetBoolProperty("show spline points",      m_ShowSplinePoints, renderer);
    node->GetIntProperty("point 2D size",       m_Point2DSize, renderer);
    node->GetIntProperty("spline point 2D size",       m_SplinePoint2DSize, renderer);
    node->GetIntProperty("point line width",    m_PointLineWidth, renderer);
    node->GetFloatProperty("point 2D distance to plane", m_DistanceToPlane, renderer );

    node->GetBoolProperty("show spline",       m_ShowSpline, renderer);
    node->GetIntProperty("line width",          m_LineWidth, renderer);

    //check for color props and use it for rendering of selected/unselected points and contour
    //due to different params in VTK (double/float) we have to convert

    double unselectedColor[4]={1.0f,1.0f,0.0f,1.0f};  //yellow
    double selectedColor[4]={1.0f,0.0f,0.0f,1.0f};    //red
    double splinePointsColor[4]={0.0f,1.0f,0.0f,1.0f};  //green
    float splineColor[4];

    float opacity = 1.0;

    GetDataNode()->GetOpacity(opacity, renderer);

    // apply color and opacity
    if(m_ShowPoints)
    {
        ls->m_UnselectedActor->VisibilityOn();
        ls->m_SelectedActor->VisibilityOn();

        if (dynamic_cast<mitk::ColorProperty*>(this->GetDataNode()->GetPropertyList(renderer)->GetProperty("unselected color")) != NULL)
        {
            mitk::Color tmpColor = dynamic_cast<mitk::ColorProperty *>(this->GetDataNode()->GetPropertyList(renderer)->GetProperty("unselected color"))->GetValue();
            unselectedColor[0] = tmpColor[0];
            unselectedColor[1] = tmpColor[1];
            unselectedColor[2] = tmpColor[2];
//            unselectedColor[3] = 1.0f; // alpha value
        }
        else if (dynamic_cast<mitk::ColorProperty*>(this->GetDataNode()->GetPropertyList(NULL)->GetProperty("unselected color")) != NULL)
        {
            mitk::Color tmpColor = dynamic_cast<mitk::ColorProperty *>(this->GetDataNode()->GetPropertyList(NULL)->GetProperty("unselected color"))->GetValue();
            unselectedColor[0] = tmpColor[0];
            unselectedColor[1] = tmpColor[1];
            unselectedColor[2] = tmpColor[2];
//            unselectedColor[3] = 1.0f; // alpha value
        }

        if (dynamic_cast<mitk::ColorProperty*>(this->GetDataNode()->GetPropertyList(renderer)->GetProperty("selected color")) != NULL)
        {
            mitk::Color tmpColor = dynamic_cast<mitk::ColorProperty *>(this->GetDataNode()->GetPropertyList(renderer)->GetProperty("selected color"))->GetValue();
            selectedColor[0] = tmpColor[0];
            selectedColor[1] = tmpColor[1];
            selectedColor[2] = tmpColor[2];
//            selectedColor[3] = 1.0f; // alpha value
        }
        else if (dynamic_cast<mitk::ColorProperty*>(this->GetDataNode()->GetPropertyList(NULL)->GetProperty("selected color")) != NULL)
        {
            mitk::Color tmpColor = dynamic_cast<mitk::ColorProperty *>(this->GetDataNode()->GetPropertyList(NULL)->GetProperty("selected color"))->GetValue();
            selectedColor[0] = tmpColor[0];
            selectedColor[1] = tmpColor[1];
            selectedColor[2] = tmpColor[2];
//            selectedColor[3] = 1.0f; // alpha value
        }

        ls->m_UnselectedActor->GetProperty()->SetColor(unselectedColor);
        ls->m_UnselectedActor->GetProperty()->SetOpacity(opacity);

        ls->m_SelectedActor->GetProperty()->SetColor(selectedColor);
        ls->m_SelectedActor->GetProperty()->SetOpacity(opacity);
    }
    else
    {
        ls->m_UnselectedActor->VisibilityOff();
        ls-> m_SelectedActor->VisibilityOff();
    }

    if(m_ShowSplinePoints)
    {
        ls->m_SplinePointsActor->VisibilityOn();

        //get selected color property
//        if (dynamic_cast<mitk::ColorProperty*>(this->GetDataNode()->GetPropertyList(renderer)->GetProperty("spline point color")) != NULL)
//        {
//            mitk::Color tmpColor = dynamic_cast<mitk::ColorProperty *>(this->GetDataNode()->GetPropertyList(renderer)->GetProperty("spline point color"))->GetValue();
//            splinePointsColor[0] = tmpColor[0];
//            splinePointsColor[1] = tmpColor[1];
//            splinePointsColor[2] = tmpColor[2];
//        }
//        else if (dynamic_cast<mitk::ColorProperty*>(this->GetDataNode()->GetPropertyList(NULL)->GetProperty("spline point color")) != NULL)
//        {
//            mitk::Color tmpColor = dynamic_cast<mitk::ColorProperty *>(this->GetDataNode()->GetPropertyList(NULL)->GetProperty("spline point color"))->GetValue();
//            splinePointsColor[0] = tmpColor[0];
//            splinePointsColor[1] = tmpColor[1];
//            splinePointsColor[2] = tmpColor[2];

//        }

        GetDataNode()->GetColor(splineColor);
        ls->m_SplinePointsActor->GetProperty()->SetColor(splineColor[0],splineColor[1],splineColor[2]);
        ls->m_SplinePointsActor->GetProperty()->SetOpacity(opacity);
    }
    else
    {
        ls->m_SplinePointsActor->VisibilityOff();
    }

//    if (m_ShowSpline)
//    {
//        ls->m_SplineConnectingLinesActor->VisibilityOn();

//        GetDataNode()->GetColor(splineColor);

//        ls->m_SplineConnectingLinesActor->GetProperty()->SetColor(splineColor[0],splineColor[1],splineColor[2]);
//        ls->m_SplineConnectingLinesActor->GetProperty()->SetOpacity(opacity);
//    }
//    else
//    {
        ls->m_SplineConnectingLinesActor->VisibilityOff();
//    }

    if(needGenerateData)
    {
        // create new vtk render objects (e.g. a circle for a point)
        this->CreateVTKRenderObjects(renderer);
    }
}

void svPathVtkMapper2D::SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer, bool overwrite)
{
//    Superclass::SetDefaultProperties(node, renderer, overwrite);

    node->AddProperty( "show points", mitk::BoolProperty::New(true), renderer, overwrite );
    node->AddProperty( "show spline points", mitk::BoolProperty::New(true), renderer, overwrite );
    node->AddProperty( "point 2D size", mitk::IntProperty::New(100), renderer, overwrite );
    node->AddProperty( "spline point 2D size", mitk::IntProperty::New(60), renderer, overwrite );
    node->AddProperty( "point line width", mitk::IntProperty::New(2), renderer, overwrite );
    node->AddProperty( "point 2D distance to plane", mitk::FloatProperty::New(0.01f), renderer, overwrite );
    node->AddProperty( "unselected color",mitk::ColorProperty::New(0,0,1),renderer, overwrite );
    node->AddProperty( "selected color",mitk::ColorProperty::New(1,0,0),renderer, overwrite );
    node->AddProperty( "spline point color",mitk::ColorProperty::New(0,1,0),renderer, overwrite );

    node->AddProperty( "show spline", mitk::BoolProperty::New(true), renderer, overwrite );
    node->AddProperty( "line width", mitk::IntProperty::New(1), renderer, overwrite );

//    node->AddProperty( "layer", mitk::IntProperty::New(1), renderer, overwrite );
    node->AddProperty( "color", mitk::ColorProperty::New(1.0f, 1.0f, 0.2f), renderer, overwrite);
    node->AddProperty( "opacity", mitk::FloatProperty::New(1.0f), renderer, overwrite );

    Superclass::SetDefaultProperties(node, renderer, overwrite);

}
