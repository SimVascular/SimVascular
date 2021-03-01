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

#include "sv4gui_PathVtkMapper2D.h"

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

sv4guiPathVtkMapper2D::LocalStorage::LocalStorage()
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

sv4guiPathVtkMapper2D::LocalStorage::~LocalStorage()
{
}

const sv4guiPath* sv4guiPathVtkMapper2D::GetInput() const
{
    return static_cast<const sv4guiPath * > ( GetDataNode()->GetData() );
}

// constructor PointSetVtkMapper2D
sv4guiPathVtkMapper2D::sv4guiPathVtkMapper2D()
    : m_ShowSpline(false),
      m_ShowSplinePoints(true),
      m_ShowPoints(true),
      m_LineWidth(1),
      m_PointLineWidth(1),
      m_Point2DSize(100.0f),
      m_DistanceToPlane(4.0f)
{
    m_SplinePoint2DSize=0.6f*m_Point2DSize;
}

sv4guiPathVtkMapper2D::~sv4guiPathVtkMapper2D()
{
}

void sv4guiPathVtkMapper2D::ResetMapper( mitk::BaseRenderer* renderer )
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
    ls->m_PropAssembly->VisibilityOff();
}

// returns propassembly
vtkProp* sv4guiPathVtkMapper2D::GetVtkProp(mitk::BaseRenderer * renderer)
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
    return ls->m_PropAssembly;
}

bool sv4guiPathVtkMapper2D::makePerpendicularVector2D(const mitk::Vector2D& in, mitk::Vector2D& out)
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

//------------------------
// CreateVTKRenderObjects
//------------------------
// Create graphics objects displayed in 2D windows.
//
// This is called when cross-hairs are moved in the 2D windows.
//
void sv4guiPathVtkMapper2D::CreateVTKRenderObjects(mitk::BaseRenderer* renderer)
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);

    // initialize polydata here, otherwise we have update problems when
    // executing this function again
    ls->m_VtkUnselectedPointsPolyData = vtkSmartPointer<vtkPolyData>::New();
    ls->m_VtkSelectedPointsPolyData = vtkSmartPointer <vtkPolyData>::New();
    ls->m_VtkSplinePointsPolyData = vtkSmartPointer<vtkPolyData>::New();
    ls->m_VtkSplineConnectingLinesPolyData = vtkSmartPointer<vtkPolyData>::New();

    sv4guiPath* input  = const_cast<sv4guiPath*>(this->GetInput());

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

    sv4guiPathElement* pathElement=input->GetPathElement(timeStep);

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

    // Process control points.
    //
    if (pathElement->GetControlPointNumber() > 0) {
        const mitk::PlaneGeometry* plane = renderer->GetCurrentWorldPlaneGeometry();
        double precisionFactor = 0.52;
        m_DistanceToPlane = plane->GetExtentInMM(2) * precisionFactor;
        #ifdef dbg_CreateVTKRenderObjects
        auto normal = plane->GetNormal();
        const mitk::BaseGeometry* refGeom = plane->GetReferenceGeometry();	
        auto origin = refGeom->GetOrigin(); 
        std::cout << "[CreateVTKRenderObject] Plane: " << std::endl;
        std::cout << "[CreateVTKRenderObject]   Normal: " << normal[0] << " " << normal[1] << " " << normal[2] << " " << std::endl;
        std::cout << "[CreateVTKRenderObject]   Origin: " << origin[0] << " " << origin[1] << " " << origin[2] << " " << std::endl;
        #endif

        for (int i = 0; i < pathElement->GetControlPointNumber(); i++) {
            mitk::Point3D point = pathElement->GetControlPoint(i);
            bool selected = pathElement->IsControlPointSelected(i);

            // Compute distance to current plane.
            float diff = plane->Distance(point);

            // Draw markers on slices a certain distance away from the points
            // location according to the tolerance threshold (m_DistanceToPlane).
            //
            if (diff*diff < m_DistanceToPlane) {
                if (selected) {
                    ls->m_SelectedPoints->InsertNextPoint(point[0], point[1], point[2]);
                    ls->m_SelectedScales->InsertNextTuple3(control_point_size, 0, 0);
                } else {
                    ls->m_UnselectedPoints->InsertNextPoint(point[0],point[1],point[2]);
                    ls->m_UnselectedScales->InsertNextTuple3(control_point_size,0,0);
                }
            }
        }
    }

    // Process spline points.
    //
    if (pathElement->GetPathPointNumber() > 1) {
        int NumberConnectingPoints = 0;
        bool pointsOnSameSideOfPlane = false;
        mitk::Point3D point = pathElement->GetPathPosPoint(0);
        mitk::Point3D lastPoint = point; 
        const mitk::PlaneGeometry* plane = renderer->GetCurrentWorldPlaneGeometry();

        // Add spline points.
        //
        // The intersection of the line between two adjacent
        // spline points and the current plane is added. 
        //
        for (int i = 0; i < pathElement->GetPathPointNumber(); i++) {
            lastPoint = point;
            point = pathElement->GetPathPosPoint(i);

            if (i > 0) {
                mitk::ScalarType distance = plane->SignedDistance(point);
                mitk::ScalarType lastDistance = plane->SignedDistance(lastPoint);

                if (distance * lastDistance <= 0.0) {
                    mitk::Line3D line;
                    line.SetPoint1(lastPoint);
                    line.SetPoint2(point);

                    mitk::Point3D interPoint;
                    plane->IntersectionPoint(line,interPoint);
                    ls->m_SplinePoints->InsertNextPoint(interPoint[0],interPoint[1],interPoint[2]);
                    ls->m_SplinePointsScales->InsertNextTuple3(path_point_size,0,0);
                }
            }
        }
    }

    // The point set must be transformed in order to obtain the appropriate glyph 
    // orientation according to the current view.
    //
    const mitk::PlaneGeometry* plane = renderer->GetCurrentWorldPlaneGeometry();
    mitk::PlaneGeometry::Pointer pp1 = plane->Clone();
    mitk::Vector3D spacing1 = pp1->GetSpacing();
    spacing1.Fill((spacing1[0]+spacing1[1]+spacing1[2])/3.0);
    pp1->SetSpacing(spacing1);
    mitk::Point3D org1;
    org1.Fill(0);
    pp1->SetOrigin(org1);
    vtkSmartPointer<vtkTransform> transform = vtkSmartPointer<vtkTransform>::New();
    transform->SetMatrix(pp1->GetVtkMatrix());

    // Show unselected points. 
    //
    // Set glyph properties.
    ls->m_UnselectedGlyphSource2D->SetGlyphTypeToDiamond();
    ls->m_UnselectedGlyphSource2D->CrossOn();
    ls->m_UnselectedGlyphSource2D->FilledOff();

    // Apply transform.
    vtkSmartPointer<vtkTransformFilter> transformFilterU = vtkSmartPointer<vtkTransformFilter>::New();
    transformFilterU->SetInputConnection(ls->m_UnselectedGlyphSource2D->GetOutputPort());
    transformFilterU->SetTransform(transform);

    ls->m_VtkUnselectedPointsPolyData->SetPoints(ls->m_UnselectedPoints);
    ls->m_VtkUnselectedPointsPolyData->GetPointData()->SetVectors(ls->m_UnselectedScales);

    // Apply transform of current plane to glyphs.
    ls->m_UnselectedGlyph3D->SetSourceConnection(transformFilterU->GetOutputPort());
    ls->m_UnselectedGlyph3D->SetInputData(ls->m_VtkUnselectedPointsPolyData);
    ls->m_UnselectedGlyph3D->SetScaleModeToScaleByVector();
    ls->m_UnselectedGlyph3D->SetVectorModeToUseVector();

    ls->m_VtkUnselectedPolyDataMapper->SetInputConnection(ls->m_UnselectedGlyph3D->GetOutputPort());
    ls->m_UnselectedActor->SetMapper(ls->m_VtkUnselectedPolyDataMapper);
    ls->m_UnselectedActor->GetProperty()->SetLineWidth(m_PointLineWidth);

    ls->m_PropAssembly->AddPart(ls->m_UnselectedActor);

    // Show selected points. 
    //
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

    // Show spline points. 
    //
    // Apply properties to glyph
    ls->m_SplineGlyphSource2D->SetGlyphTypeToCross();
    ls->m_SplineGlyphSource2D->FilledOff();

    // Apply transform
    vtkSmartPointer<vtkTransformFilter> transformFilterSP = vtkSmartPointer<vtkTransformFilter>::New();
    transformFilterSP->SetInputConnection(ls->m_SplineGlyphSource2D->GetOutputPort());
    transformFilterSP->SetTransform(transform);

    ls->m_VtkSplinePointsPolyData->SetPoints(ls->m_SplinePoints);
    ls->m_VtkSplinePointsPolyData->GetPointData()->SetVectors(ls->m_SplinePointsScales);

    // Apply transform of current plane to glyphs
    ls->m_SplineGlyph3D->SetSourceConnection(transformFilterSP->GetOutputPort());
    ls->m_SplineGlyph3D->SetInputData(ls->m_VtkSplinePointsPolyData);
    ls->m_SplineGlyph3D->SetScaleModeToScaleByVector();
    ls->m_SplineGlyph3D->SetVectorModeToUseVector();

    ls->m_VtkSplinePointsPolyDataMapper->SetInputConnection(ls->m_SplineGlyph3D->GetOutputPort());
    ls->m_SplinePointsActor->SetMapper(ls->m_VtkSplinePointsPolyDataMapper);
    ls->m_SplinePointsActor->GetProperty()->SetLineWidth(m_PointLineWidth);

    ls->m_PropAssembly->AddPart(ls->m_SplinePointsActor);
}

//-------------------------
// GenerateDataForRenderer
//-------------------------
//
void sv4guiPathVtkMapper2D::GenerateDataForRenderer( mitk::BaseRenderer *renderer )
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
    node->GetFloatProperty("point 2D display size",       m_Point2DSize, renderer);
    m_Point2DSize=10*m_Point2DSize;
    m_SplinePoint2DSize=0.6f*m_Point2DSize;

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
        //create new vtk render objects (e.g. a circle for a point)
        this->CreateVTKRenderObjects(renderer);
    }
}

void sv4guiPathVtkMapper2D::SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer, bool overwrite)
{
//    Superclass::SetDefaultProperties(node, renderer, overwrite);

    node->AddProperty( "show points", mitk::BoolProperty::New(true), renderer, overwrite );
    node->AddProperty( "show spline points", mitk::BoolProperty::New(true), renderer, overwrite );
    node->AddProperty( "point 2D display size", mitk::FloatProperty::New(10.0f), renderer, overwrite );
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
