#include <svContourGroupVtkMapper3D.h>

#include <vtkPoints.h>
#include <vtkCellArray.h>
#include <vtkProperty.h>
#include <vtkAppendPolyData.h>
#include <vtkSphereSource.h>

svContourGroupVtkMapper3D::svContourGroupVtkMapper3D()
{
}

svContourGroupVtkMapper3D::~svContourGroupVtkMapper3D()
{
}

const svContourGroup* svContourGroupVtkMapper3D::GetInput( void )
{
    return static_cast< const svContourGroup * >( GetDataNode()->GetData() );
}

vtkProp* svContourGroupVtkMapper3D::GetVtkProp(mitk::BaseRenderer* renderer)
{
    return m_LSH.GetLocalStorage(renderer)->m_Assembly;
}

void svContourGroupVtkMapper3D::GenerateDataForRenderer( mitk::BaseRenderer *renderer )
{
    LocalStorage *localStorage = m_LSH.GetLocalStorage(renderer);

    mitk::DataNode* node = GetDataNode();
    if( node == NULL )
        return;

    bool visible = true;
    node->GetVisibility(visible, renderer, "visible");
    if(!visible)
    {
        localStorage->m_Assembly->VisibilityOff();
        return;
    }

    svContourGroup* contourGroup = static_cast< svContourGroup* >(node->GetData() );

    int t = this->GetTimestep();

    //    localStorage->m_Assembly->Delete();

    //    localStorage->m_Assembly=vtkSmartPointer<vtkPropAssembly>::New();

    //    if(contourGroup==NULL||contourGroup->GetSize(t)==0){
    //        localStorage->m_Assembly->VisibilityOff();
    //        return;
    //    }

    localStorage->m_Assembly->VisibilityOn();

    localStorage->m_Assembly->GetParts()->RemoveAllItems();

    int lineWidth=1;
    node->GetIntProperty("line.width", lineWidth, renderer);

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

    float opacity = 1.0f;
    node->GetOpacity(opacity, renderer);

    int index=contourGroup->GetCurrentIndexOn2DView();

    for(int i=0;i<contourGroup->GetSize(t);i++)
    {
        svContour* contour=contourGroup->GetContour(i,t);
        if(contour && contour->GetContourPointNumber()>=2)
        {
            vtkSmartPointer<vtkPolyData> polyData;
            if(i==index){
                polyData= contour->CreateVtkPolyDataFromContour();
            }else{
                polyData= contour->CreateVtkPolyDataFromContour(false);
            }
            vtkSmartPointer<vtkPolyDataMapper> mapper = vtkSmartPointer<vtkPolyDataMapper>::New();
            vtkSmartPointer<vtkActor> actor = vtkSmartPointer<vtkActor>::New();

            mapper->SetInputData(polyData);
            actor->SetMapper(mapper);

            actor->GetProperty()->SetLineWidth(lineWidth);
            if(contour->IsSelected()){
                actor->GetProperty()->SetColor(selectedContourColor);
            }else if(groupSelected){
                actor->GetProperty()->SetColor(selectedColor);
            }else{
                actor->GetProperty()->SetColor(color[0],color[1],color[2]);
            }
            actor->GetProperty()->SetOpacity(opacity);

            localStorage->m_Assembly->AddPart(actor);
        }
    }

    //show control points for the contour which currently appears on 2D View
    svContour* contour=contourGroup->GetContour(index,t);
    if(contour!=NULL)
    {
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

        double m_PointSize = 0.1;
        mitk::FloatProperty::Pointer pointSizeProp = dynamic_cast<mitk::FloatProperty *>(this->GetDataNode()->GetProperty("point.3dsize"));
        if ( pointSizeProp.IsNotNull() )
        {
            m_PointSize = pointSizeProp->GetValue();
        }

        vtkSmartPointer<vtkAppendPolyData> m_vtkSelectedPoints=vtkSmartPointer<vtkAppendPolyData>::New();
        vtkSmartPointer<vtkAppendPolyData> m_vtkUnselectedPoints=vtkSmartPointer<vtkAppendPolyData>::New();

        vtkSmartPointer<vtkPolyDataMapper> m_VtkSelectedPolyDataMapper;
        vtkSmartPointer<vtkPolyDataMapper> m_VtkUnselectedPolyDataMapper;

        vtkSmartPointer<vtkActor> m_SelectedActor;
        vtkSmartPointer<vtkActor> m_UnselectedActor;

        int NumberOfSelectedAdded=0;
        int NumberOfUnselectedAdded=0;

        for (int i=0; i<contour->GetControlPointNumber(); i++)
        {
            vtkSmartPointer<vtkSphereSource> sphere = vtkSmartPointer<vtkSphereSource>::New();
            sphere->SetRadius(m_PointSize/2);
            mitk::Point3D point=contour->GetControlPoint(i);
            sphere->SetCenter(point[0],point[1],point[2]);
            sphere->SetThetaResolution(16);
            sphere->SetPhiResolution(8);

            if (i==contour->GetControlPointSelectedIndex())
            {
                m_vtkSelectedPoints->AddInputConnection(sphere->GetOutputPort());
                ++NumberOfSelectedAdded;
            }
            else
            {
                m_vtkUnselectedPoints->AddInputConnection(sphere->GetOutputPort());
                ++NumberOfUnselectedAdded;
            }
        }

        if(contour->IsPreviewControlPointVisible())
        {
            vtkSmartPointer<vtkSphereSource> sphere = vtkSmartPointer<vtkSphereSource>::New();
            sphere->SetRadius(m_PointSize/2);
            mitk::Point3D point=contour->GetPreviewControlPoint();
            sphere->SetCenter(point[0],point[1],point[2]);
            sphere->SetThetaResolution(16);
            sphere->SetPhiResolution(8);

            m_vtkUnselectedPoints->AddInputConnection(sphere->GetOutputPort());
            ++NumberOfUnselectedAdded;
        }

        if (NumberOfSelectedAdded > 0)
        {
            m_VtkSelectedPolyDataMapper = vtkSmartPointer<vtkPolyDataMapper>::New();
            m_VtkSelectedPolyDataMapper->SetInputConnection(m_vtkSelectedPoints->GetOutputPort());

            m_SelectedActor = vtkSmartPointer<vtkActor>::New();

            m_SelectedActor->SetMapper(m_VtkSelectedPolyDataMapper);
            m_SelectedActor->GetProperty()->SetColor(selectedPointColor);
            localStorage->m_Assembly->AddPart(m_SelectedActor);
        }

        if (NumberOfUnselectedAdded > 0)
        {
            m_VtkUnselectedPolyDataMapper = vtkSmartPointer<vtkPolyDataMapper>::New();
            m_VtkUnselectedPolyDataMapper->SetInputConnection(m_vtkUnselectedPoints->GetOutputPort());

            m_UnselectedActor = vtkSmartPointer<vtkActor>::New();

            m_UnselectedActor->SetMapper(m_VtkUnselectedPolyDataMapper);
            m_UnselectedActor->GetProperty()->SetColor(pointColor);
            localStorage->m_Assembly->AddPart(m_UnselectedActor);
        }
    }

}

void svContourGroupVtkMapper3D::ResetMapper( mitk::BaseRenderer* renderer )
{
    LocalStorage *ls = m_LSH.GetLocalStorage(renderer);
    ls->m_Assembly->VisibilityOff();
}

//void svContourGroupVtkMapper3D::Update(mitk::BaseRenderer* renderer)
//{
//    bool visible = true;
//    GetDataNode()->GetVisibility(visible, renderer, "visible");


//    svContourGroup* data  = static_cast< svContourGroup*>( GetDataNode()->GetData() );
//    if ( data == NULL )
//    {
//        return;
//    }

//    // Calculate time step of the input data for the specified renderer (integer value)
//    this->CalculateTimeStep( renderer );

//    LocalStorage *localStorage = m_LSH.GetLocalStorage(renderer);

//    if ( this->GetTimestep() == -1 )
//    {
//        return;
//    }

//    const mitk::DataNode *node = this->GetDataNode();
//    data->UpdateOutputInformation();

//    //check if something important has changed and we need to rerender
//    if ( (localStorage->m_LastUpdateTime < node->GetMTime()) //was the node modified?
//         || (localStorage->m_LastUpdateTime < data->GetPipelineMTime()) //Was the data modified?
//         || (localStorage->m_LastUpdateTime < renderer->GetCurrentWorldPlaneGeometryUpdateTime()) //was the geometry modified?
//         || (localStorage->m_LastUpdateTime < renderer->GetCurrentWorldPlaneGeometry()->GetMTime())
//         || (localStorage->m_LastUpdateTime < node->GetPropertyList()->GetMTime()) //was a property modified?
//         || (localStorage->m_LastUpdateTime < node->GetPropertyList(renderer)->GetMTime()) )
//    {
//        this->GenerateDataForRenderer( renderer );
//    }

//    // since we have checked that nothing important has changed, we can set
//    // m_LastUpdateTime to the current time
//    localStorage->m_LastUpdateTime.Modified();
//}

//void svContourGroupVtkMapper3D::ApplyContourProperties(mitk::BaseRenderer* renderer)
//{
//    LocalStorage *localStorage = m_LSH.GetLocalStorage(renderer);





//    mitk::ColorProperty::Pointer colorprop = dynamic_cast<mitk::ColorProperty*>(GetDataNode()->GetProperty
//                                                                                ("contour.color", renderer));
//    if(colorprop)
//    {
//        //set the color of the contour
//        double red = colorprop->GetColor().GetRed();
//        double green = colorprop->GetColor().GetGreen();
//        double blue = colorprop->GetColor().GetBlue();


//        vtkSmartPointer<vtkPropCollection> collection = vtkSmartPointer<vtkPropCollection>::New();
//        localStorage->m_Assembly->GetActors(collection);
//        collection->InitTraversal();
//        for(vtkIdType i = 0; i < collection->GetNumberOfItems(); i++)
//        {
//            vtkActor::SafeDownCast(collection->GetNextProp())->GetProperty()->SetColor(red, green, blue);
//        }
//    }
//}

svContourGroupVtkMapper3D::LocalStorage* svContourGroupVtkMapper3D::GetLocalStorage(mitk::BaseRenderer* renderer)
{
    return m_LSH.GetLocalStorage(renderer);
}


svContourGroupVtkMapper3D::LocalStorage::LocalStorage()
{
    m_Assembly = vtkSmartPointer<vtkPropAssembly>::New();
    //    m_contourToPolyData = mitk::ContourModelToSurfaceFilter::New();
}


void svContourGroupVtkMapper3D::SetDefaultProperties(mitk::DataNode* node, mitk::BaseRenderer* renderer, bool overwrite)
{
    node->AddProperty( "line.width", mitk::IntProperty::New(1), renderer, overwrite );
    node->AddProperty( "color", mitk::ColorProperty::New(1.0,1.0,0.0), renderer, overwrite );
    node->AddProperty( "selected.color",mitk::ColorProperty::New(0,1,0),renderer, overwrite );
    node->AddProperty( "selected.contour.color",mitk::ColorProperty::New(1,0,0),renderer, overwrite );
    node->AddProperty( "point.color",mitk::ColorProperty::New(0,1,0),renderer, overwrite );
    node->AddProperty( "selected.point.color",mitk::ColorProperty::New(1,0,0),renderer, overwrite );
    node->AddProperty( "point.3dsize", mitk::FloatProperty::New(0.1), renderer, overwrite);

    Superclass::SetDefaultProperties(node, renderer, overwrite);
}
