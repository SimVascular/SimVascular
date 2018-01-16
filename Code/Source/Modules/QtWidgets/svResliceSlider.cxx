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

#include "svResliceSlider.h"
#include "svSegmentationUtils.h"

#include <QmitkSliderNavigatorWidget.h>

#include <mitkSliceNavigationController.h>
#include <mitkLookupTable.h>
#include <mitkLookupTableProperty.h>
#include <mitkImage.h>
#include <mitkCameraController.h>
#include <mitkVtkResliceInterpolationProperty.h>

#include <QInputDialog>

svResliceSlider::svResliceSlider(QWidget *parent)
    : QWidget(parent)
    , currentDataNode(NULL)
    , resliceSize(5.0)
    , m_UseGeometrySize(true)
    , m_UseGeometrySpacing(false)
    , m_UseMinimumSpacing(true)
    , m_ResliceMode(mitk::ExtractSliceFilter::RESLICE_NEAREST)
    , m_StartingSlicePos(0)
{
}

void svResliceSlider::SetDisplayWidget(QmitkStdMultiWidget* widget)
{
    displayWidget=widget;

    currentSlicedGeometry=NULL;

    stepperSynchronized=false;

    QVBoxLayout* vlayout = new QVBoxLayout(this);
    vlayout->setContentsMargins(0,0,0,0);
    vlayout->setSpacing(0);

    sliderContainer=new QWidget(this);
    QHBoxLayout* hlayout = new QHBoxLayout(sliderContainer);
    hlayout->setContentsMargins(4,0,0,0);
    sliderContainer->setLayout(hlayout);

    setLayout(vlayout);

    resliceCheckBox=new QCheckBox("Turn on Reslicing");
    //    resliceCheckBox->setFixedWidth(80);
    resliceCheckBox->setChecked(false);
    resliceCheckBox->setToolTip("Show image reslice perpendicular to the path.");

    intensityWindow=displayWidget->GetRenderWindow1();
    QmitkSliderNavigatorWidget* intensitySlider=new QmitkSliderNavigatorWidget;
    //    intensitySlider->hide();
    intensityStepper = new QmitkStepperAdapter(intensitySlider,
                                               intensityWindow->GetSliceNavigationController()->GetSlice(),
                                               "IntensityStepper");

    potentialWindow=displayWidget->GetRenderWindow2();
    QmitkSliderNavigatorWidget* potentialSlider=new QmitkSliderNavigatorWidget;
    potentialSlider->hide();
    potentialStepper = new QmitkStepperAdapter(potentialSlider,
                                               potentialWindow->GetSliceNavigationController()->GetSlice(),
                                               "PotentialStepper");

    btnResliceSize=new QPushButton("Size");
    btnResliceSize->setToolTip("Change reslice size");

    hlayout->addWidget(new QLabel("Reslice:"));
    hlayout->addWidget(intensitySlider);
    hlayout->addWidget(btnResliceSize);
    //    hlayout->addWidget(potentialSlider);
    sliderContainer->hide();

    vlayout->addWidget(resliceCheckBox);
    vlayout->addWidget(sliderContainer);
    //    vlayout->addStretch();

    coronalWindow=displayWidget->GetRenderWindow3();

    connect(resliceCheckBox, SIGNAL(toggled(bool)), this, SLOT(changeDisplayWidget(bool)));
    connect(btnResliceSize, SIGNAL(clicked()), this, SLOT(updateResliceSize()) );

}

svResliceSlider::~svResliceSlider()
{
}

bool svResliceSlider::isResliceOn()
{
    return resliceCheckBox->isChecked();
}

bool svResliceSlider::isStepperSynchronized()
{
    return stepperSynchronized;
}


void svResliceSlider::setDataNode(mitk::DataNode::Pointer dataNode)
{
    currentDataNode=dataNode;
}

void svResliceSlider::setPathPoints(std::vector<svPathElement::svPathPoint> pathPoints)
{
    m_PathPoints=pathPoints;
}

svPathElement::svPathPoint svResliceSlider::getPathPoint(int index)
{
    svPathElement::svPathPoint pathPoint;
    if(index==-1) index=m_PathPoints.size()-1;
    if(index>-1&&index<m_PathPoints.size())
        pathPoint=m_PathPoints[index];

    return pathPoint;
}

svPathElement::svPathPoint svResliceSlider::getCurrentPathPoint()
{
    return getPathPoint(getCurrentSliceIndex());
}

void svResliceSlider::setResliceSize(double size)
{
    resliceSize=size;
}

double svResliceSlider::getResliceSize()
{
    return resliceSize;
}

void svResliceSlider::updateReslice()
{

    if(!isResliceOn()) return;

    if(m_PathPoints.size()==0) return;

//    if(!currentDataNode) return;

    mitk::Image* image=NULL;
    mitk::BaseData* baseData=NULL;
    if(currentDataNode.IsNotNull())
    {
        image= dynamic_cast<mitk::Image*>(currentDataNode->GetData());
        baseData=currentDataNode->GetData();
    }

//    if(!image) return;

    //    if(currentSlicedGeometry)
    //    {
    //        currentSlicedGeometry->Delete();
    //        currentSlicedGeometry=NULL;
    //    }

    currentSlicedGeometry=svSegmentationUtils::CreateSlicedGeometry(m_PathPoints, baseData, resliceSize);

    displayWidget->changeLayoutTo2x2Dand3DWidget();

    mitk::SliceNavigationController::Pointer intensityController=intensityWindow->GetSliceNavigationController();
    intensityController->SetInputWorldGeometry3D(currentSlicedGeometry);
    intensityController->SetViewDirection(mitk::SliceNavigationController::Original);
    intensityController->Update();

    mitk::SliceNavigationController::Pointer potentialController=potentialWindow->GetSliceNavigationController();
    potentialController->SetInputWorldGeometry3D(currentSlicedGeometry);
    potentialController->SetViewDirection(mitk::SliceNavigationController::Original);
    potentialController->Update();

    if(image)
    {
        mitk::LookupTable::Pointer mitkLut = mitk::LookupTable::New();
        mitkLut->SetType(mitk::LookupTable::GRAYSCALE);
        mitk::LookupTableProperty::Pointer mitkLutProp = mitk::LookupTableProperty::New();
        mitkLutProp->SetLookupTable(mitkLut);
        currentDataNode->SetProperty("LookupTable", mitkLutProp,potentialWindow->GetRenderer());

        currentDataNode->SetBoolProperty("show gradient", true, potentialWindow->GetRenderer());

        if(m_UseGeometrySpacing)
        {
            currentDataNode->SetBoolProperty( "in plane resample extent by geometry", mitk::BoolProperty::New( true ),intensityWindow->GetRenderer());
            currentDataNode->SetBoolProperty( "in plane resample extent by geometry", mitk::BoolProperty::New( true ),potentialWindow->GetRenderer());
        }

        if(m_UseGeometrySize)
        {
            currentDataNode->SetBoolProperty( "in plane resample size by geometry", mitk::BoolProperty::New( true ),intensityWindow->GetRenderer());
            currentDataNode->SetBoolProperty( "in plane resample size by geometry", mitk::BoolProperty::New( true ),potentialWindow->GetRenderer());
        }

        if(m_UseMinimumSpacing)
        {
            currentDataNode->SetBoolProperty( "in plane resample extent by minimum spacing", mitk::BoolProperty::New( true ),intensityWindow->GetRenderer());
            currentDataNode->SetBoolProperty( "in plane resample extent by minimum spacing", mitk::BoolProperty::New( true ),potentialWindow->GetRenderer());
        }


        mitk::VtkResliceInterpolationProperty::Pointer interProp=mitk::VtkResliceInterpolationProperty::New();
        switch(m_ResliceMode)
        {
        case mitk::ExtractSliceFilter::RESLICE_NEAREST:
            interProp->SetInterpolationToNearest();
            break;
        case mitk::ExtractSliceFilter::RESLICE_LINEAR:
            interProp->SetInterpolationToLinear();
            break;
        case mitk::ExtractSliceFilter::RESLICE_CUBIC:
            interProp->SetInterpolationToCubic();
            break;
        default:
            break;
        }

        currentDataNode->SetProperty("reslice interpolation", interProp,intensityWindow->GetRenderer());
        currentDataNode->SetProperty("reslice interpolation", interProp,potentialWindow->GetRenderer());

    }

    intensityWindow->GetRenderer()->GetCurrentWorldPlaneGeometryNode()->SetVisibility(true);
    intensityWindow->GetRenderer()->GetCurrentWorldPlaneGeometryNode()->SetBoolProperty( "in plane resample size by geometry", mitk::BoolProperty::New( true ),displayWidget->GetRenderWindow4()->GetRenderer());
    potentialWindow->GetRenderer()->GetCurrentWorldPlaneGeometryNode()->SetVisibility(false);
    coronalWindow->GetRenderer()->GetCurrentWorldPlaneGeometryNode()->SetVisibility(false);

    intensityWindow->GetRenderer()->GetCameraController()->Fit();
    potentialWindow->GetRenderer()->GetCameraController()->Fit();

    if(!stepperSynchronized){
        connect(intensityStepper, SIGNAL(Refetch()), this, SLOT(intensityOnRefetch()));
        connect(potentialStepper, SIGNAL(Refetch()), this, SLOT(potentialOnRefetch()));
        stepperSynchronized=true;
    }

    mitk::RenderingManager::GetInstance()->ForceImmediateUpdateAll();

    sliderContainer->show();

    setSlicePos(m_StartingSlicePos);

}

int svResliceSlider::getCurrentSliceIndex(){
    return intensityWindow->GetSliceNavigationController()->GetSlice()->GetPos();
}

void svResliceSlider::intensityOnRefetch()
{
    int pos1=intensityWindow->GetSliceNavigationController()->GetSlice()->GetPos();
    int pos2=potentialWindow->GetSliceNavigationController()->GetSlice()->GetPos();
    if(pos1!=pos2)
    {
        potentialWindow->GetSliceNavigationController()->GetSlice()->SetPos(pos1);
        //        intensityWindow->GetRenderer()->GetDisplayGeometry()->Fit();
        //        intensityWindow->GetRenderer()->GetVtkRenderer()->ResetCamera();
        //        potentialWindow->GetRenderer()->GetDisplayGeometry()->Fit();
        //        potentialWindow->GetRenderer()->GetVtkRenderer()->ResetCamera();
        //            mitk::RenderingManager::GetInstance()->ForceImmediateUpdateAll();
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();

    }

    emit reslicePositionChanged(pos1);
}


void svResliceSlider::potentialOnRefetch()
{
    int pos1=intensityWindow->GetSliceNavigationController()->GetSlice()->GetPos();
    int pos2=potentialWindow->GetSliceNavigationController()->GetSlice()->GetPos();
    if(pos1!=pos2)
    {
        intensityWindow->GetSliceNavigationController()->GetSlice()->SetPos(pos2);
        //        intensityWindow->GetRenderer()->GetDisplayGeometry()->Fit();
        //        intensityWindow->GetRenderer()->GetVtkRenderer()->ResetCamera();
        //        potentialWindow->GetRenderer()->GetDisplayGeometry()->Fit();
        //        potentialWindow->GetRenderer()->GetVtkRenderer()->ResetCamera();
        //            mitk::RenderingManager::GetInstance()->ForceImmediateUpdateAll();
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();

    }
}

void svResliceSlider::restoreDisplayWidget()
{
//    if(currentDataNode.IsNull())
//        return;

    if(stepperSynchronized){
        disconnect(intensityStepper, SIGNAL(Refetch()), this, SLOT(intensityOnRefetch()));
        disconnect(potentialStepper, SIGNAL(Refetch()), this, SLOT(potentialOnRefetch()));
        stepperSynchronized=false;
    }

    mitk::Image* image=NULL;
    mitk::BaseData* baseData=NULL;
    if(currentDataNode.IsNotNull())
    {
        image= dynamic_cast<mitk::Image*>(currentDataNode->GetData());
        baseData=currentDataNode->GetData();
    }

    if(image)
    {
        currentDataNode->GetPropertyList(potentialWindow->GetRenderer())->DeleteProperty("LookupTable");
        currentDataNode->GetPropertyList(potentialWindow->GetRenderer())->DeleteProperty("show gradient");

        currentDataNode->GetPropertyList(potentialWindow->GetRenderer())->DeleteProperty("in plane resample extent by geometry");
        currentDataNode->GetPropertyList(potentialWindow->GetRenderer())->DeleteProperty("in plane resample size by geometry");
        currentDataNode->GetPropertyList(potentialWindow->GetRenderer())->DeleteProperty("in plane resample extent by minimum spacing");
        currentDataNode->GetPropertyList(potentialWindow->GetRenderer())->DeleteProperty("reslice interpolation");

        currentDataNode->GetPropertyList(intensityWindow->GetRenderer())->DeleteProperty("in plane resample extent by geometry");
        currentDataNode->GetPropertyList(intensityWindow->GetRenderer())->DeleteProperty("in plane resample size by geometry");
        currentDataNode->GetPropertyList(intensityWindow->GetRenderer())->DeleteProperty("in plane resample extent by minimum spacing");
        currentDataNode->GetPropertyList(intensityWindow->GetRenderer())->DeleteProperty("reslice interpolation");
    }

    intensityWindow->GetRenderer()->GetCurrentWorldPlaneGeometryNode()->GetPropertyList(displayWidget->GetRenderWindow4()->GetRenderer())->DeleteProperty("in plane resample size by geometry");
    intensityWindow->GetRenderer()->GetCurrentWorldPlaneGeometryNode()->SetVisibility(true);
    potentialWindow->GetRenderer()->GetCurrentWorldPlaneGeometryNode()->SetVisibility(true);
    coronalWindow->GetRenderer()->GetCurrentWorldPlaneGeometryNode()->SetVisibility(true);

    //    if(currentSlicedGeometry)
    //    {
    //        currentSlicedGeometry->Delete();
    //        currentSlicedGeometry=NULL;
    //    }

    displayWidget->changeLayoutToDefault();

    if ( baseData && baseData->GetTimeGeometry()->IsValid() )
    {
        mitk::RenderingManager::GetInstance()->InitializeViews(
                    baseData->GetTimeGeometry(), mitk::RenderingManager::REQUEST_UPDATE_ALL, true );
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }


    sliderContainer->hide();
}

void svResliceSlider::changeDisplayWidget(bool checked)
{
    if(checked){
        updateReslice();
    }else{
        restoreDisplayWidget();
    }
}

void svResliceSlider::turnOnReslice(bool on)
{
    resliceCheckBox->setChecked(on);
}


void svResliceSlider::setCheckBoxVisible(bool visible)
{
    resliceCheckBox->setVisible(visible);
}


void svResliceSlider::setSlicePos(int pos)
{
    intensityWindow->GetSliceNavigationController()->GetSlice()->SetPos(pos);
    potentialWindow->GetSliceNavigationController()->GetSlice()->SetPos(pos);
    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

int svResliceSlider::GetSliceNumber()
{
    if(currentSlicedGeometry.IsNull())
        return 0;
    else
        return currentSlicedGeometry->GetSlices();
}

void svResliceSlider::moveToPathPosPoint(mitk::Point3D posPoint){

    for(int i=0;i<m_PathPoints.size();i++)
    {
        if(posPoint==m_PathPoints[i].pos)
        {
            setSlicePos(i);
            break;
        }
    }
}

void svResliceSlider::moveToClosestPathPosPoint(mitk::Point3D posPoint)
{
    int index=0;
    double dis,minDis;

    for(int i=0;i<m_PathPoints.size();i++)
    {
        dis=m_PathPoints[i].pos.EuclideanDistanceTo(posPoint);
        if(dis==0)
        {
            index=i;
            break;
        }

        if(i==0)
        {
            minDis=dis;
        }
        else if(dis<minDis)
        {
            minDis=dis;
            index=i;
        }
    }

    setSlicePos(index);
}

void svResliceSlider::updateResliceSize()
{
    bool ok;
    double newSize = QInputDialog::getDouble(this, tr("Change Reslice Size"),tr("Reslice Size:")
                                          , resliceSize, 0.1, 100.0, 2, &ok);
    if(ok){
        resliceSize=newSize;
        if(isResliceOn())
        {
            int index=getCurrentSliceIndex();
            updateReslice();
            setSlicePos(index);
        }
         emit resliceSizeChanged(newSize);
    }
}

void svResliceSlider::SetUseGeometrySize(bool use)
{
    m_UseGeometrySize=use;
}

void svResliceSlider::SetUseGeometrySpacing(bool use)
{
    m_UseGeometrySpacing=use;
}

void svResliceSlider::SetUseMinimumSpacing(bool use)
{
    m_UseMinimumSpacing=use;
}

void svResliceSlider::SetResliceMode(mitk::ExtractSliceFilter::ResliceInterpolation mode)
{
    m_ResliceMode=mode;
}

void svResliceSlider::SetStartingSlicePos(int pos)
{
    m_StartingSlicePos=pos;
}
