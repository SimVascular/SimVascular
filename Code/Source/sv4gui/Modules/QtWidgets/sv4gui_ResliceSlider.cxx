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

// The MITK classes referenced here is in MITK-2024.06/Plugins/org.mitk.gui.qt.common/src
//
#include "sv4gui_ResliceSlider.h"
#include "sv4gui_SegmentationUtils.h"

#include <QmitkSliceNavigationWidget.h>
#include <QmitkStdMultiWidgetEditor.h>

#include <mitkBaseGeometry.h>
#include <mitkArbitraryTimeGeometry.h>
#include <mitkSliceNavigationController.h>
#include <mitkLookupTable.h>
#include <mitkLookupTableProperty.h>
#include <mitkImage.h>
#include <mitkCameraController.h>
#include <mitkVtkResliceInterpolationProperty.h>

#include <QInputDialog>
#include <QVBoxLayout>

sv4guiResliceSlider::sv4guiResliceSlider(QWidget *parent)
    : QWidget(parent)
    , currentDataNode(nullptr)
    , resliceSize(5.0)
    , m_UseGeometrySize(true)
    , m_UseGeometrySpacing(false)
    , m_UseMinimumSpacing(true)
    , m_ResliceMode(mitk::ExtractSliceFilter::RESLICE_NEAREST)
    , m_StartingSlicePos(0)
{
}

//-----------------
// SetRenderWindow
//-----------------
// This replaces the SetDisplayWidget() method.
//
void sv4guiResliceSlider::SetRenderWindow(mitk::IRenderWindowPart* renderWindow)
{
    #ifdef debug_SetRenderWindow
    std::string msg = "[sv4guiResliceSlider::SetRenderWindow] ";
    std::cout << msg << std::endl;
    std::cout << msg << "========== sv4guiResliceSlider::SetRenderWindow ========== " << std::endl;
    #endif

    m_renderWindow = renderWindow;
    currentSlicedGeometry = nullptr;
    stepperSynchronized = false;

    // Construct vertical box layout objects.
    QVBoxLayout* vlayout = new QVBoxLayout(this);
    vlayout->setContentsMargins(0,0,0,0);
    vlayout->setSpacing(0);

    sliderContainer = new QWidget(this);
    QHBoxLayout* hlayout = new QHBoxLayout(sliderContainer);
    hlayout->setContentsMargins(4,0,0,0);
    sliderContainer->setLayout(hlayout);

    // Set the layout manager for this widget.
    this->setLayout(vlayout);

    resliceCheckBox = new QCheckBox("Turn on Reslicing");
    resliceCheckBox->setChecked(false);
    resliceCheckBox->setToolTip("Show image reslice perpendicular to the path.");

    // Setup axial slice window?
    //
    intensityWindow = m_renderWindow->GetQmitkRenderWindow("axial");
    QmitkSliceNavigationWidget* intensitySlider = new QmitkSliceNavigationWidget;

    // [DaveP] 
    //
    // Connect Qt-based navigators to instances of Stepper.
    //
    intensityStepper = new QmitkStepperAdapter(intensitySlider, intensityWindow->GetSliceNavigationController()->GetStepper());
    // intensityStepper = new QmitkStepperAdapter(intensitySlider, 
    //     intensityWindow->GetSliceNavigationController()->GetSlice(), "IntensityStepper");

    // Setup sagittal slice window?
    //
    potentialWindow = m_renderWindow->GetQmitkRenderWindow("sagittal");
    QmitkSliceNavigationWidget* potentialSlider = new QmitkSliceNavigationWidget;
    potentialSlider->hide();

    // [DaveP] 
    //
    potentialStepper = new QmitkStepperAdapter(potentialSlider, potentialWindow->GetSliceNavigationController()->GetStepper());
    // potentialStepper = new QmitkStepperAdapter(potentialSlider, potentialWindow->GetSliceNavigationController()->GetSlice(), 
    //     "PotentialStepper");

    btnResliceSize = new QPushButton("Size");
    btnResliceSize->setToolTip("Change reslice size");

    hlayout->addWidget(new QLabel("Reslice:"));
    hlayout->addWidget(intensitySlider);
    hlayout->addWidget(btnResliceSize);
    sliderContainer->hide();

    vlayout->addWidget(resliceCheckBox);
    vlayout->addWidget(sliderContainer);

    QHashIterator<QString, QmitkRenderWindow *> renderIter(m_renderWindow->GetQmitkRenderWindows());

    while (renderIter.hasNext()) {
      renderIter.next();
      //std::cout << msg << "renderIter.key(): " << renderIter.key() << std::endl;
      //m_Controls->renderWindowComboBox->addItem(renderIter.key());
    }

    // These seem to be the renderers used with the different slice planes.
    //
    //threeDWindow = m_renderWindow->GetQmitkRenderWindow("3d");
    //QmitkSliceNavigationWidget* threeDSlider = new QmitkSliceNavigationWidget;
    //ThreeDStepper = new QmitkStepperAdapter(threeDSlider,
    //    threeDWindow->GetSliceNavigationController()->GetSlice(), "ThreeDStepper");

    coronalWindow = m_renderWindow->GetQmitkRenderWindow("coronal");

    connect(resliceCheckBox, SIGNAL(toggled(bool)), this, SLOT(changeDisplayWidget(bool)));
    connect(btnResliceSize, SIGNAL(clicked()), this, SLOT(updateResliceSize()) );    
}

sv4guiResliceSlider::~sv4guiResliceSlider()
{
}

bool sv4guiResliceSlider::isResliceOn()
{
    return resliceCheckBox->isChecked();
}

bool sv4guiResliceSlider::isStepperSynchronized()
{
    return stepperSynchronized;
}


void sv4guiResliceSlider::setDataNode(mitk::DataNode::Pointer dataNode)
{
    currentDataNode=dataNode;
}

//---------------
// setPathPoints
//---------------
//
void sv4guiResliceSlider::setPathPoints(std::vector<sv4guiPathElement::sv4guiPathPoint> pathPoints)
{
  //std::string msg("[sv4guiResliceSlider::setPathPoints] ");
  //std::cout << msg << "========== setPathPoints ==========" << std::endl;
  //std::cout << msg << "pathPoints.size(): " << pathPoints.size() << std::endl;

  m_PathPoints = pathPoints;

  /*
  std::cout << msg << "pathPoints: " << std::endl;
  for(int i = 0; i < m_PathPoints.size();i++) {
    auto point = m_PathPoints[i].pos;
    std::cout << msg << i << " " << point[0] << " " << point[1] << " " << point[2] << std::endl;
  }
  */
}

//--------------
// getPathPoint
//--------------
//
sv4guiPathElement::sv4guiPathPoint sv4guiResliceSlider::getPathPoint(int index)
{
  std::string msg("[sv4guiResliceSlider::getPathPoint] ");
  std::cout << msg << "========== getPathPoint ==========" << std::endl;
  std::cout << msg << "index: " << index << std::endl;

  sv4guiPathElement::sv4guiPathPoint pathPoint;

  if (index == -1) {
    index = m_PathPoints.size() - 1;
  }

  if ((index > -1) && (index < m_PathPoints.size())) {
    pathPoint = m_PathPoints[index];
  }

  return pathPoint;
}

//---------------------
// getCurrentPathPoint
//---------------------
//
sv4guiPathElement::sv4guiPathPoint sv4guiResliceSlider::getCurrentPathPoint()
{
    return getPathPoint(getCurrentSliceIndex());
}

void sv4guiResliceSlider::setResliceSize(double size)
{
    resliceSize=size;
}

double sv4guiResliceSlider::getResliceSize()
{
    return resliceSize;
}

//---------------
// updateReslice
//---------------
//
// [DaveP] The `threeDWindow` object is something I as testing, was not in the original SV code.
//
void sv4guiResliceSlider::updateReslice()
{
    #ifdef debug_updateReslice
    std::string msg("[sv4guiResliceSlider::updateReslice] ");
    std::cout << msg << "========== updateReslice ==========" << std::endl;
    std::cout << msg << "isResliceOn(): " << isResliceOn() << std::endl;
    std::cout << msg << "m_PathPoints.size(): " << m_PathPoints.size() << std::endl;
    #endif

    if (!isResliceOn()) {
        return;
    }

    if (m_PathPoints.size() == 0) {
        return;
    }

    mitk::Image* image = nullptr;
    mitk::BaseData* baseData = nullptr;

    if (currentDataNode.IsNotNull()) {
        image = dynamic_cast<mitk::Image*>(currentDataNode->GetData());
        baseData = currentDataNode->GetData();
    }

    // Create a ProportionalTimeGeometry object that will contain 
    // all of the 2D image slices for the current time (always 1 for SV).
    //
    #ifdef debug_updateReslice
    std::cout << msg << "resliceSize: " << resliceSize << std::endl;
    #endif
    currentSlicedGeometry = sv4guiSegmentationUtils::CreateSlicedGeometry(m_PathPoints, baseData, resliceSize);

    // Change graphics window layout.
    //
    QmitkStdMultiWidgetEditor *multiWidgetEditor = dynamic_cast<QmitkStdMultiWidgetEditor*>(m_renderWindow);

    if (multiWidgetEditor) {
        QmitkMultiWidgetLayoutManager layoutManager(multiWidgetEditor->GetMultiWidget());
        layoutManager.SetLayoutDesign(QmitkRenderWindowMenu::LayoutDesign::ALL_2D_LEFT_3D_RIGHT);
    } else {
        MITK_ERROR <<  "No MultiWidgetEditor!" << std::endl;
        return;
    }

    // This is displayed in the 3D window.
    //auto threeDController = threeDWindow->GetSliceNavigationController();
    //threeDController->SetInputWorldTimeGeometry(currentSlicedGeometry);
    //threeDController->SetViewDirection(mitk::SliceNavigationController::Original);
    //threeDController->Update();

    // This is displayed in the axial window.
    //
    auto intensityController = intensityWindow->GetSliceNavigationController();
    intensityController->SetInputWorldTimeGeometry(currentSlicedGeometry);
    //intensityController->SetInputWorldGeometry3D(currentSlicedGeometry);

    // [DaveP] Change for SliceNavigationController is no
    intensityController->SetViewDirection(mitk::AnatomicalPlane::Original);
    //intensityController->SetViewDirection(mitk::SliceNavigationController::Original);
    intensityController->Update();

    // This is displayed in the sagittal window.
    // [DaveP]
    auto potentialController = potentialWindow->GetSliceNavigationController();
    potentialController->SetInputWorldTimeGeometry(currentSlicedGeometry);
    //potentialController->SetInputWorldGeometry3D(currentSlicedGeometry);

    // [DaveP]
    potentialController->SetViewDirection(mitk::AnatomicalPlane::Original);
    // potentialController->SetViewDirection(mitk::SliceNavigationController::Original);
    potentialController->Update();

    if (image) {
        // create vtk lookup table
        //vtkLookupTable* vtkLut = vtkLookupTable::New();
        //vtkLut->SetTableRange(0.0,10.0);
        //vtkLut->Build();

        mitk::LookupTable::Pointer mitkLut = mitk::LookupTable::New();
        //mitkLut->SetVtkLookupTable(vtkLut);
        mitkLut->SetType(mitk::LookupTable::GRAYSCALE);
        mitk::LookupTableProperty::Pointer mitkLutProp = mitk::LookupTableProperty::New();
        mitkLutProp->SetLookupTable(mitkLut);

        currentDataNode->SetProperty("LookupTable", mitkLutProp, potentialWindow->GetRenderer());

        // [DaveP] The "show gradient" propertey  requires code changes in MITK.
        currentDataNode->SetBoolProperty("show gradient", true, potentialWindow->GetRenderer());

        //currentDataNode->SetBoolProperty("texture interpolation", true, potentialWindow->GetRenderer());
        //currentDataNode->SetBoolProperty("visible", false, potentialWindow->GetRenderer());
        //float rgb[3]={1.0f, 0.0f, 0.0f};
        //currentDataNode->SetColor(rgb, potentialWindow->GetRenderer());

        auto prop = currentDataNode->GetProperty("in plane resample extent by geometry");

        #ifdef debug_updateReslice
        std::cout << msg << "m_UseGeometrySpacing: " << m_UseGeometrySpacing << std::endl;
        std::cout << msg << "m_UseGeometrySize: " << m_UseGeometrySize << std::endl;
        std::cout << msg << "m_UseMinimumSpacing: " << m_UseMinimumSpacing << std::endl;
        std::cout << msg << "m_ResliceMode: " << m_ResliceMode << std::endl;
        #endif
        auto bool_prop = mitk::BoolProperty::New(true);

        if (m_UseGeometrySpacing) {
            currentDataNode->SetBoolProperty("in plane resample extent by geometry", 
                mitk::BoolProperty::New(true),intensityWindow->GetRenderer());
            currentDataNode->SetBoolProperty("in plane resample extent by geometry", 
                mitk::BoolProperty::New(true),potentialWindow->GetRenderer());
        }

        if (m_UseGeometrySize) {
            //currentDataNode->SetBoolProperty("in plane resample size by geometry", 
            //    mitk::BoolProperty::New(true), threeDWindow->GetRenderer());
            currentDataNode->SetBoolProperty("in plane resample size by geometry", 
                mitk::BoolProperty::New(true), intensityWindow->GetRenderer());
            currentDataNode->SetBoolProperty("in plane resample size by geometry", 
                mitk::BoolProperty::New(true), potentialWindow->GetRenderer());
        }

        if (m_UseMinimumSpacing) {
            currentDataNode->SetBoolProperty("in plane resample extent by minimum spacing", mitk::BoolProperty::New(true), 
                intensityWindow->GetRenderer());
            currentDataNode->SetBoolProperty("in plane resample extent by minimum spacing", mitk::BoolProperty::New(true),
                potentialWindow->GetRenderer());
        }

        mitk::VtkResliceInterpolationProperty::Pointer interProp = mitk::VtkResliceInterpolationProperty::New();

        switch(m_ResliceMode) {
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

        //currentDataNode->SetProperty("reslice interpolation", interProp, threeDWindow->GetRenderer());
        currentDataNode->SetProperty("reslice interpolation", interProp, intensityWindow->GetRenderer());
        currentDataNode->SetProperty("reslice interpolation", interProp, potentialWindow->GetRenderer());
        currentDataNode->Update();
    }

    mitk::DataNode::PropertyListKeyNames refListNames = currentDataNode->GetPropertyListNames();
    #ifdef debug_updateReslice
    std::cout << msg << "PropertyListKeyNames: " << std::endl;
    for (const auto &name : refListNames) {
        std::cout << msg << "  property name: " << name << std::endl;
    }
    #endif

    // These seem to be the renderers used with the different slice planes.
    //
/*
    threeDWindow->GetRenderer()->GetCurrentWorldPlaneGeometryNode()->SetVisibility(true);
    threeDWindow->GetRenderer()->GetCurrentWorldPlaneGeometryNode()->SetBoolProperty("in plane resample size by geometry", 
        mitk::BoolProperty::New(true), m_renderWindow->GetQmitkRenderWindow("3d")->GetRenderer());
*/
    ////threeDWindow->GetRenderer()->GetCurrentWorldPlaneGeometryNode()->SetVisibility(false);
    //threeDWindow->GetRenderer()->GetCameraController()->Fit();

    // Controls display of slice planes in 3D view?
    //
    // from QtWidgets/src/QmitkStdMultiWidget.cpp
    //   "axial" = RenderWindow1
    //   "sagittal" = RenderWindow2
    //   "coronal" =  RenderWindow3
    //   "3d" = RenderWindow4
    //
    // axial
    //intensityWindow->GetRenderer()->GetCurrentWorldPlaneGeometryNode()->SetVisibility(false);
    intensityWindow->GetRenderer()->GetCurrentWorldPlaneGeometryNode()->SetVisibility(true);
    /* this does not seem to do anything.
    */
    intensityWindow->GetRenderer()->GetCurrentWorldPlaneGeometryNode()->SetBoolProperty(
        "in plane resample size by geometry", 
        mitk::BoolProperty::New(true), 
        m_renderWindow->GetQmitkRenderWindow("3d")->GetRenderer());
    intensityWindow->GetRenderer()->GetCameraController()->Fit();

    // sagittal
    potentialWindow->GetRenderer()->GetCurrentWorldPlaneGeometryNode()->SetVisibility(false);
    potentialWindow->GetRenderer()->GetCameraController()->Fit();

    coronalWindow->GetRenderer()->GetCurrentWorldPlaneGeometryNode()->SetVisibility(false);

    if (!stepperSynchronized) {
        connect(intensityStepper, SIGNAL(Refetch()), this, SLOT(intensityOnRefetch()));
        connect(potentialStepper, SIGNAL(Refetch()), this, SLOT(potentialOnRefetch()));
        stepperSynchronized = true;
    }

    mitk::RenderingManager::GetInstance()->ForceImmediateUpdateAll();
    sliderContainer->show();
    setSlicePos(m_StartingSlicePos);
}

//----------------------
// getCurrentSliceIndex
//----------------------
//
int sv4guiResliceSlider::getCurrentSliceIndex(){
    return intensityWindow->GetSliceNavigationController()->GetStepper()->GetPos();
    // [DaveP] return intensityWindow->GetSliceNavigationController()->GetSlice()->GetPos();
}

void sv4guiResliceSlider::intensityOnRefetch()
{
    int pos1=intensityWindow->GetSliceNavigationController()->GetStepper()->GetPos();
    int pos2=potentialWindow->GetSliceNavigationController()->GetStepper()->GetPos();

    // [DaveP] int pos1=intensityWindow->GetSliceNavigationController()->GetSlice()->GetPos();
    // [DaveP] int pos2=potentialWindow->GetSliceNavigationController()->GetSlice()->GetPos();

    if(pos1!=pos2)
    {
        // [DaveP] 
        potentialWindow->GetSliceNavigationController()->GetStepper()->SetPos(pos1);
        // potentialWindow->GetSliceNavigationController()->GetSlice()->SetPos(pos1);

        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }

    emit reslicePositionChanged(pos1);
}


void sv4guiResliceSlider::potentialOnRefetch()
{
    int pos1 = intensityWindow->GetSliceNavigationController()->GetStepper()->GetPos();
    int pos2 = potentialWindow->GetSliceNavigationController()->GetStepper()->GetPos();
    // [DaveP] int pos1=intensityWindow->GetSliceNavigationController()->GetSlice()->GetPos();
    // [DaveP] int pos2=potentialWindow->GetSliceNavigationController()->GetSlice()->GetPos();

    if(pos1!=pos2)
    {
       // [DaveP] 
       intensityWindow->GetSliceNavigationController()->GetStepper()->SetPos(pos2);
       //intensityWindow->GetSliceNavigationController()->GetSlice()->SetPos(pos2);
       mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }
}

//-----------------------
// restoreDisplayWidget
//-----------------------
//
void sv4guiResliceSlider::restoreDisplayWidget()
{
   std::cout << "[sv4guiResliceSlider::restoreDisplayWidget] " << std::endl;
   std::cout << "[sv4guiResliceSlider::restoreDisplayWidget] ========== restoreDisplayWidget ==========" << std::endl;

   if(currentDataNode.IsNull()) {
      return;
   }

    if(stepperSynchronized){
        disconnect(intensityStepper, SIGNAL(Refetch()), this, SLOT(intensityOnRefetch()));
        disconnect(potentialStepper, SIGNAL(Refetch()), this, SLOT(potentialOnRefetch()));
        stepperSynchronized=false;
    }

    mitk::Image* image=nullptr;
    mitk::BaseData* baseData=nullptr;
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

    intensityWindow->GetRenderer()->GetCurrentWorldPlaneGeometryNode()->GetPropertyList(m_renderWindow->
        GetQmitkRenderWindow("3D")->GetRenderer())->DeleteProperty("in plane resample size by geometry");
    intensityWindow->GetRenderer()->GetCurrentWorldPlaneGeometryNode()->SetVisibility(true);
    potentialWindow->GetRenderer()->GetCurrentWorldPlaneGeometryNode()->SetVisibility(true);
    coronalWindow->GetRenderer()->GetCurrentWorldPlaneGeometryNode()->SetVisibility(true);

    // displayWidget->changeLayoutToDefault();

    if ( baseData && baseData->GetTimeGeometry()->IsValid() )
    {
        mitk::RenderingManager::GetInstance()->InitializeViews(
                    baseData->GetTimeGeometry(), mitk::RenderingManager::REQUEST_UPDATE_ALL, true );
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }

    sliderContainer->hide();
}

void sv4guiResliceSlider::changeDisplayWidget(bool checked)
{
    if(checked){
        updateReslice();
    }else{
        restoreDisplayWidget();
    }
}

void sv4guiResliceSlider::turnOnReslice(bool on)
{
    resliceCheckBox->setChecked(on);
}


void sv4guiResliceSlider::setCheckBoxVisible(bool visible)
{
    resliceCheckBox->setVisible(visible);
}


void sv4guiResliceSlider::setSlicePos(int pos)
{
    // [DaveP] 
    intensityWindow->GetSliceNavigationController()->GetStepper()->SetPos(pos);
    potentialWindow->GetSliceNavigationController()->GetStepper()->SetPos(pos);
    //intensityWindow->GetSliceNavigationController()->GetSlice()->SetPos(pos);
    //potentialWindow->GetSliceNavigationController()->GetSlice()->SetPos(pos);

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

int sv4guiResliceSlider::GetSliceNumber()
{
    if (currentSlicedGeometry.IsNull()) {
        return 0;
    } else {
        // [DaveP] change method name.
        return currentSlicedGeometry->CountTimeSteps();
        //return currentSlicedGeometry->GetSlices();
    }
}

void sv4guiResliceSlider::moveToPathPosPoint(mitk::Point3D posPoint)
{
  for(int i=0;i<m_PathPoints.size();i++) {
    if (posPoint == m_PathPoints[i].pos) {
      setSlicePos(i);
      break;
    }
  }
}

void sv4guiResliceSlider::moveToClosestPathPosPoint(mitk::Point3D posPoint)
{
  //std::string msg("[sv4guiResliceSlider::moveToClosestPathPosPoint] ");
  //std::cout << msg << "========== moveToClosestPathPosPoint ==========" << std::endl;

  int index=0;
  double dis,minDis;

  for(int i=0;i<m_PathPoints.size();i++) {
    dis = m_PathPoints[i].pos.EuclideanDistanceTo(posPoint);

    if (dis == 0.0) {
      index=i;
      break;
    }

    if (i == 0) {
      minDis = dis;

    } else if (dis < minDis) {
      minDis = dis;
      index = i;
    }
  }

  //std::cout << msg << "index: " << index << std::endl;
  setSlicePos(index);
}

//-------------------
// updateResliceSize
//-------------------
// Change the reslice rectangle size.

void sv4guiResliceSlider::updateResliceSize()
{
    bool ok;
    double maxResliceWindowSize = 1000.0, minResliceWindowSize = 0.1;
    auto title = QString(tr("Change Reslice Rectangle Size"));
    auto label = QString(tr("Reslice Size"));
    int numDecimals = 2;
    double newSize = QInputDialog::getDouble(this, title, label, resliceSize, minResliceWindowSize, 
        maxResliceWindowSize, numDecimals, &ok);

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

void sv4guiResliceSlider::SetUseGeometrySize(bool use)
{
    m_UseGeometrySize=use;
}

void sv4guiResliceSlider::SetUseGeometrySpacing(bool use)
{
    m_UseGeometrySpacing=use;
}

void sv4guiResliceSlider::SetUseMinimumSpacing(bool use)
{
    m_UseMinimumSpacing=use;
}

void sv4guiResliceSlider::SetResliceMode(mitk::ExtractSliceFilter::ResliceInterpolation mode)
{
    m_ResliceMode=mode;
}

void sv4guiResliceSlider::SetStartingSlicePos(int pos)
{
    m_StartingSlicePos=pos;
}
