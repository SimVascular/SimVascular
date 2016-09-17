#include "svQmitkImageNavigator.h"
#include "ui_svQmitkImageNavigator.h"
#include "svRenderWindowPart.h"

#include <QmitkStdMultiWidget.h>
#include <QmitkStepperAdapter.h>
#include <QmitkRenderWindow.h>

#include <mitkTimeGeometry.h>
#include <mitkPlaneGeometry.h>

svQmitkImageNavigator::svQmitkImageNavigator(QWidget *parent)
  : ui(new Ui::svQmitkImageNavigator)
  , m_AxialStepper(0)
  , m_SagittalStepper(0)
  , m_FrontalStepper(0)
  , m_TimeStepper(0)
{
    CreatePartControl(parent);
}

svQmitkImageNavigator::~svQmitkImageNavigator()
{
}

void svQmitkImageNavigator::CreateQtPartControl( QWidget *parent )
{
    // create GUI widgets
  m_Parent=parent;
  ui->setupUi(parent);
  parent->resize(QSize(150,477).expandedTo(minimumSizeHint()));

  ui->m_SliceNavigatorAxial->SetInverseDirection(true);

  connect(ui->m_XWorldCoordinateSpinBox, SIGNAL(valueChanged(double)), this, SLOT(OnMillimetreCoordinateValueChanged()));
  connect(ui->m_YWorldCoordinateSpinBox, SIGNAL(valueChanged(double)), this, SLOT(OnMillimetreCoordinateValueChanged()));
  connect(ui->m_ZWorldCoordinateSpinBox, SIGNAL(valueChanged(double)), this, SLOT(OnMillimetreCoordinateValueChanged()));

  connect(GetDisplayWidget(),SIGNAL(ViewsInitialized()), this, SLOT(Initialize()));

//  m_Parent->setEnabled(false);
  Initialize();
}

void svQmitkImageNavigator::Initialize()
{

//    this->m_Parent->setEnabled(true);

    svRenderWindowPart* renderWindowPart=GetRenderWindowPart();
//    QmitkRenderWindow* renderWindow = m_DisplayWidget->GetRenderWindow1();
    QmitkRenderWindow* renderWindow = renderWindowPart->GetQmitkRenderWindow("axial");
    if (renderWindow)
    {
      if (m_AxialStepper) m_AxialStepper->deleteLater();
      m_AxialStepper = new QmitkStepperAdapter(ui->m_SliceNavigatorAxial,
                                                     renderWindow->GetSliceNavigationController()->GetSlice(),
                                                     "sliceNavigatorAxial");
      ui->m_SliceNavigatorAxial->setEnabled(true);
      ui->m_AxialLabel->setEnabled(true);
      ui->m_ZWorldCoordinateSpinBox->setEnabled(true);
      connect(m_AxialStepper, SIGNAL(Refetch()), this, SLOT(OnRefetch()));
    }
    else
    {
      ui->m_SliceNavigatorAxial->setEnabled(false);
      ui->m_AxialLabel->setEnabled(false);
      ui->m_ZWorldCoordinateSpinBox->setEnabled(false);
    }

    renderWindow = renderWindowPart->GetQmitkRenderWindow("sagittal");
    if (renderWindow)
    {
      if (m_SagittalStepper) m_SagittalStepper->deleteLater();
      m_SagittalStepper = new QmitkStepperAdapter(ui->m_SliceNavigatorSagittal,
                                                  renderWindow->GetSliceNavigationController()->GetSlice(),
                                                  "sliceNavigatorSagittal");
      ui->m_SliceNavigatorSagittal->setEnabled(true);
      ui->m_SagittalLabel->setEnabled(true);
      ui->m_YWorldCoordinateSpinBox->setEnabled(true);
      connect(m_SagittalStepper, SIGNAL(Refetch()), this, SLOT(OnRefetch()));
    }
    else
    {
      ui->m_SliceNavigatorSagittal->setEnabled(false);
      ui->m_SagittalLabel->setEnabled(false);
      ui->m_YWorldCoordinateSpinBox->setEnabled(false);
    }

    renderWindow = renderWindowPart->GetQmitkRenderWindow("coronal");
    if (renderWindow)
    {
      if (m_FrontalStepper) m_FrontalStepper->deleteLater();
      m_FrontalStepper = new QmitkStepperAdapter(ui->m_SliceNavigatorFrontal,
                                                 renderWindow->GetSliceNavigationController()->GetSlice(),
                                                 "sliceNavigatorFrontal");
      ui->m_SliceNavigatorFrontal->setEnabled(true);
      ui->m_CoronalLabel->setEnabled(true);
      ui->m_XWorldCoordinateSpinBox->setEnabled(true);
      connect(m_FrontalStepper, SIGNAL(Refetch()), this, SLOT(OnRefetch()));
    }
    else
    {
      ui->m_SliceNavigatorFrontal->setEnabled(false);
      ui->m_CoronalLabel->setEnabled(false);
      ui->m_XWorldCoordinateSpinBox->setEnabled(false);
    }

    mitk::SliceNavigationController* timeController = renderWindowPart->GetTimeNavigationController();
    if (timeController)
    {
      if (m_TimeStepper) m_TimeStepper->deleteLater();
      m_TimeStepper = new QmitkStepperAdapter(ui->m_SliceNavigatorTime,
                                              timeController->GetTime(),
                                              "sliceNavigatorTime");
      ui->m_SliceNavigatorTime->setEnabled(true);
      ui->m_TimeLabel->setEnabled(true);
    }
    else
    {
      ui->m_SliceNavigatorTime->setEnabled(false);
      ui->m_TimeLabel->setEnabled(false);
    }
  
}

int svQmitkImageNavigator::GetClosestAxisIndex(mitk::Vector3D normal)
{
  // cos(theta) = normal . axis
  // cos(theta) = (a, b, c) . (d, e, f)
  // cos(theta) = (a, b, c) . (1, 0, 0) = a
  // cos(theta) = (a, b, c) . (0, 1, 0) = b
  // cos(theta) = (a, b, c) . (0, 0, 1) = c
  double absCosThetaWithAxis[3];

  for (int i = 0; i < 3; i++)
  {
    absCosThetaWithAxis[i] = fabs(normal[i]);
  }
  int largestIndex = 0;
  double largestValue = absCosThetaWithAxis[0];
  for (int i = 1; i < 3; i++)
  {
    if (absCosThetaWithAxis[i] > largestValue)
    {
      largestValue = absCosThetaWithAxis[i];
      largestIndex = i;
    }
  }
  return largestIndex;
}

void svQmitkImageNavigator::SetBorderColors()
{
    svRenderWindowPart* renderWindowPart=GetRenderWindowPart();
  if (renderWindowPart)
  {
    QmitkRenderWindow* renderWindow = renderWindowPart->GetQmitkRenderWindow("axial");
    QString decoColor = GetDecorationColorOfGeometry(renderWindow);
    if (renderWindow)
    {
      mitk::PlaneGeometry::ConstPointer geometry = renderWindow->GetSliceNavigationController()->GetCurrentPlaneGeometry();
      if (geometry.IsNotNull())
      {
        mitk::Vector3D normal = geometry->GetNormal();
        int axis = this->GetClosestAxisIndex(normal);
        this->SetBorderColor(axis, decoColor);
      }
    }

    renderWindow =  renderWindowPart->GetQmitkRenderWindow("sagittal");
    decoColor = GetDecorationColorOfGeometry(renderWindow);
    if (renderWindow)
    {
      mitk::PlaneGeometry::ConstPointer geometry = renderWindow->GetSliceNavigationController()->GetCurrentPlaneGeometry();
      if (geometry.IsNotNull())
      {
        mitk::Vector3D normal = geometry->GetNormal();
        int axis = this->GetClosestAxisIndex(normal);
        this->SetBorderColor(axis, decoColor);
      }
    }

    renderWindow =  renderWindowPart->GetQmitkRenderWindow("coronal");
    decoColor = GetDecorationColorOfGeometry(renderWindow);
    if (renderWindow)
    {
      mitk::PlaneGeometry::ConstPointer geometry = renderWindow->GetSliceNavigationController()->GetCurrentPlaneGeometry();
      if (geometry.IsNotNull())
      {
        mitk::Vector3D normal = geometry->GetNormal();
        int axis = this->GetClosestAxisIndex(normal);
        this->SetBorderColor(axis, decoColor);
      }
    }
  }
}

QString svQmitkImageNavigator::GetDecorationColorOfGeometry(QmitkRenderWindow* renderWindow)
{
  QColor color;
  float rgb[3] = {1.0f, 1.0f, 1.0f};
  float rgbMax = 255.0f;
  mitk::BaseRenderer::GetInstance(renderWindow->GetVtkRenderWindow())->GetCurrentWorldPlaneGeometryNode()->GetColor(rgb);
  color.setRed(static_cast<int>(rgb[0]*rgbMax + 0.5));
  color.setGreen(static_cast<int>(rgb[1]*rgbMax + 0.5));
  color.setBlue(static_cast<int>(rgb[2]*rgbMax + 0.5));
  QString colorAsString = QString(color.name());
  return colorAsString;
}

void svQmitkImageNavigator::SetBorderColor(int axis, QString colorAsStyleSheetString)
{
  if (axis == 0)
  {
    this->SetBorderColor(ui->m_XWorldCoordinateSpinBox, colorAsStyleSheetString);
  }
  else if (axis == 1)
  {
    this->SetBorderColor(ui->m_YWorldCoordinateSpinBox, colorAsStyleSheetString);
  }
  else if (axis == 2)
  {
    this->SetBorderColor(ui->m_ZWorldCoordinateSpinBox, colorAsStyleSheetString);
  }
}

void svQmitkImageNavigator::SetBorderColor(QDoubleSpinBox *spinBox, QString colorAsStyleSheetString)
{
  assert(spinBox);
  spinBox->setStyleSheet(QString("border: 2px solid ") + colorAsStyleSheetString + ";");
}

void svQmitkImageNavigator::SetStepSizes()
{
  this->SetStepSize(0);
  this->SetStepSize(1);
  this->SetStepSize(2);
}

void svQmitkImageNavigator::SetStepSize(int axis)
{
    svRenderWindowPart* renderWindowPart=GetRenderWindowPart();
  if (renderWindowPart)
  {
    mitk::BaseGeometry::ConstPointer geometry = renderWindowPart->GetQmitkRenderWindow("axial")->GetSliceNavigationController()->GetInputWorldGeometry3D();

    if (geometry.IsNotNull())
    {
      mitk::Point3D crossPositionInIndexCoordinates;
      mitk::Point3D crossPositionInIndexCoordinatesPlus1;
      mitk::Point3D crossPositionInMillimetresPlus1;
      mitk::Vector3D transformedAxisDirection;

      mitk::Point3D crossPositionInMillimetres = renderWindowPart->GetSelectedPosition();
      geometry->WorldToIndex(crossPositionInMillimetres, crossPositionInIndexCoordinates);

      crossPositionInIndexCoordinatesPlus1 = crossPositionInIndexCoordinates;
      crossPositionInIndexCoordinatesPlus1[axis] += 1;

      geometry->IndexToWorld(crossPositionInIndexCoordinatesPlus1, crossPositionInMillimetresPlus1);

      transformedAxisDirection = crossPositionInMillimetresPlus1 - crossPositionInMillimetres;

      int closestAxisInMillimetreSpace = this->GetClosestAxisIndex(transformedAxisDirection);
      double stepSize = transformedAxisDirection.GetNorm();
      this->SetStepSize(closestAxisInMillimetreSpace, stepSize);
    }
  }
}

void svQmitkImageNavigator::SetStepSize(int axis, double stepSize)
{
  if (axis == 0)
  {
    ui->m_XWorldCoordinateSpinBox->setSingleStep(stepSize);
  }
  else if (axis == 1)
  {
    ui->m_YWorldCoordinateSpinBox->setSingleStep(stepSize);
  }
  else if (axis == 2)
  {
    ui->m_ZWorldCoordinateSpinBox->setSingleStep(stepSize);
  }
}

void svQmitkImageNavigator::OnMillimetreCoordinateValueChanged()
{
    svRenderWindowPart* renderWindowPart=GetRenderWindowPart();
  if (renderWindowPart)
  {
    mitk::TimeGeometry::ConstPointer geometry =  renderWindowPart->GetQmitkRenderWindow("axial")->GetSliceNavigationController()->GetInputWorldTimeGeometry();

    if (geometry.IsNotNull())
    {
      mitk::Point3D positionInWorldCoordinates;
      positionInWorldCoordinates[0] = ui->m_XWorldCoordinateSpinBox->value();
      positionInWorldCoordinates[1] = ui->m_YWorldCoordinateSpinBox->value();
      positionInWorldCoordinates[2] = ui->m_ZWorldCoordinateSpinBox->value();

      renderWindowPart->SetSelectedPosition(positionInWorldCoordinates);
    }
  }
}

void svQmitkImageNavigator::OnRefetch()
{
    svRenderWindowPart* renderWindowPart=GetRenderWindowPart();
  if (renderWindowPart)
  {
    mitk::BaseGeometry::ConstPointer geometry = renderWindowPart->GetQmitkRenderWindow("axial")->GetSliceNavigationController()->GetInputWorldGeometry3D();
    mitk::TimeGeometry::ConstPointer timeGeometry = renderWindowPart->GetQmitkRenderWindow("axial")->GetSliceNavigationController()->GetInputWorldTimeGeometry();

    if (geometry.IsNull() && timeGeometry.IsNotNull())
    {
      mitk::TimeStepType timeStep = renderWindowPart->GetQmitkRenderWindow("axial")->GetSliceNavigationController()->GetTime()->GetPos();
      geometry = timeGeometry->GetGeometryForTimeStep(timeStep);
    }

    if (geometry.IsNotNull())
    {
      mitk::BoundingBox::BoundsArrayType bounds = geometry->GetBounds();

      mitk::Point3D cornerPoint1InIndexCoordinates;
      cornerPoint1InIndexCoordinates[0] = bounds[0];
      cornerPoint1InIndexCoordinates[1] = bounds[2];
      cornerPoint1InIndexCoordinates[2] = bounds[4];

      mitk::Point3D cornerPoint2InIndexCoordinates;
      cornerPoint2InIndexCoordinates[0] = bounds[1];
      cornerPoint2InIndexCoordinates[1] = bounds[3];
      cornerPoint2InIndexCoordinates[2] = bounds[5];

      if (!geometry->GetImageGeometry())
      {
        cornerPoint1InIndexCoordinates[0] += 0.5;
        cornerPoint1InIndexCoordinates[1] += 0.5;
        cornerPoint1InIndexCoordinates[2] += 0.5;
        cornerPoint2InIndexCoordinates[0] -= 0.5;
        cornerPoint2InIndexCoordinates[1] -= 0.5;
        cornerPoint2InIndexCoordinates[2] -= 0.5;
      }

      mitk::Point3D crossPositionInWorldCoordinates = renderWindowPart->GetSelectedPosition();

      mitk::Point3D cornerPoint1InWorldCoordinates;
      mitk::Point3D cornerPoint2InWorldCoordinates;

      geometry->IndexToWorld(cornerPoint1InIndexCoordinates, cornerPoint1InWorldCoordinates);
      geometry->IndexToWorld(cornerPoint2InIndexCoordinates, cornerPoint2InWorldCoordinates);

      ui->m_XWorldCoordinateSpinBox->blockSignals(true);
      ui->m_YWorldCoordinateSpinBox->blockSignals(true);
      ui->m_ZWorldCoordinateSpinBox->blockSignals(true);

      ui->m_XWorldCoordinateSpinBox->setMinimum(std::min(cornerPoint1InWorldCoordinates[0], cornerPoint2InWorldCoordinates[0]));
      ui->m_YWorldCoordinateSpinBox->setMinimum(std::min(cornerPoint1InWorldCoordinates[1], cornerPoint2InWorldCoordinates[1]));
      ui->m_ZWorldCoordinateSpinBox->setMinimum(std::min(cornerPoint1InWorldCoordinates[2], cornerPoint2InWorldCoordinates[2]));
      ui->m_XWorldCoordinateSpinBox->setMaximum(std::max(cornerPoint1InWorldCoordinates[0], cornerPoint2InWorldCoordinates[0]));
      ui->m_YWorldCoordinateSpinBox->setMaximum(std::max(cornerPoint1InWorldCoordinates[1], cornerPoint2InWorldCoordinates[1]));
      ui->m_ZWorldCoordinateSpinBox->setMaximum(std::max(cornerPoint1InWorldCoordinates[2], cornerPoint2InWorldCoordinates[2]));

      ui->m_XWorldCoordinateSpinBox->setValue(crossPositionInWorldCoordinates[0]);
      ui->m_YWorldCoordinateSpinBox->setValue(crossPositionInWorldCoordinates[1]);
      ui->m_ZWorldCoordinateSpinBox->setValue(crossPositionInWorldCoordinates[2]);

      ui->m_XWorldCoordinateSpinBox->blockSignals(false);
      ui->m_YWorldCoordinateSpinBox->blockSignals(false);
      ui->m_ZWorldCoordinateSpinBox->blockSignals(false);
    }

    this->SetBorderColors();

  }
}
