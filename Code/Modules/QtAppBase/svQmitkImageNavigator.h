#ifndef SVQMITKIMAGENAVIGATOR_H
#define SVQMITKIMAGENAVIGATOR_H

#include "SimVascular.h"

#include <svQtAppBaseExports.h>

#include "svAbstractView.h"

namespace Ui {
class svQmitkImageNavigator;
}

class QmitkStepperAdapter;

class QDoubleSpinBox;

class SVQTAPPBASE_EXPORT svQmitkImageNavigator : public svAbstractView
{

  Q_OBJECT

public:

  svQmitkImageNavigator(QWidget *parent = 0);

  virtual ~svQmitkImageNavigator();

protected slots:

  void OnMillimetreCoordinateValueChanged();
  void OnRefetch();
  void Initialize();

protected:

    virtual void CreateQtPartControl(QWidget *parent) override;

    void SetBorderColors();
    void SetBorderColor(QDoubleSpinBox *spinBox, QString colorAsStyleSheetString);
    void SetBorderColor(int axis, QString colorAsStyleSheetString);

    void SetStepSizes();
    void SetStepSize(int axis);
    void SetStepSize(int axis, double stepSize);

    int  GetClosestAxisIndex(mitk::Vector3D normal);

    QString GetDecorationColorOfGeometry(QmitkRenderWindow *renderWindow);

    Ui::svQmitkImageNavigator *ui;

    QmitkStepperAdapter* m_AxialStepper;
    QmitkStepperAdapter* m_SagittalStepper;
    QmitkStepperAdapter* m_FrontalStepper;
    QmitkStepperAdapter* m_TimeStepper;

    QWidget* m_Parent;

};

#endif // SVQMITKIMAGENAVIGATOR_H
