#ifndef SVMODELPREFERENCEPAGE_H
#define SVMODELPREFERENCEPAGE_H

#include <berryIPreferences.h>
#include <berryIQtPreferencePage.h>

namespace Ui {
class svModelPreferencePage;
}

class svModelPreferencePage : public QObject, public berry::IQtPreferencePage
{
    Q_OBJECT
    Q_INTERFACES(berry::IPreferencePage)

public:
    svModelPreferencePage();
    ~svModelPreferencePage();

    void CreateQtControl(QWidget* parent) override;
    QWidget* GetQtControl() const override;
    void Init(berry::IWorkbench::Pointer) override;
    void PerformCancel() override;
    bool PerformOk() override;
    void Update() override;

private slots:
  void SetNURBSLoftingUDegree();
  void SetNURBSLoftingVDegree();
  void SetNURBSLoftingUKnotSpanType();
  void SetNURBSLoftingVKnotSpanType();
  void SetNURBSLoftingUParametricSpanType();
  void SetNURBSLoftingVParametricSpanType();

private:
  berry::IPreferences::Pointer m_Preferences;
  QScopedPointer<Ui::svModelPreferencePage> m_Ui;
  QWidget* m_Control;

};

#endif // SVSIMULATIONPREFERENCEPAGE_H
