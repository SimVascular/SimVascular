#ifndef SVSIMULATIONPREFERENCEPAGE_H
#define SVSIMULATIONPREFERENCEPAGE_H

#include <berryIPreferences.h>
#include <berryIQtPreferencePage.h>

namespace Ui {
class svSimulationPreferencePage;
}

class svSimulationPreferencePage : public QObject, public berry::IQtPreferencePage
{
    Q_OBJECT
    Q_INTERFACES(berry::IPreferencePage)

public:
    svSimulationPreferencePage();
    ~svSimulationPreferencePage();

    void CreateQtControl(QWidget* parent) override;
    QWidget* GetQtControl() const override;
    void Init(berry::IWorkbench::Pointer) override;
    void PerformCancel() override;
    bool PerformOk() override;
    void Update() override;

private slots:
  void SetPresolverPath();
  void SetFlowsolverPath();
  void SetMPIExecPath();
  void SetCustomTemplatePath();
  void SetPostsolverPath();

private:
  berry::IPreferences::Pointer m_Preferences;
  QScopedPointer<Ui::svSimulationPreferencePage> m_Ui;
  QWidget* m_Control;

};

#endif // SVSIMULATIONPREFERENCEPAGE_H
