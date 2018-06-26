#ifndef sv4guisvFSIPREFERENCEPAGE_H
#define sv4guisvFSIPREFERENCEPAGE_H

#include <berryIPreferences.h>
#include <berryIQtPreferencePage.h>

namespace Ui {
class sv4guisvFSIPreferencePage;
}

class sv4guisvFSIPreferencePage : public QObject, public berry::IQtPreferencePage
{
    Q_OBJECT
    Q_INTERFACES(berry::IPreferencePage)

public:
    sv4guisvFSIPreferencePage();
    ~sv4guisvFSIPreferencePage();

    void CreateQtControl(QWidget* parent) override;
    QWidget* GetQtControl() const override;
    void Init(berry::IWorkbench::Pointer) override;
    void PerformCancel() override;
    bool PerformOk() override;
    void Update() override;

private slots:
  void SetFlowsolverPath();

private:
  berry::IPreferences::Pointer m_Preferences;
  QScopedPointer<Ui::sv4guisvFSIPreferencePage> m_Ui;
  QWidget* m_Control;

};

#endif // sv4guisvFSIPREFERENCEPAGE_H
