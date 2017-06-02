#ifndef SVLOFTINGPREFERENCEPAGE_H
#define SVLOFTINGPREFERENCEPAGE_H

#include <berryIPreferences.h>
#include <berryIQtPreferencePage.h>

namespace Ui {
class svLoftParamWidget;
}

class svLoftingPreferencePage : public QObject, public berry::IQtPreferencePage
{
    Q_OBJECT
    Q_INTERFACES(berry::IPreferencePage)

public:
    svLoftingPreferencePage();
    ~svLoftingPreferencePage();

    void CreateQtControl(QWidget* parent) override;
    QWidget* GetQtControl() const override;
    void Init(berry::IWorkbench::Pointer) override;
    void PerformCancel() override;
    bool PerformOk() override;
    void Update() override;

public slots:

    void SelectionChanged(const QString &text);

private:
  berry::IPreferences::Pointer m_Preferences;
  QScopedPointer<Ui::svLoftParamWidget> m_Ui;
  QWidget* m_Control;

};

#endif // SVLOFTINGPREFERENCEPAGE_H
