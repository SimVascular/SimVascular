
#ifndef SVWORKBENCHINTROPART_H
#define SVWORKBENCHINTROPART_H

#include <QtCore/qconfig.h>

#include <berryQtIntroPart.h>
#include <ui_svWelcomeScreenViewControls.h>

class QWebView ;

class svWorkbenchIntroPart : public berry::QtIntroPart
{

    // this is needed for all Qt objects that should have a MOC object (everything that derives from QObject)
    Q_OBJECT

public:

    svWorkbenchIntroPart();
    ~svWorkbenchIntroPart();


    virtual void CreateQtPartControl(QWidget *parent) override;

    void StandbyStateChanged(bool) override;

    void SetFocus() override;

    virtual void CreateConnections();


protected slots:


    void DelegateMeTo(const QUrl& ShowMeNext);

protected:

    Ui::svWelcomeScreenViewControls* m_Controls;
    QWebView* m_view;
};

#endif /* SVWORKBENCHINTROPART_H */
