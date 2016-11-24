#ifndef SVCAPBCWIDGET_H
#define SVCAPBCWIDGET_H

#include <QWidget>

namespace Ui {
class svCapBCWidget;
}

class svCapBCWidget : public QWidget
{
    Q_OBJECT

public:
    explicit svCapBCWidget(QWidget *parent = 0);

    ~svCapBCWidget();

    void UpdateGUI(std::string capName, std::map<std::string, std::string> props);

    std::map<std::string, std::string> GetProps();

    bool CreateProps();

    bool IsDouble(QString value);

    bool AreDouble(QString values, int* count = NULL);

public slots:

    void Confirm();
    void Cancel();

    void SelectionChanged(const QString &text);

    void LoadFlowrateFromFile();

signals:
    void accepted();

private:
    Ui::svCapBCWidget *ui;

//    QString m_FileName;
    std::string m_FlowrateContent;

    std::map<std::string, std::string> m_Props;
};

#endif // SVCAPBCWIDGET_H
