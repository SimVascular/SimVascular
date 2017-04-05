#ifndef SVSPLITBCWIDGET_H
#define SVSPLITBCWIDGET_H

#include <QWidget>

namespace Ui {
class svSplitBCWidget;
}

class svSplitBCWidget : public QWidget
{
    Q_OBJECT

public:
    explicit svSplitBCWidget(QWidget *parent = 0);

    ~svSplitBCWidget();

    void UpdateGUI(QString bcType, QString splitTarget);

    std::map<std::string, std::string> GetProps();

    bool SetValues();

    bool IsDouble(QString value);

    bool AreDouble(QString values, int* count = NULL);

    QString GetBCType() {return m_BCType;}

    QString GetSplitTarget() {return m_SplitTarget;}

    double GetTotalValue() {return m_TotalValue;}

    double GetMurrayCoefficient() {return m_MurrayCoefficient;}

    double GetPercentage1() {return m_Percentage1;}
    double GetPercentage2() {return m_Percentage2;}
    double GetPercentage3() {return m_Percentage3;}

public slots:

    void Confirm();
    void Cancel();

signals:
    void accepted();

private:
    Ui::svSplitBCWidget *ui;

    QString m_BCType;
    QString m_SplitTarget;

    double m_TotalValue;
    double m_MurrayCoefficient;
    double m_Percentage1;
    double m_Percentage2;
    double m_Percentage3;
};

#endif // SVSPLITBCWIDGET_H
