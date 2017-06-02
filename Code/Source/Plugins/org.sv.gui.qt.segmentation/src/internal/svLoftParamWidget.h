#ifndef SVLOFTPARAMWIDGET_H
#define SVLOFTPARAMWIDGET_H

#include <org_sv_gui_qt_segmentation_Export.h>

#include "svContourGroup.h"

#include <QWidget>

namespace Ui {
class svLoftParamWidget;
}

class SV_QT_SEGMENTATION svLoftParamWidget : public QWidget
{
    Q_OBJECT

public:
    explicit svLoftParamWidget(QWidget *parent = 0);
    ~svLoftParamWidget();

    void UpdateGUI(svLoftingParam* param);

    void UpdateParam(svLoftingParam* param);

    void SetButtonGroupVisible(bool visible);

    //private:
    Ui::svLoftParamWidget *ui;

public slots:

    void SelectionChanged(const QString &text);


};

#endif // SVLOFTPARAMWIDGET_H
