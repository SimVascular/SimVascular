
#ifndef SVCAPSELECTIONWIDGET_H
#define SVCAPSELECTIONWIDGET_H

#include <QWidget>
#include <QAction>
#include <QStandardItemModel>
#include <QMenu>

#include <mitkDataNode.h>
#include "svModelElement.h"
#include "svContourGroup.h"
#include "svLoftParamWidget.h"

namespace Ui {
class svCapSelectionWidget;
}

class svCapSelectionWidget : public QWidget
{
    Q_OBJECT

public:
    explicit svCapSelectionWidget(QWidget *parent = 0);
    ~svCapSelectionWidget();

    void SetTableView(std::vector<std::string> caps, svModelElement* modelElement, std::string type);

    std::vector<std::string> GetUsedCapNames();

public slots:

    void TableViewContextMenuRequested( const QPoint & index );

    void UseSelected ( bool checked = false );
    void UseAll ( bool checked = false );
    void NotUseSelected ( bool checked = false );
    void UseNone ( bool checked = false );

    void Confirm();
    void Cancel();

signals:
    void accepted();

private:
    QMenu* m_NodeMenu;

    QStandardItemModel* m_TableModel;

    Ui::svCapSelectionWidget *ui;

    svModelElement* m_ModelElement;

    std::string m_ModelType;
};

#endif // SVSEGSELECTIONWIDGET_H
