#ifndef SVSEGSELECTIONWIDGET_H
#define SVSEGSELECTIONWIDGET_H

#include <QWidget>
#include <QAction>
#include <QStandardItemModel>
#include <QMenu>

#include <mitkDataNode.h>
#include <svModelElement.h>

namespace Ui {
class svSegSelectionWidget;
}

class svSegSelectionWidget : public QWidget
{
    Q_OBJECT

public:
    explicit svSegSelectionWidget(QWidget *parent = 0);
    ~svSegSelectionWidget();

    void SetTableView(std::vector<mitk::DataNode::Pointer> segNodes, svModelElement* modelElement, std::string type);

    std::vector<std::string> GetUsedSegNames();

    int GetNumSampling();

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

    Ui::svSegSelectionWidget *ui;

    int m_NumSampling;

    svModelElement* m_ModelElement;

    std::string m_ModelType;

};

#endif // SVSEGSELECTIONWIDGET_H
