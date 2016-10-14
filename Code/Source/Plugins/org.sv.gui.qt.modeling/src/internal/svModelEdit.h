#ifndef SVMODELEDIT_H
#define SVMODELEDIT_H

#include "svModel.h"
#include "svSegSelectionWidget.h"
#include "svModelDataInteractor.h"

#include <QmitkFunctionality.h>

#include <QWidget>

namespace Ui {
class svModelEdit;
}

class svModelEdit : public QmitkFunctionality
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    svModelEdit();

    virtual ~svModelEdit();

public slots:

    void CreateModel();

    void ClearAll();

    void ShowSegSelectionWidget();

    void UpdateGUI();

    void SelectItem(const QModelIndex & idx);

    void BlendModel();

    void UpdateFaceListSelection();

    void UpdateBlendTable(int index);

    void TableViewBlendContextMenuRequested( const QPoint & index );

    void SetRadius( bool checked = false );

    void ClearRadius( bool checked = false );

    void UseSelectedBlend( bool checked = false );

    void NotUseSelectedBlend( bool checked = false );

    void SetupBlendTable();

    void UpdatePolyDataBlendParam();

    void TableBlendSelectionChanged( const QItemSelection & selected, const QItemSelection & deselected );

    void SetupFaceListTable();

    void UpdateFaceData(QStandardItem* item);

    void TableFaceListSelectionChanged( const QItemSelection & selected, const QItemSelection & deselected );

    void ToggleVisibility(const QModelIndex &index);

    void ChangeColor(const QModelIndex &index);


public:

    int GetTimeStep();

    std::vector<svModelElement::svBlendParamRadius*> GetBlendRadii();

    virtual void CreateQtPartControl(QWidget *parent) override;

    virtual void OnSelectionChanged(std::vector<mitk::DataNode*> nodes) override;

    virtual void NodeChanged(const mitk::DataNode* node) override;

    virtual void NodeAdded(const mitk::DataNode* node) override;

    virtual void NodeRemoved(const mitk::DataNode* node) override;

//    virtual void Activated() override;

//    virtual void Deactivated() override;

    virtual void Visible() override;

    virtual void Hidden() override;

protected:

    QWidget* m_Parent;

    Ui::svModelEdit *ui;

    svModel* m_Model;

    std::string m_ModelType;

    mitk::DataNode::Pointer m_ModelNode;

    svSegSelectionWidget* m_SegSelectionWidget;

    svModelDataInteractor::Pointer m_DataInteractor;

    long m_ModelSelectFaceObserverTag;
    long m_ModelUpdateObserverTag;

    QmitkStdMultiWidget* m_DisplayWidget;

    QMenu* m_BlendTableMenu;
    QStandardItemModel* m_BlendTableModel;

    QMenu* m_FaceListTableMenu;
    QStandardItemModel* m_FaceListTableModel;

};

#endif // SVMODELEDIT_H
