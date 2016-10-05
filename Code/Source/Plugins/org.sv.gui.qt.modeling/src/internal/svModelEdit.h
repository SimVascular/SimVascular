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

    void HideSegSelectionWidget();

    void UpdateGUI();

    void SelectItem(const QModelIndex & idx);

    void BlendModel();

    void UpdateFaceListSelection();

    void UpdateFacesAndNodes();

    void UpdateBlendFaceList(int index);

public:

    int GetTimeStep();

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

//    bool m_RemovingNode;

    svModelDataInteractor::Pointer m_DataInteractor;

    long m_ModelSelectFaceObserverTag;
    long m_ModelUpdateFaceObserverTag;

    QmitkStdMultiWidget* m_DisplayWidget;
};

#endif // SVMODELEDIT_H
