/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef SV4GUI_MESHEDIT_H
#define SV4GUI_MESHEDIT_H

#include "sv4gui_MitkMesh.h"
#include "sv4gui_Model.h"

#include "sv4gui_ModelDataInteractor.h"
#include "sv4gui_DataNodeOperationInterface.h"
#include "sv4gui_LocalTableDelegate.h"
#include "sv4gui_QmitkFunctionality.h"

#include <vtkSphereWidget.h>

#include <QWidget>
#include <QStandardItemModel>
#include <QItemDelegate>
#include <QItemSelection>

namespace Ui {
class sv4guiMeshEdit;
}

class sv4guiMeshEdit : public sv4guiQmitkFunctionality
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    sv4guiMeshEdit();

    virtual ~sv4guiMeshEdit();

public slots:

    void RunMesher();

    void RunHistory();

    void ClearAll();

    void AddObservers();

    void RemoveObservers();

    void UpdateGUI();

    void SetEstimatedEdgeSize();

    void TableFaceListSelectionChanged( const QItemSelection & selected, const QItemSelection & deselected );

    void SetLocal( bool checked = false );

    void ClearLocal( bool checked = false );

    void TableViewLocalContextMenuRequested( const QPoint & pos );

    void TableRegionListSelectionChanged( const QItemSelection & selected, const QItemSelection & deselected );

    void TableDomainsListSelectionChanged( const QItemSelection & selected, const QItemSelection & deselected );

    void DeleteSelectedDomains( bool checked = false );

    void SetSubDomainSize( bool checked = false );

    void SetRegion( bool checked = false );

    void DeleteSelectedRegions( bool checked = false );

    void TableViewRegionContextMenuRequested( const QPoint & pos );

    void TableViewDomainsContextMenuRequested( const QPoint & pos );

    void UpdateFaceListSelection();

    void UpdateTetGenGUI();

    void UpdateMeshSimGUI();

    void UpdateAdaptGUI(int selected);

    void AddSphere();

    void AddHole();

    void AddSubDomain();

    void ShowSphereInteractor(bool checked);

    void DisplayMeshInfo();

    void SetResultFile();

    void Adapt();

    void ShowModel(bool checked = false);

public:

    int GetTimeStep();

    void SetupGUI(QWidget *parent );

    void RunCommands(bool fromGUI = true);

    double EstimateEdgeSize();

    std::vector<std::string> CreateCmdsT();

    std::vector<std::string> CreateCmdsM();

//    static void UpdateSphereData( vtkObject* caller, long unsigned int vtkNotUsed(eventId), void* vtkNotUsed(clientData), void* vtkNotUsed(callData) );

    void UpdateSphereData();

//    std::vector<int> GetSelectedFaceIDs();

    virtual void CreateQtPartControl(QWidget *parent) override;

    virtual void OnSelectionChanged(std::vector<mitk::DataNode*> nodes) override;

    virtual void NodeChanged(const mitk::DataNode* node) override;

    virtual void NodeAdded(const mitk::DataNode* node) override;

    virtual void NodeRemoved(const mitk::DataNode* node) override;

//    virtual void Activated() override;

//    virtual void Deactivated() override;

    virtual void Visible() override;

    virtual void Hidden() override;

    bool IsDouble(QString value);

    bool IsInt(QString value);

    QString GetMeshFolderPath();

protected:

    QWidget* m_Parent;

    Ui::sv4guiMeshEdit *ui;

    sv4guiMitkMesh* m_MitkMesh;

    sv4guiModel* m_Model;

    std::string m_MeshType;

    mitk::DataNode::Pointer m_MeshNode;

    mitk::DataNode::Pointer m_ModelNode;

    sv4guiModelDataInteractor::Pointer m_DataInteractor;

    long m_ModelSelectFaceObserverTag;
//    long m_SphereObserverTag;

    QmitkStdMultiWidget* m_DisplayWidget;

    QMenu* m_TableMenuLocal;
    QStandardItemModel* m_TableModelLocal;

    QMenu* m_TableMenuRegion;
    QStandardItemModel* m_TableModelRegion;

    QMenu* m_TableMenuDomains;
    QStandardItemModel* m_TableModelDomains;

    int m_SelectedRegionIndex;
    int m_SelectedDomainsIndex;

    vtkSmartPointer<vtkSphereWidget> m_SphereWidget;

    bool m_UndoAble;

    sv4guiDataNodeOperationInterface* m_Interface;

    sv4guiLocalTableDelegate* m_CustomDelegate;

    QItemDelegate* m_DefaultDelegate;

};

#endif // SV4GUI_MESHEDIT_H
