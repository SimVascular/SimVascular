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

#ifndef SV4GUI_PATHEDIT_H
#define SV4GUI_PATHEDIT_H

#include "sv4gui_PathSmooth.h"
#include "sv4gui_PathCreate.h"
#include "sv4gui_Path.h"
#include "sv4gui_PathDataInteractor.h"
#include "sv4gui_PathCreate.h"
#include "sv4gui_QmitkFunctionality.h"

#include <QmitkStdMultiWidget.h>

#include <mitkDataStorage.h>
#include <mitkDataNode.h>
#include <mitkDataInteractor.h>
#include <mitkImage.h>
#include <QItemSelection>

namespace Ui {
  class sv4guiPathEdit;
}

class sv4guiPathEdit : public sv4guiQmitkFunctionality
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    sv4guiPathEdit();

    virtual ~sv4guiPathEdit();

public slots:

    void ChangePath();

    void AddPoint(mitk::Point3D);

    void SmoothCurrentPath();

    void ClearAll();

    void SelectPoint();

    void SelectPoint(const QModelIndex & idx);

    void DeleteSelected();

    void SmartAdd();

    void ManuallyAdd();

    void UpdateGUI();

    void SetupResliceSlider();

    void UpdateSlice();

    void UpdatePathResliceSize(double newSize);

    void UpdateAddingMode(int mode);

    void NewPath();

public:

    void SelectPoint(int index);

    int GetTimeStep();

    double GetVolumeImageSpacing();

    void GetImageRealBounds(double realBounds[6]);

    virtual void CreateQtPartControl(QWidget *parent) override;

    //    virtual void OnSelectionChanged(const QList<mitk::DataNode::Pointer>& nodes ) override;
    virtual void OnSelectionChanged(std::vector<mitk::DataNode*> nodes) override;

    virtual void NodeChanged(const mitk::DataNode* node) override;

    virtual void NodeAdded(const mitk::DataNode* node) override;

    virtual void NodeRemoved(const mitk::DataNode* node) override;

//    virtual void Activated() override;

//    virtual void Deactivated() override;

    virtual void Visible() override;

    virtual void Hidden() override;

//    bool IsExclusiveFunctionality() const override;

protected:

    bool eventFilter(QObject *obj, QEvent *ev);

    long m_PathChangeObserverTag;

    long m_PointMoveObserverTag;

    mitk::DataNode::Pointer m_PathNode;

    mitk::DataNode::Pointer m_PathFolderNode;

    sv4guiPath* m_Path;

    Ui::sv4guiPathEdit *ui;

    QWidget* m_Parent;

    //    QmitkPointListView* m_PointListView;

    sv4guiPathDataInteractor::Pointer m_DataInteractor;

    sv4guiPathSmooth* m_SmoothWidget;

    sv4guiPathCreate* m_PathCreateWidget;
    sv4guiPathCreate* m_PathCreateWidget2;

    mitk::Image* m_Image;

    mitk::DataNode::Pointer m_ImageNode;

    QmitkStdMultiWidget* m_DisplayWidget;

    bool m_UpdatingGUI;
};

#endif // SV4GUI_PATHEDIT_H

