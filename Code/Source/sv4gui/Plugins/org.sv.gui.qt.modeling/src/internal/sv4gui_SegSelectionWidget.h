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

#ifndef SV4GUI_SEGSELECTIONWIDGET_H
#define SV4GUI_SEGSELECTIONWIDGET_H

#include <QWidget>
#include <QAction>
#include <QStandardItemModel>
#include <QMenu>

#include <mitkDataNode.h>
#include "sv4gui_ModelElement.h"
#include "sv4gui_ContourGroup.h"
#include "sv4gui_LoftParamWidget.h"

namespace Ui {
class sv4guiSegSelectionWidget;
}

class sv4guiSegSelectionWidget : public QWidget
{
    Q_OBJECT

public:
    explicit sv4guiSegSelectionWidget(QWidget *parent = 0);
    ~sv4guiSegSelectionWidget();

    void SetTableView(std::vector<mitk::DataNode::Pointer> segNodes, sv4guiModelElement* modelElement, std::string type);

    std::vector<std::string> GetUsedSegNames();

    int GetNumSampling();
    int IfUseUniform();
    svLoftingParam GetLoftingParam();

public slots:

    void TableViewContextMenuRequested( const QPoint & index );

    void UseSelected ( bool checked = false );
    void UseAll ( bool checked = false );
    void NotUseSelected ( bool checked = false );
    void UseNone ( bool checked = false );

    void Confirm();
    void Cancel();

    void OKLofting();

    void ApplyLofting();

    void HideLoftWidget();

    void ShowLoftWidget();

signals:
    void accepted();

private:
    QMenu* m_NodeMenu;

    QStandardItemModel* m_TableModel;

    Ui::sv4guiSegSelectionWidget *ui;

    int m_NumSampling;

//    int m_UseUniform;

    sv4guiModelElement* m_ModelElement;

    std::string m_ModelType;

    svLoftingParam m_Param;
    sv4guiLoftParamWidget* m_LoftWidget;

};

#endif // SV4GUI_SEGSELECTIONWIDGET_H
