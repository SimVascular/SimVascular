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

#ifndef SV4GUI_MAINWINDOW_H
#define SV4GUI_MAINWINDOW_H

#include <org_sv_gui_qt_application_Export.h>

#include "SimVascular.h"

#include <QMainWindow>

//#include "svCollapsiblePane.h"

#include <mitkDataStorage.h>
#include "QmitkStdMultiWidget.h"

#include <ctkPythonConsole.h>

#include <vtkRenderer.h>
#include <vtkSmartPointer.h>

#include <vtkRenderWindow.h>

class SV_QT_APPLICATION sv4guiMainWindow : public QMainWindow
{
    Q_OBJECT

public:
    explicit sv4guiMainWindow(QWidget *parent = 0);
    ~sv4guiMainWindow();
    ctkPythonConsole* pythonConsole;

public slots:
    vtkRenderer* getRenderer();

    vtkRenderWindow* getRenderWindow();

protected:

    QmitkStdMultiWidget* m_DisplayWidget;

    mitk::DataStorage::Pointer m_DataStorage;

    void connectViews();

    void closeEvent(QCloseEvent *event) Q_DECL_OVERRIDE;

private slots:

    void on_actionPythonConsole_triggered();

    void on_actionAxialOnly_triggered();

    void on_actionSagittalOnly_triggered();

    void on_actionCoronalOnly_triggered();

    void on_actionLayout2DTop_triggered();

    void on_actionLayout2DLeft_triggered();

    void on_actionLayoutStandard_triggered();

    void on_actionLayout3DOnly_triggered();

private:

    void initializePythonConsole();
};

#endif // SV4GUI_MAINWINDOW_H
