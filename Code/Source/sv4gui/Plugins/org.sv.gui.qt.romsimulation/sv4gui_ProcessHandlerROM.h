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

#ifndef SV4GUI_PROCESSHANDLERROM_H
#define SV4GUI_PROCESSHANDLERROM_H

#include "org_sv_gui_qt_romsimulation_Export.h"

#include "sv4gui_MitkROMSimJob.h"
#include "sv4gui_Model.h"
#include "sv4gui_CapBCWidgetROM.h"
#include "sv4gui_SplitBCWidgetROM.h"
#include "sv4gui_QmitkFunctionality.h"

#include "sv4gui_ModelDataInteractor.h"

//dp #include <berryIBerryPreferences.h>

#include <QWidget>
#include <QStandardItemModel>
#include <QProcess>
#include <QMessageBox>

class SV_QT_ROMSIMULATION sv4guiProcessHandlerROM : public QObject
{
    Q_OBJECT

public:

    sv4guiProcessHandlerROM(QProcess* process, mitk::DataNode::Pointer jobNode, bool multithreading=false, 
        bool stoppable=true, QWidget* parent=nullptr);

    //sv4guiProcessHandlerROM(QProcess* process, mitk::DataNode::Pointer jobNode, bool multithreading=true, 
    //    bool stoppable=true, QWidget* parent=nullptr);

    virtual ~sv4guiProcessHandlerROM();

    void Start();

    QString GetMessage(){return m_Message;}

public slots:

    void AfterProcessFinished(int exitCode, QProcess::ExitStatus exitStatus);

private:

    QProcess* m_Process;

    QWidget* m_Parent;

    QMessageBox* m_MessageBox;

    mitk::DataNode::Pointer m_JobNode;

    bool m_Stoppable;

    bool m_MultiThreading;

    QString m_Message;
};

#endif // SV4GUI_PROCESSHANDLERROM_H
