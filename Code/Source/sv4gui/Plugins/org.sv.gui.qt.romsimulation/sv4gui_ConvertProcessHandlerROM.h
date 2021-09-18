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

#ifndef SV4GUI_CONVERT_PROCESS_HANDLER_ROM_H
#define SV4GUI_CONVERT_PROCESS_HANDLER_ROM_H 

#include "sv4gui_ProcessHandler.h"

#include "sv4gui_MitkROMSimJob.h"
#include "sv4gui_Model.h"
#include "sv4gui_CapBCWidgetROM.h"
#include "sv4gui_SplitBCWidgetROM.h"
#include "sv4gui_QmitkFunctionality.h"

#include "sv4gui_ModelDataInteractor.h"

#include <berryIBerryPreferences.h>

#include <QWidget>
#include <QStandardItemModel>
#include <QProcess>
#include <QMessageBox>

class sv4guiConvertProcessHandlerROM : public QObject
{
    Q_OBJECT

public:
    sv4guiConvertProcessHandlerROM(QProcess* process, int startStep, int totalSteps, QString runDir, QWidget* parent=NULL);
    virtual ~sv4guiConvertProcessHandlerROM();

    void Start();

    void KillProcess();

public slots:

    void AfterProcessFinished(int exitCode, QProcess::ExitStatus exitStatus);

    void UpdateStatus();

    void ProcessError(QProcess::ProcessError error);

private:

    QProcess* m_Process;

    QWidget* m_Parent;

    QMessageBox* m_MessageBox;

    // mitk::DataNode::Pointer m_JobNode;

    QTimer* m_Timer;

    int m_StartStep;

    int m_TotalSteps;

    QString m_RunDir;
};

#endif // SV4GUI_SOLVERPROCESSHANDLERROM_H
