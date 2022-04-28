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

#ifndef SV4GUI_CONVERT_WORKER_ROM_H
#define SV4GUI_CONVERT_WORKER_ROM_H 

/*
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
#include <QThread>
#include <QMessageBox>
*/

#include "org_sv_gui_qt_romsimulation_Export.h"

#include <QObject>
#include <QThread>

#include <map>
#include <string>

//------------------------
// sv4guiConvertWorkerROM
//------------------------
// The sv4guiConvertWorkerROM class is used to convert ROM simulation results in a QThread.
//
class SV_QT_ROMSIMULATION sv4guiConvertWorkerROM : public QObject 
{
  Q_OBJECT

  public:
    sv4guiConvertWorkerROM();
    ~sv4guiConvertWorkerROM();
    void SetModuleName(const std::string& moduleName) { m_ModuleName = moduleName; }
    void SetParameterValues(const std::map<std::string,std::string>& parameterValues) { m_ParameterValues = parameterValues; }
    void SetOutputDirectory(const std::string& outputDirectory) { m_OutputDirectory = outputDirectory; }
    void SetThread(QThread* thread) { m_Thread = thread; }

  public slots:
    void convertResults();

  signals:
    void finished();
    void error(const QString msg);
    void showMessage(const bool errorMsg, const QString& msg);

  private:
    std::string m_OutputDirectory;
    std::map<std::string,std::string> m_ParameterValues;
    std::string m_ModuleName;
    QThread* m_Thread;
};

#endif 
