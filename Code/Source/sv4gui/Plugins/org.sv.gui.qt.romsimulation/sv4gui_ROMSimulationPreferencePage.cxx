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

#include "sv4gui_ROMSimulationPreferencePage.h"
#include "ui_sv4gui_ROMSimulationPreferencePage.h"

#include <mitkIPreferencesService.h>
#include <berryPlatform.h>

#include <mitkExceptionMacro.h>

#include <QFileDialog>
#include <QMessageBox>

sv4guiROMSimulationPreferencePage::sv4guiROMSimulationPreferencePage()
    : m_Preferences(nullptr)
    , m_Ui(new Ui::sv4guiROMSimulationPreferencePage)
    , m_Control(nullptr)
{
}

sv4guiROMSimulationPreferencePage::~sv4guiROMSimulationPreferencePage()
{
}

//---------------------------
// InitializeSolverLocations
//---------------------------
// Find the location of solver binaries and mpiexec.
//
void sv4guiROMSimulationPreferencePage::InitializeSolverLocations()
{
    SetOneDSolver();
    SetZeroDSolver();
}

void sv4guiROMSimulationPreferencePage::CreateQtControl(QWidget* parent)
{
    m_Control = new QWidget(parent);

    m_Ui->setupUi(m_Control);

    mitk::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    Q_ASSERT(prefService);

    m_Preferences = prefService->GetSystemPreferences()->Node("/org.sv.views.romsimulation");

    connect( m_Ui->SolverExecutablePath_Button, SIGNAL(clicked()), this, SLOT(SetOneDSolverFile()) );
    connect( m_Ui->ZeroDSolverExecutablePath_Button, SIGNAL(clicked()), this, SLOT(SetZeroDSolverFile()) );

    this->Update();

    // Set the locations of the solver binary.
    InitializeSolverLocations();
}

//-------------------
// SetOneDSolverFile
//-------------------
// Set the location of the 1D solver executable using a file browser.
//
void sv4guiROMSimulationPreferencePage::SetOneDSolverFile()
{
    QString filePath = QFileDialog::getOpenFileName(m_Control, "Select the 1D solver executable");

    if (!filePath.isEmpty()) {
        m_Ui->SolverExecutablePath_LineEdit->setText(filePath);
    }
}

//---------------
// SetOneDSolver
//---------------
// Set the 1D solver executable from the GUI line edit widget
// or from the default value.
//
void sv4guiROMSimulationPreferencePage::SetOneDSolver()
{
  QString solver = m_Ui->SolverExecutablePath_LineEdit->text().trimmed();
  if (!solver.isEmpty() && (solver != m_DefaultPrefs.UnknownBinary)) {
    return;
  }

  solver = m_DefaultPrefs.GetOneDSolver();
  m_Ui->SolverExecutablePath_LineEdit->setText(solver);
}

//--------------------
// SetZeroDSolverFile 
//--------------------
// Set the location of the 9D solver executable using a file browser.
//
void sv4guiROMSimulationPreferencePage::SetZeroDSolverFile()
{
    QString filePath = QFileDialog::getOpenFileName(m_Control, "Select the 0D solver executable");

    if (!filePath.isEmpty()) {
        m_Ui->ZeroDSolverExecutablePath_LineEdit->setText(filePath);
    }
}

//---------------
// SetZeroDSolver
//---------------
// Set the 0D solver executable from the GUI line edit widget
// or from the default value.
//
void sv4guiROMSimulationPreferencePage::SetZeroDSolver()
{
  QString solver = m_Ui->ZeroDSolverExecutablePath_LineEdit->text().trimmed();
  if (!solver.isEmpty() && (solver != m_DefaultPrefs.UnknownBinary)) {
    return;
  }

  solver = m_DefaultPrefs.GetZeroDSolver();
  m_Ui->ZeroDSolverExecutablePath_LineEdit->setText(solver);
}


QWidget* sv4guiROMSimulationPreferencePage::GetQtControl() const
{
    return m_Control;
}

void sv4guiROMSimulationPreferencePage::Init(berry::IWorkbench::Pointer)
{
}

void sv4guiROMSimulationPreferencePage::PerformCancel()
{
}

bool sv4guiROMSimulationPreferencePage::PerformOk()
{
    using namespace sv4guiROMSimulationPreferenceDBKey;

    QString oneDSolverPath = m_Ui->SolverExecutablePath_LineEdit->text().trimmed();
    m_Preferences->Put(ONED_SOLVER_PATH, oneDSolverPath.toStdString());

    QString zeroDSolverPath = m_Ui->ZeroDSolverExecutablePath_LineEdit->text().trimmed();
    m_Preferences->Put(ZEROD_SOLVER_PATH, zeroDSolverPath.toStdString());

    return true;
}

void sv4guiROMSimulationPreferencePage::Update()
{
    m_Ui->SolverExecutablePath_LineEdit->setText(QString::fromStdString(m_Preferences->Get("1d solver executable path","")));
    m_Ui->ZeroDSolverExecutablePath_LineEdit->setText(QString::fromStdString(m_Preferences->Get("0d solver executable path","")));
}

