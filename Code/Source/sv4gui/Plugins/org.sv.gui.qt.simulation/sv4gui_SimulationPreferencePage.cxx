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

// The sv4guiSimulationPreferencePage class is used to process information 
// about the location of the solver binary svmultiphysics and the mpiexec 
// binary used to execute a simulation presented in the 
// 'Preferences->SimVascular Simulation' panel. 
//
// sv4guiSimulationPreferencePage methods are used to 
//
//     1) Process GUI events 
//
//     2) Set the values of the solver binaries in the MITK database 
//
// The MITK database provides persistence for solver binary values between 
// SimVascular sessions.
//
// The values of the solver binaries are set from the MITK database if it 
// exists. Otherwise they are set using their default values obtained using
// an sv4guiSimulationPreferences() object.
//
// Pressing the SimVascular Simulation' panel 'OK' button calls the PerformOk() 
// method which saves the solver binary values into the MITK database.
//  

#include "sv4gui_SimulationPreferencePage.h"
#include "ui_sv4gui_SimulationPreferencePage.h"

#include <mitkIPreferencesService.h>
#include <berryPlatform.h>

#include <mitkExceptionMacro.h>

#include <QFileDialog>
#include <QMessageBox>
#include <QProcess>

using namespace sv4guiSimulationPreferenceDBKey;

//--------------------------------
// sv4guiSimulationPreferencePage
//--------------------------------
// Constructor.
//
sv4guiSimulationPreferencePage::sv4guiSimulationPreferencePage() : m_Preferences(nullptr), 
    m_Ui(new Ui::sv4guiSimulationPreferencePage) , m_Control(nullptr)
{
  // Set the default locations of the solver binaries.
  m_DefaultPrefs = sv4guiSimulationPreferences();
}

sv4guiSimulationPreferencePage::~sv4guiSimulationPreferencePage()
{
}

//---------------------------
// InitializeSolverLocations
//---------------------------
// Find the location of solver binaries and mpiexec.
//
// The the full binary path is displayed in the SimVascular 
// 'Preferences->SimVascular Simulations' page and used to 
// execute a simulation.
//
// If the values for the binaries and mpiexec are not already
// set in the SimVascular MITK database then they set to their
// default values set in the sv4guiSimulationPreferences method.
//
void sv4guiSimulationPreferencePage::InitializeSolverLocations()
{
  SetSolver();
}

//-----------
// SetSolver 
//-----------
// Set the svmultiphysics binary, with or without mpi.
//
void sv4guiSimulationPreferencePage::SetSolver()
{
  QString solver = m_Ui->lineEditSolverPath->text().trimmed();

  if (!solver.isEmpty() && (solver != m_DefaultPrefs.UnknownBinary)) {
    return;
  }

  solver = m_DefaultPrefs.GetSolver();
  m_Ui->lineEditSolverPath->setText(solver);
}

//-----------------
// CreateQtControl
//-----------------
//
void sv4guiSimulationPreferencePage::CreateQtControl(QWidget* parent)
{
    m_Control = new QWidget(parent);

    m_Ui->setupUi(m_Control);

    mitk::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    Q_ASSERT(prefService);

    m_Preferences = prefService->GetSystemPreferences()->Node("/org.sv.views.simulation");

    connect( m_Ui->toolButtonSolver, SIGNAL(clicked()), this, SLOT(SetSolverPath()) );

    this->Update();

    // Set the locations of the solver binaries and mpiexec.
    InitializeSolverLocations();
}

//----------------
// SetSolverPath
//----------------
// Process the GUI event to set the svmultiphysics path.
//
void sv4guiSimulationPreferencePage::SetSolverPath()
{
  QString filePath = QFileDialog::getOpenFileName(m_Control, "Choose SimVascular svMultiPhysics solver");

  if (!filePath.isEmpty()) {
    m_Ui->lineEditSolverPath->setText(filePath);
  }
}

QWidget* sv4guiSimulationPreferencePage::GetQtControl() const
{
    return m_Control;
}

void sv4guiSimulationPreferencePage::Init(berry::IWorkbench::Pointer)
{
}

void sv4guiSimulationPreferencePage::PerformCancel()
{
}

//-----------
// PerformOk
//-----------
// Process the 'OK' button GUI event.
//
bool sv4guiSimulationPreferencePage::PerformOk()
{
  // Get the solver paths from the GUI.
  QString solverPath = m_Ui->lineEditSolverPath->text().trimmed();

  // Set the values of the solver paths in the MITK database.
  m_Preferences->Put(SOLVER_PATH, solverPath.toStdString());

  return true;
}

//--------
// Update
//--------
// Update the GUI with the solver path values from the
// MITK database.
//
void sv4guiSimulationPreferencePage::Update()
{
  m_Ui->lineEditSolverPath->setText(QString::fromStdString(m_Preferences->Get("solver path","")));
}

