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

// The sv4guiMPIPreferencePage class is used to process information about the location 
// of the MPI mpiexec binary and its implementation (OpenMPI or MPICH). The mpiexec binary 
// is used to execute a simulation presented in the 'Preferences->SimVascular Simulation' 
// web page. 
//
// sv4guiMPIPreferencePage methods are used to 
//
//     1) Process GUI events 
//
//     2) Set MPI information in the MITK database 
//
// The MITK database provides persistence for MPI information between SimVascular sessions.
//
// MPI information is set from the MITK database if it exists. Otherwise they are set using 
// the values obtained using an sv4guiMPIPreferences() object.
//
// Pressing the 'MPI' panel 'OK' button calls the PerformOk() method which saves the MPI 
// information into the MITK database.
//  
#include "sv4gui_MPIPreferencePage.h"
#include "ui_sv4gui_MPIPreferencePage.h"
#include "sv4gui_MPIPreferences.h"

#include <berryIPreferencesService.h>
#include <berryPlatform.h>

#include <mitkExceptionMacro.h>

#include <QFileDialog>
#include <QMessageBox>
#include <QProcess>

//-------------------------
// sv4guiMPIPreferencePage
//-------------------------
// Constructor.
//
sv4guiMPIPreferencePage::sv4guiMPIPreferencePage() : m_Preferences(nullptr), 
    m_Ui(new Ui::sv4guiMPIPreferencePage) , m_Control(nullptr)
{
  // Set the default locations of the solver binaries.
  m_DefaultPrefs = sv4guiMPIPreferences();
}

sv4guiMPIPreferencePage::~sv4guiMPIPreferencePage()
{
}

//-----------------
// CreateQtControl
//-----------------
// Set the GUI widgets event/callbacks.
//
// The values for the GUI are tried to be set from the MITK 
// database by calling Update(). InitializeMPILocation() is then
// called to set values not in the database. 
//
// [Note:DaveP] The MPI diaglog page closes when pressing enter in the
// 'mpiexec' text box. 
//
void sv4guiMPIPreferencePage::CreateQtControl(QWidget* parent)
{
    m_Control = new QWidget(parent);

    m_Ui->setupUi(m_Control);

    berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    Q_ASSERT(prefService);

    // Get the preference values from the MITK database.
    m_Preferences = prefService->GetSystemPreferences()->Node("/org.sv.views.simulation");

    // Set widgets event/callbacks.
    connect(m_Ui->toolButtonMPIExec, SIGNAL(clicked()), this, SLOT(SelectMPIExecPath()) );
    connect(m_Ui->lineEditMPIExecPath, SIGNAL(returnPressed()), this, SLOT(SetMPIExecPath()) );
    m_Ui->MpiImplementationLineEdit->setReadOnly(true);

    // Update the GUI with values from the MITK database.
    this->Update();

    // Set the MPI information (e.g. mpiexec path) if they have not already been. 
    SetMpiInformation();
}

//--------
// Update
//--------
// Update the GUI with the MPI information from the MITK 
// database if it exists.
//
void sv4guiMPIPreferencePage::Update()
{
    auto useMpi = m_Preferences->GetBool(sv4guiMPIPreferenceDBKey::USE_MPI, true);
    m_Ui->checkBoxUseMPI->setChecked(useMpi);

    auto mpiexcPath = m_Preferences->Get(sv4guiMPIPreferenceDBKey::MPI_EXEC_PATH,"");
    m_Ui->lineEditMPIExecPath->setText(mpiexcPath);

    auto mpiImpl = m_Preferences->Get(sv4guiMPIPreferenceDBKey::MPI_IMPLEMENTATION, ""); 
    m_Ui->MpiImplementationLineEdit->setText(mpiImpl);
}

//-------------------
// SetMpiInformation 
//-------------------
// Set the MPI information.
//
void sv4guiMPIPreferencePage::SetMpiInformation()
{
  // Set the mpiexec binary.
  SetMpiExec(); 

  // Set the MPI implementation. 
  SetMpiImplementation();
}

//------------
// SetMpiExec
//------------
// Set the location of the MPI mpiexec binary.
//
// If the GUI value is not set then set it using the sv4guiMPIPreferences 
// object m_DefaultPrefs.
//
void sv4guiMPIPreferencePage::SetMpiExec()
{
  QString mpiExec = m_Ui->lineEditMPIExecPath->text().trimmed();

  if (!mpiExec.isEmpty() && (mpiExec != m_DefaultPrefs.UnknownBinary)) {
    return;
  }

  mpiExec = m_DefaultPrefs.GetMpiExec();
  m_Ui->lineEditMPIExecPath->setText(mpiExec);
}

//----------------------
// SetMpiImplementation 
//----------------------
// Set the installed MPI implementation.
//
void sv4guiMPIPreferencePage::SetMpiImplementation()
{
  QString mpiImpl = m_Ui->MpiImplementationLineEdit->text().trimmed();

  if (!mpiImpl.isEmpty() && (mpiImpl != m_DefaultPrefs.UnknownBinary)) {
    return;
  }

  mpiImpl = m_DefaultPrefs.GetMpiName();
  m_Ui->MpiImplementationLineEdit->setText(mpiImpl);
}

//-------------------
// SelectMPIExecPath
//-------------------
// Process the GUI event to select the location of mpiexec using a file browser.
//
void sv4guiMPIPreferencePage::SelectMPIExecPath()
{
    QString filePath = QFileDialog::getOpenFileName(m_Control, "Choose MPIExec");

    if (!filePath.isEmpty()) {
        m_Ui->lineEditMPIExecPath->setText(filePath);
        SetMPIExecPath();
    }
}

//----------------
// SetMPIExecPath
//----------------
// Process the GUI event to set the mpiexec path in a text box.
//
// Call m_DefaultPrefs.SetMpiImplementation() to determine the
// MPI implementation and returned using m_DefaultPrefs.GetMpiName().
//
void sv4guiMPIPreferencePage::SetMPIExecPath()
{
    auto filePath = m_Ui->lineEditMPIExecPath->text().trimmed();

    // Get the MPI implementation from the mpiexec path 
    // and update the GUI.
    //
    if (!filePath.isEmpty()) {
        m_DefaultPrefs.SetMpiImplementation(filePath);
        auto mpiImpl = m_DefaultPrefs.GetMpiName();
        m_Ui->MpiImplementationLineEdit->setText(mpiImpl);
        PerformOk();
    }
}

QWidget* sv4guiMPIPreferencePage::GetQtControl() const
{
    return m_Control;
}

void sv4guiMPIPreferencePage::Init(berry::IWorkbench::Pointer)
{
}

void sv4guiMPIPreferencePage::PerformCancel()
{
}

//-----------
// PerformOk
//-----------
// Process the 'OK' button GUI event.
//
// Stores values from the GUI into the MITK database.
//
bool sv4guiMPIPreferencePage::PerformOk()
{
    // Get the solver paths from the GUI
    bool useMPI = m_Ui->checkBoxUseMPI->isChecked();
    QString MPIExecPath = m_Ui->lineEditMPIExecPath->text().trimmed();

    // Set use MPI. 
    m_Preferences->PutBool(sv4guiMPIPreferenceDBKey::USE_MPI, useMPI);
    
    // Set MPI implementation.
    m_Preferences->Put(sv4guiMPIPreferenceDBKey::MPI_IMPLEMENTATION, m_DefaultPrefs.GetMpiName());

    // Set mpiexec path.
    if (useMPI) {
        m_Preferences->Put(sv4guiMPIPreferenceDBKey::MPI_EXEC_PATH, MPIExecPath);
    }

    return true;
}


