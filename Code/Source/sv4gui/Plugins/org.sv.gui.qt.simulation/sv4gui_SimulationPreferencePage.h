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
// presented in the 'Preferences->SimVascular Simulation' panel. 
//
// The 'Preferences->SimVascular Simulation' panel is used to show and set
// the location of the solver binaries (svpre, svsolver and svpost) and
// mpiexec binary used to execute a simulation.

#ifndef SV4GUI_SIMULATIONPREFERENCEPAGE_H
#define SV4GUI_SIMULATIONPREFERENCEPAGE_H

#include <sv4gui_SimulationPreferences.h>

#include <berryIPreferences.h>
#include <berryIQtPreferencePage.h>

namespace Ui {
class sv4guiSimulationPreferencePage;
}

// Define MITK Database keys.
//
// The keys are used to store property values in a MITK database.
//
namespace sv4guiSimulationPreferenceDBKey {
    const QString FLOW_SOLVER_NO_MPI_PATH = "flowsolver nompi path";
    const QString FLOW_SOLVER_PATH = "flowsolver path";
    const QString POST_SOLVER_PATH = "postsolver path";
    const QString PRE_SOLVER_PATH = "presolver path";
    const QString SOLVER_TEMPLATE_PATH = "solver template path";
    const QString USE_CUSTOM = "use custom";
};

class sv4guiSimulationPreferencePage : public QObject, public berry::IQtPreferencePage
{
    Q_OBJECT
    Q_INTERFACES(berry::IPreferencePage)

public:
    sv4guiSimulationPreferencePage();
    ~sv4guiSimulationPreferencePage();

    void CreateQtControl(QWidget* parent) override;
    QWidget* GetQtControl() const override;
    void Init(berry::IWorkbench::Pointer) override;
    void PerformCancel() override;
    bool PerformOk() override;
    void Update() override;
    void InitializeSolverLocations();

private slots:
  void SetPresolverPath();
  void SetFlowsolverPath();
  void SetFlowsolverNOMPIPath();
  void SetCustomTemplatePath();
  void SetPostsolverPath();

private:
  void SetPreSolver();
  void SetSolver();
  void SetSolverNOMPI();
  void SetPostSolver();

  berry::IPreferences::Pointer m_Preferences;
  QScopedPointer<Ui::sv4guiSimulationPreferencePage> m_Ui;
  QWidget* m_Control;
  sv4guiSimulationPreferences m_DefaultPrefs;

};

#endif // SV4GUI_SIMULATIONPREFERENCEPAGE_H
