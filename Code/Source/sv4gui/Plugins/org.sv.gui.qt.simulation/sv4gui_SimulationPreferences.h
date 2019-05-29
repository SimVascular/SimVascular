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

// The sv4guiSimulationPreferences class is used to determine which default binaries,
// (svsolver, svpre and svpost) are used by the SV Simulation plugin. The class also 
// determines which mpiexec binary is used to execute solver jobs using MPI and what
// its implementation is: MPICH or OpenMPI. 
//
// An sv4guiSimulationPreferences object is used by the sv4guiSimulationPreferencePage objectc
// to display the full path to these binaries in the Preferences -> SimVascular Simulation panel.
//
// The sv4guiSimulationView object, used to lauch simulation jobs, only reads values from 
// the sv4guiSimulationPreferencePage object when a value is changed. Because of this the 
// sv4guiSimulationView object must also use a sv4guiSimulationPreferences object to set
// the default solver binaries.

#ifndef SV4GUI_SIMULATIONPREFERENCES_H
#define SV4GUI_SIMULATIONPREFERENCES_H

#include <iostream>
#include <map>
#include <QString>

#ifdef WIN32
  #include "sv4gui_win32_use_registry.h"
#endif

//-----------------------------
// sv4guiSimulationPreferences 
//-----------------------------
class sv4guiSimulationPreferences 
{

public:
  sv4guiSimulationPreferences();
  ~sv4guiSimulationPreferences();

  void InitializeSolverLocations();
  QString GetPostSolver();
  QString GetPreSolver();
  QString GetSolver();
  QString GetSolverNOMPI();
  static const QString UnknownBinary;

private:
  QString m_svPostBinary;
  QString m_svPresolver;
  QString m_svSolver;
  QString m_svSolverNOMPI;

  void SetPostSolver(const QString& solverInstallPath, const QString& applicationPath);
  void SetPreSolver(const QString& solverInstallPath, const QString& applicationPath);
  void SetSolver(const QString& solverInstallPath, const QString& applicationPath);
  void SetSolverNOMPI(const QString& solverInstallPath, const QString& applicationPath);

};

#endif // SV4GUI_SIMULATIONPREFERENCES_H
