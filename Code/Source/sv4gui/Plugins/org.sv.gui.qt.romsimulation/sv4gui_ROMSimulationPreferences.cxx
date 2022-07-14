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

#include "SimVascular.h"
#include "sv4gui_ROMSimulationPreferences.h"

#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QProcess>

#include <iostream>

// Set the string value representing an unresolved binary.
const QString sv4guiROMSimulationPreferences::UnknownBinary("not found");

//-----------------------------
// sv4guiROMSimulationPreferences
//-----------------------------
//
sv4guiROMSimulationPreferences::sv4guiROMSimulationPreferences() 
{
  InitializeSolverLocations();
}

sv4guiROMSimulationPreferences::~sv4guiROMSimulationPreferences()
{
}

//---------------------------
// InitializeSolverLocations
//---------------------------
// Find the location of solver binaries and mpiexec.
//
// The the full binary path is displayed in the SimVascular 
// 'Preferences->SimVascular Simulations' page and used to 
//execute a simulation.
//
void sv4guiROMSimulationPreferences::InitializeSolverLocations()
{
  // Set the default install location of the solver.
  QString solverInstallPath = "/usr/local/sv/oneDSolver";
  QStringList dirList = QDir(solverInstallPath).entryList(QDir::Dirs|QDir::NoDotAndDotDot|QDir::NoSymLinks,QDir::Name);
  if (dirList.size() != 0) {
    solverInstallPath += "/" + dirList.back();
  }

  // Set the path to the SimVascular-build/bin directory.
  QString applicationPath = "";
  //QString applicationPath = QCoreApplication::applicationDirPath();

  //sv4gui_parse_registry_for_svsolver();
  
  // Set the solver binaries.
  SetOneDSolver(solverInstallPath, applicationPath);
}

//---------------
// SetOneDSolver 
//---------------
// Set the OneDSolver binary. 
//
void sv4guiROMSimulationPreferences::SetOneDSolver(const QString& solverInstallPath, const QString& applicationPath)
{
  QString svOneDSolver = UnknownBinary;

#if defined(Q_OS_LINUX) || defined(Q_OS_MAC)
  QString filePath = "";
  QString svOneDSolverName = "/OneDSolver";

  // For the flow solver with mpi, prefer to use the script one which sets 
  // paths to libs needed in Ubuntu 16. 
  //
  if(QFile(filePath=solverInstallPath+svOneDSolverName).exists()) {
    svOneDSolver = filePath;

  } else if(QFile(filePath=solverInstallPath+"/bin/"+svOneDSolverName).exists()) {
    svOneDSolver = filePath;

  } else if(QFile(filePath=applicationPath+svOneDSolverName).exists()) {
    svOneDSolver = filePath;

  } else if(QFile(filePath=applicationPath+"/bin/"+svOneDSolverName).exists()) {
    svOneDSolver = filePath;
  }

#elif defined(Q_OS_WIN)

  char result[1024];
  result[0] = '\0';

  if (sv4gui_parse_registry_for_svonedsolver("SVONEDSOLVER_EXE",result) == SV_OK) {
     svOneDSolver = result;
  }

#endif

  m_svOneDSolver = svOneDSolver;
}

QString sv4guiROMSimulationPreferences::GetOneDSolver()
{
  return m_svOneDSolver;
}




