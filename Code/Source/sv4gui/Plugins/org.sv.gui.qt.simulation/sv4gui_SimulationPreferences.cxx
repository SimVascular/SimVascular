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
#include "sv4gui_SimulationPreferences.h"

#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QProcess>

#include <iostream>

// Set the string value representing an unresolved binary.
const QString sv4guiSimulationPreferences::UnknownBinary("not found");

//-----------------------------
// sv4guiSimulationPreferences
//-----------------------------
//
sv4guiSimulationPreferences::sv4guiSimulationPreferences() 
{
  InitializeSolverLocations();
}

sv4guiSimulationPreferences::~sv4guiSimulationPreferences()
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
void sv4guiSimulationPreferences::InitializeSolverLocations()
{
  // Set the default install location of the solver.
  QString solverInstallPath = "/usr/local/sv/svsolver";
  QStringList dirList = QDir(solverInstallPath).entryList(QDir::Dirs|QDir::NoDotAndDotDot|QDir::NoSymLinks,QDir::Name);
  if (dirList.size() != 0) {
    solverInstallPath += "/" + dirList.back();
  }

  // Set the path to the SimVascular-build/bin directory.
  QString applicationPath = "";
  //QString applicationPath = QCoreApplication::applicationDirPath();

  //sv4gui_parse_registry_for_svsolver();
  
  // Set the solver binaries.
  SetPreSolver(solverInstallPath, applicationPath); 
  SetSolver(solverInstallPath, applicationPath);
  SetSolverNOMPI(solverInstallPath, applicationPath);
  SetPostSolver(solverInstallPath, applicationPath); 

}

//---------------
// SetPostSolver
//---------------
// Set the post processing binary svpost.
//
// There are two locations to check:
//
//     1) /usr/local/svsolver/      - contains the script 'svpost'
//
//     2) /usr/local/svsolver/bin   - contains the binary
//
// Use the script version if it exists.
//
void sv4guiSimulationPreferences::SetPostSolver(const QString& solverPath, const QString& applicationPath)
{
  QString svPost = UnknownBinary;

#if defined(Q_OS_LINUX) || defined(Q_OS_MAC)

  QString svPostName = "/svpost";
  QString filePath = "";

  if(QFile(filePath=solverPath+svPostName).exists()) {
    svPost = filePath;
  } else if(QFile(filePath=solverPath+"/bin/"+svPostName).exists()) {
    svPost = filePath;
  } else if(QFile(filePath=applicationPath+svPostName).exists()) {
    svPost = filePath;
  } else if(QFile(filePath=applicationPath+"/bin/"+svPostName).exists()) {
    svPost = filePath;
  }

#elif defined(Q_OS_WIN)

  char result[1024];
  result[0]='\0'; 
  if (sv4gui_parse_registry_for_svsolver("SVPOST_EXE",result) == SV_OK) {
    svPost = result;									 
  }

#endif

  m_svPostBinary = svPost;
}

QString sv4guiSimulationPreferences::GetPostSolver()
{
  return m_svPostBinary;
}


//------------------
// SetPresolverPath
//------------------
// Set the location of the svpre binary.
//
// There are two locations to check:
//
//     1) /usr/local/svsolver/      - contains the script 'svpost'
//
//     2) /usr/local/svsolver/bin   - contains the binary
//
// Use the script version if it exists.
//
void sv4guiSimulationPreferences::SetPreSolver(const QString& solverPath, const QString& applicationPath)
{
  QString filePath = "";
  QString svPreName = "/svpre";
  QString svPresolver = UnknownBinary;

#if defined(Q_OS_LINUX)
  if(QFile(filePath=solverPath+svPreName).exists()) {
    svPresolver = filePath;
  } else if(QFile(filePath=solverPath+"/bin/"+svPreName).exists()) {
    svPresolver = filePath;
  } else if(QFile(filePath=applicationPath+svPreName).exists()) {
    svPresolver = filePath;
  } else if(QFile(filePath=applicationPath+"/bin/"+svPreName).exists()) {
    svPresolver = filePath;
  }

#elif defined(Q_OS_MAC)
  if(QFile(filePath=solverPath+svPreName).exists()) {
    svPresolver = filePath;
  } else if(QFile(filePath=solverPath+"/bin/"+svPreName).exists()) {
    svPresolver = filePath;
  } else if(QFile(filePath=applicationPath+"/bin/"+svPreName).exists()) {
    svPresolver = filePath;
  } else if(QFile(filePath=applicationPath+svPreName).exists()) {
    svPresolver = filePath;
  }

#elif defined(Q_OS_WIN)

  char result[1024];
  result[0]='\0';
  if(sv4gui_parse_registry_for_svsolver("SVPRE_EXE",result) == SV_OK) {
    svPresolver = result;
  }
#endif

  m_svPresolver = svPresolver;
}

QString sv4guiSimulationPreferences::GetPreSolver()
{
  return m_svPresolver;
}

//-----------
// SetSolver 
//-----------
// Set the svsolver binary, with or without mpi.
//
// There are two locations to check:
//
//     1) /usr/local/svsolver/      - contains the script 'svpost'
//
//     2) /usr/local/svsolver/bin   - contains the binary
//
// Use the script version if it exists.
//
void sv4guiSimulationPreferences::SetSolver(const QString& solverInstallPath, const QString& applicationPath)
{
  QString svSolver = UnknownBinary;

#if defined(Q_OS_LINUX) || defined(Q_OS_MAC)
  QString filePath = "";
  QString svSolverName;

  svSolverName="/svsolver";

  // For the flow solver with mpi, prefer to use the script one which sets 
  // paths to libs needed in Ubuntu 16. 
  //
  if(QFile(filePath=solverInstallPath+svSolverName).exists()) {
    svSolver = filePath;
  } else if(QFile(filePath=solverInstallPath+"/bin/"+svSolverName).exists()) {
    svSolver = filePath;
  } else if(QFile(filePath=applicationPath+svSolverName).exists()) {
    svSolver = filePath;
  } else if(QFile(filePath=applicationPath+"/bin/"+svSolverName).exists()) {
    svSolver = filePath;
  }

#elif defined(Q_OS_WIN)

  char result[1024];
  result[0]='\0';
  if(sv4gui_parse_registry_for_svsolver("SVSOLVER_MSMPI_EXE",result) == SV_OK) {
     svSolver = result;
  }

#endif

  m_svSolver = svSolver;

}


//---------------
// SetSolverNOMPI 
//---------------
// Set the svsolver binary, with or without mpi.
//
// There are two locations to check:
//
//     1) /usr/local/svsolver/      - contains the script 'svpost'
//
//     2) /usr/local/svsolver/bin   - contains the binary
//
// Use the script version if it exists.
//
void sv4guiSimulationPreferences::SetSolverNOMPI(const QString& solverInstallPath, const QString& applicationPath)
{
  QString svSolverNOMPI = UnknownBinary;

#if defined(Q_OS_LINUX) || defined(Q_OS_MAC)
  QString filePath = "";
  QString svSolverNOMPIName;
  
  svSolverNOMPIName="/svsolver-nompi";

  // For the flow solver with mpi, prefer to use the script one which sets 
  // paths to libs needed in Ubuntu 16. 
  //
  if(QFile(filePath=solverInstallPath+svSolverNOMPIName).exists()) {
    svSolverNOMPI = filePath;
  } else if(QFile(filePath=solverInstallPath+"/bin/"+svSolverNOMPIName).exists()) {
    svSolverNOMPI = filePath;
  } else if(QFile(filePath=applicationPath+svSolverNOMPIName).exists()) {
    svSolverNOMPI = filePath;
  } else if(QFile(filePath=applicationPath+"/bin/"+svSolverNOMPIName).exists()) {
    svSolverNOMPI = filePath;
  }

#elif defined(Q_OS_WIN)

  char result[1024];
  result[0]='\0';
  if(sv4gui_parse_registry_for_svsolver("SVSOLVER_NOMPI_EXE",result) == SV_OK) {
     svSolverNOMPI = result;
  }
#endif

  m_svSolverNOMPI = svSolverNOMPI;
}

QString sv4guiSimulationPreferences::GetSolver()
{
  return m_svSolver;
}

QString sv4guiSimulationPreferences::GetSolverNOMPI()
{
  return m_svSolverNOMPI;
}


