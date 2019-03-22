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
  m_MpiEnumToString = { 
    {MpiImplementation::MPICH, "MPICH"},
    {MpiImplementation::OpenMPI, "OpenMPI"},
    {MpiImplementation::Unknown, "Unknown"}
  };

  m_MpiStringToEnum = {
    {"MPICH", MpiImplementation::MPICH}, 
    {"OpenMPI", MpiImplementation::OpenMPI},
    {"Unknown", MpiImplementation::Unknown}
  };

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

  // Set the solver binaries.
  SetPreSolver(solverInstallPath, applicationPath); 
  SetSolver(solverInstallPath, applicationPath); 
  SetPostSolver(solverInstallPath, applicationPath); 

  // Set the mpiexec binary.
  SetMpiExec(solverInstallPath, applicationPath); 

  // Set the MPI implementation. 
  SetMpiImplementation();
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

    svPost = GetRegistryValue("SimVascular\\svSolver","SVPOST_EXE");

#endif

  m_svPostBinary = svPost;
}

QString sv4guiSimulationPreferences::GetPostSolver()
{
  return m_svPostBinary;
}

//------------
// SetMpiExec
//------------
// Set the location of the MPI mpiexec binary.
//
// Don't display a warning until the solver is actually used. 
//
void sv4guiSimulationPreferences::SetMpiExec(const QString& solverPath, const QString& applicationPath)
{
  QString mpiExec = UnknownBinary;
  QString mpiExecPath;
  QString filePath = "";
  QString mpiExecName = "mpiexec";

#if defined(Q_OS_LINUX)

  mpiExecPath = "/usr/bin/";

  if (QFile(filePath = mpiExecPath + mpiExecName).exists()) {
    mpiExec = filePath;
  }

#elif defined(Q_OS_MAC)

  mpiExecPath = "/usr/local/bin/";

  if (QFile(filePath = mpiExecPath + mpiExecName).exists()) {
    mpiExec = filePath;
  }

  /* [DaveP] Do we need this?
  if(QFile(filePath = solverPath + mpiexecName).exists()) {
    mpiExec = filePath;

  } else if(QFile(filePath = applicationPath + "/.." + mpiexecName).exists()) {
    mpiExec = filePath;

  } else if(QFile(filePath = applicationPath + mpiexecName).exists()) {
    mpiExec = filePath;
  }
  */

#elif defined(Q_OS_WIN)

    QString msmpiDir = GetRegistryValue("Microsoft\\MPI","InstallRoot");

    if (msmpiDir != "") {
      if (msmpiDir.endsWith("\\")) {
        mpiExec = msmpiDir+"Bin\\mpiexec";
      } else {
        mpiExec = msmpiDir+"\\Bin\\mpiexec";
      }
    }

#endif

  m_mpiExec = mpiExec;
}

QString sv4guiSimulationPreferences::GetMpiExec() 
{ 
  return m_mpiExec; 
}

//----------------------
// SetMpiImplementation 
//----------------------
// Set the installed MPI implementation.
//
// This is needed to unsure that MPI is installed and that the
// correct implementation is installed for a given OS.
//
// Check the implementation using 'mpicc -show' and show
// it in the Preferences panel.
//
void sv4guiSimulationPreferences::SetMpiImplementation()
{
  MpiImplementation implementation = MpiImplementation::Unknown;

  QFileInfo fileInfo(m_mpiExec);
  QString mpiExecPath = fileInfo.path();

#if defined(Q_OS_LINUX) || defined(Q_OS_MAC)
  QProcess *checkMpi = new QProcess();
  QString program(mpiExecPath + "/mpicc");

  if (QFile(program).exists()) {
    QStringList arguments;
    arguments << "-show";
    checkMpi->setProgram(program);
    checkMpi->setArguments(arguments);
    checkMpi->start(program, arguments);
    checkMpi->waitForFinished(); 
    QString output(checkMpi->readAllStandardOutput());
    if (output.contains("mpich")) {
      implementation = MpiImplementation::MPICH;
    } else if (output.contains("openmpi")) {
      implementation = MpiImplementation::OpenMPI;
    } else {
      implementation = MpiImplementation::Unknown;
    }
  }

#endif

  m_MpiImplementation = implementation;
}

//------------
// GetMpiName
//------------
// Get the name of the MPI implementation.
//
const QString sv4guiSimulationPreferences::GetMpiName()
{
  return m_MpiEnumToString[m_MpiImplementation];
}

sv4guiSimulationPreferences::MpiImplementation
sv4guiSimulationPreferences::GetMpiImplementation()
{
  return m_MpiImplementation;
}

sv4guiSimulationPreferences::MpiImplementation 
sv4guiSimulationPreferences::GetMpiImplementation(const QString& name)
{
  if (m_MpiStringToEnum.find(name) == m_MpiStringToEnum.end()) {
      return sv4guiSimulationPreferences::MpiImplementation::Unknown;
  }
  return m_MpiStringToEnum[name];
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
  svPresolver = GetRegistryValue("SimVascular\\svSolver","SVPRE_EXE");

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
  bool useMPI = true;
  QString svSolver = UnknownBinary;

#if defined(Q_OS_LINUX) || defined(Q_OS_MAC)
  QString filePath = "";
  QString svSolverName;

  if (useMPI) {
    svSolverName="/svsolver";
  } else {
    svSolverName="/svsolver-nompi";
  }

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

  if (noMPI) {
    svSolver = GetRegistryValue("SimVascular\\svSolver","SVSOLVER_NOMPI_EXE");
  } else {
    svSolver = GetRegistryValue("SimVascular\\svSolver","SVSOLVER_MSMPI_EXE");
  }

#endif

  m_svSolver = svSolver;
}

QString sv4guiSimulationPreferences::GetSolver()
{
  return m_svSolver;
}


