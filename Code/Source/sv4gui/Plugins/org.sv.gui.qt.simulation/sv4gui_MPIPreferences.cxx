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

#include "sv4gui_MPIPreferences.h"

#include <QDir>
#include <QFile>
#include <QFileInfo>
#include <QProcess>
#include <QSettings>

#include <iostream>

// Set the string value representing an unresolved binary.
const QString sv4guiMPIPreferences::UnknownBinary("not found");

//-----------------------------
// sv4guiMPIPreferences
//-----------------------------
//
sv4guiMPIPreferences::sv4guiMPIPreferences() 
{
  m_MpiEnumToString = { 
    {MpiImplementation::MPICH,   sv4guiMPIPreferencesImplementationNames::MPICH},
    {MpiImplementation::OpenMPI, sv4guiMPIPreferencesImplementationNames::OpenMPI},
    {MpiImplementation::MSMPI,   sv4guiMPIPreferencesImplementationNames::MSMPI},
    {MpiImplementation::Unknown, sv4guiMPIPreferencesImplementationNames::Unknown}
  };

  m_MpiStringToEnum = {
    {sv4guiMPIPreferencesImplementationNames::MPICH,   MpiImplementation::MPICH}, 
    {sv4guiMPIPreferencesImplementationNames::OpenMPI, MpiImplementation::OpenMPI},
    {sv4guiMPIPreferencesImplementationNames::MSMPI,   MpiImplementation::MSMPI},
    {sv4guiMPIPreferencesImplementationNames::Unknown, MpiImplementation::Unknown}
  };

  InitializeMPILocation();
}

sv4guiMPIPreferences::~sv4guiMPIPreferences()
{
}

//-----------------------
// InitializeMPILocation
//-----------------------
// Find the location of solver binaries and mpiexec.
//
// The the full binary path is displayed in the SimVascular 
// 'Preferences->SimVascular Simulations' page and used to 
//execute a simulation.
//
void sv4guiMPIPreferences::InitializeMPILocation()
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

  // Set the mpiexec binary.
  SetMpiExec(solverInstallPath, applicationPath); 

  // Set the MPI implementation. 
  SetMpiImplementation();
}


#ifdef WIN32

//---------------
// FindLatestKey
//---------------

QString sv4guiMPIPreferences::FindLatestKey(QString key, QStringList keys)
{
    keys.sort();

    QString latestKey="";
    for(int i=keys.size()-1;i>-1;i--)
    {
        if(keys[i].endsWith("/"+key))
        {
            latestKey=keys[i];
            break;
        }
    }

    return latestKey;
}

//------------------
// GetRegistryValue
//------------------

QString sv4guiMPIPreferences::GetRegistryValue(QString category, QString key)
{
    QString value="";
    QStringList keys;
    QString latestKey="";
    
    QSettings settings2(category, QSettings::NativeFormat);
    value=settings2.value(key).toString().trimmed();
    if(value!="")
        return value;

    keys=settings2.allKeys();
    latestKey=FindLatestKey(key,keys);
    if(latestKey!="")
    {
        value=settings2.value(latestKey).toString().trimmed();
        if(value!="")
            return value;
    }

    return "";
}

#endif

//------------
// SetMpiExec
//------------
// Set mpiexec to its platform-dependent default locations 
//
void sv4guiMPIPreferences::SetMpiExec(const QString& solverPath, const QString& applicationPath)
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

#elif defined(Q_OS_WIN)
  
  QString msmpiDir = GetRegistryValue("HKEY_LOCAL_MACHINE\\SOFTWARE\\WOW6432Node\\Microsoft\\MPI","InstallRoot");

    if (msmpiDir != "") {
      if (msmpiDir.endsWith("\\")) {
        mpiExec = msmpiDir+"Bin\\mpiexec.exe";
      } else {
        mpiExec = msmpiDir+"\\Bin\\mpiexec.exe";
      }
    }

#endif

  m_mpiExec = mpiExec;
}

//------------
// SetMpiExec
//------------
// Set the location of the MPI mpiexec binary directly.
//
void sv4guiMPIPreferences::SetMpiExec(const QString& filePath)
{
  m_mpiExec = filePath;
  SetMpiImplementation();
}

//------------
// GetMpiExec
//------------
// Get the location of the MPI mpiexec binary.
//
QString sv4guiMPIPreferences::GetMpiExec() 
{ 
  return m_mpiExec; 
}

//----------------------
// SetMpiImplementation 
//----------------------
// Set the installed MPI implementation.
//
void sv4guiMPIPreferences::SetMpiImplementation()
{
  if (m_mpiExec == UnknownBinary) {
    return;
  }

  m_MpiImplementation = DetermineMpiImplementation(m_mpiExec);
}

//----------------------------
// DetermineMpiImplementation
//----------------------------
// Determine the MPI implementation from the mpiexec program.
//
// This is needed to ensure that MPI is installed and that the
// correct implementation is installed for a given OS.
//
// Check the implementation using 'mpiexec -version' and show
// it in the Preferences panel.
//
// Another way to check the MPI implementation is to use 'mpicc -show'
// but it is probably better to check the actual mpiexec that will be 
// used to run jobs.
//
sv4guiMPIPreferences::MpiImplementation sv4guiMPIPreferences::DetermineMpiImplementation(const QString& mpiExecName)
{
  MpiImplementation implementation = MpiImplementation::Unknown;

  QFileInfo fileInfo(mpiExecName);
  QString mpiExecPath = fileInfo.path();

#if defined(Q_OS_LINUX) || defined(Q_OS_MAC)
  QProcess *checkMpi = new QProcess();
  QString program(mpiExecName);

  if (QFile(program).exists()) {
    QStringList arguments;
    arguments << "-version";
    checkMpi->setProgram(program);
    checkMpi->setArguments(arguments);
    checkMpi->start(program, arguments);
    checkMpi->waitForFinished();
    QString output(checkMpi->readAllStandardOutput());
    if (output.contains("mpich")) {
      implementation = MpiImplementation::MPICH;
    } else if (output.contains("OpenRTE")) {
      implementation = MpiImplementation::OpenMPI;
    } else {
      implementation = MpiImplementation::Unknown;
    }
  }

#elif defined(Q_OS_WIN)
  implementation = MpiImplementation::MSMPI;
#endif

  return implementation;
}

//--------------------------
// GetMpiImplementationName
//--------------------------
// Get the name of the MPI implementation.
//
const QString sv4guiMPIPreferences::GetMpiImplementationName()
{
  return m_MpiEnumToString[m_MpiImplementation];
}

const QString sv4guiMPIPreferences::GetMpiImplementationName(const sv4guiMPIPreferences::MpiImplementation implementation)
{
  return m_MpiEnumToString[implementation];
}

sv4guiMPIPreferences::MpiImplementation
sv4guiMPIPreferences::GetMpiImplementation()
{
  return m_MpiImplementation;
}

sv4guiMPIPreferences::MpiImplementation 
sv4guiMPIPreferences::GetMpiImplementation(const QString& name)
{
  if (m_MpiStringToEnum.find(name) == m_MpiStringToEnum.end()) {
      return sv4guiMPIPreferences::MpiImplementation::Unknown;
  }
  return m_MpiStringToEnum[name];
}
