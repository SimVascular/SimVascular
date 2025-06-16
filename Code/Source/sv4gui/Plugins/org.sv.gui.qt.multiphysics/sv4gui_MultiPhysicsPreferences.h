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

// The sv4guiMultiPhysicsPreferences class is used to determine which svmultiphysics binary
// are used by the SV MultiPhysics plugin. The class also determines which mpiexec binary 
// is used to execute solver jobs using MPI and what its implementation is: MPICH or OpenMPI. 
//
// An sv4guiMultiPhysicsPreferences object is used by the sv4guiMultiPhysicsPreferencePage objectc
// to display the full path to these binaries in the Preferences -> SimVascular MultiPhysics panel.
//
// The sv4guiMultiPhysicsView object, used to lauch simulation jobs, only reads values from 
// the sv4guiMultiPhysicsPreferencePage object when a value is changed. Because of this the 
// sv4guiMultiPhysicsView object must also use a sv4guiMultiPhysicsPreferences object to set
// the default solver binaries.

#ifndef SV4GUI_MULTIPHYSICS_PREFERENCES_H
#define SV4GUI_MULTIPHYSICS_PREFERENCES_H

#include <iostream>
#include <map>
#include <QString>

#ifdef WIN32
  #include "sv4gui_win32_use_registry.h"
#endif

//-------------------------------
// sv4guiMultiPhysicsPreferences 
//-------------------------------
//
class sv4guiMultiPhysicsPreferences 
{

public:
  sv4guiMultiPhysicsPreferences();
  ~sv4guiMultiPhysicsPreferences();

  void InitializeSolverLocations();
  QString GetSolver();
  static const QString UnknownBinary;

private:
  QString m_Solver;
  void SetSolver(const QString& solverInstallPath, const QString& applicationPath);
};

#endif 
