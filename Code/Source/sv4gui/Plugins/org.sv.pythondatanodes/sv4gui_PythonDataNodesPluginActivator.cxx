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

// The sv4guiPythonDataNodesPluginActivator object provides access to
// a ctkPluginContext object, the execution context of a plugin within
// the framework used to access other methods that interact with the framework.
//
// This object is only created when running SV with the GUI, it is not
// created when running the SV Python interface from the command line.
//
#include "sv4gui_PythonDataNodesPluginActivator.h"
#include <service/cm/ctkConfigurationAdmin.h>
#include <service/cm/ctkConfiguration.h>

#include <iostream>

ctkPluginContext* sv4guiPythonDataNodesPluginActivator::m_Context = nullptr;
sv4guiPythonDataNodesPluginActivator* sv4guiPythonDataNodesPluginActivator::m_Inst = nullptr;

sv4guiPythonDataNodesPluginActivator::sv4guiPythonDataNodesPluginActivator()
{
  m_Inst = this;
}

sv4guiPythonDataNodesPluginActivator::~sv4guiPythonDataNodesPluginActivator()
{
}

sv4guiPythonDataNodesPluginActivator* sv4guiPythonDataNodesPluginActivator::GetDefault()
{
  return m_Inst;
}

//-------
// start
//-------
// Start the plugin.
//
void sv4guiPythonDataNodesPluginActivator::start(ctkPluginContext* context)
{
  berry::AbstractUICTKPlugin::start(context);
  this->m_Context = context;
}

//------
// stop
//------
// Stop the plugin.
//
void sv4guiPythonDataNodesPluginActivator::stop(ctkPluginContext* context)
{
  this->m_Context = nullptr;
}

ctkPluginContext* sv4guiPythonDataNodesPluginActivator::GetContext()
{
  return m_Context;
}

