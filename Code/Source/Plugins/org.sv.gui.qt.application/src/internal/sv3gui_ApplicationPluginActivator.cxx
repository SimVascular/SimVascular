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

#include "sv3gui_ApplicationPluginActivator.h"

#include "sv3gui_DefaultPerspective.h"
#include "sv3gui_WorkbenchIntroPart.h"
#include "sv3gui_Application.h"

#include <mitkVersion.h>
#include <mitkLogMacros.h>

#include <service/cm/ctkConfigurationAdmin.h>
#include <service/cm/ctkConfiguration.h>

ctkPluginContext* svApplicationPluginActivator::_context = nullptr;

svApplicationPluginActivator* svApplicationPluginActivator::inst = nullptr;

svApplicationPluginActivator::svApplicationPluginActivator()
{
  inst = this;
}

svApplicationPluginActivator::~svApplicationPluginActivator()
{
}

svApplicationPluginActivator* svApplicationPluginActivator::GetDefault()
{
  return inst;
}

void svApplicationPluginActivator::start(ctkPluginContext* context)
{
    berry::AbstractUICTKPlugin::start(context);

    this->_context = context;

    BERRY_REGISTER_EXTENSION_CLASS(svWorkbenchIntroPart, context)
    BERRY_REGISTER_EXTENSION_CLASS(svDefaultPerspective, context)
    BERRY_REGISTER_EXTENSION_CLASS(svApplication, context)

    ctkServiceReference cmRef = context->getServiceReference<ctkConfigurationAdmin>();
    ctkConfigurationAdmin* configAdmin = nullptr;
    if (cmRef)
    {
      configAdmin = context->getService<ctkConfigurationAdmin>(cmRef);
    }

    // Use the CTK Configuration Admin service to configure the BlueBerry help system
    if (configAdmin)
    {
      ctkConfigurationPtr conf = configAdmin->getConfiguration("org.blueberry.services.help", QString());
      ctkDictionary helpProps;
      helpProps.insert("homePage", "qthelp://org.mitk.gui.qt.extapplication/bundle/index.html");
      conf->update(helpProps);
      context->ungetService(cmRef);
    }
    else
    {
      MITK_WARN << "Configuration Admin service unavailable, cannot set home page url.";
    }
}

void svApplicationPluginActivator::stop(ctkPluginContext* context)
{
    Q_UNUSED(context)

    this->_context = nullptr;
}

ctkPluginContext* svApplicationPluginActivator::getContext()
{
    return _context;
}

