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

#include "sv4gui_AppWorkbenchAdvisor.h"
#include "sv4gui_ApplicationPluginActivator.h"
#include "sv4gui_WorkbenchWindowAdvisor.h"

#include "berryIQtStyleManager.h"

const QString sv4guiAppWorkbenchAdvisor::DEFAULT_PERSPECTIVE_ID = "org.sv.application.defaultperspective";

void sv4guiAppWorkbenchAdvisor::Initialize(berry::IWorkbenchConfigurer::Pointer configurer)
{
    berry::QtWorkbenchAdvisor::Initialize(configurer);

    configurer->SetSaveAndRestore(true);

    ctkPluginContext *pluginContext = sv4guiApplicationPluginActivator::getContext();
    ctkServiceReference serviceReference = pluginContext->getServiceReference<berry::IQtStyleManager>();

    // always granted by org.blueberry.ui.qt
    Q_ASSERT(serviceReference);

    berry::IQtStyleManager *styleManager = pluginContext->getService<berry::IQtStyleManager>(serviceReference);
    Q_ASSERT(styleManager);

    QString styleName = "simvascular";
    styleManager->AddStyle(":/org.sv.gui.qt.application/simvascular.qss", styleName);
    styleManager->SetStyle(":/org.sv.gui.qt.application/simvascular.qss");
}

berry::WorkbenchWindowAdvisor* sv4guiAppWorkbenchAdvisor::CreateWorkbenchWindowAdvisor(berry::IWorkbenchWindowConfigurer::Pointer configurer)
{
    sv4guiWorkbenchWindowAdvisor* advisor = new sv4guiWorkbenchWindowAdvisor(this, configurer);

    // Exclude the help perspective from org.blueberry.ui.qt.help from
    // the normal perspective list.
    // The perspective gets a dedicated menu entry in the help menu
    QList<QString> excludePerspectives;
    excludePerspectives.push_back("org.blueberry.perspectives.help");
    advisor->SetPerspectiveExcludeList(excludePerspectives);

    // Exclude some views from the normal view list
    QList<QString> excludeViews;
    excludeViews.push_back("org.mitk.views.modules");
    excludeViews.push_back( "org.blueberry.ui.internal.introview" );
    advisor->SetViewExcludeList(excludeViews);

    advisor->SetWindowIcon(":/org.sv.gui.qt.application/icon.png");
    return advisor;
}

QString sv4guiAppWorkbenchAdvisor::GetInitialWindowPerspectiveId()
{
    return DEFAULT_PERSPECTIVE_ID;
}

void sv4guiAppWorkbenchAdvisor::UpdateStyle()
{
  ctkPluginContext *pluginContext = sv4guiApplicationPluginActivator::getContext();
  ctkServiceReference serviceReference = pluginContext->getServiceReference<berry::IQtStyleManager>();
  // granted by org.blueberry.ui.qt
  Q_ASSERT(serviceReference);
  berry::IQtStyleManager *styleManager = pluginContext->getService<berry::IQtStyleManager>(serviceReference);
  Q_ASSERT(styleManager);

  styleManager->SetStyle("/Users/adamupdegrove/Desktop/tmp/customstyle.qss");
}
