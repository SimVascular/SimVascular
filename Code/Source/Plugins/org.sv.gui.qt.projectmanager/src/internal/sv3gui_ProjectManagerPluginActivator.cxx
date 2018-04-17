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

#include "sv3gui_ProjectManagerPluginActivator.h"
#include "sv3gui_ProjectAddImageAction.h"
#include "sv3gui_ProjectCloseAction.h"
#include "sv3gui_ProjectSaveAction.h"
#include "sv3gui_ProjectDuplicateAction.h"
#include "sv3gui_ProjectShowModelEdgesAction.h"
#include "sv3gui_ProjectShowModelFullAction.h"

//svProjectManagerPluginActivator* svProjectManagerPluginActivator::m_Instance = nullptr;
//ctkPluginContext* svProjectManagerPluginActivator::m_Context = nullptr;

svProjectManagerPluginActivator::svProjectManagerPluginActivator()
{
}

svProjectManagerPluginActivator::~svProjectManagerPluginActivator()
{
}

void svProjectManagerPluginActivator::start(ctkPluginContext* context)
{
//    m_Instance = this;
//    m_Context = context;

    BERRY_REGISTER_EXTENSION_CLASS(svProjectAddImageAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svProjectCloseAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svProjectSaveAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svProjectDuplicateAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svProjectShowModelEdgesAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svProjectShowModelFullAction, context)
}

void svProjectManagerPluginActivator::stop(ctkPluginContext* context)
{
//    Q_UNUSED(context)

//    m_Context = nullptr;
//    m_Instance = nullptr;
}

//ctkPluginContext* svProjectManagerPluginActivator::GetContext()
//{
//  return m_Context;
//}

//svProjectManagerPluginActivator* svProjectManagerPluginActivator::GetInstance()
//{
//    return m_Instance;
//}
