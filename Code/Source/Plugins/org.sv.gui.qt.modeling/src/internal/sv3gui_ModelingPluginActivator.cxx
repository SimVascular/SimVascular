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

#include "sv3gui_ModelingPluginActivator.h"
#include "sv3gui_ModelCreateAction.h"
#include "sv3gui_LoftingPreferencePage.h"
#include "sv3gui_ModelLoadAction.h"
#include "sv3gui_ModelLegacySaveAction.h"
#include "sv3gui_ModelExtractPathsAction.h"
#include "sv3gui_ModelFaceInfoExportAction.h"
#include "sv3gui_ModelEdit.h"

//svModelingPluginActivator* svModelingPluginActivator::m_Instance = nullptr;
//ctkPluginContext* svModelingPluginActivator::m_Context = nullptr;

void svModelingPluginActivator::start(ctkPluginContext* context)
{
//    m_Instance = this;
//    m_Context = context;

    BERRY_REGISTER_EXTENSION_CLASS(svModelCreateAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svLoftingPreferencePage, context)
    BERRY_REGISTER_EXTENSION_CLASS(svModelLoadAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svModelLegacySaveAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svModelExtractPathsAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svModelFaceInfoExportAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svModelEdit, context)
}

void svModelingPluginActivator::stop(ctkPluginContext* context)
{
//    Q_UNUSED(context)

//    m_Context = nullptr;
//    m_Instance = nullptr;
}

//ctkPluginContext* svModelingPluginActivator::GetContext()
//{
//  return m_Context;
//}

//svModelingPluginActivator* svModelingPluginActivator::GetInstance()
//{
//    return m_Instance;
//}
