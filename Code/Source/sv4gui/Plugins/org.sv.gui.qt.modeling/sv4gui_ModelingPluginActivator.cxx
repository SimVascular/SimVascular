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

#include "sv4gui_ModelingPluginActivator.h"
#include "sv4gui_ModelCreateAction.h"
#include "sv4gui_LoftingPreferencePage.h"
#include "sv4gui_ModelLoadAction.h"
#include "sv4gui_ModelLegacySaveAction.h"
#include "sv4gui_ModelExtractPathsAction.h"
#include "sv4gui_ModelFaceInfoExportAction.h"
#include "sv4gui_ModelEdit.h"

//sv4guiModelingPluginActivator* sv4guiModelingPluginActivator::m_Instance = nullptr;
//ctkPluginContext* sv4guiModelingPluginActivator::m_Context = nullptr;

void sv4guiModelingPluginActivator::start(ctkPluginContext* context)
{
//    m_Instance = this;
//    m_Context = context;
    std::cout << "sv4guiModelingPluginActivator::start()" << std::endl;
    BERRY_REGISTER_EXTENSION_CLASS(sv4guiModelCreateAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(sv4guiLoftingPreferencePage, context)
    BERRY_REGISTER_EXTENSION_CLASS(sv4guiModelLoadAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(sv4guiModelLegacySaveAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(sv4guiModelExtractPathsAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(sv4guiModelFaceInfoExportAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(sv4guiModelEdit, context)
}

void sv4guiModelingPluginActivator::stop(ctkPluginContext* context)
{
//    Q_UNUSED(context)

//    m_Context = nullptr;
//    m_Instance = nullptr;
}

//ctkPluginContext* sv4guiModelingPluginActivator::GetContext()
//{
//  return m_Context;
//}

//sv4guiModelingPluginActivator* sv4guiModelingPluginActivator::GetInstance()
//{
//    return m_Instance;
//}
