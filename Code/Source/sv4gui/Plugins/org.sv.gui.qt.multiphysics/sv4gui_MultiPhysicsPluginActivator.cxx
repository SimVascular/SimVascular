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

#include "sv4gui_MultiPhysicsPluginActivator.h"
#include "sv4gui_MultiPhysicsJobCreateAction.h"
#include "sv4gui_MultiPhysicsView.h"
#include "sv4gui_MultiPhysicsPreferencePage.h"
#include <QmitkNodeDescriptorManager.h>
#include <mitkNodePredicateDataType.h>

//sv4guiMultiPhysicsPluginActivator* sv4guiMultiPhysicsPluginActivator::m_Instance = nullptr;
//ctkPluginContext* sv4guiMultiPhysicsPluginActivator::m_Context = nullptr;

void sv4guiMultiPhysicsPluginActivator::start(ctkPluginContext* context)
{
//    m_Instance = this;
//    m_Context = context;

  QmitkNodeDescriptorManager* descriptorManager = QmitkNodeDescriptorManager::GetInstance();

    //mitk::NodePredicateDataType::Pointer isProjectFolder = mitk::NodePredicateDataType::New("sv4guiMultiPhysicsFolder");
    //descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("sv4guiMultiPhysicsFolder"), QString(":MultiPhysics.png"), isProjectFolder, descriptorManager));

    BERRY_REGISTER_EXTENSION_CLASS(sv4guiMultiPhysicsJobCreateAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(sv4guiMultiPhysicsView, context)
    BERRY_REGISTER_EXTENSION_CLASS(sv4guiMultiPhysicsPreferencePage, context)

}

void sv4guiMultiPhysicsPluginActivator::stop(ctkPluginContext* context)
{
//    Q_UNUSED(context)

//    m_Context = nullptr;
//    m_Instance = nullptr;
}

//ctkPluginContext* sv4guiMultiPhysicsPluginActivator::GetContext()
//{
//  return m_Context;
//}

//sv4guiMultiPhysicsPluginActivator* sv4guiMultiPhysicsPluginActivator::GetInstance()
//{
//    return m_Instance;
//}
