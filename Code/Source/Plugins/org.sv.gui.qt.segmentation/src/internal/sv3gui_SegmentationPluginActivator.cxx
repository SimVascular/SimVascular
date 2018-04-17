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

#include "sv3gui_SegmentationPluginActivator.h"
#include "sv3gui_ContourGroupCreateAction.h"
#include "sv3gui_Seg3DCreateAction.h"
#include "sv3gui_SegmentationLegacyLoadAction.h"
#include "sv3gui_SegmentationLegacySaveAction.h"
#include "sv3gui_SegmentationLoadAction.h"
#include "sv3gui_ContourGroupPoint2DSizeAction.h"
#include "sv3gui_ContourGroupPoint3DSizeAction.h"
#include "sv3gui_Seg2DEdit.h"
#include "sv3gui_Seg3DEdit.h"

//svSegmentationPluginActivator* svSegmentationPluginActivator::m_Instance = nullptr;
//ctkPluginContext* svSegmentationPluginActivator::m_Context = nullptr;

void svSegmentationPluginActivator::start(ctkPluginContext* context)
{
//    m_Instance = this;
//    m_Context = context;

    BERRY_REGISTER_EXTENSION_CLASS(svContourGroupCreateAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svSeg3DCreateAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svSegmentationLegacyLoadAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svSegmentationLegacySaveAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svSeg2DEdit, context)
    BERRY_REGISTER_EXTENSION_CLASS(svSeg3DEdit, context)
    BERRY_REGISTER_EXTENSION_CLASS(svContourGroupPoint2DSizeAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svContourGroupPoint3DSizeAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svSegmentationLoadAction, context)
}

void svSegmentationPluginActivator::stop(ctkPluginContext* context)
{
//    Q_UNUSED(context)

//    m_Context = nullptr;
//    m_Instance = nullptr;
}

//ctkPluginContext* svSegmentationPluginActivator::GetContext()
//{
//  return m_Context;
//}

//svSegmentationPluginActivator* svSegmentationPluginActivator::GetInstance()
//{
//    return m_Instance;
//}

