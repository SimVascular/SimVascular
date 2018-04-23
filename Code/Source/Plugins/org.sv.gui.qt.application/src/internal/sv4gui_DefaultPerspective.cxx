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

#include "sv4gui_DefaultPerspective.h"
#include "berryIViewLayout.h"

sv4guiDefaultPerspective::sv4guiDefaultPerspective()
{
}

void sv4guiDefaultPerspective::CreateInitialLayout(berry::IPageLayout::Pointer layout)
{
    QString editorArea = layout->GetEditorArea();

    layout->AddView("org.sv.views.datamanager", berry::IPageLayout::LEFT, 0.2f, editorArea);
    berry::IViewLayout::Pointer lo = layout->GetViewLayout("org.sv.views.datamanager");
    lo->SetCloseable(false);

//    layout->AddView("org.mitk.views.volumevisualization", berry::IPageLayout::LEFT, 0.3f, editorArea);
//    layout->AddView("org.sv.views.pathplanning", berry::IPageLayout::LEFT, 0.3f, editorArea);

    layout->AddView("org.mitk.views.imagenavigator", berry::IPageLayout::BOTTOM, 0.5f, "org.sv.views.datamanager");

    berry::IPlaceholderFolderLayout::Pointer bottomFolder = layout->CreatePlaceholderFolder("bottom", berry::IPageLayout::BOTTOM, 0.7f, editorArea);
    bottomFolder->AddPlaceholder("org.blueberry.views.logview");
    bottomFolder->AddPlaceholder("org.mitk.views.modules");

    layout->AddPerspectiveShortcut("org.mitk.extapp.defaultperspective");
    layout->AddPerspectiveShortcut("org.mitk.mitkworkbench.perspectives.editor");
    layout->AddPerspectiveShortcut("org.mitk.mitkworkbench.perspectives.visualization");
}
