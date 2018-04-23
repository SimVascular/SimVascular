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

#include "sv4gui_ContourGroupVtkMapper2D.h"
#include "sv4gui_ContourGroup.h"

sv4guiContourGroupVtkMapper2D::sv4guiContourGroupVtkMapper2D()
{
}

sv4guiContourGroupVtkMapper2D:: ~sv4guiContourGroupVtkMapper2D()
{
}

void sv4guiContourGroupVtkMapper2D::FindContourOnCurrentSlice(mitk::BaseRenderer* renderer, unsigned int t)
{
    sv4guiContourGroup* input  = static_cast<sv4guiContourGroup*>(GetDataNode()->GetData());

    // only update the input data, if the property tells us to
    bool update = true;
    this->GetDataNode()->GetBoolProperty("updateDataOnRender", update);
    if (update)
    {
        input->Update();
    }

    const mitk::PlaneGeometry *rendererPlaneGeometry = renderer->GetCurrentWorldPlaneGeometry();
    m_Contour=NULL;
    if(input!=NULL && rendererPlaneGeometry!=NULL)
    {
        int contourIndex=input->SearchContourByPlane(rendererPlaneGeometry,1.0,t);
        m_Contour=input->GetContour(contourIndex,t);

        std::string renderName(renderer->GetName());
        if(renderName=="stdmulti.widget1")
        {
            input->SetCurrentIndexOn2DView(contourIndex);
        }
    }
}


