#include "svContourGroupVtkMapper2D.h"
#include "svContourGroup.h"

svContourGroupVtkMapper2D::svContourGroupVtkMapper2D()
{
}

svContourGroupVtkMapper2D:: ~svContourGroupVtkMapper2D()
{
}

void svContourGroupVtkMapper2D::FindContourOnCurrentSlice(mitk::BaseRenderer* renderer, unsigned int t)
{
    svContourGroup* input  = static_cast<svContourGroup*>(GetDataNode()->GetData());

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


