
#include "svDefaultPerspective.h"
#include "berryIViewLayout.h"

svDefaultPerspective::svDefaultPerspective()
{
}

void svDefaultPerspective::CreateInitialLayout(berry::IPageLayout::Pointer layout)
{
    QString editorArea = layout->GetEditorArea();

    layout->AddView("org.mitk.views.datamanager", berry::IPageLayout::LEFT, 0.2f, editorArea);
    berry::IViewLayout::Pointer lo = layout->GetViewLayout("org.mitk.views.datamanager");
    lo->SetCloseable(false);

    layout->AddView("org.mitk.views.volumevisualization", berry::IPageLayout::LEFT, 0.3f, editorArea);
//    layout->AddView("org.sv.views.pathplanning", berry::IPageLayout::LEFT, 0.3f, editorArea);

    layout->AddView("org.mitk.views.imagenavigator", berry::IPageLayout::BOTTOM, 0.5f, "org.mitk.views.datamanager");

    berry::IPlaceholderFolderLayout::Pointer bottomFolder = layout->CreatePlaceholderFolder("bottom", berry::IPageLayout::BOTTOM, 0.7f, editorArea);
    bottomFolder->AddPlaceholder("org.blueberry.views.logview");
    bottomFolder->AddPlaceholder("org.mitk.views.modules");

    layout->AddPerspectiveShortcut("org.mitk.extapp.defaultperspective");
    layout->AddPerspectiveShortcut("org.mitk.mitkworkbench.perspectives.editor");
    layout->AddPerspectiveShortcut("org.mitk.mitkworkbench.perspectives.visualization");
}
