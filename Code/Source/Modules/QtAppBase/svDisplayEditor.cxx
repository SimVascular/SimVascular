#include "svDisplayEditor.h"
#include "svDisableGLHiDPI.h"
#include <mitkBaseRenderer.h>
#include <mitkTimeGeometry.h>
#include <mitkRenderingManager.h>

#include <QHBoxLayout>

svDisplayEditor::svDisplayEditor(QWidget* parent)
{
    CreatePartControl(parent);
}

svDisplayEditor::~svDisplayEditor()
{
}

void svDisplayEditor::CreateQtPartControl( QWidget *parent )
{
    m_Parent=parent;
    m_DataStorage=GetDataStorage();
    m_DisplayWidget = new QmitkStdMultiWidget();
    m_DisplayWidget->SetDataStorage(m_DataStorage);

    // Initialize views as axial, sagittal, coronar (from top-left to bottom)
    mitk::TimeGeometry::Pointer geo = m_DataStorage->ComputeBoundingGeometry3D(m_DataStorage->GetAll());
    mitk::RenderingManager::GetInstance()->InitializeViews(geo);

    // Initialize bottom-right view as 3D view
    QmitkRenderWindow *renderWindow = m_DisplayWidget->GetRenderWindow4();
    m_DisplayWidget->GetRenderWindow4()->GetRenderer()->SetMapperID(mitk::BaseRenderer::Standard3D);
#ifdef __APPLE__
        disableGLHiDPI(renderWindow->winId());
#endif
    renderWindow->GetRenderer()->SetMapperID(mitk::BaseRenderer::Standard3D);

    // Enable standard handler for levelwindow-slider
    m_DisplayWidget->EnableStandardLevelWindow();

    // Add the displayed views to the DataStorage to see their positions in 2D and 3D
    m_DisplayWidget->AddDisplayPlaneSubTree();
    m_DisplayWidget->AddPlanesToDataStorage();
    m_DisplayWidget->SetWidgetPlanesVisibility(true);

    m_DisplayWidget->DisableDepartmentLogo();

    int crosshairgapsize=0;
    m_DisplayWidget->GetWidgetPlane1()->SetIntProperty("Crosshair.Gap Size", crosshairgapsize);
    m_DisplayWidget->GetWidgetPlane2()->SetIntProperty("Crosshair.Gap Size", crosshairgapsize);
    m_DisplayWidget->GetWidgetPlane3()->SetIntProperty("Crosshair.Gap Size", crosshairgapsize);

    QHBoxLayout* layout=new QHBoxLayout(parent);
    layout->setContentsMargins(0,0,0,0);
    layout->addWidget(m_DisplayWidget);
    //resize(QSize(364,477).expandedTo(minimumSizeHint()));

}

QmitkStdMultiWidget*svDisplayEditor::GetDisplayWidget(){
    return m_DisplayWidget;
}
