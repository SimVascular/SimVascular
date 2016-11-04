#include "svMeshCreateAction.h"

#include <mitkNodePredicateDataType.h>

//#include <QmitkDataManagerView.h>

svMeshCreateAction::svMeshCreateAction()
    : m_MeshCreateWidget(NULL)
    , m_Functionality(NULL)
{
}

svMeshCreateAction::~svMeshCreateAction()
{
    if(m_MeshCreateWidget)
        delete m_MeshCreateWidget;
}

void svMeshCreateAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isMeshFolder = mitk::NodePredicateDataType::New("svMeshFolder");

    if(!isMeshFolder->CheckNode(selectedNode))
    {
        return;
    }

    try
    {
//        if(!m_Functionality)
//            return;

//        QmitkDataManagerView* dmView=dynamic_cast<QmitkDataManagerView*>(m_Functionality);

//        if(!dmView)
//            return;

//        mitk::IRenderWindowPart* renderWindowPart = dmView->GetRenderWindowPart();

//        if(!renderWindowPart)
//            return;

//        mitk::SliceNavigationController* timeNavigationController=renderWindowPart->GetTimeNavigationController();
        int timeStep=0;
//        if(timeNavigationController)
//        {
//            timeStep=timeNavigationController->GetTime()->GetPos();
//        }

        if(m_MeshCreateWidget)
        {
            delete m_MeshCreateWidget;
        }

        m_MeshCreateWidget=new svMeshCreate(m_DataStorage, selectedNode, timeStep);
        m_MeshCreateWidget->show();
        m_MeshCreateWidget->SetFocus();
    }
    catch(...)
    {
        MITK_ERROR << "Mesh Creation Error!";
    }
}


void svMeshCreateAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

void svMeshCreateAction::SetFunctionality(berry::QtViewPart *functionality)
{
    m_Functionality=functionality;
}

