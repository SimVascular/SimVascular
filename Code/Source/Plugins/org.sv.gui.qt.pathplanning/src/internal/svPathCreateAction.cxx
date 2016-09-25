#include "svPathCreateAction.h"

#include <mitkNodePredicateDataType.h>

//#include <QmitkDataManagerView.h>

svPathCreateAction::svPathCreateAction()
    : m_PathCreateWidget(NULL)
    , m_Functionality(NULL)
{
}

svPathCreateAction::~svPathCreateAction()
{
    if(m_PathCreateWidget)
        delete m_PathCreateWidget;
}

void svPathCreateAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isPathFolder = mitk::NodePredicateDataType::New("svPathFolder");

    if(!isPathFolder->CheckNode(selectedNode))
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

        if(m_PathCreateWidget)
        {
            delete m_PathCreateWidget;
        }

        m_PathCreateWidget=new svPathCreate(m_DataStorage, selectedNode, timeStep);
        m_PathCreateWidget->show();
        m_PathCreateWidget->SetFocus();


    }
    catch(...)
    {
        MITK_ERROR << "Path Creation Error!";
    }
}


void svPathCreateAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

void svPathCreateAction::SetFunctionality(berry::QtViewPart *functionality)
{
    m_Functionality=functionality;
}

