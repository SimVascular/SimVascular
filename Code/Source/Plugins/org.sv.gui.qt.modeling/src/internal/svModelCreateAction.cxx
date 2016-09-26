#include "svModelCreateAction.h"

#include <mitkNodePredicateDataType.h>

//#include <QmitkDataManagerView.h>

svModelCreateAction::svModelCreateAction()
    : m_ModelCreateWidget(NULL)
    , m_Functionality(NULL)
{
}

svModelCreateAction::~svModelCreateAction()
{
    if(m_ModelCreateWidget)
        delete m_ModelCreateWidget;
}

void svModelCreateAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isModelFolder = mitk::NodePredicateDataType::New("svModelFolder");

    if(!isModelFolder->CheckNode(selectedNode))
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

        if(m_ModelCreateWidget)
        {
            delete m_ModelCreateWidget;
        }

        m_ModelCreateWidget=new svModelCreate(m_DataStorage, selectedNode, timeStep);
        m_ModelCreateWidget->show();
        m_ModelCreateWidget->SetFocus();
    }
    catch(...)
    {
        MITK_ERROR << "Model Creation Error!";
    }
}


void svModelCreateAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

void svModelCreateAction::SetFunctionality(berry::QtViewPart *functionality)
{
    m_Functionality=functionality;
}

