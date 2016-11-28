#include "svSimJobCreateAction.h"

#include <mitkNodePredicateDataType.h>

//#include <QmitkDataManagerView.h>

svSimJobCreateAction::svSimJobCreateAction()
    : m_SimJobCreateWidget(NULL)
    , m_Functionality(NULL)
{
}

svSimJobCreateAction::~svSimJobCreateAction()
{
    if(m_SimJobCreateWidget)
        delete m_SimJobCreateWidget;
}

void svSimJobCreateAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isSimulationFolder = mitk::NodePredicateDataType::New("svSimulationFolder");

    if(!isSimulationFolder->CheckNode(selectedNode))
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
//        int timeStep=0;
//        if(timeNavigationController)
//        {
//            timeStep=timeNavigationController->GetTime()->GetPos();
//        }

        if(m_SimJobCreateWidget)
        {
            delete m_SimJobCreateWidget;
        }

        m_SimJobCreateWidget=new svSimJobCreate(m_DataStorage, selectedNode);
        m_SimJobCreateWidget->show();
        m_SimJobCreateWidget->SetFocus();
    }
    catch(...)
    {
        MITK_ERROR << "Simulation Job Creation Error!";
    }
}


void svSimJobCreateAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

void svSimJobCreateAction::SetFunctionality(berry::QtViewPart *functionality)
{
    m_Functionality=functionality;
}

