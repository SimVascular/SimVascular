#include "svSimJobStopAction.h"

#include <mitkNodePredicateDataType.h>

#include "svSimulationView.h"

svSimJobStopAction::svSimJobStopAction()
    : m_Functionality(NULL)
{
}

svSimJobStopAction::~svSimJobStopAction()
{
}

void svSimJobStopAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isMitkSimJob = mitk::NodePredicateDataType::New("svMitkSimJob");

    if(!isMitkSimJob->CheckNode(selectedNode))
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

        if (QMessageBox::question(NULL, "Stop Simulation", "Are you sure to stop simulatin for this job?",
                                  QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
        {
          return;
        }

          bool running=false;
          selectedNode->GetBoolProperty("running",running);

          if(running)
          {
              svSolverProcessHandler* handler=NULL;
              bool ok=selectedNode->GetPropertyValue<svSolverProcessHandler*>("process handler",handler);

              if(ok && handler)
              {
                  handler->KillProcess();
              }

          }
          else
          {
              QMessageBox::information(NULL,"Info","The selected job is not running.");
              return;
          }


    }
    catch(...)
    {
        MITK_ERROR << "Error during stopping job!";
    }
}


void svSimJobStopAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

void svSimJobStopAction::SetFunctionality(berry::QtViewPart *functionality)
{
    m_Functionality=functionality;
}

