#include "svSeg3DCreateAction.h"
#include "svMitkSeg3D.h"
#include "svDataNodeOperation.h"

#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>

#include <QInputDialog>
#include <QMessageBox>

svSeg3DCreateAction::svSeg3DCreateAction()
    : m_Functionality(NULL)
{
    m_Interface=new svDataNodeOperationInterface;
}

svSeg3DCreateAction::~svSeg3DCreateAction()
{
}

void svSeg3DCreateAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer segFolderNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isSegFolder = mitk::NodePredicateDataType::New("svSegmentationFolder");

    if(!isSegFolder->CheckNode(segFolderNode))
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

        bool ok;
        QString text = QInputDialog::getText(NULL, tr("Create 3D Segmentation"),
                                             tr("Name:"), QLineEdit::Normal,
                                             "", &ok);
        if (!ok || text.trimmed().isEmpty())
            return;

        std::string segName=text.trimmed().toStdString();

        mitk::DataNode::Pointer existingNode=m_DataStorage->GetNamedDerivedNode(segName.c_str(),segFolderNode);
        if(existingNode){
            QMessageBox::warning(NULL,"Name Already Exists","Please use a different name!");
            return;
        }

        svMitkSeg3D::Pointer mitkSeg3D = svMitkSeg3D::New();
        mitkSeg3D->SetDataModified();

        svSeg3D* seg3D=new svSeg3D();
        mitkSeg3D->SetSeg3D(seg3D);

        mitk::DataNode::Pointer segNode = mitk::DataNode::New();
        segNode->SetData(mitkSeg3D);
        segNode->SetName(segName);

    //    m_DataStorage->Add(groupNode,m_SegFolderNode);
        mitk::OperationEvent::IncCurrObjectEventId();

        bool undoEnabled=true;
        svDataNodeOperation* doOp = new svDataNodeOperation(svDataNodeOperation::OpADDDATANODE,m_DataStorage,segNode,segFolderNode);
        if(undoEnabled)
        {
            svDataNodeOperation* undoOp = new svDataNodeOperation(svDataNodeOperation::OpREMOVEDATANODE,m_DataStorage,segNode,segFolderNode);
            mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Interface, doOp, undoOp, "Add DataNode");
            mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
        }
        m_Interface->ExecuteOperation(doOp);

    }
    catch(...)
    {
        MITK_ERROR << "3D Segmentation Data Node Creation Error!";
    }
}


void svSeg3DCreateAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

void svSeg3DCreateAction::SetFunctionality(berry::QtViewPart *functionality)
{
    m_Functionality=functionality;
}

