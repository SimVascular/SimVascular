#include "svPathSmooth.h"
#include "ui_svPathSmooth.h"

#include "svPath.h"
#include "svPathOperation.h"

#include <mitkDataStorage.h>
#include <mitkDataNode.h>
#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>

#include <QMessageBox>
#include <QFileDialog>

#include <iostream>
using namespace std;

const QString svPathSmooth::EXTENSION_ID = "sv.pathsmooth";

svPathSmooth::svPathSmooth()
    : ui(new Ui::svPathSmooth)
{
}

svPathSmooth::~svPathSmooth()
{
    for (std::vector< std::pair< QmitkNodeDescriptor*, QAction* > >::iterator it = mDescriptorActionList.begin();it != mDescriptorActionList.end(); it++)
    {
        // first== the NodeDescriptor; second== the registered QAction
        (it->first)->RemoveAction(it->second);
    }

    delete ui;
}

void svPathSmooth::CreateQtPartControl( QWidget *parent )
{
    ui->setupUi(parent);
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(SmoothPath()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(Cancel()));

    parent->move(400,400);

}

void svPathSmooth::SetFocus( )
{
    ui->lineEditSubsample->setFocus();
}

void svPathSmooth::SmoothPath()
{
    QList<mitk::DataNode::Pointer> nodes=GetCurrentSelection();
    if(nodes.size()==0)
    {
        return;
    }

    mitk::DataNode::Pointer node=nodes.front();
    svPath* path=dynamic_cast<svPath*>(node->GetData());
    if(path==NULL) return;

    int timeStep=GetTimeStep(path);
    svPathElement* pathElement=path->GetPathElement(timeStep);
    if(pathElement==NULL)
    {
        QMessageBox::information(NULL,"No Path Selected","Please select a path in data manager!");
        return;
    }

    int numModes=ui->lineEditNumber->text().trimmed().toInt();
    if(numModes<2){
        QMessageBox::warning(NULL,"Not Enough Modes","Number of fourier mode must be greater than 1.");
        return;
    }

    int sampleRate=ui->lineEditSubsample->text().trimmed().toInt();

    int currentIndex=ui->comboBoxPointType->currentIndex();
    bool controlPointsBased=currentIndex==0?true:false;

    svPathElement* smoothedPathElement=pathElement->CreateSmoothedPathElement(sampleRate,numModes,controlPointsBased);

    svPathOperation* doOp = new svPathOperation(svPathOperation::OpSETPATHELEMENT,timeStep,smoothedPathElement);

    svPathOperation* undoOp = new svPathOperation(svPathOperation::OpSETPATHELEMENT,timeStep,pathElement);
    mitk::OperationEvent *operationEvent = new mitk::OperationEvent(path, doOp, undoOp, "Set PathElement");
    mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

    path->ExecuteOperation(doOp);

    RequestRenderWindowUpdate();

    hide();
}

void svPathSmooth::Cancel()
{
    hide();
}
