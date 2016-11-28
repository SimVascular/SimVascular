#include "svPathSmooth.h"
#include "ui_svPathSmooth.h"

#include "svPath.h"
#include "svPathOperation.h"

#include <mitkDataStorage.h>
#include <mitkDataNode.h>
#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>
#include <mitkRenderingManager.h>

#include <QMessageBox>
#include <QFileDialog>

#include <iostream>
using namespace std;

svPathSmooth::svPathSmooth(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer selectedNode, int timeStep)
    : ui(new Ui::svPathSmooth)
    , m_DataStorage(dataStorage)
    , m_SelecteNode(selectedNode)
    , m_TimeStep(timeStep)
{
    ui->setupUi(this);
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(SmoothPath()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(Cancel()));

    move(400,400);
}

svPathSmooth::~svPathSmooth()
{
    delete ui;
}

void svPathSmooth::SetFocus( )
{
    ui->lineEditSubsample->setFocus();
}

void svPathSmooth::SmoothPath()
{
    if(m_SelecteNode.IsNull())
    {
        return;
    }

    svPath* path=dynamic_cast<svPath*>(m_SelecteNode->GetData());
    if(path==NULL) return;

    int timeStep=m_TimeStep;
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

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();

    hide();
}

void svPathSmooth::Cancel()
{
    hide();
}
