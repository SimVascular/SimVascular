#include "svContourGroupCreate.h"
#include "ui_svContourGroupCreate.h"

#include "svPath.h"
#include "svDataNodeOperation.h"
#include "svLoftingUtils.h"

#include <mitkDataStorage.h>
#include <mitkDataNode.h>
#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>
#include <mitkOperationEvent.h>

#include <QMessageBox>
#include <QFileDialog>

#include <iostream>
using namespace std;

svContourGroupCreate::svContourGroupCreate(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer selectedNode, int timeStep)
    : ui(new Ui::svContourGroupCreate)
    , m_DataStorage(dataStorage)
    , m_SelecteNode(selectedNode)
    , m_TimeStep(timeStep)
    , m_SegFolderNode(NULL)
    , m_PathFolderNode(NULL)
{
    m_Interface=new svDataNodeOperationInterface;

    ui->setupUi(this);
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(CreateGroup()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(Cancel()));
    connect(ui->lineEditGroupName, SIGNAL(returnPressed()), this, SLOT(CreateGroup()));
    move(400,400);

    Activated();
}

svContourGroupCreate::~svContourGroupCreate()
{
    delete ui;
}

void svContourGroupCreate::Activated()
{
    ui->comboBox->clear();

    m_PathFolderNode=NULL;
    m_SegFolderNode=NULL;

    if(m_SelecteNode.IsNull())
        return;

    mitk::DataNode::Pointer selectedNode=m_SelecteNode;

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=m_DataStorage->GetSources (selectedNode,isProjFolder,false);

    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        mitk::NodePredicateDataType::Pointer isSegFolder = mitk::NodePredicateDataType::New("svSegmentationFolder");
        mitk::NodePredicateDataType::Pointer isGroupNode = mitk::NodePredicateDataType::New("svContourGroup");

        if(isSegFolder->CheckNode(selectedNode)){
            m_SegFolderNode=selectedNode;
        }else if(isGroupNode->CheckNode(selectedNode)){
            mitk::DataStorage::SetOfObjects::ConstPointer rs = m_DataStorage->GetSources(selectedNode);
            if(rs->size()>0){
                m_SegFolderNode=rs->GetElement(0);
            }
        }

        rs=m_DataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svPathFolder"));
        if (rs->size()>0)
        {
            m_PathFolderNode=rs->GetElement(0);

            rs=m_DataStorage->GetDerivations(m_PathFolderNode,mitk::NodePredicateDataType::New("svPath"));

            for(int i=0;i<rs->size();i++)
            {
                ui->comboBox->addItem(QString::fromStdString(rs->GetElement(i)->GetName()));
            }
        }

    }

    ui->lineEditGroupName->clear();
}

void svContourGroupCreate::SetFocus( )
{
    ui->comboBox->setFocus();
}

void svContourGroupCreate::CreateGroup()
{
    QString selectedPathName=ui->comboBox->currentText();
    if(selectedPathName=="")
    {
        QMessageBox::warning(NULL,"No Path Selected","Please select a path!");
        return;
    }

    mitk::DataNode::Pointer selectedPathNode=m_DataStorage->GetNamedDerivedNode(selectedPathName.toStdString().c_str(),m_PathFolderNode);

    if(selectedPathNode.IsNull())
    {
        QMessageBox::warning(NULL,"No Path Found!","Please select a existing path!");
        return;
    }

    std::string groupName=ui->lineEditGroupName->text().trimmed().toStdString();

    if(groupName==""){
        groupName=selectedPathNode->GetName();
    }

    mitk::DataNode::Pointer exitingNode=m_DataStorage->GetNamedDerivedNode(groupName.c_str(),m_SegFolderNode);
    if(exitingNode){
        QMessageBox::warning(NULL,"Contour Group Already Created","Please use a different group name!");
        return;
    }

    svContourGroup::Pointer group = svContourGroup::New();

    svLoftingUtils::SetPreferencedValues(group->GetLoftingParam());

    group->SetPathName(selectedPathNode->GetName());
    group->SetDataModified();

    svPath* selectedPath=dynamic_cast<svPath*>(selectedPathNode->GetData());
    if(selectedPath)
    {
        group->SetPathID(selectedPath->GetPathID());
    }

    mitk::DataNode::Pointer groupNode = mitk::DataNode::New();
    groupNode->SetData(group);
    groupNode->SetName(groupName);

//    m_DataStorage->Add(groupNode,m_SegFolderNode);
    mitk::OperationEvent::IncCurrObjectEventId();

    bool undoEnabled=true;
    svDataNodeOperation* doOp = new svDataNodeOperation(svDataNodeOperation::OpADDDATANODE,m_DataStorage,groupNode,m_SegFolderNode);
    if(undoEnabled)
    {
        svDataNodeOperation* undoOp = new svDataNodeOperation(svDataNodeOperation::OpREMOVEDATANODE,m_DataStorage,groupNode,m_SegFolderNode);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Interface, doOp, undoOp, "Add DataNode");
        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
    }
    m_Interface->ExecuteOperation(doOp);

    hide();
}

void svContourGroupCreate::Cancel()
{
    hide();
}

//void svContourGroupCreate::SetPreferencedValues(svLoftingParam* param)
//{
//    berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
//    Q_ASSERT(prefService);
//    berry::IPreferences::Pointer preferences = prefService->GetSystemPreferences()->Node("/org.sv.views.lofting");
//    if(preferences.IsNull())
//        return;

//     param->method= preferences->GetInt("Lofting Method", param->method);

//     param->uDegree= preferences->GetInt("NURBS Lofting U Degree", param->uDegree);
//     param->vDegree= preferences->GetInt("NURBS Lofting V Degree", param->vDegree);
//     param->uKnotSpanType= preferences->Get("NURBS Lofting U Knot Span Type", QString::fromStdString(param->uKnotSpanType)).toStdString();
//     param->vKnotSpanType= preferences->Get("NURBS Lofting V Knot Span Type", QString::fromStdString(param->vKnotSpanType)).toStdString();
//     param->uParametricSpanType= preferences->Get("NURBS Lofting U Parametric Span Type", QString::fromStdString(param->uParametricSpanType)).toStdString();
//     param->vParametricSpanType= preferences->Get("NURBS Lofting V Parametric Span Type", QString::fromStdString(param->vParametricSpanType)).toStdString();

//     param->numOutPtsInSegs= preferences->GetInt("Spline Sampling", param->numOutPtsInSegs);
//     param->samplePerSegment= preferences->GetInt("Spline Point Number Per Segment", param->samplePerSegment);
//     param->useLinearSampleAlongLength= preferences->GetInt("Spline Use Linear Sample", param->useLinearSampleAlongLength);
//     param->linearMuliplier= preferences->GetInt("Spline Linear Sample Factor", param->linearMuliplier);
//     param->useFFT= preferences->GetInt("Spline Use FFT", param->useFFT);
//     param->numModes= preferences->GetInt("Spline FFT Mode Number", param->numModes);
//}
