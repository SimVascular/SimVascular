/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "sv4gui_Seg2DEdit.h"
#include "ui_sv4gui_Seg2DEdit.h"
#include "ui_sv4gui_LevelSet2DWidget.h"
#include "ui_sv4gui_LoftParamWidget.h"

#include "sv4gui_Path.h"
#include "sv4gui_SegmentationUtils.h"
#include "sv4gui_Math3.h"

#include "sv4gui_ContourGroup.h"
#include "sv4gui_ContourGroupDataInteractor.h"
#include "sv4gui_ContourCircle.h"
#include "sv4gui_ContourEllipse.h"
#include "sv4gui_ContourPolygon.h"
#include "sv4gui_ContourSplinePolygon.h"
#include "sv4gui_ContourOperation.h"
#include "sv4gui_ContourModel.h"
#include "sv4gui_ContourModelThresholdInteractor.h"

#include "sv4gui_ModelUtils.h"

#include <QmitkStdMultiWidgetEditor.h>
#include <mitkOperationEvent.h>
#include <mitkUndoController.h>
#include <mitkStatusBar.h>
#include <mitkProgressBar.h>
#include <mitkNodePredicateDataType.h>

#include <usModuleRegistry.h>

// Qt
#include <QMessageBox>
#include <QMenu>
#include <QInputDialog>
#include <QWheelEvent>
#include <QVBoxLayout>
#include <QWidget>

#include <QDir>
#include <QString>
#include <QDomDocument>
#include <QDomElement>
#include <QFile>
#include <QAbstractItemView>
#include <QListWidgetItem>

#include <iostream>
using namespace std;

#include <math.h>

const QString sv4guiSeg2DEdit::EXTENSION_ID = "org.sv.views.segmentation2d";

//-----------------
// sv4guiSeg2DEdit
//-----------------
//
sv4guiSeg2DEdit::sv4guiSeg2DEdit() : ui(new Ui::sv4guiSeg2DEdit)
{
    m_ContourGroupChangeObserverTag=-1;
    m_ContourGroup=NULL;
    m_ContourGroupNode=NULL;
    m_GroupFolderNode=NULL;
    m_Path=NULL;
    m_PathNode=NULL;
    m_Image=NULL;
    m_cvImage=NULL;
    m_LoftWidget=NULL;
    m_LSParamWidget=NULL;
    m_StartLoftContourGroupObserverTag=-1;
    m_StartLoftContourGroupObserverTag2=-1;
    m_StartChangingContourObserverTag=-1;
    m_EndChangingContourObserverTag=-1;
    m_SelectContourObserverTag=-1;
    m_ContourChanging=false;
    m_DataInteractor=NULL;

    m_ManualMenu=NULL;
    m_CopyContour=NULL;
    m_ContourGroupCreateWidget=NULL;

    m_PreviewDataNode=NULL;
    m_PreviewContourModel=NULL;
    m_PreviewDataNodeInteractor=NULL;
    m_PreviewContourModelObserverFinishTag=-1;
    m_PreviewContourModelObserverUpdateTag=-1;

    m_UpdatingGUI=false;
}

sv4guiSeg2DEdit::~sv4guiSeg2DEdit()
{
    delete ui;

    if(m_LoftWidget) delete m_LoftWidget;

//    if(m_LSParamWidget) delete m_LSParamWidget;

    if(m_ContourGroupCreateWidget)
        delete m_ContourGroupCreateWidget;
}

//---------------------
// CreateQtPartControl
//---------------------
// Setup and initialize GUI controls.
//
void sv4guiSeg2DEdit::CreateQtPartControl( QWidget *parent )
{
    m_Parent=parent;
    ui->setupUi(parent);

    connect(ui->btnNewGroup,SIGNAL(clicked()), this, SLOT(NewGroup()));

    m_DisplayWidget=GetActiveStdMultiWidget();

    if(m_DisplayWidget==NULL)
    {
        parent->setEnabled(false);
        MITK_ERROR << "Plugin PathEdit Init Error: No QmitkStdMultiWidget!";
        return;
    }

    ui->resliceSlider->SetDisplayWidget(m_DisplayWidget);
    ui->resliceSlider->setCheckBoxVisible(false);
//    ui->resliceSlider->SetResliceMode(mitk::ExtractSliceFilter::RESLICE_NEAREST);
//    ui->resliceSlider->SetResliceMode(mitk::ExtractSliceFilter::RESLICE_LINEAR);
    ui->resliceSlider->SetResliceMode(mitk::ExtractSliceFilter::RESLICE_CUBIC);

    m_CurrentParamWidget=NULL;
    m_CurrentSegButton=NULL;

    QVBoxLayout* vlayout = new QVBoxLayout(ui->lsParamWidgetContainer);
    vlayout->setContentsMargins(0,0,0,0);
    vlayout->setSpacing(0);
    ui->lsParamWidgetContainer->setLayout(vlayout);
    m_LSParamWidget=new sv4guiLevelSet2DWidget(parent);
    vlayout->addWidget(m_LSParamWidget);

    ui->lsParamWidgetContainer->hide();

    ui->thresholdWidgetContainer->hide();
    ui->smoothWidget->hide();
    ui->splineWidget->hide();
    ui->batchWidget->hide();

    parent->setMinimumWidth(400);
//    parent->setFixedWidth(400);

    connect(ui->checkBoxShowPath, SIGNAL(clicked(bool)), this, SLOT(ShowPath(bool)) );

    connect(ui->btnLevelSet, SIGNAL(clicked()), this, SLOT(CreateLSContour()) );
    connect(ui->btnThreshold, SIGNAL(clicked()), this, SLOT(CreateThresholdContour()) );

    connect(ui->btnCircle, SIGNAL(clicked()), this, SLOT(CreateCircle()) );
    connect(ui->btnEllipse, SIGNAL(clicked()), this, SLOT(CreateEllipse()) );
    connect(ui->btnSplinePoly, SIGNAL(clicked()), this, SLOT(CreateSplinePoly()) );
    connect(ui->btnPolygon, SIGNAL(clicked()), this, SLOT(CreatePolygon()) );
    connect(ui->btnSmooth, SIGNAL(clicked()), this, SLOT(SmoothSelected()) );
    connect(ui->btnDelete, SIGNAL(clicked()), this, SLOT(DeleteSelected()) );
    connect(ui->listWidget,SIGNAL(itemSelectionChanged()), this, SLOT(SelectContour()) );
    connect(ui->listWidget,SIGNAL(clicked(const QModelIndex &)), this, SLOT(SelectContour(const QModelIndex &)) );

    connect(ui->checkBoxLoftingPreview, SIGNAL(clicked()), this, SLOT(LoftContourGroup()) );
    connect(ui->btnLoftingOptions, SIGNAL(clicked()), this, SLOT(ShowLoftWidget()) );

    m_LoftWidget=new sv4guiLoftParamWidget();
    m_LoftWidget->move(400,400);
    m_LoftWidget->hide();
    connect(m_LoftWidget->ui->btnOK, SIGNAL(clicked()), this, SLOT(OKLofting()) );
    connect(m_LoftWidget->ui->btnApply, SIGNAL(clicked()), this, SLOT(ApplyLofting()) );
    connect(m_LoftWidget->ui->btnClose, SIGNAL(clicked()), this, SLOT(HideLoftWidget()) );

    connect(ui->resliceSlider,SIGNAL(resliceSizeChanged(double)), this, SLOT(UpdatePathResliceSize(double)) );

    connect( ui->btnCircle, SIGNAL(customContextMenuRequested(const QPoint&))
             , this, SLOT(ManualCircleContextMenuRequested(const QPoint&)) );
    connect( ui->btnEllipse, SIGNAL(customContextMenuRequested(const QPoint&))
             , this, SLOT(ManualEllipseContextMenuRequested(const QPoint&)) );
    connect( ui->btnSplinePoly, SIGNAL(customContextMenuRequested(const QPoint&))
             , this, SLOT(ManualSplinePolyContextMenuRequested(const QPoint&)) );
    connect( ui->btnPolygon, SIGNAL(customContextMenuRequested(const QPoint&))
             , this, SLOT(ManualPolygonContextMenuRequested(const QPoint&)) );

    connect(ui->btnCopy, SIGNAL(clicked()), this, SLOT(CopyContour()) );
    connect(ui->btnPaste, SIGNAL(clicked()), this, SLOT(PasteContour()) );

    ui->btnLevelSet->setShortcut(QKeySequence("Ctrl+L"));
    ui->btnThreshold->setShortcut(QKeySequence("Ctrl+T"));
    ui->btnCopy->setShortcut(QKeySequence("Ctrl+C"));
    ui->btnPaste->setShortcut(QKeySequence("Ctrl+V"));
    ui->btnDelete->setShortcut(QKeySequence("Ctrl+D"));

    ui->listWidget->installEventFilter(this);

    //ml additions
    setupMLui();

    // If m_ContourGroup is null then the panel is not associated with any
    // contour group. This happens when tool panels from previous sessions
    // are created when SV starts.
    //
    if (m_ContourGroup == nullptr) {
        ui->SinglePathTab->setEnabled(false);
    } else {
        ui->SinglePathTab->setEnabled(true);
    }
}

void sv4guiSeg2DEdit::Visible()
{
//    ui->resliceSlider->turnOnReslice(true);
    OnSelectionChanged(GetDataManagerSelection());
}

void sv4guiSeg2DEdit::Hidden()
{
    ui->resliceSlider->turnOnReslice(false);
    ClearAll();
}

//bool sv4guiSeg2DEdit::IsExclusiveFunctionality() const
//{
//    return true;
//}

int sv4guiSeg2DEdit::GetTimeStep()
{
    mitk::SliceNavigationController* timeNavigationController = NULL;
    if(m_DisplayWidget)
    {
        timeNavigationController=m_DisplayWidget->GetTimeNavigationController();
    }

    if(timeNavigationController)
        return timeNavigationController->GetTime()->GetPos();
    else
        return 0;

}

//--------------------
// OnSelectionChanged
//--------------------
// This is called any control in the Data Manager is selected.
//
// Lots of data is set here:
//
//   m_Image
//
void sv4guiSeg2DEdit::OnSelectionChanged(std::vector<mitk::DataNode*> nodes )
{
    std::cout << "OnSelectionChanged\n";

    if(!IsVisible())
    {
        return;
    }

    if(nodes.size()==0)
    {
        ui->resliceSlider->turnOnReslice(false);
        ClearAll();
        ui->SinglePathTab->setEnabled(false);
        return;
    }

    mitk::DataNode::Pointer groupNode=nodes.front();

    if(m_ContourGroupNode==groupNode)
    {
        ui->resliceSlider->turnOnReslice(true);
        return;
    }

    ClearAll();

    m_ContourGroupNode=groupNode;
    m_ContourGroup=dynamic_cast<sv4guiContourGroup*>(groupNode->GetData());
    if(!m_ContourGroup)
    {
        std::cout << "no contour group selected\n";
        ui->resliceSlider->turnOnReslice(false);
        ClearAll();
        ui->SinglePathTab->setEnabled(false);
        return;
    }

    m_Parent->setEnabled(true);
    ui->SinglePathTab->setEnabled(true);
    ////remove_toolbox     ui->segToolbox->setCurrentIndex(1);
//    std::string groupPathName=m_ContourGroup->GetPathName();
    int  groupPathID=m_ContourGroup->GetPathID();

//    mitk::DataNode::Pointer pathNode=NULL;
    mitk::DataNode::Pointer imageNode=NULL;
    m_Image=NULL;
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (m_ContourGroupNode,isProjFolder,false);

    m_PathNode=NULL;

    if(rs->size()>0) {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        rs=GetDataStorage()->GetDerivations (projFolderNode,mitk::NodePredicateDataType::New("sv4guiImageFolder"));
        if(rs->size()>0) {
            mitk::DataNode::Pointer imageFolderNode=rs->GetElement(0);
            rs=GetDataStorage()->GetDerivations(imageFolderNode);
            if(rs->size()>0) {
                imageNode=rs->GetElement(0);
                if(imageNode.IsNotNull()) {
                    m_Image= dynamic_cast<mitk::Image*>(imageNode->GetData());
                }
            }

        }

        rs=GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiPathFolder"));
        if (rs->size()>0)
        {
            mitk::DataNode::Pointer pathFolderNode=rs->GetElement(0);
            rs=GetDataStorage()->GetDerivations(pathFolderNode);

            for(int i=0;i<rs->size();i++)
            {
                sv4guiPath* path=dynamic_cast<sv4guiPath*>(rs->GetElement(i)->GetData());

                if(path&&groupPathID==path->GetPathID())
                {
                    m_Path=path;
                    m_PathNode=rs->GetElement(i);
                    break;
                }
            }

        }

    }

//    if(!m_Image){
//        QMessageBox::warning(NULL,"No image found for this project","Make sure the image is loaded!");
////        return;
//    }

    if(!m_Path){
        QMessageBox::warning(NULL,"No path found for this contour group","Make sure the path for the contour group exits!");
        return;
    }

    rs=GetDataStorage()->GetSources(m_ContourGroupNode);
    m_GroupFolderNode=NULL;
    if(rs->size()>0)
        m_GroupFolderNode=rs->GetElement(0);

    ui->labelGroupName->setText(QString::fromStdString(m_ContourGroupNode->GetName()));

    ui->labelPathName->setText(QString::fromStdString(m_PathNode->GetName()));
    if(m_PathNode->IsVisible(NULL))
        ui->checkBoxShowPath->setChecked(true);
    else
        ui->checkBoxShowPath->setChecked(false);

    UpdateContourList();

    if(m_Image)
        m_cvImage=sv4guiSegmentationUtils::image2cvStrPts(m_Image);
    else
        m_cvImage=NULL;

    int timeStep=GetTimeStep();
    sv4guiPathElement* pathElement=m_Path->GetPathElement(timeStep);
    if(pathElement==NULL)
        return;

    std::vector<sv4guiPathElement::sv4guiPathPoint> pathPoints=pathElement->GetPathPoints();
    std::vector<mitk::Point3D> pathPosPoints=pathElement->GetPathPosPoints();

    for(int i=0;i<m_ContourGroup->GetSize(timeStep);i++)
    {
        sv4guiContour* contour=m_ContourGroup->GetContour(i,timeStep);
        if(contour==NULL) continue;

        int insertingIndex=sv4guiMath3::GetInsertintIndexByDistance(pathPosPoints, contour->GetPathPosPoint(), false);
        if(insertingIndex!=-2)
        {
            sv4guiPathElement::sv4guiPathPoint pp1=pathElement->GetPathPoint(insertingIndex);
            sv4guiPathElement::sv4guiPathPoint pp2=contour->GetPathPoint();

            //if the two path points have the same position and tangent, do not add
            if(pp1.pos[0]==pp2.pos[0] && pp1.pos[1]==pp2.pos[1] && pp1.pos[2]==pp2.pos[2]
               && pp1.tangent[0]==pp2.tangent[0] && pp1.tangent[1]==pp2.tangent[1] && pp1.tangent[2]==pp2.tangent[2])
                continue;

            pathPosPoints.insert(pathPosPoints.begin()+insertingIndex,contour->GetPathPosPoint());
            pathPoints.insert(pathPoints.begin()+insertingIndex,contour->GetPathPoint());
        }
    }

    //set tag index for contours in the group
    for(int i=0;i<m_ContourGroup->GetSize(timeStep);i++)
    {
        sv4guiContour* contour=m_ContourGroup->GetContour(i,timeStep);
        if(contour==NULL) continue;

        for(int j=0;j<pathPoints.size();j++)
        {
            if(pathPoints[j].pos==contour->GetPathPosPoint())
            {
                contour->SetTagIndex(j);
                break;
            }
        }
    }
    m_PathPoints=pathPoints;

    //set visible range for 3D view
    mitk::BaseData* baseData=NULL;
    if(imageNode.IsNotNull())
        baseData=imageNode->GetData();
    else if(m_PathNode.IsNotNull())
        baseData=m_PathNode->GetData();

    if ( baseData && baseData->GetTimeGeometry()->IsValid() )
    {
        mitk::RenderingManager::GetInstance()->InitializeViews(
                    baseData->GetTimeGeometry(), mitk::RenderingManager::REQUEST_UPDATE_ALL, true );
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }

    //set resice slider
    ui->resliceSlider->setPathPoints(pathPoints);
    if(imageNode.IsNotNull())
        ui->resliceSlider->setDataNode(imageNode);
    else
        ui->resliceSlider->setDataNode(m_PathNode);

    //ui->resliceSlider->setDataNode(m_ContourGroupNode);


    double resliceSize=m_ContourGroup->GetResliceSize();
    if(resliceSize==0)
    {
        resliceSize=5.0;
        m_ContourGroup->SetResliceSize(resliceSize);
    }
    ui->resliceSlider->setResliceSize(resliceSize);
    ui->resliceSlider->updateReslice();

    m_DataInteractor = sv4guiContourGroupDataInteractor::New();
    m_DataInteractor->SetInteraction3D(false);
    m_DataInteractor->LoadStateMachine("sv4gui_ContourGroupInteraction.xml", us::ModuleRegistry::GetModule("sv4guiModuleSegmentation"));
    m_DataInteractor->SetEventConfig("sv4gui_SegmentationConfig.xml", us::ModuleRegistry::GetModule("sv4guiModuleSegmentation"));
    m_DataInteractor->SetDataNode(m_ContourGroupNode);

    //Add Observer
    itk::SimpleMemberCommand<sv4guiSeg2DEdit>::Pointer groupChangeCommand = itk::SimpleMemberCommand<sv4guiSeg2DEdit>::New();
    groupChangeCommand->SetCallbackFunction(this, &sv4guiSeg2DEdit::UpdateContourList);
    m_ContourGroupChangeObserverTag = m_ContourGroup->AddObserver( sv4guiContourGroupEvent(), groupChangeCommand);

    itk::SimpleMemberCommand<sv4guiSeg2DEdit>::Pointer loftCommand = itk::SimpleMemberCommand<sv4guiSeg2DEdit>::New();
    loftCommand->SetCallbackFunction(this, &sv4guiSeg2DEdit::LoftContourGroup);
    m_StartLoftContourGroupObserverTag = m_ContourGroup->AddObserver( sv4guiContourGroupChangeEvent(), loftCommand);
    m_StartLoftContourGroupObserverTag2 = m_ContourGroup->AddObserver( sv4guiContourChangeEvent(), loftCommand);

    itk::SimpleMemberCommand<sv4guiSeg2DEdit>::Pointer flagOnCommand = itk::SimpleMemberCommand<sv4guiSeg2DEdit>::New();
    flagOnCommand->SetCallbackFunction(this, &sv4guiSeg2DEdit::ContourChangingOn);
    m_StartChangingContourObserverTag = m_ContourGroup->AddObserver( StartChangingContourEvent(), flagOnCommand);

    itk::SimpleMemberCommand<sv4guiSeg2DEdit>::Pointer flagOffCommand = itk::SimpleMemberCommand<sv4guiSeg2DEdit>::New();
    flagOffCommand->SetCallbackFunction(this, &sv4guiSeg2DEdit::ContourChangingOff);
    m_EndChangingContourObserverTag = m_ContourGroup->AddObserver( EndChangingContourEvent(), flagOffCommand);

    itk::SimpleMemberCommand<sv4guiSeg2DEdit>::Pointer selectCommand = itk::SimpleMemberCommand<sv4guiSeg2DEdit>::New();
    selectCommand->SetCallbackFunction(this, &sv4guiSeg2DEdit::SelectContour3D);
    m_SelectContourObserverTag = m_ContourGroup->AddObserver( SelectContourEvent(), selectCommand);

    double range[2]={0,100};
    if(m_cvImage)
        m_cvImage->GetVtkStructuredPoints()->GetScalarRange(range);

    ui->sliderThreshold->setMinimum(range[0]);
    ui->sliderThreshold->setMaximum(range[1]);

    bool lofting=false;
    m_ContourGroupNode->GetBoolProperty("lofting",lofting);
    ui->checkBoxLoftingPreview->setChecked(lofting);

    ui->resliceSlider->turnOnReslice(true);

    m_DataInteractor->SetPathPoints(m_PathPoints);
    m_DataInteractor->SetPathPoint(ui->resliceSlider->getCurrentPathPoint());
    m_DataInteractor->SetSubdivisionSpacing(GetVolumeImageSpacing());

    connect(ui->resliceSlider,SIGNAL(reslicePositionChanged(int)), this, SLOT(UpdatePathPoint(int)) );

}

double sv4guiSeg2DEdit::GetVolumeImageSpacing()
{
    double minSpacing=0.1;
    if(m_Image)
    {
        mitk::Vector3D spacing=m_Image->GetGeometry()->GetSpacing();
        minSpacing=std::min(spacing[0],std::min(spacing[1],spacing[2]));
    }
    return minSpacing;
}

void sv4guiSeg2DEdit::InsertContour(sv4guiContour* contour, int contourIndex)
{
    if(m_ContourGroup&&contour&&contourIndex>-2){

        m_ContourGroup->DeselectContours();

        contour->SetSelected(true);

        int timeStep=GetTimeStep();
        sv4guiContourOperation* doOp = new sv4guiContourOperation(sv4guiContourOperation::OpINSERTCONTOUR,timeStep,contour,contourIndex);

        sv4guiContourOperation *undoOp = new sv4guiContourOperation(sv4guiContourOperation::OpREMOVECONTOUR,timeStep, contour, contourIndex);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_ContourGroup, doOp, undoOp, "Insert Contour");

        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

        m_ContourGroup->ExecuteOperation(doOp);

        mitk::RenderingManager::GetInstance()->RequestUpdateAll();

    }
}

void sv4guiSeg2DEdit::InsertContourByPathPosPoint(sv4guiContour* contour)
{
    if(m_ContourGroup&&contour)
    {
//        mitk::OperationEvent::IncCurrObjectEventId();

        int index=m_ContourGroup->GetContourIndexByPathPosPoint(contour->GetPathPosPoint());
        if(index!=-2)
        {
            SetContour(index, contour);
        }else{
            for(int i=0;i<m_PathPoints.size();i++)
            {
                if(m_PathPoints[i].pos==contour->GetPathPosPoint())
                    contour->SetTagIndex(i);
            }

//            index=m_ContourGroup->GetInsertingContourIndexByPathPosPoint(contour->GetPathPosPoint());
            index=m_ContourGroup->GetInsertingContourIndexByTagIndex(contour->GetTagIndex());
            InsertContour(contour,index);
        }
    }

}

void sv4guiSeg2DEdit::SetContour(int contourIndex, sv4guiContour* newContour)
{
    if(m_ContourGroup&&contourIndex>-2)
    {
        sv4guiContour* originalContour=m_ContourGroup->GetContour(contourIndex);

        newContour->SetSelected(true);

        int timeStep=GetTimeStep();
        sv4guiContourOperation* doOp = new sv4guiContourOperation(sv4guiContourOperation::OpSETCONTOUR,timeStep,newContour,contourIndex);

        sv4guiContourOperation *undoOp = new sv4guiContourOperation(sv4guiContourOperation::OpSETCONTOUR,timeStep, originalContour, contourIndex);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_ContourGroup, doOp, undoOp, "Set Contour");

        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

        m_ContourGroup->ExecuteOperation(doOp);

        mitk::RenderingManager::GetInstance()->RequestUpdateAll();

    }
}

void sv4guiSeg2DEdit::RemoveContour(int contourIndex)
{
    if(m_ContourGroup&&contourIndex>-2)
    {
        sv4guiContour* contour=m_ContourGroup->GetContour(contourIndex);

        int timeStep=GetTimeStep();
        sv4guiContourOperation* doOp = new sv4guiContourOperation(sv4guiContourOperation::OpREMOVECONTOUR,timeStep,contour,contourIndex);

        sv4guiContourOperation *undoOp = new sv4guiContourOperation(sv4guiContourOperation::OpINSERTCONTOUR,timeStep, contour, contourIndex);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_ContourGroup, doOp, undoOp, "Remove Contour");

        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

        m_ContourGroup->ExecuteOperation(doOp);

        mitk::RenderingManager::GetInstance()->RequestUpdateAll();

    }
}

std::vector<int> sv4guiSeg2DEdit::GetBatchList()
{
    std::vector<int> batchList;

    int maxID=ui->resliceSlider->GetSliceNumber();
    if(maxID<1) return batchList;

    QString line=ui->lineEditBatchList->text().trimmed();
    line=line.replace("begin",QString::number(0));
    line=line.replace("end",QString::number(maxID));

    QStringList list = line.split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);

    for(int i=0;i<list.size();i++)
    {
        QString str=list[i];

        if(str.contains(":"))
        {
            QStringList list2 = str.split(QRegExp(":"));

            if(list2.size()==0 ||list2.size()>3) continue;

            if(list2.size()==1)
            {
                bool ok;
                int posID=list2[0].toInt(&ok);
                if(ok&&posID>=0&&posID<=maxID) batchList.push_back(posID);
            }
            else
            {
                bool ok;
                int beginID, endID, interval;

                beginID=list2[0].toInt(&ok);
                if(!ok) continue;

                if(list2.size()==2)
                {
                    endID=list2[1].toInt(&ok);
                    if(!ok) continue;
                    interval=1;
                }else{
                    interval=list2[1].toInt(&ok);
                    if(!ok) continue;
                    endID=list2[2].toInt(&ok);
                    if(!ok) continue;
                }

                if(beginID<0 || beginID>maxID || endID<0 || endID>maxID) continue;

                for(int j=beginID;j<=endID;j=j+interval)
                    batchList.push_back(j);

                if(batchList.back()!=endID)
                    batchList.push_back(endID);
            }

        }
        else
        {
            bool ok;
            int posID=str.toInt(&ok);
            if(ok&&posID>=0&&posID<=maxID) batchList.push_back(posID);
        }

    }

    return batchList;

}

sv4guiContour* sv4guiSeg2DEdit::PostprocessContour(sv4guiContour* contour)
{
    sv4guiContour* contourNew=contour;

    int smoothNumber=0;
    if(ui->checkBoxSmooth->isChecked())
    {
        smoothNumber=ui->spinBoxSmoothNumber->value();
    }

    int splineControlNumber=0;
    if(ui->checkBoxSpline->isChecked())
    {
        splineControlNumber=ui->spinBoxControlNumber->value();
    }

    if(smoothNumber>0)
    {
        contourNew=contour->CreateSmoothedContour(smoothNumber);
    }

    if(splineControlNumber>1)
    {
        contourNew=sv4guiContourSplinePolygon::CreateByFitting(contourNew,splineControlNumber);
        contourNew->SetSubdivisionType(sv4guiContour::CONSTANT_SPACING);
        contourNew->SetSubdivisionSpacing(GetVolumeImageSpacing());
    }

    return contourNew;
}

void sv4guiSeg2DEdit::CreateContours(SegmentationMethod method)
{
    if(m_cvImage==NULL)
        return;

    bool usingBatch;
    std::vector<int> posList;
    if(ui->checkBoxBatch->isChecked())
    {
        usingBatch=true;
        posList=GetBatchList();

        if(posList.size()>50)
        {
            QString msg = "There will be "+ QString::number(posList.size()) + " segmentations to do. Are you sure?";
            if (QMessageBox::question(NULL, "Warning", msg,
                                      QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
            {
              return;
            }
        }

    }
    else
    {
        usingBatch=false;
        posList.push_back(ui->resliceSlider->getCurrentSliceIndex());
    }

    if(posList.size()>0)
        mitk::OperationEvent::IncCurrObjectEventId();

    mitk::ProgressBar::GetInstance()->AddStepsToDo(posList.size());

    for(int i=0;i<posList.size();i++)
    {
        int posID=posList[i];

        if(usingBatch)
        {
//            ui->resliceSlider->setSlicePos(posID);
        }

        sv4guiContour* contour = NULL;

        auto pathPoint = ui->resliceSlider->getPathPoint(posID);
        auto imageVolume = m_cvImage->GetVtkStructuredPoints(); 
        auto sliceSize = ui->resliceSlider->getResliceSize();
        auto imageTransform = sv4guiSegmentationUtils::GetImageTransformation(m_Image);

        switch(method) {
            case LEVELSET_METHOD: {
                sv4guiSegmentationUtils::svLSParam tmpLSParam = m_LSParamWidget->GetLSParam();
                contour = sv4guiSegmentationUtils::CreateLSContour(pathPoint, imageVolume, &tmpLSParam, sliceSize, imageTransform); 
                break;
            }
            case THRESHOLD_METHOD: {
                auto thresholdValue = ui->sliderThreshold->value();
                contour = sv4guiSegmentationUtils::CreateThresholdContour(pathPoint, imageVolume, thresholdValue, sliceSize, imageTransform);
                break;
            }
            case ML_METHOD:
                contour = doMLContour(pathPoint);
                break;
            default:
                break;
        }

        if(contour && contour->GetContourPointNumber()>2)
        {
            contour=PostprocessContour(contour);

            InsertContourByPathPosPoint(contour);

            LoftContourGroup();

            mitk::StatusBar::GetInstance()->DisplayText("contour added");
        }
        else
        {
            if(contour)
                delete contour;

            if(posList.size()==1) {
                QMessageBox::warning(NULL, "2D Segmentation",
                  "The image could not be segmented using the current parameter settings.\nThe image widow may also not contain enough data (pixel values) to distinguish a vessel boundary.");
            }
        }

        mitk::ProgressBar::GetInstance()->Progress(1);
    }

    //LoftContourGroup();
}

void sv4guiSeg2DEdit::SetSecondaryWidgetsVisible(bool visible)
{
    ui->smoothWidget->setVisible(visible);
    ui->splineWidget->setVisible(visible);
    ui->batchWidget->setVisible(visible);
}

void sv4guiSeg2DEdit::ResetGUI()
{
    if(m_CurrentParamWidget)
    {
        m_CurrentParamWidget->hide();
        m_CurrentParamWidget=NULL;
    }

    if(m_CurrentSegButton)
        m_CurrentSegButton->setStyleSheet("");

    if(m_DataInteractor.IsNotNull())
        m_DataInteractor->SetMethod("");

    QuitPreviewInteraction();

    if(m_ContourGroup)
        m_ContourGroup->RemoveInvalidContours(GetTimeStep());

    m_ContourChanging=false;
}

void sv4guiSeg2DEdit::CreateLSContour()
{
    if(m_CurrentParamWidget==NULL||m_CurrentParamWidget!=ui->lsParamWidgetContainer)
    {
        ResetGUI();

        m_CurrentParamWidget=ui->lsParamWidgetContainer;
        m_CurrentParamWidget->show();

        m_CurrentSegButton=ui->btnLevelSet;

        SetSecondaryWidgetsVisible(true);

        return;
    }

    CreateContours(LEVELSET_METHOD);

}

void sv4guiSeg2DEdit::CreateThresholdContour()
{
    if(m_cvImage==NULL)
        return;

    if(m_CurrentParamWidget==NULL||m_CurrentParamWidget!=ui->thresholdWidgetContainer)
    {
        ResetGUI();

        m_CurrentParamWidget=ui->thresholdWidgetContainer;
        m_CurrentParamWidget->show();

        m_CurrentSegButton=ui->btnThreshold;

        SetSecondaryWidgetsVisible(true);

        return;
    }

    if(ui->checkBoxPresetThreshold->isChecked())
        CreateContours(THRESHOLD_METHOD);

    if(m_CurrentSegButton->styleSheet().trimmed()!="")
        return;

    m_CurrentSegButton->setStyleSheet("background-color: lightskyblue");

    PreparePreviewInteraction("Threshold");

}

void sv4guiSeg2DEdit::CreateMLContour()
{
    if(m_cvImage==NULL)
        return;

    if(m_CurrentSegButton!=ui->btnML)
    {
        ResetGUI();

        m_CurrentSegButton=ui->btnML;

        SetSecondaryWidgetsVisible(true);

        initialize();

        return;
    }

    CreateContours(ML_METHOD);

}

void sv4guiSeg2DEdit::UpdatePreview()
{
    if(m_PreviewContourModel.IsNull())
         return;

    if(m_PreviewDataNodeInteractor.IsNotNull())
        ui->sliderThreshold->setValue(m_PreviewDataNodeInteractor->GetCurrentValue());
}

void sv4guiSeg2DEdit::FinishPreview()
{
    if(m_PreviewContourModel.IsNull())
         return;

    int timeStep=GetTimeStep();

    sv4guiContour* contour=m_PreviewContourModel->GetContour(timeStep);
    m_PreviewContourModel->SetContour(NULL);

    if(contour && contour->GetContourPointNumber()>2)
    {
        contour=PostprocessContour(contour);

        mitk::OperationEvent::IncCurrObjectEventId();

        InsertContourByPathPosPoint(contour);

        LoftContourGroup();
    }
    else
    {
        if(contour)
            delete contour;

//        QMessageBox::warning(NULL,"No Valid Contour Created","Contour not created and added since it's invalid");
    }
}

void sv4guiSeg2DEdit::CreateEllipse()
{
    if(m_CurrentSegButton!=ui->btnEllipse)
    {
        ResetGUI();

        SetSecondaryWidgetsVisible(false);
    }

    int index=m_ContourGroup->GetContourIndexByPathPosPoint(ui->resliceSlider->getCurrentPathPoint().pos);

    sv4guiContour* existingContour=m_ContourGroup->GetContour(index);

    sv4guiContour* contour=NULL;
    if(existingContour && existingContour->GetContourPointNumber()>2)
    {
        contour=sv4guiContourEllipse::CreateByFitting(existingContour);
        if(contour)
        {
            contour->SetSubdivisionType(sv4guiContour::CONSTANT_SPACING);
            contour->SetSubdivisionSpacing(GetVolumeImageSpacing());
            mitk::OperationEvent::IncCurrObjectEventId();

            InsertContourByPathPosPoint(contour);
        }
    }

    m_CurrentSegButton=ui->btnEllipse;
    m_CurrentSegButton->setStyleSheet("background-color: lightskyblue");

    m_DataInteractor->SetMethod("Ellipse");
}

void sv4guiSeg2DEdit::CreateCircle()
{
    if(m_CurrentSegButton!=ui->btnCircle)
    {
        ResetGUI();

        SetSecondaryWidgetsVisible(false);
    }

    int index=m_ContourGroup->GetContourIndexByPathPosPoint(ui->resliceSlider->getCurrentPathPoint().pos);

    sv4guiContour* existingContour=m_ContourGroup->GetContour(index);

    sv4guiContour* contour=NULL;
    if(existingContour && existingContour->GetContourPointNumber()>2)
    {
        contour=sv4guiContourCircle::CreateByFitting(existingContour);
        if(contour)
        {
            contour->SetSubdivisionType(sv4guiContour::CONSTANT_SPACING);
            contour->SetSubdivisionSpacing(GetVolumeImageSpacing());
            mitk::OperationEvent::IncCurrObjectEventId();

            InsertContourByPathPosPoint(contour);
        }
    }

    m_CurrentSegButton=ui->btnCircle;
    m_CurrentSegButton->setStyleSheet("background-color: lightskyblue");
    m_DataInteractor->SetMethod("Circle");
}

void sv4guiSeg2DEdit::CreateSplinePoly()
{
    if(m_CurrentSegButton!=ui->btnSplinePoly)
    {
        ResetGUI();

        SetSecondaryWidgetsVisible(false);
    }

    int index=m_ContourGroup->GetContourIndexByPathPosPoint(ui->resliceSlider->getCurrentPathPoint().pos);

    sv4guiContour* existingContour=m_ContourGroup->GetContour(index);

    sv4guiContour* contour=NULL;
    if(existingContour && existingContour->GetContourPointNumber()>2)
    {
        int splineControlNumber=ui->spinBoxControlNumber->value();
        if(splineControlNumber<3)
            splineControlNumber=3;

        contour=sv4guiContourSplinePolygon::CreateByFitting(existingContour,splineControlNumber);
        if(contour)
        {
            contour->SetSubdivisionType(sv4guiContour::CONSTANT_SPACING);
            contour->SetSubdivisionSpacing(GetVolumeImageSpacing());
            mitk::OperationEvent::IncCurrObjectEventId();

            InsertContourByPathPosPoint(contour);
        }
    }

    m_CurrentSegButton=ui->btnSplinePoly;
    m_CurrentSegButton->setStyleSheet("background-color: lightskyblue");

    m_DataInteractor->SetMethod("SplinePolygon");
}

void sv4guiSeg2DEdit::CreatePolygon()
{
    if(m_CurrentSegButton!=ui->btnPolygon)
    {
        ResetGUI();

        SetSecondaryWidgetsVisible(false);
    }

    int index=m_ContourGroup->GetContourIndexByPathPosPoint(ui->resliceSlider->getCurrentPathPoint().pos);

    RemoveContour(index);

    LoftContourGroup();

    m_CurrentSegButton=ui->btnPolygon;
    m_CurrentSegButton->setStyleSheet("background-color: lightskyblue");

    m_DataInteractor->SetMethod("Polygon");
}

void sv4guiSeg2DEdit::SmoothSelected()
{
//    int fourierNumber=12;
    int fourierNumber=ui->spinBoxSmoothNumber->value();

    QModelIndexList selectedRows=ui->listWidget->selectionModel()->selectedRows();

    if(selectedRows.isEmpty())
        return;

    int index= selectedRows.front().row();

    sv4guiContour* smoothedContour=m_ContourGroup->GetContour(index)->CreateSmoothedContour(fourierNumber);

    InsertContourByPathPosPoint(smoothedContour);

    LoftContourGroup();
}

void sv4guiSeg2DEdit::DeleteSelected()
{
    QModelIndexList selectedRows=ui->listWidget->selectionModel()->selectedRows();
    if(selectedRows.size()>0)
    {
        RemoveContour(selectedRows.front().row());
    }

    LoftContourGroup();
}

void sv4guiSeg2DEdit::SelectContour(int index)
{
    if(m_UpdatingGUI)
        return;

    mitk::StatusBar::GetInstance()->DisplayText("");

    if(m_ContourGroup && index>-1)
    {

        m_ContourGroup->DeselectContours();

        m_ContourGroup->SetContourSelected(index,true);
        sv4guiContour* contour=m_ContourGroup->GetContour(index);
        if(contour && contour->GetControlPointNumber()>1)
        {
            ui->resliceSlider->moveToPathPosPoint(contour->GetPathPosPoint());

            mitk::Point3D centerPoint=contour->GetCenterPoint();
            QString info="Contour Geometry Info: Area="+QString::number(contour->GetArea())
                    +", Perimeter="+QString::number(contour->GetPerimeter())
                    +", Center Point=("+QString::number(centerPoint[0])+","+QString::number(centerPoint[1])+","+QString::number(centerPoint[2])+")";
            mitk::StatusBar::GetInstance()->DisplayText(info.toStdString().c_str());
        }

        mitk::RenderingManager::GetInstance()->ForceImmediateUpdateAll();
    }
}

void sv4guiSeg2DEdit::SelectContour()
{
    if(m_UpdatingGUI)
        return;

    int index=-1;
    QModelIndexList selectedRows=ui->listWidget->selectionModel()->selectedRows();
    if(selectedRows.size()>0)
    {
        index=selectedRows.front().row();
        SelectContour(index);
    }

}

void sv4guiSeg2DEdit::SelectContour(const QModelIndex & idx)
{
    int index=idx.row();
    SelectContour(index);
}

void sv4guiSeg2DEdit::NodeChanged(const mitk::DataNode* node)
{
}

void sv4guiSeg2DEdit::NodeAdded(const mitk::DataNode* node)
{
}

void sv4guiSeg2DEdit::NodeRemoved(const mitk::DataNode* node)
{
    OnSelectionChanged(GetDataManagerSelection());
}

void sv4guiSeg2DEdit::ClearAll()
{
    //Remove Observer
    if(m_ContourGroup && m_ContourGroupChangeObserverTag!=-1)
    {
        m_ContourGroup->RemoveObserver(m_ContourGroupChangeObserverTag);
        m_ContourGroupChangeObserverTag=-1;
    }

    if(m_ContourGroup && m_StartLoftContourGroupObserverTag!=-1)
    {
        m_ContourGroup->RemoveObserver(m_StartLoftContourGroupObserverTag);
        m_StartLoftContourGroupObserverTag=-1;
    }

    if(m_ContourGroup && m_StartLoftContourGroupObserverTag2!=-1)
    {
        m_ContourGroup->RemoveObserver(m_StartLoftContourGroupObserverTag2);
        m_StartLoftContourGroupObserverTag2=-1;
    }

    if(m_ContourGroup && m_StartChangingContourObserverTag!=-1)
    {
        m_ContourGroup->RemoveObserver(m_StartChangingContourObserverTag);
        m_StartChangingContourObserverTag=-1;
    }

    if(m_ContourGroup && m_EndChangingContourObserverTag!=-1)
    {
        m_ContourGroup->RemoveObserver(m_EndChangingContourObserverTag);
        m_EndChangingContourObserverTag=-1;
    }

    if(m_ContourGroup && m_SelectContourObserverTag!=-1)
    {
        m_ContourGroup->RemoveObserver(m_SelectContourObserverTag);
        m_SelectContourObserverTag=-1;
    }

    if(m_ContourGroupNode.IsNotNull())
    {
        m_ContourGroupNode->SetDataInteractor(NULL);
        m_DataInteractor=NULL;
    }

    m_ContourGroup=NULL;
    m_ContourGroupNode=NULL;
    m_Path=NULL;

    ui->labelGroupName->setText("");
    ui->listWidget->clear();

    disconnect(ui->resliceSlider,SIGNAL(reslicePositionChanged(int)), this, SLOT(UpdatePathPoint(int)) );

    ResetGUI();
}

void sv4guiSeg2DEdit::UpdateContourList()
{
    if(m_ContourGroup==NULL) return;

    int timeStep=GetTimeStep();

    m_UpdatingGUI=true;

    ui->listWidget->clear();

    for(int index=0;index<m_ContourGroup->GetSize(timeStep);index++){

        sv4guiContour* contour=m_ContourGroup->GetContour(index,timeStep);
        if(contour)
        {
            QString item=QString::number(index)+": "+QString::fromStdString(contour->GetType())+", "+QString::fromStdString(contour->GetMethod());
            ui->listWidget->addItem(item);
        }

    }

    int selectedIndex=m_ContourGroup->GetSelectedContourIndex(timeStep);
    if(selectedIndex>-2)
    {
        QModelIndex mIndex=ui->listWidget->model()->index(selectedIndex,0);
        ui->listWidget->selectionModel()->select(mIndex, QItemSelectionModel::ClearAndSelect);
    }

    m_UpdatingGUI=false;
}

void sv4guiSeg2DEdit::LoftContourGroup()
{
    if(m_ContourChanging)
        return;

    if(m_ContourGroupNode.IsNull())
        return;

    m_LoftSurfaceNode=GetDataStorage()->GetNamedDerivedNode("Lofted",m_ContourGroupNode);

    if(ui->checkBoxLoftingPreview->isChecked())
    {
        m_ContourGroupNode->SetBoolProperty("lofting",true);

        if(m_LoftSurfaceNode.IsNull())
        {
            m_LoftSurface=mitk::Surface::New();
            m_LoftSurfaceNode = mitk::DataNode::New();
            m_LoftSurfaceNode->SetData(m_LoftSurface);
            m_LoftSurfaceNode->SetName("Lofted");
//            m_LoftSurfaceNode->SetBoolProperty("helper object", true);
            GetDataStorage()->Add(m_LoftSurfaceNode,m_ContourGroupNode);
        }
        else
        {
            m_LoftSurface=dynamic_cast<mitk::Surface*>(m_LoftSurfaceNode->GetData());
            m_LoftSurfaceNode->SetVisibility(true);
        }

        int timeStep=GetTimeStep();

        m_ContourGroup->RemoveInvalidContours(timeStep);
        if(m_ContourGroup->GetSize()>1)
            m_LoftSurface->SetVtkPolyData(sv4guiModelUtils::CreateLoftSurface(m_ContourGroup,0,0,NULL,timeStep),timeStep);

    }
    else
    {
        m_ContourGroupNode->SetBoolProperty("lofting",false);

        if(m_LoftSurfaceNode.IsNotNull())
            m_LoftSurfaceNode->SetVisibility(false);
    }

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();

}

void sv4guiSeg2DEdit::ShowLoftWidget()
{
    svLoftingParam *param = m_ContourGroup->GetLoftingParam();
    m_LoftWidget->UpdateGUI(param);
    m_LoftWidget->show();
}

void sv4guiSeg2DEdit::UpdateContourGroupLoftingParam()
{
    svLoftingParam *param=m_ContourGroup->GetLoftingParam();
    if(m_LoftWidget)
    {
        m_LoftWidget->UpdateParam(param);
        m_ContourGroup->SetDataModified();
    }

//    param->numOutPtsInSegs=m_LoftWidget->ui->spinBoxSampling->value();
//    param->samplePerSegment=m_LoftWidget->ui->spinBoxNumPerSeg->value();
//    param->useLinearSampleAlongLength=m_LoftWidget->ui->checkBoxUseLinearSample->isChecked()?1:0;
//    param->linearMuliplier=m_LoftWidget->ui->spinBoxLinearFactor->value();
//    param->useFFT=m_LoftWidget->ui->checkBoxUseFFT->isChecked()?1:0;
//    param->numModes=m_LoftWidget->ui->spinBoxNumModes->value();
}

void sv4guiSeg2DEdit::OKLofting()
{
    UpdateContourGroupLoftingParam();
    LoftContourGroup();
    m_LoftWidget->hide();
}

void sv4guiSeg2DEdit::ApplyLofting()
{
    UpdateContourGroupLoftingParam();
    LoftContourGroup();
}

void sv4guiSeg2DEdit::HideLoftWidget()
{
    m_LoftWidget->hide();
}

void sv4guiSeg2DEdit::ContourChangingOn()
{
    m_ContourChanging=true;
}

void sv4guiSeg2DEdit::ContourChangingOff()
{
    m_ContourChanging=false;
//    if(m_CurrentSegButton)
//        m_CurrentSegButton->setStyleSheet("");
}

void sv4guiSeg2DEdit::UpdatePathResliceSize(double newSize)
{
    if(m_ContourGroup)
    {
        m_ContourGroup->SetResliceSize(newSize);
        m_ContourGroup->SetDataModified();
    }

    if(m_GroupFolderNode.IsNotNull())
        m_GroupFolderNode->SetFloatProperty("reslice size",newSize);
}

void sv4guiSeg2DEdit::ManualContextMenuRequested()
{
    m_ManualMenu->popup(QCursor::pos());
}

void sv4guiSeg2DEdit::ManualCircleContextMenuRequested(const QPoint &pos)
{
  if (m_CurrentSegButton == NULL)
    return;
  if (m_CurrentSegButton != ui->btnCircle)
    return;

  QMenu contextMenu(tr("Manual Circle Button"), m_Parent);
  QAction action1("Create Manual Circle", m_Parent);
  connect(&action1, SIGNAL(triggered()), this, SLOT(CreateManualCircle()));

  contextMenu.addAction(&action1);
  contextMenu.exec(m_CurrentSegButton->mapToGlobal(pos));
}

void sv4guiSeg2DEdit::ManualEllipseContextMenuRequested(const QPoint &pos)
{
  if (m_CurrentSegButton == NULL)
    return;
  if (m_CurrentSegButton != ui->btnEllipse)
    return;

  QMenu contextMenu(tr("Manual Ellipse Button"), m_Parent);
  QAction action1("Create Manual Ellipse", m_Parent);
  connect(&action1, SIGNAL(triggered()), this, SLOT(CreateManualEllipse()));

  contextMenu.addAction(&action1);
  contextMenu.exec(m_CurrentSegButton->mapToGlobal(pos));
}

void sv4guiSeg2DEdit::ManualSplinePolyContextMenuRequested(const QPoint &pos)
{
  if (m_CurrentSegButton == NULL)
    return;
  if (m_CurrentSegButton != ui->btnSplinePoly)
    return;

  QMenu contextMenu(tr("Manual Spline Poly Button"), m_Parent);
  QAction action1("Create Manual Spline Poly", m_Parent);
  connect(&action1, SIGNAL(triggered()), this, SLOT(CreateManualSplinePoly()));

  contextMenu.addAction(&action1);
  contextMenu.exec(m_CurrentSegButton->mapToGlobal(pos));
}

void sv4guiSeg2DEdit::ManualPolygonContextMenuRequested(const QPoint &pos)
{
  if (m_CurrentSegButton == NULL)
    return;
  if (m_CurrentSegButton != ui->btnPolygon)
    return;

  QMenu contextMenu(tr("Manual Polygon Button"), m_Parent);
  QAction action1("Create Manual Polygon", m_Parent);
  connect(&action1, SIGNAL(triggered()), this, SLOT(CreateManualPolygon()));

  contextMenu.addAction(&action1);
  contextMenu.exec(m_CurrentSegButton->mapToGlobal(pos));
}

void sv4guiSeg2DEdit::CreateManualCircle(bool)
{
    bool ok;
    QString text = QInputDialog::getText(m_Parent, tr("Circle Input"),
                                         tr("radius, or x y radius:"), QLineEdit::Normal,
                                         "", &ok);
    if (!ok || text.trimmed().isEmpty())
        return;

    QStringList list = text.trimmed().split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
    if(list.size()!=1 && list.size()!=3)
    {
        QMessageBox::warning(m_Parent,"Input Invalid","Please provide valid input!");
        return;
    }

    mitk::Point2D centerPoint,boundaryPoint;
    centerPoint[0]=0;
    centerPoint[1]=0;
    double radius;

    for(int i=0;i<list.size();i++)
    {
        ok=false;
        if(list.size()==3 && (i==0 || i==1))
            centerPoint[i]=list[i].toDouble(&ok);
        else
            radius=list[i].toDouble(&ok);
        if(!ok)
        {
            QMessageBox::warning(m_Parent,"Input Invalid","Please provide valid input!");
            return;
        }
    }
    sv4guiContour* contour=new sv4guiContourCircle();
    contour->SetPathPoint(ui->resliceSlider->getCurrentPathPoint());
    contour->SetPlaced(true);
    contour->SetMethod(contour->GetMethod());

    centerPoint[0]+=contour->GetPlaneGeometry()->GetSpacing()[0]*contour->GetPlaneGeometry()->GetBounds()[1]/2;
    centerPoint[1]+=contour->GetPlaneGeometry()->GetSpacing()[1]*contour->GetPlaneGeometry()->GetBounds()[3]/2;
    boundaryPoint[0]=centerPoint[0]+radius;
    boundaryPoint[1]=centerPoint[1];

    mitk::Point3D pt1,pt2;
    contour->GetPlaneGeometry()->Map(centerPoint,pt1);
    contour->GetPlaneGeometry()->Map(boundaryPoint,pt2);
    std::vector<mitk::Point3D> controlPoints;
    controlPoints.push_back(pt1);
    controlPoints.push_back(pt2);
    contour->SetControlPoints(controlPoints);
    contour->SetSubdivisionType(sv4guiContour::CONSTANT_SPACING);
    contour->SetSubdivisionSpacing(GetVolumeImageSpacing());


    mitk::OperationEvent::IncCurrObjectEventId();

    InsertContourByPathPosPoint(contour);
}

void sv4guiSeg2DEdit::CreateManualEllipse(bool)
{
    bool ok;
    QString text = QInputDialog::getText(m_Parent, tr("Ellipse Input"),
                                         tr("a b, or a b theta, or x y a b, or x y a b theta:"), QLineEdit::Normal,
                                         "", &ok);
    if (!ok || text.trimmed().isEmpty())
        return;

    QStringList list = text.trimmed().split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
    if(list.size()!=2 && list.size()!=3 && list.size()!=4 && list.size()!=5)
    {
        QMessageBox::warning(m_Parent,"Input Invalid","Please provide valid input!");
        return;
    }

    mitk::Point2D centerPoint,boundaryPoint1,boundaryPoint2;
    double x=0;
    double y=0;
    double a=0;
    double b=0;
    double theta=0;

    for(int i=0;i<list.size();i++)
    {
        ok=false;
        double value=list[i].toDouble(&ok);
        if(!ok)
        {
            QMessageBox::warning(m_Parent,"Input Invalid","Please provide valid input!");
            return;
        }

        if(list.size()==2)
        {
            if(i==0) a=value;
            else if(i==1) b=value;
        }

        if(list.size()==3)
        {
            if(i==0) a=value;
            else if(i==1) b=value;
            else if(i==2) theta=value;
        }

        if(list.size()==4)
        {
            if(i==0) x=value;
            else if(i==1) y=value;
            else if(i==2) a=value;
            else if(i==3) b=value;
        }

        if(list.size()==5)
        {
            if(i==0) x=value;
            else if(i==1) y=value;
            else if(i==2) a=value;
            else if(i==3) b=value;
            else if(i==4) theta=value;
        }
    }

    sv4guiContourEllipse* contour=new sv4guiContourEllipse();
    contour->SetPathPoint(ui->resliceSlider->getCurrentPathPoint());
    contour->SetPlaced(true);
    contour->SetMethod(contour->GetMethod());
    contour->SetAsCircle(false);

    centerPoint[0]=x+contour->GetPlaneGeometry()->GetSpacing()[0]*contour->GetPlaneGeometry()->GetBounds()[1]/2;
    centerPoint[1]=y+contour->GetPlaneGeometry()->GetSpacing()[1]*contour->GetPlaneGeometry()->GetBounds()[3]/2;
    boundaryPoint1[0]=centerPoint[0]+a*cos(theta/180.0*vnl_math::pi);
    boundaryPoint1[1]=centerPoint[1]+a*sin(theta/180.0*vnl_math::pi);
    boundaryPoint2[0]=centerPoint[0]+b*cos((theta+90)/180.0*vnl_math::pi);
    boundaryPoint2[1]=centerPoint[1]+b*sin((theta+90)/180.0*vnl_math::pi);

    mitk::Point3D pt0,pt2,pt3;
    contour->GetPlaneGeometry()->Map(centerPoint,pt0);
    contour->GetPlaneGeometry()->Map(boundaryPoint1,pt2);
    contour->GetPlaneGeometry()->Map(boundaryPoint2,pt3);
    std::vector<mitk::Point3D> controlPoints;
    controlPoints.push_back(pt0);
    controlPoints.push_back(pt0);
    controlPoints.push_back(pt2);
    controlPoints.push_back(pt3);

    contour->SetControlPoints(controlPoints,false);
    contour->SetControlPoint(2,pt2);

    contour->SetSubdivisionType(sv4guiContour::CONSTANT_SPACING);
    contour->SetSubdivisionSpacing(GetVolumeImageSpacing());

    mitk::OperationEvent::IncCurrObjectEventId();

    InsertContourByPathPosPoint(contour);
}

void sv4guiSeg2DEdit::CreateManualPolygonType(bool spline)
{
    bool ok;
    QString text = QInputDialog::getText(m_Parent, tr("Polygon Input"),
                                         tr("x1 y1 x2 y2 x3 y3...:"), QLineEdit::Normal,
                                         "", &ok);
    if (!ok || text.trimmed().isEmpty())
        return;

    QStringList list = text.trimmed().split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
    if(list.size()%2!=0)
    {
        QMessageBox::warning(m_Parent,"Input Invalid","Please provide valid input!");
        return;
    }

    std::vector<mitk::Point2D> points;

    for(int i=0;i<list.size();i=i+2)
    {
        bool ok1=false;
        bool ok2=false;
        double value1=list[i].toDouble(&ok1);
        double value2=list[i+1].toDouble(&ok2);
        if(!ok1 || !ok2)
        {
            QMessageBox::warning(m_Parent,"Input Invalid","Please provide valid input!");
            return;
        }

        mitk::Point2D point;
        point[0]=value1;
        point[1]=value2;

        points.push_back(point);
    }

    sv4guiContour* contour=NULL;
    if(spline)
        contour=new sv4guiContourSplinePolygon();
    else
        contour=new sv4guiContourPolygon();

    contour->SetPathPoint(ui->resliceSlider->getCurrentPathPoint());
    contour->SetPlaced(true);
    contour->SetMethod(contour->GetMethod());

    mitk::Point3D pt;
    pt[0]=0;
    pt[1]=0;
    pt[2]=0;

    std::vector<mitk::Point3D> controlPoints;
    controlPoints.push_back(pt);
    controlPoints.push_back(pt);

    double dx=contour->GetPlaneGeometry()->GetSpacing()[0]*contour->GetPlaneGeometry()->GetBounds()[1]/2;
    double dy=contour->GetPlaneGeometry()->GetSpacing()[1]*contour->GetPlaneGeometry()->GetBounds()[3]/2;

    for(int i=0;i<points.size();i++)
    {
        mitk::Point2D pt2d;
        pt2d[0]=points[i][0]+dx;
        pt2d[1]=points[i][1]+dy;
        contour->GetPlaneGeometry()->Map(pt2d,pt);
        controlPoints.push_back(pt);
    }

    contour->SetControlPoints(controlPoints);

    contour->SetSubdivisionType(sv4guiContour::CONSTANT_SPACING);
    contour->SetSubdivisionSpacing(GetVolumeImageSpacing());

    mitk::OperationEvent::IncCurrObjectEventId();

    InsertContourByPathPosPoint(contour);
}

void sv4guiSeg2DEdit::CreateManualSplinePoly(bool)
{
    CreateManualPolygonType(true);
}

void sv4guiSeg2DEdit::CreateManualPolygon(bool)
{
    CreateManualPolygonType(false);
}

void sv4guiSeg2DEdit::CopyContour()
{
    QModelIndexList selectedRows=ui->listWidget->selectionModel()->selectedRows();
    if(selectedRows.size()>0 && m_ContourGroup)
    {
        m_CopyContour=m_ContourGroup->GetContour(selectedRows.front().row(),GetTimeStep());
    }
}

void sv4guiSeg2DEdit::PasteContour()
{
    if(m_CopyContour==NULL)
        return;

    sv4guiContour* contour=m_CopyContour->Clone();
    mitk::PlaneGeometry* oldPlaneGeometry=m_CopyContour->GetPlaneGeometry();
    std::vector<mitk::Point3D> controlPoints;
    std::vector<mitk::Point3D> contourPoints;

    contour->SetPathPoint(ui->resliceSlider->getCurrentPathPoint());
    for(int i=0;i<contour->GetControlPointNumber();i++)
    {
        mitk::Point2D p2d;
        mitk::Point3D p3d;
        oldPlaneGeometry->Map(contour->GetControlPoint(i),p2d);
        contour->GetPlaneGeometry()->Map(p2d,p3d);

        controlPoints.push_back(p3d);
    }
    for(int i=0;i<contour->GetContourPointNumber();i++)
    {
        mitk::Point2D p2d;
        mitk::Point3D p3d;
        oldPlaneGeometry->Map(contour->GetContourPoint(i),p2d);
        contour->GetPlaneGeometry()->Map(p2d,p3d);

        contourPoints.push_back(p3d);
    }

    contour->SetControlPoints(controlPoints,false);
    contour->SetContourPoints(contourPoints,false);

    mitk::OperationEvent::IncCurrObjectEventId();

    InsertContourByPathPosPoint(contour);

    LoftContourGroup();
}

void sv4guiSeg2DEdit::NewGroup()
{
    if(m_ContourGroupNode.IsNull())
        return;

    if(m_ContourGroupCreateWidget)
    {
        delete m_ContourGroupCreateWidget;
    }

    m_ContourGroupCreateWidget=new sv4guiContourGroupCreate(GetDataStorage(), m_ContourGroupNode,0);
    m_ContourGroupCreateWidget->show();
    m_ContourGroupCreateWidget->SetFocus();
}

void sv4guiSeg2DEdit::ShowPath(bool checked)
{
    if(m_PathNode.IsNotNull())
    {
        m_PathNode->SetVisibility(checked);
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }
}

//---------------------------
// PreparePreviewInteraction
//---------------------------
// Setup data structures for interacting with 2D view to select a threshold value.
//
// This extractd a slice from the image volume used for threshold image segmentation.
//
void sv4guiSeg2DEdit::PreparePreviewInteraction(QString method)
{
    //std::cout << "========== sv4guiSeg2DEdit::PreparePreviewInteraction ========== " << std::endl;

    // Create Data Node to show threshold contour.
    m_PreviewContourModel = sv4guiContourModel::New();
    m_PreviewDataNode = mitk::DataNode::New();
    m_PreviewDataNode->SetData(m_PreviewContourModel);
    m_PreviewDataNode->SetName("PreviewContour");
    m_PreviewDataNode->SetBoolProperty("helper object", true);
    GetDataStorage()->Add(m_PreviewDataNode, m_ContourGroupNode);

    // Create an interactor used to select a threshold value.
    m_PreviewDataNodeInteractor = sv4guiContourModelThresholdInteractor::New();
    m_PreviewDataNodeInteractor->LoadStateMachine("sv4gui_ContourModelThresholdInteraction.xml", 
       us::ModuleRegistry::GetModule("sv4guiModuleSegmentation"));
    m_PreviewDataNodeInteractor->SetEventConfig("sv4gui_SegmentationConfig.xml", us::ModuleRegistry::GetModule("sv4guiModuleSegmentation"));
    m_PreviewDataNodeInteractor->SetDataNode(m_PreviewDataNode);
    m_PreviewDataNodeInteractor->SetVtkImageData(m_cvImage->GetVtkStructuredPoints());
    m_PreviewDataNodeInteractor->SetPathPoint(ui->resliceSlider->getCurrentPathPoint());
    m_PreviewDataNodeInteractor->SetGroupInteractor(m_DataInteractor);

    // Get the mitk itk image transformation needed to transform 
    // the 2D image slices definded from path data.
    auto imageTransform = sv4guiSegmentationUtils::GetImageTransformation(m_Image);
    m_PreviewDataNodeInteractor->SetImageTransform(imageTransform);

    // Extract a slice from the image volume.
    auto pathPoint = ui->resliceSlider->getCurrentPathPoint();
    int sliceSize = ui->resliceSlider->getResliceSize();
    cvStrPts* strPts = sv4guiSegmentationUtils::GetSlicevtkImage(pathPoint, m_cvImage->GetVtkStructuredPoints(), sliceSize, imageTransform);

    // Setup callbacks when finishing interactive selection?
    itk::SimpleMemberCommand<sv4guiSeg2DEdit>::Pointer previewFinished = itk::SimpleMemberCommand<sv4guiSeg2DEdit>::New();
    previewFinished->SetCallbackFunction(this, &sv4guiSeg2DEdit::FinishPreview);
    m_PreviewContourModelObserverFinishTag = m_PreviewContourModel->AddObserver( EndInteractionContourModelEvent(), previewFinished);

    itk::SimpleMemberCommand<sv4guiSeg2DEdit>::Pointer previewUpdating = itk::SimpleMemberCommand<sv4guiSeg2DEdit>::New();
    previewUpdating->SetCallbackFunction(this, &sv4guiSeg2DEdit::UpdatePreview);
    m_PreviewContourModelObserverUpdateTag = m_PreviewContourModel->AddObserver( UpdateInteractionContourModelEvent(), previewUpdating);
}

void sv4guiSeg2DEdit::UpdatePathPoint(int pos)
{
    if(m_PreviewDataNodeInteractor.IsNotNull())
        m_PreviewDataNodeInteractor->SetPathPoint(ui->resliceSlider->getCurrentPathPoint());

    if(m_DataInteractor.IsNotNull())
        m_DataInteractor->SetPathPoint(ui->resliceSlider->getCurrentPathPoint());

    if(m_ContourGroup)
    {
        int index=m_ContourGroup->GetContourIndexByPathPosPoint(ui->resliceSlider->getCurrentPathPoint().pos);
        if(index>-1)
            ui->listWidget->setCurrentRow(index);
    }

}

void sv4guiSeg2DEdit::QuitPreviewInteraction()
{
    if( m_PreviewContourModelObserverFinishTag!=-1)
    {
        m_PreviewContourModel->RemoveObserver(m_PreviewContourModelObserverFinishTag);
        m_PreviewContourModelObserverFinishTag=-1;
    }

    if( m_PreviewContourModelObserverUpdateTag!=-1)
    {
        m_PreviewContourModel->RemoveObserver(m_PreviewContourModelObserverUpdateTag);
        m_PreviewContourModelObserverUpdateTag=-1;
    }

    if(m_PreviewDataNode.IsNotNull())
    {
        m_PreviewDataNode->SetDataInteractor(NULL);
        GetDataStorage()->Remove(m_PreviewDataNode);
        m_PreviewDataNode=NULL;
    }

    m_PreviewDataNodeInteractor=NULL;
    m_PreviewContourModel=NULL;
}

bool sv4guiSeg2DEdit::eventFilter(QObject *obj, QEvent *event)
{
    if (obj == ui->listWidget) {
        if (event->type() == QEvent::Wheel) {

            QWheelEvent *wheelEvent = static_cast<QWheelEvent*>(event);
            QPoint numDegrees = wheelEvent->angleDelta();

            int row=-1;
            QModelIndexList selectedRows=ui->listWidget->selectionModel()->selectedRows();
            if(selectedRows.size()>0)
                row=selectedRows.front().row();

            int totalCount=ui->listWidget->count();

            if(numDegrees.y()<-50)
            {
                if(row>-1 && row<totalCount-1)
                    ui->listWidget->setCurrentRow(row+1);
            }
            else if(numDegrees.y()>50)
            {
                if(row>0 && row<totalCount)
                    ui->listWidget->setCurrentRow(row-1);
            }

            return true;
        } else {
            return false;
        }
    } else {
        return sv4guiSeg2DEdit::eventFilter(obj, event);
    }
}


void sv4guiSeg2DEdit::SelectContour3D()
{
    if(m_DataInteractor.IsNotNull())
    {
        int index=m_DataInteractor->GetSelectedContourIndex();
        if(index>-1)
            ui->listWidget->setCurrentRow(index);
    }
}

/*
machine learning additions
*/
void sv4guiSeg2DEdit::setupMLui(){
  connect(ui->multiSegButton, SIGNAL(clicked()), this, SLOT(segmentPaths()));

  connect(ui->selectAllPathsCheckBox, SIGNAL(clicked()), this, SLOT(selectAllPaths()));

  // connect(ui->sampleNetButton, SIGNAL(clicked()), this, SLOT(sampleNetwork()));

  ////remove_toolbox  connect(ui->segToolbox, SIGNAL(currentChanged(int)),
  ////remove_toolbox   this, SLOT(segTabSelected()));

  connect(ui->btnML, SIGNAL(clicked()), this, SLOT(CreateMLContour()) );

  updatePaths();

}

void sv4guiSeg2DEdit::segTabSelected(){
  //std::cout << "select seg tab\n";
  ////remove_toolbox   switch(ui->segToolbox->currentIndex()){
  ////remove_toolbox   case 1:
  ////remove_toolbox   break;

  ////remove_toolbox   case 0:
      //std::cout << "single path selected\n";
      if(!m_ContourGroup)
      {
          ui->resliceSlider->turnOnReslice(false);
          ClearAll();
          ////remove_toolbox  ui->segToolbox->setCurrentIndex(1);
          QMessageBox::warning(NULL,"No segmentation selected","Create, or Select a segmentation before choosing single path segmentation!");
      }
      ////remove_toolbox     break;
      ////remove_toolbox  }
}

/**
 * Function to initialize the SV machine learning module
 *
 * This function uses the project folder node to find the filename of the image
 * volume being used.
 * It then loads the machine learning module with the specified neural network
 * and sets the image volume path.
 *
 */
void sv4guiSeg2DEdit::initialize()
{
  std::cout << "========== sv4guiSeg2DEdit::initialize ==========" << std::endl;
  bool ml_init;
  GetDataStorage()->GetNamedNode("Segmentations")->GetBoolProperty("ml_init",ml_init);

  if(!ml_init){
    QMessageBox::information(NULL,"Initializing machine learning", "The machine learning segmentation data will now be loaded; this may take a minute.");
    GetDataStorage()->GetNamedNode("Segmentations")->SetBoolProperty("ml_init",true);
  }

  mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
  mitk::DataNode::Pointer projFolderNode = GetDataStorage()->GetNode (isProjFolder);

  if (!projFolderNode) {
    return;
  }

  std::string projPath;
  projFolderNode->GetStringProperty("project path", projPath);
  std::cout << "[initialize] projPath: " << projPath << std::endl;
  mitk::DataNode::Pointer imageNode;

  auto rs = GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiImageFolder"));
  if (rs->size() > 0) {
    mitk::DataNode::Pointer imageFolderNode = rs->GetElement(0);
    rs = GetDataStorage()->GetDerivations(imageFolderNode);
    if (rs->size() > 0) {
      imageNode = rs->GetElement(0);
    }
  } 

  if (imageNode.IsNull()) {
    std::cout << "[initialize] imageNode is nullptr " << std::endl;
    return;
  }

  if (imageNode->GetStringProperty("image_absolute_file_name", m_imageFilePath)) { 
    std::cout << "[initialize] image_absolute_file_name: " << m_imageFilePath << std::endl;
  }

  ml_utils = sv4gui_MachineLearningUtils::getInstance("googlenet_c30_train300k_aug10_clean");
  ml_utils->setImage(m_imageFilePath);
}

/**
 * Function to check/uncheck all paths when the all paths checkbox is checked
 */
void sv4guiSeg2DEdit::selectAllPaths(){
  bool checked = (ui->selectAllPathsCheckBox->checkState() == Qt::Checked);

  for (int i = 0; i < ui->pathList->count(); i++){
    if (checked){
      ui->pathList->item(i)->setCheckState(Qt::Checked);
    }else{
      ui->pathList->item(i)->setCheckState(Qt::Unchecked);
    }
  }
}

/**
 * Function to update the list of selected paths in the multi path checklist.
 *
 */
void sv4guiSeg2DEdit::updatePaths(){
  auto dss = GetDataStorage();
  if(dss.IsNull()) {
    std::cout << "No data storage found in sv4guiSeg2DEdit::updatePaths\n";
    return ;
  }

  auto path_folder_node = dss->GetNamedNode("Paths");

  if(path_folder_node == nullptr) {
    std::cout << "No path_folder_node found in sv4guiSeg2DEdit::updatePaths\n";
    return ;
  }

  auto rs       = dss->GetDerivations(path_folder_node);

  if (rs->size() == 0){
    std::cout << "No paths found\n";
    return ;
  }

  ui->pathList->clear();

  for (int i = 0; i < rs->size(); i++){
    mitk::DataNode::Pointer Node=rs->GetElement(i);
    std::cout << i << ": " << Node->GetName() << "\n";

    //auto q_name = QString(Node->GetName().c_str());

    auto item = new QListWidgetItem(Node->GetName().c_str(), ui->pathList);
    item->setFlags(item->flags() | Qt::ItemIsUserCheckable);
    item->setCheckState(Qt::Unchecked);

    ui->pathList->addItem(item);
  }
}

/**
 * Function to segment all paths selected in the multi path segmentation list
 *
 * For each selected path, the suffix code specified during segmentations
 * is used to create a new contour group (if it does not already exist)
 * this contour group is then segmented using the segmentPath function
 *
 */
void sv4guiSeg2DEdit::segmentPaths(){
  //paths
  initialize();
  auto path_folder_node = GetDataStorage()->GetNamedNode("Paths");
  auto paths_list       = GetDataStorage()->GetDerivations(path_folder_node);

  auto seg_folder_node = GetDataStorage()->GetNamedNode("Segmentations");
  auto seg_nodes = GetDataStorage()->GetDerivations(seg_folder_node);
  m_selected_paths.clear();

  // Get the suffix used to form the segmentation name.
  bool ok;
  char* msg = "Enter the suffix used to create new segmentation data nodes from the selected path(s).";
  QString seg_code = QInputDialog::getText(m_Parent, tr("Segmentation Name"), tr(msg), QLineEdit::Normal, "", &ok);

  if (!ok) {
    return;
  }

  if (seg_code.size() == 0) {
    QMessageBox::warning(NULL, "Segmentation Name", "No segmentation suffix given.");
    return;
  }

  // Get the path names for segementation.
  for (int i = 0; i < ui->pathList->count(); i++){
    if (ui->pathList->item(i)->checkState() == Qt::Checked){
      auto name = ui->pathList->item(i)->text().toStdString();
      auto new_seg_name = name + "_" + seg_code.toStdString();

      for (int j = 0; j < seg_nodes->size(); j++){
        auto seg_node = seg_nodes->GetElement(j);
        auto seg_name = seg_node->GetName();
        if (seg_name == new_seg_name){
          QMessageBox::warning(NULL,("Segmentation " + seg_name +" already exists").c_str(),"Please use a different segmentation name!");
          m_selected_paths.clear();
          return;
        }
      }

      m_selected_paths.push_back(name);
      std::cout << "selected " << name << "\n";
    }
  }

  // Perform the ML segmentation for each selected path.
  for (auto const& path_name : m_selected_paths) { 
    mitk::DataNode::Pointer pathNode = nullptr;

    for (int j = 0; j < paths_list->size(); j++){
      pathNode = paths_list->GetElement(j);
      if (pathNode->GetName() == path_name){
        break; 
      }
    }

    // Compute segmentation contours.
    m_current_path_node = pathNode;
    auto path = dynamic_cast<sv4guiPath*>(pathNode->GetData());
    auto contours = segmentPath(path);

    // Create a new data node for the segmentations.
    if (contours.size() != 0) { 
      auto segName = path_name + "_" + seg_code.toStdString();
      createContourGroup(path_name, segName, contours); 
    }
  }

}

/**
 * Function to create an empty contour group and add it to the datamangaer.
 *
 * Used when running multipath segmentation
 *
 * @param path_name, name of the associated path
 * @param seg_name, name of the segmentation to be created
 * @param contours, the list of contours for the segmentation 
 */
void sv4guiSeg2DEdit::createContourGroup(std::string path_name, std::string seg_name, std::vector<sv4guiContour*>& contours)
{
  // Create a new contour group used to store the segmentations.
  m_current_group = sv4guiContourGroup::New();
  m_current_group->SetPathName(path_name);
  m_current_group->SetDataModified();
  sv4guiPath* selectedPath = dynamic_cast<sv4guiPath*>(m_current_path_node->GetData());
  m_current_group->SetPathID(selectedPath->GetPathID());

  // Add the contours to the group.
  int n = 0;
  for (auto const& contour : contours) {
    m_current_group->IsEmptyTimeStep(0);
    m_current_group->InsertContour(n, contour, 0);
    n += 1;
  }

  // Create a new SV Data Manager Segmentations node for the group.
  auto seg_node = mitk::DataNode::New();
  seg_node->SetName(seg_name);
  seg_node->SetData(m_current_group);
  auto seg_folder_node = GetDataStorage()->GetNamedNode("Segmentations");
  GetDataStorage()->Add(seg_node, seg_folder_node);

  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

/**
 * Function to use machine learning to segment a path at discrete intervals.
 *
 * The current path node is obtained from the multipath checklist.
 *
 * Arguments:
 *
 *   path - The path to segment.
 *
 * Returns: A list of contours. 
 */
std::vector<sv4guiContour*>
sv4guiSeg2DEdit::segmentPath(sv4guiPath* path)
{
  auto path_element = path->GetPathElement(0);
  auto path_points = path_element->GetPathPoints();

  m_interval = ui->intervalEdit->value();
  m_numFourierModes = ui->intervalEdit->value();

  // Perform segmentation.
  std::vector<sv4guiContour*> contours;
  int numFailures = 0;
  for (int k = 0; k < path_points.size(); k += m_interval) {
    try { 
      auto contour = doSegmentation(path_points[k], k, k+1);
      contours.push_back(contour);
    } catch (std::exception &e) {
      numFailures += 1;
    }
  }

  // Display a message if all segmentations were not computed.
  if (numFailures != 0) { 
    QString msg;
    if (contours.size() == 0) {
      msg = "No segmentations could be computed for the image.\n\n";
    } else {
      int n = numFailures + contours.size();
      msg = QString::number(numFailures) + " segmentations out of " + QString::number(n) + " could not be computed for the image.\n\n";
    }
    msg += "The image window may not contain enough data (pixel values) to distinguish a vessel boundary.";
    QMessageBox::warning(NULL, "2D Segmentation", msg);
  }

  return contours;
}

/**
 * Function to generate a machine learning contour, apply fourier smoothing
 * insert the contour into the correct contour group and
 * update the rendering manager.
 *
 * @param path_point an instance of a SimVascular PathPoint object
 */
sv4guiContour *
sv4guiSeg2DEdit::doSegmentation(sv4guiPathElement::sv4guiPathPoint path_point, int index, int n_)
{
  sv4guiContour* contour = doMLContour(path_point);

  if (contour == nullptr) {
      throw std::runtime_error("No segmentation could be computed for the image.");
  }

  contour = contour->CreateSmoothedContour(m_numFourierModes);

  return contour;
}

/**
 * Function to use the machine learning module to segment a path point.
 *
 * This function passes the designated path point to the ML module
 * and receives a vector of double vectors in return.
 * The double vectors are then converted into a sv4guiContour
 *
 * @param path_point an instance of a SimVascular PathPoint object
 * @return contour, the corresponding segmented contour (sv4guiContour instance)
 */
sv4guiContour* sv4guiSeg2DEdit::doMLContour(sv4guiPathElement::sv4guiPathPoint path_point){
  std::vector<std::vector<double>> points = ml_utils->segmentPathPoint(path_point);

  if (points.size() <= 0){
    return NULL;
  }

  sv4guiContour* contour = new sv4guiContour();

  //create contour and add points
  contour->SetPathPoint(path_point);
  contour->SetPlaced(true);
  contour->SetMethod("ML");

  std::vector<mitk::Point3D> contourPoints;
  mitk::Point3D pt;

  for (int i = 0; i < points.size(); i++){
    pt[0] = points[i][0];
    pt[1] = points[i][1];
    pt[2] = points[i][2];

    contourPoints.push_back(pt);
  }

  contour->SetClosed(true);

  contour->SetContourPoints(contourPoints);

  return contour;
}

// void sv4guiSeg2DEdit::sampleNetwork(){
//   ml_utils->sampleNetwork();
// }
