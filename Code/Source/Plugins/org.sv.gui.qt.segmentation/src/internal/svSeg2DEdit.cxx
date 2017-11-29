#include "svSeg2DEdit.h"
#include "ui_svSeg2DEdit.h"
#include "ui_svLevelSet2DWidget.h"
#include "ui_svLoftParamWidget.h"

#include "svPath.h"
#include "svSegmentationUtils.h"
#include "svMath3.h"

#include "svContourGroup.h"
#include "svContourGroupDataInteractor.h"
#include "svContourCircle.h"
#include "svContourEllipse.h"
#include "svContourPolygon.h"
#include "svContourSplinePolygon.h"
#include "svContourOperation.h"
#include "svContourModel.h"
#include "svContourModelThresholdInteractor.h"

#include "svModelUtils.h"

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

#include <iostream>
using namespace std;

#include <math.h>

const QString svSeg2DEdit::EXTENSION_ID = "org.sv.views.segmentation2d";

svSeg2DEdit::svSeg2DEdit() :
    ui(new Ui::svSeg2DEdit)
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

svSeg2DEdit::~svSeg2DEdit()
{
    delete ui;

    if(m_LoftWidget) delete m_LoftWidget;

//    if(m_LSParamWidget) delete m_LSParamWidget;

    if(m_ContourGroupCreateWidget)
        delete m_ContourGroupCreateWidget;
}

void svSeg2DEdit::CreateQtPartControl( QWidget *parent )
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
    m_LSParamWidget=new svLevelSet2DWidget(parent);
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

    m_LoftWidget=new svLoftParamWidget();
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
}

void svSeg2DEdit::Visible()
{
//    ui->resliceSlider->turnOnReslice(true);
    OnSelectionChanged(GetDataManagerSelection());
}

void svSeg2DEdit::Hidden()
{
    ui->resliceSlider->turnOnReslice(false);
    ClearAll();
}

//bool svSeg2DEdit::IsExclusiveFunctionality() const
//{
//    return true;
//}

int svSeg2DEdit::GetTimeStep()
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

void svSeg2DEdit::OnSelectionChanged(std::vector<mitk::DataNode*> nodes )
{
//    if(!IsActivated())
    if(!IsVisible())
    {
        return;
    }

    if(nodes.size()==0)
    {
        ui->resliceSlider->turnOnReslice(false);
        ClearAll();
        m_Parent->setEnabled(false);
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
    m_ContourGroup=dynamic_cast<svContourGroup*>(groupNode->GetData());
    if(!m_ContourGroup)
    {
        ui->resliceSlider->turnOnReslice(false);
        ClearAll();
        m_Parent->setEnabled(false);
        return;
    }

    m_Parent->setEnabled(true);

//    std::string groupPathName=m_ContourGroup->GetPathName();
    int  groupPathID=m_ContourGroup->GetPathID();

//    mitk::DataNode::Pointer pathNode=NULL;
    mitk::DataNode::Pointer imageNode=NULL;
    m_Image=NULL;
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (m_ContourGroupNode,isProjFolder,false);

    m_PathNode=NULL;
    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        rs=GetDataStorage()->GetDerivations (projFolderNode,mitk::NodePredicateDataType::New("svImageFolder"));
        if(rs->size()>0)
        {

            mitk::DataNode::Pointer imageFolderNode=rs->GetElement(0);
            rs=GetDataStorage()->GetDerivations(imageFolderNode);
//            if(rs->size()<1) return;
            if(rs->size()>0)
            {
                imageNode=rs->GetElement(0);
                if(imageNode.IsNotNull())
                    m_Image= dynamic_cast<mitk::Image*>(imageNode->GetData());
            }

        }

        rs=GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svPathFolder"));
        if (rs->size()>0)
        {
            mitk::DataNode::Pointer pathFolderNode=rs->GetElement(0);
            rs=GetDataStorage()->GetDerivations(pathFolderNode);

            for(int i=0;i<rs->size();i++)
            {
                svPath* path=dynamic_cast<svPath*>(rs->GetElement(i)->GetData());

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
        m_cvImage=svSegmentationUtils::image2cvStrPts(m_Image);
    else
        m_cvImage=NULL;

    int timeStep=GetTimeStep();
    svPathElement* pathElement=m_Path->GetPathElement(timeStep);
    if(pathElement==NULL)
        return;

    std::vector<svPathElement::svPathPoint> pathPoints=pathElement->GetPathPoints();
    std::vector<mitk::Point3D> pathPosPoints=pathElement->GetPathPosPoints();

    for(int i=0;i<m_ContourGroup->GetSize(timeStep);i++)
    {
        svContour* contour=m_ContourGroup->GetContour(i,timeStep);
        if(contour==NULL) continue;

        int insertingIndex=svMath3::GetInsertintIndexByDistance(pathPosPoints, contour->GetPathPosPoint(), false);
        if(insertingIndex!=-2)
        {
            svPathElement::svPathPoint pp1=pathElement->GetPathPoint(insertingIndex);
            svPathElement::svPathPoint pp2=contour->GetPathPoint();

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
        svContour* contour=m_ContourGroup->GetContour(i,timeStep);
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

    m_DataInteractor = svContourGroupDataInteractor::New();
    m_DataInteractor->SetInteraction3D(false);
    m_DataInteractor->LoadStateMachine("svContourGroupInteraction.xml", us::ModuleRegistry::GetModule("svSegmentation"));
    m_DataInteractor->SetEventConfig("svSegmentationConfig.xml", us::ModuleRegistry::GetModule("svSegmentation"));
    m_DataInteractor->SetDataNode(m_ContourGroupNode);

    //Add Observer
    itk::SimpleMemberCommand<svSeg2DEdit>::Pointer groupChangeCommand = itk::SimpleMemberCommand<svSeg2DEdit>::New();
    groupChangeCommand->SetCallbackFunction(this, &svSeg2DEdit::UpdateContourList);
    m_ContourGroupChangeObserverTag = m_ContourGroup->AddObserver( svContourGroupEvent(), groupChangeCommand);

    itk::SimpleMemberCommand<svSeg2DEdit>::Pointer loftCommand = itk::SimpleMemberCommand<svSeg2DEdit>::New();
    loftCommand->SetCallbackFunction(this, &svSeg2DEdit::LoftContourGroup);
    m_StartLoftContourGroupObserverTag = m_ContourGroup->AddObserver( svContourGroupChangeEvent(), loftCommand);
    m_StartLoftContourGroupObserverTag2 = m_ContourGroup->AddObserver( svContourChangeEvent(), loftCommand);

    itk::SimpleMemberCommand<svSeg2DEdit>::Pointer flagOnCommand = itk::SimpleMemberCommand<svSeg2DEdit>::New();
    flagOnCommand->SetCallbackFunction(this, &svSeg2DEdit::ContourChangingOn);
    m_StartChangingContourObserverTag = m_ContourGroup->AddObserver( StartChangingContourEvent(), flagOnCommand);

    itk::SimpleMemberCommand<svSeg2DEdit>::Pointer flagOffCommand = itk::SimpleMemberCommand<svSeg2DEdit>::New();
    flagOffCommand->SetCallbackFunction(this, &svSeg2DEdit::ContourChangingOff);
    m_EndChangingContourObserverTag = m_ContourGroup->AddObserver( EndChangingContourEvent(), flagOffCommand);

    itk::SimpleMemberCommand<svSeg2DEdit>::Pointer selectCommand = itk::SimpleMemberCommand<svSeg2DEdit>::New();
    selectCommand->SetCallbackFunction(this, &svSeg2DEdit::SelectContour3D);
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

double svSeg2DEdit::GetVolumeImageSpacing()
{
    double minSpacing=0.1;
    if(m_Image)
    {
        mitk::Vector3D spacing=m_Image->GetGeometry()->GetSpacing();
        minSpacing=std::min(spacing[0],std::min(spacing[1],spacing[2]));
    }
    return minSpacing;
}

void svSeg2DEdit::InsertContour(svContour* contour, int contourIndex)
{
    if(m_ContourGroup&&contour&&contourIndex>-2){

        m_ContourGroup->DeselectContours();

        contour->SetSelected(true);

        int timeStep=GetTimeStep();
        svContourOperation* doOp = new svContourOperation(svContourOperation::OpINSERTCONTOUR,timeStep,contour,contourIndex);

        svContourOperation *undoOp = new svContourOperation(svContourOperation::OpREMOVECONTOUR,timeStep, contour, contourIndex);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_ContourGroup, doOp, undoOp, "Insert Contour");

        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

        m_ContourGroup->ExecuteOperation(doOp);

        mitk::RenderingManager::GetInstance()->RequestUpdateAll();

    }
}

void svSeg2DEdit::InsertContourByPathPosPoint(svContour* contour)
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

void svSeg2DEdit::SetContour(int contourIndex, svContour* newContour)
{
    if(m_ContourGroup&&contourIndex>-2)
    {
        svContour* originalContour=m_ContourGroup->GetContour(contourIndex);

        newContour->SetSelected(true);

        int timeStep=GetTimeStep();
        svContourOperation* doOp = new svContourOperation(svContourOperation::OpSETCONTOUR,timeStep,newContour,contourIndex);

        svContourOperation *undoOp = new svContourOperation(svContourOperation::OpSETCONTOUR,timeStep, originalContour, contourIndex);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_ContourGroup, doOp, undoOp, "Set Contour");

        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

        m_ContourGroup->ExecuteOperation(doOp);

        mitk::RenderingManager::GetInstance()->RequestUpdateAll();

    }
}

void svSeg2DEdit::RemoveContour(int contourIndex)
{
    if(m_ContourGroup&&contourIndex>-2)
    {
        svContour* contour=m_ContourGroup->GetContour(contourIndex);

        int timeStep=GetTimeStep();
        svContourOperation* doOp = new svContourOperation(svContourOperation::OpREMOVECONTOUR,timeStep,contour,contourIndex);

        svContourOperation *undoOp = new svContourOperation(svContourOperation::OpINSERTCONTOUR,timeStep, contour, contourIndex);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_ContourGroup, doOp, undoOp, "Remove Contour");

        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

        m_ContourGroup->ExecuteOperation(doOp);

        mitk::RenderingManager::GetInstance()->RequestUpdateAll();

    }
}

std::vector<int> svSeg2DEdit::GetBatchList()
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

svContour* svSeg2DEdit::PostprocessContour(svContour* contour)
{
    svContour* contourNew=contour;

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
        contourNew=svContourSplinePolygon::CreateByFitting(contourNew,splineControlNumber);
        contourNew->SetSubdivisionType(svContour::CONSTANT_SPACING);
        contourNew->SetSubdivisionSpacing(GetVolumeImageSpacing());
    }

    return contourNew;
}

void svSeg2DEdit::CreateContours(SegmentationMethod method)
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

        svContour* contour=NULL;

        switch(method)
        {
            case LEVELSET_METHOD:
            {
                svSegmentationUtils::svLSParam tmpLSParam = m_LSParamWidget->GetLSParam();
                contour=svSegmentationUtils::CreateLSContour(ui->resliceSlider->getPathPoint(posID),m_cvImage->GetVtkStructuredPoints(),&tmpLSParam,ui->resliceSlider->getResliceSize());
                break;
            }
            case THRESHOLD_METHOD:
                contour=svSegmentationUtils::CreateThresholdContour(ui->resliceSlider->getPathPoint(posID),m_cvImage->GetVtkStructuredPoints(),ui->sliderThreshold->value(), ui->resliceSlider->getResliceSize());
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

            if(posList.size()==1)
                QMessageBox::warning(NULL,"No Valid Contour Created","Contour not created and added since it's invalid");
        }

        mitk::ProgressBar::GetInstance()->Progress(1);
    }

    //LoftContourGroup();
}

void svSeg2DEdit::SetSecondaryWidgetsVisible(bool visible)
{
    ui->smoothWidget->setVisible(visible);
    ui->splineWidget->setVisible(visible);
    ui->batchWidget->setVisible(visible);
}

void svSeg2DEdit::ResetGUI()
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

void svSeg2DEdit::CreateLSContour()
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

void svSeg2DEdit::CreateThresholdContour()
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

void svSeg2DEdit::UpdatePreview()
{
    if(m_PreviewContourModel.IsNull())
         return;

    if(m_PreviewDataNodeInteractor.IsNotNull())
        ui->sliderThreshold->setValue(m_PreviewDataNodeInteractor->GetCurrentValue());
}

void svSeg2DEdit::FinishPreview()
{
    if(m_PreviewContourModel.IsNull())
         return;

    int timeStep=GetTimeStep();

    svContour* contour=m_PreviewContourModel->GetContour(timeStep);
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

void svSeg2DEdit::CreateEllipse()
{
    if(m_CurrentSegButton!=ui->btnEllipse)
    {
        ResetGUI();

        SetSecondaryWidgetsVisible(false);
    }

    int index=m_ContourGroup->GetContourIndexByPathPosPoint(ui->resliceSlider->getCurrentPathPoint().pos);

    svContour* existingContour=m_ContourGroup->GetContour(index);

    svContour* contour=NULL;
    if(existingContour && existingContour->GetContourPointNumber()>2)
    {
        contour=svContourEllipse::CreateByFitting(existingContour);
        if(contour)
        {
            contour->SetSubdivisionType(svContour::CONSTANT_SPACING);
            contour->SetSubdivisionSpacing(GetVolumeImageSpacing());
            mitk::OperationEvent::IncCurrObjectEventId();

            InsertContourByPathPosPoint(contour);
        }
    }

    m_CurrentSegButton=ui->btnEllipse;
    m_CurrentSegButton->setStyleSheet("background-color: lightskyblue");

    m_DataInteractor->SetMethod("Ellipse");
}

void svSeg2DEdit::CreateCircle()
{
    if(m_CurrentSegButton!=ui->btnCircle)
    {
        ResetGUI();

        SetSecondaryWidgetsVisible(false);
    }

    int index=m_ContourGroup->GetContourIndexByPathPosPoint(ui->resliceSlider->getCurrentPathPoint().pos);

    svContour* existingContour=m_ContourGroup->GetContour(index);

    svContour* contour=NULL;
    if(existingContour && existingContour->GetContourPointNumber()>2)
    {
        contour=svContourCircle::CreateByFitting(existingContour);
        if(contour)
        {
            contour->SetSubdivisionType(svContour::CONSTANT_SPACING);
            contour->SetSubdivisionSpacing(GetVolumeImageSpacing());
            mitk::OperationEvent::IncCurrObjectEventId();

            InsertContourByPathPosPoint(contour);
        }
    }

    m_CurrentSegButton=ui->btnCircle;
    m_CurrentSegButton->setStyleSheet("background-color: lightskyblue");

    m_DataInteractor->SetMethod("Circle");
}

void svSeg2DEdit::CreateSplinePoly()
{
    if(m_CurrentSegButton!=ui->btnSplinePoly)
    {
        ResetGUI();

        SetSecondaryWidgetsVisible(false);
    }

    int index=m_ContourGroup->GetContourIndexByPathPosPoint(ui->resliceSlider->getCurrentPathPoint().pos);

    svContour* existingContour=m_ContourGroup->GetContour(index);

    svContour* contour=NULL;
    if(existingContour && existingContour->GetContourPointNumber()>2)
    {
        int splineControlNumber=ui->spinBoxControlNumber->value();
        if(splineControlNumber<3)
            splineControlNumber=3;

        contour=svContourSplinePolygon::CreateByFitting(existingContour,splineControlNumber);
        if(contour)
        {
            contour->SetSubdivisionType(svContour::CONSTANT_SPACING);
            contour->SetSubdivisionSpacing(GetVolumeImageSpacing());
            mitk::OperationEvent::IncCurrObjectEventId();

            InsertContourByPathPosPoint(contour);
        }
    }

    m_CurrentSegButton=ui->btnSplinePoly;
    m_CurrentSegButton->setStyleSheet("background-color: lightskyblue");

    m_DataInteractor->SetMethod("SplinePolygon");
}

void svSeg2DEdit::CreatePolygon()
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

void svSeg2DEdit::SmoothSelected()
{
//    int fourierNumber=12;
    int fourierNumber=ui->spinBoxSmoothNumber->value();

    QModelIndexList selectedRows=ui->listWidget->selectionModel()->selectedRows();

    if(selectedRows.isEmpty())
        return;

    int index= selectedRows.front().row();

    svContour* smoothedContour=m_ContourGroup->GetContour(index)->CreateSmoothedContour(fourierNumber);

    InsertContourByPathPosPoint(smoothedContour);

    LoftContourGroup();
}

void svSeg2DEdit::DeleteSelected()
{
    QModelIndexList selectedRows=ui->listWidget->selectionModel()->selectedRows();
    if(selectedRows.size()>0)
    {
        RemoveContour(selectedRows.front().row());
    }

    LoftContourGroup();
}

void svSeg2DEdit::SelectContour(int index)
{
    if(m_UpdatingGUI)
        return;

    mitk::StatusBar::GetInstance()->DisplayText("");

    if(m_ContourGroup && index>-1)
    {

        m_ContourGroup->DeselectContours();

        m_ContourGroup->SetContourSelected(index,true);
        svContour* contour=m_ContourGroup->GetContour(index);
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

void svSeg2DEdit::SelectContour()
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

void svSeg2DEdit::SelectContour(const QModelIndex & idx)
{
    int index=idx.row();
    SelectContour(index);
}

void svSeg2DEdit::NodeChanged(const mitk::DataNode* node)
{
}

void svSeg2DEdit::NodeAdded(const mitk::DataNode* node)
{
}

void svSeg2DEdit::NodeRemoved(const mitk::DataNode* node)
{
    OnSelectionChanged(GetDataManagerSelection());
}

void svSeg2DEdit::ClearAll()
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

void svSeg2DEdit::UpdateContourList()
{
    if(m_ContourGroup==NULL) return;

    int timeStep=GetTimeStep();

    m_UpdatingGUI=true;

    ui->listWidget->clear();

    for(int index=0;index<m_ContourGroup->GetSize(timeStep);index++){

        svContour* contour=m_ContourGroup->GetContour(index,timeStep);
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

void svSeg2DEdit::LoftContourGroup()
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
            m_LoftSurface->SetVtkPolyData(svModelUtils::CreateLoftSurface(m_ContourGroup,0,0,NULL,timeStep),timeStep);

    }
    else
    {
        m_ContourGroupNode->SetBoolProperty("lofting",false);

        if(m_LoftSurfaceNode.IsNotNull())
            m_LoftSurfaceNode->SetVisibility(false);
    }

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();

}

void svSeg2DEdit::ShowLoftWidget()
{
    svLoftingParam *param=m_ContourGroup->GetLoftingParam();

    m_LoftWidget->UpdateGUI(param);
    m_LoftWidget->show();
}

void svSeg2DEdit::UpdateContourGroupLoftingParam()
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

void svSeg2DEdit::OKLofting()
{
    UpdateContourGroupLoftingParam();
    LoftContourGroup();
    m_LoftWidget->hide();
}

void svSeg2DEdit::ApplyLofting()
{
    UpdateContourGroupLoftingParam();
    LoftContourGroup();
}

void svSeg2DEdit::HideLoftWidget()
{
    m_LoftWidget->hide();
}

void svSeg2DEdit::ContourChangingOn()
{
    m_ContourChanging=true;
}

void svSeg2DEdit::ContourChangingOff()
{
    m_ContourChanging=false;
//    if(m_CurrentSegButton)
//        m_CurrentSegButton->setStyleSheet("");
}

void svSeg2DEdit::UpdatePathResliceSize(double newSize)
{
    if(m_ContourGroup)
    {
        m_ContourGroup->SetResliceSize(newSize);
        m_ContourGroup->SetDataModified();
    }

    if(m_GroupFolderNode.IsNotNull())
        m_GroupFolderNode->SetFloatProperty("reslice size",newSize);
}

void svSeg2DEdit::ManualContextMenuRequested()
{
    m_ManualMenu->popup(QCursor::pos());
}

void svSeg2DEdit::ManualCircleContextMenuRequested(const QPoint &pos)
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

void svSeg2DEdit::ManualEllipseContextMenuRequested(const QPoint &pos)
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

void svSeg2DEdit::ManualSplinePolyContextMenuRequested(const QPoint &pos)
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

void svSeg2DEdit::ManualPolygonContextMenuRequested(const QPoint &pos)
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

void svSeg2DEdit::CreateManualCircle(bool)
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

    svContour* contour=new svContourCircle();
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

    contour->SetSubdivisionType(svContour::CONSTANT_SPACING);
    contour->SetSubdivisionSpacing(GetVolumeImageSpacing());

    mitk::OperationEvent::IncCurrObjectEventId();

    InsertContourByPathPosPoint(contour);
}

void svSeg2DEdit::CreateManualEllipse(bool)
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

    svContourEllipse* contour=new svContourEllipse();
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

    contour->SetSubdivisionType(svContour::CONSTANT_SPACING);
    contour->SetSubdivisionSpacing(GetVolumeImageSpacing());

    mitk::OperationEvent::IncCurrObjectEventId();

    InsertContourByPathPosPoint(contour);
}

void svSeg2DEdit::CreateManualPolygonType(bool spline)
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

    svContour* contour=NULL;
    if(spline)
        contour=new svContourSplinePolygon();
    else
        contour=new svContourPolygon();

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

    contour->SetSubdivisionType(svContour::CONSTANT_SPACING);
    contour->SetSubdivisionSpacing(GetVolumeImageSpacing());

    mitk::OperationEvent::IncCurrObjectEventId();

    InsertContourByPathPosPoint(contour);
}

void svSeg2DEdit::CreateManualSplinePoly(bool)
{
    CreateManualPolygonType(true);
}

void svSeg2DEdit::CreateManualPolygon(bool)
{
    CreateManualPolygonType(false);
}

void svSeg2DEdit::CopyContour()
{
    QModelIndexList selectedRows=ui->listWidget->selectionModel()->selectedRows();
    if(selectedRows.size()>0 && m_ContourGroup)
    {
        m_CopyContour=m_ContourGroup->GetContour(selectedRows.front().row(),GetTimeStep());
    }
}

void svSeg2DEdit::PasteContour()
{
    if(m_CopyContour==NULL)
        return;

    svContour* contour=m_CopyContour->Clone();
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

void svSeg2DEdit::NewGroup()
{
    if(m_ContourGroupNode.IsNull())
        return;

    if(m_ContourGroupCreateWidget)
    {
        delete m_ContourGroupCreateWidget;
    }

    m_ContourGroupCreateWidget=new svContourGroupCreate(GetDataStorage(), m_ContourGroupNode,0);
    m_ContourGroupCreateWidget->show();
    m_ContourGroupCreateWidget->SetFocus();
}

void svSeg2DEdit::ShowPath(bool checked)
{
    if(m_PathNode.IsNotNull())
    {
        m_PathNode->SetVisibility(checked);
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }
}

void svSeg2DEdit::PreparePreviewInteraction(QString method)
{
    m_PreviewContourModel = svContourModel::New();

    m_PreviewDataNode = mitk::DataNode::New();
    m_PreviewDataNode->SetData(m_PreviewContourModel);
    m_PreviewDataNode->SetName("PreviewContour");
    m_PreviewDataNode->SetBoolProperty("helper object", true);

    GetDataStorage()->Add(m_PreviewDataNode, m_ContourGroupNode);

    m_PreviewDataNodeInteractor= svContourModelThresholdInteractor::New();
    m_PreviewDataNodeInteractor->LoadStateMachine("svContourModelThresholdInteraction.xml", us::ModuleRegistry::GetModule("svSegmentation"));
    m_PreviewDataNodeInteractor->SetEventConfig("svSegmentationConfig.xml", us::ModuleRegistry::GetModule("svSegmentation"));
    m_PreviewDataNodeInteractor->SetDataNode(m_PreviewDataNode);

    cvStrPts*  strPts=svSegmentationUtils::GetSlicevtkImage(ui->resliceSlider->getCurrentPathPoint(), m_cvImage->GetVtkStructuredPoints(), ui->resliceSlider->getResliceSize());
    m_PreviewDataNodeInteractor->SetVtkImageData(m_cvImage->GetVtkStructuredPoints());
    m_PreviewDataNodeInteractor->SetPathPoint(ui->resliceSlider->getCurrentPathPoint());
    m_PreviewDataNodeInteractor->SetGroupInteractor(m_DataInteractor);

    itk::SimpleMemberCommand<svSeg2DEdit>::Pointer previewFinished = itk::SimpleMemberCommand<svSeg2DEdit>::New();
    previewFinished->SetCallbackFunction(this, &svSeg2DEdit::FinishPreview);
    m_PreviewContourModelObserverFinishTag = m_PreviewContourModel->AddObserver( EndInteractionContourModelEvent(), previewFinished);

    itk::SimpleMemberCommand<svSeg2DEdit>::Pointer previewUpdating = itk::SimpleMemberCommand<svSeg2DEdit>::New();
    previewUpdating->SetCallbackFunction(this, &svSeg2DEdit::UpdatePreview);
    m_PreviewContourModelObserverUpdateTag = m_PreviewContourModel->AddObserver( UpdateInteractionContourModelEvent(), previewUpdating);
}

void svSeg2DEdit::UpdatePathPoint(int pos)
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

void svSeg2DEdit::QuitPreviewInteraction()
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

bool svSeg2DEdit::eventFilter(QObject *obj, QEvent *event)
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
        return svSeg2DEdit::eventFilter(obj, event);
    }
}


void svSeg2DEdit::SelectContour3D()
{
    if(m_DataInteractor.IsNotNull())
    {
        int index=m_DataInteractor->GetSelectedContourIndex();
        if(index>-1)
            ui->listWidget->setCurrentRow(index);
    }
}
