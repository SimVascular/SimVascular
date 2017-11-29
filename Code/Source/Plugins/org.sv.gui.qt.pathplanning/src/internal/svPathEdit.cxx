#include "svPathEdit.h"
#include "ui_svPathEdit.h"
#include "svPathCreate.h"
#include "svPathSmooth.h"
#include "svPath.h"
#include "svPathOperation.h"
#include "svMath3.h"

// mitk
#include <QmitkStdMultiWidgetEditor.h>
#include <mitkDataStorage.h>
#include "mitkDataNode.h"
#include "mitkProperties.h"
#include <mitkUndoController.h>
#include <mitkNodePredicateDataType.h>
#include <mitkIDataStorageService.h>
#include <mitkDataStorageEditorInput.h>

#include <usModuleRegistry.h>

// Qt
#include <QMessageBox>
#include <QInputDialog>
#include <QWheelEvent>

#include <iostream>
using namespace std;

const QString svPathEdit::EXTENSION_ID = "org.sv.views.pathplanning";

svPathEdit::svPathEdit():
    ui(new Ui::svPathEdit),
    m_PathChangeObserverTag(-1),
    m_PathNode(NULL),
    m_Path(NULL),
    m_PathFolderNode(NULL),
    m_DisplayWidget(NULL),
    m_SmoothWidget(NULL),
    m_PathCreateWidget(NULL),
    m_PathCreateWidget2(NULL),
    m_ImageNode(NULL),
    m_UpdatingGUI(false)
{
}

svPathEdit::~svPathEdit()
{
    delete ui;

    if(m_SmoothWidget) delete m_SmoothWidget;

    if(m_PathCreateWidget) delete m_PathCreateWidget;

    if(m_PathCreateWidget2) delete m_PathCreateWidget2;
}

void svPathEdit::CreateQtPartControl( QWidget *parent )
{
    m_Parent=parent;
    ui->setupUi(parent);
    //    parent->setMaximumWidth(500);

    ui->comboBoxAddingMode->setItemText(svPath::SMART,"Smart");
    ui->comboBoxAddingMode->setItemText(svPath::BEGINNING,"Beginning");
    ui->comboBoxAddingMode->setItemText(svPath::END,"End");
    ui->comboBoxAddingMode->setItemText(svPath::BEFORE,"Before");
    ui->comboBoxAddingMode->setItemText(svPath::AFTER,"After");

    m_DisplayWidget=GetActiveStdMultiWidget();

    if(m_DisplayWidget)
    {
        //instead set zero in svappication
//        m_DisplayWidget->GetWidgetPlane1()->SetIntProperty("Crosshair.Gap Size", 0);
//        m_DisplayWidget->GetWidgetPlane2()->SetIntProperty("Crosshair.Gap Size", 0);
//        m_DisplayWidget->GetWidgetPlane3()->SetIntProperty("Crosshair.Gap Size", 0);
    }
    else
    {
        parent->setEnabled(false);
        MITK_ERROR << "Plugin PathEdit Init Error: No QmitkStdMultiWidget!";
        return;
    }

    ui->resliceSlider->SetDisplayWidget(m_DisplayWidget);
//    ui->resliceSlider->setCheckBoxVisible(true);
    ui->resliceSlider->SetResliceMode(mitk::ExtractSliceFilter::RESLICE_CUBIC);

    connect(ui->btnNewPath, SIGNAL(clicked()), this, SLOT(NewPath()) );
    connect(ui->btnChange, SIGNAL(clicked()), this, SLOT(ChangePath()) );
    connect(ui->buttonAdd, SIGNAL(clicked()), this, SLOT(SmartAdd()) );
    connect(ui->buttonAddManually, SIGNAL(clicked()), this, SLOT(ManuallyAdd()) );
    connect(ui->buttonDelete, SIGNAL(clicked()), this, SLOT(DeleteSelected()) );
    connect(ui->listWidget,SIGNAL(itemSelectionChanged()), this, SLOT(SelectPoint()) );
    connect(ui->listWidget,SIGNAL(clicked(const QModelIndex &)), this, SLOT(SelectPoint(const QModelIndex &)) );

    connect(ui->btnSmooth,SIGNAL(clicked()), this, SLOT(SmoothCurrentPath()) );

    ui->buttonAdd->setShortcut(QKeySequence("Ctrl+A"));
    ui->buttonDelete->setShortcut(QKeySequence("Ctrl+D"));

    connect(ui->resliceSlider,SIGNAL(resliceSizeChanged(double)), this, SLOT(UpdatePathResliceSize(double)) );

    connect(ui->comboBoxAddingMode, SIGNAL(currentIndexChanged(int)), this, SLOT(UpdateAddingMode(int )));

    ui->listWidget->installEventFilter(this);
}

//void svPathEdit::Activated()
//{
//}

//void svPathEdit::Deactivated()
//{
//}

void svPathEdit::Visible()
{
    OnSelectionChanged(GetDataManagerSelection());
}

void svPathEdit::Hidden()
{
    ui->resliceSlider->turnOnReslice(false);
    ClearAll();
}

//bool svPathEdit::IsExclusiveFunctionality() const
//{
//    return true;
//}

int svPathEdit::GetTimeStep()
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

void svPathEdit::OnSelectionChanged(std::vector<mitk::DataNode*> nodes )
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

    mitk::DataNode::Pointer pathNode=nodes.front();

    if(m_PathNode==pathNode)
    {
        return;
    }

    ClearAll();

    m_PathNode=pathNode;
    m_Path=dynamic_cast<svPath*>(pathNode->GetData());

    if(!m_Path)
    {
        mitk::NodePredicateDataType::Pointer isContourGroup = mitk::NodePredicateDataType::New("svContourGroup");
        if(!isContourGroup->CheckNode(pathNode))
            ui->resliceSlider->turnOnReslice(false);

        ClearAll();
        m_Parent->setEnabled(false);
        return;
    }

    m_ImageNode=NULL;
    m_Image=NULL;
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (m_PathNode,isProjFolder,false);
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
                m_ImageNode=rs->GetElement(0);
                if(m_ImageNode.IsNotNull())
                    m_Image=dynamic_cast<mitk::Image*>(m_ImageNode->GetData());
            }

        }
    }

    rs=GetDataStorage()->GetSources(m_PathNode);
    m_PathFolderNode=NULL;
    if(rs->size()>0)
        m_PathFolderNode=rs->GetElement(0);

    m_Parent->setEnabled(true);

    ui->labelPathName->setText(QString::fromStdString(m_PathNode->GetName()));

    int timeStep=GetTimeStep();
    svPathElement* pathElement=m_Path->GetPathElement(timeStep);
    if(pathElement==NULL) return;

    ui->labelPathPointNumber->setText(QString::number(pathElement->GetPathPointNumber()));

    UpdateGUI();

    m_DataInteractor = svPathDataInteractor::New();
    m_DataInteractor->LoadStateMachine("svPath.xml", us::ModuleRegistry::GetModule("svPath"));
    m_DataInteractor->SetEventConfig("svPathConfig.xml", us::ModuleRegistry::GetModule("svPath"));
    m_DataInteractor->SetDataNode(m_PathNode);

    //Add Observer
    itk::SimpleMemberCommand<svPathEdit>::Pointer pathChangeCommand = itk::SimpleMemberCommand<svPathEdit>::New();
    pathChangeCommand->SetCallbackFunction(this, &svPathEdit::UpdateGUI);
    m_PathChangeObserverTag = m_Path->AddObserver( svPathEvent(), pathChangeCommand);

    itk::SimpleMemberCommand<svPathEdit>::Pointer pointMoveCommand = itk::SimpleMemberCommand<svPathEdit>::New();
    pointMoveCommand->SetCallbackFunction(this, &svPathEdit::UpdateSlice);
    m_PointMoveObserverTag = m_Path->AddObserver( svPathFinishMovePointEvent(), pointMoveCommand);

    mitk::BaseData* baseData=NULL;
    if(m_ImageNode.IsNotNull())
        baseData=m_ImageNode->GetData();
    else if(m_PathNode.IsNotNull())
        baseData=m_PathNode->GetData();

    if ( baseData && baseData->GetTimeGeometry()->IsValid() )
    {
        mitk::RenderingManager::GetInstance()->InitializeViews(
                    baseData->GetTimeGeometry(), mitk::RenderingManager::REQUEST_UPDATE_ALL, true );
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }

    SetupResliceSlider();

    ui->comboBoxAddingMode->setCurrentIndex(m_Path->GetAddingMode());
}

void svPathEdit::NodeChanged(const mitk::DataNode* node)
{
}

void svPathEdit::NodeAdded(const mitk::DataNode* node)
{
}


void svPathEdit::NodeRemoved(const mitk::DataNode* node)
{
    OnSelectionChanged(GetDataManagerSelection());
}

void svPathEdit::ClearAll()
{
    //Remove Observer
    if(m_Path && m_PathChangeObserverTag!=-1)
    {
        m_Path->RemoveObserver(m_PathChangeObserverTag);
        m_PathChangeObserverTag=-1;
    }

    if(m_PathNode.IsNotNull())
    {
        m_PathNode->SetDataInteractor(NULL);
        m_DataInteractor=NULL;
    }

    m_Path=NULL;
    m_PathNode=NULL;

    ui->labelPathName->setText("");
    ui->labelPathPointNumber->setText("");
    ui->listWidget->clear();
}

void svPathEdit::UpdateGUI()
{
    if(m_Path==NULL) return;

    int timeStep=GetTimeStep();

    svPathElement* pathElement=m_Path->GetPathElement(timeStep);
    if(pathElement==NULL) return;

    m_UpdatingGUI=true;

    //Update Path Point Number
    ui->labelPathPointNumber->setText(QString::number(pathElement->GetPathPointNumber()));

    //Update Control Point List
    ui->listWidget->clear();
    double pt[3];
    char x[12];
    char y[12];
    char z[12];
    char xyz[50];
    for(int index=0;index<pathElement->GetControlPointNumber();index++){

        mitk::Point3D pt= pathElement->GetControlPoint(index);
        sprintf(x,"%.4f",pt[0]);
        sprintf(y,"%.4f",pt[1]);
        sprintf(z,"%.4f",pt[2]);
        sprintf(xyz,"%d: %8s, %8s, %8s",index,x,y,z);
        ui->listWidget->addItem(xyz);
    }

    int selectedIndex=pathElement->GetControlPointSelectedIndex();
    if(selectedIndex!=-2)
    {
        QModelIndex mIndex=ui->listWidget->model()->index(selectedIndex,0);
        ui->listWidget->selectionModel()->select(mIndex, QItemSelectionModel::ClearAndSelect);
    }

    //Update Reslice
    switch(m_Path->GetOperationType())
    {
    case svPathOperation::OpINSERTCONTROLPOINT:
//    case svPathOperation::OpMOVECONTROLPOINT:
        SetupResliceSlider();
        if(ui->resliceSlider->isResliceOn())
            ui->resliceSlider->moveToPathPosPoint(m_Path->GetNewControlPoint());
        break;
    case svPathOperation::OpREMOVECONTROLPOINT:
    {
        mitk::Point3D lastPoint=ui->resliceSlider->getCurrentPathPoint().pos;
        SetupResliceSlider();
        if(ui->resliceSlider->isResliceOn())
            ui->resliceSlider->moveToClosestPathPosPoint(lastPoint);
    }
        break;
    case svPathOperation::OpSETPATHELEMENT:
        SetupResliceSlider();
        break;
    }

    m_UpdatingGUI=false;

}

void svPathEdit::UpdateSlice()
{
    if(m_Path->GetOperationType()==svPathOperation::OpMOVECONTROLPOINT)
    {
        SetupResliceSlider();
        if(ui->resliceSlider->isResliceOn())
            ui->resliceSlider->moveToPathPosPoint(m_Path->GetNewControlPoint());
    }
}

void svPathEdit::SetupResliceSlider()
{
//    mitk::DataNode::Pointer imageNode=NULL;
//    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
//    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (m_PathNode,isProjFolder,false);
//    if(rs->size()>0)
//    {
//        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

//        rs=GetDataStorage()->GetDerivations (projFolderNode,mitk::NodePredicateDataType::New("svImageFolder"));
//        if(rs->size()>0)
//        {

//            mitk::DataNode::Pointer imageFolderNode=rs->GetElement(0);
//            rs=GetDataStorage()->GetDerivations(imageFolderNode);
//            if(rs->size()<1) return;
//            imageNode=rs->GetElement(0);
//            m_Image=dynamic_cast<mitk::Image*>(imageNode->GetData());

//        }
//    }

//    if(imageNode.IsNull()) return;

//    if(m_ImageNode.IsNull())
//        return;

    if(m_Path==NULL)
        return;

    int timeStep=GetTimeStep();
    svPathElement* pathElement=m_Path->GetPathElement(timeStep);
    if(pathElement==NULL) return;

    if(pathElement->GetControlPointNumber()>1)
    {
        int startingIndex=0;
        if(m_ImageNode.IsNotNull())
        {
            double realBounds[6];
            GetImageRealBounds(realBounds);
            ui->resliceSlider->setPathPoints(pathElement->GetExtendedPathPoints(realBounds,GetVolumeImageSpacing(),startingIndex));
        }
        else
            ui->resliceSlider->setPathPoints(pathElement->GetPathPoints());

        ui->resliceSlider->SetStartingSlicePos(startingIndex);

        if(m_ImageNode.IsNotNull())
            ui->resliceSlider->setDataNode(m_ImageNode);
        else if(m_PathNode.IsNotNull())
            ui->resliceSlider->setDataNode(m_PathNode);
        else
            ui->resliceSlider->setDataNode(NULL);

        double resliceSize=m_Path->GetResliceSize();
        if(resliceSize==0)
        {
            resliceSize=5.0;
            m_Path->SetResliceSize(resliceSize);
        }
        ui->resliceSlider->setResliceSize(resliceSize);
        ui->resliceSlider->setEnabled(true);
        if(ui->resliceSlider->isResliceOn())
            ui->resliceSlider->updateReslice();
    }
    else
    {
        ui->resliceSlider->turnOnReslice(false);
        ui->resliceSlider->setEnabled(false);
    }
}

void svPathEdit::GetImageRealBounds(double realBounds[6])
{
    for(int i=0;i<6;i++)
        realBounds[i]=0.0;

    if(m_Image==NULL)
        return;

    mitk::Point3D ori=m_Image->GetGeometry()->GetOrigin();
    mitk::Vector3D spacing=m_Image->GetGeometry()->GetSpacing();
    mitk::BaseGeometry::BoundsArrayType bounds=m_Image->GetGeometry()->GetBounds();
    realBounds[0]=ori[0]+bounds[0]*spacing[0];
    realBounds[2]=ori[1]+bounds[2]*spacing[1];
    realBounds[4]=ori[2]+bounds[4]*spacing[2];

    realBounds[1]=ori[0]+bounds[1]*spacing[0];
    realBounds[3]=ori[1]+bounds[3]*spacing[1];
    realBounds[5]=ori[2]+bounds[5]*spacing[2];
}

double svPathEdit::GetVolumeImageSpacing()
{
    if(m_Image)
    {
        mitk::Vector3D spacing=m_Image->GetGeometry()->GetSpacing();
        double minSpacing=std::min(spacing[0],std::min(spacing[1],spacing[2]));
        return minSpacing;
    }
    else
        return 0.1;
}

void svPathEdit::ChangePath(){

    if(m_Path==NULL) return;

    int timeStep=GetTimeStep();

    svPathElement* pathElement=m_Path->GetPathElement(timeStep);
    if(pathElement==NULL) return;

    if(m_PathCreateWidget==NULL)
    {
        m_PathCreateWidget=new svPathCreate(this->GetDataStorage(), this->GetDataManagerSelection().front(), timeStep);
    }

    m_PathCreateWidget->SetCreatePath(false);
    m_PathCreateWidget->SetPathName(ui->labelPathName->text());
    m_PathCreateWidget->SetSubdivisionType(pathElement->GetMethod());

    if(pathElement->GetMethod()==svPathElement::CONSTANT_SPACING)
        m_PathCreateWidget->SetNumber(QString::number(pathElement->GetSpacing()));
    else
        m_PathCreateWidget->SetNumber(QString::number(pathElement->GetCalculationNumber()));

    m_PathCreateWidget->show();
    m_PathCreateWidget->SetFocus();
}

void svPathEdit::AddPoint(mitk::Point3D point)
{
    if(m_Path==NULL){
        QMessageBox::information(NULL,"No Path Selected","Please select a path in data manager!");
        return;
    }

    int timeStep=GetTimeStep();

    svPathElement* pathElement=m_Path->GetPathElement(timeStep);
    if(pathElement==NULL) return;

    //Check if the point already exists
    if(pathElement->SearchControlPoint(point,0)!=-2)
    {
        return;
    }

    int index=-2;

    int selectedModeIndex=ui->comboBoxAddingMode->currentIndex();

    switch(selectedModeIndex)
    {
    case svPath::SMART:
      {
        index=pathElement->GetInsertintIndexByDistance(point);
        break;
      }
    case svPath::BEGINNING:
      {
        index=0;
        break;
      }
    case svPath::END:
      {
        index=-1;
        break;
      }
    case svPath::BEFORE:
        {
          index= ui->listWidget->selectionModel()->selectedRows().front().row();
          if (index < 0)
          {
            QMessageBox::information(NULL,"No Point Selected","For 'Before' or 'After' mode, please select a point in the control point list!");
            return;
          }
          break;
        }
    case svPath::AFTER:
        {
          index= ui->listWidget->selectionModel()->selectedRows().front().row()+1;
          if (index < 0)
          {
            QMessageBox::information(NULL,"No Point Selected","For 'Before' or 'After' mode, please select a point in the control point list!");
            return;
          }
          break;
        }
    default:
        break;
    }

    if(index!=-2 ){

        //        UnselectAll(timeStep, timeInMs,true);

        mitk::OperationEvent::IncCurrObjectEventId();

        svPathOperation* doOp = new svPathOperation(svPathOperation::OpINSERTCONTROLPOINT,timeStep, point, index);
        svPathOperation *undoOp = new svPathOperation(svPathOperation::OpREMOVECONTROLPOINT,timeStep, point, index);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Path, doOp, undoOp, "Insert Control Point");
        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

        m_Path->ExecuteOperation(doOp);

        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }

}

void svPathEdit::SmartAdd()
{
    mitk::Point3D point=m_DisplayWidget->GetCrossPosition();

    AddPoint(point);
}

void svPathEdit::ManuallyAdd()
{
    bool ok;
    QString text = QInputDialog::getText(m_Parent, tr("Point Coordinates"),
                                         tr("x,y,z or x y z:"), QLineEdit::Normal,
                                         "", &ok);
    if (!ok || text.trimmed().isEmpty())
        return;

    QStringList list = text.trimmed().split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
    if(list.size()!=3)
    {
        QMessageBox::warning(m_Parent,"Coordinates Missing","Please provide valid coordinates for the point!");
        return;
    }

    mitk::Point3D point;

    for(int i=0;i<list.size();i++)
    {
        ok=false;
        point[i]=list[i].toDouble(&ok);
        if(!ok)
        {
            QMessageBox::warning(m_Parent,"Coordinates Invalid","Please provide valid coordinates for the point!");
            return;
        }
    }

    AddPoint(point);
}

void svPathEdit::DeleteSelected(){
    if(m_Path==NULL){
        QMessageBox::information(NULL,"No Path Selected","Please select a path in data manager!");
        return;
    }

    int timeStep=GetTimeStep();

    svPathElement* pathElement=m_Path->GetPathElement(timeStep);
    if(pathElement==NULL) return;

    if(pathElement->GetControlPointNumber()<1){
        return;
    }

    QModelIndexList list=ui->listWidget->selectionModel()->selectedRows();
    if(list.size()<1)
        return;

    int index= ui->listWidget->selectionModel()->selectedRows().front().row();

    mitk::OperationEvent::IncCurrObjectEventId();

    if(index>-1 && index<pathElement->GetControlPointNumber()){
        mitk::Point3D point=pathElement->GetControlPoint(index);
        svPathOperation* doOp = new svPathOperation(svPathOperation::OpREMOVECONTROLPOINT,timeStep, point, index);

        svPathOperation* undoOp = new svPathOperation(svPathOperation::OpINSERTCONTROLPOINT,timeStep, point, index);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Path, doOp, undoOp, "Remove Control Point");
        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

        m_Path->ExecuteOperation(doOp);
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }
}

void svPathEdit::SelectPoint(int index)
{
    if(m_UpdatingGUI)
        return;

    if(m_Path && index>-1)
    {
        int timeStep=GetTimeStep();
        svPathElement* pathElement=m_Path->GetPathElement(timeStep);
        if(pathElement==NULL) return;

        pathElement->DeselectControlPoint();
        pathElement->SetControlPointSelected(index,true);
        m_Path->Modified();//make sure rendering update

        if(ui->resliceSlider->isResliceOn())
        {
            ui->resliceSlider->moveToPathPosPoint(pathElement->GetControlPoint(index));
        }
        else
        {
            m_DisplayWidget->MoveCrossToPosition(pathElement->GetControlPoint(index));
        }

        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }
}

void svPathEdit::SelectPoint()
{
    int index=-1;
    QModelIndexList selectedRows=ui->listWidget->selectionModel()->selectedRows();
    if(selectedRows.size()>0)
    {
        index=selectedRows.front().row();
        SelectPoint(index);
    }

}

void svPathEdit::SelectPoint(const QModelIndex & idx)
{
    int index=idx.row();
    SelectPoint(index);
}

void svPathEdit::SmoothCurrentPath()
{
    if(m_SmoothWidget==NULL)
    {
        m_SmoothWidget=new svPathSmooth();
    }
    m_SmoothWidget->SetDataStorage(GetDataStorage());
    m_SmoothWidget->SetSelectedNode(GetDataManagerSelection().front());
    m_SmoothWidget->SetTimeStep(GetTimeStep());

    m_SmoothWidget->show();
    m_SmoothWidget->SetFocus();
}

void svPathEdit::UpdatePathResliceSize(double newSize)
{
    if(m_Path)
    {
        m_Path->SetResliceSize(newSize);
        m_Path->SetDataModified();
    }

    if(m_PathFolderNode.IsNotNull())
        m_PathFolderNode->SetFloatProperty("reslice size",newSize);
}

void svPathEdit::UpdateAddingMode(int mode)
{
    m_Path->SetAddingMode((svPath::AddingMode)mode);
}

void svPathEdit::NewPath()
{
    if(m_PathNode.IsNull())
        return;

    if(m_PathCreateWidget2)
    {
        delete m_PathCreateWidget2;
    }

    m_PathCreateWidget2=new svPathCreate(GetDataStorage(), m_PathNode, 0);

    if(m_Path)
    {
        m_PathCreateWidget2->SetSubdivisionType(m_Path->GetMethod());
        if(m_Path->GetMethod()==svPathElement::CONSTANT_SPACING)
            m_PathCreateWidget2->SetNumber(QString::number(m_Path->GetSpacing()));
        else
            m_PathCreateWidget2->SetNumber(QString::number(m_Path->GetCalculationNumber()));
    }

    m_PathCreateWidget2->show();
    m_PathCreateWidget2->SetFocus();
}

bool svPathEdit::eventFilter(QObject *obj, QEvent *event)
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
        return svPathEdit::eventFilter(obj, event);
    }
}
