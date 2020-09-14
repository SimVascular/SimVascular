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

#include "sv4gui_PathEdit.h"
#include "ui_sv4gui_PathEdit.h"
#include "sv4gui_PathCreate.h"
#include "sv4gui_PathSmooth.h"
#include "sv4gui_Path.h"
#include "sv4gui_PathOperation.h"
#include "sv4gui_Math3.h"

// mitk
//#include <QmitkStdMultiWidgetEditor.h>
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

const QString sv4guiPathEdit::EXTENSION_ID = "org.sv.views.pathplanning";

sv4guiPathEdit::sv4guiPathEdit():
    ui(new Ui::sv4guiPathEdit),
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

sv4guiPathEdit::~sv4guiPathEdit()
{
    delete ui;

    if(m_SmoothWidget) delete m_SmoothWidget;

    if(m_PathCreateWidget) delete m_PathCreateWidget;

    if(m_PathCreateWidget2) delete m_PathCreateWidget2;
}

//---------------------
// CreateQtPartControl
//---------------------
// Initialize GUI widgets.
//
void sv4guiPathEdit::CreateQtPartControl(QWidget* parent)
{
    m_Parent=parent;
    ui->setupUi(parent);

    // Setup path point Adding Mode combination box.
    ui->comboBoxAddingMode->setItemText(sv4guiPath::SMART,"Smart");
    ui->comboBoxAddingMode->setItemText(sv4guiPath::BEGINNING,"Beginning");
    ui->comboBoxAddingMode->setItemText(sv4guiPath::END,"End");
    ui->comboBoxAddingMode->setItemText(sv4guiPath::BEFORE,"Before");
    ui->comboBoxAddingMode->setItemText(sv4guiPath::AFTER,"After");
    connect(ui->comboBoxAddingMode, SIGNAL(currentIndexChanged(int)), this, SLOT(UpdateAddingMode(int )));

    // Get access to the four-window widget in the centre of the application.
    m_DisplayWidget = GetActiveStdMultiWidget();

    if(m_DisplayWidget) {
        //instead set zero in svappication
//        m_DisplayWidget->GetWidgetPlane1()->SetIntProperty("Crosshair.Gap Size", 0);
//        m_DisplayWidget->GetWidgetPlane2()->SetIntProperty("Crosshair.Gap Size", 0);
//        m_DisplayWidget->GetWidgetPlane3()->SetIntProperty("Crosshair.Gap Size", 0);
    } else {
        parent->setEnabled(false);
        MITK_ERROR << "Plugin PathEdit Init Error: No QmitkStdMultiWidget!";
        return;
    }

    // The panel top right 'Add Path' and 'Change Type' buttons.
    connect(ui->btnNewPath, SIGNAL(clicked()), this, SLOT(NewPath()) );
    connect(ui->btnChange, SIGNAL(clicked()), this, SLOT(ChangePath()) );

    // Control point buttons. 
    connect(ui->buttonAdd, SIGNAL(clicked()), this, SLOT(SmartAdd()) );
    connect(ui->buttonAddManually, SIGNAL(clicked()), this, SLOT(ManuallyAdd()) );
    connect(ui->buttonDelete, SIGNAL(clicked()), this, SLOT(DeleteSelected()) );
    connect(ui->btnSmooth,SIGNAL(clicked()), this, SLOT(SmoothCurrentPath()) );

    // Add mouse key shortcuts for adding and deleting a path points.
    ui->buttonAdd->setShortcut(QKeySequence("Ctrl+A"));
    ui->buttonDelete->setShortcut(QKeySequence("Ctrl+D"));

    // Control point list operations.
    connect(ui->listWidget,SIGNAL(itemSelectionChanged()), this, SLOT(SelectPoint()) );
    connect(ui->listWidget,SIGNAL(clicked(const QModelIndex &)), this, SLOT(SelectPoint(const QModelIndex &)) );

    ui->resliceSlider->SetDisplayWidget(m_DisplayWidget);
    //ui->resliceSlider->setCheckBoxVisible(true);
    ui->resliceSlider->SetResliceMode(mitk::ExtractSliceFilter::RESLICE_CUBIC);
    connect(ui->resliceSlider,SIGNAL(resliceSizeChanged(double)), this, SLOT(UpdatePathResliceSize(double)) );

    ui->listWidget->installEventFilter(this);
}

//void sv4guiPathEdit::Activated()
//{
//}

//void sv4guiPathEdit::Deactivated()
//{
//}

void sv4guiPathEdit::Visible()
{
    OnSelectionChanged(GetDataManagerSelection());
}

void sv4guiPathEdit::Hidden()
{
    ui->resliceSlider->turnOnReslice(false);
    ClearAll();
}

//bool sv4guiPathEdit::IsExclusiveFunctionality() const
//{
//    return true;
//}

int sv4guiPathEdit::GetTimeStep()
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

void sv4guiPathEdit::OnSelectionChanged(std::vector<mitk::DataNode*> nodes )
{
    auto msgPrefix = "[sv4guiPathEdit_OnSelectionChanged] ";
    MITK_INFO << msgPrefix;

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
    m_Path=dynamic_cast<sv4guiPath*>(pathNode->GetData());

    if(!m_Path)
    {
        mitk::NodePredicateDataType::Pointer isContourGroup = mitk::NodePredicateDataType::New("sv4guiContourGroup");
        if(!isContourGroup->CheckNode(pathNode))
            ui->resliceSlider->turnOnReslice(false);

        ClearAll();
        m_Parent->setEnabled(false);
        return;
    }

    m_ImageNode=NULL;
    m_Image=NULL;
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (m_PathNode,isProjFolder,false);
    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        rs=GetDataStorage()->GetDerivations (projFolderNode,mitk::NodePredicateDataType::New("sv4guiImageFolder"));
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

    sv4guiPathElement* pathElement=m_Path->GetPathElement(timeStep);
    if(pathElement==NULL) return;

    ui->labelPathPointNumber->setText(QString::number(pathElement->GetPathPointNumber()));

    UpdateGUI();

    m_DataInteractor = sv4guiPathDataInteractor::New();
    m_DataInteractor->LoadStateMachine("sv4gui_Path.xml", us::ModuleRegistry::GetModule("sv4guiModulePath"));
    m_DataInteractor->SetEventConfig("sv4gui_PathConfig.xml", us::ModuleRegistry::GetModule("sv4guiModulePath"));
    m_DataInteractor->SetDataNode(m_PathNode);

    //Add Observer
    itk::SimpleMemberCommand<sv4guiPathEdit>::Pointer pathChangeCommand = itk::SimpleMemberCommand<sv4guiPathEdit>::New();
    pathChangeCommand->SetCallbackFunction(this, &sv4guiPathEdit::UpdateGUI);
    m_PathChangeObserverTag = m_Path->AddObserver( sv4guiPathEvent(), pathChangeCommand);

    itk::SimpleMemberCommand<sv4guiPathEdit>::Pointer pointMoveCommand = itk::SimpleMemberCommand<sv4guiPathEdit>::New();
    pointMoveCommand->SetCallbackFunction(this, &sv4guiPathEdit::UpdateSlice);
    m_PointMoveObserverTag = m_Path->AddObserver( sv4guiPathFinishMovePointEvent(), pointMoveCommand);

    /*
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
    */

    SetupResliceSlider();

    ui->comboBoxAddingMode->setCurrentIndex(m_Path->GetAddingMode());
}

void sv4guiPathEdit::NodeChanged(const mitk::DataNode* node)
{
}

void sv4guiPathEdit::NodeAdded(const mitk::DataNode* node)
{
}


void sv4guiPathEdit::NodeRemoved(const mitk::DataNode* node)
{
    OnSelectionChanged(GetDataManagerSelection());
}

void sv4guiPathEdit::ClearAll()
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

void sv4guiPathEdit::UpdateGUI()
{
    if(m_Path==NULL) return;

    int timeStep=GetTimeStep();
    sv4guiPathElement* pathElement=m_Path->GetPathElement(timeStep);
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
    case sv4guiPathOperation::OpINSERTCONTROLPOINT:
//    case sv4guiPathOperation::OpMOVECONTROLPOINT:
        SetupResliceSlider();
        if(ui->resliceSlider->isResliceOn())
            ui->resliceSlider->moveToPathPosPoint(m_Path->GetNewControlPoint());
        break;
    case sv4guiPathOperation::OpREMOVECONTROLPOINT:
    {
        mitk::Point3D lastPoint=ui->resliceSlider->getCurrentPathPoint().pos;
        SetupResliceSlider();
        if(ui->resliceSlider->isResliceOn())
            ui->resliceSlider->moveToClosestPathPosPoint(lastPoint);
    }
        break;
    case sv4guiPathOperation::OpSETPATHELEMENT:
        SetupResliceSlider();
        break;
    }

    m_UpdatingGUI=false;

}

void sv4guiPathEdit::UpdateSlice()
{
    if(m_Path->GetOperationType()==sv4guiPathOperation::OpMOVECONTROLPOINT)
    {
        SetupResliceSlider();
        if(ui->resliceSlider->isResliceOn())
            ui->resliceSlider->moveToPathPosPoint(m_Path->GetNewControlPoint());
    }
}

void sv4guiPathEdit::SetupResliceSlider()
{
//    mitk::DataNode::Pointer imageNode=NULL;
//    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
//    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (m_PathNode,isProjFolder,false);
//    if(rs->size()>0)
//    {
//        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

//        rs=GetDataStorage()->GetDerivations (projFolderNode,mitk::NodePredicateDataType::New("sv4guiImageFolder"));
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
    sv4guiPathElement* pathElement=m_Path->GetPathElement(timeStep);
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

void sv4guiPathEdit::GetImageRealBounds(double realBounds[6])
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

double sv4guiPathEdit::GetVolumeImageSpacing()
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

void sv4guiPathEdit::ChangePath(){

    if(m_Path==NULL) return;

    int timeStep=GetTimeStep();

    sv4guiPathElement* pathElement=m_Path->GetPathElement(timeStep);
    if(pathElement==NULL) return;

    if(m_PathCreateWidget==NULL)
    {
        m_PathCreateWidget=new sv4guiPathCreate(this->GetDataStorage(), this->GetDataManagerSelection().front(), timeStep);
    }

    m_PathCreateWidget->SetCreatePath(false);
    m_PathCreateWidget->SetPathName(ui->labelPathName->text());
    m_PathCreateWidget->SetSubdivisionType(pathElement->GetMethod());

    if(pathElement->GetMethod()==sv3::PathElement::CONSTANT_SPACING)
        m_PathCreateWidget->SetNumber(QString::number(pathElement->GetSpacing()));
    else
        m_PathCreateWidget->SetNumber(QString::number(pathElement->GetCalculationNumber()));

    m_PathCreateWidget->show();
    m_PathCreateWidget->SetFocus();
}

//----------
// AddPoint
//----------
//
void sv4guiPathEdit::AddPoint(mitk::Point3D point)
{
    std::cout << "===================== sv4guiPathEdit::AddPoint =====================" << std::endl;
    std::cout << "[AddPoint] Point: " << point[0] << "  " << point[1] << "  " << point[2] << std::endl;

    if (m_Path==NULL) {
        QMessageBox::information(NULL,"No Path Selected","Please select a path in data manager!");
        return;
    }

    int timeStep=GetTimeStep();

    sv4guiPathElement* pathElement=m_Path->GetPathElement(timeStep);
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
    case sv4guiPath::SMART:
      {
        index=pathElement->GetInsertintIndexByDistance(point);
        break;
      }
    case sv4guiPath::BEGINNING:
      {
        index=0;
        break;
      }
    case sv4guiPath::END:
      {
        index=-1;
        break;
      }
    case sv4guiPath::BEFORE:
        {
          auto rows = ui->listWidget->selectionModel()->selectedRows();
          if (rows.size() == 0) {
            return; 
          }
          index = rows.front().row();
          if (index < 0)
          {
            QMessageBox::information(NULL,"No Point Selected","For 'Before' or 'After' mode, please select a point in the control point list!");
            return;
          }
          break;
        }
    case sv4guiPath::AFTER:
        {
          auto rows = ui->listWidget->selectionModel()->selectedRows();
          if (rows.size() == 0) {
            return; 
          }
          index= rows.front().row()+1;
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

    MITK_INFO << ">>> index: " << index; 

    if(index!=-2 ){

        //        UnselectAll(timeStep, timeInMs,true);

        mitk::OperationEvent::IncCurrObjectEventId();

        sv4guiPathOperation* doOp = new sv4guiPathOperation(sv4guiPathOperation::OpINSERTCONTROLPOINT,timeStep, point, index);
        sv4guiPathOperation *undoOp = new sv4guiPathOperation(sv4guiPathOperation::OpREMOVECONTROLPOINT,timeStep, point, index);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Path, doOp, undoOp, "Insert Control Point");
        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

        m_Path->ExecuteOperation(doOp);

        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }

}

void sv4guiPathEdit::SmartAdd()
{
    std::cout << "===================== sv4guiPathEdit::SmartAdd =====================" << std::endl;
    mitk::Point3D point = m_DisplayWidget->GetCrossPosition();

    AddPoint(point);
}

void sv4guiPathEdit::ManuallyAdd()
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

void sv4guiPathEdit::DeleteSelected(){
    if(m_Path==NULL){
        QMessageBox::information(NULL,"No Path Selected","Please select a path in data manager!");
        return;
    }

    int timeStep=GetTimeStep();

    sv4guiPathElement* pathElement=m_Path->GetPathElement(timeStep);
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
        sv4guiPathOperation* doOp = new sv4guiPathOperation(sv4guiPathOperation::OpREMOVECONTROLPOINT,timeStep, point, index);

        sv4guiPathOperation* undoOp = new sv4guiPathOperation(sv4guiPathOperation::OpINSERTCONTROLPOINT,timeStep, point, index);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Path, doOp, undoOp, "Remove Control Point");
        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

        m_Path->ExecuteOperation(doOp);
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }
}

void sv4guiPathEdit::SelectPoint(int index)
{
    if(m_UpdatingGUI)
        return;

    if(m_Path && index>-1)
    {
        int timeStep=GetTimeStep();
        sv4guiPathElement* pathElement=m_Path->GetPathElement(timeStep);
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

void sv4guiPathEdit::SelectPoint()
{
    int index=-1;
    QModelIndexList selectedRows=ui->listWidget->selectionModel()->selectedRows();
    if(selectedRows.size()>0)
    {
        index=selectedRows.front().row();
        SelectPoint(index);
    }

}

void sv4guiPathEdit::SelectPoint(const QModelIndex & idx)
{
    int index=idx.row();
    SelectPoint(index);
}

void sv4guiPathEdit::SmoothCurrentPath()
{
    if(m_SmoothWidget==NULL)
    {
        m_SmoothWidget=new sv4guiPathSmooth();
    }
    m_SmoothWidget->SetDataStorage(GetDataStorage());
    m_SmoothWidget->SetSelectedNode(GetDataManagerSelection().front());
    m_SmoothWidget->SetTimeStep(GetTimeStep());

    m_SmoothWidget->show();
    m_SmoothWidget->SetFocus();
}

void sv4guiPathEdit::UpdatePathResliceSize(double newSize)
{
    if(m_Path)
    {
        m_Path->SetResliceSize(newSize);
        m_Path->SetDataModified();
    }

    if(m_PathFolderNode.IsNotNull())
        m_PathFolderNode->SetFloatProperty("reslice size",newSize);
}

void sv4guiPathEdit::UpdateAddingMode(int mode)
{
    m_Path->SetAddingMode((sv4guiPath::AddingMode)mode);
}

void sv4guiPathEdit::NewPath()
{
    if(m_PathNode.IsNull())
        return;

    if(m_PathCreateWidget2)
    {
        delete m_PathCreateWidget2;
    }

    m_PathCreateWidget2=new sv4guiPathCreate(GetDataStorage(), m_PathNode, 0);

    if(m_Path)
    {
        m_PathCreateWidget2->SetSubdivisionType(m_Path->GetMethod());
        if(m_Path->GetMethod()==sv3::PathElement::CONSTANT_SPACING)
            m_PathCreateWidget2->SetNumber(QString::number(m_Path->GetSpacing()));
        else
            m_PathCreateWidget2->SetNumber(QString::number(m_Path->GetCalculationNumber()));
    }

    m_PathCreateWidget2->show();
    m_PathCreateWidget2->SetFocus();
}

bool sv4guiPathEdit::eventFilter(QObject *obj, QEvent *event)
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
        return sv4guiPathEdit::eventFilter(obj, event);
    }
}
