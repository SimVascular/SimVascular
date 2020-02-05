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

#include "sv4gui_ModelEdit.h"
#include "ui_sv4gui_ModelEdit.h"

#include "sv4gui_FaceListDelegate.h"
#include "sv4gui_Path.h"
#include "sv4gui_MitkSeg3D.h"
#include "sv4gui_SegmentationUtils.h"
#include "sv4gui_ModelElementFactory.h"
#include "sv4gui_ModelElementAnalytic.h"
#include "sv4gui_ModelExtractPathsAction.h"
#include "sv4gui_QmitkDataManagerView.h"

#include "sv_polydatasolid_utils.h"

#include <QmitkStdMultiWidgetEditor.h>
#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>
#include <mitkSliceNavigationController.h>
#include <mitkProgressBar.h>
#include <mitkStatusBar.h>
#include <mitkUnstructuredGrid.h>

#include <usModuleRegistry.h>

#include <vtkProperty.h>

#include <QStandardItemModel>
#include <QInputDialog>
#include <QColorDialog>
#include <QSignalMapper>
#include <QMessageBox>
#include <QScrollArea>
#include <QVBoxLayout>
#include <QItemSelection>

#include <iostream>
#include <time.h> 

using namespace std;

const QString sv4guiModelEdit::EXTENSION_ID = "org.sv.views.modeling";

sv4guiModelEdit::sv4guiModelEdit() :
    ui(new Ui::sv4guiModelEdit)
{
    m_Model=NULL;
    m_ModelNode=NULL;

    m_SegSelectionWidget=NULL;
    m_CapSelectionWidget=NULL;

    m_DataInteractor=NULL;
    m_ModelSelectFaceObserverTag=-1;
    m_ModelUpdateObserverTag=-1;

    m_BlendTableMenu=NULL;
    m_BlendTableModel=NULL;

    m_SphereWidget=NULL;
    m_PlaneWidget=NULL;
    m_BoxWidget=NULL;

    m_PathFolderNode=NULL;

    m_OperatingWholeTableModel=false;

    m_LocalOperationforBlendRegion=false;
}

sv4guiModelEdit::~sv4guiModelEdit()
{
    delete ui;

    if(m_SegSelectionWidget) delete m_SegSelectionWidget;
    if(m_CapSelectionWidget) delete m_CapSelectionWidget;
}

void sv4guiModelEdit::CreateQtPartControl( QWidget *parent )
{
    m_Parent=parent;
    ui->setupUi(parent);

    QSignalMapper* signalMapper = new QSignalMapper(this);

    //    parent->setMaximumWidth(450);

    m_DisplayWidget=GetActiveStdMultiWidget();

    if(m_DisplayWidget==NULL)
    {
        parent->setEnabled(false);
        MITK_ERROR << "Plugin ModelEdit Init Error: No QmitkStdMultiWidget!";
        return;
    }

    //for top part
    //=================================================================
    connect(ui->btnUpdateModel, SIGNAL(clicked()), this, SLOT(ShowSegSelectionWidget()) );

    m_SegSelectionWidget=new sv4guiSegSelectionWidget();
    m_SegSelectionWidget->move(400,400);
    m_SegSelectionWidget->hide();
    m_SegSelectionWidget->setWindowFlags(Qt::WindowStaysOnTopHint);

    connect(m_SegSelectionWidget,SIGNAL(accepted()), this, SLOT(CreateModel()));

    connect(ui->btnChangeFacet, SIGNAL(clicked()), this, SLOT(ChangeFacetSize()) );
    connect(ui->btnConvert, SIGNAL(clicked()), this, SLOT(ConvertToPolyDataModel()) );

    //for tab Face List
    //=================================================================
    sv4guiFaceListDelegate* itemDelegate=new sv4guiFaceListDelegate(this);
    m_FaceListTableModel = new QStandardItemModel(this);
    ui->tableViewFaceList->setModel(m_FaceListTableModel);
    ui->tableViewFaceList->setItemDelegateForColumn(2,itemDelegate);
    ui->tableViewFaceList->setItemDelegateForColumn(5,itemDelegate);

    connect( m_FaceListTableModel, SIGNAL(itemChanged(QStandardItem*))
             , this, SLOT(UpdateFaceData(QStandardItem*)) );

    connect( ui->tableViewFaceList->selectionModel()
             , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
             , this
             , SLOT( TableFaceListSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );

    connect( ui->tableViewFaceList
             , SIGNAL( doubleClicked( const QModelIndex & ) )
             , this
             , SLOT( ToggleVisibility ( const QModelIndex & ) ) );

    connect( ui->tableViewFaceList
             , SIGNAL( doubleClicked( const QModelIndex & ) )
             , this
             , SLOT( ChangeColor ( const QModelIndex & ) ) );

    m_FaceListTableMenu=new QMenu(ui->tableViewFaceList);
    QAction* showAction=m_FaceListTableMenu->addAction("Show");
    QAction* hideAction=m_FaceListTableMenu->addAction("Hide");
    QAction* changeTypeAction=m_FaceListTableMenu->addAction("Change Type");
    QAction* changeColorAction=m_FaceListTableMenu->addAction("Change Color");
    QAction* changeOpacityAction=m_FaceListTableMenu->addAction("Change Opacity");

    connect( showAction, SIGNAL( triggered(bool) ) , this, SLOT( ShowSelected(bool) ) );
    connect( hideAction, SIGNAL( triggered(bool) ) , this, SLOT( HideSelected(bool) ) );
    connect( changeTypeAction, SIGNAL( triggered(bool) ) , this, SLOT( ChangeTypeSelected(bool) ) );
    connect( changeColorAction, SIGNAL( triggered(bool) ) , this, SLOT( ChangeColorSelected(bool) ) );
    connect( changeOpacityAction, SIGNAL( triggered(bool) ) , this, SLOT( ChangeOpacitySelected(bool) ) );

    connect( ui->tableViewFaceList, SIGNAL(customContextMenuRequested(const QPoint&))
             , this, SLOT(TableViewFaceListContextMenuRequested(const QPoint&)) );

    //various ops
    //-----------------------------------------------------------------
    ui->toolBoxPolyData->setCurrentIndex(0);

    signalMapper->setMapping(ui->btnDeleteFaces, DELETE_FACES);
    connect(ui->btnDeleteFaces, SIGNAL(clicked()),signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnFillHoleIDs, FILL_HOLES_WITH_IDS);
    connect(ui->btnFillHoleIDs, SIGNAL(clicked()),signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnCombineFaces, COMBINE_FACES);
    connect(ui->btnCombineFaces, SIGNAL(clicked()),signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnRemeshFaces, REMESH_FACES);
    connect(ui->btnEstimateEdgeSize0, SIGNAL(clicked()), this, SLOT(SetEstimatedEdgeSize()) );
    connect(ui->btnRemeshFaces, SIGNAL(clicked()),signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnExtractFaces, EXTRACT_FACES);
    connect(ui->btnExtractFaces, SIGNAL(clicked()),signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnFillHoles, FILL_HOLES);
    connect(ui->btnFillHoles, SIGNAL(clicked()),signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnSelectLargestConnected, SELECT_LARGEST_CONNECTED);
    connect(ui->btnSelectLargestConnected, SIGNAL(clicked()),signalMapper, SLOT(map()));

#ifdef SV_USE_MMG
    signalMapper->setMapping(ui->btnRemeshG, REMESH_GLOBAL);
    connect(ui->btnEstimateEdgeSize1, SIGNAL(clicked()), this, SLOT(SetEstimatedEdgeSize()) );
    connect(ui->btnRemeshG, SIGNAL(clicked()),signalMapper, SLOT(map()));
#endif

    signalMapper->setMapping(ui->btnDecimateG, DECIMATE_GLOBAL);
    connect(ui->btnDecimateG, SIGNAL(clicked()),signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnLapSmoothG, LAPLACIAN_SMOOTH_GLOBAL);
    connect(ui->btnLapSmoothG, SIGNAL(clicked()),signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnBFSubdivideG, BUTTERFLY_SUBDIVIDE_GLOBAL);
    connect(ui->btnBFSubdivideG, SIGNAL(clicked()),signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnWSSmoothG, WINDOWSINC_SMOOTH_GLOBAL);
    connect(ui->btnWSSmoothG, SIGNAL(clicked()),signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnDensifyG, DENSIFY_GLOBAL);
    connect(ui->btnDensifyG, SIGNAL(clicked()),signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnDecimateL, DECIMATE_LOCAL);
    connect(ui->btnDecimateL, SIGNAL(clicked()),signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnLapSmoothL, LAPLACIAN_SMOOTH_LOCAL);
    connect(ui->btnLapSmoothL, SIGNAL(clicked()),signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnCstrSmoothL, CONSTRAIN_SMOOTH_LOCAL);
    connect(ui->btnCstrSmoothL, SIGNAL(clicked()),signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnLSubdivideL, LINEAR_SUBDIVIDE_LOCAL);
    connect(ui->btnLSubdivideL, SIGNAL(clicked()),signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnLoopSubdivideL, LOOP_SUBDIVIDE_LOCAL);
    connect(ui->btnLoopSubdivideL, SIGNAL(clicked()),signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnCutAbove, CUT_ABOVE);
    connect(ui->btnCutAbove, SIGNAL(clicked()),signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnCutBelow, CUT_BELOW);
    connect(ui->btnCutBelow, SIGNAL(clicked()),signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnCutBox, CUT_BOX);
    connect(ui->btnCutBox, SIGNAL(clicked()),signalMapper, SLOT(map()));

    connect(signalMapper, SIGNAL(mapped(int)), this, SLOT(ModelOperate(int)));

    connect(ui->checkBoxSphere, SIGNAL(toggled(bool)), this, SLOT(ShowSphereInteractor(bool)));

    //for trim
    connect(ui->checkBoxShowPlane, SIGNAL(toggled(bool)), this, SLOT(ShowPlaneInteractor(bool)));
    connect(ui->comboBoxPathPlane, SIGNAL(currentIndexChanged(int)), this, SLOT(SetupSliderPathPlane(int )));
    connect(ui->sliderPathPlane, SIGNAL(valueChanged(double)), this, SLOT(UpdatePlaneWidget(double )));

    connect(ui->checkBoxShowBox, SIGNAL(toggled(bool)), this, SLOT(ShowBoxInteractor(bool)));
    connect(ui->comboBoxPathBox, SIGNAL(currentIndexChanged(int)), this, SLOT(SetupSliderPathBox(int )));
    connect(ui->sliderPathBox, SIGNAL(valueChanged(double)), this, SLOT(UpdateBoxWidget(double )));

    //for tab Blend
    //=====================================================================
    m_BlendTableModel = new QStandardItemModel(this);
    ui->tableViewBlend->setModel(m_BlendTableModel);
    m_BlendTableMenu=new QMenu(ui->tableViewBlend);

    QAction* setRadiusAction=m_BlendTableMenu->addAction("Set Radius");
    QAction* clearRadiusAction=m_BlendTableMenu->addAction("Clear Radius");
    QAction* useSelectedBlendAction=m_BlendTableMenu->addAction("Use Selected Faces");
    QAction* notUseSelectedBlendAction=m_BlendTableMenu->addAction("Not Use Selected Faces");

    connect( setRadiusAction, SIGNAL( triggered(bool) ) , this, SLOT( SetRadius(bool) ) );
    connect( clearRadiusAction, SIGNAL( triggered(bool) ) , this, SLOT( ClearRadius(bool) ) );
    connect( useSelectedBlendAction, SIGNAL( triggered(bool) ) , this, SLOT( UseSelectedBlend(bool) ) );
    connect( notUseSelectedBlendAction, SIGNAL( triggered(bool) ) , this, SLOT( NotUseSelectedBlend(bool) ) );

    connect( ui->tableViewBlend, SIGNAL(customContextMenuRequested(const QPoint&))
             , this, SLOT(TableViewBlendContextMenuRequested(const QPoint&)) );

    connect( ui->tableViewBlend->selectionModel()
             , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
             , this
             , SLOT( TableBlendSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );


    connect(ui->btnBlend, SIGNAL(clicked()), this, SLOT(BlendModel()) );
    //    connect(ui->tabWidget,SIGNAL(currentChanged(int)), this, SLOT(UpdateBlendTable(int)) );
    //

    //for mmg remesh
    //=====================================================================
    int idx=0;
    QWidget* widgetGRemesh=ui->toolBoxGlobalOps->widget(idx);
    QString title=ui->toolBoxGlobalOps->itemText(idx);
    widgetGRemesh->hide();
    ui->toolBoxGlobalOps->removeItem(idx);
#ifdef SV_USE_MMG
    ui->toolBoxGlobalOps->insertItem(idx,widgetGRemesh,title);
    widgetGRemesh->show();
#endif

    //for extracting centerlines
    //=================================================================
    m_CapSelectionWidget=new sv4guiCapSelectionWidget();
    m_CapSelectionWidget->move(400,400);
    m_CapSelectionWidget->hide();
    m_CapSelectionWidget->setWindowFlags(Qt::WindowStaysOnTopHint);

    connect(ui->btnExtractCenterlines, SIGNAL(clicked()), this, SLOT(ShowCapSelectionWidget()) );

    connect(m_CapSelectionWidget,SIGNAL(accepted()), this, SLOT(ExtractCenterlines()));
}

void sv4guiModelEdit::Visible()
{
    ui->tabWidget->setCurrentIndex(0);
    OnSelectionChanged(GetDataManagerSelection());
}

void sv4guiModelEdit::Hidden()
{
    //    ClearAll();
    RemoveObservers();
}

//------------------
// SetTimeModified
//------------------
// Set the time that the model surface was updated.
//
//
void sv4guiModelEdit::SetTimeModified()
{
    if (m_ModelNode != nullptr) {
        time_t time = std::time(NULL); 
        m_ModelNode->SetStringProperty("time modified", ctime(&time)); 
    }
}

int sv4guiModelEdit::GetTimeStep()
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

void sv4guiModelEdit::OnSelectionChanged(std::vector<mitk::DataNode*> nodes )
{
    m_LocalOperationforBlendRegion=false;

    //    if(!IsActivated())
    if(!IsVisible())
    {
        return;
    }

    if(nodes.size()==0)
    {
        RemoveObservers();
        m_Parent->setEnabled(false);
        return;
    }

    mitk::DataNode::Pointer selectedNode=nodes.front();
    sv4guiModel* model=dynamic_cast<sv4guiModel*>(selectedNode->GetData());

    if(!model)
    {
        RemoveObservers();
        m_Parent->setEnabled(false);
        return;
    }

    if(m_ModelNode==selectedNode)
    {
        AddObservers();
        m_Parent->setEnabled(true);
        return;
    }

    if(m_ModelNode.IsNotNull())
        RemoveObservers();

    m_ModelNode=selectedNode;
    m_Model=model;
    m_ModelType=m_Model->GetType();

    m_Parent->setEnabled(true);
    AddObservers();

    ui->tabWidget->setCurrentIndex(0);

    UpdateGUI();

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void sv4guiModelEdit::UpdateGUI()
{
    //update top part
    //------------------------------------------------------------------------
    ui->labelModelName->setText(QString::fromStdString(m_ModelNode->GetName()));
    ui->labelModelType->setText(QString::fromStdString(m_ModelType));

    if(m_ModelType=="Parasolid" || m_ModelType=="OpenCASCADE")
        ui->widgetAnalytic->show();
    else
        ui->widgetAnalytic->hide();

    //update tab face list
    //--------------------------------------------------------------------
    if(!m_Model) return;

    SetupFaceListTable();

    UpdateFaceListSelection();

    if(m_ModelType=="PolyData")
        ui->toolBoxPolyData->show();
    else
        ui->toolBoxPolyData->hide();

    //if(m_ModelType=="OpenCASCADE")
    //    ui->widgetOCC->show();
    //else
    //    ui->widgetOCC->hide();

    //----------------------
    UpdatePathListForTrim();

    //update tab Blend
    //------------------------------------------------------
    if(m_ModelType=="Discrete")
        ui->tabWidget->setTabEnabled(1,false);
    else
        ui->tabWidget->setTabEnabled(1,true);

    if(m_ModelType=="PolyData")
    {
        ui->widgetBlendDecimation->show();
        ui->groupBoxBlendIters->show();
        UpdatePolyDataBlendParam();
    }else{
        ui->widgetBlendDecimation->hide();
        ui->groupBoxBlendIters->hide();
    }

        SetupBlendTable();
}

void sv4guiModelEdit::UpdatePathListForTrim()
{
    ui->comboBoxPathPlane->clear();
    ui->comboBoxPathPlane->setEnabled(false);
    ui->sliderPathPlane->setEnabled(false);

    ui->comboBoxPathBox->clear();
    ui->comboBoxPathBox->setEnabled(false);
    ui->sliderPathBox->setEnabled(false);

    if(m_ModelNode.IsNull())
        return;

    disconnect(ui->comboBoxPathPlane, SIGNAL(currentIndexChanged(int)), this, SLOT(SetupSliderPathPlane(int )));

    disconnect(ui->comboBoxPathBox, SIGNAL(currentIndexChanged(int)), this, SLOT(SetupSliderPathBox(int )));

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (m_ModelNode,isProjFolder,false);

    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        rs=GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiPathFolder"));
        if (rs->size()>0)
        {
            m_PathFolderNode=rs->GetElement(0);

            rs=GetDataStorage()->GetDerivations(rs->GetElement(0),mitk::NodePredicateDataType::New("sv4guiPath"));

            for(int i=0;i<rs->size();i++)
            {
                ui->comboBoxPathPlane->addItem(QString::fromStdString(rs->GetElement(i)->GetName()));
                ui->comboBoxPathBox->addItem(QString::fromStdString(rs->GetElement(i)->GetName()));
            }

            if(rs->size()>0)
            {
                ui->comboBoxPathPlane->setEnabled(true);
                ui->comboBoxPathPlane->setCurrentIndex(-1);

                ui->comboBoxPathBox->setEnabled(true);
                ui->comboBoxPathBox->setCurrentIndex(-1);
            }
        }

    }

    connect(ui->comboBoxPathPlane, SIGNAL(currentIndexChanged(int)), this, SLOT(SetupSliderPathPlane(int )));

    connect(ui->comboBoxPathBox, SIGNAL(currentIndexChanged(int)), this, SLOT(SetupSliderPathBox(int )));
}

void sv4guiModelEdit::SetupSliderPathPlane(int idx)
{
    if(m_PathFolderNode.IsNull())
        return;

    QString selectedPathName=ui->comboBoxPathPlane->currentText();

    mitk::DataNode::Pointer pathNode=GetDataStorage()->GetNamedDerivedNode (selectedPathName.toStdString().c_str(), m_PathFolderNode);
    if(pathNode.IsNull())
        return;

    sv4guiPath* path=dynamic_cast<sv4guiPath*>(pathNode->GetData());
    if(path==NULL)
        return;

    sv4guiPathElement* pe=path->GetPathElement(GetTimeStep());
    if(pe==NULL)
        return;

    if(pe->GetPathPointNumber()>0)
    {
        ui->sliderPathPlane->setMinimum(0);
        ui->sliderPathPlane->setMaximum(pe->GetPathPointNumber()-1);
        ui->sliderPathPlane->setEnabled(true);
    }else
    {
        ui->sliderPathPlane->setEnabled(false);
    }

}

void sv4guiModelEdit::UpdatePlaneWidget(double idx)
{
    if(m_PlaneWidget==NULL || !m_PlaneWidget->GetEnabled())
        return;

    if(m_PathFolderNode.IsNull())
        return;

    QString selectedPathName=ui->comboBoxPathPlane->currentText();

    mitk::DataNode::Pointer pathNode=GetDataStorage()->GetNamedDerivedNode (selectedPathName.toStdString().c_str(), m_PathFolderNode);
    if(pathNode.IsNull())
        return;

    sv4guiPath* path=dynamic_cast<sv4guiPath*>(pathNode->GetData());
    if(path==NULL)
        return;

    sv4guiPathElement* pe=path->GetPathElement(GetTimeStep());
    if(pe==NULL)
        return;

    int posIdx=idx;

    sv4guiPathElement::sv4guiPathPoint pathPoint=pe->GetPathPoint(posIdx);

    m_PlaneWidget->SetCenter(pathPoint.pos[0],pathPoint.pos[1],pathPoint.pos[2]);
    m_PlaneWidget->SetNormal(pathPoint.tangent[0],pathPoint.tangent[1],pathPoint.tangent[2]);
    m_PlaneWidget->UpdatePlacement();
    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void sv4guiModelEdit::SetupSliderPathBox(int idx)
{
    if(m_PathFolderNode.IsNull())
        return;

    QString selectedPathName=ui->comboBoxPathBox->currentText();

    mitk::DataNode::Pointer pathNode=GetDataStorage()->GetNamedDerivedNode (selectedPathName.toStdString().c_str(), m_PathFolderNode);
    if(pathNode.IsNull())
        return;

    sv4guiPath* path=dynamic_cast<sv4guiPath*>(pathNode->GetData());
    if(path==NULL)
        return;

    sv4guiPathElement* pe=path->GetPathElement(GetTimeStep());
    if(pe==NULL)
        return;

    if(pe->GetPathPointNumber()>0)
    {
        ui->sliderPathBox->setMinimum(0);
        ui->sliderPathBox->setMaximum(pe->GetPathPointNumber()-1);
        ui->sliderPathBox->setEnabled(true);
    }else
    {
        ui->sliderPathBox->setEnabled(false);
    }

}

void sv4guiModelEdit::UpdateBoxWidget(double idx)
{
    if(m_BoxWidget==NULL || !m_BoxWidget->GetEnabled())
        return;

    if(m_PathFolderNode.IsNull())
        return;

    QString selectedPathName=ui->comboBoxPathBox->currentText();

    mitk::DataNode::Pointer pathNode=GetDataStorage()->GetNamedDerivedNode (selectedPathName.toStdString().c_str(), m_PathFolderNode);
    if(pathNode.IsNull())
        return;

    sv4guiPath* path=dynamic_cast<sv4guiPath*>(pathNode->GetData());
    if(path==NULL)
        return;

    sv4guiPathElement* pe=path->GetPathElement(GetTimeStep());
    if(pe==NULL)
        return;

    int posIdx=idx;

    sv4guiPathElement::sv4guiPathPoint pathPoint=pe->GetPathPoint(posIdx);

    m_BoxWidget->PlaceWidget (-3, 3, -3, 3, -3, 3);
    m_BoxWidget->SetTransform(sv4guiSegmentationUtils::GetvtkTransformBox(pathPoint,6));

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void sv4guiModelEdit::UpdateFaceListSelection()
{
    if(!m_Model) return;
    sv4guiModelElement* modelElement=m_Model->GetModelElement();
    if(!modelElement) return;

    if(m_FaceListTableModel==NULL)
        return;

    disconnect( ui->tableViewFaceList->selectionModel()
                , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
                , this
                , SLOT( TableFaceListSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );


    ui->tableViewFaceList->clearSelection();

    int count=m_FaceListTableModel->rowCount();

    for(int i=0;i<count;i++)
    {
        QStandardItem* itemID= m_FaceListTableModel->item(i,0);
        int id=itemID->text().toInt();

        if(modelElement->IsFaceSelected(id))
        {
            QModelIndex mIndex=m_FaceListTableModel->index(i,1);
            ui->tableViewFaceList->selectionModel()->select(mIndex, QItemSelectionModel::Select|QItemSelectionModel::Rows);
        }
    }

    connect( ui->tableViewFaceList->selectionModel()
             , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
             , this
             , SLOT( TableFaceListSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );

}

void sv4guiModelEdit::SetupFaceListTable()
{
    if(!m_Model)
        return;

    m_OperatingWholeTableModel=true;
    m_FaceListTableModel->clear();

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    std::vector<sv4guiModelElement::svFace*> faces=modelElement->GetFaces();

    QStringList faceListHeaders;
    faceListHeaders << "ID" << "Name" << "Type" << "V" << "C" << "O";
    m_FaceListTableModel->setHorizontalHeaderLabels(faceListHeaders);
    m_FaceListTableModel->setColumnCount(6);

    // Block signals to m_FaceListTableModel so that the UpdateFaceData() 
    // callback does not get called when adding rows/cols to the table. 
    m_FaceListTableModel->blockSignals(true);

    int rowIndex=-1;

    for(int i=0;i<faces.size();i++)
    {
        if(faces[i]==NULL )
            continue;

        rowIndex++;
        m_FaceListTableModel->insertRow(rowIndex);

        QStandardItem* item;

        item= new QStandardItem(QString::number(faces[i]->id));
        item->setEditable(false);
        m_FaceListTableModel->setItem(rowIndex, 0, item);

        item= new QStandardItem(QString::fromStdString(faces[i]->name));
        m_FaceListTableModel->setItem(rowIndex, 1, item);

        item= new QStandardItem(QString::fromStdString(faces[i]->type));
        m_FaceListTableModel->setItem(rowIndex, 2, item);

        item = new QStandardItem();
        item->setEditable(false);
        if(faces[i]->visible)
        {
            item->setIcon(QIcon(":/show.png"));
        }else{
            item->setIcon(QIcon(":/hide.png"));
        }
        m_FaceListTableModel->setItem(rowIndex,3,item);

        item= new QStandardItem();
        item->setEditable(false);
        QBrush brush(QColor(255*faces[i]->color[0],255*faces[i]->color[1],255*faces[i]->color[2]));
        item->setBackground(brush);
        m_FaceListTableModel->setItem(rowIndex, 4, item);

        item= new QStandardItem();
        item->setData((int)(faces[i]->opacity*100)/100.0, Qt::EditRole);
        m_FaceListTableModel->setItem(rowIndex, 5, item);

    }

    ui->tableViewFaceList->horizontalHeader()->setSectionResizeMode(0, QHeaderView::Fixed);
    ui->tableViewFaceList->horizontalHeader()->resizeSection(0,20);
    ui->tableViewFaceList->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Interactive);
    ui->tableViewFaceList->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Fixed);
    ui->tableViewFaceList->horizontalHeader()->resizeSection(2,60);
    ui->tableViewFaceList->horizontalHeader()->setSectionResizeMode(3, QHeaderView::Fixed);
    ui->tableViewFaceList->horizontalHeader()->resizeSection(3,26);
    ui->tableViewFaceList->horizontalHeader()->setSectionResizeMode(4, QHeaderView::Fixed);
    ui->tableViewFaceList->horizontalHeader()->resizeSection(4,26);
    ui->tableViewFaceList->horizontalHeader()->setSectionResizeMode(5, QHeaderView::Fixed);
    ui->tableViewFaceList->horizontalHeader()->resizeSection(5,60);

    ui->tableViewFaceList->setColumnHidden(0,true);

    // Unblock signals to m_FaceListTableModel.
    m_FaceListTableModel->blockSignals(false);

    m_OperatingWholeTableModel=false;
}

void sv4guiModelEdit::UpdateFaceData(QStandardItem* item)
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    int row=item->index().row();
    int col=item->index().column();

    QStandardItem* itemID= m_FaceListTableModel->item(row,0);
    int id=itemID->text().toInt();

    sv4guiModelElement::svFace* face=modelElement->GetFace(id);

    if(face==NULL)
        return;

    if(col==1)
    {
        modelElement->SetFaceName(item->text().trimmed().toStdString(), id);
        SetupBlendTable();
    }else if(col==2){
        face->type=item->text().trimmed().toStdString();
    }else if(col==5){
        face->opacity=item->text().trimmed().toFloat();
    }

    if(!m_OperatingWholeTableModel)
        m_Model->SetDataModified();

    if(col==5)
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();

}

void sv4guiModelEdit::TableFaceListSelectionChanged( const QItemSelection & /*selected*/, const QItemSelection & /*deselected*/ )
{
    mitk::StatusBar::GetInstance()->DisplayText("");

    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    if(m_FaceListTableModel==NULL)
        return;

    QModelIndexList indexesOfSelectedRows = ui->tableViewFaceList->selectionModel()->selectedRows();

    modelElement->ClearFaceSelection();

    bool useFirst=true;
    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();

        QStandardItem* itemID= m_FaceListTableModel->item(row,0);
        int id=itemID->text().toInt();

        modelElement->SelectFace(id);

        if(useFirst)
        {
            double faceArea=modelElement->GetFaceArea(id);
            QString info="Face "+QString::fromStdString(modelElement->GetFaceName(id))+": Area="+QString::number(faceArea);
            mitk::StatusBar::GetInstance()->DisplayText(info.toStdString().c_str());
            useFirst=false;
        }
    }

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}


void sv4guiModelEdit::ToggleVisibility(const QModelIndex &index){

    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    int row=index.row();
    int col=index.column();

    if(col!=3)
        return;

    QStandardItem* itemID= m_FaceListTableModel->item(row,0);
    int id=itemID->text().toInt();

    sv4guiModelElement::svFace* face=modelElement->GetFace(id);

    if(face==NULL)
        return;

    QStandardItem* itemV= m_FaceListTableModel->item(row,col);

    if(face->visible)
    {
        face->visible=false;
        itemV->setIcon(QIcon(":/hide.png"));
        m_Model->SetDataModified();
        m_Model->Modified();
    }
    else
    {
        face->visible=true;
        itemV->setIcon(QIcon(":/show.png"));
        m_Model->SetDataModified();
        m_Model->Modified();
    }

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();

}

void sv4guiModelEdit::ChangeColor(const QModelIndex &index)
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    int row=index.row();
    int col=index.column();

    if(col!=4)
        return;

    QStandardItem* itemID= m_FaceListTableModel->item(row,0);
    int id=itemID->text().toInt();

    sv4guiModelElement::svFace* face=modelElement->GetFace(id);

    if(face==NULL)
        return;

    QStandardItem* itemC= m_FaceListTableModel->item(row,col);

    QColor initial(face->color[0]*255,face->color[1]*255,face->color[2]*255);

    QColor newColor=QColorDialog::getColor(initial,m_Parent,"Change Color");
    if(!newColor.isValid())
        return;

    face->color[0]=newColor.red()/255.0f;
    face->color[1]=newColor.green()/255.0f;
    face->color[2]=newColor.blue()/255.0f;
    m_Model->SetDataModified();

    QBrush brush(newColor);
    itemC->setBackground(brush);

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();

}

void sv4guiModelEdit::TableViewFaceListContextMenuRequested( const QPoint & pos )
{
    m_FaceListTableMenu->popup(QCursor::pos());
}

void sv4guiModelEdit::ShowSelected( bool )
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    if(m_FaceListTableModel==NULL)
        return;

    QModelIndexList indexesOfSelectedRows = ui->tableViewFaceList->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
        return;
    }

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();

        QStandardItem* itemID= m_FaceListTableModel->item(row,0);
        int id=itemID->text().toInt();
        sv4guiModelElement::svFace* face=modelElement->GetFace(id);

        if(face==NULL)
            continue;

        face->visible=true;
        m_Model->SetDataModified();

        QStandardItem* itemV= m_FaceListTableModel->item(row,3);
        itemV->setIcon(QIcon(":/show.png"));
    }
}

void sv4guiModelEdit::HideSelected( bool )
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    if(m_FaceListTableModel==NULL)
        return;

    QModelIndexList indexesOfSelectedRows = ui->tableViewFaceList->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
        return;
    }

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();

        QStandardItem* itemID= m_FaceListTableModel->item(row,0);
        int id=itemID->text().toInt();
        sv4guiModelElement::svFace* face=modelElement->GetFace(id);

        if(face==NULL)
            continue;

        face->visible=false;
        m_Model->SetDataModified();

        QStandardItem* itemV= m_FaceListTableModel->item(row,3);
        itemV->setIcon(QIcon(":/hide.png"));
    }

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();

}

void sv4guiModelEdit::ChangeOpacitySelected( bool )
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    if(m_FaceListTableModel==NULL)
        return;

    QModelIndexList indexesOfSelectedRows = ui->tableViewFaceList->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
        return;
    }

    bool ok;
    float opacity=QInputDialog::getDouble(m_Parent, "Change Opacity", "Opacity:", 1.0, 0, 1.0, 2, &ok);

    if(!ok)
        return;

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();

        QStandardItem* itemID= m_FaceListTableModel->item(row,0);
        int id=itemID->text().toInt();
        sv4guiModelElement::svFace* face=modelElement->GetFace(id);

        if(face==NULL)
            continue;

        //        face->opacity=opacity; //done by UpateFaceData()

        QStandardItem* itemO= m_FaceListTableModel->item(row,5);
        itemO->setData((int)(opacity*100)/100.0, Qt::EditRole);
    }
}

void sv4guiModelEdit::ChangeColorSelected( bool )
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    if(m_FaceListTableModel==NULL)
        return;

    QModelIndexList indexesOfSelectedRows = ui->tableViewFaceList->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
        return;
    }

    QColor newColor=QColorDialog::getColor(Qt::white,m_Parent,"Change Color");
    if(!newColor.isValid())
        return;

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();

        QStandardItem* itemID= m_FaceListTableModel->item(row,0);
        int id=itemID->text().toInt();
        sv4guiModelElement::svFace* face=modelElement->GetFace(id);

        if(face==NULL)
            continue;

        face->color[0]=newColor.red()/255.0f;
        face->color[1]=newColor.green()/255.0f;
        face->color[2]=newColor.blue()/255.0f;
        m_Model->SetDataModified();

        QStandardItem* itemC= m_FaceListTableModel->item(row,4);
        QBrush brush(newColor);
        itemC->setBackground(brush);
    }
}

void sv4guiModelEdit::ChangeTypeSelected( bool )
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    if(m_FaceListTableModel==NULL)
        return;

    QModelIndexList indexesOfSelectedRows = ui->tableViewFaceList->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
        return;
    }

    QStringList items;
    //    items << tr("wall") << tr("inlet") << tr("outlet") << tr("cap");
    items << tr("wall") << tr("cap");

    bool ok;
    QString type = QInputDialog::getItem(m_Parent, "Change Type", "Type:", items, 0, false, &ok);
    if (!ok || type.isEmpty())
        return;

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();

        QStandardItem* itemID= m_FaceListTableModel->item(row,0);
        int id=itemID->text().toInt();
        sv4guiModelElement::svFace* face=modelElement->GetFace(id);

        if(face==NULL)
            continue;

        //        face->type=type; //done by UpateFaceData()

        QStandardItem* itemT= m_FaceListTableModel->item(row,2);
        itemT->setData(type, Qt::EditRole);
    }
}

void sv4guiModelEdit::UpdateBlendTable(int index)
{
    if(index!=1)
        return;

    //    SetupBlendTable();
}

void sv4guiModelEdit::SetupBlendTable()
{
    if(m_LocalOperationforBlendRegion)
        return;

    if(!m_Model)
        return;

    m_BlendTableModel->clear();

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    std::vector<sv4guiModelElement::svFace*> faces=modelElement->GetFaces();

    QStringList blendHeaders;
    blendHeaders << "Use" << "Face 1" << "Face 2" << "Radius";
    m_BlendTableModel->setHorizontalHeaderLabels(blendHeaders);
    m_BlendTableModel->setColumnCount(4);

    int rowIndex=-1;

    // Block signals to m_BlendTableModel so events are not processed 
    // when adding rows/cols to the table. 
    m_BlendTableModel->blockSignals(true);

    for(int i=0;i<faces.size();i++)
    {
        if(faces[i]==NULL || faces[i]->type=="cap" || faces[i]->type=="inlet" || faces[i]->type=="outlet" || faces[i]->name.substr(0,10)=="wall_blend")
            continue;

        for(int j=i+1;j<faces.size();j++)
        {
            if(faces[j]==NULL || faces[j]->type=="cap" || faces[j]->type=="inlet" || faces[j]->type=="outlet" || faces[j]->name.substr(0,10)=="wall_blend")
                continue;

            //To do: check if two faces are adjcent;
            //Todo: create linking list in sv4guiModelElementPolyData, avoiding to create multiple times in the plugin
            //            vtkSmartPointer<vtkIntersectionPolyDataFilter> intersectionPolyDataFilter =
            //                    vtkSmartPointer<vtkIntersectionPolyDataFilter>::New();
            //            intersectionPolyDataFilter->SetInputData(0, facec[i]->vpd);
            //            intersectionPolyDataFilter->SetInputData(1, facec[j]->vpd);
            //            intersectionPolyDataFilter->Update();

            //            if(inintersectionPolyDataFilter->GetOutput()->GetNumberOfCells()<1)
            //                continue;


            rowIndex++;
            m_BlendTableModel->insertRow(rowIndex);

            QStandardItem* item;
            sv4guiModelElement::svBlendParamRadius* blendParam= modelElement->GetBlendParamRadius(faces[i]->id, faces[j]->id);

            item = new QStandardItem();
            item->setCheckable(true);
            if(blendParam)
            {
                item->setCheckState(Qt::Unchecked);
            }else{
                item->setCheckState(Qt::Checked);
            }
            m_BlendTableModel->setItem(rowIndex, 0, item);

            item= new QStandardItem(QString::fromStdString(faces[i]->name));
            item->setEditable(false);
            if(blendParam)
            {
                QBrush brush(Qt::lightGray);
                item->setBackground(brush);
            }
            m_BlendTableModel->setItem(rowIndex, 1, item);

            item= new QStandardItem(QString::fromStdString(faces[j]->name));
            item->setEditable(false);
            if(blendParam)
            {
                QBrush brush(Qt::lightGray);
                item->setBackground(brush);
            }
            m_BlendTableModel->setItem(rowIndex, 2, item);

            item= new QStandardItem();
            if(blendParam)
            {
                item->setText(QString::number(blendParam->radius));
                QBrush brush(Qt::lightGray);
                item->setBackground(brush);
            }

            m_BlendTableModel->setItem(rowIndex, 3, item);

        }

    }

    ui->tableViewBlend->setColumnWidth(0,60);
    ui->tableViewBlend->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Interactive);
    ui->tableViewBlend->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Interactive);
    ui->tableViewBlend->horizontalHeader()->setSectionResizeMode(3, QHeaderView::Stretch);
    m_BlendTableModel->blockSignals(false);
}

void sv4guiModelEdit::UpdatePolyDataBlendParam()
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElementPolyData* mepd=dynamic_cast<sv4guiModelElementPolyData*>(m_Model->GetModelElement(timeStep));
    if(mepd==NULL) return;

    sv4guiModelElement::svBlendParam* param=mepd->GetBlendParam();

    ui->dsbDecimation->setValue(param->targetdecimation);
    ui->sbBlendIters->setValue(param->numblenditers);
    ui->sbSubBlendIters->setValue(param->numsubblenditers);
    ui->sbCstrSmoothIters->setValue(param->numcgsmoothiters);
    ui->sbLapSmoothIters->setValue(param->numlapsmoothiters);
    ui->sbSubdivisionIters->setValue(param->numsubdivisioniters);
}

void sv4guiModelEdit::TableBlendSelectionChanged( const QItemSelection & /*selected*/, const QItemSelection & /*deselected*/ )
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    if(m_BlendTableModel==NULL)
        return;

    QModelIndexList indexesOfSelectedRows = ui->tableViewBlend->selectionModel()->selectedRows();

    modelElement->ClearFaceSelection();

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();

        QStandardItem* itemFace1= m_BlendTableModel->item(row,1);
        QStandardItem* itemFace2= m_BlendTableModel->item(row,2);

        modelElement->SelectFace(itemFace1->text().toStdString());
        modelElement->SelectFace(itemFace2->text().toStdString());
    }

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    UpdateFaceListSelection();
}

void sv4guiModelEdit::TableViewBlendContextMenuRequested( const QPoint & pos )
{
    m_BlendTableMenu->popup(QCursor::pos());
}

void sv4guiModelEdit::SetRadius(bool)
{
    if(m_BlendTableModel==NULL)
        return;

    QModelIndexList indexesOfSelectedRows = ui->tableViewBlend->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
        return;
    }

    bool ok;
    double radius=QInputDialog::getDouble(m_Parent, "Set Blending Radius", "Radius:", 0.05, 0, 100, 3, &ok);

    if(!ok)
        return;

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();

        QStandardItem* item= m_BlendTableModel->item(row,3);
        item->setText(QString::number(radius));
    }
}

void sv4guiModelEdit::ClearRadius(bool)
{
    if(m_BlendTableModel==NULL)
        return;

    QModelIndexList indexesOfSelectedRows = ui->tableViewBlend->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
        return;
    }

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();

        QStandardItem* item= m_BlendTableModel->item(row,3);
        item->setText("");
    }
}

void sv4guiModelEdit::UseSelectedBlend(bool)
{
    if(m_BlendTableModel==NULL)
        return;

    QModelIndexList indexesOfSelectedRows = ui->tableViewBlend->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
        return;
    }

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();

        QStandardItem* item= m_BlendTableModel->item(row,0);
        item->setCheckState(Qt::Checked);
    }
}

void sv4guiModelEdit::NotUseSelectedBlend(bool)
{
    if(m_BlendTableModel==NULL)
        return;

    QModelIndexList indexesOfSelectedRows = ui->tableViewBlend->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
        return;
    }

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();

        QStandardItem* item= m_BlendTableModel->item(row,0);
        item->setCheckState(Qt::Unchecked);
    }
}

void sv4guiModelEdit::NodeChanged(const mitk::DataNode* node)
{
    if(m_ModelNode==node)
        ui->labelModelName->setText(QString::fromStdString(m_ModelNode->GetName()));
}

void sv4guiModelEdit::NodeAdded(const mitk::DataNode* node)
{
}

void sv4guiModelEdit::NodeRemoved(const mitk::DataNode* node)
{
}

void sv4guiModelEdit::AddObservers()
{
    if(m_ModelNode.IsNotNull())
    {
        if(m_ModelNode->GetDataInteractor().IsNull())
        {
            m_DataInteractor = sv4guiModelDataInteractor::New();
            m_DataInteractor->LoadStateMachine("sv4gui_ModelInteraction.xml", us::ModuleRegistry::GetModule("sv4guiModuleModel"));
            m_DataInteractor->SetEventConfig("sv4gui_ModelConfig.xml", us::ModuleRegistry::GetModule("sv4guiModuleModel"));
            m_DataInteractor->SetDataNode(m_ModelNode);
        }
        m_ModelNode->SetStringProperty("interactor user","modeling");
        sv4guiModelDataInteractor* interactor=dynamic_cast<sv4guiModelDataInteractor*>(m_ModelNode->GetDataInteractor().GetPointer());
        if(interactor)
            interactor->SetFaceSelectionOnly(false);
    }

    if(m_ModelSelectFaceObserverTag==-1)
    {
        itk::SimpleMemberCommand<sv4guiModelEdit>::Pointer modelSelectFaceCommand = itk::SimpleMemberCommand<sv4guiModelEdit>::New();
        modelSelectFaceCommand->SetCallbackFunction(this, &sv4guiModelEdit::UpdateFaceListSelection);
        m_ModelSelectFaceObserverTag = m_Model->AddObserver( sv4guiModelSelectFaceEvent(), modelSelectFaceCommand);
    }

    if(m_ModelUpdateObserverTag==-1)
    {
        itk::SimpleMemberCommand<sv4guiModelEdit>::Pointer modelUpdateCommand = itk::SimpleMemberCommand<sv4guiModelEdit>::New();
        modelUpdateCommand->SetCallbackFunction(this, &sv4guiModelEdit::UpdateGUI);
        m_ModelUpdateObserverTag = m_Model->AddObserver( sv4guiModelSetEvent(), modelUpdateCommand);
    }
}

void sv4guiModelEdit::RemoveObservers()
{
    if(m_Model && m_ModelSelectFaceObserverTag!=-1)
    {
        m_Model->RemoveObserver(m_ModelSelectFaceObserverTag);
        m_ModelSelectFaceObserverTag=-1;
    }

    if(m_Model && m_ModelUpdateObserverTag!=-1)
    {
        m_Model->RemoveObserver(m_ModelUpdateObserverTag);
        m_ModelUpdateObserverTag=-1;
    }

    if(m_ModelNode)
    {
        std::string user="";
        m_ModelNode->GetStringProperty("interactor user", user);
        if(user=="modeling")
            m_ModelNode->SetDataInteractor(NULL);
    }
    m_DataInteractor=NULL;
}

void sv4guiModelEdit::ClearAll()
{
    m_Model=NULL;
    m_ModelNode=NULL;

    ui->labelModelName->setText("");
    ui->labelModelType->setText("");
}

void sv4guiModelEdit::ShowSegSelectionWidget()
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSubset(isProjFolder);

    if(rs->size()<1) return;

    mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

    rs=GetDataStorage()->GetDerivations (projFolderNode,mitk::NodePredicateDataType::New("sv4guiSegmentationFolder"));
    if(rs->size()<1) return;

    mitk::DataNode::Pointer segFolderNode=rs->GetElement(0);
    rs=GetDataStorage()->GetDerivations(segFolderNode);
    if(rs->size()<1) return;

    std::vector<mitk::DataNode::Pointer> segNodes;
    for(int i=0;i<rs->size();i++)
    {
        if(m_ModelType=="OpenCASCADE" || m_ModelType=="Parasolid")
        {
            sv4guiContourGroup* group = dynamic_cast<sv4guiContourGroup*>(rs->GetElement(i)->GetData());
            if(group==NULL)
                continue;
        }
        segNodes.push_back(rs->GetElement(i));
    }

    m_SegSelectionWidget->SetTableView(segNodes,modelElement,m_ModelType);
    m_SegSelectionWidget->show();
}

void sv4guiModelEdit::ShowCapSelectionWidget()
{
    if(!m_Model)
        return;

    if (m_ModelType != "PolyData")
    {
      QMessageBox::warning(m_Parent,"Error","Cannot currently extract centerlines of anyting other than a PolyData model");
      return;
    }

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);

    std::vector<sv4guiModelElement::svFace*> faces=modelElement->GetFaces();
    std::vector<std::string> caps;

    int rowIndex=-1;

    for(int i=0;i<faces.size();i++)
    {
        if(faces[i]==NULL )
            continue;

        if (faces[i]->type=="cap")
          caps.push_back(faces[i]->name);
    }

    m_CapSelectionWidget->SetTableView(caps,modelElement,m_ModelType);
    m_CapSelectionWidget->show();
}

void sv4guiModelEdit::CreateModel()
{
    std::vector<std::string> segNames=m_SegSelectionWidget->GetUsedSegNames();
    int numSampling=m_SegSelectionWidget->GetNumSampling();

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSubset(isProjFolder);

    if(rs->size()<1) return;

    mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

    rs=GetDataStorage()->GetDerivations (projFolderNode,mitk::NodePredicateDataType::New("sv4guiSegmentationFolder"));
    if(rs->size()<1) return;

    mitk::DataNode::Pointer segFolderNode=rs->GetElement(0);

    std::vector<mitk::DataNode::Pointer> segNodes;

    for(int i=0;i<segNames.size();i++)
    {
        mitk::DataNode::Pointer node=GetDataStorage()->GetNamedDerivedNode(segNames[i].c_str(),segFolderNode);
        if(node.IsNotNull())
            segNodes.push_back(node);
    }

    //sanity check
    int numSeg2D=0;
    int numSeg3D=0;

    for(int i=0;i<segNodes.size();i++)
    {
        sv4guiContourGroup* group = dynamic_cast<sv4guiContourGroup*>(segNodes[i]->GetData());
        if(group!=NULL)
        {
            numSeg2D++;
            continue;
        }
        sv4guiMitkSeg3D* seg3D = dynamic_cast<sv4guiMitkSeg3D*>(segNodes[i]->GetData());
        if(seg3D!=NULL)
        {
            numSeg3D++;
            continue;
        }

    }

    if(numSeg2D+numSeg3D==0)
    {
        QMessageBox::warning(m_Parent,"Warning","No valid segmentations are used.");
        return;
    }

    if(numSeg2D==0 && (m_ModelType=="OpenCASCADE" || m_ModelType=="Parasolid"))
    {
        QMessageBox::warning(m_Parent,"Warning","No valid 2D segmentations are used.");
        return;
    }

    sv4guiModelElement* newModelElement=NULL;
    sv4guiModelElement* modelElement=m_Model->GetModelElement();

    mitk::ProgressBar::GetInstance()->Reset();
    mitk::ProgressBar::GetInstance()->AddStepsToDo(3);
    mitk::StatusBar::GetInstance()->DisplayText("Creating model...");
    mitk::ProgressBar::GetInstance()->Progress();
    WaitCursorOn();

    svLoftingParam* param=NULL;
    int useUniform=m_SegSelectionWidget->IfUseUniform();
    if(useUniform)
        param=new svLoftingParam(m_SegSelectionWidget->GetLoftingParam());

    int created = 1;
    QString statusText="Model has been created.";

    sv4guiModelElement* tempElement=sv4guiModelElementFactory::CreateModelElement(m_ModelType);
    if(tempElement)
    {
        int stats[2]={0};
        if(m_ModelType=="PolyData")
        {
            newModelElement=tempElement->CreateModelElement(segNodes,numSampling,param,stats);
        }
        else if(m_ModelType=="OpenCASCADE")
        {
            newModelElement=tempElement->CreateModelElement(segNodes,numSampling,param,NULL,20.0);
        }
        else if(m_ModelType=="Parasolid")
        {
            newModelElement=tempElement->CreateModelElement(segNodes,numSampling,NULL,NULL,1.0);
        }

        if(newModelElement==NULL)
        {
            statusText="Failed to create model.";
            created = 0;
        }
        else if(m_ModelType=="PolyData")
        {
            statusText=statusText+" Number of Free Edges: "+ QString::number(stats[0])+", Number of Bad Edges: "+ QString::number(stats[1]);
        }

        if(newModelElement)
        {
            newModelElement->SetUseUniform(useUniform);
            if(useUniform)
                newModelElement->SetLoftingParam(param);
        }
    }

    delete tempElement;

//    if(param)
//        delete param;

    WaitCursorOff();
    mitk::ProgressBar::GetInstance()->Progress(2);
    mitk::StatusBar::GetInstance()->DisplayText(statusText.toStdString().c_str());
    if (!created)
    {
      QMessageBox::warning(m_Parent,"Warning","Error creating model.");
      return;
    }

    if(newModelElement!=NULL)
    {
//        m_LocalOperationforBlendRegion=false;

        int timeStep=GetTimeStep();

        mitk::OperationEvent::IncCurrObjectEventId();
        sv4guiModelOperation* doOp = new sv4guiModelOperation(sv4guiModelOperation::OpSETMODELELEMENT,timeStep,newModelElement);
        sv4guiModelOperation* undoOp = new sv4guiModelOperation(sv4guiModelOperation::OpSETMODELELEMENT,timeStep,modelElement);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Model, doOp, undoOp, "Set ModelElement");
        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

        m_Model->ExecuteOperation(doOp);

//        UpdateGUI();

        std::vector<std::string> segNames=newModelElement->GetSegNames();
        std::vector<std::string> faceNames=newModelElement->GetFaceNames();
        if(faceNames.size()<=2*segNames.size()+1)
            return;

        //find possible extra faces
        std::vector<std::string> faceNamesToCheck;

        std::string wallPrefix="wall_";
        std::string capPrefix="cap_";
        if(newModelElement->GetType()=="Parasolid")
            capPrefix="";

        for(int i=0;i<segNames.size();++i)
        {
            int capNumber=0;
            int wallNumber=0;

            QString wallName=QString::fromStdString(wallPrefix+segNames[i]);
            QString capName=QString::fromStdString(capPrefix+segNames[i]);

            for(int j=0;j<faceNames.size();j++)
            {
                QString faceName=QString::fromStdString(faceNames[j]);

                if(faceName.contains(wallName))
                {
                    faceName.remove(wallName);
                    if(faceName=="")
                        wallNumber++;
                    else
                    {
                        faceName.remove(0,1);
                        bool ok;
                        faceName.toInt(&ok);
                        if(ok)
                            wallNumber++;
                    }
                }
                else if (faceName.contains(capName))
                {
                    faceName.remove(capName);
                    if(faceName=="")
                        capNumber++;
                    else
                    {
                        faceName.remove(0,1);
                        bool ok;
                        faceName.toInt(&ok);
                        if(ok)
                            capNumber++;
                    }
                }
            }

            if(capNumber>1)
            {
                for(int j=0;j<capNumber;++j)
                {
                    QString suffix="";
                    if(j>0)
                        suffix="_"+QString::number(j+1);

                    faceNamesToCheck.push_back((capName+suffix).toStdString());
                }
            }

            if(wallNumber>1)
            {
                for(int j=0;j<wallNumber;++j)
                {
                    QString suffix="";
                    if(j>0)
                        suffix="_"+QString::number(j+1);

                    faceNamesToCheck.push_back((wallName+suffix).toStdString());
                }
            }

        }

        if(faceNamesToCheck.size()>0)
        {
            std::string faceList ="";
            for(int i=0;i<faceNamesToCheck.size();i++)
            {
                faceList+="\n"+faceNamesToCheck[i];
                newModelElement->SelectFace(faceNamesToCheck[i]);
            }

            UpdateFaceListSelection();

            // Display a warning message listing the problematic faces.
            std::string info = "There may be vessels that only partially intersect.\n\n";
            info += "Please check the faces listed under Details and highlighted in the Face List browser.";
            auto text = QString::fromStdString(info);
            QString title = "A problem was encountered when creating the model";
            QMessageBox::Icon icon = QMessageBox::Warning;
            QMessageBox mb(NULL); 
            mb.setWindowTitle(title);
            mb.setText(text+"                                                                                         ");
            mb.setIcon(icon);
            mb.setDetailedText(QString::fromStdString(faceList));
            mb.exec();

            return;
        }

    }
}

void sv4guiModelEdit::ExtractCenterlines()
{
    if (m_ModelType != "PolyData")
    {
      QMessageBox::warning(m_Parent,"Error","Cannot currently extract centerlines of anyting other than a PolyData model");
      return;
    }

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);

    std::vector<std::string> capNames = m_CapSelectionWidget->GetUsedCapNames();
    std::vector<int> capIds;
    for (int i=0; i<capNames.size(); i++)
      capIds.push_back(modelElement->GetFaceID(capNames[i]));

    sv4guiModelExtractPathsAction *extractPathsAction = new sv4guiModelExtractPathsAction();
    extractPathsAction->SetDataStorage(this->GetDataStorage());
    extractPathsAction->SetFunctionality(this);
    extractPathsAction->SetSourceCapIds(capIds);
    QList<mitk::DataNode::Pointer> selectedNode;
    selectedNode.push_back(m_ModelNode);
    extractPathsAction->Run(selectedNode);

    return;
}

std::vector<sv4guiModelElement::svBlendParamRadius*> sv4guiModelEdit::GetBlendRadii()
{
    std::vector<sv4guiModelElement::svBlendParamRadius*> blendRadii;

    if(!m_Model)
        return blendRadii;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL)
        return blendRadii;

    if(m_BlendTableModel==NULL)
        return blendRadii;

    int rowCount=m_BlendTableModel->rowCount();

    for (int i=0;i<rowCount;i++)
    {
        QStandardItem* itemUse= m_BlendTableModel->item(i,0);
        QStandardItem* itemFace1= m_BlendTableModel->item(i,1);
        QStandardItem* itemFace2= m_BlendTableModel->item(i,2);
        QStandardItem* itemRadius= m_BlendTableModel->item(i,3);

        if(itemUse->checkState()==Qt::Unchecked || itemRadius->text().trimmed()=="")
            continue;

        double radius=itemRadius->text().trimmed().toDouble();
        if(radius<=0)
            continue;

        std::string faceName1=itemFace1->text().toStdString();
        std::string faceName2=itemFace2->text().toStdString();
        int faceID1=modelElement->GetFaceID(faceName1);
        int faceID2=modelElement->GetFaceID(faceName2);

        blendRadii.push_back(new sv4guiModelElement::svBlendParamRadius(faceID1,faceID2,faceName1,faceName2,radius));
    }

    return blendRadii;
}

void sv4guiModelEdit::BlendModel()
{
    if(m_Model==NULL) return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);

    if(modelElement==NULL) return;

    sv4guiModelElement* newModelElement=NULL;

    std::vector<sv4guiModelElement::svBlendParamRadius*> blendRadii=GetBlendRadii();
    if(blendRadii.size()==0)
        return;

    mitk::ProgressBar::GetInstance()->Reset();
    mitk::ProgressBar::GetInstance()->AddStepsToDo(2);
    mitk::StatusBar::GetInstance()->DisplayText("Blending model...");
    mitk::ProgressBar::GetInstance()->Progress();
    WaitCursorOn();

    sv4guiModelElement::svBlendParam* param=NULL;
    std::string status="Blending done.";
    if(m_ModelType=="PolyData"){
        param=new sv4guiModelElement::svBlendParam();

        param->numblenditers=ui->sbBlendIters->value();
        param->numsubblenditers=ui->sbSubBlendIters->value();
        param->numcgsmoothiters=ui->sbCstrSmoothIters->value();
        param->numlapsmoothiters=ui->sbLapSmoothIters->value();
        param->numsubdivisioniters=ui->sbSubdivisionIters->value();
        param->targetdecimation=ui->dsbDecimation->value();
    }

    try{
        newModelElement=modelElement->CreateModelElementByBlend(blendRadii,param);
    }
    catch(...)
    {
        status="Error during blending";
    }

    if(param)
        delete param;

    WaitCursorOff();
    mitk::ProgressBar::GetInstance()->Progress();
    mitk::StatusBar::GetInstance()->DisplayText(status.c_str());

    if(newModelElement==NULL) return;

//    m_LocalOperationforBlendRegion=false;

    mitk::OperationEvent::IncCurrObjectEventId();

    sv4guiModelOperation* doOp = new sv4guiModelOperation(sv4guiModelOperation::OpSETMODELELEMENT,timeStep,newModelElement);
    sv4guiModelOperation* undoOp = new sv4guiModelOperation(sv4guiModelOperation::OpSETMODELELEMENT,timeStep,modelElement);
    mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Model, doOp, undoOp, "Set ModelElement");
    mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

    m_Model->ExecuteOperation(doOp);

    //    SetupFaceListTable();

    //    UpdateFaceListSelection();

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

std::vector<int> sv4guiModelEdit::GetSelectedFaceIDs()
{
    std::vector<int> faceIDs;

    if(!m_Model)
        return faceIDs;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return faceIDs;

    if(m_FaceListTableModel==NULL)
        return faceIDs;

    QModelIndexList indexesOfSelectedRows = ui->tableViewFaceList->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
        return faceIDs;
    }

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();

        QStandardItem* itemID= m_FaceListTableModel->item(row,0);
        int id=itemID->text().toInt();
        faceIDs.push_back(id);
    }

    return faceIDs;
}

bool sv4guiModelEdit::MarkCells(sv4guiModelElementPolyData* modelElement)
{
    modelElement->RemoveActiveCells();

    bool hasFaces=false;
    if(ui->checkBoxFaceJunctions->isChecked())
    {
        hasFaces=true;
        std::vector<sv4guiModelElement::svBlendParamRadius*> blendRadii=GetBlendRadii();
        if(blendRadii.size()==0)
            return false;

        for(int i=0;i<blendRadii.size();i++)
        {
            if(blendRadii[i])
            {
                std::vector<int> facePair={blendRadii[i]->faceID1,blendRadii[i]->faceID2};
                if(!modelElement->MarkCellsByFaceJunctions(facePair,blendRadii[i]->radius))
                    return false;
            }
        }
        m_LocalOperationforBlendRegion=true;
    }
    else
    {
        if(modelElement->GetSelectedFaceIDs().size()>0)
        {
            hasFaces=true;
            if(!modelElement->MarkCellsByFaces(modelElement->GetSelectedFaceIDs()))
                return false;
        }
    }

    bool hasCells=false;
    if(modelElement->GetSelectedCellIDs().size()>0)
    {
        hasCells=true;
        if(!modelElement->MarkCells(modelElement->GetSelectedCellIDs()))
            return false;
    }

    bool hasSphere=false;
    if(m_SphereWidget!=NULL && m_SphereWidget->GetEnabled() && m_SphereWidget->GetRadius()>0)
    {
        hasSphere=true;
        if(!modelElement->MarkCellsBySphere(m_SphereWidget->GetRadius(), m_SphereWidget->GetCenter()))
            return false;
    }

    if(!hasFaces && !hasCells && !hasSphere)
        return false;
    else
        return true;

}

void sv4guiModelEdit::ModelOperate(int operationType)
{
    if(m_Model==NULL) return;

    int timeStep=GetTimeStep();
    sv4guiModelElementPolyData* modelElement=dynamic_cast<sv4guiModelElementPolyData*>(m_Model->GetModelElement(timeStep));

    if(modelElement==NULL) return;

    sv4guiModelElementPolyData* newModelElement=modelElement->Clone();

    bool ok=false;

    m_LocalOperationforBlendRegion=false;

    mitk::ProgressBar::GetInstance()->Reset();
    mitk::ProgressBar::GetInstance()->AddStepsToDo(3);
    mitk::StatusBar::GetInstance()->DisplayText("Processing model...");
    mitk::ProgressBar::GetInstance()->Progress();
    WaitCursorOn();

    switch(operationType)
    {
    case DELETE_FACES:
        ok=newModelElement->DeleteFaces(GetSelectedFaceIDs());
        break;
    case FILL_HOLES_WITH_IDS:
        ok=newModelElement->FillHolesWithIDs();
        break;
    case COMBINE_FACES:
        ok=newModelElement->CombineFaces(GetSelectedFaceIDs());
        break;
    case REMESH_FACES:
        {
          QString qstring_size=ui->lineEditEstimateEdgeSize0->text().trimmed();
          bool valid_number=false;
          double edge_size = qstring_size.toDouble(&valid_number);
          if(valid_number)
          {
            ok=newModelElement->RemeshFaces(GetSelectedFaceIDs(), edge_size);
          }
          else
          {
            QMessageBox::warning(m_Parent,"Warning","Error in Egde Size!");
          }
          break;
        }
    case EXTRACT_FACES:
        ok=newModelElement->ExtractFaces(ui->sbSeparationAngle->value());
        break;
    case FILL_HOLES:
        ok=newModelElement->FillHoles();
        break;
    case SELECT_LARGEST_CONNECTED:
        ok=newModelElement->SelectLargestConnectedRegion();
        break;
#ifdef SV_USE_MMG
    case REMESH_GLOBAL:
        {
          QString qstring_size=ui->lineEditEstimateEdgeSize1->text().trimmed();
          bool valid_number=false;
          double edge_size = qstring_size.toDouble(&valid_number);
          if(valid_number)
          {
            ok=newModelElement->RemeshG(edge_size, edge_size);
          }
          else
          {
            QMessageBox::warning(m_Parent,"Warning","Error in Egde Size!");
          }
          break;
        }
#endif
    case DECIMATE_GLOBAL:
        ok=newModelElement->Decimate(ui->dsbTargetRateG->value());
        break;
    case LAPLACIAN_SMOOTH_GLOBAL:
        ok=newModelElement->LaplacianSmooth(ui->sbLapItersG->value(),ui->dsbLapRelaxG->value());
        break;
    case BUTTERFLY_SUBDIVIDE_GLOBAL:
        ok=newModelElement->ButterflySubdivide(ui->sbBFDivisionsG->value());
        break;
    case WINDOWSINC_SMOOTH_GLOBAL:
        ok=newModelElement->WindowSincSmooth(ui->sbWSItersG->value(),ui->dsbWSBandG->value());
        break;
    case DENSIFY_GLOBAL:
        ok=newModelElement->Densify(ui->sbDensifyDivisionsG->value());
        break;
    case DECIMATE_LOCAL:
        if(MarkCells(newModelElement))
            ok=newModelElement->DecimateLocal(ui->dsbTargetRateL->value());
        break;
    case LAPLACIAN_SMOOTH_LOCAL:
        if(MarkCells(newModelElement))
            ok=newModelElement->LaplacianSmoothLocal(ui->sbLapItersL->value(),ui->dsbLapRelaxL->value());
        break;
    case CONSTRAIN_SMOOTH_LOCAL:
        if(MarkCells(newModelElement))
            ok=newModelElement->ConstrainSmoothLocal(ui->sbCstrItersL->value(),ui->dsbCstrFactorL->value());
        break;
    case LINEAR_SUBDIVIDE_LOCAL:
        if(MarkCells(newModelElement))
            ok=newModelElement->LinearSubdivideLocal(ui->sbLinearDivisionsL->value());
        break;
    case LOOP_SUBDIVIDE_LOCAL:
        if(MarkCells(newModelElement))
            ok=newModelElement->LoopSubdivideLocal(ui->sbLoopDivisionsL->value());
        break;
    case CUT_ABOVE:
        if(m_PlaneWidget!=NULL && m_PlaneWidget->GetEnabled())
        {
            double* origin=m_PlaneWidget->GetOrigin();
            double* point1=m_PlaneWidget->GetPoint1();
            double* point2=m_PlaneWidget->GetPoint2();
            ok=newModelElement->CutByPlane(origin,point1,point2,true);
        }
        break;
    case CUT_BELOW:
        if(m_PlaneWidget!=NULL && m_PlaneWidget->GetEnabled())
        {
            double* origin=m_PlaneWidget->GetOrigin();
            double* point1=m_PlaneWidget->GetPoint1();
            double* point2=m_PlaneWidget->GetPoint2();
            ok=newModelElement->CutByPlane(origin,point1,point2,false);
        }
        break;
    case CUT_BOX:
        if(m_BoxWidget!=NULL && m_BoxWidget->GetEnabled())
        {
            vtkSmartPointer<vtkPlanes> boxPlanes=vtkSmartPointer<vtkPlanes>::New();
            m_BoxWidget->GetPlanes(boxPlanes);
            ok=newModelElement->CutByBox(boxPlanes,true);
        }
        break;
    default:
        break;
    }

    WaitCursorOff();
    mitk::ProgressBar::GetInstance()->Progress(2);

    if(!ok)
    {
        delete newModelElement;
        mitk::StatusBar::GetInstance()->DisplayText("Model pressing not successful");
        return;
    }

    mitk::OperationEvent::IncCurrObjectEventId();

    sv4guiModelOperation* doOp = new sv4guiModelOperation(sv4guiModelOperation::OpSETMODELELEMENT,timeStep,newModelElement);
    sv4guiModelOperation* undoOp = new sv4guiModelOperation(sv4guiModelOperation::OpSETMODELELEMENT,timeStep,modelElement);
    mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Model, doOp, undoOp, "Set ModelElement");
    mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

    m_Model->ExecuteOperation(doOp);

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();

    mitk::StatusBar::GetInstance()->DisplayText("Model processing done");

    m_LocalOperationforBlendRegion=false;

    // Set the time that the model surface was modified.
    SetTimeModified();
}

void sv4guiModelEdit::ShowSphereInteractor(bool checked)
{
    if(!checked)
    {
        if(m_SphereWidget!=NULL)
        {
            m_SphereWidget->Off();
        }

        return;
    }

    if(m_Model==NULL) return;

    int timeStep=GetTimeStep();
    sv4guiModelElementPolyData* modelElement=dynamic_cast<sv4guiModelElementPolyData*>(m_Model->GetModelElement(timeStep));

    if(modelElement==NULL) return;

    if(modelElement->GetWholeVtkPolyData()==NULL) return;
    if(m_SphereWidget==NULL)
    {
        m_SphereWidget = vtkSmartPointer<vtkSphereWidget>::New();
        m_SphereWidget->SetInteractor(m_DisplayWidget->GetRenderWindow4()->GetVtkRenderWindow()->GetInteractor());
        //    m_SphereWidget->SetRepresentationToSurface();
    }

    m_SphereWidget->SetInputData(modelElement->GetWholeVtkPolyData());
    m_SphereWidget->PlaceWidget();

    m_SphereWidget->On();
}

void sv4guiModelEdit::ShowPlaneInteractor(bool checked)
{
    if(!checked)
    {
        if(m_PlaneWidget!=NULL)
        {
            m_PlaneWidget->Off();
        }

        return;
    }

    if(m_Model==NULL) return;

    int timeStep=GetTimeStep();
    sv4guiModelElementPolyData* modelElement=dynamic_cast<sv4guiModelElementPolyData*>(m_Model->GetModelElement(timeStep));

    if(modelElement==NULL) return;

    if(modelElement->GetWholeVtkPolyData()==NULL) return;
    if(m_PlaneWidget==NULL)
    {
        m_PlaneWidget = vtkSmartPointer<vtkPlaneWidget>::New();
        m_PlaneWidget->SetInteractor(m_DisplayWidget->GetRenderWindow4()->GetVtkRenderWindow()->GetInteractor());
        m_PlaneWidget->GetHandleProperty()->SetOpacity(0.8);
        m_PlaneWidget->GetPlaneProperty()->SetLineWidth(1);
        //    m_PlaneWidget->SetRepresentationToSurface();
    }

    m_PlaneWidget->SetInputData(modelElement->GetWholeVtkPolyData());
    m_PlaneWidget->PlaceWidget();

    m_PlaneWidget->On();
}

void sv4guiModelEdit::ShowBoxInteractor(bool checked)
{
    if(!checked)
    {
        if(m_BoxWidget!=NULL)
        {
            m_BoxWidget->Off();
        }

        return;
    }

    if(m_Model==NULL) return;

    int timeStep=GetTimeStep();
    sv4guiModelElementPolyData* modelElement=dynamic_cast<sv4guiModelElementPolyData*>(m_Model->GetModelElement(timeStep));

    if(modelElement==NULL) return;

    if(modelElement->GetWholeVtkPolyData()==NULL) return;
    if(m_BoxWidget==NULL)
    {
        m_BoxWidget = vtkSmartPointer<vtkBoxWidget>::New();
        m_BoxWidget->SetInteractor(m_DisplayWidget->GetRenderWindow4()->GetVtkRenderWindow()->GetInteractor());
        m_BoxWidget->OutlineCursorWiresOff();
        m_BoxWidget->RotationEnabledOn();
        m_BoxWidget->TranslationEnabledOn();
        m_BoxWidget->GetHandleProperty()->SetOpacity(0.6);
        m_BoxWidget->GetOutlineProperty()->SetLineWidth(1);
        //        m_BoxWidget->SetHandleSize(0.005);
        //    m_BoxWidget->SetRepresentationToSurface();
    }

    m_BoxWidget->SetInputData(modelElement->GetWholeVtkPolyData());
    m_BoxWidget->PlaceWidget();

    m_BoxWidget->On();
}

void sv4guiModelEdit::ChangeFacetSize()
{
    if(m_Model==NULL || m_Model->GetModelElement(GetTimeStep())==NULL)
        return;

    double facetSize=0;
    QString sizeType="";

    sv4guiModelElementAnalytic* meAnalytic=dynamic_cast<sv4guiModelElementAnalytic*>(m_Model->GetModelElement(GetTimeStep()));
    if(meAnalytic==NULL)
        return;

    if(m_ModelType=="OpenCASCADE")
        sizeType="Max Angle Dev";
    else if(m_ModelType=="Parasolid")
        sizeType="Max Edge Size";

    facetSize=meAnalytic->GetMaxDist();

    if(sizeType=="")
        return;

    QString title="Change Facet "+ sizeType;
    QString title2=sizeType+":";

    bool ok=false;
    double newSize = QInputDialog::getDouble(m_Parent, tr(title.toStdString().c_str()),tr(title2.toStdString().c_str())
                                             , facetSize, 0.01, 100.0, 2, &ok);

    if(!ok)
        return;

    mitk::ProgressBar::GetInstance()->Reset();
    mitk::ProgressBar::GetInstance()->AddStepsToDo(2);
    mitk::StatusBar::GetInstance()->DisplayText("Creating surface with new facet size...");
    mitk::ProgressBar::GetInstance()->Progress();
    WaitCursorOn();

    meAnalytic->SetMaxDist(newSize);
    meAnalytic->SetWholeVtkPolyData(meAnalytic->CreateWholeVtkPolyData());
    std::vector<sv4guiModelElement::svFace*> faces=meAnalytic->GetFaces();
    for(int i=0;i<faces.size();i++)
    {
        faces[i]->vpd=meAnalytic->CreateFaceVtkPolyData(faces[i]->id);
    }
    m_Model->SetDataModified();

    mitk::StatusBar::GetInstance()->DisplayText("New surface has been created with new facet size.");
    mitk::ProgressBar::GetInstance()->Progress();
    WaitCursorOff();

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void sv4guiModelEdit::ConvertToPolyDataModel()
{
    if(m_Model==NULL || m_Model->GetModelElement(GetTimeStep())==NULL)
        return;

    bool ok=false;
    QString newModelName=QInputDialog::getText(m_Parent, "Create PolyData Model", "Model Name:", QLineEdit::Normal, "", &ok);

    if(!ok)
        return;

    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (m_ModelNode);
    if(rs->size()==0)
        return;

    mitk::DataNode::Pointer modelFolderNode=rs->GetElement(0);

    mitk::DataNode::Pointer exitingNode=GetDataStorage()->GetNamedDerivedNode(newModelName.toStdString().c_str(),modelFolderNode);
    if(exitingNode){
        QMessageBox::warning(m_Parent,"Model Already Created","Please use a different model name!");
        return;
    }

    sv4guiModelElement* modelElement=NULL;

    mitk::ProgressBar::GetInstance()->Reset();
    mitk::ProgressBar::GetInstance()->AddStepsToDo(2);
    mitk::StatusBar::GetInstance()->DisplayText("Converting to PolyData model...");
    mitk::ProgressBar::GetInstance()->Progress();
    WaitCursorOn();

    sv4guiModelElementAnalytic* meAnalytic=dynamic_cast<sv4guiModelElementAnalytic*>(m_Model->GetModelElement());
    if(meAnalytic)
        modelElement=meAnalytic->ConverToPolyDataModel();

    mitk::StatusBar::GetInstance()->DisplayText("PolyData model has been created.");
    mitk::ProgressBar::GetInstance()->Progress();
    WaitCursorOff();

    if(modelElement==NULL)
        return;

    sv4guiModel::Pointer solidModel=sv4guiModel::New();
    solidModel->SetDataModified();
    solidModel->SetType(modelElement->GetType());
    solidModel->SetModelElement(modelElement);

    mitk::DataNode::Pointer solidModelNode = mitk::DataNode::New();
    solidModelNode->SetData(solidModel);
    solidModelNode->SetName(newModelName.toStdString());

    GetDataStorage()->Add(solidModelNode,modelFolderNode);
}

void sv4guiModelEdit::SetEstimatedEdgeSize()
{
    double edgeSize=EstimateEdgeSize();

    ui->lineEditEstimateEdgeSize0->setText(QString::number(edgeSize));
    ui->lineEditEstimateEdgeSize1->setText(QString::number(edgeSize));
}


double sv4guiModelEdit::EstimateEdgeSize()
{
    if(m_Model==NULL || m_Model->GetModelElement(GetTimeStep())==NULL)
        return 0;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return 0;

    double edgeSize= sqrt(modelElement->GetMinFaceArea()/3.1415)/2.5;
    edgeSize=round(10000*edgeSize)/10000;

    return edgeSize;
}
