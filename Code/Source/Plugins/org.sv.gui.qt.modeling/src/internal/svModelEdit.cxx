#include "svModelEdit.h"
#include "ui_svModelEdit.h"

#include "svModelUtils.h"
#include "svFaceListDelegate.h"
#include "svPath.h"
#include "svSegmentationUtils.h"

#include "svMeshTetGen.h"
#include "svMitkMesh.h"

#include "cv_polydatasolid_utils.h"

#include <mitkNodePredicateDataType.h>
#include <mitkSurface.h>
#include <mitkUndoController.h>
#include <mitkSliceNavigationController.h>
#include <mitkProgressBar.h>
#include <mitkStatusBar.h>
#include <mitkUnstructuredGrid.h>

#include <usModuleRegistry.h>

#include <vtkProperty.h>

#include <QTreeView>
#include <QStandardItemModel>
#include <QInputDialog>
#include <QColorDialog>
#include <QSignalMapper>
#include <QMessageBox>

#include <iostream>
using namespace std;

const QString svModelEdit::EXTENSION_ID = "org.sv.views.modeling";

svModelEdit::svModelEdit() :
    ui(new Ui::svModelEdit)
{
    m_Model=NULL;
    m_ModelNode=NULL;

    m_SegSelectionWidget=NULL;

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
}

svModelEdit::~svModelEdit()
{
    delete ui;

    if(m_SegSelectionWidget) delete m_SegSelectionWidget;
}

void svModelEdit::CreateQtPartControl( QWidget *parent )
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

    m_SegSelectionWidget=new svSegSelectionWidget();
    m_SegSelectionWidget->move(400,400);
    m_SegSelectionWidget->hide();
    m_SegSelectionWidget->setWindowFlags(Qt::WindowStaysOnTopHint);

    connect(m_SegSelectionWidget,SIGNAL(accepted()), this, SLOT(CreateModel()));

    connect(ui->btnChangeFacet, SIGNAL(clicked()), this, SLOT(ChangeFacetSize()) );
    connect(ui->btnConvert, SIGNAL(clicked()), this, SLOT(ConvertToPolyDataModel()) );

    //for tab Face List
    //=================================================================
    svFaceListDelegate* itemDelegate=new svFaceListDelegate(this);
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
    connect(ui->btnRemeshFaces, SIGNAL(clicked()),signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnExtractFaces, EXTRACT_FACES);
    connect(ui->btnExtractFaces, SIGNAL(clicked()),signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnFillHoles, FILL_HOLES);
    connect(ui->btnFillHoles, SIGNAL(clicked()),signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnSelectLargestConnected, SELECT_LARGEST_CONNECTED);
    connect(ui->btnSelectLargestConnected, SIGNAL(clicked()),signalMapper, SLOT(map()));

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

}

void svModelEdit::Visible()
{
    ui->tabWidget->setCurrentIndex(0);
    OnSelectionChanged(GetDataManagerSelection());
}

void svModelEdit::Hidden()
{
    //    ClearAll();
    RemoveObservers();
}

int svModelEdit::GetTimeStep()
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

void svModelEdit::OnSelectionChanged(std::vector<mitk::DataNode*> nodes )
{
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
    svModel* model=dynamic_cast<svModel*>(selectedNode->GetData());

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

void svModelEdit::UpdateGUI()
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

    if(m_ModelType=="OpenCASCADE")
        ui->widgetOCC->show();
    else
        ui->widgetOCC->hide();

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

void svModelEdit::UpdatePathListForTrim()
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

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (m_ModelNode,isProjFolder,false);

    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        rs=GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svPathFolder"));
        if (rs->size()>0)
        {
            m_PathFolderNode=rs->GetElement(0);

            rs=GetDataStorage()->GetDerivations(rs->GetElement(0),mitk::NodePredicateDataType::New("svPath"));

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

void svModelEdit::SetupSliderPathPlane(int idx)
{
    if(m_PathFolderNode.IsNull())
        return;

    QString selectedPathName=ui->comboBoxPathPlane->currentText();

    mitk::DataNode::Pointer pathNode=GetDataStorage()->GetNamedDerivedNode (selectedPathName.toStdString().c_str(), m_PathFolderNode);
    if(pathNode.IsNull())
        return;

    svPath* path=dynamic_cast<svPath*>(pathNode->GetData());
    if(path==NULL)
        return;

    svPathElement* pe=path->GetPathElement(GetTimeStep());
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

void svModelEdit::UpdatePlaneWidget(double idx)
{
    if(m_PlaneWidget==NULL || !m_PlaneWidget->GetEnabled())
        return;

    if(m_PathFolderNode.IsNull())
        return;

    QString selectedPathName=ui->comboBoxPathPlane->currentText();

    mitk::DataNode::Pointer pathNode=GetDataStorage()->GetNamedDerivedNode (selectedPathName.toStdString().c_str(), m_PathFolderNode);
    if(pathNode.IsNull())
        return;

    svPath* path=dynamic_cast<svPath*>(pathNode->GetData());
    if(path==NULL)
        return;

    svPathElement* pe=path->GetPathElement(GetTimeStep());
    if(pe==NULL)
        return;

    int posIdx=idx;

    svPathElement::svPathPoint pathPoint=pe->GetPathPoint(posIdx);

    m_PlaneWidget->SetCenter(pathPoint.pos[0],pathPoint.pos[1],pathPoint.pos[2]);
    m_PlaneWidget->SetNormal(pathPoint.tangent[0],pathPoint.tangent[1],pathPoint.tangent[2]);
    m_PlaneWidget->UpdatePlacement();
    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void svModelEdit::SetupSliderPathBox(int idx)
{
    if(m_PathFolderNode.IsNull())
        return;

    QString selectedPathName=ui->comboBoxPathBox->currentText();

    mitk::DataNode::Pointer pathNode=GetDataStorage()->GetNamedDerivedNode (selectedPathName.toStdString().c_str(), m_PathFolderNode);
    if(pathNode.IsNull())
        return;

    svPath* path=dynamic_cast<svPath*>(pathNode->GetData());
    if(path==NULL)
        return;

    svPathElement* pe=path->GetPathElement(GetTimeStep());
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

void svModelEdit::UpdateBoxWidget(double idx)
{
    if(m_BoxWidget==NULL || !m_BoxWidget->GetEnabled())
        return;

    if(m_PathFolderNode.IsNull())
        return;

    QString selectedPathName=ui->comboBoxPathBox->currentText();

    mitk::DataNode::Pointer pathNode=GetDataStorage()->GetNamedDerivedNode (selectedPathName.toStdString().c_str(), m_PathFolderNode);
    if(pathNode.IsNull())
        return;

    svPath* path=dynamic_cast<svPath*>(pathNode->GetData());
    if(path==NULL)
        return;

    svPathElement* pe=path->GetPathElement(GetTimeStep());
    if(pe==NULL)
        return;

    int posIdx=idx;

    svPathElement::svPathPoint pathPoint=pe->GetPathPoint(posIdx);

    m_BoxWidget->PlaceWidget (-3, 3, -3, 3, -3, 3);
    m_BoxWidget->SetTransform(svSegmentationUtils::GetvtkTransformBox(pathPoint,6));

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void svModelEdit::UpdateFaceListSelection()
{
    if(!m_Model) return;
    svModelElement* modelElement=m_Model->GetModelElement();
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

void svModelEdit::SetupFaceListTable()
{
    if(!m_Model)
        return;

    m_OperatingWholeTableModel=true;
    m_FaceListTableModel->clear();

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    std::vector<svModelElement::svFace*> faces=modelElement->GetFaces();

    QStringList faceListHeaders;
    faceListHeaders << "ID" << "Name" << "Type" << "V" << "C" << "O";
    m_FaceListTableModel->setHorizontalHeaderLabels(faceListHeaders);
    m_FaceListTableModel->setColumnCount(6);

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

    m_OperatingWholeTableModel=false;
}

void svModelEdit::UpdateFaceData(QStandardItem* item)
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    int row=item->index().row();
    int col=item->index().column();

    QStandardItem* itemID= m_FaceListTableModel->item(row,0);
    int id=itemID->text().toInt();

    svModelElement::svFace* face=modelElement->GetFace(id);

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

void svModelEdit::TableFaceListSelectionChanged( const QItemSelection & /*selected*/, const QItemSelection & /*deselected*/ )
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    if(m_FaceListTableModel==NULL)
        return;

    QModelIndexList indexesOfSelectedRows = ui->tableViewFaceList->selectionModel()->selectedRows();

    modelElement->ClearFaceSelection();

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();

        QStandardItem* itemID= m_FaceListTableModel->item(row,0);
        int id=itemID->text().toInt();

        modelElement->SelectFace(id);
    }

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void svModelEdit::ToggleVisibility(const QModelIndex &index){

    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    int row=index.row();
    int col=index.column();

    if(col!=3)
        return;

    QStandardItem* itemID= m_FaceListTableModel->item(row,0);
    int id=itemID->text().toInt();

    svModelElement::svFace* face=modelElement->GetFace(id);

    if(face==NULL)
        return;

    QStandardItem* itemV= m_FaceListTableModel->item(row,col);

    if(face->visible)
    {
        face->visible=false;
        itemV->setIcon(QIcon(":/hide.png"));
        m_Model->SetDataModified();
    }
    else
    {
        face->visible=true;
        itemV->setIcon(QIcon(":/show.png"));
        m_Model->SetDataModified();
    }

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();

}

void svModelEdit::ChangeColor(const QModelIndex &index)
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    int row=index.row();
    int col=index.column();

    if(col!=4)
        return;

    QStandardItem* itemID= m_FaceListTableModel->item(row,0);
    int id=itemID->text().toInt();

    svModelElement::svFace* face=modelElement->GetFace(id);

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

void svModelEdit::TableViewFaceListContextMenuRequested( const QPoint & pos )
{
    m_FaceListTableMenu->popup(QCursor::pos());
}

void svModelEdit::ShowSelected( bool )
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
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
        svModelElement::svFace* face=modelElement->GetFace(id);

        if(face==NULL)
            continue;

        face->visible=true;
        m_Model->SetDataModified();

        QStandardItem* itemV= m_FaceListTableModel->item(row,3);
        itemV->setIcon(QIcon(":/show.png"));
    }
}

void svModelEdit::HideSelected( bool )
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
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
        svModelElement::svFace* face=modelElement->GetFace(id);

        if(face==NULL)
            continue;

        face->visible=false;
        m_Model->SetDataModified();

        QStandardItem* itemV= m_FaceListTableModel->item(row,3);
        itemV->setIcon(QIcon(":/hide.png"));
    }
}

void svModelEdit::ChangeOpacitySelected( bool )
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
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
        svModelElement::svFace* face=modelElement->GetFace(id);

        if(face==NULL)
            continue;

        //        face->opacity=opacity; //done by UpateFaceData()

        QStandardItem* itemO= m_FaceListTableModel->item(row,5);
        itemO->setData((int)(opacity*100)/100.0, Qt::EditRole);
    }
}

void svModelEdit::ChangeColorSelected( bool )
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
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
        svModelElement::svFace* face=modelElement->GetFace(id);

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

void svModelEdit::ChangeTypeSelected( bool )
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
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
        svModelElement::svFace* face=modelElement->GetFace(id);

        if(face==NULL)
            continue;

        //        face->type=type; //done by UpateFaceData()

        QStandardItem* itemT= m_FaceListTableModel->item(row,2);
        itemT->setData(type, Qt::EditRole);
    }
}

void svModelEdit::UpdateBlendTable(int index)
{
    if(index!=1)
        return;

    //    SetupBlendTable();
}

void svModelEdit::SetupBlendTable()
{
    if(!m_Model)
        return;

    m_BlendTableModel->clear();

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    std::vector<svModelElement::svFace*> faces=modelElement->GetFaces();

    QStringList blendHeaders;
    blendHeaders << "Use" << "Face 1" << "Face 2" << "Radius";
    m_BlendTableModel->setHorizontalHeaderLabels(blendHeaders);
    m_BlendTableModel->setColumnCount(4);

    int rowIndex=-1;

    for(int i=0;i<faces.size();i++)
    {
        if(faces[i]==NULL || faces[i]->type=="cap" || faces[i]->type=="inlet" || faces[i]->type=="outlet")
            continue;

        for(int j=i+1;j<faces.size();j++)
        {
            if(faces[j]==NULL || faces[j]->type=="cap" || faces[j]->type=="inlet" || faces[j]->type=="outlet")
                continue;

            //To do: check if two faces are adjcent;
            //Todo: create linking list in svModelElementPolyData, avoiding to create multiple times in the plugin
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
            svModelElement::svBlendParamRadius* blendParam= modelElement->GetBlendParamRadius(faces[i]->id, faces[j]->id);

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
}

void svModelEdit::UpdatePolyDataBlendParam()
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    svModelElementPolyData* mepd=dynamic_cast<svModelElementPolyData*>(m_Model->GetModelElement(timeStep));
    if(mepd==NULL) return;

    svModelElementPolyData::svBlendParam* param=mepd->GetBlendParam();

    ui->dsbDecimation->setValue(param->targetdecimation);
    ui->sbBlendIters->setValue(param->numblenditers);
    ui->sbSubBlendIters->setValue(param->numsubblenditers);
    ui->sbCstrSmoothIters->setValue(param->numcgsmoothiters);
    ui->sbLapSmoothIters->setValue(param->numlapsmoothiters);
    ui->sbSubdivisionIters->setValue(param->numsubdivisioniters);
}

void svModelEdit::TableBlendSelectionChanged( const QItemSelection & /*selected*/, const QItemSelection & /*deselected*/ )
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
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

void svModelEdit::TableViewBlendContextMenuRequested( const QPoint & pos )
{
    m_BlendTableMenu->popup(QCursor::pos());
}

void svModelEdit::SetRadius(bool)
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

void svModelEdit::ClearRadius(bool)
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

void svModelEdit::UseSelectedBlend(bool)
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

void svModelEdit::NotUseSelectedBlend(bool)
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

void svModelEdit::NodeChanged(const mitk::DataNode* node)
{
    if(m_ModelNode==node)
        ui->labelModelName->setText(QString::fromStdString(m_ModelNode->GetName()));
}

void svModelEdit::NodeAdded(const mitk::DataNode* node)
{
}

void svModelEdit::NodeRemoved(const mitk::DataNode* node)
{
}

void svModelEdit::AddObservers()
{
    if(m_ModelNode.IsNotNull())
    {
        if(m_ModelNode->GetDataInteractor().IsNull())
        {
            m_DataInteractor = svModelDataInteractor::New();
            m_DataInteractor->LoadStateMachine("svModelInteraction.xml", us::ModuleRegistry::GetModule("svModel"));
            m_DataInteractor->SetEventConfig("svModelConfig.xml", us::ModuleRegistry::GetModule("svModel"));
            m_DataInteractor->SetDataNode(m_ModelNode);
        }
        m_ModelNode->SetStringProperty("interactor user","modeling");
        svModelDataInteractor* interactor=dynamic_cast<svModelDataInteractor*>(m_ModelNode->GetDataInteractor().GetPointer());
        if(interactor)
            interactor->SetFaceSelectionOnly(false);
    }

    if(m_ModelSelectFaceObserverTag==-1)
    {
        itk::SimpleMemberCommand<svModelEdit>::Pointer modelSelectFaceCommand = itk::SimpleMemberCommand<svModelEdit>::New();
        modelSelectFaceCommand->SetCallbackFunction(this, &svModelEdit::UpdateFaceListSelection);
        m_ModelSelectFaceObserverTag = m_Model->AddObserver( svModelSelectFaceEvent(), modelSelectFaceCommand);
    }

    if(m_ModelUpdateObserverTag==-1)
    {
        itk::SimpleMemberCommand<svModelEdit>::Pointer modelUpdateCommand = itk::SimpleMemberCommand<svModelEdit>::New();
        modelUpdateCommand->SetCallbackFunction(this, &svModelEdit::UpdateGUI);
        m_ModelUpdateObserverTag = m_Model->AddObserver( svModelSetEvent(), modelUpdateCommand);
    }
}

void svModelEdit::RemoveObservers()
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

void svModelEdit::ClearAll()
{
    m_Model=NULL;
    m_ModelNode=NULL;

    ui->labelModelName->setText("");
    ui->labelModelType->setText("");
}

void svModelEdit::ShowSegSelectionWidget()
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSubset(isProjFolder);

    if(rs->size()<1) return;

    mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

    rs=GetDataStorage()->GetDerivations (projFolderNode,mitk::NodePredicateDataType::New("svSegmentationFolder"));
    if(rs->size()<1) return;

    mitk::DataNode::Pointer segFolderNode=rs->GetElement(0);
    rs=GetDataStorage()->GetDerivations(segFolderNode);
    if(rs->size()<1) return;

    std::vector<mitk::DataNode::Pointer> segNodes;
    for(int i=0;i<rs->size();i++)
        segNodes.push_back(rs->GetElement(i));


    m_SegSelectionWidget->SetTableView(segNodes,modelElement,m_ModelType);
    m_SegSelectionWidget->show();
}

void svModelEdit::CreateModel()
{
    std::vector<std::string> segNames=m_SegSelectionWidget->GetUsedSegNames();
    int numSampling=m_SegSelectionWidget->GetNumSampling();

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSubset(isProjFolder);

    if(rs->size()<1) return;

    mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

    rs=GetDataStorage()->GetDerivations (projFolderNode,mitk::NodePredicateDataType::New("svSegmentationFolder"));
    if(rs->size()<1) return;

    mitk::DataNode::Pointer segFolderNode=rs->GetElement(0);

    std::vector<mitk::DataNode::Pointer> segNodes;

    for(int i=0;i<segNames.size();i++)
    {
        mitk::DataNode::Pointer node=GetDataStorage()->GetNamedDerivedNode(segNames[i].c_str(),segFolderNode);
        if(node.IsNotNull())
            segNodes.push_back(node);
    }

    svModelElement* newModelElement=NULL;
    svModelElement* modelElement=m_Model->GetModelElement();

    mitk::ProgressBar::GetInstance()->Reset();
    mitk::ProgressBar::GetInstance()->AddStepsToDo(3);
    mitk::StatusBar::GetInstance()->DisplayText("Creating model...");
    mitk::ProgressBar::GetInstance()->Progress();
    WaitCursorOn();

    QString statusText="Model has been created.";
    if(m_ModelType=="PolyData"){
        int stats[2]={0};
        newModelElement=svModelUtils::CreateModelElementPolyData(segNodes,numSampling,stats);
        if(newModelElement==NULL)
        {
            statusText="Failed to create model.";
        }
        else
        {
            statusText=statusText+" Number of Free Edges: "+ QString::number(stats[0])+", Number of Bad Edges: "+ QString::number(stats[1]);
        }
    }
#ifdef SV_USE_OpenCASCADE
    else if(m_ModelType=="OpenCASCADE")
    {
        newModelElement=svModelUtils::CreateModelElementOCCT(segNodes,numSampling);
        if(newModelElement==NULL)
        {
            statusText="Failed to create model.";
        }
    }
#endif

    WaitCursorOff();
    mitk::ProgressBar::GetInstance()->Progress(2);
    mitk::StatusBar::GetInstance()->DisplayText(statusText.toStdString().c_str());

    if(newModelElement!=NULL)
    {
        int timeStep=GetTimeStep();

        mitk::OperationEvent::IncCurrObjectEventId();
        svModelOperation* doOp = new svModelOperation(svModelOperation::OpSETMODELELEMENT,timeStep,newModelElement);
        svModelOperation* undoOp = new svModelOperation(svModelOperation::OpSETMODELELEMENT,timeStep,modelElement);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Model, doOp, undoOp, "Set ModelElement");
        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

        m_Model->ExecuteOperation(doOp);

//        UpdateGUI();
    }
}

std::vector<svModelElement::svBlendParamRadius*> svModelEdit::GetBlendRadii()
{
    std::vector<svModelElement::svBlendParamRadius*> blendRadii;

    if(!m_Model)
        return blendRadii;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
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

        blendRadii.push_back(new svModelElement::svBlendParamRadius(faceID1,faceID2,faceName1,faceName2,radius));
    }

    return blendRadii;
}

void svModelEdit::BlendModel()
{
    if(m_Model==NULL) return;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);

    if(modelElement==NULL) return;

    svModelElement* newModelElement=NULL;

    std::vector<svModelElement::svBlendParamRadius*> blendRadii=GetBlendRadii();

    mitk::ProgressBar::GetInstance()->Reset();
    mitk::ProgressBar::GetInstance()->AddStepsToDo(2);
    mitk::StatusBar::GetInstance()->DisplayText("Blending model...");
    mitk::ProgressBar::GetInstance()->Progress();
    WaitCursorOn();

    if(m_ModelType=="PolyData"){

        svModelElementPolyData* mepd=dynamic_cast<svModelElementPolyData*>(modelElement);
        if(!mepd) return;

        svModelElementPolyData::svBlendParam* param=new svModelElementPolyData::svBlendParam();

        param->numblenditers=ui->sbBlendIters->value();
        param->numsubblenditers=ui->sbSubBlendIters->value();
        param->numcgsmoothiters=ui->sbCstrSmoothIters->value();
        param->numlapsmoothiters=ui->sbLapSmoothIters->value();
        param->numsubdivisioniters=ui->sbSubdivisionIters->value();
        param->targetdecimation=ui->dsbDecimation->value();

        newModelElement=svModelUtils::CreateModelElementPolyDataByBlend(mepd, blendRadii, param);
    }
#ifdef SV_USE_OpenCASCADE
    else if(m_ModelType=="OpenCASCADE")
    {
        svModelElementOCCT* meocct=dynamic_cast<svModelElementOCCT*>(modelElement);
        if(!meocct) return;

        newModelElement=svModelUtils::CreateModelElementOCCTByBlend(meocct, blendRadii);
    }
#endif

    WaitCursorOff();
    mitk::ProgressBar::GetInstance()->Progress();
    mitk::StatusBar::GetInstance()->DisplayText("Blending done.");

    if(newModelElement==NULL) return;

    mitk::OperationEvent::IncCurrObjectEventId();

    svModelOperation* doOp = new svModelOperation(svModelOperation::OpSETMODELELEMENT,timeStep,newModelElement);
    svModelOperation* undoOp = new svModelOperation(svModelOperation::OpSETMODELELEMENT,timeStep,modelElement);
    mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Model, doOp, undoOp, "Set ModelElement");
    mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

    m_Model->ExecuteOperation(doOp);

    //    SetupFaceListTable();

    //    UpdateFaceListSelection();

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

std::vector<int> svModelEdit::GetSelectedFaceIDs()
{
    std::vector<int> faceIDs;

    if(!m_Model)
        return faceIDs;

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
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

bool svModelEdit::MarkCells(svModelElementPolyData* modelElement)
{
    modelElement->RemoveActiveCells();

    bool hasFaces=false;
    if(modelElement->GetSelectedFaceIDs().size()>0)
    {
        hasFaces=true;
        if(!modelElement->MarkCellsByFaces(modelElement->GetSelectedFaceIDs()))
            return false;
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

void svModelEdit::ModelOperate(int operationType)
{
    if(m_Model==NULL) return;

    int timeStep=GetTimeStep();
    svModelElementPolyData* modelElement=dynamic_cast<svModelElementPolyData*>(m_Model->GetModelElement(timeStep));

    if(modelElement==NULL) return;

    svModelElementPolyData* newModelElement=modelElement->Clone();

    bool ok=false;

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
        ok=newModelElement->RemeshFaces(GetSelectedFaceIDs(),ui->dsbRemeshSize->value());
        break;
    case EXTRACT_FACES:
        ok=newModelElement->ExtractFaces(ui->sbSeparationAngle->value());
        break;
    case FILL_HOLES:
        ok=newModelElement->FillHoles();
        break;
    case SELECT_LARGEST_CONNECTED:
        ok=newModelElement->SelectLargestConnectedRegion();
        break;
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

    svModelOperation* doOp = new svModelOperation(svModelOperation::OpSETMODELELEMENT,timeStep,newModelElement);
    svModelOperation* undoOp = new svModelOperation(svModelOperation::OpSETMODELELEMENT,timeStep,modelElement);
    mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Model, doOp, undoOp, "Set ModelElement");
    mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

    m_Model->ExecuteOperation(doOp);

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();

    mitk::StatusBar::GetInstance()->DisplayText("Model processing done");
}

void svModelEdit::ShowSphereInteractor(bool checked)
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
    svModelElementPolyData* modelElement=dynamic_cast<svModelElementPolyData*>(m_Model->GetModelElement(timeStep));

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

void svModelEdit::ShowPlaneInteractor(bool checked)
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
    svModelElementPolyData* modelElement=dynamic_cast<svModelElementPolyData*>(m_Model->GetModelElement(timeStep));

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

void svModelEdit::ShowBoxInteractor(bool checked)
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
    svModelElementPolyData* modelElement=dynamic_cast<svModelElementPolyData*>(m_Model->GetModelElement(timeStep));

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

void svModelEdit::ChangeFacetSize()
{
    if(m_Model==NULL || m_Model->GetModelElement(GetTimeStep())==NULL)
        return;

    double facetSize=0;
    QString sizeType="";

#ifdef SV_USE_OpenCASCADE
    if(m_ModelType=="OpenCASCADE")
    {
        svModelElementOCCT* meocct=dynamic_cast<svModelElementOCCT*>(m_Model->GetModelElement(GetTimeStep()));
        if(meocct==NULL)
            return;

        sizeType="Max Angle Dev";
        facetSize=meocct->GetMaxDist();

    }
#endif

    if(m_ModelType=="Parasolid")
    {
//        svModelElementParasolid* meps=dynamic_cast<svModelElementParasolid*>(m_Model->GetModelElement(GetTimeStep()));
//        if(meps==NULL)
//            return;

//        sizeType="Max Edge Size";
//        faceSize=meps->GetMaxDist();
    }

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

#ifdef SV_USE_OpenCASCADE
    if(m_ModelType=="OpenCASCADE")
    {
        svModelElementOCCT* meocct=dynamic_cast<svModelElementOCCT*>(m_Model->GetModelElement(GetTimeStep()));
        meocct->SetMaxDist(newSize);
        meocct->SetWholeVtkPolyData(meocct->CreateWholeVtkPolyData());
        std::vector<svModelElement::svFace*> faces=meocct->GetFaces();
        for(int i=0;i<faces.size();i++)
        {
            faces[i]->vpd=meocct->CreateFaceVtkPolyData(faces[i]->id);
        }
        m_Model->SetDataModified();
    }
#endif

    if(m_ModelType=="Parasolid")
    {
    }

    mitk::StatusBar::GetInstance()->DisplayText("New surface has been created with new facet size.");
    mitk::ProgressBar::GetInstance()->Progress();
    WaitCursorOff();

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void svModelEdit::ConvertToPolyDataModel()
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

    svModelElement* modelElement=NULL;

    mitk::ProgressBar::GetInstance()->Reset();
    mitk::ProgressBar::GetInstance()->AddStepsToDo(2);
    mitk::StatusBar::GetInstance()->DisplayText("Converting to PolyData model...");
    mitk::ProgressBar::GetInstance()->Progress();
    WaitCursorOn();

#ifdef SV_USE_OpenCASCADE
    if(m_ModelType=="OpenCASCADE")
    {
        svModelElementOCCT* meocct=dynamic_cast<svModelElementOCCT*>(m_Model->GetModelElement());
        if(meocct)
        {
            svModelElementPolyData* mepd=new svModelElementPolyData();
            mepd->SetSegNames(meocct->GetSegNames());

            vtkSmartPointer<vtkPolyData> wholevpd=NULL;
            if(meocct->GetWholeVtkPolyData())
            {
                wholevpd=vtkSmartPointer<vtkPolyData>::New();
                wholevpd->DeepCopy(meocct->GetWholeVtkPolyData());
            }
            mepd->SetWholeVtkPolyData(wholevpd);

            std::vector<svModelElement::svFace*> faces;
            std::vector<svModelElement::svFace*> oldFaces=meocct->GetFaces();
            for(int i=0;i<oldFaces.size();i++)
            {
                if(oldFaces[i])
                {
                    svModelElement::svFace* face=new svModelElement::svFace(*(oldFaces[i]),false);

                    vtkSmartPointer<vtkPolyData> facevpd=NULL;
                    if(oldFaces[i]->vpd)
                    {
                        facevpd=vtkSmartPointer<vtkPolyData>::New();
                        facevpd->DeepCopy(oldFaces[i]->vpd);
                    }
                    face->vpd=facevpd;
                    faces.push_back(face);
                }
            }
            mepd->SetFaces(faces);
            modelElement=mepd;
        }
    }
#endif

    mitk::StatusBar::GetInstance()->DisplayText("PolyData model has been created.");
    mitk::ProgressBar::GetInstance()->Progress();
    WaitCursorOff();

    if(modelElement==NULL)
        return;

    svModel::Pointer solidModel=svModel::New();
    solidModel->SetDataModified();
    solidModel->SetType(modelElement->GetType());
    solidModel->SetModelElement(modelElement);

    mitk::DataNode::Pointer solidModelNode = mitk::DataNode::New();
    solidModelNode->SetData(solidModel);
    solidModelNode->SetName(newModelName.toStdString());

    GetDataStorage()->Add(solidModelNode,modelFolderNode);
}
