#include "svModelEdit.h"
#include "ui_svModelEdit.h"

#include "svModel.h"
#include "svModelUtils.h"
#include "svModelElementPolyData.h"

#include "svFaceListDelegate.h"

#include "cv_polydatasolid_utils.h"

#include <mitkNodePredicateDataType.h>
#include <mitkSurface.h>
#include <mitkUndoController.h>
#include <mitkSliceNavigationController.h>

#include <usModuleRegistry.h>

#include <QTreeView>
#include <QStandardItemModel>
#include <QInputDialog>
#include <QColorDialog>
#include <QSignalMapper>

#include <iostream>
using namespace std;

const QString svModelEdit::EXTENSION_ID = "org.sv.views.modeledit";

svModelEdit::svModelEdit() :
    ui(new Ui::svModelEdit)
{
    m_Model=NULL;
    m_ModelNode=NULL;

    m_SegSelectionWidget=NULL;

    m_ModelSelectFaceObserverTag=0;
    m_ModelUpdateObserverTag=0;

    m_BlendTableMenu=NULL;
    m_BlendTableModel=NULL;
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

    //for tab Face List
    //=================================================================
    svFaceListDelegate* itemDelegate=new svFaceListDelegate(this);
    m_FaceListTableModel = new QStandardItemModel(this);
    ui->tableViewFaceList->setModel(m_FaceListTableModel);
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
    QAction* changeColorAction=m_FaceListTableMenu->addAction("Change Color");
    QAction* changeOpacityAction=m_FaceListTableMenu->addAction("Change Opacity");

    connect( showAction, SIGNAL( triggered(bool) ) , this, SLOT( ShowSelected(bool) ) );
    connect( hideAction, SIGNAL( triggered(bool) ) , this, SLOT( HideSelected(bool) ) );
    connect( changeColorAction, SIGNAL( triggered(bool) ) , this, SLOT( ChangeColorSelected(bool) ) );
    connect( changeOpacityAction, SIGNAL( triggered(bool) ) , this, SLOT( ChangeOpacitySelected(bool) ) );

    connect( ui->tableViewFaceList, SIGNAL(customContextMenuRequested(const QPoint&))
      , this, SLOT(TableViewFaceListContextMenuRequested(const QPoint&)) );

    //face ops
    //-----------------------------------------------------------------
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


    connect(signalMapper, SIGNAL(mapped(int)), this, SLOT(ModelOperate(int)));

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
    ClearAll();
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
        ClearAll();
        m_Parent->setEnabled(false);
        return;
    }

    mitk::DataNode::Pointer modelNode=nodes.front();

//    if(m_ModelNode==modelNode)
//    {
////        return;
//    }

    ClearAll();

    m_ModelNode=modelNode;
    m_Model=dynamic_cast<svModel*>(modelNode->GetData());
    if(!m_Model)
    {
        ClearAll();
        m_Parent->setEnabled(false);
        return;
    }

    m_Parent->setEnabled(true);
    ui->tabWidget->setCurrentIndex(0);

    m_ModelType=m_Model->GetType();

    UpdateGUI();

    m_DataInteractor = svModelDataInteractor::New();
    m_DataInteractor->LoadStateMachine("svModelInteraction.xml", us::ModuleRegistry::GetModule("svModel"));
    m_DataInteractor->SetEventConfig("svModelConfig.xml", us::ModuleRegistry::GetModule("svModel"));
    m_DataInteractor->SetDataNode(m_ModelNode);

    //Add Observers
    itk::SimpleMemberCommand<svModelEdit>::Pointer modelSelectFaceCommand = itk::SimpleMemberCommand<svModelEdit>::New();
    modelSelectFaceCommand->SetCallbackFunction(this, &svModelEdit::UpdateFaceListSelection);
    m_ModelSelectFaceObserverTag = m_Model->AddObserver( svModelSelectFaceEvent(), modelSelectFaceCommand);

    itk::SimpleMemberCommand<svModelEdit>::Pointer modelUpdateCommand = itk::SimpleMemberCommand<svModelEdit>::New();
    modelUpdateCommand->SetCallbackFunction(this, &svModelEdit::UpdateGUI);
    m_ModelUpdateObserverTag = m_Model->AddObserver( svModelSetEvent(), modelUpdateCommand);

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void svModelEdit::UpdateGUI()
{
    //update top part
    //------------------------------------------------------------------------
    ui->labelModelName->setText(QString::fromStdString(m_ModelNode->GetName()));
    ui->labelModelType->setText(QString::fromStdString(m_ModelType));

    if(m_ModelType=="Parasolid" || m_ModelType=="OpenCASCADE")
        ui->btnConvert->show();
    else
        ui->btnConvert->hide();

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

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    std::vector<svModelElement::svFace*> faces=modelElement->GetFaces();

    m_FaceListTableModel->clear();
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
//        item->setEditable(false);
        m_FaceListTableModel->setItem(rowIndex, 1, item);

        item= new QStandardItem(QString::fromStdString(faces[i]->type));
        item->setEditable(false);
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
    ui->tableViewFaceList->horizontalHeader()->resizeSection(2,40);
    ui->tableViewFaceList->horizontalHeader()->setSectionResizeMode(3, QHeaderView::Fixed);
    ui->tableViewFaceList->horizontalHeader()->resizeSection(3,30);
    ui->tableViewFaceList->horizontalHeader()->setSectionResizeMode(4, QHeaderView::Fixed);
    ui->tableViewFaceList->horizontalHeader()->resizeSection(4,30);
    ui->tableViewFaceList->horizontalHeader()->setSectionResizeMode(5, QHeaderView::Fixed);
    ui->tableViewFaceList->horizontalHeader()->resizeSection(5,60);

    ui->tableViewFaceList->setColumnHidden(0,true);
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
    }else if(col==5){
        face->opacity=item->text().trimmed().toFloat();
    }

    if(col!=1)
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

        modelElement->SetSelectedFace(id);
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
    }
    else
    {
        face->visible=true;
        itemV->setIcon(QIcon(":/show.png"));
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

        QStandardItem* itemC= m_FaceListTableModel->item(row,4);
        QBrush brush(newColor);
        itemC->setBackground(brush);
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

    int timeStep=GetTimeStep();
    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL) return;

    std::vector<svModelElement::svFace*> faces=modelElement->GetFaces();

    m_BlendTableModel->clear();
    QStringList blendHeaders;
    blendHeaders << "Use" << "Face 1" << "Face 2" << "Radius";
    m_BlendTableModel->setHorizontalHeaderLabels(blendHeaders);
    m_BlendTableModel->setColumnCount(4);

    int rowIndex=-1;

    for(int i=0;i<faces.size();i++)
    {
        if(faces[i]==NULL || faces[i]->type=="cap")
            continue;

        for(int j=i+1;j<faces.size();j++)
        {
            if(faces[j]==NULL || faces[j]->type=="cap")
                continue;

            //To do: check if two faces are adjcent;

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

        modelElement->SetSelectedFace(itemFace1->text().toStdString());
        modelElement->SetSelectedFace(itemFace2->text().toStdString());
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

void svModelEdit::ClearAll()
{
    if(m_Model && m_ModelSelectFaceObserverTag)
    {
        m_Model->RemoveObserver(m_ModelSelectFaceObserverTag);
    }

    if(m_Model && m_ModelUpdateObserverTag)
    {
        m_Model->RemoveObserver(m_ModelUpdateObserverTag);
    }

    if(m_ModelNode)
    {
        m_ModelNode->SetDataInteractor(NULL);
        m_DataInteractor=NULL;
    }

    m_Model=NULL;
    m_ModelNode=NULL;

    ui->labelModelName->setText("");
    ui->labelModelType->setText("");
//    ui->listWidget->clear();
//    ui->plainTextEditBlend->clear();
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


    m_SegSelectionWidget->SetTableView(segNodes,modelElement);
    m_SegSelectionWidget->show();
}

void svModelEdit::CreateModel()
{
    std::vector<std::string> segNames=m_SegSelectionWidget->GetUsedSegNames();

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

    if(m_ModelType=="PolyData"){
        newModelElement=svModelUtils::CreateModelElementPolyData(segNodes);
    }
    else if(m_ModelType=="Parasolid")
    {

    }

    int timeStep=GetTimeStep();

    mitk::OperationEvent::IncCurrObjectEventId();
    svModelOperation* doOp = new svModelOperation(svModelOperation::OpSETMODELELEMENT,timeStep,newModelElement);
    svModelOperation* undoOp = new svModelOperation(svModelOperation::OpSETMODELELEMENT,timeStep,modelElement);
    mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Model, doOp, undoOp, "Set ModelElement");
    mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

    m_Model->ExecuteOperation(doOp);

    UpdateGUI();
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

        int faceID1=modelElement->GetFaceID(itemFace1->text().toStdString());
        int faceID2=modelElement->GetFaceID(itemFace2->text().toStdString());

        blendRadii.push_back(new svModelElement::svBlendParamRadius(faceID1,faceID2,radius));
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
    else if(m_ModelType=="Parasolid")
    {

    }

    if(newModelElement==NULL) return;

    mitk::OperationEvent::IncCurrObjectEventId();

    svModelOperation* doOp = new svModelOperation(svModelOperation::OpSETMODELELEMENT,timeStep,newModelElement);
    svModelOperation* undoOp = new svModelOperation(svModelOperation::OpSETMODELELEMENT,timeStep,modelElement);
    mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Model, doOp, undoOp, "Set ModelElement");
    mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

    m_Model->ExecuteOperation(doOp);

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

void svModelEdit::ModelOperate(int operationType)
{
    if(m_Model==NULL) return;

    int timeStep=GetTimeStep();
    svModelElementPolyData* modelElement=dynamic_cast<svModelElementPolyData*>(m_Model->GetModelElement(timeStep));

    if(modelElement==NULL) return;

    svModelElementPolyData* newModelElement=modelElement->Clone();

    bool ok=false;

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
    default:
        break;
    }

    if(!ok)
    {
        delete newModelElement;
        return;
    }

    mitk::OperationEvent::IncCurrObjectEventId();

    svModelOperation* doOp = new svModelOperation(svModelOperation::OpSETMODELELEMENT,timeStep,newModelElement);
    svModelOperation* undoOp = new svModelOperation(svModelOperation::OpSETMODELELEMENT,timeStep,modelElement);
    mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Model, doOp, undoOp, "Set ModelElement by Combining Faces");
    mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

    m_Model->ExecuteOperation(doOp);

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();

}
