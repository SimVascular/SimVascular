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

#include <vtkIdList.h>
#include <vtkNew.h>
#include <vtkProperty.h>
#include <vtkVertexGlyphFilter.h>

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

//------------------
// sv4guiModelEdit
//------------------
//
sv4guiModelEdit::sv4guiModelEdit() : ui(new Ui::sv4guiModelEdit)
{
    m_Model=nullptr;
    m_ModelNode=nullptr;

    m_SegSelectionWidget=nullptr;
    m_CapSelectionWidget=nullptr;

    m_DataInteractor=nullptr;
    m_ModelSelectFaceObserverTag=-1;
    m_ModelUpdateObserverTag=-1;

    m_BlendTableMenu=nullptr;
    m_BlendTableModel=nullptr;

    m_SphereWidget=nullptr;
    m_PlaneWidget=nullptr;
    m_BoxWidget=nullptr;

    m_PathFolderNode=nullptr;

    m_OperatingWholeTableModel=false;

    m_LocalOperationforBlendRegion=false;
}

sv4guiModelEdit::~sv4guiModelEdit()
{
    delete ui;

    if(m_SegSelectionWidget) delete m_SegSelectionWidget;
    if(m_CapSelectionWidget) delete m_CapSelectionWidget;
}

//----------------------
// CreateQtPartControl
//----------------------
//
void sv4guiModelEdit::CreateQtPartControl( QWidget *parent )
{
    //std::string msg("[sv4guiModelEdit::CreateQtPartControl] ");
    //std::cout << msg << "========== CreateQtPartControl ==========" << std::endl;
    //std::cout << msg << "parent: " << parent << std::endl;

    m_Parent = parent;
    ui->setupUi(parent);

    m_RenderWindow = GetRenderWindowPart(mitk::WorkbenchUtil::OPEN);
    //std::cout << msg << "parent: " << parent << std::endl;

    if (m_RenderWindow == nullptr) {
        parent->setEnabled(false);
        MITK_ERROR << "Plugin ModelEdit Init Error: No M_renderWindow!";
        return;
    }

    // Widget for selecting segmentations used to construct the model.
    //
    connect(ui->btnUpdateModel, SIGNAL(clicked()), this, SLOT(ShowSegSelectionWidget()) );
    m_SegSelectionWidget = new sv4guiSegSelectionWidget();
    m_SegSelectionWidget->move(400,400);
    m_SegSelectionWidget->hide();
    m_SegSelectionWidget->setWindowFlags(Qt::WindowStaysOnTopHint);

    connect(m_SegSelectionWidget,SIGNAL(accepted()), this, SLOT(CreateModel()));
    connect(ui->btnChangeFacet, SIGNAL(clicked()), this, SLOT(ChangeFacetSize()) );
    connect(ui->btnConvert, SIGNAL(clicked()), this, SLOT(ConvertToPolyDataModel()) );

    // Face table events. 
    //
    sv4guiFaceListDelegate* itemDelegate = new sv4guiFaceListDelegate(this);
    m_FaceListTableModel = new QStandardItemModel(this);
    ui->tableViewFaceList->setModel(m_FaceListTableModel);
    ui->tableViewFaceList->setItemDelegateForColumn(2,itemDelegate);
    ui->tableViewFaceList->setItemDelegateForColumn(5,itemDelegate);

    connect( m_FaceListTableModel, SIGNAL(itemChanged(QStandardItem*)), this, SLOT(UpdateFaceData(QStandardItem*)) );

    connect( ui->tableViewFaceList->selectionModel() , SIGNAL( selectionChanged(const QItemSelection&, const QItemSelection&)), this, 
        SLOT( TableFaceListSelectionChanged(const QItemSelection&, const QItemSelection&)));

    connect( ui->tableViewFaceList, SIGNAL( doubleClicked( const QModelIndex & ) ), this, 
        SLOT( ToggleVisibility ( const QModelIndex & ) ) );

    connect( ui->tableViewFaceList, SIGNAL( doubleClicked( const QModelIndex & ) ), this, 
        SLOT( ChangeColor ( const QModelIndex & ) ) );

    // Face popup menu used to set various dislpay attributes.
    //
    m_FaceListTableMenu = new QMenu(ui->tableViewFaceList);
    QAction* showAction = m_FaceListTableMenu->addAction("Show");
    QAction* hideAction = m_FaceListTableMenu->addAction("Hide");
    QAction* changeTypeAction = m_FaceListTableMenu->addAction("Change Type");
    QAction* changeColorAction = m_FaceListTableMenu->addAction("Change Color");
    QAction* changeOpacityAction = m_FaceListTableMenu->addAction("Change Opacity");

    connect( showAction, SIGNAL( triggered(bool) ) , this, SLOT( ShowSelected(bool) ) );
    connect( hideAction, SIGNAL( triggered(bool) ) , this, SLOT( HideSelected(bool) ) );
    connect( changeTypeAction, SIGNAL( triggered(bool) ) , this, SLOT( ChangeTypeSelected(bool) ) );
    connect( changeColorAction, SIGNAL( triggered(bool) ) , this, SLOT( ChangeColorSelected(bool) ) );
    connect( changeOpacityAction, SIGNAL( triggered(bool) ) , this, SLOT( ChangeOpacitySelected(bool) ) );

    connect( ui->tableViewFaceList, SIGNAL(customContextMenuRequested(const QPoint&)), this, 
        SLOT(TableViewFaceListContextMenuRequested(const QPoint&)) );

    ui->toolBoxPolyData->setCurrentIndex(0);

    // Buttons for delete, fill, etc.
    //
    QSignalMapper* signalMapper = new QSignalMapper(this);

    signalMapper->setMapping(ui->btnDeleteFaces, DELETE_FACES);
    connect(ui->btnDeleteFaces, SIGNAL(clicked()), signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnFillHoleIDs, FILL_HOLES_WITH_IDS);
    connect(ui->btnFillHoleIDs, SIGNAL(clicked()), signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnCombineFaces, COMBINE_FACES);
    connect(ui->btnCombineFaces, SIGNAL(clicked()), signalMapper, SLOT(map()));

    signalMapper->setMapping(ui->btnRemeshFaces, REMESH_FACES);
    connect(ui->btnEstimateEdgeSize0, SIGNAL(clicked()), this, SLOT(SetEstimatedEdgeSize()) );
    connect(ui->btnRemeshFaces, SIGNAL(clicked()), signalMapper, SLOT(map()));

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

    // [DaveP] mapped(int) is obsolete.
    connect(signalMapper, SIGNAL(mappedInt(int)), this, SLOT(ModelOperate(int)));
    //connect(signalMapper, SIGNAL(mapped(int)), this, SLOT(ModelOperate(int)));

    connect(ui->checkBoxSphere, SIGNAL(toggled(bool)), this, SLOT(ShowSphereInteractor(bool)));

    // Trim sub-panel. 
    //
    connect(ui->checkBoxShowPlane, SIGNAL(toggled(bool)), this, SLOT(ShowPlaneInteractor(bool)));
    connect(ui->comboBoxPathPlane, SIGNAL(currentIndexChanged(int)), this, SLOT(SetupSliderPathPlane(int )));
    connect(ui->sliderPathPlane, SIGNAL(valueChanged(double)), this, SLOT(UpdatePlaneWidget(double )));

    connect(ui->checkBoxShowBox, SIGNAL(toggled(bool)), this, SLOT(ShowBoxInteractor(bool)));
    connect(ui->comboBoxPathBox, SIGNAL(currentIndexChanged(int)), this, SLOT(SetupSliderPathBox(int )));
    connect(ui->sliderPathBox, SIGNAL(valueChanged(double)), this, SLOT(UpdateBoxWidget(double )));

    // Blend sub-panel. 
    //
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

    // Remsh using mmg. 
    //
    int idx=0;
    QWidget* widgetGRemesh=ui->toolBoxGlobalOps->widget(idx);
    QString title=ui->toolBoxGlobalOps->itemText(idx);
    widgetGRemesh->hide();
    ui->toolBoxGlobalOps->removeItem(idx);
#ifdef SV_USE_MMG
    ui->toolBoxGlobalOps->insertItem(idx,widgetGRemesh,title);
    widgetGRemesh->show();
#endif

    // Extract Centerlines sub-panel 
    //
    m_CapSelectionWidget = new sv4guiCapSelectionWidget();
    m_CapSelectionWidget->move(400,400);
    m_CapSelectionWidget->hide();
    m_CapSelectionWidget->setWindowFlags(Qt::WindowStaysOnTopHint);

    connect(ui->btnExtractCenterlines, SIGNAL(clicked()), this, SLOT(ShowCapSelectionWidget()) );
    connect(m_CapSelectionWidget,SIGNAL(accepted()), this, SLOT(ExtractCenterlines()));

    m_MarkersNode = mitk::DataNode::New();
    m_MarkersNode->SetName("markers");
    m_MarkersNode->SetVisibility(true);
    m_MarkersContainer = sv4guiModelMarkerContainer::New();
    m_MarkersNode->SetData(m_MarkersContainer);
    GetDataStorage()->Add(m_MarkersNode, m_ModelNode);

    if (m_MarkerMapper.IsNull()) { 
      m_MarkerMapper = sv4guiModelMarkerMapper::New();
      m_MarkerMapper->SetDataNode(m_MarkersNode);
      m_MarkerMapper->SetColor(1.0, 0.0, 0.0);
      m_MarkersNode->SetMapper(mitk::BaseRenderer::Standard3D, m_MarkerMapper);
  }


}

//---------
// Visible
//---------
//
void sv4guiModelEdit::Visible()
{
  //std::string msg("[sv4guiModelEdit::Visible] ");
  //std::cout << msg << "========== Visible ==========" << std::endl;
  m_isVisible = true;
  ui->tabWidget->setCurrentIndex(0);
  OnSelectionChanged(berry::IWorkbenchPart::Pointer(), GetDataManagerSelection());
}

void sv4guiModelEdit::Hidden()
{
    m_isVisible = false;
    RemoveObservers();
}

void sv4guiModelEdit::Activated()
{
}

void sv4guiModelEdit::Deactivated()
{
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
        time_t time = std::time(nullptr); 
        m_ModelNode->SetStringProperty("time modified", ctime(&time)); 
    }
}

//-------------
// GetTimeStep
//-------------
//
int sv4guiModelEdit::GetTimeStep()
{
  //std::string msg("[sv4guiModelEdit::GetTimeStep] ");
  //std::cout << msg << "========== GetTimeStep ==========" << std::endl;

   mitk::TimeNavigationController* timeNavigationController = nullptr;

   if (m_RenderWindow) {
      timeNavigationController = m_RenderWindow->GetTimeNavigationController();
   }

   if (timeNavigationController) {
      // [DaveP] not sure which one to use, GetSelectedTimeStep() or
      // GetStepper()->GetPos(), maybe they do the same thing.
      return timeNavigationController->GetSelectedTimeStep();
      //return timeNavigationController->GetStepper()->GetPos();
      //return timeNavigationController->GetTime()->GetPos();
   } else {
      return 0;
   }
}

//--------------------
// OnSelectionChanged
//--------------------
//
void sv4guiModelEdit::OnSelectionChanged(berry::IWorkbenchPart::Pointer part, const QList<mitk::DataNode::Pointer>& nodes)
{
  //std::string msg("[sv4guiModelEdit::OnSelectionChanged] ");
  //std::cout << msg << "========== OnSelectionChanged ==========" << std::endl;
  //std::cout << msg << "m_isVisible: " << m_isVisible << std::endl;

  if (!m_isVisible) {
    return;
  }

  m_LocalOperationforBlendRegion = false;

  if (nodes.size() == 0) {
    RemoveObservers();
    m_Parent->setEnabled(false);
    return;
  }

  mitk::DataNode::Pointer selectedNode=nodes.front();
  sv4guiModel* model=dynamic_cast<sv4guiModel*>(selectedNode->GetData());

  if (!model) {
    RemoveObservers();
    m_Parent->setEnabled(false);
    return;
  }

  if (m_ModelNode == selectedNode) {
    AddObservers();
    m_Parent->setEnabled(true);
    return;
  }

  if (m_ModelNode.IsNotNull()) {
    RemoveObservers();
  }

  m_ModelNode = selectedNode;
  m_Model = model;
  m_ModelType = m_Model->GetType();

  m_Parent->setEnabled(true);
  AddObservers();

  ui->tabWidget->setCurrentIndex(0);

  UpdateGUI();

  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void sv4guiModelEdit::UpdateGUI()
{
    //std::string msg("[sv4guiModelEdit::UpdateGUI] ");
    //std::cout << msg << "========== UpdateGUI ==========" << std::endl;

    auto modelElement = dynamic_cast<sv4guiModelElementPolyData*>(m_Model->GetModelElement(0));
    //std::cout << msg << "modelElement: " << modelElement << std::endl;

    if (modelElement != nullptr) {
      auto faces = modelElement->GetFaces();
      //std::cout << msg << "faces[0]->vpd->GetNumberOfCells(): " <<  faces[0]->vpd->GetNumberOfCells() << std::endl;
    }

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
    //std::string msg("[sv4guiModelEdit::UpdatePathListForTrim] ");
    //std::cout << msg << "========== UpdatePathListForTrim ==========" << std::endl;

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
    if(path==nullptr)
        return;

    sv4guiPathElement* pe=path->GetPathElement(GetTimeStep());
    if(pe==nullptr)
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
    if(m_PlaneWidget==nullptr || !m_PlaneWidget->GetEnabled())
        return;

    if(m_PathFolderNode.IsNull())
        return;

    QString selectedPathName=ui->comboBoxPathPlane->currentText();

    mitk::DataNode::Pointer pathNode=GetDataStorage()->GetNamedDerivedNode (selectedPathName.toStdString().c_str(), m_PathFolderNode);
    if(pathNode.IsNull())
        return;

    sv4guiPath* path=dynamic_cast<sv4guiPath*>(pathNode->GetData());
    if(path==nullptr)
        return;

    sv4guiPathElement* pe=path->GetPathElement(GetTimeStep());
    if(pe==nullptr)
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
    if(path==nullptr)
        return;

    sv4guiPathElement* pe=path->GetPathElement(GetTimeStep());
    if(pe==nullptr)
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
    if(m_BoxWidget==nullptr || !m_BoxWidget->GetEnabled())
        return;

    if(m_PathFolderNode.IsNull())
        return;

    QString selectedPathName=ui->comboBoxPathBox->currentText();

    mitk::DataNode::Pointer pathNode=GetDataStorage()->GetNamedDerivedNode (selectedPathName.toStdString().c_str(), m_PathFolderNode);
    if(pathNode.IsNull())
        return;

    sv4guiPath* path=dynamic_cast<sv4guiPath*>(pathNode->GetData());
    if(path==nullptr)
        return;

    sv4guiPathElement* pe=path->GetPathElement(GetTimeStep());
    if(pe==nullptr)
        return;

    int posIdx=idx;

    sv4guiPathElement::sv4guiPathPoint pathPoint=pe->GetPathPoint(posIdx);

    m_BoxWidget->PlaceWidget (-3, 3, -3, 3, -3, 3);
    m_BoxWidget->SetTransform(sv4guiSegmentationUtils::GetvtkTransformBox(pathPoint,6));

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void sv4guiModelEdit::UpdateFaceListSelection()
{
    //std::string msg("[sv4guiModelEdit::UpdateFaceListSelection] ");
    //std::cout << msg << "========== UpdateFaceListSelection ==========" << std::endl;
    //std::cout << msg << "m_Model: " << m_Model << std::endl;

    if(!m_Model) return;

    sv4guiModelElement* modelElement = m_Model->GetModelElement();

    if(!modelElement) return;

    if(m_FaceListTableModel==nullptr)
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
    if(modelElement==nullptr) return;

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
        if(faces[i]==nullptr )
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
    //std::string msg("[sv4guiModelEdit::UpdateFaceData] ");
    //std::cout << msg << "========== UpdateFaceData ==========" << std::endl;
    //std::cout << msg << "m_Model: " << m_Model << std::endl;

    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==nullptr) return;

    int row=item->index().row();
    int col=item->index().column();

    QStandardItem* itemID= m_FaceListTableModel->item(row,0);
    int id=itemID->text().toInt();

    sv4guiModelElement::svFace* face=modelElement->GetFace(id);

    if(face==nullptr)
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

//-------------------------------
// TableFaceListSelectionChanged
//-------------------------------
//
void sv4guiModelEdit::TableFaceListSelectionChanged( const QItemSelection & /*selected*/, const QItemSelection & /*deselected*/ )
{
  //std::string msg("[sv4guiModelEdit::TableFaceListSelectionChanged] ");
  //std::cout << msg << "========== TableFaceListSelectionChanged ==========" << std::endl;

  mitk::StatusBar::GetInstance()->DisplayText("");

  if (!m_Model) {
    return;
  }

  int timeStep = GetTimeStep();
  sv4guiModelElement* modelElement = m_Model->GetModelElement(timeStep);

  //std::cout << msg << "timeStep: " << timeStep << std::endl;
  //std::cout << msg << "modelElement: " << modelElement << std::endl;

  if (modelElement == nullptr) {
    return;
  }

  if (m_FaceListTableModel == nullptr) {
    return;
  }

  //std::cout << msg << "Search for face ... " << std::endl;
  QModelIndexList indexesOfSelectedRows = ui->tableViewFaceList->selectionModel()->selectedRows();
  modelElement->ClearFaceSelection();
  bool useFirst = true;

  for (QModelIndexList::iterator it = indexesOfSelectedRows.begin() ; it != indexesOfSelectedRows.end(); it++) {
    int row = (*it).row();
    //std::cout << msg << "  row: " << row << std::endl;
    QStandardItem* itemID = m_FaceListTableModel->item(row,0);
    int id = itemID->text().toInt();
    //std::cout << msg << "  id: " << id << std::endl;

    modelElement->SelectFace(id);

    if (useFirst) {
      double faceArea = modelElement->GetFaceArea(id);
      QString info = "Face "+QString::fromStdString(modelElement->GetFaceName(id))+": Area="+QString::number(faceArea);
      mitk::StatusBar::GetInstance()->DisplayText(info.toStdString().c_str());
      useFirst = false;
    }
  }

  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}


void sv4guiModelEdit::ToggleVisibility(const QModelIndex &index)
{
    //std::string msg("[sv4guiModelEdit::ToggleVisibility] ");
    //std::cout << msg << "========== ToggleVisibility ==========" << std::endl;
    //std::cout << msg << "m_Model: " << m_Model << std::endl;

    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==nullptr) return;

    int row=index.row();
    int col=index.column();

    if(col!=3)
        return;

    QStandardItem* itemID= m_FaceListTableModel->item(row,0);
    int id=itemID->text().toInt();

    sv4guiModelElement::svFace* face=modelElement->GetFace(id);

    if(face==nullptr)
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
    if(modelElement==nullptr) return;

    int row=index.row();
    int col=index.column();

    if(col!=4)
        return;

    QStandardItem* itemID= m_FaceListTableModel->item(row,0);
    int id=itemID->text().toInt();

    sv4guiModelElement::svFace* face=modelElement->GetFace(id);

    if(face==nullptr)
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
    if(modelElement==nullptr) return;

    if(m_FaceListTableModel==nullptr)
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

        if(face==nullptr)
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
    if(modelElement==nullptr) return;

    if(m_FaceListTableModel==nullptr)
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

        if(face==nullptr)
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
    if(modelElement==nullptr) return;

    if(m_FaceListTableModel==nullptr)
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

        if(face==nullptr)
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
    if(modelElement==nullptr) return;

    if(m_FaceListTableModel==nullptr)
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

        if(face==nullptr)
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
    if(modelElement==nullptr) return;

    if(m_FaceListTableModel==nullptr)
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

        if(face==nullptr)
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
    if(modelElement==nullptr) return;

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
        if(faces[i]==nullptr || faces[i]->type=="cap" || faces[i]->type=="inlet" || faces[i]->type=="outlet" || faces[i]->name.substr(0,10)=="wall_blend")
            continue;

        for(int j=i+1;j<faces.size();j++)
        {
            if(faces[j]==nullptr || faces[j]->type=="cap" || faces[j]->type=="inlet" || faces[j]->type=="outlet" || faces[j]->name.substr(0,10)=="wall_blend")
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
    if(mepd==nullptr) return;

    sv4guiModelElement::svBlendParam* param=mepd->GetBlendParam();

    ui->dsbDecimation->setValue(param->targetdecimation);
    ui->sbBlendIters->setValue(param->numblenditers);
    ui->sbSubBlendIters->setValue(param->numsubblenditers);
    ui->sbCstrSmoothIters->setValue(param->numcgsmoothiters);
    ui->sbLapSmoothIters->setValue(param->numlapsmoothiters);
    ui->sbSubdivisionIters->setValue(param->numsubdivisioniters);
}

//----------------------------
// TableBlendSelectionChanged
//----------------------------
//
void sv4guiModelEdit::TableBlendSelectionChanged( const QItemSelection & /*selected*/, const QItemSelection & /*deselected*/ )
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==nullptr) return;

    if(m_BlendTableModel==nullptr)
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
    if(m_BlendTableModel==nullptr)
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
    if(m_BlendTableModel==nullptr)
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
    if(m_BlendTableModel==nullptr)
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
    if(m_BlendTableModel==nullptr)
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
            m_ModelNode->SetDataInteractor(nullptr);
    }
    m_DataInteractor=nullptr;
}

void sv4guiModelEdit::ClearAll()
{
    //std::string msg("[sv4guiModelEdit::ClearAll] ");
    //std::cout << msg << "========== ClearAll ==========" << std::endl;
    m_Model=nullptr;
    m_ModelNode=nullptr;

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
            if(group==nullptr)
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
        if(faces[i]==nullptr )
            continue;

        if (faces[i]->type=="cap")
          caps.push_back(faces[i]->name);
    }

    m_CapSelectionWidget->SetTableView(caps,modelElement,m_ModelType);
    m_CapSelectionWidget->show();
}

//-------------
// CreateModel
//-------------
//
void sv4guiModelEdit::CreateModel()
{
    #define n_debug_CreateModel_
    #ifdef debug_CreateModel_
    std::string msg("[sv4guiModelEdit::CreateModel] ");
    std::cout << msg << std::endl;
    std::cout << msg << "========== CreateModel ==========" << std::endl;
    #endif

    std::vector<std::string> segNames=m_SegSelectionWidget->GetUsedSegNames();
    int numSampling=m_SegSelectionWidget->GetNumSampling();
    #ifdef debug_CreateModel_
    std::cout << msg << "numSampling: " << numSampling << std::endl;
    #endif

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSubset(isProjFolder);

    if(rs->size()<1) return;

    mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

    rs=GetDataStorage()->GetDerivations (projFolderNode,mitk::NodePredicateDataType::New("sv4guiSegmentationFolder"));
    if(rs->size()<1) return;

    mitk::DataNode::Pointer segFolderNode=rs->GetElement(0);

    std::vector<mitk::DataNode::Pointer> segNodes;

    for (int i = 0; i < segNames.size(); i++) {
        mitk::DataNode::Pointer node = GetDataStorage()->GetNamedDerivedNode(segNames[i].c_str(),segFolderNode);
        if (node.IsNotNull()) {
            segNodes.push_back(node);
        }
    }

    // Sanity check
    int numSeg2D = 0;
    int numSeg3D = 0;

    for(int i=0;i<segNodes.size();i++) {
        sv4guiContourGroup* group = dynamic_cast<sv4guiContourGroup*>(segNodes[i]->GetData());

        if(group!=nullptr) {
            numSeg2D++;
            continue;
        }

        sv4guiMitkSeg3D* seg3D = dynamic_cast<sv4guiMitkSeg3D*>(segNodes[i]->GetData());

        if(seg3D!=nullptr) {
            numSeg3D++;
            continue;
        }
    }

    if(numSeg2D+numSeg3D==0) {
        QMessageBox::warning(m_Parent,"Warning","No valid segmentations are used.");
        return;
    }

    if(numSeg2D==0 && (m_ModelType=="OpenCASCADE" || m_ModelType=="Parasolid")) {
        QMessageBox::warning(m_Parent,"Warning","No valid 2D segmentations are used.");
        return;
    }

    sv4guiModelElement* newModelElement = nullptr;
    sv4guiModelElement* modelElement = m_Model->GetModelElement();
    #ifdef debug_CreateModel_
    std::cout << msg << "modelElement: " << modelElement << std::endl;
    #endif

    mitk::ProgressBar::GetInstance()->Reset();
    mitk::ProgressBar::GetInstance()->AddStepsToDo(3);
    mitk::StatusBar::GetInstance()->DisplayText("Creating model...");
    mitk::ProgressBar::GetInstance()->Progress();
    WaitCursorOn();

    svLoftingParam* param=nullptr;
    int useUniform = m_SegSelectionWidget->IfUseUniform();
    #ifdef debug_CreateModel_
    std::cout << msg << "useUniform: " << useUniform << std::endl;
    #endif

    if (useUniform) {
        param = new svLoftingParam(m_SegSelectionWidget->GetLoftingParam());
    }

    bool created = true;
    QString statusText = "Model has been created.";
    std::string exceptionText;
    PolyDataSolidCheckResults check_results;

    sv4guiModelElement* tempElement = sv4guiModelElementFactory::CreateModelElement(m_ModelType);
    #ifdef debug_CreateModel_
    std::cout << msg << "tempElement: " << tempElement << std::endl;
    #endif

    if (tempElement) {
        #ifdef debug_CreateModel_
        std::cout << msg << "CreateModelElement ... " << std::endl;
        std::cout << msg << "m_ModelType: " << m_ModelType << std::endl;
        #endif
        int stats[2]={0};

        try {

        if (m_ModelType == "PolyData") {
            newModelElement = tempElement->CreateModelElement(segNodes,numSampling,param,check_results,stats);

        } else if (m_ModelType == "OpenCASCADE") {
            newModelElement = tempElement->CreateModelElement(segNodes,numSampling,param,check_results,nullptr,20.0);

        } else if (m_ModelType == "Parasolid") {
            newModelElement = tempElement->CreateModelElement(segNodes,numSampling,nullptr,check_results,nullptr,1.0);
        }

        #ifdef debug_CreateModel_
        std::cout << msg << "newModelElement: " << newModelElement << std::endl;
        #endif

        } catch (const PolyDataException& exception) {
          std::cout << "[CreateModel] ERROR: The Boolan union has failed." << std::endl;
          std::cout << "[CreateModel] ERROR: " << exception.what() << std::endl;
          exceptionText = exception.what();

          auto points = exception.getPoints();
          DisplayPoints(points);

          auto geometry = exception.getGeometry();
          DisplayGeometry(geometry);

          newModelElement = nullptr; 

        } catch (const std::exception& exception) {
          std::cout << "[CreateModel] ERROR: The Boolan union has failed." << std::endl;
          std::cout << "[CreateModel] ERROR: " << exception.what() << std::endl;
          exceptionText = exception.what();
          newModelElement = nullptr; 
        }

        if (newModelElement == nullptr) {
            if (exceptionText != "") {
              statusText = QString::fromStdString(exceptionText);
            } else {
              statusText = "Failed to create model.";
            }
            created = false;

        } else if (m_ModelType == "PolyData") {
            statusText = statusText+" Number of Free Edges: "+ QString::number(stats[0])+", Number of Bad Edges: "+ QString::number(stats[1]);
            #ifdef debug_CreateModel_
            std::cout << msg << "Number of Free Edges: " << stats[0] << std::endl;
            std::cout << msg << "Number of bad Edges: " << stats[1] << std::endl;
            #endif

        }

        if (newModelElement) {
            newModelElement->SetUseUniform(useUniform);
            if (useUniform) {
                newModelElement->SetLoftingParam(param);
            }
        }
    }

    delete tempElement;

    WaitCursorOff();
    mitk::ProgressBar::GetInstance()->Progress(2);
    mitk::StatusBar::GetInstance()->DisplayText(statusText.toStdString().c_str());

    if (!created) {
      QMessageBox::warning(m_Parent, "Warning", statusText);
      return;
    }

    if (newModelElement != nullptr) {
        int timeStep = GetTimeStep();

        mitk::OperationEvent::IncCurrObjectEventId();
        sv4guiModelOperation* doOp = new sv4guiModelOperation(sv4guiModelOperation::OpSETMODELELEMENT,timeStep,newModelElement);
        sv4guiModelOperation* undoOp = new sv4guiModelOperation(sv4guiModelOperation::OpSETMODELELEMENT,timeStep,modelElement);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Model, doOp, undoOp, "Set ModelElement");
        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

        m_Model->ExecuteOperation(doOp);

        std::vector<std::string> segNames = newModelElement->GetSegNames();
        std::vector<std::string> faceNames = newModelElement->GetFaceNames();

        // This seems to return if the model is valid, even if it 
        // has free edges.
        //
        if ( faceNames.size() <= 2*segNames.size()+1 ) {
            #ifdef debug_CreateModel_
            std::cout << msg << "#### Number of faceNames <= 2 * number of segment names " << std::endl;
            std::cout << msg << "  faceNames.size(): " << faceNames.size() << std::endl;
            std::cout << msg << "  2*segNames.size()+1: " << 2*segNames.size()+1 << std::endl;

            std::cout << msg << "segNames ... " << std::endl;
            for (auto name : segNames) {
               std::cout << msg << "  name: " << name << std::endl;
            }

            std::cout << msg << "faceNames ... " << std::endl;
            for (auto name : faceNames) {
               std::cout << msg << "  name: " << name << std::endl;
            }
 
            std::cout << msg << "return " << std::endl;
            #endif

            ProcessResultsCheck(newModelElement, check_results);

            return;
        }

        // Find possible extra faces.
        //
        std::vector<std::string> faceNamesToCheck;
        std::string wallPrefix = "wall_";
        std::string capPrefix = "cap_";

        if (newModelElement->GetType() == "Parasolid") {
            capPrefix = "";
        }

        for (int i = 0; i < segNames.size(); ++i) {
            #ifdef debug_CreateModel_
            std::cout << msg << "---------- seg " << i << " ----------" << std::endl;
            std::cout << msg << "segNames[i]: " << segNames[i] << std::endl;
            #endif

            int capNumber = 0;
            int wallNumber = 0;

            QString wallName = QString::fromStdString(wallPrefix+segNames[i]);
            QString capName = QString::fromStdString(capPrefix+segNames[i]);
            #ifdef debug_CreateModel_
            std::cout << msg << "wallName: " << wallName << std::endl;
            std::cout << msg << "capName: " << capName << std::endl;
            #endif

            for(int j=0;j<faceNames.size();j++)
            {
                #ifdef debug_CreateModel_
                std::cout << msg << "----- face " << j << " -----" << std::endl;
                std::cout << msg << "faceNames[j]: " << faceNames[j] << std::endl;
                #endif

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

            #ifdef debug_CreateModel_
            std::cout << msg << "capNumber: " << capNumber << std::endl;
            std::cout << msg << "wallNumber: " << wallNumber << std::endl;
            #endif

            if(capNumber>1)
            {
                for(int j=0;j<capNumber;++j)
                {
                    QString suffix="";
                    if(j>0) {
                        suffix="_"+QString::number(j+1);
                    }

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
            std::string info = "There may be vessels with an end not fully enclosed in an intersecting vessel. There may also be vessels that don't intersect any other vessels.\n\n";
            info += "Please check the faces listed under Details and highlighted in the Face List browser.";
            auto text = QString::fromStdString(info);
            QString title = "A problem was encountered when creating the model";
            QMessageBox::Icon icon = QMessageBox::Warning;
            QMessageBox mb(nullptr); 
            mb.setWindowTitle(title);
            mb.setText(text+"                                                                                         ");
            mb.setIcon(icon);
            mb.setDetailedText(QString::fromStdString(faceList));
            mb.exec();

            return;
        }

    }
}

//---------------
// DisplayPoints
//---------------
// Display points showing where a union computation has possibly failed.
//
void sv4guiModelEdit::DisplayPoints(std::vector<std::array<double,3>>& points)
{
  if (points.size() == 0) {
    return;
  }

  auto cell_points = vtkSmartPointer<vtkPoints>::New();

  for (const auto& point : points) { 
      cell_points->InsertNextPoint(point[0], point[1], point[2]);
      std::cout << "[sv4guiModelEdit::DisplayPoints] point: " << point[0] << " " << point[1] << " " << point[2] << std::endl;
  }

  auto pointsPolydata = vtkSmartPointer<vtkPolyData>::New();
  pointsPolydata->SetPoints(cell_points);
  auto vertexFilter = vtkSmartPointer<vtkVertexGlyphFilter>::New();
  vertexFilter->SetInputData(pointsPolydata);
  vertexFilter->Update();

  auto polydata = vtkSmartPointer<vtkPolyData>::New();
  polydata->ShallowCopy(vertexFilter->GetOutput());

  m_MarkersContainer->SetMarkers(polydata);

  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

//-----------------
// DisplayGeometry
//-----------------
// Display a vtkPolyData object showing where a union computation has possibly failed.
//
void sv4guiModelEdit::DisplayGeometry(vtkSmartPointer<vtkPolyData> geometry)
{
  if (geometry == nullptr) {
    return;
  }

  m_MarkersContainer->SetGeometry(geometry);
}

//---------------------
// ProcessResultsCheck 
//---------------------
//
void sv4guiModelEdit::ProcessResultsCheck(sv4guiModelElement* newModelElement, PolyDataSolidCheckResults& check_results)
{
  #define n_debug_ProcessResultsCheck
  #ifdef debug_ProcessResultsCheck
  std::string msg("[sv4guiModelEdit::ProcessResultsCheck] ");
  std::cout << msg << "========== ProcessResultsCheck ==========" << std::endl;
  #endif

  if (check_results.invalid_cells.size() == 0) {
    return;
  }

  auto model = newModelElement->GetWholeVtkPolyData();

  auto model_faces = vtkIntArray::SafeDownCast(model->GetCellData()-> GetScalars("ModelFaceID"));
  model->BuildLinks();
  auto model_points = model->GetPoints();

  int num_invalid_cells = check_results.invalid_cells.size();
  auto cell_centers = vtkSmartPointer<vtkPoints>::New();

  for (int cell_id : check_results.invalid_cells) {
    int face_id = model_faces->GetValue(cell_id);

    const vtkIdType *pts;
    vtkIdType npts = 0;
    model->GetCellPoints(cell_id, npts, pts);

    auto cellNeighbors = vtkSmartPointer<vtkIdList>::New();
    #ifdef debug_ProcessResultsCheck
    std::cout << msg << ">>>> cell id: " << cell_id << "  face id: " << face_id << std::endl;
    #endif
    double cell_center[3] = {};
    double point[3];

    for (int j = 0; j < npts; j++) {
      vtkIdType p0 = pts[j];
      model_points->GetPoint(p0, point);
      cell_center[0] += point[0];
      cell_center[1] += point[1];
      cell_center[2] += point[2];
      cell_centers->InsertNextPoint(point);
      #ifdef debug_ProcessResultsCheck
      std::cout << msg << "  point: " << point[0] << "  " << point[1] << " " << point[2] << std::endl;
      #endif
    }

    double point1[3], point2[3], point3[3];
    model->GetPoint(pts[0],point1);
    model->GetPoint(pts[1],point2);
    model->GetPoint(pts[2],point3);
    double area = vtkTriangle::TriangleArea(point1,point2,point3);

    cell_center[0] /= npts;
    cell_center[1] /= npts;
    cell_center[2] /= npts;
    //cell_centers->InsertNextPoint(cell_center);
    #ifdef debug_ProcessResultsCheck
    std::cout << msg << "  cell_center: " << cell_center[0] << "  " << cell_center[1] << " " << cell_center[2] << std::endl;
    std::cout << msg << "  cell area: " << area << std::endl;
    #endif

    for (int j = 0; j < npts; j++) {
      vtkIdType p0 = pts[j];
      vtkIdType p1 = pts[(j+1) % npts];
      model->GetCellEdgeNeighbors(cell_id, p0, p1, cellNeighbors);
      int num_adj_cells = cellNeighbors->GetNumberOfIds();
      #ifdef debug_ProcessResultsCheck
      std::cout << msg << "     num_adj_cells: " << num_adj_cells << std::endl;
      #endif

      if (num_adj_cells == 1) {
        #ifdef debug_ProcessResultsCheck
        std::cout << msg << "  num_adj_cells: " << num_adj_cells << std::endl;
        #endif
        for (vtkIdType k = 0; k < num_adj_cells; k++) {
          int adj_cell = cellNeighbors->GetId(k);
          int adj_face_id = model_faces->GetValue(adj_cell);
          #ifdef debug_ProcessResultsCheck
          std::cout << msg << "  adj_cell: " << adj_cell << "  face id: " << adj_face_id << std::endl;
          #endif
        }
      }
    }
  }

  auto pointsPolydata = vtkSmartPointer<vtkPolyData>::New();
  pointsPolydata->SetPoints(cell_centers);
  auto vertexFilter = vtkSmartPointer<vtkVertexGlyphFilter>::New();
  vertexFilter->SetInputData(pointsPolydata);
  vertexFilter->Update();

  auto polydata = vtkSmartPointer<vtkPolyData>::New();
  polydata->ShallowCopy(vertexFilter->GetOutput());
  #ifdef debug_ProcessResultsCheck
  std::cout << msg << "polydata num points: " << polydata->GetNumberOfPoints() << std::endl;
  #endif

  m_MarkersContainer->SetMarkers(polydata);

  mitk::RenderingManager::GetInstance()->RequestUpdateAll();

  QMessageBox::warning(m_Parent, "Warning", 
    QString::number(num_invalid_cells) + " invalid triangles have been identified for the model and are shown as read markers.\n" +
    "The model is displayed only for reference and should not be used for meshing.\n"); 
}

//--------------------
// ExtractCenterlines
//--------------------
//
void sv4guiModelEdit::ExtractCenterlines()
{
    if (m_ModelType != "PolyData")
    {
      QMessageBox::warning(m_Parent,"Error","Cannot currently extract centerlines of anyting other than a PolyData model");
      return;
    }

    int timeStep = GetTimeStep();
    sv4guiModelElement* modelElement = m_Model->GetModelElement(timeStep);
    std::vector<std::string> capNames = m_CapSelectionWidget->GetUsedCapNames();

    // Count the number of caps.
    //
    std::vector<sv4guiModelElement::svFace*> faces = modelElement->GetFaces();
    int num_caps = 0;

    for(int i = 0; i < faces.size(); i++) {
      if(faces[i] == nullptr) {
        continue;
      }

      if (faces[i]->type == "cap") {
        num_caps += 1;
      }
    }

    if (num_caps == capNames.size()) {
      QMessageBox::warning(m_Parent,"Error","Cannot use all of the caps to extract centerlines.");
      return;
    }

    std::vector<int> capIds;
    for (int i=0; i<capNames.size(); i++) {
      capIds.push_back(modelElement->GetFaceID(capNames[i]));
    }

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
    if(modelElement==nullptr)
        return blendRadii;

    if(m_BlendTableModel==nullptr)
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
    if(m_Model==nullptr) return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);

    if(modelElement==nullptr) return;

    sv4guiModelElement* newModelElement=nullptr;

    std::vector<sv4guiModelElement::svBlendParamRadius*> blendRadii=GetBlendRadii();
    if(blendRadii.size()==0)
        return;

    mitk::ProgressBar::GetInstance()->Reset();
    mitk::ProgressBar::GetInstance()->AddStepsToDo(2);
    mitk::StatusBar::GetInstance()->DisplayText("Blending model...");
    mitk::ProgressBar::GetInstance()->Progress();
    WaitCursorOn();

    sv4guiModelElement::svBlendParam* param=nullptr;
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

    if(newModelElement==nullptr) return;

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
    if(modelElement==nullptr) return faceIDs;

    if(m_FaceListTableModel==nullptr)
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
    if(m_SphereWidget!=nullptr && m_SphereWidget->GetEnabled() && m_SphereWidget->GetRadius()>0)
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

//--------------
// ModelOperate
//--------------
//
void sv4guiModelEdit::ModelOperate(int operationType)
{
    std::string msg("[sv4guiModelEdit::ModelOperate] ");
    std::cout << msg << "========== ModelOperate ==========" << std::endl;
    std::cout << msg << "m_Model: " << m_Model << std::endl;
    std::cout << msg << "operationType: " << operationType << std::endl;

    if(m_Model==nullptr) return;

    int timeStep=GetTimeStep();
    sv4guiModelElementPolyData* modelElement=dynamic_cast<sv4guiModelElementPolyData*>(m_Model->GetModelElement(timeStep));

    if(modelElement==nullptr) return;

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
        if(m_PlaneWidget!=nullptr && m_PlaneWidget->GetEnabled())
        {
            double* origin=m_PlaneWidget->GetOrigin();
            double* point1=m_PlaneWidget->GetPoint1();
            double* point2=m_PlaneWidget->GetPoint2();
            ok=newModelElement->CutByPlane(origin,point1,point2,true);
        }
        break;
    case CUT_BELOW:
        if(m_PlaneWidget!=nullptr && m_PlaneWidget->GetEnabled())
        {
            double* origin=m_PlaneWidget->GetOrigin();
            double* point1=m_PlaneWidget->GetPoint1();
            double* point2=m_PlaneWidget->GetPoint2();
            ok=newModelElement->CutByPlane(origin,point1,point2,false);
        }
        break;
    case CUT_BOX:
        if(m_BoxWidget!=nullptr && m_BoxWidget->GetEnabled())
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
        if(m_SphereWidget!=nullptr)
        {
            m_SphereWidget->Off();
        }

        return;
    }

    if(m_Model==nullptr) return;

    int timeStep=GetTimeStep();
    sv4guiModelElementPolyData* modelElement=dynamic_cast<sv4guiModelElementPolyData*>(m_Model->GetModelElement(timeStep));

    if(modelElement==nullptr) return;

    if(modelElement->GetWholeVtkPolyData()==nullptr) return;
    if(m_SphereWidget==nullptr)
    {
        m_SphereWidget = vtkSmartPointer<vtkSphereWidget>::New();
        // m_SphereWidget->SetInteractor(m_DisplayWidget->GetRenderWindow4()->GetVtkRenderWindow()->GetInteractor());
        m_SphereWidget->SetInteractor(m_RenderWindow->GetQmitkRenderWindow("3d")->GetVtkRenderWindow()->GetInteractor());
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
        if(m_PlaneWidget!=nullptr)
        {
            m_PlaneWidget->Off();
        }

        return;
    }

    if(m_Model==nullptr) return;

    int timeStep=GetTimeStep();
    sv4guiModelElementPolyData* modelElement=dynamic_cast<sv4guiModelElementPolyData*>(m_Model->GetModelElement(timeStep));

    if(modelElement==nullptr) return;

    if(modelElement->GetWholeVtkPolyData()==nullptr) return;
    if(m_PlaneWidget==nullptr)
    {
        m_PlaneWidget = vtkSmartPointer<vtkPlaneWidget>::New();
        // m_PlaneWidget->SetInteractor(m_DisplayWidget->GetRenderWindow4()->GetVtkRenderWindow()->GetInteractor());
        m_PlaneWidget->SetInteractor(m_RenderWindow->GetQmitkRenderWindow("3d")->GetVtkRenderWindow()->GetInteractor());
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
        if(m_BoxWidget!=nullptr)
        {
            m_BoxWidget->Off();
        }

        return;
    }

    if(m_Model==nullptr) return;

    int timeStep=GetTimeStep();
    sv4guiModelElementPolyData* modelElement=dynamic_cast<sv4guiModelElementPolyData*>(m_Model->GetModelElement(timeStep));

    if(modelElement==nullptr) return;

    if(modelElement->GetWholeVtkPolyData()==nullptr) return;
    if(m_BoxWidget==nullptr)
    {
        m_BoxWidget = vtkSmartPointer<vtkBoxWidget>::New();
        // m_BoxWidget->SetInteractor(m_DisplayWidget->GetRenderWindow4()->GetVtkRenderWindow()->GetInteractor());
        m_BoxWidget->SetInteractor(m_RenderWindow->GetQmitkRenderWindow("3d")->GetVtkRenderWindow()->GetInteractor());
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
    if(m_Model==nullptr || m_Model->GetModelElement(GetTimeStep())==nullptr)
        return;

    double facetSize=0;
    QString sizeType="";

    sv4guiModelElementAnalytic* meAnalytic=dynamic_cast<sv4guiModelElementAnalytic*>(m_Model->GetModelElement(GetTimeStep()));
    if(meAnalytic==nullptr)
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
    if(m_Model==nullptr || m_Model->GetModelElement(GetTimeStep())==nullptr)
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

    sv4guiModelElement* modelElement=nullptr;

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

    if(modelElement==nullptr)
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
    if(m_Model==nullptr || m_Model->GetModelElement(GetTimeStep())==nullptr)
        return 0;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==nullptr) return 0;

    double edgeSize= sqrt(modelElement->GetMinFaceArea()/3.1415)/2.5;
    edgeSize=round(10000*edgeSize)/10000;

    return edgeSize;
}
