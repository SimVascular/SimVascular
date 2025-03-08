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

#include "sv4gui_MeshEdit.h"
#include "ui_sv4gui_MeshEdit.h"

#include "sv4gui_VtkMeshSphereWidget.h"

#include "sv4gui_Model.h"
#include "sv4gui_MeshFactory.h"
#include "sv4gui_Mesh.h"
#include "sv4gui_MitkMesh.h"
#include "sv4gui_MitkMeshOperation.h"
#include "sv4gui_MitkMeshIO.h"
#include "sv4gui_ProjectManager.h"

#include "sv4gui_ModelElementPolyData.h"
#include "sv4gui_ModelElementAnalytic.h"

#include "sv4gui_DataNodeOperation.h"

#include <mitkIPreferencesService.h>
#include <mitkIPreferences.h>
#include <berryPlatform.h>

#include <QmitkRenderWindow.h>
#include <QmitkStdMultiWidgetEditor.h>
#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>
#include <mitkSliceNavigationController.h>
#include <mitkProgressBar.h>
#include <mitkStatusBar.h>

#include <usModuleRegistry.h>

#include <vtkProperty.h>
#include <vtkXMLUnstructuredGridReader.h>

#include <QMessageBox>
#include <QInputDialog>
#include <QFileDialog>
#include <QRegularExpression>

#include <iostream>
using namespace std;

const QString sv4guiMeshEdit::EXTENSION_ID = "org.sv.views.meshing";

sv4guiMeshEdit::sv4guiMeshEdit() :
    ui(new Ui::sv4guiMeshEdit)
{
    m_MitkMesh=nullptr;
    m_Model=nullptr;
    m_MeshNode=nullptr;
    m_ModelNode=nullptr;

    m_DataInteractor=nullptr;
    m_ModelSelectFaceObserverTag=-1;

    m_TableModelLocal=nullptr;
    m_TableMenuLocal=nullptr;

    m_TableModelRegion=nullptr;
    m_TableMenuRegion=nullptr;

    m_TableModelDomains=nullptr;
    m_TableMenuDomains=nullptr;

    m_SphereWidget=nullptr;

    m_SelectedRegionIndex=-1;
    m_SelectedDomainsIndex=-1;

    m_UndoAble=false;

    m_Interface=new sv4guiDataNodeOperationInterface;

    m_CustomDelegate=new sv4guiLocalTableDelegate(this);
    m_DefaultDelegate=new QItemDelegate(this);
}

sv4guiMeshEdit::~sv4guiMeshEdit()
{
    delete ui;

    if(m_TableModelLocal)
        delete m_TableModelLocal;

    if(m_TableMenuLocal)
        delete m_TableMenuLocal;

    if(m_TableModelRegion)
        delete m_TableModelRegion;

    if(m_TableMenuRegion)
        delete m_TableMenuRegion;

    if(m_TableMenuDomains)
        delete m_TableMenuDomains;

    if(m_TableModelDomains)
        delete m_TableModelDomains;

    if(m_CustomDelegate)
        delete m_CustomDelegate;

    if(m_DefaultDelegate)
        delete m_DefaultDelegate;
}

void sv4guiMeshEdit::CreateQtPartControl( QWidget *parent )
{
    m_Parent=parent;
    ui->setupUi(parent);

    m_RenderWindow = GetRenderWindowPart(mitk::WorkbenchUtil::OPEN);

    if(m_RenderWindow==nullptr)
    {
        parent->setEnabled(false);
        MITK_ERROR << "Plugin MeshEdit Init Error: No m_RenderWindow Available!";
        return;
    }

    connect(ui->btnRunMesher, SIGNAL(clicked()), this, SLOT(RunMesher()) );

    SetupGUI(parent);

    if(m_SphereWidget==nullptr)
    {
        m_SphereWidget = vtkSmartPointer<sv4guiVtkMeshSphereWidget>::New();
        m_SphereWidget->SetInteractor(m_RenderWindow->GetQmitkRenderWindow("3d")->GetVtkRenderWindow()->GetInteractor());
    //    m_SphereWidget->SetRepresentationToSurface();
        sv4guiVtkMeshSphereWidget* sphereWidget=dynamic_cast<sv4guiVtkMeshSphereWidget*>(m_SphereWidget.GetPointer());
        sphereWidget->SetMeshEdit(this);
    }

    connect(ui->btnMeshInfo, SIGNAL(clicked()), this, SLOT(DisplayMeshInfo()) );
    connect(ui->checkBoxShowModel, SIGNAL(clicked(bool)), this, SLOT(ShowModel(bool)) );
}

void sv4guiMeshEdit::SetupGUI(QWidget *parent )
{
    //global - tetgen
    connect(ui->btnEstimateT, SIGNAL(clicked()), this, SLOT(SetEstimatedEdgeSize()) );

    ui->toolBox->setCurrentIndex(0);

    //for local table
    m_TableModelLocal = new QStandardItemModel(this);
    ui->tableViewLocal->setModel(m_TableModelLocal);

    ui->tableViewLocal->setItemDelegateForColumn(4,m_CustomDelegate);
    ui->tableViewLocal->setItemDelegateForColumn(6,m_CustomDelegate);
    ui->tableViewLocal->setItemDelegateForColumn(7,m_CustomDelegate);

    connect( ui->tableViewLocal->selectionModel()
      , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
      , this
      , SLOT( TableFaceListSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );

    connect( ui->tableViewLocal, SIGNAL(customContextMenuRequested(const QPoint&))
      , this, SLOT(TableViewLocalContextMenuRequested(const QPoint&)) );

    //local menu
    m_TableMenuLocal=new QMenu(ui->tableViewLocal);
    QAction* setLocalTAction=m_TableMenuLocal->addAction("Set Local Size");
    QAction* clearLocalTAction=m_TableMenuLocal->addAction("Clear Local Size");
    connect( setLocalTAction, SIGNAL( triggered(bool) ) , this, SLOT( SetLocal(bool) ) );
    connect( clearLocalTAction, SIGNAL( triggered(bool) ) , this, SLOT( ClearLocal(bool) ) );

    //for regional table
    connect(ui->checkBoxSphere, SIGNAL(toggled(bool)), this, SLOT(ShowSphereInteractor(bool)));
    connect(ui->btnAddSphere, SIGNAL(clicked()), this, SLOT(AddSphere()) );

    m_TableModelRegion = new QStandardItemModel(this);
    ui->tableViewRegion->setModel(m_TableModelRegion);

    connect( ui->tableViewRegion->selectionModel()
      , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
      , this
      , SLOT( TableRegionListSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );

    m_TableMenuRegion=new QMenu(ui->tableViewRegion);
    QAction* setRegionTAction=m_TableMenuRegion->addAction("Set Regional Size");
    QAction* deleteRegionTAction=m_TableMenuRegion->addAction("Delete");
    connect( setRegionTAction, SIGNAL( triggered(bool) ) , this, SLOT( SetRegion(bool) ) );
    connect( deleteRegionTAction, SIGNAL( triggered(bool) ) , this, SLOT( DeleteSelectedRegions(bool) ) );

    connect( ui->tableViewRegion, SIGNAL(customContextMenuRequested(const QPoint&))
      , this, SLOT(TableViewRegionContextMenuRequested(const QPoint&)) );

    //for multi-domains
    connect(ui->btnAddHole, SIGNAL(clicked()), this, SLOT(AddHole()) );
    connect(ui->btnAddSubDomain, SIGNAL(clicked()), this, SLOT(AddSubDomain()) );

    m_TableModelDomains = new QStandardItemModel(this);
    ui->tableViewDomains->setModel(m_TableModelDomains);

    connect( ui->tableViewDomains->selectionModel()
      , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
      , this
      , SLOT( TableDomainsListSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );

    m_TableMenuDomains=new QMenu(ui->tableViewDomains);
    QAction* setDomainSizeTAction=m_TableMenuDomains->addAction("Set SubDomain Size");
    QAction* deleteDomainsTAction=m_TableMenuDomains->addAction("Delete");
    QAction* addHoleTAction=m_TableMenuDomains->addAction("Add Hole");
    QAction* addSubDomainTAction=m_TableMenuDomains->addAction("Add SubDomain");
    connect( setDomainSizeTAction, SIGNAL( triggered(bool) ) , this, SLOT( SetSubDomainSize(bool) ) );
    connect( deleteDomainsTAction, SIGNAL( triggered(bool) ) , this, SLOT( DeleteSelectedDomains(bool) ) );
    connect( addHoleTAction, SIGNAL( triggered(bool) ) , this, SLOT( AddHole() ) );
    connect( addSubDomainTAction, SIGNAL( triggered(bool) ) , this, SLOT( AddSubDomain() ) );

    connect( ui->tableViewDomains, SIGNAL(customContextMenuRequested(const QPoint&))
      , this, SLOT(TableViewDomainsContextMenuRequested(const QPoint&)) );

    //for adaptor
    connect(ui->comboBoxOption, SIGNAL(currentIndexChanged(int)), this, SLOT(UpdateAdaptGUI(int)));
    ui->comboBoxOption->setCurrentIndex(0);
    UpdateAdaptGUI(0);

    connect(ui->toolButtonResultFile, SIGNAL(clicked()), this, SLOT(SetResultFile()));

    connect(ui->btnAdapt, SIGNAL(clicked()), this, SLOT(Adapt()));

    // Advanced flags.
    ui->MinDihedralAngleSpinBox->setRange(0.0, 45.0);
}

void sv4guiMeshEdit::TableFaceListSelectionChanged( const QItemSelection & /*selected*/, const QItemSelection & /*deselected*/ )
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==nullptr) return; 

    QStandardItemModel* tableModel=m_TableModelLocal;
    QTableView* tableView=ui->tableViewLocal;

    if(tableModel==nullptr || tableView==nullptr)
        return;

    QModelIndexList indexesOfSelectedRows = tableView->selectionModel()->selectedRows();

    modelElement->ClearFaceSelection();

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
         ; it != indexesOfSelectedRows.end(); it++)
    {
        int row=(*it).row();

        QStandardItem* itemID= tableModel->item(row,0);
        int id=itemID->text().toInt();

        modelElement->SelectFace(id);
    }

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void sv4guiMeshEdit::SetLocal(bool)
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==nullptr) return;

    QStandardItemModel* tableModel=m_TableModelLocal;
    QTableView* tableView=ui->tableViewLocal;

    if(tableModel==nullptr || tableView==nullptr)
        return;

    QModelIndexList indexesOfSelectedRows = tableView->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
      return;
    }

    bool ok=false;
    QString localInfo="";

    double localSize=QInputDialog::getDouble(m_Parent, "Set Local Size", "Local Size:", 0.0, 0, 100, 4, &ok);
    localInfo=QString::number(localSize);

    if(!ok)
        return;

    int columnIndex=0;
    if(m_MeshType=="TetGen")
        columnIndex=3;
    else if(m_MeshType=="MeshSim")
        columnIndex=5;
    else
        return;


    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
       ; it != indexesOfSelectedRows.end(); it++)
     {
       int row=(*it).row();

       QStandardItem* item= tableModel->item(row,columnIndex);
       item->setText(localInfo);
     }
}

void sv4guiMeshEdit::ClearLocal(bool)
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==nullptr) return;

    QStandardItemModel* tableModel=m_TableModelLocal;
    QTableView* tableView=ui->tableViewLocal;

    if(tableModel==nullptr || tableView==nullptr)
        return;

    QModelIndexList indexesOfSelectedRows = tableView->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
      return;
    }

    int columnIndex=0;
    if(m_MeshType=="TetGen")
        columnIndex=3;
    else if(m_MeshType=="MeshSim")
        columnIndex=5;
    else
        return;

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
       ; it != indexesOfSelectedRows.end(); it++)
     {
       int row=(*it).row();

       QStandardItem* item= tableModel->item(row,columnIndex);
       item->setText("");
     }
}

void sv4guiMeshEdit::TableViewLocalContextMenuRequested( const QPoint & pos )
{
    m_TableMenuLocal->popup(QCursor::pos());
}

void sv4guiMeshEdit::TableRegionListSelectionChanged( const QItemSelection & /*selected*/, const QItemSelection & /*deselected*/ )
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==nullptr) return;

    QStandardItemModel* tableModel=m_TableModelRegion;
    QTableView* tableView=ui->tableViewRegion;

    if(tableModel==nullptr || tableView==nullptr)
        return;

    QModelIndexList indexesOfSelectedRows = tableView->selectionModel()->selectedRows();

    m_SelectedRegionIndex=-1;

    if(m_SphereWidget&&m_SphereWidget->GetEnabled())
    {
        m_SphereWidget->GetSphereProperty()->SetColor(1.0,1.0,1.0);
    }

    if(indexesOfSelectedRows.size()==0)
    {
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
        return;
    }

    int row=indexesOfSelectedRows[0].row();
    m_SelectedRegionIndex=row;

    QStandardItem* itemShape= tableModel->item(row,0);
    QString shape=itemShape->text();

    QStandardItem* itemLocal= tableModel->item(row,1);
    QString localSize=itemLocal->text();

    QStandardItem* itemParams= tableModel->item(row,2);
    QString params=itemParams->text();

    if(m_SphereWidget&&m_SphereWidget->GetEnabled())
    {
        m_SphereWidget->GetSphereProperty()->SetColor(1.0,0.0,0.0);
        QStringList plist = params.split(QRegularExpression("\\s+"));
        m_SphereWidget->SetRadius(plist[0].toDouble());
        m_SphereWidget->SetCenter(plist[1].toDouble(),plist[2].toDouble(),plist[3].toDouble());
    }

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void sv4guiMeshEdit::TableDomainsListSelectionChanged( const QItemSelection & /*selected*/, const QItemSelection & /*deselected*/ )
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==nullptr) return;

    QStandardItemModel* tableModel=m_TableModelDomains;
    QTableView* tableView=ui->tableViewDomains;

    if(tableModel==nullptr || tableView==nullptr)
        return;

    QModelIndexList indexesOfSelectedRows = tableView->selectionModel()->selectedRows();

    m_SelectedDomainsIndex=-1;

    if(indexesOfSelectedRows.size()==0)
    {
        return;
    }

    int row=indexesOfSelectedRows[0].row();
    m_SelectedDomainsIndex=row;

    QStandardItem* itemType= tableModel->item(row,0);
    QString type=itemType->text();

    QStandardItem* itemSize= tableModel->item(row,1);
    QString localSize=itemSize->text();

    QStandardItem* itemLocation= tableModel->item(row,2);
    QString location=itemLocation->text();

    QStringList plist = location.split(QRegularExpression("\\s+"));
    if(plist.size()==3)
    {
        mitk::Point3D point;
        point[0]= plist[0].toDouble();
        point[1]= plist[1].toDouble();
        point[2]= plist[2].toDouble();

        // [DaveP] what's going on here ?
        std::cout << "MoveCrossToPosition does not exist" << std::endl << std::flush;
        exit(1);
        // m_DisplayWidget->MoveCrossToPosition(point);
    }
}

void sv4guiMeshEdit::SetRegion(bool)
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==nullptr) return;

    QStandardItemModel* tableModel=m_TableModelRegion;
    QTableView* tableView=ui->tableViewRegion;

    if(tableModel==nullptr || tableView==nullptr)
        return;

    QModelIndexList indexesOfSelectedRows = tableView->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
      return;
    }

    bool ok=false;
    QString localInfo="";

    double localSize=QInputDialog::getDouble(m_Parent, "Set Edge Size", "Edge Size:", 0.0, 0, 100, 4, &ok);
    localInfo=QString::number(localSize);

    if(!ok)
        return;

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
       ; it != indexesOfSelectedRows.end(); it++)
     {
       int row=(*it).row();

       QStandardItem* item= tableModel->item(row,1);
       item->setText(localInfo);
     }
}

void sv4guiMeshEdit::SetSubDomainSize(bool)
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==nullptr) return;

    QStandardItemModel* tableModel=m_TableModelDomains;
    QTableView* tableView=ui->tableViewDomains;

    if(tableModel==nullptr || tableView==nullptr)
        return;

    QModelIndexList indexesOfSelectedRows = tableView->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
      return;
    }

    bool ok=false;
    QString domainInfo="";

    double domainSize=QInputDialog::getDouble(m_Parent, "Set Edge Size", "Edge Size:", 0.0, 0, 100, 4, &ok);
    domainInfo=QString::number(domainSize);

    if(!ok)
        return;

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
       ; it != indexesOfSelectedRows.end(); it++)
     {
       int row=(*it).row();

       QStandardItem* itemType= tableModel->item(row,0);
       QString type=itemType->text();

       if (type=="SubDomain")
       {
         QStandardItem* item= tableModel->item(row,1);
         item->setText(domainInfo);
       }
     }
}

void sv4guiMeshEdit::DeleteSelectedRegions(bool)
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==nullptr) return;

    QStandardItemModel* tableModel=m_TableModelRegion;
    QTableView* tableView=ui->tableViewRegion;

    if(tableModel==nullptr || tableView==nullptr)
        return;

    QModelIndexList indexesOfSelectedRows = tableView->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
      return;
    }

    std::vector<int> rows;
    for(int i=0;i<indexesOfSelectedRows.size();i++)
        rows.push_back(indexesOfSelectedRows[i].row());

    std::sort(rows.begin(), rows.end(), std::greater<int>());

    for(int i=0;i<rows.size();i++)
        tableModel->removeRow(rows[i]);

}

void sv4guiMeshEdit::DeleteSelectedDomains(bool)
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==nullptr) return;

    QStandardItemModel* tableModel=m_TableModelDomains;
    QTableView* tableView=ui->tableViewDomains;

    if(tableModel==nullptr || tableView==nullptr)
        return;

    QModelIndexList indexesOfSelectedRows = tableView->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
      return;
    }

    std::vector<int> rows;
    for(int i=0;i<indexesOfSelectedRows.size();i++)
        rows.push_back(indexesOfSelectedRows[i].row());

    std::sort(rows.begin(), rows.end(), std::greater<int>());

    for(int i=0;i<rows.size();i++)
        tableModel->removeRow(rows[i]);

}

void sv4guiMeshEdit::TableViewRegionContextMenuRequested( const QPoint & pos )
{
    m_TableMenuRegion->popup(QCursor::pos());
}

void sv4guiMeshEdit::TableViewDomainsContextMenuRequested( const QPoint & pos )
{
    m_TableMenuDomains->popup(QCursor::pos());
}

//----------------------
// SetEstimatedEdgeSize
//----------------------
//
void sv4guiMeshEdit::SetEstimatedEdgeSize()
{
  double edgeSize = EstimateEdgeSize();

  ui->lineEditGlobalEdgeSizeT->setText(QString::number(edgeSize));
}

//------------------
// EstimateEdgeSize
//------------------
//
double sv4guiMeshEdit::EstimateEdgeSize()
{
  //std::string msg("[sv4guiMeshEdit::EstimateEdgeSize] ");
  //std::cout << msg << "========== EstimateEdgeSize ==========" << std::endl;

  if (!m_MitkMesh) {
    //std::cout << msg << "No m_MitkMesh " << std::endl;
    return 0.0;
  }

  if (!m_Model) {
    //std::cout << msg << "No m_Model " << std::endl;
    return 0.0;
  }

  sv4guiModelElement* modelElement = dynamic_cast<sv4guiModelElement*>(m_Model->GetModelElement());

  if (!modelElement) {
    //std::cout << msg << "No modelElement " << std::endl;
    return 0;
  }

  double min_face_area = modelElement->GetMinFaceArea();
  //std::cout << msg << "min_face_area: " << min_face_area << std::endl;

  double edgeSize = sqrt(min_face_area / 3.1415) /2.5;
  edgeSize = round(10000.0*edgeSize) / 10000.0;
  //std::cout << msg << "edgeSize: " << edgeSize << std::endl;

  return edgeSize;
}

void sv4guiMeshEdit::RunMesher()
{
    RunCommands(true);
}

void sv4guiMeshEdit::RunHistory()
{
    RunCommands(false);
}

//-------------
// RunCommands
//-------------
//
void sv4guiMeshEdit::RunCommands(bool fromGUI)
{
  //std::string msg("[sv4guiMeshEdit::RunCommands] ");
  //std::cout << msg << "========== RunCommands ==========" << std::endl;
  //std::cout << msg << "fromGUI: " << fromGUI << std::endl;

  int timeStep = GetTimeStep();
  sv4guiMesh* originalMesh = m_MitkMesh->GetMesh(timeStep);

  if(originalMesh&&originalMesh->GetSurfaceMesh() == nullptr) {
    std::string path = "";
    m_MeshNode->GetStringProperty("path",path);

    if (path!= "") {
      std::string surfaceFileName = path+"/"+m_MeshNode->GetName()+".vtp";
      std::ifstream surfaceFile(surfaceFileName);
      if(surfaceFile) {
        if (QMessageBox::question(m_Parent, "Previous Mesh exists", 
            "Previous mesh created already, but not loaded from file. Do you want to create new mesh?", 
            QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes) {
          return;
        }
      }
    }
  }

  if (QMessageBox::question(m_Parent, "Meshing", "The meshing may take a while. Do you want to continue?",
        QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes) {
    return;
  }

  if(!m_MitkMesh) return;

  if(!m_Model) return;

  sv4guiModelElement* modelElement = m_Model->GetModelElement();
  std::string modelType = modelElement->GetType();

  if (!modelElement) {
    return;
  }

  if( m_MeshType=="MeshSim" && (modelType=="PolyData" || modelType=="OpenCASCADE") ) {
    QMessageBox::warning(nullptr,"Not Compatible!", QString::fromStdString(m_MeshType)+ " doesn't work with " +
       QString::fromStdString(modelType) + " model.");
    return;
  }

  if (m_MeshType == "TetGen" && modelType != "PolyData") {
    QMessageBox::warning(nullptr,"Not Compatible!", QString::fromStdString(m_MeshType)+ " only works with PolyData model.");
    return;
  }

  QString ges="";

  if(m_MeshType=="TetGen") {
    ges = ui->lineEditGlobalEdgeSizeT->text().trimmed();
  } else if(m_MeshType=="MeshSim") {
    ges = ui->lineEditGlobalSizeM->text().trimmed();
  }

  bool ok = false;
  ges.toDouble(&ok);

  if (!ok) {
    QMessageBox::warning(m_Parent,"Warning","Error in Global Size!");
    return;
  }

  sv4guiMesh* newMesh = sv4guiMeshFactory::CreateMesh(m_MeshType);

  if(newMesh == nullptr) {
    return;
  }

  //add fake progress
  mitk::ProgressBar::GetInstance()->AddStepsToDo(3);

  mitk::StatusBar::GetInstance()->DisplayText("Creating mesh...");
  mitk::ProgressBar::GetInstance()->Progress();
  WaitCursorOn();

  newMesh->InitNewMesher();
  newMesh->SetModelElement(modelElement);
  std::vector<std::string> cmds;

  if(fromGUI) {
    if(m_MeshType=="TetGen") {
      cmds = CreateCmdsT();
    } else if(m_MeshType=="MeshSim") {
      cmds = CreateCmdsM();
   }
  } else {
    cmds = originalMesh->GetCommandHistory();
  }

  //std::cout << msg << "+++ Execute commands ... " << std::endl;
  std::string error_msg;

  if (!newMesh->ExecuteCommands(cmds, error_msg)) {
    WaitCursorOff();
    mitk::ProgressBar::GetInstance()->Progress(2);
    QMessageBox::warning(m_Parent,"Error during executing",QString::fromStdString(error_msg));
    delete newMesh;
    return;
  }
  //std::cout << msg << "--- Done Execute commands " << std::endl;

  newMesh->SetCommandHistory(cmds);

  if(m_UndoAble) {
    mitk::OperationEvent::IncCurrObjectEventId();
  }

  sv4guiMitkMeshOperation* doOp = new sv4guiMitkMeshOperation(sv4guiMitkMeshOperation::OpSETMESH,timeStep,newMesh);

  if(m_UndoAble) {
    sv4guiMitkMeshOperation* undoOp = new sv4guiMitkMeshOperation(sv4guiMitkMeshOperation::OpSETMESH,timeStep,originalMesh);
    mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_MitkMesh, doOp, undoOp, "Set Mesh");
    mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
  }

  m_MitkMesh->ExecuteOperation(doOp);

  if(!m_UndoAble) {
    delete originalMesh;
  }

  WaitCursorOff();
  mitk::ProgressBar::GetInstance()->Progress(2);
  mitk::StatusBar::GetInstance()->DisplayText("Meshing done.");

  mitk::RenderingManager::GetInstance()->RequestUpdateAll();

  DisplayMeshInfo();
}

std::vector<std::string> sv4guiMeshEdit::CreateCmdsT()
{
    std::vector<std::string> cmds;

    if(ui->checkBoxSurfaceT->isChecked())
        cmds.push_back("option surface 1");
    else
        cmds.push_back("option surface 0");

    if(ui->checkBoxVolumeT->isChecked())
        cmds.push_back("option volume 1");
    else
        cmds.push_back("option volume 0");

    if(ui->checkBoxRadiusBasedT->isChecked())
    {
        cmds.push_back("option UseMMG 0");
        ui->checkBoxFastMeshing->setChecked(false);
    }
    else
    {
        cmds.push_back("option UseMMG 1");
    }

    cmds.push_back("option GlobalEdgeSize "+ui->lineEditGlobalEdgeSizeT->text().trimmed().toStdString());

    if(ui->checkBoxBoundaryLayerT->isChecked())
      ui->checkBoxFastMeshing->setChecked(false);

    if(!ui->checkBoxFastMeshing->isChecked())
        cmds.push_back("setWalls");

    if(ui->checkBoxRadiusBasedT->isChecked())
    {
        cmds.push_back("useCenterlineRadius");
        cmds.push_back("functionBasedMeshing "+ ui->lineEditGlobalEdgeSizeT->text().trimmed().toStdString() +" DistanceToCenterlines");
    }

    if(ui->checkBoxBoundaryLayerT->isChecked())
    {
      int useConstantThickness = ui->checkBoxConstantThicknessBL->isChecked();
      cmds.push_back("boundaryLayer "+QString::number(ui->sbLayersT->value()).toStdString()
                       +" "+QString::number(ui->dsbPortionT->value()).toStdString()+" "+QString::number(ui->dsbRatioT->value()).toStdString()+" "+QString::number(useConstantThickness).toStdString());

      int boundaryLayerDirection = ui->checkBoxBoundaryLayerDirection->isChecked();
      cmds.push_back("option BoundaryLayerDirection "+QString::number(boundaryLayerDirection).toStdString());

      if (ui->checkBoxConvertBLToNewRegion->isChecked())
      {
        int convertBLToNewRegion = ui->checkBoxConvertBLToNewRegion->isChecked();
        cmds.push_back("option NewRegionBoundaryLayer");
      }
    }


    for(int i=0;i<m_TableModelLocal->rowCount();i++)
    {
        QStandardItem* itemName= m_TableModelLocal->item(i,1);
        QString name=itemName->text();

        QStandardItem* itemLocal= m_TableModelLocal->item(i,3);
        QString localSize=itemLocal->text().trimmed();

        if(!localSize.isEmpty())
            cmds.push_back("localSize " + name.toStdString() + " " + localSize.toStdString());
    }

    for(int i=0;i<m_TableModelRegion->rowCount();i++)
    {
        QStandardItem* itemShape= m_TableModelRegion->item(i,0);
        QString shape=itemShape->text();

        QStandardItem* itemLocal= m_TableModelRegion->item(i,1);
        QString localSize=itemLocal->text().trimmed();

        QStandardItem* itemParams= m_TableModelRegion->item(i,2);
        QString params=itemParams->text();
        QStringList plist = params.split(QRegularExpression("\\s+"));

        if(!localSize.isEmpty())
            cmds.push_back("sphereRefinement " + localSize.toStdString() + " " + plist[0].toStdString()
                    + " " + plist[1].toStdString() + " " + plist[2].toStdString() + " " + plist[3].toStdString());
    }

    if(ui->checkBoxFlagO->isChecked())
        cmds.push_back("option Optimization "+QString::number(ui->sliderFlagO->value()).toStdString());

    if(ui->checkBoxFlagT->isChecked())
        cmds.push_back("option Epsilon "+ui->lineEditFlagT->text().toStdString());

    if(ui->checkBoxFlagQ->isChecked())
        cmds.push_back("option QualityRatio "+QString::number(ui->sliderFlagQ->value()).toStdString());

    if(ui->MinDihedralAngleCheckBox->isChecked()) {
        cmds.push_back("option MinDihedral " + QString::number(ui->MinDihedralAngleSpinBox->value()).toStdString());
    }

    if(ui->checkBoxFlagY->isChecked())
        cmds.push_back("option NoBisect");

    if(ui->checkBoxFlagM->isChecked())
        cmds.push_back("option NoMerge");

    if(ui->checkBoxFlagD->isChecked())
        cmds.push_back("option Diagnose");

    if(ui->checkBoxFlagC->isChecked())
        cmds.push_back("option Check");

    if(ui->checkBoxFlagQ2->isChecked())
        cmds.push_back("option Quiet");

    if(ui->checkBoxFlagV->isChecked())
        cmds.push_back("option Verbose");

    for(int i=0;i<m_TableModelDomains->rowCount();i++)
    {
        QStandardItem* itemType= m_TableModelDomains->item(i,0);
        QString type=itemType->text();

        QStandardItem* itemSize= m_TableModelDomains->item(i,1);
        QString size=itemSize->text();

        QStandardItem* itemLocation= m_TableModelDomains->item(i,2);
        QString location=itemLocation->text();
        QStringList locationList = location.split(QRegularExpression("\\s+"));

        if (type=="Hole")
        {
          if(!locationList.isEmpty())
              cmds.push_back("option AddHole " + locationList[0].toStdString()
                      + " " + locationList[1].toStdString() + " " + locationList[2].toStdString());
        }
        else if (type=="SubDomain")
        {
          if(!locationList.isEmpty())
          {
            if (!size.isEmpty())
            {
              cmds.push_back("option AddSubDomain " + size.toStdString() + " " + locationList[0].toStdString()
                      + " " + locationList[1].toStdString() + " " + locationList[2].toStdString());
            }
            else
            {
              cmds.push_back("option AddSubDomain -1.0 " + locationList[0].toStdString()
                      + " " + locationList[1].toStdString() + " " + locationList[2].toStdString());
            }
          }
        }
    }

    if (ui->checkBoxMultipleRegions->isChecked()) {
        cmds.push_back("AllowMultipleRegions 1");
    } 
    else {
        cmds.push_back("AllowMultipleRegions 0");
    }

    cmds.push_back("generateMesh");

    cmds.push_back("writeMesh");

    return cmds;
}

std::vector<std::string> sv4guiMeshEdit::CreateCmdsM()
{
    std::vector<std::string> cmds;

    std::string meshFolderPath=GetMeshFolderPath().trimmed().toStdString();

    //logon
    cmds.push_back("logon "+meshFolderPath+"/"+m_MeshNode->GetName()+".logfile");

    //init meshsim mesh
    cmds.push_back("newMesh");

    //surface options
    if(ui->checkBoxSurfaceM->isChecked())
        cmds.push_back("option surface 1");
    else
        cmds.push_back("option surface 0");

    if(ui->checkBoxSurfaceOptimizationM->isChecked())
        cmds.push_back("option surface optimization 1");
    else
        cmds.push_back("option surface optimization 0");

    QString surfsmooth="option surface smoothing " + QString::number(int(ui->sliderPassesM->value()));
    cmds.push_back(surfsmooth.toStdString());

    //volume options
    if(ui->checkBoxVolumeM->isChecked())
        cmds.push_back("option volume 1");
    else
        cmds.push_back("option volume 0");

    if(ui->checkBoxVolumeSmoothingM->isChecked())
        cmds.push_back("option volume smoothing 1");
    else
        cmds.push_back("option volume smoothing 0");

    if(ui->checkBoxVolumeOptimizationM->isChecked())
        cmds.push_back("option volume optimization 1");
    else
        cmds.push_back("option volume optimization 0");

    //global type and size
    QString gType="";
    switch(ui->comboBoxGlobalTypeM->currentIndex())
    {
    case 0:
        gType="gsize";
        break;
    case 1:
        gType="gcurv";
        break;
    case 2:
        gType="gmincurv";
        break;
    }

    QString gSizeType="";
    if(ui->comboBoxGlobalSizeTypeM->currentIndex()==0)
        gSizeType="1";
    else
        gSizeType="2";

    QString globalSize=gType+" "+gSizeType+" "+ui->lineEditGlobalSizeM->text().trimmed();
    cmds.push_back(globalSize.toStdString());

    //local and boundary layer
    int faceidColIndex=0;
    int facenameColIndex=1;
    int facetypeColIndex=2;
    int ltypeColIndex=3;
    int lsizetypeColIndex=4;
    int lsizeColIndex=5;
    int btypeColIndex=6;
    int bdirectionColIndex=7;
    int layernumberColIndex=8;
    int paramsColIndex=9;

    for(int i=0;i<m_TableModelLocal->rowCount();i++)
    {
        QStandardItem* itemName= m_TableModelLocal->item(i,facenameColIndex);
        QString name=itemName->text().trimmed();

        QStandardItem* itemLtype= m_TableModelLocal->item(i,ltypeColIndex);
        QString ltype=itemLtype->text().trimmed();
        if(ltype=="Max Curv")
            ltype="curv";
        else if(ltype=="Min Curv")
            ltype="mincurv";
        else
            ltype="size";

        QStandardItem* itemLsizetype= m_TableModelLocal->item(i,lsizetypeColIndex);
        QString lsizetype=itemLsizetype->text().trimmed();
        if(lsizetype=="relative")
            lsizetype="2";
        else
            lsizetype="1";

        QStandardItem* itemLocal= m_TableModelLocal->item(i,lsizeColIndex);
        QString localSize=itemLocal->text().trimmed();

        if(!localSize.isEmpty())
            cmds.push_back(ltype.toStdString() + " " + name.toStdString() + " " + lsizetype.toStdString()+ " " + localSize.toStdString());

        QStandardItem* itemBtype= m_TableModelLocal->item(i,btypeColIndex);
        QString btype=itemBtype->text().trimmed();
        if(btype.startsWith("(1)"))
            btype="1";
        else if(btype.startsWith("(2)"))
            btype="2";
        else if(btype.startsWith("(3)"))
            btype="3";
        else
            btype="4";

        QStandardItem* itemBdirection= m_TableModelLocal->item(i,bdirectionColIndex);
        QString bdirection=itemBdirection->text().trimmed();
        if(bdirection=="negative")
            bdirection="0";
        else if(bdirection=="positive")
            bdirection="1";
        else
            bdirection="2";

        QStandardItem* itemLayernumber= m_TableModelLocal->item(i,layernumberColIndex);
        QString layernumber=itemLayernumber->text().trimmed();

        QStandardItem* itemParams= m_TableModelLocal->item(i,paramsColIndex);
        QString params=itemParams->text().trimmed();

        if(!layernumber.isEmpty() && !params.isEmpty())
            cmds.push_back("boundaryLayer " + name.toStdString() + " " + btype.toStdString()+ " " + bdirection.toStdString()
                           + " " + layernumber.toStdString()+ " " + params.toStdString());
    }

    //regional
    for(int i=0;i<m_TableModelRegion->rowCount();i++)
    {
        QStandardItem* itemShape= m_TableModelRegion->item(i,0);
        QString shape=itemShape->text();

        QStandardItem* itemLocal= m_TableModelRegion->item(i,1);
        QString localSize=itemLocal->text().trimmed();

        QStandardItem* itemParams= m_TableModelRegion->item(i,2);
        QString params=itemParams->text();
        QStringList plist = params.split(QRegularExpression("\\s+"));

        if(!localSize.isEmpty())
            cmds.push_back("sphereRefinement " + localSize.toStdString() + " " + plist[0].toStdString()
                    + " " + plist[1].toStdString() + " " + plist[2].toStdString() + " " + plist[3].toStdString());
    }

    if(ui->checkBoxFlagV->isChecked())
        cmds.push_back("option Verbose");

    cmds.push_back("generateMesh");

    cmds.push_back("writeMesh "+meshFolderPath+"/"+m_MeshNode->GetName()+".sms");

    if(ui->checkBoxWriteStatM->isChecked())
        cmds.push_back("writeStats "+meshFolderPath+"/"+m_MeshNode->GetName()+".sts");

    cmds.push_back("deleteMesh");
    cmds.push_back("logoff");

    return cmds;
}

void sv4guiMeshEdit::Activated()
{
}

void sv4guiMeshEdit::Deactivated()
{
}

void sv4guiMeshEdit::Visible()
{
  //std::string msg("[sv4guiMeshEdit::Visible] ");
  //std::cout << msg << "========== Visible ==========" << std::endl;
  m_isVisible = true;
  OnSelectionChanged(berry::IWorkbenchPart::Pointer(), GetDataManagerSelection());
}

void sv4guiMeshEdit::Hidden()
{
  m_isVisible = false;
  RemoveObservers();
}

//-------------
// GetTimeStep
//-------------
//
int sv4guiMeshEdit::GetTimeStep()
{
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

   return 0;
}

//--------------------
// OnSelectionChanged
//--------------------
//
void sv4guiMeshEdit::OnSelectionChanged(berry::IWorkbenchPart::Pointer part, const QList<mitk::DataNode::Pointer>& nodes)
{
  //std::string msg("[sv4guiMeshEdit::OnSelectionChanged] ");
  //std::cout << msg << "========== OnSelectionChanged ==========" << std::endl;
  //std::cout << msg << "m_isVisible: " << m_isVisible << std::endl;
  //std::cout << msg << "nodes.size(): " << nodes.size() << std::endl;

  if (!m_isVisible) {
    return;
  }

  if (nodes.size() == 0) {
    RemoveObservers();
    m_Parent->setEnabled(false);
    return;
  }

  mitk::DataNode::Pointer meshNode = nodes.front();
  sv4guiMitkMesh* mitkMesh = dynamic_cast<sv4guiMitkMesh*>(meshNode->GetData());

  if(!mitkMesh) {
    //std::cout << msg << "No mitkMesh " << std::endl;
    RemoveObservers();
    m_Parent->setEnabled(false);
    return;
  }

  if(m_MeshNode==meshNode) {
    AddObservers();
    m_Parent->setEnabled(true);
    return;
  }

  std::string modelName = mitkMesh->GetModelName();

  mitk::DataNode::Pointer modelNode=nullptr;
  mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
  mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (meshNode,isProjFolder,false);

  if(rs->size()>0) {
    mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);
    rs=GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiModelFolder"));
    if (rs->size()>0) {
      mitk::DataNode::Pointer modelFolderNode=rs->GetElement(0);
      modelNode=GetDataStorage()->GetNamedDerivedNode(modelName.c_str(),modelFolderNode);
    }
  }

  sv4guiModel* model=nullptr;
  if(modelNode.IsNotNull()) {
    model=dynamic_cast<sv4guiModel*>(modelNode->GetData());
  }

  if(m_MeshNode.IsNotNull()) {
        RemoveObservers();
  }

  m_ModelNode=modelNode;
  m_Model=model;
  m_MeshNode=meshNode;
  m_MitkMesh=mitkMesh;
  m_MeshType=m_MitkMesh->GetType();

  if(m_Model==nullptr) {
    m_Parent->setEnabled(false);
    QMessageBox::warning(m_Parent,"No Model Found","No model found for this mesh!");
  } else {
    m_Parent->setEnabled(true);
    AddObservers();
  }

  UpdateGUI();

  mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void sv4guiMeshEdit::UpdateGUI()
{
    //update top part
    //======================================================================
    ui->labelMeshName->setText(QString::fromStdString(m_MeshNode->GetName()));
    ui->labelMeshType->setText(QString::fromStdString(m_MeshType));
    ui->checkBoxShowModel->setChecked(false);
    if(m_ModelNode.IsNotNull())
    {
        ui->labelModelName->setText(QString::fromStdString(m_ModelNode->GetName()));
        if(m_ModelNode->IsVisible(nullptr))
            ui->checkBoxShowModel->setChecked(true);
    }
    else
        ui->labelModelName->setText("No model found");

    if(!m_MitkMesh)
        return;

    if(!m_Model)
        return;

    if(m_MeshType=="TetGen")
    {
        ui->widgetGlobal_T->show();
        ui->widgetGlobal_M->hide();
        ui->widgetAdvancedT->show();
        ui->widgetAdvancedM->hide();
        ui->widgetAdvancedFlagsT->show();
        UpdateTetGenGUI();
    }
    else if(m_MeshType=="MeshSim")
    {
        ui->widgetGlobal_T->hide();
        ui->widgetGlobal_M->show();
        ui->widgetAdvancedT->hide();
        ui->widgetAdvancedM->show();
        ui->widgetAdvancedFlagsT->hide();
        UpdateMeshSimGUI();
    }
}

void sv4guiMeshEdit::UpdateTetGenGUI()
{
    //put default values
    //==========================================

    //global size
    ui->lineEditGlobalEdgeSizeT->clear();

    //advanced options
    ui->checkBoxBoundaryLayerT->setChecked(false);
    ui->dsbPortionT->setValue(0.5);
    ui->sbLayersT->setValue(2);
    ui->dsbRatioT->setValue(0.8);

    ui->checkBoxBoundaryLayerDirection->setChecked(true);
    ui->checkBoxConstantThicknessBL->setChecked(false);
    ui->checkBoxConvertBLToNewRegion->setChecked(false);

    ui->checkBoxRadiusBasedT->setChecked(false);

    ui->checkBoxSurfaceT->setChecked(true);
    ui->checkBoxVolumeT->setChecked(true);

    ui->checkBoxFastMeshing->setChecked(false);

    //local table
    ui->tableViewLocal->setItemDelegateForColumn(3,m_DefaultDelegate);

    m_TableModelLocal->clear();

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==nullptr) return;

    std::vector<sv4guiModelElement::svFace*> faces=modelElement->GetFaces();

    QStringList faceListHeaders;
    faceListHeaders << "ID" << "Name" << "Type" << "Local Size";
    m_TableModelLocal->setHorizontalHeaderLabels(faceListHeaders);
    m_TableModelLocal->setColumnCount(faceListHeaders.size());

    int rowIndex=-1;

    for(int i=0;i<faces.size();i++)
    {
        if(faces[i]==nullptr )
            continue;

        rowIndex++;
        m_TableModelLocal->insertRow(rowIndex);

        QStandardItem* item;

        item= new QStandardItem(QString::number(faces[i]->id));
        item->setEditable(false);
        m_TableModelLocal->setItem(rowIndex, 0, item);

        item= new QStandardItem(QString::fromStdString(faces[i]->name));
        item->setEditable(false);
        m_TableModelLocal->setItem(rowIndex, 1, item);

        item= new QStandardItem(QString::fromStdString(faces[i]->type));
        item->setEditable(false);
        m_TableModelLocal->setItem(rowIndex, 2, item);

        item= new QStandardItem("");
        m_TableModelLocal->setItem(rowIndex, 3, item);
    }

    ui->tableViewLocal->horizontalHeader()->setSectionResizeMode(0, QHeaderView::Fixed);
    ui->tableViewLocal->horizontalHeader()->resizeSection(0,20);
    ui->tableViewLocal->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Interactive);
    ui->tableViewLocal->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Fixed);
    ui->tableViewLocal->horizontalHeader()->resizeSection(2,60);
    ui->tableViewLocal->horizontalHeader()->setSectionResizeMode(3, QHeaderView::Fixed);
    ui->tableViewLocal->horizontalHeader()->resizeSection(3,80);

    ui->tableViewLocal->setColumnHidden(0,true);

    UpdateFaceListSelection();

    //regional refinement
    m_TableModelRegion->clear();

    QStringList regionListHeaders;
    regionListHeaders << "Type" << "Local Size" << "Radius x y z";
    m_TableModelRegion->setHorizontalHeaderLabels(regionListHeaders);
    m_TableModelRegion->setColumnCount(3);

    ui->tableViewRegion->horizontalHeader()->setSectionResizeMode(0, QHeaderView::Fixed);
    ui->tableViewRegion->horizontalHeader()->resizeSection(0,80);
    ui->tableViewRegion->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Fixed);
    ui->tableViewRegion->horizontalHeader()->resizeSection(1,80);
    ui->tableViewRegion->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Interactive);

    //multi-domain region
    m_TableModelDomains->clear();

    QStringList domainsListHeaders;
    domainsListHeaders << "Type" << "SubDomain\nSize" << "Location\n(x y z)";
    m_TableModelDomains->setHorizontalHeaderLabels(domainsListHeaders);
    m_TableModelDomains->setColumnCount(3);

    ui->tableViewDomains->horizontalHeader()->setSectionResizeMode(0, QHeaderView::Fixed);
    ui->tableViewDomains->horizontalHeader()->resizeSection(0,120);
    ui->tableViewDomains->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Fixed);
    ui->tableViewDomains->horizontalHeader()->resizeSection(0,100);
    ui->tableViewDomains->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Interactive);
    ui->tableViewDomains->horizontalHeader()->resizeSection(0,80);

    //advanced flags
    ui->checkBoxFlagO->setChecked(true);
    ui->sliderFlagO->setValue(3);

    ui->checkBoxFlagQ->setChecked(true);
    ui->sliderFlagQ->setValue(1.4);

    ui->checkBoxFlagT->setChecked(false);
    ui->lineEditFlagT->setText("1e-8");

    ui->checkBoxFlagY->setChecked(true);
    ui->checkBoxFlagM->setChecked(false);
    ui->checkBoxFlagD->setChecked(false);
    ui->checkBoxFlagC->setChecked(false);
    ui->checkBoxFlagQ2->setChecked(false);
    ui->checkBoxFlagV->setChecked(false);

    //then udpate with command history
    //========================================
    sv4guiMesh* mesh=m_MitkMesh->GetMesh(GetTimeStep());
    if(mesh==nullptr)
        return;

    std::vector<std::string> cmdHistory=mesh->GetCommandHistory();
    std::string flag="";
    double values[20]={0};
    std::string strValues[5]={""};
    bool option=false;
    std::string msg="";
    int regionRowIndex=-1;
    int domainsRowIndex=-1;

    if(cmdHistory.size()>0)
        ui->checkBoxFastMeshing->setChecked(true);

    for(int i=0;i<cmdHistory.size();i++)
    {
        if(cmdHistory[i]=="")
            continue;

        if(!mesh->ParseCommand(cmdHistory[i],flag,values,strValues,option,msg))
        {
            QMessageBox::warning(m_Parent,"Parsing Error","Error in parsing command history!");
            return;
        }

        if(flag=="GlobalEdgeSize")
        {
            ui->lineEditGlobalEdgeSizeT->setText(QString::number(values[0]));
        }
        else if(flag=="boundaryLayer")
        {
            ui->checkBoxBoundaryLayerT->setChecked(true);
            ui->sbLayersT->setValue(values[0]);
            ui->dsbPortionT->setValue(values[1]);
            ui->dsbRatioT->setValue(values[2]);
        }
        else if(flag=="useCenterlineRadius")
        {
            ui->checkBoxRadiusBasedT->setChecked(true);
        }
        else if(flag=="SurfaceMeshFlag")
        {
            if(values[0]==0.0)
                ui->checkBoxSurfaceT->setChecked(false);
            else
                ui->checkBoxSurfaceT->setChecked(true);
        }
        else if(flag=="VolumeMeshFlag")
        {
            if(values[0]==0.0)
                ui->checkBoxVolumeT->setChecked(false);
            else
                ui->checkBoxVolumeT->setChecked(true);
        }
        else if(flag=="setWalls")
        {
            ui->checkBoxFastMeshing->setChecked(false);
        }
        else if(flag=="Optimization")
        {
            ui->checkBoxFlagO->setChecked(true);
            ui->sliderFlagO->setValue(values[0]);
        }
        else if (flag=="MinDihedral")
        {
        ui->MinDihedralAngleCheckBox->setChecked(true);
        ui->MinDihedralAngleSpinBox->setValue(values[0]);
        }
        else if(flag=="QualityRatio")
        {
            ui->checkBoxFlagQ->setChecked(true);
            ui->sliderFlagQ->setValue(values[0]);
        }
        else if(flag=="Epsilon")
        {
            ui->checkBoxFlagT->setChecked(true);
            ui->lineEditFlagT->setText(QString::number(values[0]));
        }
        else if(flag=="NoBisect")
        {
            ui->checkBoxFlagY->setChecked(true);
        }
        else if(flag=="NoMerge")
        {
            ui->checkBoxFlagM->setChecked(true);
        }
        else if(flag=="Diagnose")
        {
            ui->checkBoxFlagD->setChecked(true);
        }
        else if(flag=="Check")
        {
            ui->checkBoxFlagC->setChecked(true);
        }
        else if(flag=="Quiet")
        {
            ui->checkBoxFlagQ2->setChecked(true);
        }
        else if(flag=="Verbose")
        {
            ui->checkBoxFlagV->setChecked(true);
        }
        else if(flag=="LocalEdgeSize")
        {
            int faceID=modelElement->GetFaceID(strValues[0]);

            for(int j=0;j<m_TableModelLocal->rowCount(); j++)
            {
                QStandardItem* itemID= m_TableModelLocal->item(j,0);
                int id=itemID->text().toInt();

                if(faceID==id)
                {
                    QStandardItem* item= m_TableModelLocal->item(j,3);
                    item->setText(QString::number(values[0]));
                    break;
                }
            }
        }
        else if(flag=="sphereRefinement")
        {
            regionRowIndex++;
            m_TableModelRegion->insertRow(regionRowIndex);

            QStandardItem* item;

            item= new QStandardItem("Sphere");
            item->setEditable(false);
            m_TableModelRegion->setItem(regionRowIndex, 0, item);

            item= new QStandardItem(QString::number(values[0]));
            m_TableModelRegion->setItem(regionRowIndex, 1, item);

            item= new QStandardItem(QString::number(values[1])+" "+QString::number(values[2])+" "+QString::number(values[3])+" "+QString::number(values[4]));
            item->setEditable(false);
            m_TableModelRegion->setItem(regionRowIndex, 2, item);
        }
        else if (flag=="AddHole")
        {
          domainsRowIndex++;
          m_TableModelDomains->insertRow(domainsRowIndex);

          QStandardItem* item;

          item= new QStandardItem("Hole");
          item->setEditable(false);
          m_TableModelDomains->setItem(domainsRowIndex, 0, item);

          item= new QStandardItem("N/A");
          item->setEditable(false);
          m_TableModelDomains->setItem(domainsRowIndex, 1, item);

          item= new QStandardItem(QString::number(values[0])+" "+QString::number(values[1])+" "+QString::number(values[2]));
          m_TableModelDomains->setItem(domainsRowIndex, 2, item);
        }
        else if (flag=="AddSubDomain")
        {
          domainsRowIndex++;
          m_TableModelDomains->insertRow(domainsRowIndex);

          QStandardItem* item;

          item= new QStandardItem("SubDomain");
          item->setEditable(false);
          m_TableModelDomains->setItem(domainsRowIndex, 0, item);

          item= new QStandardItem(QString::number(values[0]));
          m_TableModelDomains->setItem(domainsRowIndex, 1, item);

          item= new QStandardItem(QString::number(values[1])+" "+QString::number(values[2])+" "+QString::number(values[3]));
          m_TableModelDomains->setItem(domainsRowIndex, 2, item);
        }
        else if (flag == "NewRegionBoundaryLayer")
        {
          ui->checkBoxConvertBLToNewRegion->setChecked(true);
        }
        else if (flag == "BoundaryLayerDirection")
        {
          ui->checkBoxBoundaryLayerDirection->setChecked(true);
        }
        else
        {
            //do nothing
        }

    }

    //adaptor options
//    ui->lineEditMaxEdgeSize->setText(ui->lineEditGlobalEdgeSizeT->text());
    ui->comboBoxStrategy->setCurrentIndex(0);
    ui->comboBoxStrategy->setEnabled(false);
}

void sv4guiMeshEdit::UpdateMeshSimGUI()
{
    //put default values
    //==========================================

    //global size
    ui->comboBoxGlobalTypeM->setCurrentIndex(0);
    ui->comboBoxGlobalSizeTypeM->setCurrentIndex(0);
    ui->lineEditGlobalSizeM->clear();

    //advanced options
    ui->checkBoxSurfaceM->setChecked(true);
    ui->checkBoxSurfaceOptimizationM->setChecked(true);
    ui->sliderPassesM->setValue(3);

    ui->checkBoxVolumeM->setChecked(true);
    ui->checkBoxVolumeSmoothingM->setChecked(true);
    ui->checkBoxVolumeOptimizationM->setChecked(true);

    ui->checkBoxWriteStatM->setChecked(false);

    //local table
    ui->tableViewLocal->setItemDelegateForColumn(3,m_CustomDelegate);

    m_TableModelLocal->clear();

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==nullptr) return;

    std::vector<sv4guiModelElement::svFace*> faces=modelElement->GetFaces();

    QStringList faceListHeaders;
    faceListHeaders << "ID" << "Name" << "Type"
                    << "LType" << "LSizeType" <<"LSize"
                    << "BType" << "Direction" <<"Layers" << "Params";
                       ;
    int faceidColIndex=0;
    int facenameColIndex=1;
    int facetypeColIndex=2;
    int ltypeColIndex=3;
    int lsizetypeColIndex=4;
    int lsizeColIndex=5;
    int btypeColIndex=6;
    int bdirectionColIndex=7;
    int layernumberColIndex=8;
    int paramsColIndex=9;
    m_TableModelLocal->setHorizontalHeaderLabels(faceListHeaders);
    m_TableModelLocal->setColumnCount(faceListHeaders.size());

    int rowIndex=-1;

    for(int i=0;i<faces.size();i++)
    {
        if(faces[i]==nullptr )
            continue;

        rowIndex++;
        m_TableModelLocal->insertRow(rowIndex);

        QStandardItem* item;

        item= new QStandardItem(QString::number(faces[i]->id));
        item->setEditable(false);
        m_TableModelLocal->setItem(rowIndex, faceidColIndex, item);

        item= new QStandardItem(QString::fromStdString(faces[i]->name));
        item->setEditable(false);
        m_TableModelLocal->setItem(rowIndex, facenameColIndex, item);

        item= new QStandardItem(QString::fromStdString(faces[i]->type));
        item->setEditable(false);
        m_TableModelLocal->setItem(rowIndex, facetypeColIndex, item);

        item= new QStandardItem("Max Edge");
        m_TableModelLocal->setItem(rowIndex, ltypeColIndex, item);

        item= new QStandardItem("absolute");
        m_TableModelLocal->setItem(rowIndex, lsizetypeColIndex, item);

        item= new QStandardItem("");
        m_TableModelLocal->setItem(rowIndex, lsizeColIndex, item);

        item= new QStandardItem("(1)t0 tb");
        item->setToolTip("t0: first layer height\ntb: total height\ntn-1: last layer height\ng: gradation factor(0<g<1)");
        m_TableModelLocal->setItem(rowIndex, btypeColIndex, item);

        item= new QStandardItem("both");
        m_TableModelLocal->setItem(rowIndex, bdirectionColIndex, item);

        item= new QStandardItem("");
        m_TableModelLocal->setItem(rowIndex, layernumberColIndex, item);

        item= new QStandardItem("");
        m_TableModelLocal->setItem(rowIndex, paramsColIndex, item);
    }

    ui->tableViewLocal->horizontalHeader()->setSectionResizeMode(faceidColIndex, QHeaderView::Fixed);
    ui->tableViewLocal->horizontalHeader()->resizeSection(faceidColIndex,20);
    ui->tableViewLocal->setColumnHidden(faceidColIndex,true);

    ui->tableViewLocal->horizontalHeader()->setSectionResizeMode(facenameColIndex, QHeaderView::Interactive);

    ui->tableViewLocal->horizontalHeader()->setSectionResizeMode(facetypeColIndex, QHeaderView::Fixed);
    ui->tableViewLocal->horizontalHeader()->resizeSection(facetypeColIndex,40);

    ui->tableViewLocal->horizontalHeader()->setSectionResizeMode(ltypeColIndex, QHeaderView::Fixed);
    ui->tableViewLocal->horizontalHeader()->resizeSection(ltypeColIndex,80);

    ui->tableViewLocal->horizontalHeader()->setSectionResizeMode(lsizetypeColIndex, QHeaderView::Fixed);
    ui->tableViewLocal->horizontalHeader()->resizeSection(lsizetypeColIndex,80);

    ui->tableViewLocal->horizontalHeader()->setSectionResizeMode(lsizeColIndex, QHeaderView::Fixed);
    ui->tableViewLocal->horizontalHeader()->resizeSection(lsizeColIndex,60);

    ui->tableViewLocal->horizontalHeader()->setSectionResizeMode(btypeColIndex, QHeaderView::Fixed);
    ui->tableViewLocal->horizontalHeader()->resizeSection(btypeColIndex,80);

    ui->tableViewLocal->horizontalHeader()->setSectionResizeMode(bdirectionColIndex, QHeaderView::Fixed);
    ui->tableViewLocal->horizontalHeader()->resizeSection(bdirectionColIndex,80);

    ui->tableViewLocal->horizontalHeader()->setSectionResizeMode(layernumberColIndex, QHeaderView::Fixed);
    ui->tableViewLocal->horizontalHeader()->resizeSection(layernumberColIndex,60);

    ui->tableViewLocal->horizontalHeader()->setSectionResizeMode(paramsColIndex, QHeaderView::Interactive);
//    ui->tableViewLocal->horizontalHeader()->resizeSection(paramsColIndex,80);

    UpdateFaceListSelection();

    //regional refinement
    m_TableModelRegion->clear();

    QStringList regionListHeaders;
    regionListHeaders << "Type" << "Local Size" << "Radius x y z";
    m_TableModelRegion->setHorizontalHeaderLabels(regionListHeaders);
    m_TableModelRegion->setColumnCount(regionListHeaders.size());

    ui->tableViewRegion->horizontalHeader()->setSectionResizeMode(0, QHeaderView::Fixed);
    ui->tableViewRegion->horizontalHeader()->resizeSection(0,80);
    ui->tableViewRegion->horizontalHeader()->setSectionResizeMode(1, QHeaderView::Fixed);
    ui->tableViewRegion->horizontalHeader()->resizeSection(1,80);
    ui->tableViewRegion->horizontalHeader()->setSectionResizeMode(2, QHeaderView::Interactive);

    //then udpate with command history
    //========================================
    sv4guiMesh* mesh=m_MitkMesh->GetMesh(GetTimeStep());
    if(mesh==nullptr)
        return;

    std::vector<std::string> cmdHistory=mesh->GetCommandHistory();
    std::string flag="";
    double values[20]={0};
    std::string strValues[5]={""};
    bool option=false;
    std::string msg="";
    int regionRowIndex=-1;

    for(int i=0;i<cmdHistory.size();i++)
    {
        if(cmdHistory[i]=="")
            continue;

        if(!mesh->ParseCommand(cmdHistory[i],flag,values,strValues,option,msg))
        {
            QMessageBox::warning(m_Parent,"Parsing Error","Error in parsing command history!");
            return;
        }

        if(flag=="GlobalEdgeSize" || flag=="GlobalCurvature" || flag=="GlobalCurvatureMin" )
        {
            if(flag=="GlobalEdgeSize")
                ui->comboBoxGlobalTypeM->setCurrentIndex(0);
            else if(flag=="GlobalCurvature")
                ui->comboBoxGlobalTypeM->setCurrentIndex(1);
            else
                ui->comboBoxGlobalTypeM->setCurrentIndex(2);

            if(values[0]==1)
                ui->comboBoxGlobalSizeTypeM->setCurrentIndex(0);
            else
                ui->comboBoxGlobalSizeTypeM->setCurrentIndex(1);

            ui->lineEditGlobalSizeM->setText(QString::number(values[1]));
        }
        else if(flag=="SurfaceMeshFlag")
        {
            if(values[0]==0.0)
                ui->checkBoxSurfaceM->setChecked(false);
            else
                ui->checkBoxSurfaceM->setChecked(true);
        }
        else if(flag=="SurfaceOptimization")
        {
            if(values[0]==0.0)
                ui->checkBoxSurfaceOptimizationM->setChecked(false);
            else
                ui->checkBoxSurfaceOptimizationM->setChecked(true);
        }
        else if(flag=="SurfaceSmoothing")
        {
            ui->sliderPassesM->setValue(values[0]);
        }
        else if(flag=="VolumeMeshFlag")
        {
            if(values[0]==0.0)
                ui->checkBoxVolumeM->setChecked(false);
            else
                ui->checkBoxVolumeM->setChecked(true);
        }
        else if(flag=="VolumeOptimization")
        {
            if(values[0]==0.0)
                ui->checkBoxVolumeOptimizationM->setChecked(false);
            else
                ui->checkBoxVolumeOptimizationM->setChecked(true);
        }
        else if(flag=="VolumeSmoothing")
        {
            if(values[0]==0.0)
                ui->checkBoxVolumeSmoothingM->setChecked(false);
            else
                ui->checkBoxVolumeSmoothingM->setChecked(true);
        }
        else if(flag=="LocalEdgeSize" || flag=="LocalCurvature" || flag=="LocalCurvatureMin")
        {
            int faceID=modelElement->GetFaceID(strValues[0]);

            for(int j=0;j<m_TableModelLocal->rowCount(); j++)
            {
                QStandardItem* itemID= m_TableModelLocal->item(j,faceidColIndex);
                int id=itemID->text().toInt();

                if(faceID==id)
                {
                    QStandardItem* item= m_TableModelLocal->item(j,ltypeColIndex);
                    if(flag=="LocalEdgeSize")
                        item->setText("Max Edge");
                    else if(flag=="LocalCurvature")
                        item->setText("Max Curv");
                    else
                        item->setText("Min Curv");

                    item= m_TableModelLocal->item(j,lsizetypeColIndex);
                    if(values[1]==1)
                        item->setText("absolute");
                    else
                        item->setText("relative");

                    item= m_TableModelLocal->item(j,lsizeColIndex);
                    item->setText(QString::number(values[2]));

                    break;
                }
            }
        }
        else if(flag=="boundaryLayer")
        {
            int faceID=modelElement->GetFaceID(strValues[0]);

            for(int j=0;j<m_TableModelLocal->rowCount(); j++)
            {
                QStandardItem* itemID= m_TableModelLocal->item(j,faceidColIndex);
                int id=itemID->text().toInt();

                if(faceID==id)
                {
                    QStandardItem* item= m_TableModelLocal->item(j,btypeColIndex);
                    if(values[1]==1)
                        item->setText("(1)t0 tb");
                    else if(values[1]==2)
                        item->setText("(2)t0 g");
                    else if(values[1]==3)
                        item->setText("(3)t0 ... tn-1");
                    else if(values[1]==4)
                        item->setText("(4)g");

                    item= m_TableModelLocal->item(j,bdirectionColIndex);
                    if(values[2]==2)
                        item->setText("both");
                    else if(values[2]==1)
                        item->setText("positive");
                    else if(values[2]==0)
                        item->setText("negative");

                    item= m_TableModelLocal->item(j,layernumberColIndex);
                    item->setText(QString::number(int(values[3])));

                    QString params="";
                    for(int i=0;i<values[4];i++)
                        params=params+" "+QString::number(values[5+i]);

                    item= m_TableModelLocal->item(j,paramsColIndex);
                    item->setText(params);

                    break;
                }
            }
        }
        else if(flag=="sphereRefinement")
        {
            regionRowIndex++;
            m_TableModelRegion->insertRow(regionRowIndex);

            QStandardItem* item;

            item= new QStandardItem("Sphere");
            item->setEditable(false);
            m_TableModelRegion->setItem(regionRowIndex, 0, item);

            item= new QStandardItem(QString::number(values[0]));
            m_TableModelRegion->setItem(regionRowIndex, 1, item);

            item= new QStandardItem(QString::number(values[1])+" "+QString::number(values[2])+" "+QString::number(values[3])+" "+QString::number(values[4]));
            item->setEditable(false);
            m_TableModelRegion->setItem(regionRowIndex, 2, item);
        }
        else if(flag=="writeStats")
        {
            ui->checkBoxWriteStatM->setChecked(true);
        }
        else
        {
            //do nothing
        }

    }

    //adaptor options
//    ui->lineEditMaxEdgeSize->setText(ui->lineEditGlobalSizeM->text());
    ui->comboBoxStrategy->setEnabled(true);
}

void sv4guiMeshEdit::AddSphere()
{
    if(m_SphereWidget==nullptr || !m_SphereWidget->GetEnabled() || m_SphereWidget->GetRadius()==0)
        return;

    int regionRowIndex=m_TableModelRegion->rowCount();

    QStandardItem* item;

    item= new QStandardItem("Sphere");
    item->setEditable(false);
    m_TableModelRegion->setItem(regionRowIndex, 0, item);

    item= new QStandardItem("");
    m_TableModelRegion->setItem(regionRowIndex, 1, item);

    double center[3];
    m_SphereWidget->GetCenter(center);
    item= new QStandardItem(QString::number(m_SphereWidget->GetRadius())+" "+QString::number(center[0])+" "+QString::number(center[1])+" "+QString::number(center[2]));
    m_TableModelRegion->setItem(regionRowIndex, 2, item);
}

void sv4guiMeshEdit::AddHole()
{
    int regionRowIndex=m_TableModelDomains->rowCount();

    QStandardItem* item;

    item= new QStandardItem("Hole");
    item->setEditable(false);
    m_TableModelDomains->setItem(regionRowIndex, 0, item);

    item= new QStandardItem("N/A");
    item->setEditable(false);
    m_TableModelDomains->setItem(regionRowIndex, 1, item);

    // [DaveP] what's going on here ?
    std::cout << "GetCrossPosition does not exist" << std::endl << std::flush;
    exit(1);

    // mitk::Point3D point=m_DisplayWidget->GetCrossPosition();
    // QString coordinates=QString::number(point[0])+" "+QString::number(point[1])+" "+QString::number(point[2]);

    // item= new QStandardItem(coordinates);
    // m_TableModelDomains->setItem(regionRowIndex, 2, item);
}

void sv4guiMeshEdit::AddSubDomain()
{
    int regionRowIndex=m_TableModelDomains->rowCount();

    QStandardItem* item;

    item= new QStandardItem("SubDomain");
    item->setEditable(false);
    m_TableModelDomains->setItem(regionRowIndex, 0, item);

    item= new QStandardItem("");
    m_TableModelDomains->setItem(regionRowIndex, 1, item);

    // [DaveP] what's going on here ?
    std::cout << "GetCrossPosition does not exist" << std::endl << std::flush;
    exit(1);
    // mitk::Point3D point=m_DisplayWidget->GetCrossPosition();
    // QString coordinates=QString::number(point[0])+" "+QString::number(point[1])+" "+QString::number(point[2]);

    // item= new QStandardItem(coordinates);
    // m_TableModelDomains->setItem(regionRowIndex, 2, item);
}

void sv4guiMeshEdit::ShowSphereInteractor(bool checked)
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
    sv4guiModelElement* modelElement=dynamic_cast<sv4guiModelElement*>(m_Model->GetModelElement(timeStep));

    if(modelElement==nullptr) return;

    if(modelElement->GetWholeVtkPolyData()==nullptr) return;

    m_SphereWidget->SetInputData(modelElement->GetWholeVtkPolyData());
    m_SphereWidget->PlaceWidget();

    m_SphereWidget->On();

    TableRegionListSelectionChanged(QItemSelection(),QItemSelection());
}

void sv4guiMeshEdit::UpdateSphereData()
{
    if(m_SelectedRegionIndex>-1 && m_SphereWidget->GetRadius()>0)
    {
        QStandardItem* item= m_TableModelRegion->item(m_SelectedRegionIndex,2);
        if(item)
        {
            double center[3];
            m_SphereWidget->GetCenter(center);
            item->setText(QString::number(m_SphereWidget->GetRadius())+" "+QString::number(center[0])+" "+QString::number(center[1])+" "+QString::number(center[2]));
        }
    }
}

void sv4guiMeshEdit::UpdateFaceListSelection()
{
    if(!m_Model)
        return;

    int timeStep=GetTimeStep();
    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==nullptr) return;

    QStandardItemModel* tableModel=m_TableModelLocal;
    QTableView* tableView=ui->tableViewLocal;

    if(tableModel==nullptr || tableView==nullptr)
        return;

    disconnect( tableView->selectionModel()
      , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
      , this
      , SLOT( TableFaceListSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );


    tableView->clearSelection();

    int count=tableModel->rowCount();

    for(int i=0;i<count;i++)
    {
        QStandardItem* itemID= tableModel->item(i,0);
        int id=itemID->text().toInt();

        if(modelElement->IsFaceSelected(id))
        {
            QModelIndex mIndex=tableModel->index(i,1);
            tableView->selectionModel()->select(mIndex, QItemSelectionModel::Select|QItemSelectionModel::Rows);
        }
    }

    connect( tableView->selectionModel()
      , SIGNAL( selectionChanged ( const QItemSelection &, const QItemSelection & ) )
      , this
      , SLOT( TableFaceListSelectionChanged ( const QItemSelection &, const QItemSelection & ) ) );

}

void sv4guiMeshEdit::NodeChanged(const mitk::DataNode* node)
{
    if(m_MeshNode==node)
    {
        ui->labelMeshName->setText(QString::fromStdString(m_MeshNode->GetName()));
    }
}

void sv4guiMeshEdit::NodeAdded(const mitk::DataNode* node)
{
}

void sv4guiMeshEdit::NodeRemoved(const mitk::DataNode* node)
{
}

void sv4guiMeshEdit::AddObservers()
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
        m_ModelNode->SetStringProperty("interactor user","meshing");
        sv4guiModelDataInteractor* interactor=dynamic_cast<sv4guiModelDataInteractor*>(m_ModelNode->GetDataInteractor().GetPointer());
        if(interactor)
            interactor->SetFaceSelectionOnly();
    }

    if(m_Model && m_ModelSelectFaceObserverTag==-1)
    {
        itk::SimpleMemberCommand<sv4guiMeshEdit>::Pointer modelSelectFaceCommand = itk::SimpleMemberCommand<sv4guiMeshEdit>::New();
        modelSelectFaceCommand->SetCallbackFunction(this, &sv4guiMeshEdit::UpdateFaceListSelection);
        m_ModelSelectFaceObserverTag = m_Model->AddObserver( sv4guiModelSelectFaceEvent(), modelSelectFaceCommand);
    }

    sv4guiVtkMeshSphereWidget* sphereWidget=dynamic_cast<sv4guiVtkMeshSphereWidget*>(m_SphereWidget.GetPointer());
    if(sphereWidget)
        sphereWidget->AddMyObserver();
}

void sv4guiMeshEdit::RemoveObservers()
{
    if(m_Model && m_ModelSelectFaceObserverTag!=-1)
    {
        m_Model->RemoveObserver(m_ModelSelectFaceObserverTag);
        m_ModelSelectFaceObserverTag=-1;
    }

    if(m_ModelNode)
    {
        std::string user="";
        m_ModelNode->GetStringProperty("interactor user", user);
        if(user=="meshing")
            m_ModelNode->SetDataInteractor(nullptr);
    }
    m_DataInteractor=nullptr;

    sv4guiVtkMeshSphereWidget* sphereWidget=dynamic_cast<sv4guiVtkMeshSphereWidget*>(m_SphereWidget.GetPointer());
    if(sphereWidget)
    {
        sphereWidget->RemoveMyObserver();
    }
}

void sv4guiMeshEdit::ClearAll()
{
    m_Model=nullptr;
    m_MeshNode=nullptr;
    m_MitkMesh=nullptr;
    m_ModelNode=nullptr;

    ui->labelMeshName->setText("");
    ui->labelMeshType->setText("");
    ui->labelModelName->setText("");
}

void sv4guiMeshEdit::DisplayMeshInfo()
{
    if(m_MeshNode.IsNull())
        return;

    sv4guiMitkMesh* mitkMesh=dynamic_cast<sv4guiMitkMesh*>(m_MeshNode->GetData());
    if(!mitkMesh)
        return;

    sv4guiMesh* mesh=mitkMesh->GetMesh();
    if(!mesh)
        return;

    std::string path="";
    m_MeshNode->GetStringProperty("path",path);
    std::string surfaceFileName = path+"/"+m_MeshNode->GetName()+".vtp";
    std::string volumeFileName = path+"/"+m_MeshNode->GetName()+".vtu";

    vtkSmartPointer<vtkPolyData> surfaceMesh=mesh->GetSurfaceMesh();
    if(surfaceMesh==nullptr && path!="")
    {
        surfaceMesh=mesh->CreateSurfaceMeshFromFile(surfaceFileName);
    }

    vtkSmartPointer<vtkUnstructuredGrid> volumeMesh=mesh->GetVolumeMesh();
    if(volumeMesh==nullptr && path!="")
    {
        volumeMesh=mesh->CreateVolumeMeshFromFile(volumeFileName);
    }

    int num_nodes = 0;
    int nMeshFaces = 0;
    int num_elems = 0;
    int nMeshEdges = 0;

    if(surfaceMesh)
    {
      nMeshFaces = surfaceMesh->GetNumberOfCells();
      nMeshEdges=nMeshFaces*3/2;
    }

    if(volumeMesh)
    {
      num_nodes = volumeMesh->GetNumberOfPoints();
      num_elems = volumeMesh->GetNumberOfCells();
    }

    QString stat="Number of Nodes: " + QString::number(num_nodes)
            + "\n" + "Number of Elems: " + QString::number(num_elems)
            + "\n" + "Number of Edges: " + QString::number(nMeshEdges)
            + "\n" + "Number of Faces: " + QString::number(nMeshFaces);

    std::cout << stat << std::endl << std::flush;

    QMessageBox::information(m_Parent,"Mesh Statistics","Mesh done. Statistics:           \n\n"+stat);
}

void sv4guiMeshEdit::UpdateAdaptGUI(int selected)
{
    switch(selected)
    {
    case 0:
        ui->widgetStartStep->hide();
        ui->widgetStepIncrement->hide();
        ui->labelEndStep->setText("Step Number:");
        break;
    case 1:
        ui->widgetStartStep->show();
        ui->widgetStepIncrement->show();
        ui->labelEndStep->setText("End Step Number:");
        break;
    }
}

void sv4guiMeshEdit::SetResultFile()
{
    QString lastFileOpenPath=ui->lineEditResultFile->text().trimmed();

    mitk::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    mitk::IPreferences* prefs;

    if (prefService)
    {
        prefs = prefService->GetSystemPreferences()->Node("/General");
    }
    else
    {
        prefs = nullptr; 
    }

    if(lastFileOpenPath=="" || !QFile(lastFileOpenPath).exists())
    {
        lastFileOpenPath="";

        if(prefs != nullptr)
        {
            lastFileOpenPath = QString::fromStdString(prefs->Get("LastFileOpenPath", ""));
        }
        if(lastFileOpenPath=="")
            lastFileOpenPath=QDir::homePath();
    }

    QString resultVtuFile = QFileDialog::getOpenFileName(m_Parent, tr("Select Result VTU File")
                                                            , lastFileOpenPath
                                                            , tr("VTU Files (*.vtu)"));
    resultVtuFile=resultVtuFile.trimmed();
    if(resultVtuFile.isEmpty())
        return;

    if(prefs != nullptr)
     {
         prefs->Put("LastFileOpenPath", resultVtuFile.toStdString());
         prefs->Flush();
     }

    ui->lineEditResultFile->setText(resultVtuFile);
}

void sv4guiMeshEdit::Adapt()
{
    if(m_MeshNode.IsNull())
        return;

    if(m_ModelNode.IsNull())
        return;

    if(m_Model==nullptr)
        return;

    sv4guiModelElement* modelElement=m_Model->GetModelElement();
    if(modelElement==nullptr)
        return;

    bool ok;
    QString adaptedMeshName = QInputDialog::getText(m_Parent, tr("Name for Adapted Mesh"),
                                                    tr("Mesh Name:"), QLineEdit::Normal,
                                                    QString("adapted-")+QString::fromStdString(m_MeshNode->GetName()), &ok);
    if (!ok || adaptedMeshName.isEmpty())
        return;

    mitk::DataNode::Pointer meshFolderNode=nullptr;
    std::string meshFolderPath="";
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources(m_MeshNode,isProjFolder,false);

    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);
        std::string projPath="";
        projFolderNode->GetStringProperty("project path", projPath);

        rs=GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiMeshFolder"));
        if (rs->size()>0)
        {
            meshFolderNode=rs->GetElement(0);
            meshFolderPath=projPath+"/"+meshFolderNode->GetName();
        }
    }
    if(meshFolderPath=="")
        return;

    int currentIndex=ui->comboBoxOption->currentIndex();

    //check if user inputs are valid
    QString resultFile=ui->lineEditResultFile->text().trimmed();
    if(!QFile(resultFile).exists())
    {
        QMessageBox::warning(m_Parent,"File not existing","Make sure the result vtu file exists!");
        return;
    }

    QString startStep="0";
    QString stepInc="1";
    if(currentIndex==1)
    {
        startStep =ui->lineEditStartStep->text().trimmed();
        if(!IsInt(startStep))
        {
            QMessageBox::warning(m_Parent,"Format error","Please provide start step in a correct format!");
            return;
        }
        stepInc =ui->lineEditStepIncrement->text().trimmed();
        if(!IsInt(stepInc))
        {
            QMessageBox::warning(m_Parent,"Format error","Please provide step increment in a correct format!");
            return;
        }
    }

    QString endStep=ui->lineEditEndStep->text().trimmed();
    if(!IsInt(endStep))
    {
        QMessageBox::warning(m_Parent,"Format error","Please provide step number in a correct format!");
        return;
    }

    QString errorFactor=ui->lineEditErrorFactor->text().trimmed();
    if(!IsDouble(errorFactor))
    {
        QMessageBox::warning(m_Parent,"Format error","Please provide erro factor in a correct format!");
        return;
    }

    QString minSize=ui->lineEditMinEdgeSize->text().trimmed();
    if(!IsDouble(minSize))
    {
        QMessageBox::warning(m_Parent,"Format error","Please provide min edge size in a correct format!");
        return;
    }

    QString maxSize=ui->lineEditMaxEdgeSize->text().trimmed();
    if(!IsDouble(maxSize))
    {
        QMessageBox::warning(m_Parent,"Format error","Please provide max edge size in a correct format!");
        return;
    }

    sv4guiMeshAdaptor* adaptor=sv4guiMeshFactory::CreateAdaptor(m_MeshType);
    if(adaptor==nullptr)
    {
        QMessageBox::warning(m_Parent,"No Adaptor","Failed in creating adaptor!");
        return;
    }

    if(!adaptor->SetModelElement(modelElement))
    {
        QMessageBox::warning(m_Parent,"Error","Failed in loading model.");
        delete adaptor;
        return;
    }

    if(m_MeshType=="MeshSim")
    {
        std::string meshFolderPath=GetMeshFolderPath().trimmed().toStdString();
        std::string originalMeshFile=meshFolderPath+"/"+m_MeshNode->GetName()+".sms";
        if(!adaptor->LoadMesh(originalMeshFile))
        {
            QMessageBox::warning(m_Parent,"Error","Failed in loading original mesh.");
            delete adaptor;
            return;
        }
    }

    if(!adaptor->LoadMesh(resultFile.toStdString()))
    {
        QMessageBox::warning(m_Parent,"Error","Failed in loading result mesh.");
        delete adaptor;
        return;
    }

    int strategy=ui->comboBoxStrategy->currentIndex()+1;
    adaptor->SetAdaptOptions("strategy",strategy);
    adaptor->SetAdaptOptions("metric_option",currentIndex+2);
    adaptor->SetAdaptOptions("outstep",endStep.toInt());
    adaptor->SetAdaptOptions("ratio",errorFactor.toDouble());
    adaptor->SetAdaptOptions("hmin",minSize.toDouble());
    adaptor->SetAdaptOptions("hmax",maxSize.toDouble());
    if(currentIndex==1)
    {
        adaptor->SetAdaptOptions("instep",startStep.toInt());
        adaptor->SetAdaptOptions("step_incr",stepInc.toInt());
    }

    if(!adaptor->Adapt())
    {
        QMessageBox::warning(m_Parent,"Error","Failed in adapting the mesh.");
        delete adaptor;
        return;
    }

    if(m_MeshType=="MeshSim")
    {
        QString adaptedMeshSMSFilePath=QString::fromStdString(meshFolderPath)+"/"+adaptedMeshName+".sms";
        adaptedMeshSMSFilePath=QDir::toNativeSeparators(adaptedMeshSMSFilePath);
        if(!adaptor->WriteAdaptedMesh(adaptedMeshSMSFilePath.toStdString()))
        {
            QMessageBox::warning(m_Parent,"Error","Failed in write adapted sms mesh.");
            delete adaptor;
            return;
        }
    }

    sv4guiMesh* adaptedMesh=adaptor->GetAdaptedMesh();
    if(adaptedMesh==nullptr)
    {
        QMessageBox::warning(m_Parent,"Error","Failed in getting adapted mesh.");
        return;
    }

    std::string resultFileString = resultFile.toStdString();
    std::string resultPathName;
    int split = resultFileString.find_last_of("/\\");
    if (split < 0)
      resultPathName = ".";
    else
      resultPathName = resultFileString.substr(0, split);

    //QString solutionFilePath=QString::fromStdString(meshFolderPath)+"/adapted-restart."+endStep+".1";
    //solutionFilePath=QDir::toNativeSeparators(solutionFilePath);
    std::string solutionFileName = resultPathName + "/adapted-restart." + endStep.toStdString() + ".1";

    if(!adaptor->WriteAdaptedSolution(solutionFileName))
    {
        QMessageBox::warning(m_Parent,"Error","Failed in writing adapted solution (restart).");
        delete adaptor;
        return;
    }

    delete adaptor;

    std::vector<std::string> cmds;
    if(m_MeshType=="TetGen")
        cmds.push_back("option GlobalEdgeSize "+maxSize.toStdString());
    else if(m_MeshType=="MeshSim")
        cmds.push_back("option GlobalEdgeSize 1 "+maxSize.toStdString());

    adaptedMesh->SetCommandHistory(cmds);

    sv4guiMitkMesh::Pointer mitkMesh = sv4guiMitkMesh::New();
    mitkMesh->SetModelName(m_ModelNode->GetName());
    mitkMesh->SetType(adaptedMesh->GetType());
    mitkMesh->SetMesh(adaptedMesh);
    mitkMesh->SetDataModified();

    mitk::DataNode::Pointer meshNode = mitk::DataNode::New();
    meshNode->SetData(mitkMesh);
    meshNode->SetName(adaptedMeshName.toStdString());

    bool undoEnabled=true;
    if(undoEnabled)
        mitk::OperationEvent::IncCurrObjectEventId();

    sv4guiDataNodeOperation* doOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpADDDATANODE,GetDataStorage(),meshNode,meshFolderNode);
    if(undoEnabled)
    {
        sv4guiDataNodeOperation* undoOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpREMOVEDATANODE,GetDataStorage(),meshNode,meshFolderNode);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Interface, doOp, undoOp, "Add DataNode");
        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
    }
    m_Interface->ExecuteOperation(doOp);
}

bool sv4guiMeshEdit::IsInt(QString value)
{
    bool ok;
    value.toInt(&ok);
    return ok;
}

bool sv4guiMeshEdit::IsDouble(QString value)
{
    bool ok;
    value.toDouble(&ok);
    return ok;
}

QString sv4guiMeshEdit::GetMeshFolderPath()
{
    QString meshFolderPath="";

    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=GetDataStorage()->GetSources (m_MeshNode,isProjFolder,false);

    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        std::string projPath;
        projFolderNode->GetStringProperty("project path",projPath);

        rs=GetDataStorage()->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiMeshFolder"));
        if(rs->size()>0)
        {
            mitk::DataNode::Pointer meshFolderNode=rs->GetElement(0);
            std::string meshFolderName=meshFolderNode->GetName();

            meshFolderPath=QString::fromStdString(projPath+"/"+meshFolderName);
        }
    }

    return meshFolderPath;
}

void sv4guiMeshEdit::ShowModel(bool checked)
{
    if(m_ModelNode.IsNotNull())
    {
        m_ModelNode->SetVisibility(checked);
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }
}
