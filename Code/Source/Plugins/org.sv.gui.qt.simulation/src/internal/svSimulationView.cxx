#include "svSimulationView.h"
#include "ui_svSimulationView.h"

#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>
#include <mitkSliceNavigationController.h>
#include <mitkProgressBar.h>
#include <mitkStatusBar.h>

#include <usModuleRegistry.h>

#include <QTreeView>
#include <QInputDialog>
#include <QMessageBox>

const QString svSimulationView::EXTENSION_ID = "org.sv.views.simulation";

svSimulationView::svSimulationView() :
    ui(new Ui::svSimulationView)
{
    m_MitkJob=NULL;
    m_Model=NULL;
    m_JobNode=NULL;
    m_ModelNode=NULL;

    m_DataInteractor=NULL;
    m_ModelSelectFaceObserverTag=0;

    m_TableModelCap=NULL;
    m_TableMenuCap=NULL;

    m_TableModelWall=NULL;
    m_TableMenuWall=NULL;

    m_CapBCWidget=NULL;
}

svSimulationView::~svSimulationView()
{
    delete ui;

    if(m_TableModelCap)
        delete m_TableModelCap;

    if(m_TableMenuCap)
        delete m_TableMenuCap;

    if(m_TableModelWall)
        delete m_TableModelWall;

    if(m_TableMenuWall)
        delete m_TableMenuWall;
}

void svSimulationView::CreateQtPartControl( QWidget *parent )
{
    m_Parent=parent;
    ui->setupUi(parent);

//    m_DisplayWidget=GetActiveStdMultiWidget();

//    if(m_DisplayWidget==NULL)
//    {
//        parent->setEnabled(false);
//        MITK_ERROR << "Plugin Simulation Init Error: No QmitkStdMultiWidget Available!";
//        return;
//    }

//    connect(ui->btnCreateDataFiles, SIGNAL(clicked()), this, SLOT(CreateDataFiles()) );

}
