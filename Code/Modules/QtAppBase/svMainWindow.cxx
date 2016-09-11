#include "svMainWindow.h"

#include "svApplication.h"
#include "svDisplayEditor.h"
#include "svQmitkDataManager.h"
#include "svQmitkImageNavigator.h"

#include "QmitkStatusBar.h"
#include <QmitkProgressBar.h>
#include <QmitkMemoryUsageIndicatorView.h>

#include <mitkIOUtil.h>
#include <mitkRenderingManager.h>

#include <ctkCollapsibleGroupBox.h>

#include <itksys/SystemTools.hxx>

#include <vtkRendererCollection.h>

#include <QSplitter>
#include <QSignalMapper>
#include <QStatusBar>
#include <QApplication>
#include <QFile>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QFileDialog>
#include <QMenuBar>
#include <QToolBar>
#include <QStatusBar>
#include <QMessageBox>
#include <QCloseEvent>

svMainWindow::svMainWindow(QWidget *parent) :
    QMainWindow(parent)
{
    setWindowTitle("SimVascular 3.0");

    //setup window central widget
    QWidget* centralWidget=new QWidget;
    setCentralWidget(centralWidget);

    // get DataStorage
    //========================================================
    m_DataStorage= svApplication::application()->dataStorage();

    // set up DisplatWidget
    //========================================================
    svDisplayEditor* displayEditor=new svDisplayEditor;
    m_DisplayWidget=displayEditor->GetDisplayWidget();
    svApplication::application()->setDisplayWidget(m_DisplayWidget);
    svApplication::application()->pythonManager()->addObjectToPythonMain("svDisplay", m_DisplayWidget);

    //set up DataManager
    //==============================================================
    svQmitkDataManager* dataManager= new svQmitkDataManager;
    svApplication::application()->setDataManager(dataManager);

    //set up ImageNavigator
    //==============================================================
    svQmitkImageNavigator* svImageNavigator= new svQmitkImageNavigator;
    //svApplication::application()->pythonManager()->addObjectToPythonMain("svNavigator", svImageNavigator);

    //Load plugins
    QFile xmlFile(":plugin.xml");
    svApplication::application()->setExtensionManager(new svExtensionManager(&xmlFile));

    //Layout
    ctkCollapsibleGroupBox *groupBox = new ctkCollapsibleGroupBox("Image Navigator", this);
    groupBox->setCollapsed(true);
    groupBox->setCollapsedHeight(0);
    QVBoxLayout *vbox = new QVBoxLayout;
    vbox->addWidget(svImageNavigator);
    vbox->setContentsMargins(0,5,0,0);
    groupBox->setLayout(vbox);

    QWidget* leftPanel=new QWidget(centralWidget);
    QVBoxLayout *leftLayout=new QVBoxLayout(leftPanel);
    leftLayout->setMargin(0);
    leftLayout->setSpacing(5);
    leftLayout->addWidget(dataManager);
    leftLayout->addWidget(groupBox);

    QWidget* centerPanel=new QWidget(centralWidget);
    QHBoxLayout *centerLayout=new QHBoxLayout(centerPanel);
    centerLayout->setMargin(0);
    centerLayout->setSpacing(5);

    centerLayout->addWidget(svApplication::application()->extensionManager()->getLeftPane());
    centerLayout->addWidget(displayEditor);
    centerPanel->resize(QSize(800,477).expandedTo(minimumSizeHint()));

    QSplitter* splitter = new QSplitter(centralWidget);
    splitter->addWidget(leftPanel);
    splitter->addWidget(centerPanel);
    splitter->setStretchFactor(0,1);
    splitter->setStretchFactor(1,4);

    QVBoxLayout *verticalLayout=new QVBoxLayout(centralWidget);
    verticalLayout->addWidget(splitter);

    initializePythonConsole();

    QMenu* currentMenu=NULL;
    QToolBar* currentToolBar;
    QAction* action;

    //Get extension info
    QList<svExtensionManager::svViewInfo*> viewList=svApplication::application()->extensionManager()->getViewList();

    //Add menus and toolbars for extensions
    QString currentMenuName="";
    QString currentToolBarName="";
    QSignalMapper* signalMapper = new QSignalMapper(this);

    for(int i=0;i<viewList.size();i++)
    {
        svExtensionManager::svViewInfo* viewInfo=viewList.at(i);

        if(viewInfo->type=="menu_separator" && currentMenu)
        {
            currentMenu->addSeparator();
            continue;
        }

        if(viewInfo->inMenu)
        {
            if(viewInfo->category!=currentMenuName)
            {
                currentMenu=menuBar()->addMenu(viewInfo->category);
                currentMenuName=viewInfo->category;
            }

            action=currentMenu->addAction(QIcon(":"+viewInfo->icon),viewInfo->name);
            signalMapper->setMapping(action, QString(viewInfo->id));
            connect(action, SIGNAL(triggered()),signalMapper, SLOT(map()));
        }

        if(viewInfo->inToolBar)
        {
            if(viewInfo->category!=currentToolBarName)
            {
                currentToolBar=addToolBar(viewInfo->category);
                currentToolBarName=viewInfo->category;
            }

            action=currentToolBar->addAction(QIcon(":"+viewInfo->icon),viewInfo->name);
            signalMapper->setMapping(action, QString(viewInfo->id));
            connect(action, SIGNAL(triggered()),signalMapper, SLOT(map()));
        }

    }

    connect(signalMapper, SIGNAL(mapped(QString)),svApplication::application()->extensionManager(), SLOT(useExtension(QString)));

    currentMenu=menuBar()->addMenu("Python");
    action=currentMenu->addAction(QIcon(":Python.png"),"Show Console");
    connect(action, SIGNAL(triggered()),this,  SLOT(on_actionPythonConsole_triggered()));

    currentMenu=menuBar()->addMenu("Display");
    action=currentMenu->addAction("Standard Layout");
    connect(action, SIGNAL(triggered()),this,  SLOT(on_actionLayoutStandard_triggered()));
    action=currentMenu->addAction("3D Only");
    connect(action, SIGNAL(triggered()),this,  SLOT(on_actionLayout3DOnly_triggered()));
    action=currentMenu->addAction("Axial Only");
    connect(action, SIGNAL(triggered()),this,  SLOT(on_actionAxialOnly_triggered()));
    action=currentMenu->addAction("Coronal Only");
    connect(action, SIGNAL(triggered()),this,  SLOT(on_actionCoronalOnly_triggered()));
    action=currentMenu->addAction("Sagittal Only");
    connect(action, SIGNAL(triggered()),this,  SLOT(on_actionSagittalOnly_triggered()));
    action=currentMenu->addAction("2D Left");
    connect(action, SIGNAL(triggered()),this,  SLOT(on_actionLayout2DLeft_triggered()));
    action=currentMenu->addAction("2D Top");
    connect(action, SIGNAL(triggered()),this,  SLOT(on_actionLayout2DTop_triggered()));

    currentToolBar=addToolBar("Python Interactor");
    action=currentToolBar->addAction(QIcon(":Python.png"),"Python Interactor");
    connect(action, SIGNAL(triggered()),this,  SLOT(on_actionPythonConsole_triggered()));

    //Add toolbar for display layout
    currentToolBar=addToolBar("Display Layout");
    action=currentToolBar->addAction(QIcon(":LayoutStandard.png"),"LayoutStandard");
    connect(action, SIGNAL(triggered()),this,  SLOT(on_actionLayoutStandard_triggered()));
    action=currentToolBar->addAction(QIcon(":Layout2DTop.png"),"Layout2DTop");
    connect(action, SIGNAL(triggered()),this,  SLOT(on_actionLayout2DTop_triggered()));
    action=currentToolBar->addAction(QIcon(":Layout2DLeft.png"),"Layout2DLeft");
    connect(action, SIGNAL(triggered()),this,  SLOT(on_actionLayout2DLeft_triggered()));
    action=currentToolBar->addAction(QIcon(":Layout3DOnly.png"),"Layout3DOnly");
    connect(action, SIGNAL(triggered()),this,  SLOT(on_actionLayout3DOnly_triggered()));

//    QStatusBar* statusBar=new QStatusBar(this);
//    setStatusBar(statusBar);

    auto   qStatusBar = new QStatusBar(this);
    auto  statusBar = new QmitkStatusBar(qStatusBar);
    statusBar->SetSizeGripEnabled(false);
    auto  progBar = new QmitkProgressBar();

    qStatusBar->addPermanentWidget(progBar, 0);
    progBar->hide();

    setStatusBar(qStatusBar);
    auto   memoryIndicator = new QmitkMemoryUsageIndicatorView();
    qStatusBar->addPermanentWidget(memoryIndicator, 0);

}

svMainWindow::~svMainWindow()
{
}

void svMainWindow::initializePythonConsole(){
    pythonConsole=new ctkPythonConsole();
    pythonConsole->setWindowTitle("SimVascular Python Interactor");

//  ===============
     pythonConsole->initialize(svApplication::application()->pythonManager());

    QStringList autocompletePreferenceList;
//    autocompletePreferenceList << "sv" << "sv.app" << "qt.QPushButton";
    autocompletePreferenceList << "qt.QPushButton";
    pythonConsole->completer()->setAutocompletePreferenceList(autocompletePreferenceList);

    //pythonConsole->setAttribute(Qt::WA_QuitOnClose, false);
    pythonConsole->resize(600, 300);

//    svApplication::application()->settingsDialog()->addPanel(
//      "Python", new svSettingsPythonPanel);

//  =================
//    QObject::connect(pythonConsole, SIGNAL(aboutToExecute(const QString&)),
//      this, SLOT(onPythonConsoleUserInput(const QString&)));

//  ==================
    pythonConsole->installEventFilter(this);
    pythonConsole->setWindowFlags(Qt::WindowStaysOnTopHint);
}

vtkRenderer* svMainWindow::getRenderer(){
        return static_cast<vtkRenderer*>(m_DisplayWidget->GetRenderWindow4()->GetVtkRenderWindow()->GetRenderers()->GetItemAsObject(1));
}

vtkRenderWindow* svMainWindow::getRenderWindow(){
        return static_cast<vtkRenderWindow*>(m_DisplayWidget->GetRenderWindow4()->GetVtkRenderWindow());
}

void svMainWindow::closeEvent(QCloseEvent *event)
{
    QMessageBox::StandardButton answerButton = QMessageBox::question( this
                                                                      , tr("Exit")
                                                                      , "Are you sure to exit?");

    if(answerButton == QMessageBox::Yes)
    {
        event->accept();
    }else{
        event->ignore();
    }

}

void svMainWindow::on_actionPythonConsole_triggered()
{
    //    pythonConsole->show();
    //    pythonConsole->setWindowFlags(Qt::WindowStaysOnTopHint|Qt::X11BypassWindowManagerHint);

        pythonConsole->show();
    //    pythonConsole->activateWindow();
    //    pythonConsole->raise();
}

void svMainWindow::on_actionAxialOnly_triggered()
{
    m_DisplayWidget->changeLayoutToWidget1();
}

void svMainWindow::on_actionSagittalOnly_triggered()
{
    m_DisplayWidget->changeLayoutToWidget2();
}

void svMainWindow::on_actionCoronalOnly_triggered()
{
    m_DisplayWidget->changeLayoutToWidget3();
}

void svMainWindow::on_actionLayout2DTop_triggered()
{
    m_DisplayWidget->changeLayoutTo2DImagesUp();
}

void svMainWindow::on_actionLayout2DLeft_triggered()
{
    m_DisplayWidget->changeLayoutTo2DImagesLeft();
}

void svMainWindow::on_actionLayoutStandard_triggered()
{
    m_DisplayWidget->changeLayoutToDefault();
}

void svMainWindow::on_actionLayout3DOnly_triggered()
{
    m_DisplayWidget->changeLayoutToBig3D();
}


