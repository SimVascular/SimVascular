#include "svCapSelectionWidget.h"
#include "ui_svCapSelectionWidget.h"
#include "ui_svLoftParamWidget.h"
#include "svLoftingUtils.h"

#include <QMessageBox>

svCapSelectionWidget::svCapSelectionWidget(QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::svCapSelectionWidget)
    , m_TableModel(NULL)
    , m_NumSampling(0)
    , m_ModelElement(NULL)
    , m_ModelType("")
    , m_LoftWidget(NULL)
{
    ui->setupUi(this);

    ui->tableView->horizontalHeader()->setSectionResizeMode(QHeaderView::Stretch);
//    ui->tableView->setSortingEnabled(true);

    m_NodeMenu = new QMenu(ui->tableView);

    QAction* useSelectedAction=m_NodeMenu->addAction("Use Selected");
    QAction* useAllAction=m_NodeMenu->addAction("Use All");
    QAction* notUseAction=m_NodeMenu->addAction("Not Use Selected");
    QAction* useNoneAction=m_NodeMenu->addAction("Use None");

    QObject::connect( useSelectedAction, SIGNAL( triggered(bool) ) , this, SLOT( UseSelected(bool) ) );
    QObject::connect( useAllAction, SIGNAL( triggered(bool) ) , this, SLOT( UseAll(bool) ) );
    QObject::connect( notUseAction, SIGNAL( triggered(bool) ) , this, SLOT( NotUseSelected(bool) ) );
    QObject::connect( useNoneAction, SIGNAL( triggered(bool) ) , this, SLOT( UseNone(bool) ) );

    QObject::connect( ui->tableView, SIGNAL(customContextMenuRequested(const QPoint&))
      , this, SLOT(TableViewContextMenuRequested(const QPoint&)) );

    connect(ui->buttonBox,SIGNAL(accepted()), this, SLOT(Confirm()));
    connect(ui->buttonBox,SIGNAL(rejected()), this, SLOT(Cancel()));

    m_LoftWidget=new svLoftParamWidget();
    m_LoftWidget->move(400,400);
    m_LoftWidget->hide();
    m_LoftWidget->setWindowFlags(Qt::WindowStaysOnTopHint);

    connect(m_LoftWidget->ui->btnOK, SIGNAL(clicked()), this, SLOT(OKLofting()) );
    connect(m_LoftWidget->ui->btnApply, SIGNAL(clicked()), this, SLOT(ApplyLofting()) );
    connect(m_LoftWidget->ui->btnClose, SIGNAL(clicked()), this, SLOT(HideLoftWidget()) );

    connect(ui->btnUniformParameters, SIGNAL(clicked()), this, SLOT(ShowLoftWidget()) );

}

svCapSelectionWidget::~svCapSelectionWidget()
{
    delete ui;

    if(m_LoftWidget)
        delete m_LoftWidget;
}

void svCapSelectionWidget::SetTableView(std::vector<std::string> caps, svModelElement* modelElement, std::string type)
{
    m_ModelElement=modelElement;
    m_ModelType=type;
    if (type !="PolyData")
    {
      QMessageBox::warning(this,"Error","Cannot currently extract centerlines of anyting other than a PolyData model");
      return;
    }

    int capNum=caps.size();

    m_TableModel = new QStandardItemModel(capNum,2,this);

    for(int row = 0; row < capNum; row++)
    {
        for(int col = 0; col < 2; col++)
        {
            if(col==0)
            {
                QStandardItem* item= new QStandardItem(QString::fromStdString(caps[row]));
                item->setEditable(false);
                m_TableModel->setItem(row,col,item);
            }
            else if(col==1)
            {
                QStandardItem* item= new QStandardItem(false);
                item->setCheckable(true);
                item->setCheckState(Qt::Unchecked);
                m_TableModel->setItem(row,col,item);
            }
        }
    }

    QStringList headers;
    headers << "Cap" << "Use";
    m_TableModel->setHorizontalHeaderLabels(headers);

    ui->tableView->setModel(m_TableModel);
    //ui->tableView->setColumnWidth(0,150);
}

std::vector<std::string> svCapSelectionWidget::GetUsedCapNames()
{
    std::vector<std::string> capNames;
    if(m_TableModel==NULL)
        return capNames;

//    int rowCount=m_TableModel->rowCount(QModelIndex());
    int rowCount=m_TableModel->rowCount();
    for(int i=0;i<rowCount;i++)
    {
        QModelIndex index= m_TableModel->index(i,1, QModelIndex());
        if(index.data(Qt::CheckStateRole) == Qt::Checked){
            QModelIndex idx= m_TableModel->index(i,0, QModelIndex());
            capNames.push_back(idx.data().toString().toStdString());
        }
    }
    return capNames;
}

int svCapSelectionWidget::GetNumSampling()
{
    return m_NumSampling;
}

int svCapSelectionWidget::IfUseUniform()
{
    return ui->checkboxUseUniform->isChecked()?1:0;
}

svLoftingParam svCapSelectionWidget::GetLoftingParam()
{
    return m_Param;
}

void svCapSelectionWidget::Confirm()
{
    QString strNum=ui->lineEditNumSampling->text().trimmed();
//    m_UseUniform = ui->checkboxUseUniform->isChecked()?1:0;

    if(strNum=="")
    {
        if(m_ModelType!="PolyData")
        {
            QMessageBox::warning(this,"Value Mising","Pleases provide the number of sampling points.");
            return;
        }

        m_NumSampling=0;
    }
    else
    {
        bool ok;
        int num=strNum.toInt(&ok);
        if(ok)
        {
            if(num<1)
            {
                QMessageBox::warning(this,"Value Error","Pleases give a positive integer format if you want to provide the number of sampling points.");
                return;
            }
            m_NumSampling=num;
        }
        else
        {
            QMessageBox::warning(this,"Format Error","Pleases give a correct format if you want to provide the number of sampling points.");
            return;
        }
    }
    hide();
    emit accepted();
}

void svCapSelectionWidget::Cancel()
{
    hide();
}

void svCapSelectionWidget::TableViewContextMenuRequested( const QPoint & pos )
{
    m_NodeMenu->popup(QCursor::pos());
}

void svCapSelectionWidget::UseSelected(bool)
{
    if(m_TableModel==NULL)
        return;

    QModelIndexList indexesOfSelectedRows = ui->tableView->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
      return;
    }

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
       ; it != indexesOfSelectedRows.end(); it++)
     {
       int row=(*it).row();

       QStandardItem* item= m_TableModel->item(row,1);
       item->setCheckState(Qt::Checked);
     }
}

void svCapSelectionWidget::UseAll(bool)
{
    if(m_TableModel==NULL)
        return;

    int rowCount=m_TableModel->rowCount();

    for (int i=0;i<rowCount;i++)
    {
        QStandardItem* item= m_TableModel->item(i,1);
        item->setCheckState(Qt::Checked);
    }
}


void svCapSelectionWidget::NotUseSelected(bool)
{
    if(m_TableModel==NULL)
        return;

    QModelIndexList indexesOfSelectedRows = ui->tableView->selectionModel()->selectedRows();
    if(indexesOfSelectedRows.size() < 1)
    {
      return;
    }

    for (QModelIndexList::iterator it = indexesOfSelectedRows.begin()
       ; it != indexesOfSelectedRows.end(); it++)
     {
       int row=(*it).row();

       QStandardItem* item= m_TableModel->item(row,1);
       item->setCheckState(Qt::Unchecked);
     }
}

void svCapSelectionWidget::UseNone(bool)
{
    if(m_TableModel==NULL)
        return;

    int rowCount=m_TableModel->rowCount();

    for (int i=0;i<rowCount;i++)
    {
        QStandardItem* item= m_TableModel->item(i,1);
        item->setCheckState(Qt::Unchecked);
    }
}

void svCapSelectionWidget::ShowLoftWidget()
{
    if(m_ModelElement && m_ModelElement->IfUseUniform() && m_ModelElement->GetLoftingParam())
        m_Param=*(m_ModelElement->GetLoftingParam());
    else
        svLoftingUtils::SetPreferencedValues(&m_Param);

    m_LoftWidget->UpdateGUI(&m_Param);

    m_LoftWidget->show();
}

void svCapSelectionWidget::OKLofting()
{
    m_LoftWidget->UpdateParam(&m_Param);
    m_LoftWidget->hide();
}

void svCapSelectionWidget::ApplyLofting()
{
    m_LoftWidget->UpdateParam(&m_Param);
}

void svCapSelectionWidget::HideLoftWidget()
{
    m_LoftWidget->hide();
}
