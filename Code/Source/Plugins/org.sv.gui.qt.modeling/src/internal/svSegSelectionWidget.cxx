#include "svSegSelectionWidget.h"
#include "ui_svSegSelectionWidget.h"

#include <QMessageBox>

svSegSelectionWidget::svSegSelectionWidget(QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::svSegSelectionWidget)
    , m_TableModel(NULL)
    , m_NumSampling(0)
    , m_ModelElement(NULL)
    , m_ModelType("")
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
}

svSegSelectionWidget::~svSegSelectionWidget()
{
    delete ui;
}

void svSegSelectionWidget::SetTableView(std::vector<mitk::DataNode::Pointer> segNodes, svModelElement* modelElement, std::string type)
{
    m_ModelElement=modelElement;
    m_ModelType=type;

    int segNum=segNodes.size();

    m_TableModel = new QStandardItemModel(segNum,2,this);

    for(int row = 0; row < segNum; row++)
    {
        for(int col = 0; col < 2; col++)
        {
            if(col==0)
            {
                QStandardItem* item= new QStandardItem(QString::fromStdString(segNodes[row]->GetName()));
                item->setEditable(false);
                m_TableModel->setItem(row,col,item);
            }
            else if(col==1)
            {
                if(modelElement&&modelElement->HasSeg(segNodes[row]->GetName()))
                {
                    QStandardItem* item= new QStandardItem(true);
                    item->setCheckable(true);
                    item->setCheckState(Qt::Checked);
                    m_TableModel->setItem(row,col,item);
                }
                else
                {
                    QStandardItem* item= new QStandardItem(false);
                    item->setCheckable(true);
                    item->setCheckState(Qt::Unchecked);
                    m_TableModel->setItem(row,col,item);
                }

            }
        }
    }

    QStringList headers;
    headers << "Segmentation" << "Use";
    m_TableModel->setHorizontalHeaderLabels(headers);

    ui->tableView->setModel(m_TableModel);
    //ui->tableView->setColumnWidth(0,150);

    int numSampling=0;
    if(modelElement)
    {
        numSampling=modelElement->GetNumSampling();

        if(numSampling>0)
            ui->lineEditNumSampling->setText(QString::number(numSampling));
        else
            ui->lineEditNumSampling->setText("");
    }
    else
    {
        if(type=="PolyData")
            ui->lineEditNumSampling->setText("");
        else
            ui->lineEditNumSampling->setText("20");
    }
}

std::vector<std::string> svSegSelectionWidget::GetUsedSegNames()
{
    std::vector<std::string> segNames;
    if(m_TableModel==NULL)
        return segNames;

//    int rowCount=m_TableModel->rowCount(QModelIndex());
    int rowCount=m_TableModel->rowCount();
    for(int i=0;i<rowCount;i++)
    {
        QModelIndex index= m_TableModel->index(i,1, QModelIndex());
        if(index.data(Qt::CheckStateRole) == Qt::Checked){
            QModelIndex idx= m_TableModel->index(i,0, QModelIndex());
            segNames.push_back(idx.data().toString().toStdString());
        }
    }
    return segNames;
}

int svSegSelectionWidget::GetNumSampling()
{
    return m_NumSampling;
}

void svSegSelectionWidget::Confirm()
{
    QString strNum=ui->lineEditNumSampling->text().trimmed();

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

void svSegSelectionWidget::Cancel()
{
    hide();
}

void svSegSelectionWidget::TableViewContextMenuRequested( const QPoint & pos )
{
    m_NodeMenu->popup(QCursor::pos());
}

void svSegSelectionWidget::UseSelected(bool)
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

void svSegSelectionWidget::UseAll(bool)
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


void svSegSelectionWidget::NotUseSelected(bool)
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

void svSegSelectionWidget::UseNone(bool)
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
