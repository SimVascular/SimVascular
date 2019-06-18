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

#include "sv4gui_CapSelectionWidget.h"
#include "ui_sv4gui_CapSelectionWidget.h"
#include "ui_sv4gui_LoftParamWidget.h"
#include "sv4gui_LoftingUtils.h"

#include <QMessageBox>

sv4guiCapSelectionWidget::sv4guiCapSelectionWidget(QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::sv4guiCapSelectionWidget)
    , m_TableModel(NULL)
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

sv4guiCapSelectionWidget::~sv4guiCapSelectionWidget()
{
    delete ui;
}

void sv4guiCapSelectionWidget::SetTableView(std::vector<std::string> caps, sv4guiModelElement* modelElement, std::string type)
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
    ui->tableView->setSelectionMode(QAbstractItemView::SingleSelection);
    //ui->tableView->setColumnWidth(0,150);
}

std::vector<std::string> sv4guiCapSelectionWidget::GetUsedCapNames()
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

void sv4guiCapSelectionWidget::SetUsedCapNames(std::set<std::string> capNames)
{
    if(m_TableModel==NULL) {
        return;
    }

    int rowCount = m_TableModel->rowCount();

    for (int i = 0; i < rowCount; i++) {
        QModelIndex index = m_TableModel->index(i,1, QModelIndex());
        QModelIndex idx = m_TableModel->index(i,0, QModelIndex());
        auto capName = idx.data().toString().toStdString();
        if (capNames.find(capName) != capNames.end()) { 
            QStandardItem* item = m_TableModel->item(i,1);
            item->setCheckState(Qt::Checked);
        }
    }

}


std::vector<std::string> sv4guiCapSelectionWidget::GetUnselectedCapNames()
{
    std::vector<std::string> capNames;

    if(m_TableModel==NULL) {
        return capNames;
    }

    int rowCount=m_TableModel->rowCount();

    for(int i=0;i<rowCount;i++) {
        QModelIndex index= m_TableModel->index(i,1, QModelIndex());
        if(index.data(Qt::CheckStateRole) != Qt::Checked){
            QModelIndex idx= m_TableModel->index(i,0, QModelIndex());
            capNames.push_back(idx.data().toString().toStdString());
        }
    }
    return capNames;
}


void sv4guiCapSelectionWidget::Confirm()
{
    hide();
    emit accepted();
}

void sv4guiCapSelectionWidget::Cancel()
{
    hide();
}

void sv4guiCapSelectionWidget::TableViewContextMenuRequested( const QPoint & pos )
{
    m_NodeMenu->popup(QCursor::pos());
}

void sv4guiCapSelectionWidget::UseSelected(bool)
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

void sv4guiCapSelectionWidget::UseAll(bool)
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


void sv4guiCapSelectionWidget::NotUseSelected(bool)
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

void sv4guiCapSelectionWidget::UseNone(bool)
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
