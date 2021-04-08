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

#include "sv4gui_TableSolverDelegateROM.h"
#include <QLineEdit>

sv4guiTableSolverDelegateROM::sv4guiTableSolverDelegateROM(QObject *parent) :
    QItemDelegate(parent)
{
}

QWidget* sv4guiTableSolverDelegateROM::createEditor(QWidget *parent, const QStyleOptionViewItem &option, const QModelIndex &index) const
{
    int row=index.row();
    int column=index.column();

    switch(column){
    case 1:
    {
        QModelIndex typeIndex=index.model()->index(row,2);
        QModelIndex enumListIndex=index.model()->index(row,3);
        QString type=index.model()->data(typeIndex).toString();
        QString enumList=index.model()->data(enumListIndex).toString();

        if(type=="enum")
        {
            QStringList list=enumList.split(";");
            QComboBox* cb=new QComboBox(parent);
            for(int i=0;i<list.size();i++)
                cb->addItem(list[i]);

            return cb;
        }
        else if(type!="")
        {
            return new QLineEdit(parent);
        }
    }
    default:
        return new QWidget(parent);
    }
}

void sv4guiTableSolverDelegateROM::setEditorData(QWidget *editor, const QModelIndex &index) const
{
    int row=index.row();
    int column=index.column();

    switch(column){
    case 1:
    {
        QModelIndex typeIndex=index.model()->index(row,2);
        QString type=index.model()->data(typeIndex).toString();

        if(type=="enum")
        {
            QComboBox* cb=dynamic_cast<QComboBox*>(editor);
            if(cb)
            {
                QString value=index.model()->data(index).toString();
                cb->setCurrentText(value);
            }
        }
        else if(type!="")
        {
            QLineEdit* le=dynamic_cast<QLineEdit*>(editor);
            if(le)
            {
                QString value=index.model()->data(index).toString();
                le->setText(value);
            }
        }
    }
        break;
    default:
        break;
    }
}

void sv4guiTableSolverDelegateROM::setModelData(QWidget *editor, QAbstractItemModel *model, const QModelIndex &index) const
{
    int row=index.row();
    int column=index.column();

    switch(column){
    case 1:
    {
        QModelIndex typeIndex=index.model()->index(row,2);
        QString type=index.model()->data(typeIndex).toString();

        if(type=="enum")
        {
            QComboBox* cb=dynamic_cast<QComboBox*>(editor);
            if(cb)
            {
                QString previousValue=index.model()->data(index, Qt::EditRole).toString();
                if(previousValue!=cb->currentText())
                {
                    model->setData(index, cb->currentText(), Qt::EditRole);
                }
            }
        }
        else if(type!="")
        {
            QLineEdit* le=dynamic_cast<QLineEdit*>(editor);
            if(le)
            {
                QString previousValue=index.model()->data(index, Qt::EditRole).toString().trimmed();
                if(previousValue!=le->text().trimmed())
                {
                    model->setData(index, le->text().trimmed(), Qt::EditRole);
                }
            }
        }
    }
        break;
    default:
        break;
    }
}

void sv4guiTableSolverDelegateROM::updateEditorGeometry(QWidget *editor, const QStyleOptionViewItem &option, const QModelIndex &index) const
{
    int column=index.column();

    switch(column){
    case 1:
    {
        editor->setGeometry(option.rect);
    }
        break;
    default:
        break;
    }
}
