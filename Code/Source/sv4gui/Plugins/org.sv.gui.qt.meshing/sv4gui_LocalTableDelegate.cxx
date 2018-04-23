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

#include "sv4gui_LocalTableDelegate.h"

sv4guiLocalTableDelegate::sv4guiLocalTableDelegate(QObject *parent) :
    QItemDelegate(parent)
{
}

QWidget* sv4guiLocalTableDelegate::createEditor(QWidget *parent, const QStyleOptionViewItem &option, const QModelIndex &index) const
{
    int column=index.column();

    switch(column){
    case 3:
    {
        QComboBox* cb=new QComboBox(parent);
        cb->addItem("Max Edge");
        cb->addItem("Max Curv");
        cb->addItem("Min Curv");
        return cb;
    }
    case 4:
    {
        QComboBox* cb=new QComboBox(parent);
        cb->addItem("absolute");
        cb->addItem("relative");
        return cb;
    }
    case 6:
    {
        QComboBox* cb=new QComboBox(parent);
        cb->addItem("(1)t0 tb");
        cb->addItem("(2)t0 g");
        cb->addItem("(3)t0 ... tn-1");
        cb->addItem("(4)g");
        cb->setToolTip("t0: first layer height\ntb: total height\ntn-1: last layer height\ng: gradation factor(0<g<1)");
        return cb;
    }
    case 7:
    {
        QComboBox* cb=new QComboBox(parent);
        cb->addItem("both");
        cb->addItem("positive");
        cb->addItem("negative");
        return cb;
    }    default:
        return new QWidget(parent);
    }
}

void sv4guiLocalTableDelegate::setEditorData(QWidget *editor, const QModelIndex &index) const
{
    int column=index.column();

    switch(column){
    case 3:
    case 4:
    case 6:
    case 7:
    {
        QComboBox* cb=dynamic_cast<QComboBox*>(editor);
        if(cb)
        {
            QString type=index.model()->data(index, Qt::EditRole).toString();
            cb->setCurrentText(type);
        }
    }
        break;
    default:
        break;
    }
}

void sv4guiLocalTableDelegate::setModelData(QWidget *editor, QAbstractItemModel *model, const QModelIndex &index) const
{
    int column=index.column();

    switch(column){
    case 3:
    case 4:
    case 6:
    case 7:
    {
        QComboBox* cb=dynamic_cast<QComboBox*>(editor);
        if(cb)
        {
            model->setData(index, cb->currentText(), Qt::EditRole);
        }
    }
        break;
    default:
        break;
    }
}

void sv4guiLocalTableDelegate::updateEditorGeometry(QWidget *editor, const QStyleOptionViewItem &option, const QModelIndex &index) const
{
    int column=index.column();

    switch(column){
    case 3:
    case 4:
    case 6:
    case 7:
    {
        QComboBox* cb=dynamic_cast<QComboBox*>(editor);
        if(cb)
        {
            editor->setGeometry(option.rect);
        }
    }
        break;
    default:
        break;
    }


}
