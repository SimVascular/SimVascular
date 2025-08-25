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

#include "sv4gui_TableCapDelegate.h"
#include "sv4gui_SimJob.h"

sv4guiTableCapDelegate::sv4guiTableCapDelegate(QObject *parent) :
    QItemDelegate(parent)
{
}

//--------------
// createEditor
//--------------
// This seems to just create the BC Type combo box used to display
// bc types in the cap table.
//
// Note: these names need to match those set in sv4gui_CapBCWidget.ui 
//
QWidget* sv4guiTableCapDelegate::createEditor(QWidget *parent, const QStyleOptionViewItem &option, const QModelIndex &index) const
{
    int column = index.column();

    switch (column){

      case 1: {
        QComboBox* cb = new QComboBox(parent);
        cb->addItem( QString::fromStdString(sv4guiSimJobBCType::flow) );
        cb->addItem( QString::fromStdString(sv4guiSimJobBCType::resistance) );
        cb->addItem( QString::fromStdString(sv4guiSimJobBCType::rcr) );
        cb->addItem( QString::fromStdString(sv4guiSimJobBCType::lpm) );
        return cb;
      }

    default:
        return new QWidget(parent);
    }
}

//---------------
// setEditorData
//---------------
//
void sv4guiTableCapDelegate::setEditorData(QWidget *editor, const QModelIndex &index) const
{
    #define n_debug_setEditorData
    #ifdef debug_setEditorData
    std::string msg("[sv4guiTableCapDelegate::setEditorData] ");
    std::cout << msg << "========== setEditorData ==========" << std::endl;
    #endif

    int column = index.column();

    switch(column){
    case 1:
    {
        QComboBox* cb=dynamic_cast<QComboBox*>(editor);
        if(cb)
        {
            QString type = index.model()->data(index, Qt::EditRole).toString();
            cb->setCurrentText(type);
            #ifdef debug_setEditorData
            std::cout << msg << "type: " << type.toStdString() << std::endl;
            #endif
        }
    }
        break;
    default:
        break;
    }
}

//--------------
// setModelData
//--------------
//
void sv4guiTableCapDelegate::setModelData(QWidget *editor, QAbstractItemModel *model, const QModelIndex &index) const
{
    #define n_debug_setModelData
    #ifdef debug_setModelData 
    std::string msg("[sv4guiTableCapDelegate::setModelData] ");
    std::cout << msg << "========== setModelData ==========" << std::endl;
    #endif

    int column=index.column();

    switch(column){
    case 1:
    {
        QComboBox* cb=dynamic_cast<QComboBox*>(editor);
        if(cb)
        {
            QString previousType=index.model()->data(index, Qt::EditRole).toString();
            if(previousType!=cb->currentText())
            {
                model->setData(index, cb->currentText(), Qt::EditRole);
            }
        }
    }
        break;
    default:
        break;
    }
}

//----------------------
// updateEditorGeometry
//----------------------
//
void sv4guiTableCapDelegate::updateEditorGeometry(QWidget *editor, const QStyleOptionViewItem &option, const QModelIndex &index) const
{
    #define n_debug_updateEditorGeometry
    #ifdef debug_updateEditorGeometry 
    std::string msg("[sv4guiTableCapDelegate::updateEditorGeometry] ");
    std::cout << msg << "========== updateEditorGeometry ==========" << std::endl;
    #endif

    int column=index.column();

    switch(column){
    case 1:
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
