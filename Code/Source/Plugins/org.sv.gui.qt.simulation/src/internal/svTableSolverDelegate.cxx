#include "svTableSolverDelegate.h"
#include <QLineEdit>

svTableSolverDelegate::svTableSolverDelegate(QObject *parent) :
    QItemDelegate(parent)
{
}

QWidget* svTableSolverDelegate::createEditor(QWidget *parent, const QStyleOptionViewItem &option, const QModelIndex &index) const
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

void svTableSolverDelegate::setEditorData(QWidget *editor, const QModelIndex &index) const
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

void svTableSolverDelegate::setModelData(QWidget *editor, QAbstractItemModel *model, const QModelIndex &index) const
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

void svTableSolverDelegate::updateEditorGeometry(QWidget *editor, const QStyleOptionViewItem &option, const QModelIndex &index) const
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
