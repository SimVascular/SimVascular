#include "svTableCapDelegate.h"

svTableCapDelegate::svTableCapDelegate(QObject *parent) :
    QItemDelegate(parent)
{
}

QWidget* svTableCapDelegate::createEditor(QWidget *parent, const QStyleOptionViewItem &option, const QModelIndex &index) const
{
    int column=index.column();

    switch(column){
    case 1:
    {
        QComboBox* cb=new QComboBox(parent);
        cb->addItem("Prescribed Velocities");
        cb->addItem("Resistance");
        cb->addItem("RCR");
        cb->addItem("Coronary");
        cb->addItem("Impedance");
        return cb;
    }
    default:
        return new QWidget(parent);
    }
}

void svTableCapDelegate::setEditorData(QWidget *editor, const QModelIndex &index) const
{
    int column=index.column();

    switch(column){
    case 1:
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

void svTableCapDelegate::setModelData(QWidget *editor, QAbstractItemModel *model, const QModelIndex &index) const
{
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

void svTableCapDelegate::updateEditorGeometry(QWidget *editor, const QStyleOptionViewItem &option, const QModelIndex &index) const
{
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
