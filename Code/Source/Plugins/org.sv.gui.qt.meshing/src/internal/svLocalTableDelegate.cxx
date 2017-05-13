#include "svLocalTableDelegate.h"

svLocalTableDelegate::svLocalTableDelegate(QObject *parent) :
    QItemDelegate(parent)
{
}

QWidget* svLocalTableDelegate::createEditor(QWidget *parent, const QStyleOptionViewItem &option, const QModelIndex &index) const
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

void svLocalTableDelegate::setEditorData(QWidget *editor, const QModelIndex &index) const
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

void svLocalTableDelegate::setModelData(QWidget *editor, QAbstractItemModel *model, const QModelIndex &index) const
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

void svLocalTableDelegate::updateEditorGeometry(QWidget *editor, const QStyleOptionViewItem &option, const QModelIndex &index) const
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
