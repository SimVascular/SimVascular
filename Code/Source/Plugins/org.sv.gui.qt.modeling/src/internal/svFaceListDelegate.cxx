#include "svFaceListDelegate.h"

svFaceListDelegate::svFaceListDelegate(QObject *parent) :
    QItemDelegate(parent)
{
}

QWidget* svFaceListDelegate::createEditor(QWidget *parent, const QStyleOptionViewItem &option, const QModelIndex &index) const
{
    int column=index.column();

    switch(column){
    case 2:
    {
        QComboBox* cb=new QComboBox(parent);
        cb->addItem("wall");
//        cb->addItem("inlet");
//        cb->addItem("outlet");
        cb->addItem("cap");

        return cb;
    }
    case 5:
    {
        QDoubleSpinBox* dsb=new QDoubleSpinBox(parent);
        dsb->setMinimum(0);
        dsb->setMaximum(1.0);
        dsb->setDecimals(2);
        dsb->setSingleStep(0.2);
        return dsb;
    }
    default:
        return new QWidget(parent);
    }
}

void svFaceListDelegate::setEditorData(QWidget *editor, const QModelIndex &index) const
{
    int column=index.column();

    switch(column){
    case 2:
    {
        QComboBox* cb=dynamic_cast<QComboBox*>(editor);
        if(cb)
        {
            QString type=index.model()->data(index, Qt::EditRole).toString();
            cb->setCurrentText(type);
        }
    }
        break;
    case 5:
    {
        QDoubleSpinBox* dsb=dynamic_cast<QDoubleSpinBox*>(editor);
        if(dsb)
        {
            double value=index.model()->data(index, Qt::EditRole).toDouble();
            dsb->setValue(value);
        }
    }
        break;
    default:
        break;
    }
}

void svFaceListDelegate::setModelData(QWidget *editor, QAbstractItemModel *model, const QModelIndex &index) const
{
    int column=index.column();

    switch(column){
    case 2:
    {
        QComboBox* cb=dynamic_cast<QComboBox*>(editor);
        if(cb)
        {
            model->setData(index, cb->currentText(), Qt::EditRole);
        }
    }
        break;
    case 5:
    {
        QDoubleSpinBox* dsb=dynamic_cast<QDoubleSpinBox*>(editor);
        if(dsb)
        {
            dsb->interpretText();
            double value = dsb->value();
            model->setData(index, value, Qt::EditRole);
        }
    }
        break;
    default:
        break;
    }
}

void svFaceListDelegate::updateEditorGeometry(QWidget *editor, const QStyleOptionViewItem &option, const QModelIndex &index) const
{
    int column=index.column();

    switch(column){
    case 2:
    {
        QComboBox* cb=dynamic_cast<QComboBox*>(editor);
        if(cb)
        {
            editor->setGeometry(option.rect);
        }
    }
        break;
    case 5:
    {
        QDoubleSpinBox* dsb=dynamic_cast<QDoubleSpinBox*>(editor);
        if(dsb)
        {
            editor->setGeometry(option.rect);
        }
    }
        break;
    default:
        break;
    }


}
