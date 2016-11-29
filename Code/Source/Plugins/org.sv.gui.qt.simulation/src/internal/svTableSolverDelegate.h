#ifndef SVTABLESOLVERDELEGATE_H
#define SVTABLESOLVERDELEGATE_H

#include <QItemDelegate>
#include <QModelIndex>
#include <QObject>
#include <QSize>
#include <QDoubleSpinBox>
#include <QComboBox>

class svTableSolverDelegate : public QItemDelegate
{
    Q_OBJECT
public:
    explicit svTableSolverDelegate(QObject *parent = 0);

    QWidget* createEditor(QWidget *parent, const QStyleOptionViewItem &option, const QModelIndex &index) const;

    void setEditorData(QWidget *editor, const QModelIndex &index) const;

    void setModelData(QWidget *editor, QAbstractItemModel *model, const QModelIndex &index) const;

    void updateEditorGeometry(QWidget *editor, const QStyleOptionViewItem &option, const QModelIndex &index) const;

signals:

public slots:

};

#endif // SVTABLESOLVERDELEGATE_H
