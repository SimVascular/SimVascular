/*===================================================================

The Medical Imaging Interaction Toolkit (MITK)

Copyright (c) German Cancer Research Center,
Division of Medical and Biological Informatics.
All rights reserved.

This software is distributed WITHOUT ANY WARRANTY; without
even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE.

See LICENSE.txt or http://www.mitk.org for details.

===================================================================*/

#ifndef svQmitkDataManagerItemDelegate_h
#define svQmitkDataManagerItemDelegate_h

#include <QStyledItemDelegate>

class svQmitkDataManagerItemDelegate : public QStyledItemDelegate
{
  Q_OBJECT

public:
  explicit svQmitkDataManagerItemDelegate(QObject* parent = nullptr);
  ~svQmitkDataManagerItemDelegate();

  void setEditorData(QWidget* editor, const QModelIndex& index) const override;
};

#endif
