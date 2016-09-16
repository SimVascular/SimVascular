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

#ifndef QmitkDataManagerItemDelegate_h
#define QmitkDataManagerItemDelegate_h

#include "SimVascular.h"

#include <svQtAppBaseExports.h>

#include <QStyledItemDelegate>

class SVQTAPPBASE_EXPORT QmitkDataManagerItemDelegate : public QStyledItemDelegate
{
  Q_OBJECT

public:
  explicit QmitkDataManagerItemDelegate(QObject* parent = nullptr);
  ~QmitkDataManagerItemDelegate();

  void setEditorData(QWidget* editor, const QModelIndex& index) const override;
};

#endif
