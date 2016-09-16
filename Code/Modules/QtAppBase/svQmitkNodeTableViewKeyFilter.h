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

#ifndef SVQMITKNODETABLEVIEWKEYFILTER_H_
#define SVQMITKNODETABLEVIEWKEYFILTER_H_

#include "SimVascular.h"
#include <svQtAppBaseExports.h>

#include <QObject>

///
/// A small class which "eats" all Del-Key-pressed events on the node table.
/// When the Del Key is pressed selected nodes should be removed.
///

class SVQTAPPBASE_EXPORT svQmitkNodeTableViewKeyFilter : public QObject
{
  Q_OBJECT
public:
  svQmitkNodeTableViewKeyFilter(QObject* _DataManager = nullptr);
protected:
  bool eventFilter(QObject *obj, QEvent *event) override;

};

#endif // SVQMITKNODETABLEVIEWKEYFILTER_H_
