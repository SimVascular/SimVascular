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

#include "svQmitkDelKeyFilter.h"

#include <QKeyEvent>
#include <svQmitkDataManagerView.h>

svQmitkDelKeyFilter::svQmitkDelKeyFilter( QObject* _DataManagerView )
: QObject(_DataManagerView)
{

}

bool svQmitkDelKeyFilter::eventFilter( QObject *obj, QEvent *event )
{
  if (event->type() == QEvent::KeyPress)
  {
    svQmitkDataManagerView* _DataManagerView = qobject_cast<svQmitkDataManagerView*>(this->parent());
    QKeyEvent *keyEvent = static_cast<QKeyEvent *>(event);
    if(keyEvent->key() == Qt::Key_Delete && _DataManagerView)
    {
      // trigger deletion of selected node(s)
      _DataManagerView->ActionRemoveTriggered(false);
      // return true: this means the delete key event is not send to the table
      return true;
    }
  }
  // standard event processing
  return QObject::eventFilter(obj, event);
}
