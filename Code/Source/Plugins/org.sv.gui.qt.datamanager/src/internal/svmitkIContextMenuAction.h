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
#ifndef svmitkIContextMenuAction_H_
#define svmitkIContextMenuAction_H_

#include <berryMacros.h>
#include <berryQtViewPart.h>

#include <vector>
#include <mitkDataNode.h>
#include "mitkDataStorage.h"

class QmitkStdMultiWidget;

namespace svmitk
{
  /**
  * A context menu action, which is linked to the context menu <br>
  * through an extension point. For an example check the <br>
  * <code> plugin.xml </code> and the connected classes of <br>
  * the the segmentation bundle and also the <code> svQmitkDataManagerView.cpp </code> <br>
  * in this bundle.
  */
  struct IContextMenuAction
  {

      /**
      * @brief Executes the action, that linked to the context menu entry.
      */
      virtual void Run( const QList<mitk::DataNode::Pointer>& selectedNodes ) = 0;

    // Setters
    virtual void SetDataStorage(mitk::DataStorage* dataStorage) = 0;
    virtual void SetSmoothed(bool smoothed) = 0;
    virtual void SetDecimated(bool decimated) = 0;
    virtual void SetFunctionality(berry::QtViewPart* functionality) = 0;
  };
}

Q_DECLARE_INTERFACE(svmitk::IContextMenuAction, "org.sv.datamanager.IContextMenuAction")

#endif // svmitkIContextMenuAction_H_
