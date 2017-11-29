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
#include "svDataManagerPluginActivator.h"
#include "svQmitkDataManagerView.h"
#include "svQmitkDataManagerPreferencePage.h"
#include "svQmitkDataManagerHotkeysPrefPage.h"

//namespace mitk {

void svDataManagerPluginActivator::start(ctkPluginContext* context)
{
  BERRY_REGISTER_EXTENSION_CLASS(svQmitkDataManagerView, context)
  BERRY_REGISTER_EXTENSION_CLASS(svQmitkDataManagerPreferencePage, context)
  BERRY_REGISTER_EXTENSION_CLASS(svQmitkDataManagerHotkeysPrefPage, context)
}

void svDataManagerPluginActivator::stop(ctkPluginContext* context)
{
  Q_UNUSED(context)
}

//}
