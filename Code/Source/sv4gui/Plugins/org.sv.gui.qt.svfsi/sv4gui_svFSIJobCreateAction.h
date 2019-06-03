/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef sv4guisvFSIJobCreateACTION_H
#define sv4guisvFSIJobCreateACTION_H

#include <org_sv_gui_qt_svfsi_Export.h>

#include "sv4gui_svFSIJobCreate.h"

//#include "sv4gui_DataNodeOperationInterface.h"
//#include <mitkIContextMenuAction.h>

#include <sv4gui_mitkIContextMenuAction.h>

#include <mitkDataNode.h>

#include <QObject>

class SV_QT_SVFSI sv4guisvFSIJobCreateAction : public QObject, public svmitk::IContextMenuAction 
{
  Q_OBJECT
  Q_INTERFACES(svmitk::IContextMenuAction)

public:
  sv4guisvFSIJobCreateAction();
  ~sv4guisvFSIJobCreateAction();

  // IContextMenuAction
  void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
  void SetDataStorage(mitk::DataStorage *dataStorage) override;
  void SetSmoothed(bool smoothed) override {}
  void SetDecimated(bool decimated) override {}
  void SetFunctionality(berry::QtViewPart *functionality) override;

private:
  sv4guisvFSIJobCreateAction(const sv4guisvFSIJobCreateAction &);
  sv4guisvFSIJobCreateAction & operator=(const sv4guisvFSIJobCreateAction &);

  mitk::DataStorage::Pointer m_DataStorage;
  berry::QtViewPart *m_Functionality;

  sv4guisvFSIJobCreate* m_svFSIJobCreateWidget;

  //sv4guiDataNodeOperationInterface* m_Interface;
};

#endif
