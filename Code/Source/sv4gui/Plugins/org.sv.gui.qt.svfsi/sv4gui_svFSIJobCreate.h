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

#ifndef SV4GUI_SVFSICREATE_H
#define SV4GUI_SVFSICREATE_H

#include "sv4gui_DataNodeOperationInterface.h"
#include <mitkDataStorage.h>
#include <QWidget>

namespace Ui {
class sv4guisvFSIJobCreate;
}

class sv4guisvFSIJobCreate : public QWidget
{
    Q_OBJECT

public:

    sv4guisvFSIJobCreate(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer selectedNode, int timeStep = 0);

    virtual ~sv4guisvFSIJobCreate();

public slots:

    void CreateJob();

    void Cancel();

    void SetFocus();

    void Activated();

protected:

    Ui::sv4guisvFSIJobCreate *ui;

    QWidget* m_Parent;

    mitk::DataNode::Pointer m_SimulationFolderNode;

    mitk::DataNode::Pointer m_ModelFolderNode;

    mitk::DataStorage::Pointer m_DataStorage;

    mitk::DataNode::Pointer m_SelecteNode;

    int m_TimeStep;

    sv4guiDataNodeOperationInterface* m_Interface;
};

#endif // SV4GUI_SVFSICREATE_H
