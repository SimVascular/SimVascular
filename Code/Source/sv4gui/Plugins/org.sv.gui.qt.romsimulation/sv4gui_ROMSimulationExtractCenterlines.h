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

#ifndef SV4GUI_ROM_SIMULATION_EXTRACT_CENTERLINES_H
#define SV4GUI_ROM_SIMULATION_EXTRACT_CENTERLINES_H

#include <org_sv_gui_qt_romsimulation_Export.h>
#include "sv4gui_DataNodeOperationInterface.h"

#include <sv4gui_mitkIContextMenuAction.h>
#include <mitkDataNode.h>

#include <QObject>
#include <QThread>

#include <vtkSmartPointer.h>
#include <vtkPolyData.h>

class SV_QT_ROMSIMULATION sv4guiROMSimulationExtractCenterlines : public QObject, public svmitk::IContextMenuAction
{
  Q_OBJECT
  Q_INTERFACES(svmitk::IContextMenuAction)

  public:
    sv4guiROMSimulationExtractCenterlines();
    ~sv4guiROMSimulationExtractCenterlines();

    // IContextMenuAction
    void Run(const QList<mitk::DataNode::Pointer> &selectedNodes) override;
    void SetDataStorage(mitk::DataStorage *dataStorage) override;
    void SetSmoothed(bool smoothed) override {}
    void SetDecimated(bool decimated) override {}
    void SetFunctionality(berry::QtViewPart *functionality) override { m_View = functionality; }
    vtkSmartPointer<vtkPolyData> GetCenterlineGeometry();

    // Custom functionality
    void SetSourceCapIds(std::vector<int> sourceCapIds);
    mitk::DataNode::Pointer m_JobNode; 
    QString m_CenterlinesFileName;

  public slots:
    void UpdateStatus();

  private:
    sv4guiROMSimulationExtractCenterlines(const sv4guiROMSimulationExtractCenterlines &);
    sv4guiROMSimulationExtractCenterlines & operator=(const sv4guiROMSimulationExtractCenterlines &);

    mitk::DataStorage::Pointer m_DataStorage;
    mitk::DataNode::Pointer m_ProjFolderNode;

    sv4guiDataNodeOperationInterface* m_Interface;

    std::vector<int> m_SourceCapIds;
    vtkSmartPointer<vtkPolyData> m_CenterlineGeometry; 
    berry::QtViewPart* m_View;

    class WorkThread : public QThread
    {
      public:
        WorkThread(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer selectedNode,
                   std::vector<int> sourceCapIds);

        QString GetStatus(){return m_Status;}
        mitk::DataNode::Pointer GetPathFolderNode(){return m_PathFolderNode;}
        std::vector<mitk::DataNode::Pointer> GetPathNodes(){return m_PathNodes;}
        mitk::DataNode::Pointer GetCenterlinesModelNode() {return m_CenterlinesModelNode;}
        mitk::DataNode::Pointer GetMergedCenterlinesModelNode() {return m_MergedCenterlinesModelNode;}
        mitk::DataNode::Pointer GetSelectedNode(){return m_SelectedNode;}
        vtkSmartPointer<vtkPolyData> m_CenterlineGeometry; 
        mitk::DataNode::Pointer m_JobNode; 
        QString m_CenterlinesFileName;

      private:
        mitk::DataNode::Pointer m_SelectedNode;
        mitk::DataStorage::Pointer mm_DataStorage;
        mitk::DataNode::Pointer m_PathFolderNode;
        QString m_Status;
        std::vector<mitk::DataNode::Pointer> m_PathNodes;
        mitk::DataNode::Pointer m_CenterlinesModelNode;
        mitk::DataNode::Pointer m_MergedCenterlinesModelNode;
        std::vector<int> mm_SourceCapIds;

        void run();
    };

    WorkThread* m_Thread;



};

#endif
