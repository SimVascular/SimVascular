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
#ifndef QMITKCREATEPOLYGONMODEL_H
#define QMITKCREATEPOLYGONMODEL_H

#include "svAbstractView.h"

class QmitkCreatePolygonModel : public svAbstractView
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    QmitkCreatePolygonModel();

    ~QmitkCreatePolygonModel();

public slots:

    virtual void Exec() override;

    void Create();

    void CreateSmoothed();

    void CreatePolygonModel(const QList<mitk::DataNode::Pointer> &selectedNodes);

    void SetSmoothed(bool smoothed) ;

    void SetDecimated(bool decimated) ;

    void OnSurfaceCalculationDone();

private:

    QmitkCreatePolygonModel(const QmitkCreatePolygonModel &);
    QmitkCreatePolygonModel & operator=(const QmitkCreatePolygonModel &);

    bool m_IsSmoothed;

    bool m_IsDecimated;

    std::vector< std::pair< QmitkNodeDescriptor*, QAction* > > mDescriptorActionList;
};

#endif
