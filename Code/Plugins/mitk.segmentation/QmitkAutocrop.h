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
#ifndef QMITKAUTOCROP_H
#define QMITKAUTOCROP_H

#include "svAbstractView.h"

#include "vector"
#include "mitkDataNode.h"
#include "mitkImage.h"

class  QmitkAutocrop : public svAbstractView
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    QmitkAutocrop();

    virtual ~QmitkAutocrop();

public slots:

    virtual void Exec() override;

    void Crop(const QList<mitk::DataNode::Pointer> &selectedNodes);

protected:

    mitk::Image::Pointer IncreaseCroppedImageSize( mitk::Image::Pointer image );

    std::vector< std::pair< QmitkNodeDescriptor*, QAction* > > mDescriptorActionList;

private:

};

#endif // QMITKAUTOCROP_H
