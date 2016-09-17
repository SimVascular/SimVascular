#ifndef SVGENERALINIT_H
#define SVGENERALINIT_H

#include "svAbstractView.h"

#include <QmitkNodeDescriptor.h>

class svGeneralInit : public svAbstractView
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    svGeneralInit();

    virtual ~svGeneralInit();

public slots:

    void RemoveSelectedNodes( bool checked = false );

    void RenameSelectedNode( bool checked = false );

    void AddImage(bool checked=false);

    void CloseProject(bool checked=false);

protected:
    std::vector< std::pair< QmitkNodeDescriptor*, QAction* > > m_DescriptorActionList;

};

#endif // SVGENERALINIT_H
