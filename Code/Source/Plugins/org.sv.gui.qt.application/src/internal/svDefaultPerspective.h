#ifndef SVDEFAULTPERSPECTIVE_H
#define SVDEFAULTPERSPECTIVE_H

#include <berryIPerspectiveFactory.h>

class svDefaultPerspective : public QObject, public berry::IPerspectiveFactory
{
    Q_OBJECT
    Q_INTERFACES(berry::IPerspectiveFactory)

public:

    svDefaultPerspective();

    void CreateInitialLayout(berry::IPageLayout::Pointer layout) override;

};

#endif /* SVDEFAULTPERSPECTIVE_H */
