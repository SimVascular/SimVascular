#ifndef SVCLOSEALL_H
#define SVCLOSEALL_H

#include "svAbstractView.h"

class svCloseAll : public svAbstractView
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    svCloseAll();

    virtual ~svCloseAll();

    virtual void Exec() override;
};

#endif // SVCLOSEALL_H
