#ifndef SVEXIT_H
#define SVEXIT_H

#include "svAbstractView.h"

class svExit : public svAbstractExtension
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    svExit();

    virtual ~svExit();

    virtual void Exec() override;
};

#endif // SVEXIT_H
