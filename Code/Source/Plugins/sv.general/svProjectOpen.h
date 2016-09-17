#ifndef SVPROJECTOPEN_H
#define SVPROJECTOPEN_H

#include "svAbstractView.h"

class svProjectOpen : public svAbstractView
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    svProjectOpen();

    virtual ~svProjectOpen();

    virtual void Exec() override;
};

#endif // SVPROJECTOPEN_H
