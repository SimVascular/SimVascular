#ifndef SVREDO_H
#define SVREDO_H

#include "svAbstractExtension.h"

class svRedo : public svAbstractExtension
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    svRedo();

    virtual ~svRedo();

    virtual void Exec() override;
};

#endif // SVREDO_H
