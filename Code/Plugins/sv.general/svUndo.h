#ifndef SVUNDO_H
#define SVUNDO_H

#include "svAbstractExtension.h"

class svUndo : public svAbstractExtension
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    svUndo();

    virtual ~svUndo();

    virtual void Exec() override;
};

#endif // SVUNDO_H
