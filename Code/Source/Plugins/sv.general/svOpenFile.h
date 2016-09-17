#ifndef SVOPENFILE_H
#define SVOPENFILE_H

#include "svAbstractView.h"

class svOpenFile : public svAbstractView
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    svOpenFile();

    virtual ~svOpenFile();

    virtual void Exec() override;
};

#endif // SVOPENFILE_H
