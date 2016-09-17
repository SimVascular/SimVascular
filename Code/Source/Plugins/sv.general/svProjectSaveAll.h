#ifndef SVPROJECTSAVEALL_H
#define SVPROJECTSAVEALL_H

#include "svAbstractView.h"

class svProjectSaveAll : public svAbstractView
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    svProjectSaveAll();

    virtual ~svProjectSaveAll();

    virtual void Exec() override;
};

#endif // SVPROJECTSAVEALL_H
