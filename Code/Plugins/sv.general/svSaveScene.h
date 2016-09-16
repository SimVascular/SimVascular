#ifndef SVSAVESCENE_H
#define SVSAVESCENE_H

#include "svAbstractView.h"

class svSaveScene : public svAbstractView
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    svSaveScene();

    virtual ~svSaveScene();

    virtual void Exec() override;
};

#endif // SVSAVESCENE_H
