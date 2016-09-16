#ifndef SVABSTRACTEXTENSION_H
#define SVABSTRACTEXTENSION_H

#include "SimVascular.h"

#include <svQtAppBaseExports.h>

#include <QWidget>

class SVQTAPPBASE_EXPORT svAbstractExtension : public QWidget
{

    Q_OBJECT

public:

    svAbstractExtension(){}

    virtual ~svAbstractExtension(){}

    virtual void CreatePartControl(QWidget* parent=0){ }

    virtual void Enter() {}

    virtual void SetFocus() {}

    virtual void Leave() {}

    virtual void Exec() {}
};

#endif // SVABSTRACTEXTENSION_H
