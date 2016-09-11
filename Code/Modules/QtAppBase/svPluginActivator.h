#ifndef SVPLUGINACTIVATOR_H
#define SVPLUGINACTIVATOR_H

#include "SimVascular.h"

#include <svQtAppBaseExports.h>

#include "svMacros.h"
#include <QtPlugin>

class SVQTAPPBASE_EXPORT svPluginActivator
{
public:
    virtual ~svPluginActivator(){}
    virtual void start() = 0;
    virtual void stop() = 0;

};

Q_DECLARE_INTERFACE(svPluginActivator, "svpluginactivator")

#endif // SVPLUGINACTIVATOR_H
