#ifndef SVAPPLICATIONPLUGINACTIVATOR_H
#define SVAPPLICATIONPLUGINACTIVATOR_H

#include <berryAbstractUICTKPlugin.h>

#include <QString>

class svApplicationPluginActivator : public berry::AbstractUICTKPlugin

{
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "org_sv_gui_qt_application")
    Q_INTERFACES(ctkPluginActivator)

public:

    svApplicationPluginActivator();
    ~svApplicationPluginActivator();

    static svApplicationPluginActivator* GetDefault();

    void start(ctkPluginContext* context) override;
    void stop(ctkPluginContext* context) override;

    static ctkPluginContext* getContext();

private:

private Q_SLOTS:

private:

    static ctkPluginContext* _context;

    static svApplicationPluginActivator* inst;
};

#endif /* SVAPPLICATIONPLUGINACTIVATOR_H */
