#ifndef SVAPPLICATIONPLUGINACTIVATOR_H
#define SVAPPLICATIONPLUGINACTIVATOR_H

//#include <ctkPluginActivator.h>
#include <berryAbstractUICTKPlugin.h>

#include <QString>

//class svApplicationPluginActivator : public QObject, public ctkPluginActivator
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

    //  void loadDataFromDisk(const QStringList& args, bool globalReinit);
    //  void startNewInstance(const QStringList& args, const QStringList &files);

private Q_SLOTS:

    //  void handleIPCMessage(const QByteArray &msg);

private:

    static ctkPluginContext* _context;

    static svApplicationPluginActivator* inst;
};

#endif /* SVAPPLICATIONPLUGINACTIVATOR_H */
