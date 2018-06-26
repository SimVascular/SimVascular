#ifndef sv4guisvFSIPLUGINACTIVATOR_H
#define sv4guisvFSIPLUGINACTIVATOR_H

#include <ctkPluginActivator.h>

class sv4guisvFSIPluginActivator :
        public QObject, public ctkPluginActivator
{
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "org_sv_gui_qt_svfsi")
    Q_INTERFACES(ctkPluginActivator)

public:

//    static ctkPluginContext* GetContext();
//    static sv4guisvFSIPluginActivator* GetInstance();

    void start(ctkPluginContext* context) override;
    void stop(ctkPluginContext* context) override;

private:
//    static sv4guisvFSIPluginActivator* m_Instance;
//    static ctkPluginContext* m_Context;

};

#endif // sv4guisvFSIPLUGINACTIVATOR_H
