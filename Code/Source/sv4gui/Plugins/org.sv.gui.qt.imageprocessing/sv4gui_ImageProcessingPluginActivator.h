#ifndef sv4gui_ImagePROCESSINGPLUGINACTIVATOR_H
#define sv4gui_ImagePROCESSINGPLUGINACTIVATOR_H

#include <ctkPluginActivator.h>

class sv4guiImageProcessingPluginActivator :
        public QObject, public ctkPluginActivator
{
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "org_sv_gui_qt_imageprocessing")
    Q_INTERFACES(ctkPluginActivator)

public:

//    static ctkPluginContext* GetContext();
//    static svPathPlanningPluginActivator* GetInstance();

    void start(ctkPluginContext* context) override;
    void stop(ctkPluginContext* context) override;

private:

};

#endif // sv4guiImagePROCESSINGPLUGINACTIVATOR_H
