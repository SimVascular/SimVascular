
#include <QObject>

class ctkPluginContext;
class QmitkPreferencesDialog;

class svWorkbenchWindowAdvisorHack : public QObject
{
    Q_OBJECT

public slots:

    void onUndo();
    void onRedo();
    void onImageNavigator();
    void onViewNavigator();
    void onEditPreferences();
    void onQuit();

    void onResetPerspective();
    void onClosePerspective();
    void onNewWindow();
    void onIntro();

    /**
     * @brief This slot is called if the user klicks the menu item "help->context help" or presses F1.
     * The help page is shown in a workbench editor.
     */
    void onHelp();

    void onHelpOpenHelpPerspective();

    /**
     * @brief This slot is called if the user clicks in help menu the about button
     */
    void onAbout();

public:

    svWorkbenchWindowAdvisorHack();
    ~svWorkbenchWindowAdvisorHack();

    static svWorkbenchWindowAdvisorHack* undohack;
};
