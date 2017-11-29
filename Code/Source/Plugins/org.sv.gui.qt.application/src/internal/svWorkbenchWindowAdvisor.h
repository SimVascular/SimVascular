#ifndef SVWORKBENCHWINDOWADVISOR_H
#define SVWORKBENCHWINDOWADVISOR_H

#include <org_sv_gui_qt_application_Export.h>

#include "svDataNodeOperationInterface.h"

#include <QmitkNodeDescriptor.h>
#include <mitkDataNode.h>
#include <mitkDataStorage.h>

#include <berryWorkbenchWindowAdvisor.h>

#include <berryIPartListener.h>
#include <berryIEditorPart.h>
#include <berryIWorkbenchPage.h>
#include <berryWorkbenchAdvisor.h>
#include <berryWorkbenchWindowAdvisor.h>

#include <QAction>
#include <QList>

class QAction;
class QMenu;

class SV_QT_APPLICATION svWorkbenchWindowAdvisor : public QObject, public berry::WorkbenchWindowAdvisor
{
  Q_OBJECT

public:

    svWorkbenchWindowAdvisor(berry::WorkbenchAdvisor* wbAdvisor,
        berry::IWorkbenchWindowConfigurer::Pointer configurer);

    ~svWorkbenchWindowAdvisor();

    berry::SmartPointer<berry::ActionBarAdvisor> CreateActionBarAdvisor(
        berry::SmartPointer<berry::IActionBarConfigurer> configurer) override;

    QWidget* CreateEmptyWindowContents(QWidget* parent) override;

    void PostWindowCreate() override;

    void PreWindowOpen() override;

    void PostWindowOpen() override;

    void PostWindowClose() override;

    void ShowViewToolbar(bool show);

    void ShowPerspectiveToolbar(bool show);

    void ShowVersionInfo(bool show);

    void ShowSVVersionInfo(bool show);

    void ShowViewMenuItem(bool show);

    void ShowNewWindowMenuItem(bool show);

    void ShowClosePerspectiveMenuItem(bool show);

    bool GetShowClosePerspectiveMenuItem();

    void ShowMemoryIndicator(bool show);

    bool GetShowMemoryIndicator();

    //TODO should be removed when product support is here
    void SetProductName(const QString& product);
    void SetWindowIcon(const QString& wndIcon);

    void SetPerspectiveExcludeList(const QList<QString> &v);
    QList<QString> GetPerspectiveExcludeList();

    void SetViewExcludeList(const QList<QString> &v);
    QList<QString> GetViewExcludeList();

    mitk::DataStorage::Pointer GetDataStorage();
    std::list< mitk::DataNode::Pointer > GetSelectedDataNodes();
    void SetupDataManagerDoubleClick();
    void AddCustomMenuItemsForDataManager();

    void ToggleSlicePlane(QString name);

    void SetCrosshairGapZero();

protected slots:

    virtual void onIntro();
    virtual void onHelp();
    virtual void onHelpOpenHelpPerspective();
    virtual void onAbout();

    void ShowSVView();

    void ExitApplication();

    void RemoveSelectedNodes( bool checked = false );
    void RenameSelectedNode( bool checked = false );

    void CopyDataNode( bool checked );
    void PasteDataNode( bool checked );

    void ToggleAxialPlane(bool checked);

    void ToggleSagittalPlane(bool checked);

    void ToggleCoronalPlane(bool checked);

protected:
    std::vector< std::pair< QmitkNodeDescriptor*, QAction* > > m_DescriptorActionList;

private:

  void HookTitleUpdateListeners(berry::IWorkbenchWindowConfigurer::Pointer configurer);

  QString ComputeTitle();

  void RecomputeTitle();

  QString GetQSettingsFile() const;

  void UpdateTitle(bool editorHidden);

  void PropertyChange(const berry::Object::Pointer& /*source*/, int propId);

  static QString QT_SETTINGS_FILENAME;

  QScopedPointer<berry::IPartListener> titlePartListener;
  QScopedPointer<berry::IPerspectiveListener> titlePerspectiveListener;
  QScopedPointer<berry::IPerspectiveListener> menuPerspectiveListener;
  QScopedPointer<berry::IPartListener> imageNavigatorPartListener;
  QScopedPointer<berry::IPartListener> viewNavigatorPartListener;
  QScopedPointer<berry::IPropertyChangeListener> editorPropertyListener;
  friend struct berry::PropertyChangeIntAdapter<svWorkbenchWindowAdvisor>;
  friend class PartListenerForTitle;
  friend class PerspectiveListenerForTitle;
  friend class PerspectiveListenerForMenu;
  friend class PartListenerForImageNavigator;
  friend class PartListenerForViewNavigator;

  berry::IEditorPart::WeakPtr lastActiveEditor;
  berry::IPerspectiveDescriptor::WeakPtr lastPerspective;
  berry::IWorkbenchPage::WeakPtr lastActivePage;
  QString lastEditorTitle;
  berry::IAdaptable* lastInput;

  berry::WorkbenchAdvisor* wbAdvisor;
  bool showViewToolbar;
  bool showPerspectiveToolbar;
  bool showVersionInfo;
  bool showSVVersionInfo;
  bool showViewMenuItem;
  bool showNewWindowMenuItem;
  bool showClosePerspectiveMenuItem;
  bool viewNavigatorFound;
  bool showMemoryIndicator;
  QString productName;
  QString windowIcon;

  // enables DnD on the editor area
  QScopedPointer<berry::IDropTargetListener> dropTargetListener;

  // stringlist for excluding perspectives from the perspective menu entry (e.g. Welcome Perspective)
  QList<QString> perspectiveExcludeList;

  // stringlist for excluding views from the menu entry
  QList<QString> viewExcludeList;

  // maps perspective ids to QAction objects
  QHash<QString, QAction*> mapPerspIdToAction;

  // actions which will be enabled/disabled depending on the application state
  QList<QAction*> viewActions;
  QAction* fileSaveProjectAction;
  //QAction* closeProjectAction;
  QAction* undoAction;
  QAction* redoAction;
  QAction* imageNavigatorAction;
  QAction* viewNavigatorAction;
  QAction* resetPerspAction;
  QAction* closePerspAction;
  QAction* openDicomEditorAction;

  QAction* saveSVProjectAction;
  QAction* saveSVProjectAsAction;
  QAction* closeSVProjectAction;
  QList<QAction*> svViewActions;
  QList<QAction*> otherViewActions;

  bool m_UndoEnabled;

  svDataNodeOperationInterface* m_Interface;

  mitk::DataNode::Pointer  m_CopyDataNode;

};

#endif /*SVWORKBENCHWINDOWADVISOR_H*/
