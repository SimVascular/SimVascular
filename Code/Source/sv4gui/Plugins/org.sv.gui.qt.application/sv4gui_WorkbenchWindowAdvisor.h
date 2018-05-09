/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef SV4GUI_WORKBENCHWINDOWADVISOR_H
#define SV4GUI_WORKBENCHWINDOWADVISOR_H

#include <org_sv_gui_qt_application_Export.h>

#include "sv4gui_DataNodeOperationInterface.h"

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

class SV_QT_APPLICATION sv4guiWorkbenchWindowAdvisor : public QObject, public berry::WorkbenchWindowAdvisor
{
  Q_OBJECT

public:

    sv4guiWorkbenchWindowAdvisor(berry::WorkbenchAdvisor* wbAdvisor,
        berry::IWorkbenchWindowConfigurer::Pointer configurer);

    ~sv4guiWorkbenchWindowAdvisor();

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
  friend struct berry::PropertyChangeIntAdapter<sv4guiWorkbenchWindowAdvisor>;
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

  sv4guiDataNodeOperationInterface* m_Interface;

  mitk::DataNode::Pointer  m_CopyDataNode;

};

#endif /*SV4GUI_WORKBENCHWINDOWADVISOR_H*/
