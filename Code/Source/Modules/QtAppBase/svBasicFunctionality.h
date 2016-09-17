#ifndef SVBASICFUNCTIONALITY_H_
#define SVBASICFUNCTIONALITY_H_

#include "SimVascular.h"

#include <svQtAppBaseExports.h>

#ifdef __MINGW32__
// We need to inlclude winbase.h here in order to declare
// atomic intrinsics like InterlockedIncrement correctly.
// Otherwhise, they would be declared wrong within qatomic_windows.h .
#include <windows.h>
#endif

#include "svAbstractExtension.h"

#include <QWidget>
#include <mitkDataStorage.h>
#include <QmitkNodeDescriptorManager.h>
#include <mitkStandaloneDataStorage.h>

namespace mitk {
  class DataNode;
}

class svBasicFunctionalityPrivate;

/**
 *
 * svBasicFunctionality provides several convenience methods that ease the introduction of a new widget:
 *
 * <ol>
 *   <li> Access to the DataStorage (~ the shared data repository)
 *   <li> Access to the DisplayWidget
 *   <li> Access to and update notification for the view's preferences
 *   <li> Access to and update notification for the current DataNode selection / to DataNode selection events send through the SelectionService
 *   <li> Access to and update notification for DataNode events (added/removed/modified)
 *   <li> Methods to send DataNode selections through the SelectionService
 *   <li> Some minor important convenience methods (like changing the mouse cursor/exception handling)
 * </ol>
 *
 * Usually all widgets inherit from svBasicFunctionality to achieve a consistent behavior.
 *
 * When inheriting fromsvBasicFunctionality, you must implement the following methods:
 * <ul>
 * <li>void CreateQtPartControl(QWidget* parent)
 * <li>void SetFocus()
 * </ul>
 *
 * You may reimplement the following private virtual methods to be notified about certain changes:
 * <ul>
 * <li>void OnSelectionChanged(berry::IWorkbenchPart::Pointer part, const QList<mitk::DataNode::Pointer> &nodes)
 * <li>void OnNullSelection(berry::IWorkbenchPart::Pointer part)
 * <li>void OnPreferencesChanged(const berry::IBerryPreferences*)
 * <li>void NodeAdded(const mitk::DataNode* node)
 * <li>void NodeChanged(const mitk::DataNode* node)
 * <li>void NodeRemoved(const mitk::DataNode* node)
 * <li>void DataStorageModified()
 * </ul>
 *
 */
class SVQTAPPBASE_EXPORT svBasicFunctionality : public svAbstractExtension
{

    Q_OBJECT

public:

  /**
   * Nothing to do in the standard ctor. <b>Initiliaze your GUI in CreateQtPartControl(QWidget*)</b>
   */
  svBasicFunctionality();

  /**
   * Disconnects all standard event listeners
   */
  virtual ~svBasicFunctionality();

  /**
   * Returns the Preferences object for this View.
   * <b>Important</b>: When refering to this preferences, e.g. in a PreferencePage: The ID
   * for this preferences object is "/<VIEW-ID>", e.g. "/org.mitk.views.datamanager"
   */
//  berry::IPreferences::Pointer GetPreferences() const;

  /**
   * Returns the currently active DataStorage.
   */
  static mitk::DataStorage::Pointer GetDataStorage();

  static mitk::StandaloneDataStorage::Pointer GetStandaloneDataStorage();

  /**
   * Outputs an error message to the console and displays a message box containing
   * the exception description.
   * \param e the exception which should be handled
   * \param showDialog controls, whether additionally a message box should be
   *        displayed to inform the user that something went wrong
   */
  static void HandleException( std::exception& e, QWidget* parent = nullptr, bool showDialog = true );

  /**
   * Calls HandleException ( std::exception&, QWidget*, bool ) internally
   * \see HandleException ( std::exception&, QWidget*, bool )
   */
  static void HandleException( const char* str, QWidget* parent = nullptr, bool showDialog = true );

  /**
   * Convenient method to set and reset a wait cursor ("hourglass")
   */
  static void WaitCursorOn();

  /**
   * Convenient method to restore the standard cursor
   */
  static void WaitCursorOff();

  /**
   * Convenient method to set and reset a busy cursor
   */
  static void BusyCursorOn();

  /**
   * Convenient method to restore the standard cursor
   */
  static void BusyCursorOff();

  /**
   * Convenient method to restore the standard cursor
   */
  static void RestoreOverrideCursor();

  static void useExtension(QString extensionID);

  static svAbstractExtension* GetExtension(QString extensionID);

  static QmitkNodeDescriptorManager* getNodeDescriptorManager();

protected:

  /**
   * Creates a scroll area for this view and calls CreateQtPartControl
   */
  void CreatePartControl(QWidget* parent = 0) override;

  virtual void AfterCreateQtPartControl();

  void showEvent(QShowEvent *se) override;

  void hideEvent(QHideEvent *he) override;

  void Enter() override;

  void Leave() override;

private:

  /**
   * Called when the preferences object of this view changed.
   * May be reimplemented by deriving classes.
   *
   * \see GetPreferences()
   */
//  virtual void OnPreferencesChanged(const berry::IBerryPreferences*);

  /**
   * Called when a DataStorage Add event was thrown. May be reimplemented
   * by deriving classes.
   */
  virtual void NodeAdded(const mitk::DataNode* node);

  /**
   * Called when a DataStorage Changed event was thrown. May be reimplemented
   * by deriving classes.
   */
  virtual void NodeChanged(const mitk::DataNode* node);

  /**
   * Called when a DataStorage Remove event was thrown. May be reimplemented
   * by deriving classes.
   */
  virtual void NodeRemoved(const mitk::DataNode* node);

  /**
   * Called when a DataStorage add *or* remove *or* change event from the currently active
   * data storage is thrown.
   *
   * May be reimplemented by deriving classes.
   */
  virtual void DataStorageModified();

  /**
   * Called immediately after CreateQtPartControl().
   * Here standard event listeners for a QmitkAbstractView are registered
   */

  virtual void CreateQtPartControl(QWidget* parent);

//  virtual void SetFocus() ;

  virtual void Visible();

  virtual void Hidden();

  virtual void Activated();

  virtual void Deactivated();

private:

  friend class svBasicFunctionalityPrivate;

  Q_DISABLE_COPY(svBasicFunctionality)

  const QScopedPointer<svBasicFunctionalityPrivate> d;

protected:

  bool m_Active=false;

  bool IsActivated();

  void SetActivated(bool activated);

};

#endif /*SVBASICFUNCTIONALITY_H_*/
