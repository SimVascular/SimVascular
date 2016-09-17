#ifndef SVABSTRACTVIEW_H_
#define SVABSTRACTVIEW_H_

#include "SimVascular.h"

#include <svQtAppBaseExports.h>

#include "svAbstractFunctionality.h"
#include "svQmitkDataManager.h"
#include <QItemSelectionModel>

class svAbstractViewPrivate;

/**
 *
 * svAbstractView provides several convenience methods that ease the introduction of a new widget:
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
 * Usually all widgets inherit from svAbstractView to achieve a consistent behavior.
 *
 * When inheriting fromsvAbstractView, you must implement the following methods:
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

class SVQTAPPBASE_EXPORT svAbstractView : public svAbstractFunctionality
{

    Q_OBJECT

public:

  /**
   * Nothing to do in the standard ctor. <b>Initiliaze your GUI in CreateQtPartControl(QWidget*)</b>
   */
  svAbstractView();

  /**
   * Disconnects all standard event listeners
   */
  virtual ~svAbstractView();

  /**
   * \return The current selection in DataManager or an empty list
   *         if there is no selection or if it is empty.
   */
  static QList<mitk::DataNode::Pointer> GetCurrentSelection();

  /**
   * Queries the state of the current selection.
   *
   * \return If the current selection is <code>NULL</code>, this method returns
   * <code>false</code> and <code>true</code> otherwise.
   */
  static bool IsCurrentSelectionValid();

  static svQmitkDataManager* GetDataManager();

protected:

  /**
   * Informs that the node is selected.
   */
  void FireNodeSelected(mitk::DataNode::Pointer node);

  /**
   * Informs that the nodes are selected.
   */
  void FireNodesSelected(const QList<mitk::DataNode::Pointer>& nodes);

  /**
   * Called immediately after CreateQtPartControl().
   * Here standard event listeners for a QmitkAbstractView are registered
   */
  void AfterCreateQtPartControl() override;

  protected slots:

  void HandleOnSelectionChanged();

//  void AppendDataManagerMenuActions();

private:

  /**
   * Called when the selection changed.
   * May be reimplemented by deriving classes.
   *
   * \param nodes A list of selected nodes.
   */
  virtual void OnSelectionChanged(const QList<mitk::DataNode::Pointer> &nodes);

  /**
   * Called when a <code>NULL</code> selection occurs.
   *
   * \param part The source part responsible for the selection change.
   */
  virtual void OnNullSelection();

//  virtual void CreateQtPartControl(QWidget* parent) = 0;

//  virtual void SetFocus() ;

//  virtual void AppendMenuActions(QMenu* menu);

    friend class svAbstractViewPrivate;

    Q_DISABLE_COPY(svAbstractView)

    const QScopedPointer<svAbstractViewPrivate> dd;

};

#endif /*SVABSTRACTVIEW_H_*/
