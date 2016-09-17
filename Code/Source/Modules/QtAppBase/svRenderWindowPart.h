#ifndef SVRENDERWINDOWPART_H
#define SVRENDERWINDOWPART_H

#include "SimVascular.h"

#include <svQtAppBaseExports.h>

#include <QString>
#include <QStringList>
#include <QHash>
#include <QtPlugin>

//#include <mitkNumericTypes.h>
#include <mitkRenderingManager.h>
#include <QmitkStdMultiWidget.h>

class QmitkRenderWindow;

namespace mitk {
class SliceNavigationController;
}

/**
 *
 * A RenderWindowPart provides zero or more QmitkRenderWindow instances which can
 * be controlled via this interface. QmitkRenderWindow instances have an associated
 * \e id, which is implementation specific. However, implementations should consider
 * to use one of the following ids for certain QmitkRenderWindow instances to maximize
 * reusability (they are free to map multiple ids to one QmitkRenderWindow internally):
 * <ul>
 * <li>axial</li>
 * <li>sagittal</li>
 * <li>coronal</li>
 * <li>3d</li>
 * </ul>
 */
class SVQTAPPBASE_EXPORT svRenderWindowPart : public QObject
{
  Q_OBJECT

public:

  svRenderWindowPart();

  virtual ~svRenderWindowPart();

public slots:

  static QmitkStdMultiWidget* GetDisplayWidget();

//  virtual QmitkRenderWindow* GetActiveQmitkRenderWindow();

  static QHash<QString,QmitkRenderWindow*> GetQmitkRenderWindows();

  static QmitkRenderWindow* GetQmitkRenderWindow(const QString& id);

  static mitk::RenderingManager* GetRenderingManager();

  static void RequestUpdate(mitk::RenderingManager::RequestType requestType = mitk::RenderingManager::REQUEST_UPDATE_ALL);

  static void ForceImmediateUpdate(mitk::RenderingManager::RequestType requestType = mitk::RenderingManager::REQUEST_UPDATE_ALL);

  static mitk::SliceNavigationController* GetTimeNavigationController();

  static mitk::Point3D GetSelectedPosition(const QString& id = QString());

  static void SetSelectedPosition(const mitk::Point3D& pos, const QString& id = QString());

};

#endif // SVRENDERWINDOWPART_H
